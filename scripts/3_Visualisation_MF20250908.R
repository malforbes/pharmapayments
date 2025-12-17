# Spencer Schien
# September 2025
# This script creates the figures in the paper. 
# Requires payments_cleaned_updated.csv and table_2.csv.

# ---------------------------------------------------------------------------
# DATA PREP
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse); 
  library(stringr); 
  library(lubridate); 
  library(scales); 
  library(Cairo);
  library(RColorBrewer);
  library(ggrepel);
  library(ggsci);
  library(glue);
  library(DescTools)
})

payments <- read_csv("payments_cleaned_updated.csv")

# bmj colors
bmj_blue <- "#2a6ebb"
bmj_grey <- "#747678"
bmj_black <- "#333333"
bmj_orange <- "#e37222"
bmj_red <- "#CD202C"

theme_set(
  theme_minimal(base_family = "Montserrat", base_size = 14) +
    theme(
      text = element_text(color = bmj_black),
      panel.grid.minor = element_blank(),
      plot.title.position = "plot",
      plot.title = element_text(face = "bold",
                                margin = margin(b = 8),
                                size = 20),
      plot.subtitle = element_text(margin = margin(b = 30),
                                   size = 16),
      plot.caption.position = "plot",
      plot.caption = element_text(hjust = 0),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10)),
      plot.margin = margin(rep(10, 4))
    )
)

update_geom_defaults("text",  list(family = "Montserrat"))

# ---------------------------------------------------------------------------
# FIGURE 1
# ---------------------------------------------------------------------------

fig1_data <- payments |> 
  filter(specialty != "NURSING") |>
  group_by(specialty) |>
  summarise(total_payment = sum(total_payment, na.rm = TRUE), .groups = "drop") |>
  arrange(desc(total_payment)) |> 
  mutate(
    specialty_label = str_replace_all(specialty, "_", " ") |> 
      str_to_title(),
    specialty_label = str_replace_all(specialty_label, "And", "and"),
    perc_of_total = total_payment / sum(total_payment),
    ind = row_number(),
    specialty_label = ifelse(ind > 10, "All Other Specialties", specialty_label)
  ) |> 
  group_by(specialty = specialty_label) |> 
  summarise(total_payment = sum(total_payment)) |> 
  mutate(perc = total_payment / sum(total_payment)) |> 
  filter(specialty != "All Other Specialties")

fig1_data |> 
  ggplot(aes(x = reorder(specialty, total_payment), 
             y = total_payment, 
             fill = specialty)) +
  geom_col(fill = bmj_blue) +
  geom_text(aes(
    label = label_dollar(suffix = "m", scale = 1e-6, accuracy = .1)(total_payment)
  ), color = "white", fontface = "bold", size = 6, hjust = 1, nudge_y = -2e5,
  family = "Montserrat") +
  coord_flip() +
  scale_x_discrete(labels = function(x) str_wrap(x, 20)) +
  scale_y_continuous(labels = label_dollar(scale = 1e-6, prefix = "A$"), 
                     expand = expansion(mult = c(0, 0.05)),
                     breaks = c(10e6, 20e6)) +
  # scale_fill_bmj() +
  labs(title = "Total payments by specialty",
       x = NULL,
       y = "Total payments (million AUD)") +
  theme_minimal(base_family = "Montserrat") +
  theme(
    plot.margin = margin(t = 10, r = 10, b = 20, l = 10),
    # axis.text.y = element_text(lineheight = .4),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(size = 18),
    plot.title = element_text(face = "bold", color = bmj_black),
    plot.title.position = "plot",
    axis.title.x = element_text(margin = margin(t = 10))
  )

ggsave(
  "figure_1.png", bg = "white",
  device = png, width = 4000, height = 2400, units = "px", res = 300
)

# ---------------------------------------------------------------------------
# FIGURE 2
# ---------------------------------------------------------------------------

f2_payments <- payments |>
  mutate(total_payment = suppressWarnings(as.numeric(total_payment))) |>
  filter(total_payment > 0)

hcp_summary <- f2_payments |>
  group_by(specialty, name) |>
  summarise(total_payment_per_hcp = sum(total_payment, na.rm = TRUE), .groups = "drop")

data_summary <- hcp_summary |>
  group_by(specialty) |>
  summarise(
    unique_hcps            = n_distinct(name),
    total_payment          = sum(total_payment_per_hcp, na.rm = TRUE),
    median_payment_per_hcp = median(total_payment_per_hcp, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(unique_hcps >= 100, total_payment > 0, median_payment_per_hcp > 0)  # <-- exclude <100

highlights <- c(
  "HAEMATOLOGY AND ONCOLOGY",
  "RHEUMATOLOGY",
  "CARDIOLOGY",
  "GENERAL PRACTICE",
  "NURSING"
)

pal <- setNames(pal_bmj()(5), highlights)
data_summary <- data_summary |>
  mutate(
    is_highlight = specialty %in% highlights,
    colour = if_else(is_highlight, pal[specialty], "#747678"),
    label  = if_else(is_highlight, specialty, NA_character_),
    label = str_to_title(label) |> 
      str_replace_all("And", "and") |> 
      str_wrap(15),
    lab_x = case_when(
      str_detect(label, "^Haem") ~ median_payment_per_hcp - 300,
      str_detect(label, "^Card") ~ median_payment_per_hcp + 300,
      TRUE ~ median_payment_per_hcp
    )
  )

size_breaks <- c(500, 1500, 3000)
size_breaks <- size_breaks[size_breaks >= min(data_summary$unique_hcps) &
                             size_breaks <= max(data_summary$unique_hcps)]

ggplot(data_summary, aes(x = median_payment_per_hcp, y = total_payment)) +
  geom_point(data = subset(data_summary, !is_highlight),
             aes(size = unique_hcps, colour = colour), alpha = 0.6) +
  geom_point(data = subset(data_summary, is_highlight),
             aes(size = unique_hcps, colour = colour), alpha = 0.9) +
  geom_text(
    data = subset(data_summary, is_highlight),
    aes(label = label, colour = colour,
        x = lab_x,
        hjust = case_when(
          str_detect(label, "^Haema") ~ 1,
          str_detect(label, "^Rheum") ~ 1,
          str_detect(label, "^Card") ~ 0,
          TRUE ~ .5
        ),
        vjust = case_when(
          str_detect(label, "^Gen|^Rheum") ~ 2,
          str_detect(label, "^Nurs") ~ -3,
          TRUE ~ .5
        )),
    size = 5, fontface = "bold", show.legend = FALSE,
    lineheight = .9
  ) +
  geom_smooth(method = "lm", se = FALSE, colour = "grey65", 
              linetype = 2, linewidth = .5) +
  scale_x_log10(labels = label_dollar(prefix = "A$", accuracy = 1),
                limits = c(750, 10000)) +
  scale_y_log10(labels = label_dollar(prefix = "A$")) +
  scale_size_continuous(
    range = c(2, 18),
    breaks = size_breaks,
    labels = comma,
    name   = "Unique clinicians with ≥1 payment"
  ) +
  scale_colour_identity() +
  labs(
    title = "Total payments vs median per-clinician payment by specialty",
    subtitle = "Specialties with <100 paid clinicians excluded; selected specialties highlighted",
    x = "Median payment per clinician (log scale)",
    y = "Total payments to specialty (log scale)"
  ) +
  theme(
    legend.position = "bottom"
  )



ggsave(
  "figure_2.png", bg = "white",
  device = png, width = 4000, height = 2400, units = "px", res = 300
)

# ---------------------------------------------------------------------------
# FIGURE 3
# ---------------------------------------------------------------------------

d <- readr::read_csv("table2_over150.csv", show_col_types = FALSE) |>
  janitor::clean_names() |>
  rename(
    clinicians_paid  = unique_with_payment,
    total_clinicians = denominator_2023_24,
    est        = proportion_pct,
    low_95_ci  = ci_lower_pct,
    high_95_ci = ci_upper_pct
  ) |>
  mutate_at(.vars = vars(est:high_95_ci), function(x) x / 100)

p <- d |>
  mutate(lab = glue("{label_percent(.1)(est)} ({clinicians_paid}/{total_clinicians})")) |>
  ggplot(aes(reorder(specialty, est), est)) +
  geom_col(fill = bmj_blue) +
  geom_errorbar(aes(
    xmin = reorder(specialty, est), xmax = reorder(specialty, est),
    ymin = low_95_ci, ymax = high_95_ci
  ), width = .4, linewidth = 1, color = bmj_black) +
  geom_text(aes(label = lab, y = high_95_ci), hjust = 0,
            nudge_y = .01, family = "Montserrat") +
  scale_y_continuous(labels = label_percent(),
                     limits = c(0, 1),
                     breaks = seq(from = 0, to = 1, by = .25),
                     expand = expansion(add = 0)) +
  scale_x_discrete(labels = function(x) str_wrap(x, 20)) +
  coord_flip(clip = "off") +
  theme(panel.grid.major = element_blank(),
        axis.line.x = element_line(color = bmj_black),
        plot.margin = margin(r = 30, 10, 10, 10),
        axis.ticks.x = element_line(color = bmj_black),
        axis.ticks.length.x = unit(2, "mm")) +
  labs(y = "Proportion of practitioners", x = "",
       caption = "* Combined denominator = clinical haematology (RACP) + medical oncology",
       title = "Specialty reach")


Cairo::CairoJPEG("figure_3_over150.jpg", width = 4000, height = 2400, units = "px", dpi = 320, bg = "white")
print(p)
dev.off()

tbl <- d |>
  transmute(
    Specialty = specialty,
    `Clinicians paid / total` = paste0(clinicians_paid, "/", total_clinicians),
    `Proportion paid` = est,
    `95% CI` = glue("{label_percent(.1)(low_95_ci)}–{label_percent(.1)(high_95_ci)}")
  ) |>
  arrange(desc(`Proportion paid`)) |>
  gt() |>
  fmt_percent(columns = `Proportion paid`, decimals = 1) |>
  cols_align("center", columns = everything()) |>
  tab_header(title = "Specialty reach (n ≥ 150)") |>
  tab_source_note(md("*Combined denominator = clinical haematology (RACP) + medical oncology*"))

gtsave(tbl, "table2_over150.html")

# ---------------------------------------------------------------------------
# FIGURE 4
# ---------------------------------------------------------------------------

company_year_top5_real <- readr::read_csv("company_year_top5_real.csv", show_col_types = FALSE)

d <- company_year_top5_real %>%
  select(reporting_year, company, payments_real_2023_24_AUD_million) %>%
  group_by(reporting_year, company) %>%
  summarise(total_payment = sum(payments_real_2023_24_AUD_million, na.rm = TRUE), .groups = "drop")

fy_levels <- d %>% distinct(reporting_year) %>%
  mutate(start = as.integer(substr(reporting_year, 1, 4))) %>%
  arrange(start) %>% pull(reporting_year)

data_company_time <- d %>%
  mutate(reporting_year = factor(reporting_year, levels = fy_levels),
         fy_index = as.integer(reporting_year))

top_companies_by_total <- data_company_time %>%
  group_by(company) %>%
  summarise(total_all_periods = sum(total_payment, na.rm = TRUE), .groups = "drop") %>%
  slice_max(order_by = total_all_periods, n = 5) %>%
  pull(company)

data_top_companies <- data_company_time %>%
  filter(company %in% top_companies_by_total)

comp_labs <- data_top_companies %>%
  group_by(company) %>%
  filter(fy_index == max(fy_index)) %>%
  ungroup()

p <- ggplot() +
  geom_smooth(data = data_top_companies,
              aes(x = fy_index, y = total_payment, colour = company),
              method = "loess", se = FALSE, linewidth = 1.2, span = 0.9) +
  geom_text(data = comp_labs,
            aes(x = fy_index, y = total_payment, label = company, color = company,
                vjust = case_when(
                  company == "Novartis" ~ -1.2,
                  company == "Pfizer" ~ 0,
                  company == "AstraZeneca" ~ 0.2,
                  company == "Bayer" ~ 1.2,
                  company == "Amgen" ~ 0.8,
                  TRUE ~ 0
                )),
            nudge_x = 0.6, hjust = 0, size = 8, fontface = "bold") +
  scale_x_continuous(breaks = seq_along(fy_levels), labels = fy_levels,
                     expand = expansion(add = c(0.5, 2))) +
  scale_y_continuous(labels = label_dollar(prefix = "A$", accuracy = 0.1)) +
  scale_colour_bmj() +
  labs(title = "Payment trends over time - Top 5 companies",
       x = "Reporting period", y = "Total payments (million AUD)", colour = "Company") +
  coord_cartesian(clip = "off") +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        axis.line.x = element_line(color = bmj_black),
        axis.ticks.x = element_line(color = bmj_black),
        axis.ticks.length = unit(2, "mm"),
        plot.margin = margin(r = 175, 10, 10, 10))

Cairo::CairoJPEG("figure_4_real.jpg", width = 4000, height = 2400, units = "px", dpi = 320, bg = "white")
print(p)
dev.off()

# ---------------------------------------------------------------------------
# FIGURE S1
# ---------------------------------------------------------------------------

pay <- read_csv("payments_cleaned_updated.csv", show_col_types = FALSE) |>
  mutate(
    total_payment = as.numeric(total_payment),
    recipient_id  = paste0(trimws(name), " | ", trimws(specialty))
  )

recip_totals <- pay |>
  summarise(total = sum(total_payment, na.rm = TRUE), .by = recipient_id) |>
  arrange(desc(total))

N   <- nrow(recip_totals)
k1  <- ceiling(0.01 * N); k5 <- ceiling(0.05 * N); k10 <- ceiling(0.10 * N)
tot <- sum(recip_totals$total)

top_shares <- tibble(
  group = c("Top 1%", "Top 5%", "Top 10%"),
  n     = c(k1, k5, k10),
  share = round(100 * c(sum(recip_totals$total[1:k1]),
                        sum(recip_totals$total[1:k5]),
                        sum(recip_totals$total[1:k10])) / tot, 2)
)

recip_totals_asc <- recip_totals |> arrange(total)
lorenz_df <- recip_totals_asc |>
  mutate(
    cum_recip = row_number() / n(),
    cum_share = cumsum(total) / sum(total)
  )
gini_coef <- tibble(Gini = round(Gini(recip_totals$total), 3))

lorenz_df |> 
  ggplot(aes(cum_recip, cum_share)) +
  geom_line(linewidth = 1.2, color = bmj_blue) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, color = bmj_black) +
  scale_y_continuous(labels = label_percent(),
                     expand = expansion(add = c(0, .05))) +
  scale_x_continuous(labels = label_percent(),
                     expand = c(.001,0)) +
  coord_cartesian(clip = "off") +
  theme(panel.grid.major.x = element_blank(),
        plot.margin = margin(r = 20, 10,10,10),
        axis.text.x = element_text(margin = margin(t = 5)),
        axis.line.x = element_line(color = bmj_black),
        axis.ticks.x = element_line(color = bmj_black, lineend = "round"),
        axis.ticks.length.x = unit(2, "mm")) +
  labs(x = "Cumulative share of recipients", 
       y = "Cumulative share of payments",
       title = paste0("Lorenz curve of payments (Gini = ", 
                      gini_coef$Gini, ")")) 

ggsave(
  "sup_figure_1.png", bg = "white",
  device = png, width = 4000, height = 2400, units = "px", res = 300
)

# ---------------------------------------------------------------------------
# FIGURE S2
# ---------------------------------------------------------------------------

this_data <- payments |> 
  mutate(
    period_clean = str_trim(period),
    period_date = case_when(
      # this case throws a warning because no period matches—-SS
      str_detect(period_clean, "^[A-Za-z]+ \\d{4}$") ~
        lubridate::dmy(paste0("01 ", period_clean)),
      str_detect(period_clean, "^[A-Za-z]+ \\d{4} to [A-Za-z]+ \\d{4}$") ~
        lubridate::dmy(paste0("01 ", str_extract(period_clean, "^[A-Za-z]+ \\d{4}"))),
      TRUE ~ as.Date(NA),
    ),
    reporting_year = case_when(
      period %in% c("November 2015 to April 2016", "May 2016 to October 2016") ~ "2015/16",
      period %in% c("November 2016 to April 2017", "May 2017 to October 2017") ~ "2016/17",
      period %in% c("November 2017 to April 2018", "May 2018 to October 2018") ~ "2017/18",
      period %in% c("November 2018 to April 2019", "May 2019 to October 2019") ~ "2018/19",
      period %in% c("November 2019 to April 2020", "May 2020 to October 2020") ~ "2019/20",
      period %in% c("November 2020 to April 2021", "May 2021 to October 2021") ~ "2020/21",
      period %in% c("November 2021 to April 2022", "May 2022 to October 2022") ~ "2021/22",
      period %in% c("November 2022 to April 2023", "May 2023 to October 2023") ~ "2022/23",
      period %in% c("November 2023 to April 2024", "May 2024 to October 2024") ~ "2023/24",
      TRUE ~ NA_character_
    )
  ) |> 
  mutate(p_year = year(period_date))

med_data <- this_data |> 
  group_by(p_year = reporting_year) |> 
  summarise(med = median(total_payment, na.rm = TRUE),
            total = sum(total_payment, na.rm = TRUE),
            n = n_distinct(name)) |> 
  mutate(lab = glue(
    "{label_dollar(prefix ='A$')(total)}\n",
    "n={label_comma()(n)}"
  ))

this_data |> 
  ggplot(aes(reporting_year, total_payment)) +
  geom_jitter(alpha = .1, width = .25, color = bmj_blue,
              size = .5) +
  # geom_segment(data = med_data,
  #              aes(x = p_year - .25, xend = p_year + .25,
  #                  y = med, yend = med), color = bmj_black,
  #              linewidth = 2) +
  stat_summary( data = med_data,
                aes(x = p_year, y = med),
                fun = median,
                fun.min = median,
                fun.max = median,
                geom = "crossbar", 
                width = .6,
                linewidth = 0.5,   # controls how wide the line segment is
                color = "black",
                size = 1
  ) +
  geom_text(data = med_data,
            aes(
              x = p_year, y = 1.5,
              label = lab
            ), family = "Montserrat", vjust = 0) +
  scale_y_log10(labels = label_dollar(prefix = "A$"), n.breaks = 6) +
  coord_cartesian(clip = "off") +
  theme(panel.grid.major.x = element_blank(),
        axis.line.x = element_line(color = bmj_black),
        axis.ticks.x = element_line(color = bmj_black),
        axis.ticks.length = unit(2, "mm")) +
  labs(x = "", y = "Payment amount",
       title = "Individual payments by year",
       subtitle = "Median shown by black bar")

ggsave(
  "sup_figure_2.png", bg = "white",
  device = png, width = 4000, height = 2400, units = "px", res = 300
)

