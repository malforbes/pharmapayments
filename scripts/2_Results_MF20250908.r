# Malcolm Forbes
# September 2025
# This script analyses payments_cleaned_updated.csv
# Read alongside Results section of manuscript

suppressPackageStartupMessages({
library(tidyverse); library(readxl); library(stringr); library(lubridate); library(knitr); 
library(scales); library(binom); library(janitor); library(writexl); library(DescTools)
})
payments <- read_csv("payments_cleaned_updated.csv")
payments <- payments |>
  mutate(
    name      = str_to_upper(str_squish(name)),
    specialty = str_to_upper(str_squish(specialty)))

# ---- PAYMENT VOLUME ---- 
payments |>
  summarise(
    n_obs = n(),
    n_unique_hcps = n_distinct(name),
    n_unique_hcps_narm = n_distinct(name, specialty),
    n_specialties = n_distinct(specialty, na.rm = TRUE),
    n_companies = n_distinct(company, na.rm = TRUE),
    n_services = n_distinct(service, na.rm = TRUE),
    n_events = n_distinct(event, na.rm = TRUE),
    n_payments = n_distinct(payment_to, na.rm = TRUE),
    total_payments = sum(total_payment, na.rm = TRUE),
    q1_payment = unname(quantile(total_payment, 0.25, na.rm = TRUE)),
    q3_payment = unname(quantile(total_payment, 0.75, na.rm = TRUE)),
    median_payment = median(total_payment, na.rm = TRUE),
    iqr_payment = IQR(total_payment, na.rm = TRUE), 
    min_payment = min(total_payment, na.rm = TRUE), 
    max_payment = max(total_payment, na.rm = TRUE)
  )

# ---- TABLE 1 ---- 
top4 <- c(
  "Educational meeting attendee",
  "Educational meeting speaker or chair",
  "Advisory board committee attendee",
  "Consultant")
payments2 <- payments |>
  mutate(service = if_else(!is.na(service) & service %in% top4, service, "Other"))
total_clinicians <- payments2 |> distinct(name, specialty) |> nrow()
tab1 <- payments2 |>
  summarise(
    total_aud     = sum(total_payment, na.rm = TRUE),
    n_unique_hcps = n_distinct(name, specialty),
    .by = service
  ) |>
  mutate(
    pct_of_all_clinicians = round(100 * n_unique_hcps / total_clinicians, 1),
    pct_hcps              = round(100 * n_unique_hcps / sum(n_unique_hcps), 0) 
  ) |>
  arrange(desc(total_aud))
tab1

# ---- PROFESSIONAL AND SPECIALTY REACH ---- 
payments |> 
  group_by(profession) |> 
  summarise(total_payment = sum(total_payment, na.rm = TRUE), .groups = "drop") |> 
  mutate(percentage = 100 * total_payment / sum(total_payment)) |> 
  arrange(desc(total_payment)) |> 
  print()

medical_practitioner <- 142569
nurse                <- 477822
pharmacist           <- 38610

payments |>
  group_by(profession) |>
  summarise(
    n_with_payment = n_distinct(name, specialty),
    total_aud      = sum(total_payment, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(denominator = case_when(
            profession == "Medical Professionals" ~ medical_practitioner,
            profession == "Nurses"                ~ nurse,
            profession == "Pharmacists"           ~ pharmacist,
            TRUE ~ NA_real_),
         proportion_pct = round(100 * n_with_payment / denominator, 1)) |> 
         arrange(desc(proportion_pct))

# ---- FIGURE 1 ----
figure1 <- payments |> 
  filter(specialty != "NURSING") |>
  group_by(specialty) |>
  summarise(total_payment = sum(total_payment, na.rm = TRUE), .groups = "drop") |>
  slice_max(total_payment, n = 10) |>
  ggplot(aes(x = fct_reorder(specialty, total_payment), y = total_payment)) +
  geom_col() +
  coord_flip() +
  labs(x = "Specialty", y = "Total Payment (A$)", title = "Top 10 Specialties by Total Payment") +
  theme_bw()
print(figure1)
ggsave("figure1.png", plot = figure1, width = 8, height = 6)

# ---- FIGURE 2 ----
suppressPackageStartupMessages({
  library(tidyverse); library(readr); library(ggrepel); library(scales); library(RColorBrewer)
  library(showtext)
})

if (!"Montserrat" %in% sysfonts::font_families()) {
  showtext::font_add_google("Montserrat", family = "Montserrat")
}
showtext::showtext_auto()

payments <- payments |>
  mutate(total_payment = suppressWarnings(as.numeric(total_payment))) |>
  filter(total_payment > 0)

hcp_summary <- payments |>
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
  filter(unique_hcps >= 100, total_payment > 0, median_payment_per_hcp > 0)  

highlights <- c(
  "HAEMATOLOGY AND ONCOLOGY",
  "RHEUMATOLOGY",
  "CARDIOLOGY",
  "GENERAL PRACTICE",
  "NURSING"
)

pal <- setNames(brewer.pal(5, "Dark2"), highlights)
data_summary <- data_summary |>
  mutate(
    is_highlight = specialty %in% highlights,
    colour = if_else(is_highlight, pal[specialty], "#8c8c8c"),
    label  = if_else(is_highlight, specialty, NA_character_)
  )
  
size_breaks <- c(100, 1500, 3000)
size_breaks <- size_breaks[size_breaks >= min(data_summary$unique_hcps) &
                           size_breaks <= max(data_summary$unique_hcps)]

p_bubbles <- ggplot(data_summary, aes(x = median_payment_per_hcp, y = total_payment)) +
  geom_point(data = subset(data_summary, !is_highlight),
             aes(size = unique_hcps, colour = colour), alpha = 0.6) +
  geom_point(data = subset(data_summary, is_highlight),
             aes(size = unique_hcps, colour = colour), alpha = 0.9) +
  geom_text_repel(
    data = subset(data_summary, is_highlight),
    aes(label = label, colour = colour),
    size = 5, fontface = "bold", box.padding = 0.35, point.padding = 0.25,
    max.overlaps = Inf, show.legend = FALSE
  ) +
  geom_smooth(method = "lm", se = FALSE, colour = "grey65", linetype = "dashed") +
  scale_x_log10(labels = label_dollar(prefix = "A$")) +
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
  theme_bw(base_family = "Montserrat") +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 9),
    legend.text  = element_text(size = 9)
  )

print(p_bubbles)
ggsave("bubble_specialties_filtered.png", p_bubbles, width = 9, height = 6, dpi = 300)

# ---- TABLE 2 ---- (THIS HAS NOW BEEN REPLACED BY FIGURE 3 - SPENCER CODE)
ahpra_xlsx <- "ahpra_202324.XLSX"
payments <- read_csv("payments_cleaned_updated.csv", show_col_types = FALSE) |>
  mutate(SPEC_UP = normalise_up(specialty))
r9_raw <- read_excel(ahpra_xlsx, sheet = "R9", skip = 2, .name_repair = "minimal")
nm <- names(r9_raw)
prof_col <- nm[str_detect(nm, regex("^\\s*Profession\\s*$", ignore_case = TRUE))][1]
tot_col  <- nm[str_detect(nm, regex("^\\s*Total\\s*\\n?\\s*2023\\s*/\\s*24\\s*$", ignore_case = TRUE))][1]
r9 <- r9_raw |>
  transmute(
    Profession    = as.character(.data[[prof_col]]),
    Total_2023_24 = suppressWarnings(as.numeric(.data[[tot_col]]))  
  ) |>
  filter(!is.na(Profession), !is.na(Total_2023_24))
normalise_up <- \(x) x |> str_replace_all("[\u2013\u2014]", "-") |> str_squish() |> str_to_upper()
r9 <- r9 |> mutate(PROF_UP = normalise_up(Profession))

numerators <- payments |>
  semi_join(r9, by = c("SPEC_UP" = "PROF_UP")) |>
  summarise(Unique_with_payment = n_distinct(name), .by = SPEC_UP)

totals <- payments |>
  summarise(Total_to_specialty = sum(total_payment, na.rm = TRUE), .by = SPEC_UP)

base <- numerators |>
  left_join(r9 |> select(PROF_UP, Profession, Total_2023_24), by = c("SPEC_UP" = "PROF_UP")) |>
  left_join(totals, by = "SPEC_UP") |>
  mutate(Specialty = Profession) |>
  transmute(Specialty,
            Unique_with_payment,
            Denominator_2023_24 = Total_2023_24,
            Proportion_pct = round(100 * Unique_with_payment / Denominator_2023_24, 1),
            Total_to_specialty)

haem_onc_num <- payments |>
  filter(total_payment > 0, str_to_upper(specialty) == "HAEMATOLOGY AND ONCOLOGY") |>
  summarise(Unique_with_payment = n_distinct(name),
            Total_to_specialty  = sum(total_payment, na.rm = TRUE), .groups = "drop")

haem_denom <- r9 |> filter(PROF_UP == "HAEMATOLOGY") |> summarise(val = max(Total_2023_24, na.rm = TRUE)) |> pull(val)
onc_denom  <- r9 |> filter(PROF_UP == "MEDICAL ONCOLOGY") |> summarise(val = max(Total_2023_24, na.rm = TRUE)) |> pull(val)
haem_onc_denom <- haem_denom + onc_denom

haem_onc_row <- haem_onc_num |>
  mutate(Specialty = "Haematology and oncology",
         Denominator_2023_24 = haem_onc_denom,
         Proportion_pct = round(100 * Unique_with_payment / Denominator_2023_24, 1)) |>
  select(Specialty, Unique_with_payment, Denominator_2023_24, Proportion_pct, Total_to_specialty)

add_wilson <- function(df) {
  ci <- binom::binom.confint(x = df$Unique_with_payment,
                              n = df$Denominator_2023_24,
                              methods = "wilson")
  df |>
    mutate(
      ci_lower_pct = round(100 * ci$lower, 1),
      ci_upper_pct = round(100 * ci$upper, 1),
      prop_with_ci = sprintf("%.1f%% (%.1f–%.1f)", Proportion_pct, ci_lower_pct, ci_upper_pct)
    )
}

# ---- TABLE WITH >150 fellows ---- 
table2_over150 <- base |>
  filter(Denominator_2023_24 > 150) |>
  bind_rows(haem_onc_row) |>
  arrange(desc(Proportion_pct), desc(Total_to_specialty)) |>
  add_wilson()
print(table2_over150, n = 10)
write.csv(table2_over150, "table2_over150.csv", row.names = FALSE)

# ---- SUPPLEMENT: FULL TABLE ----
table2_all <- base |>
  bind_rows(haem_onc_row) |>
  arrange(desc(Proportion_pct), desc(Total_to_specialty)) |>
  add_wilson()
print(table2_all, n = 10)
write_xlsx(
  list(
    Table2_Over150Fellows = table2_over150,
    Table2_AllSpecialties = table2_all
  ),
  path = "table2_outputs.xlsx"
)

# ---- PAYMENT CONCENTRATION ----
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

# ---- SUPPLEMENTARY FIGURE 1 ---- 
lorenz_plot <- ggplot(lorenz_df, aes(cum_recip, cum_share)) +
  geom_line(linewidth = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  labs(x = "Cumulative share of recipients", y = "Cumulative share of payments",
       title = paste0("Lorenz curve of payments (Gini = ", gini_coef$Gini, ")")) +
  theme_bw(base_size = 11)
ggsave("lorenz_gini.png", lorenz_plot, width = 6, height = 6, dpi = 300)

winsorise_and_conc <- function(x, frac) {
  x <- sort(x, decreasing = TRUE)
  k  <- ceiling(frac * length(x))
  cut_val <- x[pmax(1, k)]
  xw <- pmin(x, cut_val)
  tibble(
    Winsorised_at = scales::percent(frac, accuracy = 1),
    Top_1_pct  = round(100 * sum(xw[1:ceiling(0.01 * length(xw))]) / sum(xw), 1),
    Top_5_pct  = round(100 * sum(xw[1:ceiling(0.05 * length(xw))]) / sum(xw), 1),
    Top_10_pct = round(100 * sum(xw[1:ceiling(0.10 * length(xw))]) / sum(xw), 1)
  )
}
winsor_tbl <- bind_rows(
  winsorise_and_conc(recip_totals$total, 0.01),
  winsorise_and_conc(recip_totals$total, 0.05),
  winsorise_and_conc(recip_totals$total, 0.10)
)

x <- pay |> filter(total_payment > 0)
per_name_periods <- x |>
  count(name, period) |>
  count(name, name = "n_periods")

persistence_overall <- tibble(
  gt2    = sum(per_name_periods$n_periods > 2),
  gt3    = sum(per_name_periods$n_periods > 3),
  gt4    = sum(per_name_periods$n_periods > 4),
  pct_gt2 = round(100 * gt2 / nrow(per_name_periods), 1),
  pct_gt3 = round(100 * gt3 / nrow(per_name_periods), 1),
  pct_gt4 = round(100 * gt4 / nrow(per_name_periods), 1)
)

primary_spec <- x |>
  group_by(name, specialty) |>
  summarise(tot = sum(total_payment, na.rm = TRUE), .groups = "drop_last") |>
  slice_max(tot, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(name, specialty)

persistence_by_primary_spec <- per_name_periods |>
  filter(n_periods > 2) |>
  left_join(primary_spec, by = "name") |>
  count(specialty, sort = TRUE) |>
  slice_head(n = 10)

comp_per_clin <- x |>
  filter(!is.na(company)) |>
  distinct(name, company) |>
  count(name, name = "n_comp")

N_names  <- nrow(comp_per_clin)
k1_names <- ceiling(0.01 * N_names)
thresh_top1 <- comp_per_clin |>
  arrange(desc(n_comp)) |>
  slice(k1_names) |>
  pull(n_comp)

company_breadth_summary <- tibble(
  N_names           = N_names,
  median_companies  = median(comp_per_clin$n_comp),
  iqr_lower         = as.numeric(quantile(comp_per_clin$n_comp, .25)),
  iqr_upper         = as.numeric(quantile(comp_per_clin$n_comp, .75)),
  p99               = as.numeric(quantile(comp_per_clin$n_comp, .99, type = 7)),
  top1pct_threshold = thresh_top1,
  top1pct_n         = sum(comp_per_clin$n_comp >= thresh_top1),
  max_companies     = max(comp_per_clin$n_comp)
)

top_names <- comp_per_clin |> filter(n_comp >= thresh_top1) |> pull(name)
top1pct_primary_specialties <- primary_spec |>
  filter(name %in% top_names) |>
  count(specialty, sort = TRUE)

write_xlsx(
  list(
    TopShares                    = top_shares,
    Lorenz_Data                  = lorenz_df,
    Gini                         = gini_coef,
    Winsorisation                = winsor_tbl,
    Persistence_overall          = persistence_overall,
    Persistence_by_primarySpec   = persistence_by_primary_spec,
    Company_breadth_summary      = company_breadth_summary,
    Top1pct_primary_specialties  = top1pct_primary_specialties
  ),
  path = "concentration_outputs.xlsx"
)

# ---- PAYMENTS ACROSS THE STUDY PERIOD ----
annual_summary <- payments |>
  mutate(
    total_payment  = readr::parse_number(as.character(total_payment)),
    reporting_year = as.integer(str_extract(period, "\\d{4}$")) 
  ) |>
  group_by(reporting_year) |>
  summarise(
    payments_AUD_million = sum(total_payment, na.rm = TRUE) / 1e6,
    recipients           = n_distinct(name),
    .groups = "drop"
  ) |>
  arrange(reporting_year) |>
  mutate(payments_AUD_million = round(payments_AUD_million, 2)) |>
  filter(reporting_year %in% c(2017,2018,2019,2020,2022,2023,2024))
print(annual_summary)

table(payments$period)

data_company_time <- payments |> 
  mutate(
    period_clean = str_trim(period),
    period_date = case_when(
      str_detect(period_clean, "^[A-Za-z]+ \\d{4}$") ~ 
        lubridate::dmy(paste0("01 ", period_clean)),
      str_detect(period_clean, "^[A-Za-z]+ \\d{4} to [A-Za-z]+ \\d{4}$") ~ 
        lubridate::dmy(paste0("01 ", str_extract(period_clean, "^[A-Za-z]+ \\d{4}"))),
      TRUE ~ as.Date(NA)
    )
  ) |> 
  drop_na(period_date) |> 
  group_by(period_date, company) |> 
  summarise(total_payment = sum(total_payment, na.rm = TRUE), .groups = "drop")

top_companies_by_total <- data_company_time |> 
  group_by(company) |> 
  summarise(total_all_periods = sum(total_payment, na.rm = TRUE), .groups = "drop") |> 
  slice_max(order_by = total_all_periods, n = 5) |> 
  pull(company)

data_top_companies <- data_company_time |> 
  filter(company %in% top_companies_by_total)

# ---- FIGURE 4 ---- 
p_combined_trends <- ggplot() +
  geom_smooth(data = data_top_companies, aes(x = period_date, y = total_payment, colour = company), 
              method = "loess", se = FALSE, linewidth = 1.2) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
  scale_y_continuous(
    labels = scales::comma
  ) +
  scale_colour_bmj() +
  labs(title = "Payment trends over time - Top 5 companies",
       x = "Reporting period", y = "Total payments ($ AUD)", colour = "Company") +
  theme_minimal(base_family = "Montserrat") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9)) -> p_combined_trends
print(p_combined_trends)
CairoJPEG("company_trends_bmj.jpg", width = 2000, height = 1200, res = 300)
print(p_combined_trends)
dev.off()

# ---- SUPPLEMENT ----
payments <- payments |>
  mutate(
    total_payment = readr::parse_number(as.character(total_payment)),
    reporting_year = as.integer(str_extract(period, "\\d{4}$"))
  )

tableS1 <- payments |>
  filter(total_payment > 0, !is.na(company)) |>
  summarise(total_payment = sum(total_payment, na.rm = TRUE), .by = company) |>
  arrange(desc(total_payment)) |>
  mutate(
    Share_of_total_payments = round(100 * total_payment / sum(total_payment, na.rm = TRUE), 1)
  )
tableS1

company_year <- payments |>
  filter(total_payment > 0, !is.na(company), !is.na(reporting_year)) |>
  summarise(total_payment = sum(total_payment, na.rm = TRUE), .by = c(reporting_year, company)) |>
  group_by(reporting_year) |>
  mutate(
    rank = dense_rank(desc(total_payment)),
    share_pct = 100 * total_payment / sum(total_payment, na.rm = TRUE)
  ) |>
  ungroup() |>
  arrange(reporting_year, rank)

up     <- \(x) x |> str_replace_all("[\u2013\u2014]", "-") |> str_squish() |> str_to_upper()
pretty <- \(s) ifelse(up(s) == "HAEMATOLOGY AND ONCOLOGY", "Haematology and Oncology", str_to_title(s))

spec_totals <- payments |>
  filter(total_payment > 0) |>
  mutate(SPEC_UP = up(specialty)) |>
  summarise(spec_total = sum(total_payment, na.rm = TRUE), .by = SPEC_UP)

spec_company <- payments |>
  filter(total_payment > 0, !is.na(company)) |>
  mutate(SPEC_UP = up(specialty)) |>
  summarise(amount = sum(total_payment, na.rm = TRUE), .by = c(SPEC_UP, company)) |>
  left_join(spec_totals, by = "SPEC_UP") |>
  mutate(share_pct = 100 * amount / spec_total)

top_company_by_spec <- spec_company |>
  group_by(SPEC_UP) |>
  slice_max(amount, n = 1, with_ties = FALSE) |>
  ungroup() |>
  filter(amount >= 1e5) |>
  transmute(
    Specialty = pretty(SPEC_UP),
    `Top company` = company,
    `Amount (A$)` = round(amount, 0),
    `Share of specialty total (%)` = round(share_pct, 0)
  ) |>
  arrange(desc(`Share of specialty total (%)`))

example_specs <- c("PALLIATIVE MEDICINE", "PAIN MEDICINE", "OPHTHALMOLOGY")
examples <- spec_company |>
  filter(SPEC_UP %in% example_specs) |>
  group_by(SPEC_UP) |>
  slice_max(amount, n = 1, with_ties = FALSE) |>
  ungroup() |>
  transmute(
    Specialty = pretty(SPEC_UP),
    Company   = company,
    `Amount (A$)` = round(amount, 0),
    `Share of payments to specialty (%)` = round(share_pct, 1)
  )

write_xlsx(
  list(
    Table3_AllCompanies      = table3,
    Company_Rankings_ByYear  = company_year,
    TopCompany_BySpecialty   = top_company_by_spec,
    Company_Specialty_Examples = examples
  ),
  path = "supplement_companies_outputs.xlsx"
)

# ---- REVISION ----
# Figure S1 
annual_summary <- payments |> 
  mutate(
    total_payment  = readr::parse_number(as.character(total_payment))
  ) |>
  group_by(period) |>
  summarise(
    payments_AUD_million = sum(total_payment, na.rm = TRUE) / 1e6,
    recipients           = n_distinct(name),
    .groups = "drop"
  ) |>
  arrange(period) |>
  mutate(payments_AUD_million = round(payments_AUD_million, 2))
print(annual_summary)

annual_summary <- payments |>
  mutate(
    total_payment = readr::parse_number(as.character(total_payment)),
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
  filter(!is.na(reporting_year)) |>
  group_by(reporting_year) |>
  summarise(
    payments_AUD_million = sum(total_payment, na.rm = TRUE) / 1e6,
    recipients = n_distinct(name),
    .groups = "drop"
  ) |>
  arrange(reporting_year)
print(annual_summary)



# get the number of companies reporting for each year 
companies_per_year <- payments |>
  mutate(
    total_payment = readr::parse_number(as.character(total_payment)),
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
  filter(!is.na(reporting_year), !is.na(company)) |>
  group_by(reporting_year) |>
  summarise(n_companies = n_distinct(company), .groups = "drop") |>
  arrange(reporting_year)
companies_per_year

# find which 3 companies are missing from 2017/18 to 2018/19
comp_2017_18 <- payments |>
  mutate(
    total_payment = readr::parse_number(as.character(total_payment)),
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
  filter(!is.na(reporting_year), !is.na(company), reporting_year == "2017/18") |>
  distinct(company) |> 
  arrange(company) |> 
  pull(company)
comp_2018_19 <- payments |>
  mutate(
    total_payment = readr::parse_number(as.character(total_payment)),
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
  filter(!is.na(reporting_year), !is.na(company), reporting_year == "2018/19") |>
  distinct(company) |> 
  arrange(company) |> 
  pull(company)
comp_2018_19