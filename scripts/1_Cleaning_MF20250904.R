# Malcolm Forbes 
# August 2025
# This script cleans the final dataset that uploaded by Ashleigh Hooimeyer to GDrive in April 2025 (payments.csv)

suppressPackageStartupMessages({
  library(tidyverse)
  library(stringr)
  library(readr)
  library(lubridate)
  library(stringi)
})

# Load 
payments <- read_csv("payments.csv", show_col_types = FALSE)

# Helper
up_text <- \(x) x |> str_replace_all("[\u2013\u2014]", "-") |> str_squish()
spec_norm <- \(x) up_text(x) |> str_to_upper()

add_periods_to_initials <- function(given) {
  toks <- str_split(str_squish(given), "\\s+")[[1]]
  if (length(toks) == 0) return("")
  toks <- vapply(toks, function(tk) if (str_detect(tk, "^[A-Za-z]$")) paste0(tk, ".") else tk, character(1))
  paste(toks, collapse = " ")
}
normalise_name_commas_only <- function(nm_raw) {
  if (is.na(nm_raw)) return(NA_character_)
  nm <- nm_raw |>
    as.character() |>
    str_squish() |>
    str_replace_all("\\s*,\\s*", ", ") |>
    str_remove("^,\\s*") |>
    str_remove("\\s*,$")
  if (nm == "") return(nm)
  if (!str_detect(nm, ","))
    nm <- str_replace(nm, "^(\\S+)\\s+(.+)$", "\\1, \\2")
  parts <- str_split(nm, ",\\s*", n = 2)[[1]]
  if (length(parts) < 2) return(nm)
  paste0(parts[1], ", ", add_periods_to_initials(parts[2]))
}

payments <- payments |>
  mutate(across(c(total_payment, registration_fees, travel_costs, fees_for_service),
                ~ suppressWarnings(parse_number(as.character(.x)))))

# Speciality tidy 
payments <- payments |>
  mutate(specialty = up_text(specialty)) |>
  mutate(specialty = case_when(
    specialty == "GP" ~ "GENERAL PRACTICE",
    specialty == "HAEMATOLOGY_ONCOLOGY" ~ "HAEMATOLOGY AND ONCOLOGY",
    specialty == "NEURLOGY" ~ "NEUROLOGY",
    specialty == "Non-clinical" ~ "NON-CLINICAL",
    specialty == "Nurse" ~ "NURSING",
    specialty == "OPTOMETRIST" ~ "OPTOMETRY OR ORTHOPTICS",
    specialty == "OTHER" ~ "NON-CLINICAL",
    specialty == "GASTROENTEROLOGY" ~ "GASTROENTEROLOGY AND HEPATOLOGY",
    specialty == "PAEDIATRIC HAEMATOLOGY_ONCOLOGY" ~ "PAEDIATRIC HAEMATOLOGY AND ONCOLOGY",
    specialty == "PAEDIATRIC MEDICAL ONCOLOGY" ~ "PAEDIATRIC HAEMATOLOGY AND ONCOLOGY",
    specialty == "Pharmacist" ~ "PHARMACY",
    specialty %in% c("Physiotherapist or Occupational Therapist") ~ "PHYSIOTHERAPY OR OCCUPATIONAL THERAPY",
    specialty %in% c("Radiographer","Radiography") ~ "RADIOGRAPHY",
    specialty == "RHEUMATOLOGIST" ~ "RHEUMATOLOGY",
    specialty == "REPRODUCTIVE ENDOCRINOLOGY AND INFERTILITY" ~ "OBSTETRICS AND GYNAECOLOGY",
    specialty == "PAEDIATRICS INFECTIOUS DISEASES" ~ "PAEDIATRIC INFECTIOUS DISEASES",
    TRUE ~ specialty
  ))

# Profession tidy
spec_up_vec <- str_to_upper(payments$specialty)
payments <- payments |>
  mutate(profession = case_when(
    spec_up_vec == "NURSING"   ~ "Nurses",
    spec_up_vec == "PHARMACY"  ~ "Pharmacists",
    spec_up_vec == "SCIENTIST" ~ "Scientists",
    spec_up_vec %in% c("DIETETICS","EXERCISE PHYSIOLOGY","PHYSIOTHERAPY OR OCCUPATIONAL THERAPY","PODIATRY","SPEECH PATHOLOGY","SOCIAL WORK") ~ "Allied health",
    spec_up_vec %in% c("DENTISTRY","OPTOMETRY OR ORTHOPTICS","RADIOGRAPHY","PSYCHOLOGY","PARAMEDICINE","VETERINARY MEDICINE","NON-CLINICAL") ~ "Other",
    TRUE ~ "Medical Professionals"
  ))

# Company standardisation 
payments <- payments |>
  mutate(company_raw = company,
         company_key = company_raw |>
           str_squish() |>
           stri_trans_general("Latin-ASCII") |>
           str_to_lower()) |>
  mutate(company = recode(company_key,
    "a. menarini australia pty ltd" = "Menarini",
    "alexion pharmaceuticals australasia pty ltd" = "Alexion",
    "astrazeneca australia" = "AstraZeneca",
    "bayer australia" = "Bayer", "bayer" = "Bayer",
    "biomarin pharmaceutical australia pty ltd" = "BioMarin",
    "biogen australia" = "Biogen",
    "daiichi sankyo australia pty ltd" = "Daiichi Sankyo",
    "eli lilly australia" = "Eli Lilly",
    "glaxosmithkline australia pty ltd" = "GlaxoSmithKline",
    "ipsen pty ltd" = "Ipsen",
    "janssen-cilag pty ltd" = "Janssen",
    "kyowa kirin australia pty ltd" = "Kyowa Kirin",
    "merck healthcare pty ltd" = "Merck KGaA",
    "merck sharp & dohde (australia) pty ltd" = "MSD (Merck & Co.)",
    "merck sharp & dohde (australia) pty ltd." = "MSD (Merck & Co.)",
    "norgine pty ltd" = "Norgine",
    "novartis pharmaceutical australia pty ltd" = "Novartis",
    "organon australia" = "Organon",
    "takeda pharmaceuticals australia pty ltd" = "Takeda",
    "vertex australia" = "Vertex",
    "bristol-myers squibb" = "Bristol Myers Squibb",
    "astellas pharma" = "Astellas",
    "alexion alexion pharmaceuticals australasia pty ltd" = "Alexion",
    "ucb pharma" = "UCB",
    "vifor pharma" = "CSL Vifor",
    "pfizer ltd" = "Pfizer",
    .default = company_raw
  )) |>
  select(-company_key)

# Service standardisation
payments <- payments |>
  mutate(.srv = service |>
           str_replace_all("/", " ") |>
           str_squish() |>
           str_to_lower(),
         service = case_when(
           str_detect(.srv, "^advisory.*(board|committee)")                ~ "Advisory board committee attendee",
           str_detect(.srv, "^company\\s*meeting.*attendee")               ~ "Company meeting attendee",
           str_detect(.srv, "^consultant") | str_detect(.srv, "engagement")~ "Consultant",
           str_detect(.srv, "^educational.*(speaker|chair)")               ~ "Educational meeting speaker or chair",
           str_detect(.srv, "^educational.*attendee")                      ~ "Educational meeting attendee",
           str_detect(.srv, "^independent.*attendee")                      ~ "Third-party educational meeting attendee",
           str_detect(.srv, "^market\\s*research.*participant")            ~ "Market research participant",
           str_detect(.srv, "^virtual.*meeting.*attendee")                 ~ "Virtual meeting attendee",
           str_detect(.srv, "^sponsorship$")                               ~ "Sponsorship",
           str_detect(.srv, "^not\\s*specified$")                          ~ "Not specified",
           TRUE                                                            ~ str_to_sentence(.srv)
         )) |>
  select(-.srv)

# Event standardisation
payments <- payments |>
  mutate(.evt = event |>
           as.character() |>
           str_replace_all("[\u2013\u2014]", "-") |>
           str_squish() |>
           str_to_lower(),
         event = case_when(
           str_starts(.evt, "advisory board") | str_detect(.evt, "committee") ~ "Advisory board / committee meeting",
           str_starts(.evt, "company meeting")                                 ~ "Company meeting",
           str_starts(.evt, "independent meeting")                              ~ "Independent meeting",
           str_starts(.evt, "educational meeting")                              ~ "Educational meeting",
           str_starts(.evt, "consulting")                                       ~ "Consulting session",
           str_starts(.evt, "market research")                                  ~ "Market research",
           str_starts(.evt, "virtual meeting")                                  ~ "Virtual meeting",
           str_starts(.evt, "other")                                            ~ "Other",
           TRUE                                                                 ~ str_to_sentence(.evt)
         )) |>
  select(-.evt)

# Payment_to standardisation
payments <- payments |>
  mutate(.pt = payment_to |>
           as.character() |>
           str_squish() |>
           str_to_lower(),
         payment_to = case_when(
           str_detect(.pt, "third")                        ~ "Third party",
           str_detect(.pt, "employer") | .pt == "employer" ~ "Health care professional's employer",
           str_detect(.pt, "professional")                 ~ "Health care professional",
           TRUE                                            ~ str_to_sentence(.pt)
         ),
         route = if_else(str_detect(.pt, "employer|third"), "Indirect", "Direct")) |>
  select(-.pt)

# Period tidy
payments <- payments |>
  mutate(period = str_replace(period,
                              "^October 2015 to April 2016$",
                              "November 2015 to April 2016"))

# Name normalisation
payments <- payments |>
  mutate(name_raw = as.character(name),
         name     = vapply(name_raw, normalise_name_commas_only, character(1)))

# Specialty corrections based on known errors
corr <- tribble(
  ~name, ~new_specialty,
  "Al-Mukhtar, Omar","CARDIOLOGY","Ali, Baha","GENERAL PRACTICE",
  "Ambler, Geoffrey","PAEDIATRIC ENDOCRINOLOGY","Anis, Sameh T.","PSYCHIATRY",
  "Bailey, Christopher","OPHTHALMOLOGY","Bosco, Anthony","HAEMATOLOGY AND ONCOLOGY",
  "Bulfone, Liliana","PHARMACY","Byrne, Anthony","RESPIRATORY AND SLEEP MEDICINE",
  "Chowdhury, Jyotsna P.","GENERAL PRACTICE","Dharmadmajan, Anoop","CARDIOLOGY",
  "Dickson, David","OPHTHALMOLOGY","Downie Doyle, Sarah","SCIENTIST",
  "Dunn, Anna","RESPIRATORY AND SLEEP MEDICINE","Fitzgerald, John","GENERAL PRACTICE",
  "Forwood, Kathryn","HAEMATOLOGY AND ONCOLOGY","Grant, Michelle","NURSING",
  "Hamdorf, Mostyn C.","GENERAL PRACTICE","Hancock, Melissa","HAEMATOLOGY AND ONCOLOGY",
  "Hayes, Rachel","DIETETICS","Hensby, Alisha M.","SCIENTIST",
  "Hillebrand, Paulina","HAEMATOLOGY AND ONCOLOGY","Hope, Nicole","OBSTETRICS AND GYNAECOLOGY",
  "Hopkinson, Kim","PHYSIOTHERAPY OR OCCUPATIONAL THERAPY","Jahan, Nargis","GENERAL PRACTICE",
  "James, Alan","RESPIRATORY AND SLEEP MEDICINE","Kaur, Jaswinder","GENERAL PRACTICE",
  "Khan, Mohammad A.","GENERAL PRACTICE","Kumar, Arun","GENERAL PRACTICE",
  "Lafferty, Antony R.","PAEDIATRIC ENDOCRINOLOGY","Lawrence, Joanne","GENERAL PRACTICE",
  "Lee, Chee K.","HAEMATOLOGY AND ONCOLOGY","McIntyre, David","ENDOCRINOLOGY",
  "McMahon, Lawrence","NEPHROLOGY","Medical, Practitioner",NA_character_,
  "Mikhail, Peter","PHARMACY","Moore, Melissa","HAEMATOLOGY AND ONCOLOGY",
  "Murphy, Kate","PSYCHIATRY","Nguyen, Phillip","HAEMATOLOGY AND ONCOLOGY",
  "Nguyen, Yvonne","DERMATOLOGY","Pentony, Peta E.","RHEUMATOLOGY",
  "Radford, Andrea","SCIENTIST","Ritchie, Angus, G.","NEPHROLOGY",
  "Rogers, James","CARDIOLOGY","Senthil Kumaran, Sivaupramanian","GENERAL PRACTICE",
  "Shahzad, Anwar","CARDIOLOGY","Shaik, Abdul R.","GASTROENTEROLOGY AND HEPATOLOGY",
  "Shaw, Sally","GENERAL PRACTICE","Smith, Amanda","NURSING",
  "Soong, Mei-Min","GERIATRIC MEDICINE","Walker, Stephen","HAEMATOLOGY AND ONCOLOGY",
  "Wang, Ying","NEPHROLOGY","Whitton, Bradley","NURSING",
  "Wong, Vincent","ENDOCRINOLOGY","Zacharin, Margaret","PAEDIATRIC ENDOCRINOLOGY"
)

payments <- payments |>
  mutate(name_key = str_squish(name)) |>
  left_join(corr |> mutate(name_key = name) |> select(name_key, new_specialty), by = "name_key") |>
  mutate(specialty = coalesce(new_specialty, specialty)) |>
  select(-name_key, -new_specialty)

# Relevant columns 
payments <- payments |>
  select(name, specialty, profession, address,
         registration_fees, travel_costs, fees_for_service, total_payment,
         company, service, event, payment_to, route, period, date_event)

# Check duplicates
dup_all <- payments |>
  group_by(across(everything())) |>
  summarise(n = n(), .groups = "drop") |>
  filter(n > 1)
message("Exact duplicate rows (all-cols): ", nrow(dup_all))

dup_key <- payments |>
  count(name, company, total_payment, period, name = "n") |>
  filter(n > 1)
message("Duplicate key groups (name, company, amount, period): ", nrow(dup_key))

dup5_groups <- payments |>
  group_by(across(everything())) |>
  summarise(n = n(), .groups = "drop") |>
  filter(n >= 5)
dup5_rows <- payments |>
  inner_join(dup5_groups |> select(-n), by = names(payments)) |>
  arrange(name)

dat <- payments |> filter(!is.na(date_event))
dups_by_date <- dat |> count(name, date_event, name = "n") |> filter(n > 1) |> select(name, date_event)
dat_dups <- dat |> inner_join(dups_by_date, by = c("name","date_event")) |>
  arrange(name, date_event, company, total_payment)

# Checks
tibble(
  rows_raw = nrow(payments),
  rows_clean = nrow(payments_cleaned),
  exact_dup_rows = nrow(dup_all),
  key_dup_groups = nrow(dup_key),
  unique_name_specialty = n_distinct(payments_cleaned$name, payments_cleaned$specialty),
  missing_names = sum(is.na(payments_cleaned$name)),
  missing_specialties = sum(is.na(payments_cleaned$specialty)),
  total_AUD = sum(payments_cleaned$total_payment, na.rm = TRUE)
) |> print()

# Remove zero payments 
payments <- payments |>
  filter(total_payment > 0)

# Check missing specialties 
missing_specialty_analysis <- payments |>
  filter(is.na(specialty)) |>
  summarise(
    rows_with_missing_specialty = n(),
    unique_names_with_missing_specialty = n_distinct(name, na.rm = TRUE)
  )
print(missing_specialty_analysis)

# Export
payments_cleaned <- payments
write_csv(payments_cleaned, "payments_cleaned.csv")

# Additional corrections from K.H. (3 September 2025)
payments_cleaned <- read_csv("payments_cleaned_updated.csv")
