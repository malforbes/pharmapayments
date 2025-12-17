# Malcolm Forbes 
# 26 February 2025
# Pharmaceutical company payments to Australian healthcare professionals

# Accuracy checks on analysis run by Yeshna

#### 1. Libraries ####
library(tidyverse)
library(janitor)
library(stringdist)
library(scales) 
library(lubridate)
library(ggrepel)
library(RColorBrewer)
library(readxl)
library(stringr)
library(ggridges)
library(ggsci)
library(gridExtra)
library(showtext)
source("scripts/utils.R")

#### 2. Structure ####
data <- read_csv("PharmaData_MF20241211.csv")
View(data)
head(data)
glimpse(data)

#### 3. Check and clean #### 
data_filtered <- data |> 
  filter(!is.na(yeshna_specialty))
data_filtered <- data_filtered |> 
  mutate(
    similarity_score = stringdist::stringdist(str_trim(tolower(yeshna_specialty)), str_trim(tolower(final_spec)), method = "jw")
  )
non_matching <- data_filtered |> 
  filter(similarity_score > 0.0)
non_matching_unique <- non_matching |> 
  select(healthcare_professional, final_name, final_spec, yeshna_name, yeshna_specialty, total_payment) |> 
  distinct(healthcare_professional, .keep_all = TRUE)
write_csv(non_matching_unique, "non_matching_specialties_unique.csv")

# Focus on rheumatology
rheumatology_mismatch <- data |> 
  filter(
    !is.na(yeshna_specialty) & 
      str_to_upper(yeshna_specialty) == "RHEUMATOLOGY" & 
      str_to_upper(final_spec) != "RHEUMATOLOGY"
  )
rheumatology_mismatch_list <- rheumatology_mismatch |> 
  select(healthcare_professional, final_name, final_spec, yeshna_name, yeshna_specialty, total_payment)
rheumatology_mismatch |> 
  group_by(final_name, final_spec) |> 
  summarise(total_payment = sum(total_payment, na.rm = TRUE)) |> 
  ungroup() |> 
  select(final_name, final_spec, total_payment)

# Clean specialties and generate matched_full_specialty column
data <- clean_specialties(data)
table(data$final_spec)

# Check name (final_name)
# Check where final_name (from manual searches) and healthcare_professional (from Medicines Australia) are different 
data <- data %>%
  mutate(
    similarity_score = 1 - stringdist(tolower(final_name), tolower(healthcare_professional), method = "jw")
  )
name_mismatch <- data |> 
  filter(final_name != healthcare_professional) |> 
  select(final_name, healthcare_professional, similarity_score, final_spec, total_payment) |> 
  distinct(final_name, .keep_all = TRUE)
View(name_mismatch) 

data <- data %>%
  mutate(
    final_name = if_else(final_name == final_spec, healthcare_professional, final_name)
  ) # above issue corrected 

# We have money paid to "Aggregate" - this is de-identified and so can't be used in this analysis 
data <- data |> 
  filter(final_name != "Aggregate")

# Now we'll see if there's any additional errors in final_name where final_spec was entered 
data <- data %>%
  mutate(
    is_in_final_spec = final_name %in% final_spec
  )
data |> 
  filter(is_in_final_spec == TRUE) |>
  select(final_name, healthcare_professional, final_spec, total_payment) |> 
  head() # Jaclyn White needs to be fixed 

data <- data |> 
  mutate(
    final_name = case_when(
      healthcare_professional == "White, Jaclyn" ~ "White, Jaclyn",
      TRUE ~ final_name
    )
  )
data |> 
  filter(final_name == "White, Jaclyn") |> 
  select(final_name, healthcare_professional, final_spec, total_payment) |> 
  head() # Jaclyn White fixed

# Check healthcare professional type (healthcare_professional_type)
# Check healthcare_professional_type 
table(data$healthcare_professional_type) # these are the categories used by Medicines Australia 
table(data$final_spec) # when we examine results from our manual searches, we see discrepancies 
# e.g. nurse 10840 vs 10158; psychologist 262 vs 204; physio or OT 270 vs 204; scientist 592 vs 53; pharmacist 1716 vs 1613
# all past research (including my own) that has used the Medicines Australia categories will be slightly incorrect - over-counting doctors and under-counting nurses, pharmacists, psychologists, physiotherapists, occupational therapists, and scientists.
# this reinforces our recommendation that Medicines Australia should mandate recording of AHPRA numbers in future reports. 

# Check address (practice_address) 
mismatched_addresses <- data |> 
  group_by(final_name) |> 
  summarise(
    distinct_addresses = n_distinct(practice_address),
    all_addresses = paste(unique(practice_address), collapse = "; ") 
  ) |> 
  filter(distinct_addresses > 1) |> 
  arrange(desc(distinct_addresses)) 
head(mismatched_addresses, 20) # we see Nick Pavlakis has 59 distinct addresses 

data |> # let's search Nick Pavlakis 
  filter(final_name == "Pavlakis, Nick") |> 
  distinct(practice_address) |> 
  print(n= 59)

data |> # let's search John Amerena 
  filter(final_name == "Amerena, John") |> 
  select(final_name, practice_address) |> 
  print(n= 200)

data |> 
  filter(final_name == "Amerena, John") |> 
  select(final_name, healthcare_professional) |> 
  distinct() |>
  head(10) # The Medicines Australia website is not case sensitive but won't return results if you don't have the exact name - this is a problem for John Amerena who has received payments from various companies with 7 slightly different names 

# Check pharmaceutical company names (company_name)
data |> 
  select(company_name) |>
  distinct() |> 
  print(n = 100)

# Map to simplify 
company_name_mapping <- c(
  "AbbVie" = "AbbVie",
  "Shire" = "Shire",
  "Seqirus" = "Seqirus",
  "Servier" = "Servier",
  "Bristol Myers Squibb" = "Bristol Myers Squibb",
  "Bristol-Myers Squibb" = "Bristol Myers Squibb",
  "Pfizer" = "Pfizer",
  "Pfizer Ltd" = "Pfizer",
  "Novartis Pharmaceutical Australia Pty Ltd" = "Novartis",
  "Novartis" = "Novartis",
  "Janssen" = "Janssen",
  "Janssen-Cilag Pty Ltd" = "Janssen",
  "Gilead Sciences" = "Gilead Sciences",
  "Gilead" = "Gilead Sciences",
  "MSD" = "MSD",
  "AstraZeneca" = "AstraZeneca",
  "AstraZeneca Australia" = "AstraZeneca",
  "Bayer" = "Bayer",
  "Bayer Australia" = "Bayer",
  "Merck Sharp & Dohme (Australia) Pty Ltd" = "MSD",
  "Merck Healthcare Pty Ltd" = "MSD",
  "CSL Behring" = "CSL Behring",
  "CSL Vifor" = "CSL Vifor", # I believe CSL acquired Vifor in 2022 so we'll keep both CSL Vifor and Vifor Pharma 
  "Vifor Pharma" = "Vifor Pharma", 
  "Novo Nordisk" = "Novo Nordisk",
  "Boehringer Ingelheim" = "Boehringer Ingelheim",
  "Sanofi" = "Sanofi",
  "Amgen" = "Amgen",
  "Actelion" = "Actelion",
  "Actelion Pharmaceuticals Ltd" = "Actelion",
  "A. Menarini Australia Pty Ltd" = "Menarini",
  "Menarini Australia" = "Menarini",
  "Eli Lilly" = "Eli Lilly",
  "Eli Lilly Australia" = "Eli Lilly",
  "Besins" = "Besins Healthcare",
  "Besins Healthcare" = "Besins Healthcare",
  "UCB Pharma" = "UCB",
  "GlaxoSmithKline" = "GlaxoSmithKline",
  "GlaxoSmithKline Australia Pty Ltd" = "GlaxoSmithKline",
  "Roche" = "Roche",
  "Ipsen" = "Ipsen",
  "Ipsen Pty Ltd" = "Ipsen",
  "Organon Australia" = "Organon",
  "BioMarin Pharmaceutical Australia Pty Ltd" = "BioMarin",
  "Merck Serono" = "Merck",
  "Takeda Pharmaceuticals Australia Pty Ltd" = "Takeda",
  "Takeda" = "Takeda",
  "Biogen" = "Biogen",
  "Biogen Australia" = "Biogen",
  "Eisai" = "Eisai",
  "Vertex Australia" = "Vertex",
  "iNova" = "iNova",
  "Celgene" = "Celgene",
  "Celgene Pty Ltd" = "Celgene",
  "Norgine" = "Norgine",
  "Norgine Pty Ltd" = "Norgine",
  "Alexion Pharmaceuticals Australasia Pty Ltd" = "Alexion",
  "Astellas" = "Astellas",
  "Astellas Pharma" = "Astellas",
  "BeiGene" = "BeiGene",
  "Alcon" = "Alcon",
  "PTC Therapeutics Australia Pty Ltd" = "PTC Therapeutics"
)

company_name_mapping <- setNames(
  company_name_mapping,
  toupper(names(company_name_mapping))
)

data <- data |> 
  mutate(
    standardised_company = company_name_mapping[toupper(company_name)]
  )

unmatched <- data |> 
  filter(is.na(company_name)) |> 
  distinct(standardised_company)
print(unmatched)

data |> 
  select(standardised_company) |>
  distinct() |> 
  print(n = 100)

# Check type of service (type_of_service)
table(data$type_of_service)

# Map to simplify
data <- standardize_service(data)

# Check for unmatched rows again
unmatched <- data |> 
  filter(is.na(standardised_service)) |> 
  distinct(type_of_service)
print(unmatched)

# Check event (type_of_event)
table(data$type_of_event)

# Map to simplify 
data$type_of_event <- str_to_lower(data$type_of_event)
event_mapping <- c(
  # Advisory Board
  "advisory board" = "Advisory Board",
  "advisory board or committee meeting" = "Advisory Board",
  "advisory board / committee member" = "Advisory Board",
  "advisory board/committee meeting" = "Advisory Board",
  "advisory board orcommitteemeeting" = "Advisory Board",
  "advisory board/ committee member" = "Advisory Board",
  "advisory board or committee member" = "Advisory Board",
  "advisory board or committee meeting overseas" = "Advisory Board",
  "advisory board overseas" = "Advisory Board",
  "advisory board meeting" = "Advisory Board",
  "advisory borad or committee meeting" = "Advisory Board",
  "advisory board or committee" = "Advisory Board",
  "advisory board or committee - car-t steakholder meeting" = "Advisory Board",
  "advisory board or committee - mm advisory board (sydney)" = "Advisory Board",
  "advisory board or committee - mm advisory board (melbourne)" = "Advisory Board",
  "committee meeting" = "Advisory Board",
  
  # Company Meeting
  "company meeting" = "Company Meeting",
  "company meeting australia" = "Company Meeting",
  "company meeting overseas" = "Company Meeting",
  "company meeting in australia" = "Company Meeting",
  "company meeting in overseas" = "Company Meeting",
  "company meeting in  australia" = "Company Meeting",
  "compnay meeting in australia" = "Company Meeting",
  "company meeting oversease - apac preceptorship (south korea)" = "Company Meeting",
  "company meeting in australia - new frontiers in haematology, brisbane" = "Company Meeting",
  "company meeting in australia - blood meeting brisbane" = "Company Meeting",
  "company meeting in australia - webex pilot meeting" = "Company Meeting",
  "international company meeting" = "Company Meeting",
  
  # Independent Meeting
  "independent meeting" = "Independent Meeting",
  "independent meeting australia" = "Independent Meeting",
  "independent meeting overseas" = "Independent Meeting",
  "independent meeting in australia" = "Independent Meeting",
  "independent meeting in overseas" = "Independent Meeting",
  "independentmeeting inaustralia" = "Independent Meeting",
  "independentmeeting overseas" = "Independent Meeting",
  "independent virtual meeting overseas" = "Independent Meeting",
  "independent overseas meeting delivered online" = "Independent Meeting",
  "independent meeting overseas (also attended a company meeting immediately before)" = "Independent Meeting",
  "independent meeting overseaseas" = "Independent Meeting",
  "independent meeting oversease" = "Independent Meeting",
  "independent meeting oversease - the european hematology association" = "Independent Meeting",
  "independent meeting australia - hsanz- post eha meeting" = "Independent Meeting",
  "independent meting in australia" = "Independent Meeting",
  "indpenedent meeting in australia" = "Independent Meeting",
  "independent meeting overseas (includes pre-course)" = "Independent Meeting",
  "independentmeeting in australia" = "Independent Meeting",                                    
  
  # Consultant/Consulting Services
  "consulting" = "Consulting",
  "consulting services" = "Consulting",
  "consulting service" = "Consulting",
  "consulting service (slide deck preparation)" = "Consulting",
  "consulting service to training internal staff" = "Consulting",
  "consulting service overseas" = "Consulting",
  "consultant" = "Consulting",
  "consultant service" = "Consulting",
  "consultant meeting" = "Consulting",
  "consultancy" = "Consulting",
  
  
  # Educational Meeting
  "educational meeting" = "Educational Meeting",
  "educational meeting attendee" = "Educational Meeting",
  "educational meeting speaker" = "Educational Meeting",
  "educational meeting speaker or chair person" = "Educational Meeting",
  "educational meeting speaker/chair" = "Educational Meeting",
  "education meeting attendee" = "Educational Meeting",
  "education meeting speaker or chair person" = "Educational Meeting",
  "symposia" = "Educational Meeting",
  "symposium" = "Educational Meeting",
  "congress" = "Educational Meeting",
  "conference" = "Educational Meeting",
  "workshop attendee" = "Educational Meeting",
  "workshop" = "Educational Meeting",
  "medical education" = "Educational Meeting",
  
  # Virtual Meeting
  "virtual meeting" = "Advisory Board",
  "virtual" = "Educational Meeting",
  "virtual event" = "Educational Meeting",
  "webinar" = "Educational Meeting",
  
  # Miscellaneous
  "other" = "Other",
  "cancelled activity (for which payment to a hcp is still required)" = "Other",
  "cancelled activity (for which payment  to a hcp is still required)" = "Other",
  "training for internal staff" = "Other",
  "third party meeting attendee" = "Other",
  "filming work" = "Other",
  "content for educational meetings" = "Other",
  "market research" = "Other",
  "individual hcp sponsorship only (flights, accomodation or registration)" = "Other",
  "not specified" = "Other"
)

data <- data |> 
  mutate(
    standardised_event = event_mapping[type_of_event]
  )

unmatched <- data |> 
  filter(is.na(standardised_event)) |> 
  select(type_of_event) |> 
  distinct()
print(unmatched, n = 50)

# Check payment recipient (payment_made_to)
table(data$payment_made_to)

# Map to simplify 
payment_mapping <- c(
  # Health Care Professional
  "Hcp" = "Health Care Professional",
  "hcp" = "Health Care Professional",
  "HCP" = "Health Care Professional",
  "Health care professional" = "Health Care Professional",
  "Health Care professional" = "Health Care Professional",
  "Health Care Professional" = "Health Care Professional",
  "Health Care Practitioner" = "Health Care Professional",
  "healthcare professional" = "Health Care Professional",
  "Healthcare professional" = "Health Care Professional",
  "Healthcare Professional" = "Health Care Professional",
  "Heathcare professional" = "Health Care Professional",
  "Heathcare Professional" = "Health Care Professional",
  "The Healthcare Professional" = "Health Care Professional",
  "Third party & Healthcare Professional" = "Health Care Professional",
  "Third party and Healthcare Professional" = "Health Care Professional",
  "Third Party And Healthcare Professional" = "Health Care Professional",
  "Health Care Professional, Health Care Professional's Employer" = "Health Care Professional",
  "Healthcare Professional, Healthcare Professional's Employer" = "Health Care Professional",
  "Health Care Professional And Healthcare Professional's Employer" = "Health Care Professional",
  "Healthcare Professional And Healthcare Professional's Employer" = "Health Care Professional",
  "Healthcare Professional And Third Party" = "Health Care Professional",
  "Healthcare Professional Andthird Party" = "Health Care Professional",
  "Third Party HCP" = "Health Care Professional",
  "health care professional" = "Health Care Professional",
  "the healthcare professional" = "Health Care Professional",
  "heathcare professional" = "Health Care Professional",
  "health care practitioner" = "Health Care Professional",
  "health careprofessional" = "Health Care Professional",
  "health care professional's employer  health care professional" = "Health Care Professional",
  "health care professional health care professional's employer" = "Health Care Professional",
  "travel/accommodation - health care professional consultancy - health care professionals employer" = "Health Care Professional",
  "travel - health care professional consultancy - health care professionals employer" = "Health Care Professional",
  "hcp's employer hcp" = "Health Care Professional",
  "hcp  hcp employer" = "Health Care Professional",
  "hcp   hcp employer" = "Health Care Professional",
  "third party and healthcare professional" = "Health Care Professional",
  "third party & healthcare professional" = "Health Care Professional",
  "health care professional, health care professional's employer" = "Health Care Professional",
  "healthcare professional, healthcare professional's employer" = "Health Care Professional",
  "healthcare professional and healthcare professional's employer" = "Health Care Professional",
  "healthcare professionalhealthcare professional's employer" = "Health Care Professional",
  "health care professionals 's employer" = "Health Care Professional",
  "health care professional third party" = "Health Care Professional",
  "hcpthird party" = "Health Care Professional",
  "hcp  third party" = "Health Care Professional",
  "healthcare professional and third party" = "Health Care Professional",
  "healthcare professional andthird party" = "Health Care Professional",
  "company meeting overseas" = "Health Care Professional",                                  
  "health care professionalthird party" = "Health Care Professional",     
  "travel - health care professional" = "Health Care Professional",
  "healthcare professional healthcare professional's employer" = "Health Care Professional",
  "hcp's employerhealt h care professional" = "Health Care Professional",
  # note that payment to HCP means that the full amount or a portion of the payment was made to the HCP
  
  # Employer
  "Hcp's Employer" = "Employer",
  "HCP's Employer" = "Employer",
  "Hcp Employer" = "Employer",
  "HCP Employer" = "Employer",
  "Health Care Professional's employer" = "Employer",
  "Health Care Professional's Employer" = "Employer",
  "healthcare professional's employer" = "Employer",
  "Healthcare Professional's employer" = "Employer",
  "Healthcare Professional's Employer" = "Employer",
  "Heathcare Professional's Employer" = "Employer",
  "Department Of Endocrinology,  Children's Hospital Westmead" = "Employer",
  "Paid to employer" = "Employer", 
  "health care professional's employer" = "Employer",
  "hcp employer" = "Employer",
  "hcp's employer" = "Employer",
  "the healthcare professional's employer" = "Employer",
  "health care professionals employer" = "Employer",
  "healthcare professional's e" = "Employer",
  "healthcare professional's employer" = "Employer",
  "healthcare professionals employer" = "Employer",
  "paid to employer" = "Employer",
  "employer - austin health" = "Employer",
  "department of endocrinology,  children's hospital westmead" = "Employer",
  "health careprofessional'semployer" = "Employer",
  "heathcare professional's employer" = "Employer",
  "health care professional's employerthird party" = "Employer",
  "hcps employer" = "Employer",
  "health care professional 's employer" = "Employer",                     
  "health care professional employer" = "Employer", # note that payment to Employer means the full amount was made to the employer
  
  # Third Party
  "3rd party" = "Third Party",
  "Third party" = "Third Party",
  "Third Party" = "Third Party",
  "Third Party HCP employer" = "Third Party",
  "third party" = "Third Party",
  "third party hcp employer" = "Third Party",
  "third party hcp" = "Third Party",

  # Miscellaneous 
  "Independent Meeting In Australia" = "Health Care Professional",
  "Company Meeting Overseas" = "Health Care Professional",
  "Company Meeting In Australia" = "Health Care Professional",
  "Travel - Health Care Professional" = "Health Care Professional",
  "Travel/Accommodation - Health Care Professional Consultancy - Health Care Professionals Employer" = "Health Care Professional",
  "Consultancy - Health Care Professionals Employer / Travel /Accommodation - Health Care Professional" = "Health Care Professional",
  "consultancy - health care professionals employer / travel /accommodation - health care professional" = "Health Care Professional",
  "independent meeting in australia" = "Health Care Professional",
  "professional" = "Health Care Professional",
  "health care professsionl" = "Health Care Professional",
  "accommodation: health care professional consultancy: health care professionals employer" = "Health Care Professional"
)

data <- data |> 
  mutate(
    payment_made_to = str_trim(payment_made_to),
    payment_made_to = tolower(payment_made_to),
    standardised_payment = payment_mapping[payment_made_to]
  )

unmatched <- data |> 
  filter(is.na(standardised_payment)) |> 
  select(payment_made_to) |> 
  distinct()
print(unmatched, n = 60)

# Final checks
table(data$final_spec)
table(data$healthcare_professional_type)
table(data$standardised_company)
table(data$standardised_service)
table(data$standardised_event)
table(data$standardised_payment)

write_csv(data, "PharmaData_allColumns.csv")

# Checks on rows 
data <- read_csv("PharmaData_MF20241211.csv")
data_final <- read_csv("PharmaData_allColumns.csv") # there are 213 rows missing in PharmaData_allColumns.csv
missing_rows <- anti_join(data, data_final, by = "final_name")

# We need to include these back into the dataset 
missing_rows <- missing_rows |> 
  filter(healthcare_professional != "Aggregate")
data_final_updated <- bind_rows(data_final, missing_rows)
write_csv(data_final_updated, "PharmaData_allColumns_updated.csv") # we now have 91,653, which excludes name == Aggregate 

# Clean up workspace 
rm(list = setdiff(ls(), "data_final_updated"))

# Further checks
data_final_updated <- data_final_updated |> 
  mutate(
    is_in_final_spec = final_name %in% unique(final_spec)
  ) 
data_final_updated |> 
  filter(is_in_final_spec == TRUE) |> 
  print(n = 100) # here the final_spec has been entered into final_name; this needs to be corrected 

# Correction to final_name 
data_final_updated <- data_final_updated %>%
  mutate(
    final_name = case_when(
      is.na(final_name) & is_in_final_spec == TRUE & !is.na(healthcare_professional) ~ healthcare_professional, 
      final_name == "Procter, Kenneth  D" ~ "Procter, Kenneth",
      final_name == "Retallack, Christine, J" ~ "Retallack, Christine",
      final_name == "Speller, Claire F" ~ "Speller, Claire",
      final_name == "Styles, Kathleen" ~ "Styles, Kathleen Isabel",
      TRUE ~ final_name 
    )
  )

# Further corrections to final_name 
data_final_updated <- data_final_updated %>%
  mutate(
    final_name = if_else(
      healthcare_professional %in% c("Chuah, Eunice", "Moodley, Shivani", 
                                     "White, Jaclyn", "Harding, Anton", 
                                     "Larkins, Peter, A"),
      healthcare_professional, final_name))

# Final specialty check 
# We'll flag the rows where our final_spec differs from BOTH yeshna_specialty and matched_full_specialty
data_final_updated2 <- data_final_updated |> 
  filter(
    yeshna_specialty == matched_full_specialty &  
      final_spec != yeshna_specialty &             
      !is.na(yeshna_specialty) &                
      !is.na(matched_full_specialty) & 
      !is.na(final_spec)
  )
write.csv(data_final_updated2, "MISMATCH.csv")

# Correction to final_spec 
data_final_updated <- data_final_updated |> 
  mutate(
    final_spec = case_when(
      final_name == "Tran, Phong T." ~ "ORTHOPAEDIC SURGERY",
      final_name == "Carroll, Lisa Ljubica" ~ "RHEUMATOLOGY",
      final_name == "Malouf, Monique A." ~ "RESPIRATORY AND SLEEP MEDICINE",
      final_name == "Mansberg, Ginni" ~ "GENERAL PRACTICE",
      final_name == "Sorin, Diana" ~ "DERMATOLOGY",
      final_name == "Ciciriello, Sabina" ~ "RHEUMATOLOGY",
      final_name == "Baker, Caroline" ~ "GENERAL SURGERY",
      final_name == "Lowe, Patricia" ~ "DERMATOLOGY",
      final_name == "Mitchell, Paul" ~ "HAEMATOLOGY_ONCOLOGY",
      final_name == "Bortz, Peter" ~ "CARDIOLOGY",
      final_name == "Lee, Liane ." ~ "RHEUMATOLOGY",
      final_name == "Leonardo, Nieves" ~ "RHEUMATOLOGY",
      final_name == "Sachdev, Nisha" ~ "OPHTHALMOLOGY",
      healthcare_professional == "Anderson, Gary" &
        practice_address == "Astley Medical Centre, Shop 5-8 370 Great Western Hwy ST MARYS NSW 2760" ~ "GENERAL PRACTICE",
      TRUE ~ final_spec),
    final_name = case_when(
      healthcare_professional == "Atkins, Andrew" & 
        healthcare_professional_type == "Medical Practitioner" ~ "Atkins, Andrew Malcolm",
      healthcare_professional == "Anderson, Gary" &
        practice_address == "Astley Medical Centre, Shop 5-8 370 Great Western Hwy ST MARYS NSW 2760" ~ "Anderson, Gary Bernard",
      healthcare_professional == "Lee, Joy" &
        practice_address == "Royal Childrens Hospital Genetic Health Services 50 Flemington Road PARKVILLE VIC 3052" ~ "Lee, Joy Yaplito", TRUE ~ final_name))

# Summarise total payments by individual and specialty
top_300 <- data_final_updated |> 
  group_by(final_name, final_spec) |> 
  summarise(
    total_payments = sum(total_payment),
    .groups = "drop") |> 
  arrange(desc(total_payments)) |> 
  slice_head(n = 300) |>           
  mutate(cumulative_payments = cumsum(total_payments)) |> 
  select(final_name, final_spec, total_payments, cumulative_payments) # we need to repeat manual searches so we're 100% on the individuals with highest payments (~ $41m total)
write_csv(top_300, "Top300Payments.csv") # Top300Payments.csv will be the first additional manual search required by A.P. 

# Find NAs in specialty
specialty_NAs <- data_final_updated |> 
  filter(is.na(final_spec)) |> 
  select(final_name, final_spec, practice_address) |> 
  distinct()
write_csv(specialty_NAs, "Specialty_NAs.csv") # Specialty_NAs.csv will be the second additional manual search required by K.H. 
specialty_NAs

# Correct a small typographical error - NURSE should be NURSING 
data_final_updated <- data_final_updated |> 
  mutate(
    final_spec = case_when(
      final_spec %in% c("Nurse", "NURSE", "Nursing") ~ "NURSING",
      final_spec %in% c("Pharmacist", "PHARMACY") ~ "PHARMACY",
      TRUE ~ final_spec  # Keep all other specialties unchanged
    )
  )
table(data_final_updated$final_spec)

# I've realised colorectal surgery is not a registered subspecialty (https://www.medicalboard.gov.au/Registration/Types/Specialist-Registration.aspx) so we'll need to reclassify them as GENERAL SURGERY 
data_final_updated <- data_final_updated |> 
  mutate(
    final_spec = case_when(
      final_spec == "COLORECTAL SURGERY" ~ "GENERAL SURGERY",
      TRUE ~ final_spec
    )
  )
# Save dataset
data_final_updated |> 
  write_csv("PharmaData_final.csv")
data_final_updated <- read.csv("PharmaData_final.csv")

# Misclassification of nurses 
data_final_updated |> 
  filter(final_spec == "NURSING" & healthcare_professional_type == "Medical Practitioner") |> 
  select(final_name, final_spec, healthcare_professional_type) |> 
  distinct() |> 
  View()

data_final_updated |> 
  filter(final_spec == "NURSING") |> 
  select(final_name, final_spec, healthcare_professional_type) |> 
  distinct() |> 
  View()

#### 4. Minimal dataset ####
# Restrict to requires columns 
data <- data_final_updated |> 
  clean_names() |> 
  select(name = final_name, specialty = final_spec, address = practice_address, registration_fees, travel_costs, fees_for_service, total_payment, company = standardised_company, service = standardised_service, event = standardised_event, payment_to = standardised_payment, period = reporting_period)
data <- data |> 
  mutate(
    company = as.character(company),  
    service = as.character(service),
    event = as.character(event),
    payment_to = as.character(payment_to)
  )
table(data$specialty)

# Save cleaned dataset 
write_csv(data, "PharmaData_cleaned.csv")

#### 5. Load data ####
rm(list = ls())
data <- read_csv("PharmaData_cleaned.csv")
attr(data, "spec") <- NULL
attr(data, "problems") <- NULL
glimpse(data)

table(data$specialty)

# Check for missing values
data |> 
  select(specialty, company, service, event, payment_to) |> 
  map(~sum(is.na(.x)))

#### 6. Exploration #### 
# Payment by specialty
data |> 
  group_by(specialty) |> 
  summarise(
    total_payment = sum(total_payment, na.rm = TRUE),
    count_hcp = n_distinct(name)
  ) |> 
  arrange(desc(total_payment)) |> 
  mutate(
    average_payment_per_hcp = total_payment / count_hcp
  ) |> 
  head(10) |> 
  ggplot(aes(x = reorder(specialty, total_payment), y = total_payment, fill = specialty)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(title = "Top 10 Specialties by Total Payments",
       x = "Specialty",
       y = "Total Payments ($ AUD)") +
  theme_minimal()

# Distribution of payments for top specialties
data_summary <- data %>%
  group_by(specialty) %>%
  summarise(
    total_payment = sum(total_payment, na.rm = TRUE),
    count_hcp = n_distinct(name),
    average_payment_per_hcp = total_payment / count_hcp
  ) %>%
  arrange(desc(total_payment)) %>% 
  head(10) 

ridgeline_data <- data %>%
  filter(specialty %in% data_summary$specialty) %>%
  mutate(specialty = factor(specialty, levels = rev(data_summary$specialty)))

ggplot(ridgeline_data, aes(x = total_payment, y = specialty, fill = specialty)) +
  geom_density_ridges(alpha = 0.8, scale = 1) +
  scale_x_continuous(labels = scales::comma) +
  scale_fill_viridis_d(option = "C") +
  coord_cartesian(xlim = c(0, 5000)) + 
  labs(
    title = "Payment Distributions for Top 10 Specialties",
    x = "Total Payments ($ AUD)",
    y = "Specialty"
  ) +
  theme_bw() +
  theme(text = element_text(size = 9,
                            family = "Montserrat"),
        legend.position = "none")

# Companies by specialty 
top_companies_specialties <- data |> 
  group_by(company, specialty) |> 
  summarise(
    total_payment = sum(total_payment, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  arrange(desc(total_payment)) |> 
  slice_max(order_by = total_payment, n = 10)

ggplot(top_companies_specialties, aes(x = reorder(specialty, total_payment), y = total_payment, fill = company)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(title = "Top Companies by Specialty",
       x = "Specialty",
       y = "Total Payments ($ AUD)") +
  theme_minimal()

# Global payments by time
data_cleaned <- data |> 
  mutate(
    period_start = str_extract(period, "^[A-Za-z]+ \\d{4}"), 
    period_date  = dmy(paste0("01 ", period_start)), 
    year_month   = format(period_date, "%Y-%m")    
  ) |> 
  drop_na(period_date) |> 
  group_by(year_month) |> 
  summarise(total_payment = sum(total_payment, na.rm = TRUE)) |> 
  ungroup()

ggplot(data_cleaned, aes(x = as.Date(paste0(year_month, "-01")), y = total_payment)) +
  geom_line(linewidth = 1, colour = "steelblue") +
  geom_point(colour = "steelblue") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") + 
  scale_y_continuous(labels = comma) +
  labs(title = "Payment Trends Over Time",
       x = "Year-Month",
       y = "Total Payments ($ AUD)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # why the drop in 2018? 

# Individual company payments by time 
data_cleaned <- data |> 
  mutate(
    period_start = str_extract(period, "^[A-Za-z]+ \\d{4}"), 
    period_date  = dmy(paste0("01 ", period_start)), 
    year_month   = format(period_date, "%Y-%m")    
  ) |> 
  drop_na(period_date) |> 
  group_by(year_month, company) |>  # Include company in grouping
  summarise(total_payment = sum(total_payment, na.rm = TRUE), .groups = "drop") |> 
  ungroup()

high_payment_companies <- data_cleaned |> 
  filter(total_payment > 1200000) |> 
  pull(company) |> 
  unique()
data_filtered <- data_cleaned |> 
  filter(company %in% high_payment_companies)
ggplot(data_filtered, aes(x = as.Date(paste0(year_month, "-01")), 
                          y = total_payment, 
                          colour = company, 
                          group = company)) +
  geom_line(linewidth = 1) +
  geom_point() +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") + 
  scale_y_continuous(labels = comma) +
  labs(title = "Payment Trends Over Time by Company (>$1m in any period)",
       x = "Year-Month",
       y = "Total Payments ($ AUD)",
       colour = "Company") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # the drop in 2018 doesn't seem to be due to specific companies dropping off, however Servier is one company that stopped being a Medicines Australia member company 

# Servier payments over time 
data_cleaned <- data |> 
  mutate(
    period_start = str_extract(period, "^[A-Za-z]+ \\d{4}"), 
    period_date  = dmy(paste0("01 ", period_start)), 
    year_month   = format(period_date, "%Y-%m")    
  ) |> 
  drop_na(period_date) |> 
  filter(company == "Servier") |> 
  group_by(year_month) |> 
  summarise(total_payment = sum(total_payment, na.rm = TRUE)) |> 
  ungroup()
ggplot(data_cleaned, aes(x = as.Date(paste0(year_month, "-01")), y = total_payment)) +
  geom_line(linewidth = 1, colour = "red") +
  geom_point(colour = "red") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") + 
  scale_y_continuous(labels = comma) +
  labs(title = "Servier Payment Trends Over Time",
       x = "Time",
       y = "Total Payments ($ AUD)") +
  theme_minimal() # no payments after this date 

# Servier payment total 
data |> 
  filter(company == "Servier") |> 
  summarise(total_payment = sum(total_payment, na.rm = TRUE))

# Top earners 
top_earners <- data |> 
  group_by(name, specialty) |> 
  summarise(total_payment = sum(total_payment, na.rm = TRUE), .groups = "drop") |> 
  arrange(desc(total_payment)) |> 
  head(10)

ggplot(top_earners, aes(x = reorder(name, total_payment), y = total_payment, fill = specialty)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(title = "Top 10 Healthcare Professionals by Payments",
       x = "Healthcare Professional",
       y = "Total Payments ($)") +
  theme_minimal()

# Top companies 
company_contribution <- data |> 
  group_by(company) |> 
  summarise(total_payment = sum(total_payment, na.rm = TRUE), .groups = "drop") |> 
  arrange(desc(total_payment)) |> 
  mutate(cum_percentage = cumsum(total_payment) / sum(total_payment) * 100)

head(company_contribution, 10) |> 
  ggplot(aes(x = reorder(company, total_payment), y = total_payment)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(title = "Top Companies by Payments",
       x = "Company",
       y = "Total Payments ($)") +
  theme_minimal()

# Top services 
data |> 
  filter(!is.na(service)) |>  # Exclude rows with NA in the event column
  group_by(service) |> 
  summarise(
    total_payment = sum(total_payment, na.rm = TRUE),
    count_hcp = n_distinct(name[!is.na(name)]),  # Exclude NA names when counting
    avg_payment = ifelse(count_hcp > 0, total_payment / count_hcp, 0)  # Avoid division by 0
  ) |> 
  filter(total_payment > 0) |>  # Remove zero payment rows (small no. of these)
  arrange(desc(total_payment)) |> 
  head(10) |> 
  ggplot(aes(x = reorder(service, total_payment), y = total_payment, fill = service)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(title = "Top 10 Service Types by Total Payments",
       x = "Sevice Type",
       y = "Total Payments ($)") +
  theme_minimal()

# Top events 
data |> 
  filter(!is.na(event)) |>  # Exclude rows with NA in the event column
  group_by(event) |> 
  summarise(
    total_payment = sum(total_payment, na.rm = TRUE),
    count_hcp = n_distinct(name[!is.na(name)]),  # Exclude NA names when counting
    avg_payment = ifelse(count_hcp > 0, total_payment / count_hcp, 0)  # Avoid division by 0
  ) |> 
  filter(total_payment > 0) |>  # Remove zero payment rows
  arrange(desc(total_payment)) |> 
  head(10) |> 
  ggplot(aes(x = reorder(event, total_payment), y = total_payment, fill = event)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(title = "Top 10 Event Types by Total Payments",
       x = "Event Type",
       y = "Total Payments ($)") +
  theme_minimal()

# Total payments to unique HCPs per company
data |> 
  group_by(company) |> 
  summarise(
    total_payment = sum(total_payment, na.rm = TRUE),
    unique_hcps = n_distinct(name)
  ) |> 
  ggplot(aes(x = unique_hcps, y = total_payment, label = company)) +
  geom_point(alpha = 0.7, color = "blue") +
  geom_text(vjust = 1.5, hjust = 1.5, size = 3) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(labels = comma) +
  labs(title = "Total Payments vs Unique HCPs per Company",
       x = "Number of Unique HCPs",
       y = "Total Payments ($)") +
  theme_minimal() # Bayer and Servier are outliers 

# Total payments to unique HCPs per company (restricted to those who made payments > $1m)
summarised_data <- data |> 
  group_by(company) |> 
  summarise(
    total_payment = sum(total_payment, na.rm = TRUE),
    unique_hcps = n_distinct(name)
  )
summarised_data |> 
  ggplot(aes(x = unique_hcps, y = total_payment, label = company)) +
  geom_point(alpha = 0.7, color = "blue") +
  geom_text_repel(
    data = summarised_data |> filter(total_payment > 1000000),  
    aes(label = company)
  ) + 
  scale_y_continuous(labels = comma) +
  scale_x_continuous(labels = comma) +
  labs(
    title = "Total Payments vs Unique HCPs per Company (> $1m in payments)",
    x = "Number of Unique HCPs",
    y = "Total Payments ($)"
  ) +
  theme_minimal() # These are companies that gave > $1m in total payments

# Total payment by specialty
data |> 
  group_by(specialty) |> 
  summarise(
    total_payment = sum(total_payment, na.rm = TRUE),
    unique_hcps = n_distinct(name)  # Number of unique HCPs per specialty
  ) |> 
  ggplot(aes(x = unique_hcps, y = total_payment, label = specialty)) +
  geom_point(alpha = 0.7, color = "green") +
  geom_text(vjust = 1.5, hjust = 1.5, size = 3) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(labels = comma) +
  labs(
    title = "Total Payments vs Unique HCPs by Specialty",
    x = "Number of Unique HCPs",
    y = "Total Payments ($)"
  ) +
  theme_minimal() # this looks like a mess so we'll log transform 

# Plot with log transformation
data_summary <- data |>
  group_by(specialty) |>
  summarise(
    unique_hcps = log(n_distinct(name)),           
    total_payment = log(sum(total_payment, na.rm = TRUE))  
  ) |>
  ungroup()

specialties_to_label <- c(
  "OPHTHALMOLOGY", 
  "GASTROENTEROLOGY AND HEPATOLOGY", 
  "NEUROLOGY", 
  "RESPIRATORY AND SLEEP MEDICINE", 
  "NURSING", 
  "RHEUMATOLOGY", 
  "ENDOCRINOLOGY", 
  "GENERAL PRACTICE", 
  "CARDIOLOGY", 
  "HAEMATOLOGY_ONCOLOGY"
)

data_summary <- data_summary |>
  mutate(label = ifelse(specialty %in% specialties_to_label, specialty, NA))

num_labels <- length(specialties_to_label)
dark_pastel_palette <- brewer.pal(n = min(8, num_labels), "Dark2")  
if (num_labels > 8) {
  dark_pastel_palette <- colorRampPalette(brewer.pal(8, "Dark2"))(num_labels)
}

data_summary <- data_summary |>
  mutate(color = ifelse(specialty %in% specialties_to_label, dark_pastel_palette[match(specialty, specialties_to_label)], "darkblue"))

ggplot(data_summary, aes(x = unique_hcps, y = total_payment)) +
  geom_point(aes(color = color), size = 2) +
  geom_text_repel(aes(label = label, color = color), size = 3.5, max.overlaps = 15, show.legend = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "powderblue", linetype = "dashed", alpha = 0.5) + 
  scale_color_identity() +  
  labs(
    title = "Log-Transformed Total Payments vs Unique HCPs",
    x = "Log(Number of Unique HCPs)",
    y = "Log(Total Payments)"
  ) +
  theme_minimal()

#### 7. Analysis ####
# Read in data on specialty numbers in Australia in 2023/24 to obtain per capita payment estimates 
# R9 of Ahpra---Annual-report-2023-24---Registration-supplementary-tables.xlsx 
file_path <- "Ahpra---Annual-report-2023-24---Registration-supplementary-tables.XLSX"
table_r9 <- read_excel(file_path, sheet = "R9", col_names = FALSE)
ahpra_clean <- table_r9 |> 
  select(Profession = 1, `Total 2023/24` = 11) |> 
  rename(specialty = Profession, total = `Total 2023/24`) |>
  filter(!is.na(specialty)) |> 
  slice(-1, -2, -116, -117)
View(ahpra_clean)
# Check for duplicates 
ahpra_clean |> 
  group_by(specialty) |> 
  summarise(count = n()) |> 
  filter(count > 1) 
# Clean 
ahpra_clean <- ahpra_clean %>%
  mutate(
    specialty = str_to_upper(specialty),
    specialty = case_when(
      specialty == "CLINICAL GENETICS" & total == 66 ~ "PAEDIATRIC CLINICAL GENETICS",
      specialty == "HAEMATOLOGY" ~ "HAEMATOLOGY_ONCOLOGY",
      specialty == "MEDICAL ONCOLOGY" ~ "HAEMATOLOGY_ONCOLOGY",
      specialty == "DENTAL PRACTITIONER" ~ "DENTISTRY",
      specialty == "PODIATRIST" ~ "PODIATRY",
      specialty == "CARDIO-THORACIC SURGERY" ~ "CARDIOTHORACIC SURGERY",
      specialty == "OCCUPATIONAL AND ENVIRONMENTAL MEDICINE" ~ "OCCUPATIONAL MEDICINE",
      specialty == "PAEDIATRIC HAEMATOLOGY" ~ "PAEDIATRIC HAEMATOLOGY_ONCOLOGY",
      specialty == "PAEDIATRIC MEDICAL ONCOLOGY" ~ "PAEDIATRIC HAEMATOLOGY_ONCOLOGY",
      TRUE ~ specialty
    )
  )

ahpra_clean <- ahpra_clean |> 
  mutate(total = as.numeric(total))
ahpra_clean <- ahpra_clean |> # from R3 of the spreadsheet 
  bind_rows(
    tibble(specialty = "DOCTOR-IN-TRAINING", total = 49907),
    tibble(specialty = "DENTISTRY", total = 27583),
    tibble(specialty = "NURSING", total = 477822),
    tibble(specialty = "RADIOGRAPHY", total = 19851),
    tibble(specialty = "PHYSIOTHERAPY OR OCCUPATIONAL THERAPY", total = 76942),
    tibble(specialty = "OPTOMETRY OR ORTHOPTICS", total = 7051),
    tibble(specialty = "PHARMACY", total = 38610),
    tibble(specialty = "PODIATRY", total = 6135),
    tibble(specialty = "PSYCHOLOGY", total = 48240),
    tibble(specialty = "PARAMEDICINE", total = 25345)
  ) # social workers, dieticians and speech pathologists are not registered with AHPRA 

ahpra_clean <- ahpra_clean |> 
  group_by(specialty) |> 
  summarise(total = sum(total, na.rm = TRUE), .groups = "drop") # this groups haematologists from RACP and RCPA as well as medical oncologists 

# Obtain 'per capita' payments and payment coverage (how many specialists received money / total specialists at 2023/24)
merged_data <- data %>%
  group_by(specialty) %>%
  summarise(
    total_payment = sum(total_payment, na.rm = TRUE),  
    unique_recipients = n_distinct(name)              
  ) %>%
  left_join(ahpra_clean, by = "specialty") %>%       
  mutate(
    average_payment = total_payment / total,          
    payment_coverage = (unique_recipients / total) * 100
  ) %>%
  arrange(desc(average_payment)) 
View(merged_data) 

# Summary data for professional groups
table(data$specialty)
data <- data |> 
  mutate(
    specialty_group = case_when(
      specialty == "NURSING" ~ "Nursing",
      specialty == "DENTISTRY" ~ "Dentistry",
      specialty == "DIETETICS" ~ "Dietetics",
      specialty == "EXERCISE PHYSIOLOGY" ~ "Exercise Physiology",
      specialty %in% c("OPTOMETRY", "ORTHOPTICS", "OPTOMETRY OR ORTHOPTICS") ~ "Optometry or Orthoptics",
      specialty == "PARAMEDICINE" ~ "Paramedicine",
      specialty == "PHARMACY" ~ "Pharmacy",
      specialty %in% c("PHYSIOTHERAPY", "OCCUPATIONAL THERAPY", "PHYSIOTHERAPY OR OCCUPATIONAL THERAPY") ~ "Physiotherapy or Occupational Therapy",
      specialty == "PODIATRY" ~ "Podiatry",
      specialty == "PSYCHOLOGY" ~ "Psychology",
      specialty == "RADIOGRAPHY" ~ "Radiography",
      specialty == "SCIENTIST" ~ "Scientist",
      specialty == "SOCIAL WORK" ~ "Social Work",
      specialty == "SPEECH PATHOLOGY" ~ "Speech Pathology",
      specialty == "VETERINARY MEDICINE" ~ "Veterinary Medicine",
      TRUE ~ "Medical Practitioners" # All other categories
    )
  )

# Summary data for professional groups
# Sum all total_payment 
data |> 
  summarise(
    total_payment = sum(total_payment, na.rm = TRUE))
# Percentage to each group 
data |> 
  group_by(specialty_group) |> 
  summarise(
    total_payment = sum(total_payment, na.rm = TRUE),
    count_hcp = n_distinct(name),
    average_payment_per_hcp = total_payment / count_hcp
  ) |> 
  arrange(desc(total_payment)) |> 
  mutate(
    percentage_of_total = total_payment / sum(total_payment) * 100
  ) |> 
  head(10)
# Median and IQR for each group 
data %>%
  group_by(specialty) %>%
  summarise(
    median_payment = median(total_payment, na.rm = TRUE),
    IQR_payment = IQR(total_payment, na.rm = TRUE)
  ) |> 
  arrange(desc(median_payment)) |>
  print(n = 75)
# Get top 300 by total_payments
top_300 <- data |> 
  group_by(name, specialty) |> 
  summarise(
    total_payments = sum(total_payment),
    .groups = "drop") |> 
  arrange(desc(total_payments)) |> 
  slice_head(n = 300) |>           
  mutate(cumulative_payments = cumsum(total_payments)) |> 
  select(name, specialty, total_payments, cumulative_payments) 
View(top_300)

# Summary of service 
data |> 
  group_by(service) |> 
  summarise(
    total_payment = sum(total_payment, na.rm = TRUE),
    count_hcp = n_distinct(name),
    average_payment_per_hcp = total_payment / count_hcp
  ) |> 
  arrange(desc(total_payment)) |> 
  mutate(
    percentage_of_total = total_payment / sum(total_payment) * 100
  ) |> 
  head(10)

#### 8. Figures #### 
# Figure: Payments by major companies over study period 
font_add_google("Montserrat")
showtext_auto()
data_cleaned <- data |> 
  mutate(
    period_start = str_extract(period, "^[A-Za-z]+ \\d{4}"), 
    period_date  = dmy(paste0("01 ", period_start)), 
    year_month   = format(period_date, "%Y-%m")    
  ) |> 
  drop_na(period_date) |> 
  group_by(year_month, company) |> 
  summarise(total_payment = sum(total_payment, na.rm = TRUE), .groups = "drop") |> 
  ungroup()

high_payment_companies <- data_cleaned |> 
  filter(total_payment > 1200000) |> 
  pull(company) |> 
  unique()
data_filtered <- data_cleaned |> 
  filter(company %in% high_payment_companies)
ggplot(data_filtered, aes(x = as.Date(paste0(year_month, "-01")), 
                          y = total_payment, 
                          colour = company, 
                          group = company)) +
  geom_line(linewidth = 1) +
  geom_point() +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") + 
  scale_y_continuous(labels = comma) +
  scale_color_bmj() + 
  labs(title = "Payment Trends Over Time by Major Pharmaceutical Company",
       x = "Year-Month",
       y = "Total Payments ($ AUD)",
       colour = "Company") +
  theme(
    text = element_text(size = 10, family = "Montserrat"), 
    axis.text.x = element_text(angle = 45, hjust = 1))

# Figure: Top 10 groups by total payments 
data <- data |> 
  mutate(
    specialty = case_when(
      specialty == "HAEMATOLOGY_ONCOLOGY" ~ "HAEMATOLOGY AND ONCOLOGY",
      TRUE ~ specialty # Keep all other specialties unchanged
    )
  )
data |> 
  group_by(specialty) |> 
  summarise(
    total_payment = sum(total_payment, na.rm = TRUE),
    count_hcp = n_distinct(name)
  ) |> 
  arrange(desc(total_payment)) |> 
  mutate(
    average_payment_per_hcp = total_payment / count_hcp
  ) |> 
  head(8) |> 
  ggplot(aes(x = reorder(specialty, total_payment), y = total_payment, fill = specialty)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  scale_fill_bmj() + 
  theme_bw() + 
  theme(text = element_text(size = 10, family = "Montserrat")) + 
  labs(title = "Top 10 Specialties by Total Payments",
       x = "Profession",
       y = "Total Payments ($ AUD)") +
  theme_minimal()

# Figure: Log-transformed payments to specialties 
font_add_google("Montserrat")
showtext_auto()

hcp_summary <- data |>
  group_by(specialty, name) |>
  summarise(
    total_payment_per_hcp = sum(total_payment, na.rm = TRUE),
    .groups = "drop"
  )

data_summary <- hcp_summary |>
  group_by(specialty) |>
  summarise(
    unique_hcps = n_distinct(name),
    total_payment = sum(total_payment_per_hcp, na.rm = TRUE),
    median_payment_per_hcp = median(total_payment_per_hcp, na.rm = TRUE)
  ) |>
  mutate(
    unique_hcps_log = unique_hcps,  
    total_payment_log = log(total_payment), 
    median_payment_per_hcp_log = log(median_payment_per_hcp)
  ) |>
  ungroup()

data_summary <- data_summary |>
  mutate(
    specialty = case_when(
      specialty == "HAEMATOLOGY_ONCOLOGY" ~ "HAEMATOLOGY AND ONCOLOGY",  
      TRUE ~ specialty
    )
  )

specialties_to_label <- c(
  "OPHTHALMOLOGY", 
  "GASTROENTEROLOGY AND HEPATOLOGY", 
  "NEUROLOGY", 
  "RESPIRATORY AND SLEEP MEDICINE", 
  "NURSING", 
  "RHEUMATOLOGY", 
  "ENDOCRINOLOGY", 
  "GENERAL PRACTICE", 
  "CARDIOLOGY", 
  "HAEMATOLOGY AND ONCOLOGY"
)

dark_pastel_palette <- if (length(specialties_to_label) > 8) {
  colorRampPalette(brewer.pal(8, "Dark2"))(length(specialties_to_label))
} else {
  brewer.pal(min(8, length(specialties_to_label)), "Dark2")
}

data_summary <- data_summary |>
  mutate(
    label = ifelse(specialty %in% specialties_to_label, specialty, NA),
    color = ifelse(specialty %in% specialties_to_label, 
                   dark_pastel_palette[match(specialty, specialties_to_label)], 
                   "darkblue")
  )

ggplot(data_summary, aes(x = median_payment_per_hcp_log, y = total_payment_log)) +
  geom_point(aes(size = unique_hcps, color = color), alpha = 0.7) +  
  geom_text_repel(aes(label = label), size = 3.5, max.overlaps = 10, show.legend = FALSE) +
  geom_smooth(method = "lm", se = FALSE, color = "powderblue", linetype = "dashed", alpha = 0.5) +
  scale_size_continuous(
    range = c(1, 25), 
    name = "Unique Clinicians Receiving Payment", 
    breaks = c(1, 10, 100, 1000, 5000)
  ) +  
  scale_color_identity() + 
  labs(
    title = "Total Payments to each Profession vs Median Payment per Clinician",
    x = "Median Payment per Clinician (log scale)",
    y = "Total Payments (log scale)"
  ) +
  theme_bw() + 
  theme(
    text = element_text(size = 10, family = "Montserrat"),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10)
  )

#### 9. Updates #### 
# Update 20 January 2025 - AH completed 500 random searches within AHPRA +/- Google and found 5% (25/500) misclassification 
# Following corrections are required: 
corrections <- c("Anjara, Pamela" = "RHEUMATOLOGY",
                 "Banerji, Jayant" = "GENERAL PRACTICE",
                 "Bosman, Hermanus" = "GENERAL MEDICINE",
                 "Burke, Michael" = "GENERAL PRACTICE",
                 "Cao, Christopher" = "CARDIOTHORACIC SURGERY",
                 "Chan, Derek" = "OPHTHALMOLOGY",
                 "Corry, June F." = "RADIATION ONCOLOGY",
                 "D'udekem D'acoz, Yves" = "CARDIOTHORACIC SURGERY",
                 "Fuller, Kim" = "NURSING",
                 "Gibson, David J" = "GENERAL PRACTICE",
                 "Goh, Paul" = "GENERAL MEDICINE",
                 "Harvey, Yasmin J" = "PATHOLOGY",
                 "Hurst, Richard" = "GENERAL MEDICINE",
                 "Kannangara, Siri N" = "SPORT AND EXERCISE MEDICINE",
                 "Lau, Hui P" = "GENERAL PRACTICE",
                 "McIntyre, David" = "ENDOCRINOLOGY",
                 "Mckay, Michael" = "OPHTHALMOLOGY",
                 "Puno, Josefina" = "GENERAL MEDICINE",
                 "Rogers, John R" = "PSYCHIATRY",
                 "Soong, Mei-Min" = "GENERAL MEDICINE",
                 "Ssentamu, Michael" = "RESPIRATORY AND SLEEP MEDICINE",
                 "Stewart, Richard" = "GENERAL MEDICINE",
                 "Tan, Roger" = "PAIN MEDICINE",
                 "Tomasevic, Milivoje" = "GENERAL MEDICINE",
                 "Wong, Caroline" = "DOCTOR-IN-TRAINING")
data$specialty <- ifelse(data$name %in% names(corrections), corrections[data$name], data$specialty)

# Get name == "O'Brien, Richard C."
data |> 
  filter(name == "O'Brien, Richard C.") |> 
  select(name, specialty, total_payment) |> 
  print(n = 100)

# Get name starts with "O'Brien, Richard"
data |> 
  filter(str_detect(name, "O'Brien, Richard")) |> 
  select(name, specialty, total_payment) |> 
  print(n = 120)

# Get full details for all "Ho, Kenneth"
data |> 
  filter(str_detect(name, "Ho, Kenneth")) |> 
  select(name, specialty, total_payment, address) |> 
  print(n = 120) |> 
  # sum total 
  summarise(
    total_payment = sum(total_payment, na.rm = TRUE)
  )

#### 10. Notes ####
library("ggsci")
library("ggplot2")
library("gridExtra")
library("showtext")
font_add_google("Montserrat")
showtext_auto()
data("diamonds")
p1 <- ggplot(
  subset(diamonds, carat >= 2.2),
  aes(x = table, y = price, colour = cut)
) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "loess", alpha = 0.05, linewidth = 1, span = 1) +
  theme_bw() +
  theme(text = element_text(size = 9,
                            family = "Montserrat"))
p2 <- ggplot(
  subset(diamonds, carat > 2.2 & depth > 55 & depth < 70),
  aes(x = depth, fill = cut)
) +
  geom_histogram(colour = "black", binwidth = 1, position = "dodge") +
  theme_bw() + 
  theme(text = element_text(size = 9,
                            family = "Montserrat"))
p1_bmj <- p1 + scale_color_bmj()
p2_bmj <- p2 + scale_fill_bmj()
grid.arrange(p1_bmj, p2_bmj, ncol = 2)

# Malcolm Forbes 
# 26 February 2025
# Pharmaceutical company payments to Australian healthcare professionals

# PharmaData2_MF20250226.R - this script incorporates random checks completed by Ashleigh Hooimeyer on the Oct 15 to Oct 23 dataset 
#### 1. Libraries ####
library(tidyverse)
library(readxl)

#### 2. Import ####
df_randomcheck <- read_excel("Randomcheck_AH20250210.xlsx", guess_max = 100000) # Ashleigh's dataset with new columns - new_name, new_specialty, correction_reason
df_pharma <- read_csv("PharmaData_cleaned.csv")
glimpse(df_randomcheck)
glimpse(df_pharma)

df_randomcheck |> 
  filter(!is.na(new_name))

#### 3. Check mismatches ####
df_randomcheck |> 
  filter(new_name != name) |> 
  distinct(name, new_name) # 169 cases where names did not match 

df_randomcheck |> 
  filter(new_specialty != specialty) |> 
  distinct(specialty, new_specialty) # 77 cases where specialty did not match 

#### 4. Correct mismatches ####
df_randomcheck <- df_randomcheck |> 
  mutate(
    name = if_else(!is.na(new_name), new_name, name),
    specialty = if_else(!is.na(new_specialty), new_specialty, specialty)
  ) %>%
  select(-new_name, -new_specialty, -correction_reason)

#### 5. Export new dataset ####
write_csv(df_randomcheck, "PharmaData_cleaned2.csv")

