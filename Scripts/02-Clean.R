# =================
# Cleaning datasets
# =================

# Author: Roxanne Beauclair

# Description: This script cleans the raw dataset from the Needs Assessment,
# removes excess variables that won't be useful for the analysis,
# and saves cleaned datasets for subsequent analyses

# ===================
# Relative file paths
# ===================
wd <- getwd()
rdata <- paste0(wd, "/Data/Raw") 
cdata <- paste0(wd, "/Data/Cleaned")
script <- paste0(wd, "/Scripts")
importdf <- paste0(cdata, "/01-imported.rda")
cleandf <- paste0(cdata, "/02-cleaned.rda")
fxn <- paste0(script, "/00-Functions.R")

# ====================
# Loading dependencies
# ====================
source(fxn)

InstallLoad("tidyverse")

# =============
# Load datasets
# =============
load(importdf)

# =================================
# Remove those that did not consent
# =================================
df1 <- df %>%
  filter((consent_1 == "Yes" | consent_1 == "Yes, Yes") & 
           (consent_2 == "Yes" | consent_2 == "Yes, Yes"))


# ====================================
# Remove irrelevant variables
# ====================================

df2 <- df1 %>%
  select(-consent_1:-confirm_consent_2, 
         -firstname,
         -surname,
         -identify_people:-person_email_5_1)
  


# ==========================
# Rename long variable names
# ==========================

df3 <- df2 %>%
  rename(time_org = time_organization,
         org_dataman = organization_dataman,
         org_dataman_whynot = organization_dataman_whynot,
         org_datavis = organization_datavis,
         org_datavis_whynot = organization_datavis_whynot,
         org_stats = organization_stats,
         org_stats_whynot = organization_stats_whynot,
         org_model = organization_model,
         org_model_whynot = organization_model_whynot,
         model_collab_loc = modelling_collaborators_location,
         model_collab_org = modelling_collaborators_organization,
         model_collab_fund = modelling_collaborators_funding,
         model_collab_unfund = modelling_collaborators_unfunded,
         analysis_collab_loc = analysis_collaborators_location,
         analysis_collab_org = analysis_collaborators_organization,
         analysis_collab_fund = analysis_collaborators_funding,
         analysis_collab_unfund = analysis_collaborators_unfunded,
         quant_skills_org = quant_skills_organization,
         pref_collab_loc = preference_collaborator_location,
         pref_collab_loc_why = preference_collaborator_location_why,
         aware_other_orgs = aware_other_organizations,
         other_orgs = other_organizations,
         cofund_research_org = cofund_research_organization,
         host_org = host_organization,
         other_collabs = other_collaborations)


# =============================
# Recode some character vectors
# =============================

df4 <- df3 %>%
  mutate(highest_degree = fct_recode(highest_degree,
                                     `Doctoral (e.g. PhD, DPhil, etc.)` = "Doctoral (e.g. PhD, DPhil, etc.), Doctoral (e.g. PhD, DPhil, etc.)",
                                     `Medical fellowship` = "Fellow of the College of Pathology, Colleges of Medicine of South Africa"),
         affiliation_type = fct_recode(affiliation_type,
                                       Academic = "Academic, Academic"),
         role = fct_recode(role,
                           `Researcher/Lecturer` = "Researcher/Lecturer, Researcher/Lecturer"),
         time_org = as.numeric(fct_recode(time_org,
                               `28` = "28, 28")),
         sector = fct_recode(sector,
                             Research = "Research, Research",
                             `Surveillance and Response` = "communicable disease surveillance and respons",
                             `Surveillance and Response` = "surveillance"),
         time_publichealth = as.numeric(fct_recode(time_publichealth,
                                                   `25` = "25, 24")),
         org_model_whynot = fct_collapse(org_model_whynot,
                                     `Lack of adequate skills` = c("Economic modeling is a challenge and no in-house expertise",
                                                                   "lack of adequate skills",
                                                                   "Lack of sufficiently skilled and experienced personnel"),
                                     `Other modelling partnerships` = c("Partnership with other modellers")),
         recent_modelling_whynot = fct_recode(recent_modelling_whynot,
                                              Other = "I still have to learn modelling"),
         meed_needs_how = fct_recode(meet_needs_how,
                                     `Formal contract-based external service provision` = "Formal contract-based external service provision (i.e. fee-for-service)",
                                     `Funded external collaboration` = "Funded external collaboration (i.e. funding source external to both organizations and jointly applied for"),
         affiliation = fct_collapse(affiliation,
                                `Sefako Makatho University` = c("Sefako Makgatho  Health Sciences university",
                                                                "Sefako Makgatho Health Sciences University",
                                                                "Teaching the question not clear"),
                                `University of Cape Town` = c("Unviersity of Cape Town",
                                                              "UCT"),
                                `WHRI Ezintsha` = c("Ezintsha Wits RHI", 
                                                    "Wits Ezintsha"),
                                NICD = c("Centre for HIV and STIs")))


# =========================================
# Create new vars out of checkbox questions
# =========================================

df5 <- df4 %>%
  mutate(bureau_barriers_modelling = ifelse(str_detect(recent_modelling_whynot,
                                                       "Bureaucratic barriers"),
                                            "Yes", "No"),
         sufficient_capacity_modelling = ifelse(str_detect(recent_modelling_whynot,
                                                            "Sufficient in-house capacity"),
                                                 "Yes", "No"),
         no_org_modelling = ifelse(str_detect(recent_modelling_whynot,
                                              "Unable to identify external organization that could meet need"),
                                   "Yes", "No"),
         working_difficult_modelling = ifelse(str_detect(recent_modelling_whynot,
                                                         "Working with modellers is difficult"),
                                              "Yes", "No"),
         no_funding_modelling = ifelse(str_detect(recent_modelling_whynot,
                                                  "Insufficient funding to support collaborations"),
                                       "Yes", "No"),
         not_injob_modelling = ifelse(str_detect(recent_modelling_whynot,
                                                 "not within my job description"),
                                      "Yes", "No"),
         bureau_barriers_analysis = ifelse(str_detect(recent_analysis_whynot,
                                                      "Bureaucratic barriers"),
                                           "Yes", "No"),
         sufficient_capacity_analysis = ifelse(str_detect(recent_analysis_whynot,
                                                           "Sufficient in-house capacity"),
                                                "Yes", "No"),
         no_org_analysis = ifelse(str_detect(recent_analysis_whynot,
                                              "Unable to identify external organization that could meet need"),
                                   "Yes", "No"),
         working_difficult_analysis = ifelse(str_detect(recent_analysis_whynot,
                                                         "Working with modellers is difficult"),
                                              "Yes", "No"),
         no_funding_analysis = ifelse(str_detect(recent_analysis_whynot,
                                                  "Insufficient funding to support collaborations"),
                                       "Yes", "No"),
         not_injob_analysis = ifelse(str_detect(recent_analysis_whynot,
                                                 "not within my job description"),
                                      "Yes", "No"),
         alc = ifelse(str_detect(health_areas, "Alcohol and substance abuse"),
                      "Yes", "No"),
         amr = ifelse(str_detect(health_areas, "Anti-microbial resistance"),
                      "Yes", "No"),
         biosecurity = ifelse(str_detect(health_areas, "Biosecurity"),
                      "Yes", "No"),
         cancer = ifelse(str_detect(health_areas, "Cancer"),
                      "Yes", "No"),
         diabetes = ifelse(str_detect(health_areas, "Diabetes"),
                      "Yes", "No"),
         emergency = ifelse(str_detect(health_areas, "Emergency medicine"),
                      "Yes", "No"),
         food_safety = ifelse(str_detect(health_areas, "Food safety"),
                      "Yes", "No"),
         health_systems = ifelse(str_detect(health_areas, "Health systems research and planning"),
                      "Yes", "No"),
         hiv = ifelse(str_detect(health_areas, "HIV and STIs"),
                      "Yes", "No"),
         hypertension = ifelse(str_detect(health_areas, "Hypertension and other cardiovascular diseases"),
                      "Yes", "No"),
         injury = ifelse(str_detect(health_areas, "Injury and violence prevention"),
                               "Yes", "No"),
         maternal = ifelse(str_detect(health_areas, "Maternal and reproductive health"),
                               "Yes", "No"),
         mental = ifelse(str_detect(health_areas, "Mental Health"),
                               "Yes", "No"),
         nutrition = ifelse(str_detect(health_areas, "Nutrition and hunger elimination"),
                               "Yes", "No"),
         occupational = ifelse(str_detect(health_areas, "Occupational Health"),
                               "Yes", "No"),
         outbreak = ifelse(str_detect(health_areas, "Outbreak/epidemic response"),
                               "Yes", "No"),
         tb = ifelse(str_detect(health_areas, "Tuberculosis"),
                           "Yes", "No"),
         vaccine = ifelse(str_detect(health_areas, "Vaccine-preventable diseases"),
                           "Yes", "No"),
         vector = ifelse(str_detect(health_areas, "Vector-borne and zoonotic infections"),
                           "Yes", "No"),
         bioinformatics = ifelse(str_detect(modelling_applications, "Bioinformatics"),
                         "Yes", "No"), 
         me = ifelse(str_detect(modelling_applications, "Evaluation and assessment / Monitoring & Evaluation"),
                                 "Yes", "No"), 
         forecasting = ifelse(str_detect(modelling_applications, "Forecasting of future public health events"),
                                 "Yes", "No"), 
         transmission = ifelse(str_detect(modelling_applications, "Fundamental investigation of transmission dynamics"),
                                 "Yes", "No"), 
         measurement = ifelse(str_detect(modelling_applications, "Improving diagnostics and/ or the measurement and interpretation of biomarkers"),
                                 "Yes", "No"), 
         surveillance = ifelse(str_detect(modelling_applications, "Improving population-based surveillance"),
                                 "Yes", "No"), 
         rapid_response = ifelse(str_detect(modelling_applications, "Responding quickly to emerging disease threats"),
                                 "Yes", "No"), 
         risk_assessment = ifelse(str_detect(modelling_applications, "Risk assessment"),
                                 "Yes", "No"), 
         scenario_analysis = ifelse(str_detect(modelling_applications, "Scenario analysis to compare policy options"),
                                 "Yes", "No"), 
         simulation = ifelse(str_detect(modelling_applications, "Simulation for study design, including clinical trials"),
                                 "Yes", "No"), 
         tactical_modelling = ifelse(str_detect(modelling_applications, "Tactical and operational modelling"),
                                 "Yes", "No"))
# ===================
# Check out variables
# ===================

df5 %>%
  select(1:50) %>%
  map(~count(data.frame(x = .x), x))


# ====================================================
# Detach libraries and remove objects from environment
# ====================================================

Vectorize(detach)(name = paste0("package:", c("tidyverse")), 
                  unload = TRUE, 
                  character.only = TRUE)

rm(list=ls())

