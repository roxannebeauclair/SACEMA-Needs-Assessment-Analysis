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
         no_need_modelling = `recent_modelling_whynot_No need`,
         sufficient_capacity_modelling = `recent_modelling_whynot_Sufficient in-house capacity`,
         no_org_modelling = `recent_modelling_whynot_Unable to identify external organization that could meet need`,
         working_difficult_modelling = `recent_modelling_whynot_Working with modellers is difficult`,
         no_funding_modelling = `recent_modelling_whynot_Insufficient funding to support collaborations`,
         bureau_barriers_modelling = `recent_modelling_whynot_Bureaucratic barriers (e.g. restrictions on data-sharing)`,
         not_injob_modelling = `recent_modelling_whynot_\tConducting or overseeing modelling work is not within my job description`,
         other_modelling = recent_modelling_whynot_Other,
         model_collab_loc = modelling_collaborators_location,
         model_collab_org = modelling_collaborators_organization,
         model_collab_fund = modelling_collaborators_funding,
         model_collab_unfund = modelling_collaborators_unfunded,
         no_need_analysis = `recent_analysis_whynot_No need`,
         sufficient_capacity_analysis = `recent_analysis_whynot_Sufficient in-house capacity`,
         no_org_analysis = `recent_analysis_whynot_Unable to identify external organization that could meet need`,
         working_difficult_analysis = `recent_analysis_whynot_Working with analysts is difficult`,
         no_funding_analysis = `recent_analysis_whynot_Insufficient funding to support collaborations`,
         bureau_barriers_analysis = `recent_analysis_whynot_Bureaucratic barriers (e.g. restrictions on data-sharing)`,
         not_injob_analysis = `recent_analysis_whynot_Conducting or overseeing analysis is not within my job description`,
         other_analysis = recent_analysis_whynot_Other,
         analysis_collab_loc = analysis_collaborators_location,
         analysis_collab_org = analysis_collaborators_organization,
         analysis_collab_fund = analysis_collaborators_funding,
         analysis_collab_unfund = analysis_collaborators_unfunded,
         alc = `health_areas_Alcohol and substance abuse`,
         amr = `health_areas_Anti-microbial resistance`,
         biosecurity = health_areas_Biosecurity,
         cancer = health_areas_Cancer,
         diabetes = health_areas_Diabetes,
         emergency = `health_areas_Emergency medicine`,
         food_safety = `health_areas_Food safety`,
         health_systems = `health_areas_Health systems research and planning`,
         hiv = `health_areas_HIV and STIs`,
         hypertension = `health_areas_Hypertension and other cardiovascular diseases`,
         injury = `health_areas_Injury and violence prevention`,
         maternal = `health_areas_Maternal and reproductive health`,
         mental = `health_areas_Mental Health`,
         nutrition = `health_areas_Nutrition and hunger elimination`,
         occupational = `health_areas_Occupational Health`,
         outbreak = `health_areas_Outbreak/epidemic response`,
         tb = health_areas_Tuberculosis,
         vaccine = `health_areas_Vaccine-preventable diseases`,
         vector = `health_areas_Vector-borne and zoonotic infections`,
         health_none = `health_areas_None Of The Above`,
         bioinformatics = `modelling_applications_ Bioinformatics`,
         me = `modelling_applications_Evaluation and assessment /  Monitoring & Evaluation`,
         forecasting = `modelling_applications_Forecasting of future public health events`, 
         transmission = `modelling_applications_Fundamental investigation of  transmission dynamics`,
         measurement = `modelling_applications_Improving  diagnostics and/ or the measurement and interpretation of  biomarkers`,
         surveillance = `modelling_applications_Improving population-based  surveillance`,
         rapid_response = `modelling_applications_Responding quickly to emerging disease threats`,
         risk_assessment = `modelling_applications_ Risk assessment`,
         scenario_analysis = `modelling_applications_ Scenario analysis to compare policy options`,
         simulation = `modelling_applications_Simulation for study design, including clinical trials`,
         tactical_modelling = `modelling_applications_Tactical and operational modelling`,
         applications_none = `modelling_applications_None Of The Above`,
         quant_skills_org = quant_skills_organization,
         pref_collab_loc = preference_collaborator_location,
         pref_collab_loc_why = preference_collaborator_location_why,
         skills_dm = `quant_skills_outsource_ Data management`,
         skills_dv = `quant_skills_outsource_ Data visualization`,
         skills_stats = `quant_skills_outsource_ Statistical analysis`,
         skills_modelling = `quant_skills_outsource_ Modelling`,
         skills_automation = `quant_skills_outsource_Automation of analysis including statistical programming`,
         skills_interpretation = `quant_skills_outsource_Interpretation of  surveillance data and results from models and statistical analysis`,
         skills_other = quant_skills_outsource_Other,
         skills_none = `quant_skills_outsource_None Of The Above`,
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
                                       Academic = "Academic, Academic",
                                       Government = "Government, Government"),
         role = fct_recode(role,
                           `Researcher/Lecturer` = "Researcher/Lecturer, Researcher/Lecturer",
                           Epidemiologist = "Epidemiologist, Epidemiologist"),
         time_org = as.numeric(fct_recode(time_org,
                               `28` = "28, 28",
                               `9` = "9, 9")),
         sector = fct_recode(sector,
                             Research = "Research, Research",
                             `Surveillance and Response` = "communicable disease surveillance and respons",
                             `Surveillance and Response` = "surveillance"),
         time_publichealth = as.numeric(fct_recode(time_publichealth,
                                                   `25` = "25, 24",
                                                   `9` = "9, 9")),
         org_model_whynot = fct_collapse(org_model_whynot,
                                     `Lack of adequate skills` = c("Economic modeling is a challenge and no in-house expertise",
                                                                   "lack of adequate skills",
                                                                   "Lack of sufficiently skilled and experienced personnel"),
                                     `Other modelling partnerships` = c("Partnership with other modellers")),
         meet_needs_how = fct_recode(meet_needs_how,
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
                                NICD = c("Centre for HIV and STIs",
                                         "National Cancer Registry")))


# =========================================
# Make factors and add yes/no levels to
# previous Checkbox variables
# =========================================
ny <- c("No", "Yes") # c(0, 1)

df5 <- df4 %>%
  mutate(no_need_modelling = factor(no_need_modelling, 
                                    levels = c(0, 1), 
                                    labels = ny),
         sufficient_capacity_modelling = factor(sufficient_capacity_modelling, 
                                    levels = c(0, 1), 
                                    labels = ny),
         no_org_modelling = factor(no_org_modelling, 
                                    levels = c(0, 1), 
                                    labels = ny),
         working_difficult_modelling = factor(working_difficult_modelling, 
                                    levels = c(0, 1), 
                                    labels = ny), 
         no_funding_modelling = factor(no_funding_modelling, 
                                    levels = c(0, 1), 
                                    labels = ny),
         bureau_barriers_modelling = factor(bureau_barriers_modelling, 
                                    levels = c(0, 1), 
                                    labels = ny),
         not_injob_modelling = factor(not_injob_modelling, 
                                    levels = c(0, 1), 
                                    labels = ny),
         no_need_analysis = factor(no_need_analysis, 
                                    levels = c(0, 1), 
                                    labels = ny),
         sufficient_capacity_analysis = factor(sufficient_capacity_analysis, 
                                                levels = c(0, 1), 
                                                labels = ny),
         no_org_analysis = factor(no_org_analysis, 
                                   levels = c(0, 1), 
                                   labels = ny),
         working_difficult_analysis = factor(working_difficult_analysis, 
                                              levels = c(0, 1), 
                                              labels = ny), 
         no_funding_analysis = factor(no_funding_analysis, 
                                       levels = c(0, 1), 
                                       labels = ny),
         bureau_barriers_analysis = factor(bureau_barriers_analysis, 
                                            levels = c(0, 1), 
                                            labels = ny),
         not_injob_analysis = factor(not_injob_analysis, 
                                      levels = c(0, 1), 
                                      labels = ny),
         alc = factor(alc, 
                      levels = c(0, 1), 
                      labels = ny),
         amr = factor(amr, 
                      levels = c(0, 1), 
                      labels = ny),
         biosecurity = factor(biosecurity, 
                              levels = c(0, 1), 
                              labels = ny),
         cancer = factor(cancer, 
                         levels = c(0, 1), 
                         labels = ny),
         diabetes = factor(diabetes, 
                           levels = c(0, 1), 
                           labels = ny),
         emergency = factor(emergency,
                            levels = c(0, 1), 
                            labels = ny),
         food_safety = factor(food_safety, 
                              levels = c(0, 1), 
                              labels = ny),
         health_systems = factor(health_systems, 
                                     levels = c(0, 1), 
                                     labels = ny),
         hiv = factor(hiv, 
                      levels = c(0, 1), 
                      labels = ny),
         hypertension = factor(hypertension, 
                               levels = c(0, 1), 
                               labels = ny),
         injury = factor(injury, 
                         levels = c(0, 1), 
                         labels = ny),
         maternal = factor(maternal,
                           levels = c(0, 1), 
                           labels = ny),
         mental = factor(mental, 
                         levels = c(0, 1), 
                         labels = ny),
         nutrition = factor(nutrition, 
                            levels = c(0, 1), 
                            labels = ny),
         occupational = factor(occupational, 
                               levels = c(0, 1), 
                               labels = ny),
         outbreak = factor(outbreak, 
                           levels = c(0, 1), 
                           labels = ny),
         tb = factor(tb, 
                     levels = c(0, 1), 
                     labels = ny),
         vaccine = factor(vaccine, 
                          levels = c(0, 1), 
                          labels = ny),
         vector = factor(vector, 
                         levels = c(0, 1), 
                         labels = ny),
         health_none = factor(health_none, 
                         levels = c(0, 1), 
                         labels = ny),
         bioinformatics = factor(bioinformatics, 
                         levels = c(0, 1), 
                         labels = ny),
         me = factor(me, 
                     levels = c(0, 1), 
                     labels = ny),
         forecasting = factor(forecasting, 
                         levels = c(0, 1), 
                         labels = ny),
         transmission = factor(transmission, 
                         levels = c(0, 1), 
                         labels = ny),
         measurement = factor(measurement, 
                         levels = c(0, 1), 
                         labels = ny),
         surveillance = factor(surveillance, 
                         levels = c(0, 1), 
                         labels = ny),
         rapid_response = factor(rapid_response, 
                         levels = c(0, 1), 
                         labels = ny),
         risk_assessment = factor(risk_assessment, 
                         levels = c(0, 1), 
                         labels = ny),
         scenario_analysis = factor(scenario_analysis, 
                         levels = c(0, 1), 
                         labels = ny),
         simulation = factor(simulation, 
                         levels = c(0, 1), 
                         labels = ny),
         tactical_modelling = factor(tactical_modelling, 
                         levels = c(0, 1), 
                         labels = ny),
         applications_none = factor(applications_none, 
                         levels = c(0, 1), 
                         labels = ny),
         skills_dm = factor(skills_dm, 
                         levels = c(0, 1), 
                         labels = ny),
         skills_dv = factor(skills_dv, 
                         levels = c(0, 1), 
                         labels = ny),
         skills_stats = factor(skills_stats, 
                         levels = c(0, 1), 
                         labels = ny),
         skills_modelling = factor(skills_modelling, 
                         levels = c(0, 1), 
                         labels = ny),
         skills_automation = factor(skills_automation, 
                         levels = c(0, 1), 
                         labels = ny),
         skills_interpretation = factor(skills_interpretation, 
                         levels = c(0, 1), 
                         labels = ny),
         skills_none = factor(skills_none, 
                         levels = c(0, 1), 
                         labels = ny),
         org_model = factor(org_model,
                            levels = c("No", "Yes")))


# =======================
# Re-level likert factors
# =======================

importance_vars <- c("importance_indicators", "importance_predict","importance_studydesign",
                     "importance_compare_effects", "importance_compare_costs", "importance_strategy",
                     "importance_impact", "importance_assess_risk", "importance_communication",
                     "importance_understand_spread")

recent_vars <- c("recent_modelling", "recent_analysis")

interaction_vars <- c("cofund_research_sacema", "cofund_research_org", "host_org", "host_sacema",
                      "cosupervise", "non_degree_program", "degree_program", "contract_sacema")


df6 <- df5 %>%
  mutate_at(importance_vars, fct_relevel, 
            "Not important", "Moderately important", "Very important") %>%
  mutate_at(recent_vars, fct_relevel, 
            "In the past month", "In the past year", "In the past 3 years", "In the past 5 years", "Never") %>%
  mutate_at(interaction_vars, fct_relevel, 
            "Very interested", "Somewhat  interested", "Not at all interested", "I am not in a position to know what my organization would be interested in pursuing")


# ===================
# Check out variables
# ===================

# df3 %>%
#   select(101:149) %>%
#   map(~count(data.frame(x = .x), x))
# 
# df6 %>%
#   select(101:149) %>%
#   map(~levels(.))

# ====================
# Save cleaned dataset
# ====================

df <- df6

save(df, file = cleandf)


# ====================================================
# Detach libraries and remove objects from environment
# ====================================================

Vectorize(detach)(name = paste0("package:", c("tidyverse")), 
                  unload = TRUE, 
                  character.only = TRUE)

rm(list=ls())

