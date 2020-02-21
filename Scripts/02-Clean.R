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

# ====================================
# Remove irrelevant variables
# ====================================

df1 <- df %>%
  select(-consent_1:-confirm_consent_2, -identify_people:-person_email_5_1)
  


# ==========================
# Rename long variable names
# ==========================

df2 <- df1 %>%
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


df2 %>%
  select_if(is.character) %>%
  map(~count(data.frame(x = .x), x))





