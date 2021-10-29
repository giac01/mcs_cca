base::rm(list=ls())
base::gc()


# Load Functions Needed for Analyses 
  source("Red_3 - RED Functions (Paper 2).R")
  
  #Load Canonical Correlation Analysis Code 
  # devtools::source_url("https://github.com/giac01/MiscellaneousRCode/blob/master/CanonicalCorrelationAnalysis.R?raw=TRUE")
  # source("CanonicalCorrelationAnalysis.R") # local copy of above file

  
# Load Original Data 
    load(file.path("Data", "MCS Data", "Processed Data", "MCS_MasterDataset_FINAL.Rdata"))
    load(file.path("Data", "MCS Data", "Processed Data", "MCS_MasterDataset_group1_FINAL.Rdata"))
    load(file.path("Data", "MCS Data", "Processed Data", "MCS_MasterDataset_group2_FINAL.Rdata"))
    
  
# Load Imputed Data 
  
  if (file.exists(file.path("Data", "MCS Data", "Processed Data","df0_imputed_BothGroups_FINAL.Rdata"))){
    load(file.path("Data", "MCS Data", "Processed Data","df0_imputed_BothGroups_FINAL.Rdata"))
  }
  if (file.exists(file.path("Data", "MCS Data", "Processed Data","df0_imputed_group1_FINAL.Rdata"))){
    load(file.path("Data", "MCS Data", "Processed Data","df0_imputed_group1_FINAL.Rdata"))
  }
  if (file.exists(file.path("Data", "MCS Data", "Processed Data","df0_imputed_group2_FINAL.Rdata"))){
    load(file.path("Data", "MCS Data", "Processed Data","df0_imputed_group2_FINAL.Rdata"))
  }
  if (file.exists(file.path("Data", "MCS Data", "Processed Data","df0_imputed_Teach_BothGroups_FINAL.Rdata"))){
    load(file.path("Data", "MCS Data", "Processed Data","df0_imputed_Teach_BothGroups_FINAL.Rdata"))
  }
  if (file.exists(file.path("Data", "MCS Data", "Processed Data","df0_imputed_Teach_group1_FINAL.Rdata"))){
    load(file.path("Data", "MCS Data", "Processed Data","df0_imputed_Teach_group1_FINAL.Rdata"))
  }
  if (file.exists(file.path("Data", "MCS Data", "Processed Data","df0_imputed_Teach_group2_FINAL.Rdata"))){
    load(file.path("Data", "MCS Data", "Processed Data","df0_imputed_Teach_group2_FINAL.Rdata"))
  }

# Predictor Variables - Names in R data frames 

envvar=c(
  "S1_SES_EducationAvg", "S1_SES_AverageLastJob","S1_SES_EquivIncome_Norm",
  # "S1_Geo_England",
  # "S1_Fam_NCarers", #THIS CANNOT BE INCLUDED WITH RELATIONSHIP QUALITY METRIC AS THEYRE INCOMPATIBLE
  "S2_NumberParents",
  "S1_Fam_NSiblings",
  "S1_Health_PretermBirth", "S1_Health_LowBirthWeight", "S1_Parent_GeneralHealth", "S1_Health_SmokedThroughoutPregnancy", "S1_Health_EverTriedBreastFeeding", "S1_Health_CurrentSmoking", "S1_Parent_AlcoholCurrent","S1_Parent_AlcoholPregnant",
  "S1_Fam_PlannedPregnancy", "S1_FeelingsPregnancy", 
  "S12_AirPollution",
  "S1_PWB_RelationshipQuality",
  "S1_PWB_SelfEsteem",
  "S1_PWB_ParentMentalHealth",
  "S2_PWB_ParentMentalHealth",
  "S1_PWB_LocusOfControl",
  # "S1_PWB_SocialSupport", #dropped due to low reliability
  "S1_ParentNeighbourhoodRating",
  "S1_IMDDeprivation",
  "S1_ParentLiteracyProblems",
  "S1_HousingQuality",
  "S2_HomeRating",
  "S2_ParentRating",
  "S2_HelpWithLearning",
  "S2_Parent_RecreationalDrugUse",
  "S2_HarshParenting",
  # "S2_Alcoholism", #too much missing data
  "S2_Misc_EnglishMonolingualHome"
)

# Predictor Variables - Pretty Labels 

envvar_labels = c(
  "PR1 SES-Education", "PR1 SES-Employment", "PR1 SES-Income",
  "PR2 Two Carers at Home",
  "PR1 Num Siblings",
  "PR1 Preterm Birth", "PR1 Low Birth Weight", "PR1 Parent General Health", "PR1 Smoked During Pregnancy", "PR1 Breastfeeding", "PR1 Current Smoking", "PR1 Current Alcohol", "PR1 Alcohol During Pregnancy",
  "PR1 Planned Pregnancy", "PR1 Feelings About Pregnancy",
  "S12 Air Pollution",
  "PR1 Parent Relationship Quality",
  "PR1 Parent Self Esteem",
  "PR1 Parent Mental Health",
  "PR2 Parent Mental Health",
  "PR1 Parent Locus Of Control",
  "PR1 Neighbourhood Quality",
  "S1 Index Multiple Deprivation",
  "PR1 Parent Literacy Problems",
  "PR1 Housing Quality",
  "OR2 Housing Quality",
  "OR2 Parent Responsivity",
  "PR2 Home Learning Environment",
  "PR2 Recreational Drug Use",
  "PR2 Harsh Parenting",
  "PR2 Monolingual-English Home"
)

# Outcome Variables - Names in R Data Frames 

outcomes=c(
  "S6_Parent_SDQ_Emotion","S6_Parent_SDQ_Conduct","S6_Parent_SDQ_Hyper","S6_Parent_SDQ_Peer" ,"S6_Parent_SDQ_Prosocial",
  "S6_MentalHealth_HateSelf","S6_MentalHealth_Tired",
  "S6_SelfEsteem",
  "S6_Wellbeing",
  "S6_RiskyBehaviours_Cig","S6_RiskyBehaviours_ECig","S6_RiskyBehaviours_Alcohol","S6_RiskyBehaviours_Cannabis",
  "S6_ChildQ_SelfHarm",
  "S5_Cog_VS_Ability",
  "S6_Cog_WordScoreIRT",
  "S5_Cog_SWM_TotalErrors48",
  "S5_Cog_SWM_Strategy",
  "S5_Cog_CGT_RiskTaking",
  "S6_Cog_CGT_RiskTaking",
  "S6_AntiSocialBehaviour",
  "S6_Bullying"
)

# Outcome Variables - Pretty Labels 
outcomes_labels = c(
  "PR6 SDQ Emotional Problems",
  "PR6 SDQ Conduct Problems",
  "PR6 SDQ Hyperactivity",
  "PR6 SDQ Peer Problems",
  "PR6 SDQ Prosocial",
  "CR6 Depression",
  "CR6 Anhedonia",
  "CR6 Self Esteem",
  "CR6 Wellbeing",
  "CR6 Cigarette Use",
  "CR6 E-Cigarette Use",
  "CR6 Alcohol Use",
  "CR6 Cannabis Use",
  "CR6 Self Harm",
  "CR5 Vocabulary",
  "CR6 Vocabulary",
  "CC5 Spatial WM Total Errors",
  "CC5 Spatial WM Strategy",
  "CC5 CGT Risk Taking",
  "CC6 CGT Risk Taking",
  "CR6 Anti-Social Behaviour",
  "CR6 Bullying Others"
)

# Outcome variables including Teacher Reports 
outcomes_labels2 = c(
  "TR5 SDQ Emotional Problems",
  "TR5 SDQ Conduct Problems",
  "TR5 SDQ Hyperactivity",
  "TR5 SDQ Peer Problems",
  "TR5 SDQ Prosocial",
  "TR5 Academic Ability",
  "PR6 SDQ Emotional Problems",
  "PR6 SDQ Conduct Problems",
  "PR6 SDQ Hyperactivity",
  "PR6 SDQ Peer Problems",
  "PR6 SDQ Prosocial",
  "CR6 Depression",
  "CR6 Anhedonia",
  "CR6 Self Esteem",
  "CR6 Wellbeing",
  "CR6 Cigarette Use",
  "CR6 E-Cigarette Use",
  "CR6 Alcohol Use",
  "CR6 Cannabis Use",
  "CR6 Self Harm",
  "CR5 Vocabulary",
  "CR6 Vocabulary",
  "CC5 Spatial WM Total Errors",
  "CC5 Spatial WM Strategy",
  "CC5 CGT Risk Taking",
  "CC6 CGT Risk Taking",
  "CR6 Anti-Social Behaviour",
  "CR6 Bullying Others"
)

# Outcomes Variables - R Variable Names including TEACHER VARIABLES 
outcomes2=c(
  "S5_Teach_SDQ_Emotion", "S5_Teach_SDQ_Conduct","S5_Teach_SDQ_Hyper","S5_Teach_SDQ_Peer","S5_Teach_SDQ_Prosocial",
  "S5_TeacherRating",
  "S6_Parent_SDQ_Emotion","S6_Parent_SDQ_Conduct","S6_Parent_SDQ_Hyper","S6_Parent_SDQ_Peer" ,"S6_Parent_SDQ_Prosocial",
  "S6_MentalHealth_HateSelf","S6_MentalHealth_Tired",
  "S6_SelfEsteem",
  "S6_Wellbeing",
  "S6_RiskyBehaviours_Cig","S6_RiskyBehaviours_ECig","S6_RiskyBehaviours_Alcohol","S6_RiskyBehaviours_Cannabis",
  "S6_ChildQ_SelfHarm",
  "S5_Cog_VS_Ability",
  "S6_Cog_WordScoreIRT",
  "S5_Cog_SWM_TotalErrors48",
  "S5_Cog_SWM_Strategy",
  "S5_Cog_CGT_RiskTaking",
  "S6_Cog_CGT_RiskTaking",
  "S6_AntiSocialBehaviour",
  "S6_Bullying"
)

all_var = c(envvar, outcomes)
all_var2 = c(envvar, outcomes2)
all_var_labels = c(envvar_labels, outcomes_labels)
all_var_labels2 = c(envvar_labels, outcomes_labels2)


