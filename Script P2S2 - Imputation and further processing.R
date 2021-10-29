rm(list=ls())

setwd("/home/rstudio/")
# Load files
# RED_FUNCTION_LOCATIONS = file.path(PARENT_FOLDER,"Paper 2 - Cumulative Risk/Paper 2 - Cumulative Risk/Red_3 - RED Functions (Paper 2).R")
OUPUTDATA_LOCATION = file.path(getwd(),"Data/MCS Data/Processed Data")

# Load Functions Needed for Analyses 
source("Red_3 - RED Functions (Paper 2).R")

# Load Original Data 
load(file.path(OUPUTDATA_LOCATION,"MCS_MasterDataset_FINAL.Rdata"))
load(file.path(OUPUTDATA_LOCATION,"MCS_MasterDataset_group1_FINAL.Rdata"))
load(file.path(OUPUTDATA_LOCATION,"MCS_MasterDataset_group2_FINAL.Rdata"))


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
  # "S2_Alcoholism", #too many missing data
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
  "OR2 Parent Rating",
  "PR2 Home Learning Environment",
  "PR2 Recreational Drug Use",
  "PR2 Harsh Parenting",
  "PR2 Monolingual-English Home"
)

# Outcome Variables - Names in R Data Frames 

outcomes=c(
  # findMCS("S5_Parent_SDQ"),
  findMCS("S6_Parent_SDQ_[a-zA-z]"),
  # "S5_TeacherRating",
  # findMCS("S5_ChildQ_Feelings"),
  # findMCS("S6_ChildQ_Feelings"),
  "S6_MentalHealth_HateSelf","S6_MentalHealth_Tired",
  "S6_SelfEsteem",
  "S6_Wellbeing",
  findMCS("S6_RiskyBehaviours"),
  "S6_ChildQ_SelfHarm",
  "S5_Cog_VS_Ability",
  "S6_Cog_WordScoreIRT",
  "S5_Cog_SWM_TotalErrors48",
  "S5_Cog_SWM_Strategy",
  "S5_Cog_CGT_RiskTaking",
  "S6_Cog_CGT_RiskTaking",
  "S6_AntiSocialBehaviour",
  "S6_Bullying"
  
  # findMCS("S5_Cog_CGT"),
  # findMCS("S6_Cog_CGT")
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
  # findMCS("S5_Parent_SDQ"),
  findMCS("S5_Teach_SDQ_[a-zA-z]"),
  "S5_TeacherRating",
  findMCS("S6_Parent_SDQ_[a-zA-z]"),
  # "S5_TeacherRating",
  # findMCS("S5_ChildQ_Feelings"),
  # findMCS("S6_ChildQ_Feelings"),
  "S6_MentalHealth_HateSelf","S6_MentalHealth_Tired",
  "S6_SelfEsteem",
  "S6_Wellbeing",
  findMCS("S6_RiskyBehaviours"),
  "S6_ChildQ_SelfHarm",
  "S5_Cog_VS_Ability",
  "S6_Cog_WordScoreIRT",
  "S5_Cog_SWM_TotalErrors48",
  "S5_Cog_SWM_Strategy",
  "S5_Cog_CGT_RiskTaking",
  "S6_Cog_CGT_RiskTaking",
  "S6_AntiSocialBehaviour",
  "S6_Bullying"
  
  # findMCS("S5_Cog_CGT"),
  # findMCS("S6_Cog_CGT")
)


all_var = c(envvar, outcomes)
all_var2 = c(envvar, outcomes2)
all_var_labels = c(envvar_labels, outcomes_labels)
all_var_labels2 = c(envvar_labels, outcomes_labels2)


############## Imputation 
library(mice)
IterationsN = 1 #For some parallel analyses are run in data_preprocessing 

#Combine groups for final analysis
df0_BothGroups = rbind.data.frame(df0_group1,df0_group2)

table(colnames(df0_group1)==colnames(df0_group2))

#Function to process variables and calcualte summary stats
data_preprocessing = function(dat_input=df0_group1){
    # dat_input=df0_group1
    df0_out = dat_input
    
    #Air Pollution
    df0_out$S12_AirPollution = calcFactorScore(df1=df0_out, var=findMCS("S1_Air|S2_Air"), verbose = TRUE)
    
    
    #Calculate score for word task (S6) using IRT 
    S6WORD = IRTcheck(df1=df0_out,vars="S6_Cog_Word_", guess_param = 1/5,mirtoutput =TRUE, mirt_modeltype="3PL", nCores=1, mirt_Method="EM", mirt_Optimizer="BFGS")
    df0_out$S6_Cog_WordScoreIRT = S6WORD$mirt_scores
  
    #Reduce Airpollution data
    (cor(df0_out[,findMCS("S_AirPollution")], use="pairwise.complete.obs"))
    psych::fa.parallel(df0_out[,findMCS("S_AirPollution")], use="pairwise.complete.obs", n.iter=IterationsN, error.bars = TRUE)
    df0_out$AirPollution = calcFactorScore(df1=df0_out,var = findMCS("S_AirPollution"), verbose=TRUE)
    
    #Perinatal Health Questions
      df0_out$S1_Health_PretermBirth = as.numeric((df0_out$S1_Health_GestationTime/7)>37)  #Using WHO definition of preterm birth at 37 weeks https://www.who.int/news-room/fact-sheets/detail/preterm-birth
      df0_out$S1_Health_LowBirthWeight = as.numeric(df0_out$S1_Health_BirthWeight>=2.5)
    
    #Reduce Parenting Questionnaire data 

      df0_out$S1_Attachment = calcFactorScore(df1=df0_out, var=findMCS("S1_Parenting_Attachment"), verbose = TRUE)
      df0_out$S1_Beliefs = calcFactorScore(df1=df0_out, var=findMCS("S1_Parenting_Belief"), verbose = TRUE)
      
      # S1_ParentingFA_Feelings = mirt::mirt(data=df0_out[c("S1_Parenting_ThinkAboutBaby","S1_Parenting_FeelingsLeavingBaby")],1, itemtype = "nominal", technical = list(removeEmptyRows=TRUE))
      # summary(S1_ParentingFA_Feelings)
      # plot(S1_ParentingFA_Feelings,type="trace")
    #Reduce Parent Well-being (PWB) questionnaire data

      df0_out$S1_PWB_RelationshipQuality = calcFactorScore(df1=df0_out, var=findMCS("S1_ParentRelationship"), verbose = TRUE)
      df0_out$S1_PWB_SelfEsteem = calcFactorScore(df1=df0_out, var=findMCS("S1_ParentSelfEsteem"), verbose = TRUE)
      df0_out$S1_PWB_LocusOfControl = calcFactorScore(df1=df0_out, var=findMCS("S1_Locus"), verbose = TRUE)
      df0_out$S1_PWB_SocialSupport = calcFactorScore(df1=df0_out, var=findMCS("S1_ParentSocialSupport"), verbose = TRUE) #dropped this due to low reliability 
      df0_out$S1_PWB_ParentMentalHealth = calcFactorScore(df1=df0_out, var=findMCS("S1_ParentMentalHealth"), verbose = TRUE)
      df0_out$S2_PWB_ParentMentalHealth = calcFactorScore(df1=df0_out, var=findMCS("S2_ParentMentalHealth"), verbose = TRUE)
    
   #Parent literacy problems - changed mind about how to code this so this is no longer applicable
      # cor(df0_out[findMCS("S1_ParentLiteracy")], use="pairwise.complete.obs")
      # df0_out$S1_ParentLiteracyProblems = calcFactorScore(df1=df0_out, var=findMCS("S1_ParentLiteracy"), verbose=TRUE)
      # 
   #Parent neighbourhood rating
      psych::fa.parallel(df0_out[findMCS("S1_Parent_Neighbourhood")], n.iter = IterationsN, error.bars = TRUE)
      df0_out$S1_ParentNeighbourhoodRating = calcFactorScore(df1=df0_out, var=findMCS("S1_Parent_Neighbourhood"), verbose=TRUE)
      
  #IMD ratings
      psych::fa.parallel(df0_out[findMCS("S1_IMD_")], n.iter = IterationsN, error.bars = TRUE)
      PlotCorrelationMatrix(df0[findMCS("S1_IMD_")])
      
      df0_out$S1_IMDDeprivation =  calcFactorScore(df1=df0_out,var=findMCS("S1_IMD"), verbose=TRUE)
      
  #Housing
      PlotCorrelationMatrix(df0[findMCS("S1_Housing")])
      fa(df0[findMCS("S1_Housing")])
      psych::alpha(df0[findMCS("S1_Housing")])
      df0_out$S1_HousingQuality = calcFactorScore(df1=df0_out,var=findMCS("S1_Housing"), verbose=TRUE)
      
  # Exposure to Violence 
      # ViolenceVar = c("S1_ParentRelationshipQuality_PartUseForce","S2_ParentObs_MotherSlapped")
      
  #Home & Parent Observations (S2)
      apply(df0_out[,findMCS("S2_HomeObs")],2,table)
      PlotCorrelationMatrix(df0_out[,findMCS("S2_HomeObs")])
      df0_out$S2_HomeRating = calcFactorScore(df1=df0_out,var=findMCS("S2_HomeObs"), verbose=TRUE)
      df0_out$S2_ParentRating = calcFactorScore(df1=df0_out,var=findMCS("S2_ParentObs"), verbose=TRUE)
      
  #Parenting Behaviours / Activities
      df0_out$S2_HelpWithLearning = calcFactorScore(df1=df0_out,var=c("S2_Parenting_ReadToChild","S2_Parenting_HelpWithAlphabet","S2_Parenting_HelpWithCounting"), verbose=TRUE)
      df0_out$S2_HarshParenting = calcFactorScore(df1=df0_out,var=findMCS("S2_HarshParenting"), verbose=TRUE)
      
      
  #Parental Alcoholism
      df0_out$S2_Alcoholism = calcFactorScore(df1=df0_out, var = findMCS("S2_Parent_Alcoholism"), verbose = TRUE)
      
  #Parent SDQ
      # fa.parallel(df0[findMCS("S6_Parent_SDQ")])
      # fa(df0[findMCS("S6_Parent_SDQ")], nfactors = 5)
      # S6_SDQ = calcFactorScore(df1=df0_out, var=findMCS("S6_Parent_SDQ"), nfact=5)
  #Teacher Ratings
      df_AA = na.omit(df0_out[,c("S5_Teach_AA_Eng","S5_Teach_AA_Math","S5_Teach_AA_Science")])
      mirt_model = mirt::mirt(df_AA, 1, itemtype = "nominal", technical = list(NCYCLES = 40000,parallel=FALSE,removeEmptyRows=FALSE), method="EM",optimizer = "BFGS")
      mirt_scores = mirt::fscores(mirt_model,method="EAP")[match(rownames(df0_out), rownames(df_AA))]
      df0_out$S5_TeacherRating = mirt_scores
      
      # plot(mirt_model, type="rxx")
      # plot(mirt_model, type="trace")
      # 
      
      
  # Child Mental Health
      findMCS("S6_ChildQ_Feelings")
      fa.parallel(df0_out[,findMCS("S6_ChildQ_Feelings")], n.iter=IterationsN, error.bars = TRUE)
      fa(df0_out[,findMCS("S6_ChildQ_Feelings")], nfact=2)
      
      df0_out$S6_MentalHealth_HateSelf = calcFactorScore(df1=df0_out, var=c("S6_ChildQ_Feelings_NoGood","S6_ChildQ_Feelings_Cried","S6_ChildQ_Feelings_HateSelf","S6_ChildQ_Feelings_BadPerson","S6_ChildQ_Feelings_Lonely","S6_ChildQ_Feelings_Unloved","S6_ChildQ_Feelings_NotGood","S6_ChildQ_Feelings_Wrong"), verbose=TRUE)
      df0_out$S6_MentalHealth_Tired = calcFactorScore(df1=df0_out, var=c("S6_ChildQ_Feelings_Unhappy","S6_ChildQ_Feelings_Anhedonia","S6_ChildQ_Feelings_Tired","S6_ChildQ_Feelings_Restless","S6_ChildQ_Feelings_Concentrate"), verbose=TRUE)
  
  #Child Self-Esteem
      fa.parallel(df0[,findMCS("S6_ChildQ_SelfEsteem")], n.iter=IterationsN, error.bars = TRUE)
      df0_out$S6_SelfEsteem = calcFactorScore(df1=df0_out, var=findMCS("S6_ChildQ_SelfEsteem"), verbose=TRUE)
      
  #Child Well-being
      fa.parallel(df0[,findMCS("S6_ChildQ_Well")], n.iter=IterationsN, error.bars = TRUE)
      df0_out$S6_Wellbeing = calcFactorScore(df1=df0_out, var=findMCS("S6_ChildQ_Well"), verbose=TRUE)
      
  # Bullying
      df0_out$S6_Bullying = calcFactorScore(df1=df0_out, var=findMCS("S6_ChildQ_Bully"), verbose=TRUE)
      
  # Antisocial behaviour
      AB = findMCS("S6_ChildQ_BB|S6_ChildQ_Truancy_Ever")
      # fa.parallel(df0_out[,AB], n.iter=IterationsN, error.bars = TRUE)
      df0_out$S6_AntiSocialBehaviour = calcFactorScore(df1=df0_out,var=AB, verbose=TRUE,nfact = 1)
      
      
return(df0_out)
}

#Run function on three datastets:
df0_group1 = data_preprocessing(dat_input = df0_group1)
df0_group2 = data_preprocessing(dat_input = df0_group2)
df0_BothGroups = data_preprocessing(dat_input = df0_BothGroups)
df0_teach_bothgroups = df0_BothGroups #Dataset which will include teacher metrics 
df0_teach_group1 = df0_group1
df0_teach_group2 = df0_group2


# #Split Teacher data into two groups
# set.seed(100)
# RandOrder = sample(nrow(df0_teach_bothgroups))
# df0_teach_group1 = df0_teach_bothgroups[RandOrder[1:9621],]
# df0_teach_group2 = df0_teach_bothgroups[RandOrder[1:9621],]
# 



#Check Data 
  #Missing Data Analysis - before culling 
  sort(apply(df0_BothGroups[,c(all_var)],2,function(x) length(which(!is.na(x)))))
  PercentMissingDataPerParticipant= (apply(df0_BothGroups[,all_var],1, function(x) length(which(!is.na(x)))/length(all_var) ))
  hist(PercentMissingDataPerParticipant)


#Perform any global edits to variables

  #Remove participants with more than X% Missing Data
    
  #df0_group1
  PercentMissing = (apply(df0_group1[,all_var],1, function(x) length(which(!is.na(x)))/length(all_var) ))
  df0_group1 = df0_group1[PercentMissing>.80,]
    save(df0_group1, file=file.path(OUPUTDATA_LOCATION,"/DataWithExclusionsNoImputations/df0_group1_FINAL.Rdata"))
    write.csv(df0_group1, file=file.path(OUPUTDATA_LOCATION,"/DataWithExclusionsNoImputations/df0_group1_FINAL.csv"))
    rm(PercentMissing)
  
  #df0_group2
  PercentMissing = (apply(df0_group2[,all_var],1, function(x) length(which(!is.na(x)))/length(all_var) ))
  df0_group2 = df0_group2[PercentMissing>.80,]
    save(df0_group2, file=file.path(OUPUTDATA_LOCATION,"/DataWithExclusionsNoImputations/df0_group2_FINAL.Rdata"))
    write.csv(df0_group2, file=file.path(OUPUTDATA_LOCATION,"/DataWithExclusionsNoImputations/df0_group2_FINAL.csv"))
    rm(PercentMissing)
    
  #df0_Bothgroups
  PercentMissing = (apply(df0_BothGroups[,all_var],1, function(x) length(which(!is.na(x)))/length(all_var) ))
  df0_BothGroups = df0_BothGroups[PercentMissing>.80,]
    save(df0_BothGroups, file=file.path(OUPUTDATA_LOCATION,"/DataWithExclusionsNoImputations/df0_BothGroups_FINAL.Rdata"))
    write.csv(df0_BothGroups, file=file.path(OUPUTDATA_LOCATION,"/DataWithExclusionsNoImputations/df0_BothGroups_FINAL.csv"))
    rm(PercentMissing)
    
  #df0_teach_bothgroups
  PercentMissing = (apply(df0_teach_bothgroups[,all_var2],1, function(x) length(which(!is.na(x)))/length(all_var2) ))
  df0_teach_bothgroups = df0_teach_bothgroups[PercentMissing>.80 & !is.na(df0_teach_bothgroups$S5_TeacherRating),]
    save(df0_teach_bothgroups, file=file.path(OUPUTDATA_LOCATION,"/DataWithExclusionsNoImputations/df0_teach_bothgroups_FINAL.Rdata"))
    write.csv(df0_teach_bothgroups, file=file.path(OUPUTDATA_LOCATION,"/DataWithExclusionsNoImputations/df0_teach_bothgroups_FINAL.csv"))
    rm(PercentMissing)
    
  
  #df0_teach_group1
  PercentMissing = (apply(df0_teach_group1[,all_var2],1, function(x) length(which(!is.na(x)))/length(all_var2) ))
  df0_teach_group1 = df0_teach_group1[PercentMissing>.80 & !is.na(df0_teach_group1$S5_TeacherRating),]
    save(df0_teach_group1, file=file.path(OUPUTDATA_LOCATION,"/DataWithExclusionsNoImputations/df0_teach_group1_FINAL.Rdata"))
    write.csv(df0_teach_group1, file=file.path(OUPUTDATA_LOCATION,"/DataWithExclusionsNoImputations/df0_teach_group1_FINAL.csv"))
    rm(PercentMissing)
  
  #df0_teach_group2
  PercentMissing = (apply(df0_teach_group2[,all_var2],1, function(x) length(which(!is.na(x)))/length(all_var2) ))
  df0_teach_group2 = df0_teach_group2[PercentMissing>.80 & !is.na(df0_teach_group2$S5_TeacherRating),]
    save(df0_teach_group2, file=file.path(OUPUTDATA_LOCATION,"/DataWithExclusionsNoImputations/df0_teach_group2_FINAL.Rdata"))
    write.csv(df0_teach_group2, file=file.path(OUPUTDATA_LOCATION,"/DataWithExclusionsNoImputations/df0_teach_group2_FINAL.csv"))
    rm(PercentMissing)
  
  
#### Normalise Data
  df0_group1[,all_var]            = apply(df0_group1[,all_var],2, function(x) map_normal(x, MinDim=10, rescale = TRUE, signif_digits = 8))
  df0_group2[,all_var]            = apply(df0_group2[,all_var],2, function(x) map_normal(x, MinDim=10, rescale = TRUE, signif_digits = 8))
  df0_BothGroups[,all_var]        = apply(df0_BothGroups[,all_var],2, function(x) map_normal(x, MinDim=10, rescale = TRUE, signif_digits = 8))
  df0_teach_bothgroups[,all_var2] = apply(df0_teach_bothgroups[,all_var2],2, function(x) map_normal(x, MinDim=10, rescale = TRUE, signif_digits = 8))
  df0_teach_group1[,all_var2]     = apply(df0_teach_group1[,all_var2],2, function(x) map_normal(x, MinDim=10, rescale = TRUE, signif_digits = 8))
  df0_teach_group2[,all_var2]     = apply(df0_teach_group2[,all_var2],2, function(x) map_normal(x, MinDim=10, rescale = TRUE, signif_digits = 8))
  
#### Impute Missing Data
  data.frame(sort(apply(df0_BothGroups[,all_var],2,function(x) length(which(!is.na(x))))))
  data.frame(sort(apply(df0_BothGroups[,all_var],2,function(x) sd(x, na.rm=TRUE))))
  
  #Percent Missing Data Per Variable
  df0_all = list(df0_group1,df0_group2,df0_BothGroups,df0_teach_bothgroups,df0_teach_group1,df0_teach_group2)
  PercentMissing=function(variablename="", data=df0_group1){
    if(variablename %in% colnames(data)){
      var = data[,variablename]
      return(length(which(!is.na(var)))/length(var))
    } else {
      return(NA)
    }
  }
  
  MissingDataPerVariable =
  sapply(df0_all, function(dat) 
    sapply(all_var2, function(var)
      PercentMissing(variablename=var,data=dat)))


  #df0_group1
  imputed_environment = mice::mice(df0_group1[,envvar],m=1, method="cart", seed=1032)
  imputed_outcome = mice::mice(df0_group1[,outcomes],m=1, method="cart", seed=1032)
  df0_imputed_group1 = cbind.data.frame(mice::complete(imputed_environment),mice::complete(imputed_outcome))
  rm(imputed_outcome,imputed_environment)
  
  #df0_group2
  imputed_environment = mice::mice(df0_group2[,envvar],m=1, method="cart", seed=1032)
  imputed_outcome = mice::mice(df0_group2[,outcomes],m=1, method="cart", seed=1032)
  df0_imputed_group2 = cbind.data.frame(mice::complete(imputed_environment),mice::complete(imputed_outcome))
  rm(imputed_outcome,imputed_environment)
  
  #df0_BothGroups
  imputed_environment = mice::mice(df0_BothGroups[,envvar],m=1, method="cart", seed=1032)
  imputed_outcome = mice::mice(df0_BothGroups[,outcomes],m=1, method="cart", seed=1032)
  df0_imputed_BothGroups= cbind.data.frame(mice::complete(imputed_environment),mice::complete(imputed_outcome))
  rm(imputed_outcome,imputed_environment)
  
  #df0_Teach
  imputed_environment = mice::mice(df0_teach_bothgroups[,envvar],m=1, method="cart", seed=1032)
  imputed_outcome = mice::mice(df0_teach_bothgroups[,outcomes2],m=1, method="cart", seed=1032)
  df0_imputed_Teach_BothGroups = cbind.data.frame(mice::complete(imputed_environment),mice::complete(imputed_outcome))
  rm(imputed_outcome,imputed_environment)
  
  #df0_teach_group1
  imputed_environment = mice::mice(df0_teach_group1[,envvar],m=1, method="cart", seed=1032)
  imputed_outcome = mice::mice(df0_teach_group1[,outcomes2],m=1, method="cart", seed=1032)
  df0_imputed_Teach_group1 = cbind.data.frame(mice::complete(imputed_environment),mice::complete(imputed_outcome))
  rm(imputed_outcome,imputed_environment)
  
  #df0_teach_group2
  imputed_environment = mice::mice(df0_teach_group2[,envvar],m=1, method="cart", seed=1032)
  imputed_outcome = mice::mice(df0_teach_group2[,outcomes2],m=1, method="cart", seed=1032)
  df0_imputed_Teach_group2 = cbind.data.frame(mice::complete(imputed_environment),mice::complete(imputed_outcome))
  rm(imputed_outcome,imputed_environment)
  

  
  save(df0_imputed_group1, file=file.path(OUPUTDATA_LOCATION,"df0_imputed_group1_FINAL.Rdata"))
  save(df0_imputed_group2, file=file.path(OUPUTDATA_LOCATION,"df0_imputed_group2_FINAL.Rdata"))
  save(df0_imputed_BothGroups, file=file.path(OUPUTDATA_LOCATION,"df0_imputed_BothGroups_FINAL.Rdata"))
  
  save(df0_imputed_Teach_BothGroups, file=file.path(OUPUTDATA_LOCATION,"df0_imputed_Teach_BothGroups_FINAL.Rdata"))
  save(df0_imputed_Teach_group1, file=file.path(OUPUTDATA_LOCATION,"df0_imputed_Teach_group1_FINAL.Rdata"))
  save(df0_imputed_Teach_group2, file=file.path(OUPUTDATA_LOCATION,"df0_imputed_Teach_group2_FINAL.Rdata"))
  
  
  
  write.csv(df0_imputed_group1, file=file.path(OUPUTDATA_LOCATION,"df0_imputed_group1_FINAL.csv"))
  write.csv(df0_imputed_group2, file=file.path(OUPUTDATA_LOCATION,"df0_imputed_group2_FINAL.csv"))
  write.csv(df0_imputed_BothGroups, file=file.path(OUPUTDATA_LOCATION,"df0_imputed_BothGroups_FINAL.csv"))
  
  write.csv(df0_imputed_Teach_BothGroups, file=file.path(OUPUTDATA_LOCATION,"df0_imputed_Teach_BothGroups_FINAL.csv"))
  write.csv(df0_imputed_Teach_group1, file=file.path(OUPUTDATA_LOCATION,"df0_imputed_Teach_group1_FINAL.csv"))
  write.csv(df0_imputed_Teach_group2, file=file.path(OUPUTDATA_LOCATION,"df0_imputed_Teach_group2_FINAL.csv"))
  
  
  
  

  # 
  # 
  # ####Create Plots  -- need to edit!
  # {
  #   dev.off()
  #   pdf(file="Plots/hist_df0_BothGroups.pdf", width=30, height=30)
  #   par(mfrow=c(8,7), mar=c(2,1,1,1))
  #   
  #   lapply(all_var, function(x) 
  #     hist(df0_group1[,x], main=x, xlab=NULL, ylab=NULL, col="black")
  #   )
  #   dev.off()
  #   
  #   pdf(file="Plots/hist_df0_imputed_BothGroups.pdf", width=30, height=30)
  #   par(mfrow=c(8,7), mar=c(2,1,1,1))
  #   
  #   lapply(all_var, function(x) 
  #     hist(df0_imputed_group1[,x], main=x, xlab=NULL, ylab=NULL, col="black")
  #   )
  #   dev.off()
  #   
  #   
  #   
  #   
  #   
  #   
  #   
  #   
  #   pdf(file="Plots/hist_envvar_group1.pdf", width=30, height=30)
  #   par(mfrow=c(10,10), mar=c(2,1,1,1))
  #   
  #   lapply(envvar, function(x) 
  #     hist(df0_original[,x], main=x, xlab=NULL, ylab=NULL, col="black")
  #   )
  #   dev.off()
  #   
  #   
  #   pdf(file="Plots/hist_outcomes_normed_group1.pdf", width=30, height=30)
  #   par(mfrow=c(10,10), mar=c(2,1,1,1))
  #   
  #   lapply(outcomes, function(x) 
  #     hist(df0[,x], main=x, xlab=NULL, ylab=NULL, col="black")
  #   )
  #   dev.off()
  #   
  #   pdf(file="Plots/hist_outcomes_group1.pdf", width=30, height=30)
  #   par(mfrow=c(10,10), mar=c(2,1,1,1))
  #   
  #   lapply(outcomes, function(x) 
  #     hist(df0_original[,x], main=x, xlab=NULL, ylab=NULL, col="black")
  #   )
  #   dev.off()
  #   
  #   pdf(file="Plots/correlationmatrix_group1.pdf", width=17, height=17)
  #   PlotCorrelationMatrix(df0[all_var])
  #   dev.off()
  # }
  # 
  # 
  # 
  # 
  # 
