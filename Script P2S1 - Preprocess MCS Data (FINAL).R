
rm(list=ls())
library(data.table) ; library(psych) ; library(stringr) ; library(ltm)

#Constants, libraries
PARENT_FOLDER = "/home/rstudio"

MCS_DATA_LOCATION = file.path(PARENT_FOLDER,"Data/MCS Data/Raw Data")
RED_FUNCTION_LOCATIONS = file.path(PARENT_FOLDER,"Red_3 - RED Functions (Paper 2).R")
OUPUTDATA_LOCATION = file.path(PARENT_FOLDER,"Data/MCS Data/Processed Data")

setwd(MCS_DATA_LOCATION)
source(RED_FUNCTION_LOCATIONS)


#Load master dataset (contains all family IDs present in sweep 1 & 2)
df0 = fread("UKDA-8172-tab Longitudinal family file/tab/mcs_longitudinal_family_file.tab", stringsAsFactors = FALSE, data.table = FALSE)

convertSDQ=function(SDQ,codes=1:3,varnames="SDQ_"){
  SDQ = apply(SDQ,2,function(x) Convert(x,codes,1:3))
  recode_questions = c(2,3,5,6,8,10,12,13,15,16,18,19,22,23,24)
  for (i in recode_questions){
    SDQ[,i] = swapsies(SDQ[,i],codes,3:1)
  }
  colnames(SDQ) = paste0(varnames,1:25)
  print(fa(SDQ))
  
  #Theory-Based Coding
  Emotion = apply(SDQ[,c(3,8,13,16,24)], 1, function(x) mean(x, na.rm=TRUE)) 
  Conduct = apply(SDQ[,c(5,7,12,18,22)], 1, function(x) mean(x, na.rm=TRUE)) 
  Hyper = apply(SDQ[,c(2,10,15,21,25)], 1, function(x) mean(x, na.rm=TRUE)) 
  Peer = apply(SDQ[,c(6,11,14,19,23)], 1, function(x) mean(x, na.rm=TRUE)) 
  Prosocial = apply(SDQ[,c(1,4,9,17,20)], 1, function(x) mean(x, na.rm=TRUE)) 
  
  SDQ_Theory = cbind.data.frame(Emotion, Conduct, Hyper, Peer, Prosocial)
  colnames(SDQ_Theory) = paste0(varnames,c("Emotion", "Conduct", "Hyper", "Peer", "Prosocial"))
  SDQ_Theory[apply(SDQ_Theory,2,is.nan)]=NA
  

  
  SDQ = cbind.data.frame(SDQ,SDQ_Theory)
  
  # df0$S2_SDQ_Conduct = apply(df0[,paste0("S2_SDQ", c(5,7,12,18,22))], 1, function(x) mean(x, na.rm=TRUE)) 
  # df0$S2_SDQ_Hyper = apply(df0[,paste0("S2_SDQ", c(2,10,15,21,25))], 1, function(x) mean(x, na.rm=TRUE)) 
  # df0$S2_SDQ_Peer = apply(df0[,paste0("S2_SDQ", c(6,11,14,19,23))], 1, function(x) mean(x, na.rm=TRUE)) 
  # df0$S2_SDQ_Prosocial = apply(df0[,paste0("S2_SDQ", c(1,4,9,17,20))], 1, function(x) mean(x, na.rm=TRUE)) 
  # rm(SDQ)
  
  return(SDQ)
}

#######################################################################################################################
####################################################     MCS 1    #################################################### 
#######################################################################################################################


S1Der = fread("UKDA-4683-tab MCS1/tab/mcs1_derived_variables.tab", stringsAsFactors = FALSE, data.table = FALSE)
S1Der = S1Der[match(df0$MCSID, S1Der$MCSID),]

S1ParInt = fread("UKDA-4683-tab MCS1/tab/mcs1_parent_interview.tab", stringsAsFactors = FALSE, data.table = FALSE)
S1ParInt = S1ParInt[match(df0$MCSID, S1ParInt$mcsid),]

S1Geo = fread("UKDA-4683-tab MCS1/tab/mcs1_geographically_linked_data.tab", stringsAsFactors = FALSE, data.table = FALSE)
S1Geo = S1Geo[match(df0$MCSID, S1Geo$mcsid),]


#Geographically Linked Data

df0$S1_Geo_Country = S1Geo[,2] #Pos. = 2	Variable = aactry00	Variable label = S1:ADMIN Interview Country (E,W,S,NI)
df0$S1_Geo_England = Convert(S1Geo[,2],1:4,c(1,0,0,0)) #IS the family based in England? 


df0$S1_IMD_Income     =       rowSums(cbind(S1Geo[,11],S1Geo[,19],S1Geo[,28],S1Geo[,36] ), na.rm = TRUE)
df0$S1_IMD_Employment =       rowSums(cbind(S1Geo[,12],S1Geo[,20],S1Geo[,29],S1Geo[,37] ), na.rm = TRUE)
df0$S1_IMD_Health     =       rowSums(cbind(S1Geo[,13],S1Geo[,21],S1Geo[,30],S1Geo[,38] ), na.rm = TRUE)
df0$S1_IMD_Education  =       rowSums(cbind(S1Geo[,14],S1Geo[,22],S1Geo[,31],S1Geo[,39] ), na.rm = TRUE)
df0$S1_IMD_Housing    =       rowSums(cbind(S1Geo[,15],S1Geo[,23],S1Geo[,32]            ), na.rm = TRUE)
df0$S1_IMD_Crime      =       rowSums(cbind(S1Geo[,16],S1Geo[,24],           S1Geo[,40] ), na.rm = TRUE)
df0$S1_IMD_Living     =       rowSums(cbind(S1Geo[,17],S1Geo[,25],           S1Geo[,41]  ), na.rm = TRUE)


table(df0$S1_Geo_Country,S1Geo[,11])

apply(df0[,findMCS("S1_IMD")],2,table)


#Family Variables

df0$S1_Main_RelationshipToCohortMember = S1ParInt[,63] # 7=Natural Parent. Pos. = 63	Variable = amcrel00	Variable label = S1 MAIN Relationship to Cohort Member  
df0$S1_Main_Sex = S1ParInt[,60] #Pos. = 60	Variable = ampsex00	Variable label = S1 MAIN Sex
df0$S1_Main_NaturalMother =  as.numeric(df0$S1_Main_Sex==2 & df0$S1_Main_RelationshipToCohortMember==7) #1 = Natural mother
df0$S1_Main_NaturalMotherInterviewed  = Convert(S1ParInt[,1726], 1:10, c(1,rep(0,8),NA)) #Pos. = 1726	Variable = admres00	Variable label = S1 DV Main respondent identity and interview status (exlcuded 20 mothers who were not interviewed)

df0$S1_Main_Identity_AMDRES00 = S1Der$AMDRES00
df0$S1_Part_Identity_APDRES00 = S1Der$APDRES00
df0$S1_Main_Education_AMDNVQ00 = S1Der$AMDNVQ00
df0$S1_Part_Education_APDNVQ00 = S1Der$APDNVQ00 

df0$S1_Ethnicity_England_ADCEEAA0 = S1Der$ADCEEAA0
df0$S1_Ethnicity_Wales_BDCEWAA0 = S1Der$BDCEWAA0
df0$S1_Ethnicity_Scotland_BDCESAA0 = S1Der$BDCESAA0
df0$S1_Ethnicity_NI_BDCENAA0 = S1Der$BDCENAA0


df0$S1_SES_PovertyIndicator_ADOEDP00 = S1Der$ADOEDP00

df0$S1_Fam_NCarers = Convert(S1Der[,5],1:2,1:0) #1 = two carers in household, 0= 0 carers in household. 
df0$S1_Fam_MotherPresent = swapsies(setNA(S1Der[,8],3), 1:2, 1:0) #1= resident 0=not resident , NA=deceased 
df0$S1_Fam_FatherPresent = swapsies(setNA(S1Der[,10],3), 1:2, 1:0) #1= resident 0=not resident , NA=deceased 
df0$S1_Fam_NSiblings = S1Der[,11] #Pos. = 11	Variable = ADOTHS00	Variable label = S1 DV Number of siblings of CM in household 
df0$S1_Fam_ParentsEverSeperated = Convert(S1ParInt[,631], 1:2, 0:1) # 1=no, 0=yes, na=other (e.g. never together) Pos. = 631	Variable = ampasd00	Variable label = S1 MAIN Parents ever separated 

df0$S1_Fam_ParentDeath = Convert(S1ParInt[,11], c(1,2,-1), c(0,0,1)) #1=no parents have died, 0= one parent died before or after birth
df0$S1_Fam_PlannedPregnancy = Convert(S1ParInt[,191], 1:2, 1:0) #1 = pregnancy was planned, 0=alternative Pos. = 191	Variable = amprpl00	Variable label = S1 MAIN Planned pregnancy  
df0$S1_FeelingsPregnancy = Convert(S1ParInt[,202], 1:5,5:1) #Pos. = 202 Variable = amprfe00 Variable label = S1 MAIN How felt when became pregnant 

df0$S1_SES_FinancialHelpFromParents = Convert(S1ParInt[,642], c(0:5,51:53), c(0,1,1,1,1,1,1,1,1))

df0$S1_SES_EquivIncome = setNA(S1Der[,41], -1)
df0$S1_SES_EquivIncome_Norm = Normalise(df0$S1_SES_EquivIncome)
df0$S1_SES_Income_Predicted = setNA(S1Der[,49],-1) #Pos. = 49	Variable = AOEDEX00	Variable label = S1 DV PREDICTED weekly net family income

df0$S1_SES_MainLastJob =  Convert(S1Der[,82], 1:7, 7:1) #Variable = AMD13C00	Variable label = S1 MAIN  NS-SEC major categories (last known job)   
df0$S1_SES_PartnerLastJob = Convert(S1Der[,115], 1:7, 7:1) #Pos. = 115	Variable = APD07C00	Variable label = S1 PARTNER  NS-SEC 7 classes (last known job)   
df0$S1_SES_AverageLastJob = apply(df0[,c("S1_SES_MainLastJob","S1_SES_PartnerLastJob")],1, function(x) mean(x, na.rm=TRUE))

df0$S1_SES_EducationMain_AMDNVQ00 = S1Der$AMDNVQ00

df0$S1_SES_EducationMain = Convert(S1Der[64],c(96,1,2,3,4,5), 0:5) #Pos. = 64	Variable = AMDNVQ00	Variable label = S1 MAIN Respondent NVQ highest level   
df0$S1_SES_EducationPartner = Convert(S1Der[97],c(96,1,2,3,4,5), 0:5) #Pos. = 97	Variable = APDNVQ00	Variable label = S1 PARTNER Respondent NVQ highest level 
df0$S1_SES_EducationAvg = apply(df0[,c("S1_SES_EducationMain","S1_SES_EducationPartner")],1, function(x) mean(x, na.rm=TRUE))

df0$S1_SES_FA = calcFactorScore(var=c("S1_SES_EquivIncome_Norm","S1_SES_AverageLastJob","S1_SES_EducationAvg"), verbose=TRUE)
df0$S1_SES_FSN = Normalise(calcFactorScore(var=c("S1_SES_EquivIncome_Norm","S1_SES_AverageLastJob","S1_SES_EducationAvg"), verbose=TRUE))
df0$S1_SES_10D = as.numeric(Normalise(calcFactorScore(var=c("S1_SES_EquivIncome_Norm","S1_SES_AverageLastJob","S1_SES_EducationAvg"), verbose=TRUE),return = "percentile")>.10)

#df0$S1_SES_Employment = Convert(S1Der[,70], 1:7, 7:1) #Pos. = 70	Variable = AMD07S00	Variable label = S1 MAIN Respondent NS-SEC 7 classes (current job) 

df0$S1_SES_HomeOwnership_ADROOW00 = S1Der$ADROOW00
df0$S1_SES_HomeOwnership = Convert(S1Der[,43], 1:10, c(1,1,1,0,0,0,0,0,0,0)) #1 = Own or part own property 0 = do not own

df0$S1_SES_McClementsPovertyLine = Convert(S1ParInt[,1733],0:1,1:0)

#Raw income data - DO NOT TRUST ABSOLUTE VALUES YET!
df0$S1_SES_Income_TotalHousehold_JOINT = Convert(S1ParInt[,1097],2:19, c(30,100,170,220,255,280,335,415,490,560,625,715,845,960,1100,1400,1900,2550,3250))
df0$S1_SES_Income_TotalHousehold_NOPARTNER = Convert(S1ParInt[,1096],2:19, c(10,80,165,205,225,240,280,340,400,465,535,620,755,885,1000,1225,1690,2050,2150))
df0$S1_SES_Income_TotalHousehold = df0$S1_SES_Income_TotalHousehold_JOINT
df0[which(!is.na(df0$S1_SES_Income_TotalHousehold_NOPARTNER)),"S1_SES_Income_TotalHousehold"]=df0$S1_SES_Income_TotalHousehold_NOPARTNER[which(!is.na(df0$S1_SES_Income_TotalHousehold_NOPARTNER))]


#child Health Variables
Birthweight_ParentKiloBirthWight = setNA(S1ParInt[,253], -1:-8) #Don't use, this is just to check the below variable is definately generated from the questionnaire 

df0$S1_Health_BirthWeight = setNA(S1Der[,125], -1:-8) #Variable = ADBWGTA0	Variable label = S1 DV Cohort Member birth weight in kilos C1
df0$S1_Health_BirthWeight_Norm = Normalise(df0$S1_Health_BirthWeight) #Variable = ADBWGTA0	Variable label = S1 DV Cohort Member birth weight in kilos C1
df0$S1_Health_GestationTime = setNA(S1Der[,127],-1:-9) #Pos. = 127	Variable = ADGESTA0	Variable label = S1 DV Cohort Member Gestation time in days (estimated) C1   

df0$S1_Health_DaysPreTerm = setNA(S1Der[,126], -888)*-1 #Reverse coded - Variable = ADERLTA0	Variable label = S1 DV Cohort Member Number of days past due date C1 
df0$S1_Health_DaysPreTerm_Norm = Normalise(df0$S1_Health_DaysPreTerm)

df0$S1_Health_EverTriedBreastFeeding = Convert(S1ParInt[,308],1:2,1:0) #1=YES, 0=NO, Pos. = 308	Variable = ambfeva0	Variable label = S1 MAIN Ever tried to breastfeed C1  
df0$S1_Health_AgeLastHadBreastMilk = Convert(S1ParInt[,310],1:6,c(0,1,1,1,1,1))

df0$S1_Health_HearingProblems = Convert(S1ParInt[,353],1:2,0:1) #1=yes, 0=no - however they also ask if these problems still apply Pos. = 354	Variable = amstpra0	Variable label = S1 MAIN Still problems with hearing C1  

df0$S1_Health_CurrentSmoking = Convert(S1ParInt[,709],c(1:5,95),c(1,0,0,0,0,0)) #(Main parent regardless of sex) Pos. = 709	Variable = amsmus0a	Variable label = S1 MAIN Current smoking  MC1 NOTE- THE PERSON RESPONDING MAY NOT BE THE BIRTH MOTHER
# 
# 
# df0$S1_Health_EverSmokedRegularly = Convert(S1ParInt[,715],1:2,0:1) #Pos. = 715	Variable = amsmev00	Variable label = S1 MAIN Ever smoked REGULALRY 
# 

df0$S1_Health_SmokedLastTwoYears = S1ParInt[,714] #Pos. = 714	Variable = amsmty00	Variable label = S1 MAIN Smoked in last 2 years  
df0$S1_Health_EverSmokedCaution = (S1ParInt[,715]) #Pos. = 715	Variable = amsmev00	Variable label = S1 MAIN Ever smoked  (N.B. they only respond to this if they haven't smoked in the last two years)
df0$S1_Health_SmokingBeforePregnancy = (S1ParInt[,716]) #Pos. = 716	Variable = amcipr00	Variable label = S1 MAIN Number of cigarettes smoked per day before preg  
df0$S1_Health_ChangedSmokingDuringPregnancy = Convert(S1ParInt[,717],1:2,1:0) #Pos. = 717	Variable = amsmch00	Variable label = S1 MAIN Changed number smoked during pregnancy  
df0$S1_Health_WhenChanged = Convert(S1ParInt[,718],-99:99,-99:99) #Pos. = 718	Variable = amwhch00	Variable label = S1 MAIN When changed smoking habits   
df0$S1_Health_NumberSmokedAfterChange = Convert(S1ParInt[,719], c(0:96),c(0:95,.5) ) #Pos. = 719	Variable = amcich00	Variable label = S1 MAIN Number smoked per day after change 

#Created summary variable from here
df0$S1_Health_NaturalMothersNOTSmokedLastTwoYears = Convert(df0$S1_Health_SmokedLastTwoYears, c(-1,1,2),c(0,0,1)) #Natural mothers who have not smoked in the last two years (0) 
  df0$S1_Health_NaturalMothersNOTSmokedLastTwoYears[df0$S1_Main_NaturalMotherInterviewed==0]=NA
  
###Natural mothers split into 3 groups:
df0$S1_NaturalMothersNOTSmokingBeforePregnancy = as.numeric(S1ParInt[,716]==0 | df0$S1_Health_NaturalMothersNOTSmokedLastTwoYears==1) #Mothers who were not smoking before preg or never in the last two years
df0$S1_NaturalMothersStoppedSmokingDuringPregnancy = as.numeric(S1ParInt[,719]==0) #Mothers who reported quiting during pregnancy
df0$S1_NaturalMothersContinuingSmoking = as.numeric( (df0$S1_Health_ChangedSmokingDuringPregnancy==0 & df0$S1_Health_SmokingBeforePregnancy >0 ) | df0$S1_Health_NumberSmokedAfterChange>0 ) #Parents who reported NOT changing smoking habits (and smoked before pregnancy) OR did change habits but continued to smoke

  #Put 3 groups into main variable
  df0$S1_Health_SmokingPregnantDerived = NA
    df0$S1_Health_SmokingPregnantDerived[df0$S1_NaturalMothersNOTSmokingBeforePregnancy==1]=1
    df0$S1_Health_SmokingPregnantDerived[df0$S1_NaturalMothersStoppedSmokingDuringPregnancy==1]=1
        # table(df0$S1_NaturalMothersContinuingSmoking,df0$S1_Health_SmokingPregnantDerived, useNA="always")
    df0$S1_Health_SmokingPregnantDerived[df0$S1_NaturalMothersContinuingSmoking==1]=0
    
df0$S1_Health_SmokedThroughoutPregnancy = df0$S1_Health_SmokingPregnantDerived

#OLD CODE FOR VARIABLE
# df0$S1_Health_SmokedThroughoutPregnancy = as.numeric(S1ParInt[,719]<=0)
#   df0$S1_Health_SmokedThroughoutPregnancy[df0$S1_Main_NaturalMother!=1] = NA  #Only keep natural mother's responses
#   table(df0$S1_Health_SmokedThroughoutPregnancy)



df0$S1_Health_SmokesNearBaby = Convert(S1ParInt[,720],1:2,0:1)

#Alcohol Consumption
df0$S1_Parent_AlcoholCurrent = Convert(S1ParInt[,721],1:7,1:7) #Pos. = 721	Variable = amaldr00	Variable label = S1 MAIN Frequency of current alcohol consumption  
df0$S1_Parent_AlcoholPregnant = Convert(S1ParInt[,724],1:7,1:7) #Pos. = 724	Variable = amdrof00	Variable label = S1 MAIN Frequency of alcohol consumption before preg   --- though looking at the questionnaire documnetation (pg 97) this should refer to alcohol DURING pregnancy!! 


#Parent Health
df0$S1_Parent_GeneralHealth =  Convert(S1ParInt[,666],1:4,4:1) #Pos. = 666	Variable = amgehe00	Variable label = S1 MAIN General Health  
df0$S1_Parent_Illness =  Convert(S1ParInt[,667],1:2,0:1)
df0$S1_Parent_Illness[which(S1ParInt[,674]==1)] = -1 #1=no illness, 0=illness (not limiting) -1=limiting illness
df0$S1_Parent_Health_AvgNorm = Normalise(apply(df0[,c("S1_Parent_GeneralHealth","S1_Parent_Illness")],1, function(x) mean(x, na.rm=TRUE)))

df0$S1_Parent_Mood1 = Convert(S1ParInt[,693],1:2,0:1)
df0$S1_Parent_Mood2 = Convert(S1ParInt[,694],1:2,0:1)
df0$S1_Parent_Mood_AvgNorm = Normalise(apply(df0[,c("S1_Parent_Mood1","S1_Parent_Mood2")],1, function(x) mean(x, na.rm=TRUE)))


#Self completion Section below
df0$S1_amscac00 = S1ParInt[,727] #Pos. = 727	Variable = amscac00	Variable label = S1 MAIN Whether accepted self-completion  



#Parenting Beliefs - originally developed by ALSPAC team 

  df0$S1_Parenting_Beliefs_Cry = Convert(S1ParInt[,783],1:5, 5:1 )  #Pos. = 783	Variable = ampcry00	Variable label = S1 MAIN Picked up whenevr cry  
  df0$S1_Parenting_Beliefs_Sleep = Convert(S1ParInt[,784],1:5, 5:1 )
  df0$S1_Parenting_Beliefs_Stimulation = Convert(S1ParInt[,785],1:5, 5:1 )
  df0$S1_Parenting_Beliefs_Talking = Convert(S1ParInt[,786],1:5, 5:1 )
  df0$S1_Parenting_Beliefs_Cuddling = Convert(S1ParInt[,787],1:5, 5:1 )
  
#Parent Attachment - Condon Maternal Attachment Questionnaire
  df0$S1_Parenting_Attachment_Annoyance = Convert(S1ParInt[,788],1:6, 1:6 )
  df0$S1_Parenting_Attachment_ThinkAboutBaby = Convert(S1ParInt[,789],1:6, 6:1 )
  df0$S1_Parenting_Attachment_FeelingsLeavingBaby = Convert(S1ParInt[,790],1:5, 5:1 )
  df0$S1_Parenting_Attachment_Competance = Convert(S1ParInt[,791],1:4, 1:4 )
  df0$S1_Parenting_Attachment_Patience = Convert(S1ParInt[,792],1:4, 1:4 )
  df0$S1_Parenting_Attachment_Resentment = Convert(S1ParInt[,793],1:4, 1:4 )

#Parent Mental Health - Parental Psychosocial Distress - Rutter Malaise Inventory

  df0$S1_ParentMentalHealth_Tired = Convert(S1ParInt[,794], 1:2,0:1)      #Pos. = 794	Variable = amtire00	Variable label = S1 MAIN Tired most of time
  df0$S1_ParentMentalHealth_Depressed = Convert(S1ParInt[,795], 1:2,0:1)  #Pos. = 795	Variable = amdepr00	Variable label = S1 MAIN Often miserable or depressed  
  df0$S1_ParentMentalHealth_Worried = Convert(S1ParInt[,796], 1:2,0:1) #Pos. = 796	Variable = amworr00	Variable label = S1 MAIN Often worried about things  
  df0$S1_ParentMentalHealth_Rage = Convert(S1ParInt[,797], 1:2,0:1) #Pos. = 797	Variable = amrage00	Variable label = S1 MAIN Often gets in violent rage  
  df0$S1_ParentMentalHealth_Scared = Convert(S1ParInt[,798], 1:2,0:1) #Pos. = 798	Variable = amscar00	Variable label = S1 MAIN Suddenly scared for no good reason  
  df0$S1_ParentMentalHealth_Upset = Convert(S1ParInt[,799], 1:2,0:1) #Pos. = 799	Variable = amupse00	Variable label = S1 MAIN Easily upset or irritated  
  df0$S1_ParentMentalHealth_Jittery = Convert(S1ParInt[,800], 1:2,0:1) #Pos. = 800	Variable = amkeyd00	Variable label = S1 MAIN Constantly keyed up or jittery
  df0$S1_ParentMentalHealth_Nerves = Convert(S1ParInt[,801], 1:2,0:1) #Pos. = 801	Variable = amnerv00	Variable label = S1 MAIN Every little thing gets on nerves  
  df0$S1_ParentMentalHealth_Heart = Convert(S1ParInt[,802], 1:2,0:1) #Pos. = 802	Variable = amhera00	Variable label = S1 MAIN Heart often races like mad   

  #calcFactorScore(var=findMCS("S1_ParentMentalHealth"), verbose=TRUE)
  
#Parent Social Support
  df0$S1_ParentSocialSupport_ShareFeelings = Convert(S1ParInt[,803], 1:5,1:5)     #Pos. = 803	Variable = ampesh00	Variable label = S1 MAIN No-one to share feelings with  
  df0$S1_ParentSocialSupport_OtherTalk = Convert(S1ParInt[,804], 1:5,5:1)     #Pos. = 804	Variable = ampeta00	Variable label = S1 MAIN Other parents can talk to   
  df0$S1_ParentSocialSupport_MoneyHelp = Convert(S1ParInt[,805], 1:5,5:1)     #Pos. = 805	Variable = ampefp00	Variable label = S1 MAIN Family would help if financial problems
  #calcFactorScore(var=findMCS("S1_ParentSocial"), verbose=TRUE)
  
#Parent Relationship Quality
  #Golombok Rust Inventory of Marital Stat
  df0$S1_ParentRelationshipQuality_Sensitive = Convert(S1ParInt[,806], 1:5,5:1)     #Pos. = 806	Variable = amrese00	Variable label = S1 MAIN Partner sensitive and aware of needs   
  df0$S1_ParentRelationshipQuality_Listens = Convert(S1ParInt[,807], 1:5,1:5)     #Pos. = 807	Variable = amreis00	Variable label = S1 MAIN Partner doesnt listen  
  df0$S1_ParentRelationshipQuality_Lonely = Convert(S1ParInt[,808], 1:5,1:5)     #Pos. = 808	Variable = amrelo00	Variable label = S1 MAIN Sometime lonely when with partner  
  df0$S1_ParentRelationshipQuality_Joy = Convert(S1ParInt[,809], 1:5,5:1)     #Pos. = 809	Variable = amrejo00	Variable label = S1 MAIN Relationship full of joy and excitement  
  df0$S1_ParentRelationshipQuality_Warmth = Convert(S1ParInt[,810], 1:5,1:5)     #Pos. = 810	Variable = amrewa00	Variable label = S1 MAIN Wishes was more warmth and affection  
  df0$S1_ParentRelationshipQuality_Seperation = Convert(S1ParInt[,811], 1:5,1:5)     #Pos. = 811	Variable = amresn00	Variable label = S1 MAIN Suspects on brink of separation  
  df0$S1_ParentRelationshipQuality_Reconcilate = Convert(S1ParInt[,812], 1:5,5:1)     #Pos. = 812	Variable = ammaup00	Variable label = S1 MAIN Can make up quickly after argument  
  
  #These don't seem to be from the above scale
  df0$S1_ParentRelationshipQuality_EffectBaby = Convert(S1ParInt[,813], 1:4,c(3,1,2))     #Pos. = 813	Variable = amclos00	Variable label = S1 MAIN Effect of having baby on relationship  
  df0$S1_ParentRelationshipQuality_DatesFreq = Convert(S1ParInt[,814], 1:4,4:1)     #Pos. = 814	Variable = amcolt00	Variable label = S1 MAIN Frequency go out as a couple  
  df0$S1_ParentRelationshipQuality_Happy = Convert(S1ParInt[,815], 1:7,1:7)     #Pos. = 815	Variable = amhare00	Variable label = S1 MAIN Happy/Unhappy with relationship   
  df0$S1_ParentRelationshipQuality_PartUseForce = Convert(S1ParInt[,816], 1:2,0:1)     #Pos. = 816	Variable = amforc00	Variable label = S1 MAIN Partner ever used force  


#Parent Self Esteem 

  #Rosenberg Self Esteem Inventory
  
  df0$S1_ParentSelfEsteem_Satisfied = Convert(S1ParInt[,949],1:4,4:1) #Pos. = 949	Variable = amsati00	Variable label = S1 MAIN Satisfied with self 
  df0$S1_ParentSelfEsteem_NoGood = Convert(S1ParInt[,950],1:4,1:4)    #Pos. = 950 Variable = amgood00 Variable label = S1 MAIN Thinks is no good at all at times  
  df0$S1_ParentSelfEsteem_Able = Convert(S1ParInt[,951],1:4,4:1)      #Pos. = 951 Variable = amwell00 Variable label = S1 MAIN Able to do things as well as others  
  df0$S1_ParentSelfEsteem_Useless = Convert(S1ParInt[,952],1:4,1:4)   #Pos. = 952 Variable = amusel00 Variable label = S1 MAIN Feels useless at times  
  df0$S1_ParentSelfEsteem_Failure = Convert(S1ParInt[,953],1:4,1:4)   #Pos. = 953 Variable = amfail00 Variable label = S1 MAIN Feels is a failure  
  df0$S1_ParentSelfEsteem_Positive = Convert(S1ParInt[,954],1:4,4:1)  #Pos. = 954 Variable = amposi00 Variable label = S1 MAIN Has positive attitude towards self  

#Locus of Control
  df0$S1_LocusControl_1 = Convert(S1ParInt[,955],1:2,0:1)      #Pos. = 955	Variable = amwant00	Variable label = S1 MAIN Gets what wants out of life  
  df0$S1_LocusControl_2 = Convert(S1ParInt[,956],1:2,1:0)      #Pos. = 956	Variable = amcont00	Variable label = S1 MAIN Control over life  
  df0$S1_LocusControl_3 = Convert(S1ParInt[,957],1:2,1:0)      #Pos. = 957	Variable = amruli00	Variable label = S1 MAIN Can run own life  
  
  
  
#Life Satisfaction
  df0$S1_ParentLifeSatisfaction = Convert(S1ParInt[,958],1:10,1:10) #Pos. = 958	Variable = amwali00	Variable label = S1 MAIN Life satisfaction  


#Self-Rated Reading/Math Ability 

df0$S1_ParentLiteracy_ReadStoryBooks = Convert(S1ParInt[,1118],1:3,3:1) #Pos. = 1118	Variable = amread00	Variable label = S1 MAIN Reading ability - childrens storybook  
df0$S1_ParentLiteracy_ReadForms = Convert(S1ParInt[,1119],1:3,3:1) #Pos. = 1119	Variable = amform00	Variable label = S1 MAIN Reading ability - forms  
df0$S1_ParentLiteracy_Numeric = Convert(S1ParInt[,1120],1:3,3:1)   #Pos. = 1120	Variable = ammath00	Variable label = S1 MAIN Numerical ability - change in shops  
df0$S1_ParentLiteracy_DayActivities = Convert(S1ParInt[,1121],c(-1,1,2),c(3,1,2))  #only applicable to those with problems above - Pos. = 1121	Variable = ammana00	Variable label = S1 MAIN Problems with day-to-day activities  

df0$S1_ParentLiteracyProblems = (df0$S1_ParentLiteracy_ReadStoryBooks == 3 & df0$S1_ParentLiteracy_ReadForms == 3 & df0$S1_ParentLiteracy_Numeric == 3)


#Parent Neighbourhood satisfaction
#df0$S1_Parent_Neighbourhood_1 = Convert(S1ParInt[,1167],1:5,5:1) #Pos. = 1167	Variable = amhosa00	Variable label = S1 MAIN Satisfaction with home  
df0$S1_Parent_Neighbourhood_2 = Convert(S1ParInt[,1168],1:5,5:1) #Pos. = 1168	Variable = amarea00	Variable label = S1 MAIN Satisfaction with area   
df0$S1_Parent_Neighbourhood_3 = Convert(S1ParInt[,1169],1:4,1:4) #Pos. = 1169	Variable = amarnn00	Variable label = S1 MAIN Noisy neighbours  
df0$S1_Parent_Neighbourhood_4 = Convert(S1ParInt[,1170],1:4,1:4) #Pos. = 1170	Variable = amarru00	Variable label = S1 MAIN Rubbish and litter  
df0$S1_Parent_Neighbourhood_5 = Convert(S1ParInt[,1171],1:4,1:4) #Pos. = 1171	Variable = amarvd00	Variable label = S1 MAIN Vandalism and damage to property  
df0$S1_Parent_Neighbourhood_6 = Convert(S1ParInt[,1172],1:4,1:4) #Pos. = 1172	Variable = amarrc00	Variable label = S1 MAIN Racist insults or attacks  
# df0$S1_Parent_Neighbourhood_7 = Convert(S1ParInt[,1173],1:4,1:4) #Pos. = 1173	Variable = amarsr00	Variable label = S1 MAIN Religion-based insults or attacks  - NOT INCLUDED AS THIS ONLY APPLIED TO NORTHERN IRELAND!!
df0$S1_Parent_Neighbourhood_8 = Convert(S1ParInt[,1174],1:4,1:4) #Pos. = 1174	Variable = amtran00	Variable label = S1 MAIN Poor public transport  
df0$S1_Parent_Neighbourhood_9 = Convert(S1ParInt[,1175],1:4,4:1) #Pos. = 1175	Variable = amshop00	Variable label = S1 MAIN Food shops/supermarkets in easy access  
df0$S1_Parent_Neighbourhood_10 = Convert(S1ParInt[,1176],1:4,1:4) #Pos. = 1176	Variable = amarpg00	Variable label = S1 MAIN Pollution, grime, environmental problems  
df0$S1_Parent_Neighbourhood_11 = Convert(S1ParInt[,1177],1:2,1:0) #Pos. = 1177	Variable = amplsa00	Variable label = S1 MAIN Any places where children can play safely  

# df0$S1_Parent_Neighbourhood_FSN = Normalise(calcFactorScore(var=paste0("S1_Parent_Neighbourhood_",1:11),MissingTolerance = 3, verbose=TRUE))

#Housing & Livig Standards
df0$S1_Housing_Satisfaction = Convert(S1ParInt[,1167],1:5,5:1) #Pos. = 1167	Variable = amhosa00	Variable label = S1 MAIN Satisfaction with home  
df0$S1_Housing_Garden = Convert(S1ParInt[,1142], 1:3,c(1,1,0)) #Pos. = 1142 Variable = amgdac00 Variable label = S1 MAIN Access to garden 
df0$S1_Housing_Cold = Convert(S1ParInt[,1147], 1:5, c(1,1,1,0,0)) #Pos. = 1147	Variable = amtempa0	Variable label = S1 MAIN Temp. in babys room at coldest time of year  
df0$S1_Housing_Damp = Convert(S1ParInt[,1150], 1:2, 0:1) #Pos. = 1150	Variable = amdamp00	Variable label = S1 MAIN Damp or condensation  
df0$S1_Housing_Phone = Convert(S1ParInt[,1151], 1:3, c(1,1,0)) #Pos. = 1151	Variable = amphon00	Variable label = S1 MAIN Working telephone  
#   S1_Housing_Vehicle = S1ParInt[,1165] #Pos. = 1165	Variable = amcaru00	Variable label = S1 MAIN Vehicle useage  
#   df0$S1_Housing_VehicleNumber = Convert(S1ParInt[,1166],1:10,c(1,2,3,3,3,3,3,3,3,3))
# df0$S1_Housing_VehicleNumber[S1_Housing_Vehicle==2] = 0 #Set to 0 if they have no car
 
# Early Developmental Milestones  - Developmental Milestones - Denver Developmental Screening Test (For Edwin's Analysis)

df0$S1_Development_Smiles = Convert(S1ParInt[,515],1:3,3:1) #Pos. = 515	Variable = amsmila0	Variable label = S1 MAIN Development: smiles C1
df0$S1_Development_Sits = Convert(S1ParInt[,516],1:3,3:1) #Pos. = 516	Variable = amsitua0	Variable label = S1 MAIN Development: sits up C1
df0$S1_Development_Stands = Convert(S1ParInt[,517],1:3,3:1) #Pos. = 517	Variable = amstana0	Variable label = S1 MAIN Development: stands up holding on C1
df0$S1_Development_Hands = Convert(S1ParInt[,518],1:3,3:1) # Pos. = 518	Variable = amhanda0	Variable label = S1 MAIN Development: hands together C1  
df0$S1_Development_GrabsObjects = Convert(S1ParInt[,519],1:3,3:1) #Pos. = 519	Variable = amgraba0	Variable label = S1 MAIN Development: grabs objects C1  
df0$S1_Development_Holds = Convert(S1ParInt[,520],1:3,3:1) # Pos. = 520	Variable = ampicka0	Variable label = S1 MAIN Development: holds small objects C1  
df0$S1_Development_PassToy = Convert(S1ParInt[,521],1:3,3:1) #Pos. = 521	Variable = amptoya0	Variable label = S1 MAIN Development: passes a toy C1  
df0$S1_Development_Walks = Convert(S1ParInt[,522],1:3,3:1) #Pos. = 522	Variable = amwalka0	Variable label = S1 MAIN Development: walks a few steps C1  
df0$S1_Development_GivesToy = Convert(S1ParInt[,523],1:3,3:1) # Pos. = 523	Variable = amgivea0	Variable label = S1 MAIN Development: gives toy C1  
df0$S1_Development_Waves = Convert(S1ParInt[,524],1:3,3:1) # Pos. = 524	Variable = amwavea0	Variable label = S1 MAIN Development: waves bye-bye C1  
df0$S1_Development_ExtendsArmts = Convert(S1ParInt[,525],1:3,3:1) # Pos. = 525	Variable = amarmsa0	Variable label = S1 MAIN Development: extends arms C1  
df0$S1_Development_Nods = Convert(S1ParInt[,526],1:3,3:1) # Pos. = 526	Variable = amnodsa0	Variable label = S1 MAIN Development: nods for yes C1
df0$S1_Development_Move = Convert(S1ParInt[,527],1:2,1:0) # Pos. = 527	Variable = ammovea0	Variable label = S1 MAIN Can move from place to place C1  







#Misc variables
# S1hhgrid = fread("UKDA-4683-tab MCS1/tab/mcs1_hhgrid.tab", stringsAsFactors = FALSE, data.table = FALSE)
# S1hhgrid = S1hhgrid[S1hhgrid$AHCNUM00==1,]
# S1hhgrid = S1hhgrid[match(df0$MCSID, S1hhgrid$MCSID),]
# 
# df0$S1_ChildBirthMonth = Convert(S1hhgrid[,6],1:12,1:12)
# df0$S1_ChildBirthYear = setNA(S1hhgrid[,7],-1:-9)
# df0$S1_ChildRelativeAge = Convert(S1hhgrid[,6],c(9:12,1:8),12:1) #Relative age. 1= born in august (youngest) 12= born in july
# 



#remove dataframes
rm(S1Der, pmh, prq,S1ParInt,pls, S1Geo)
#######################################################################################################################
####################################################     MCS 2    #################################################### 
#######################################################################################################################

#Derived Info
S2Der = fread("UKDA-5350-tab MCS2/tab/mcs2_derived_variables.tab", stringsAsFactors = FALSE, data.table = FALSE)
S2Der = S2Der[match(df0$MCSID, S2Der$MCSID),]

df0$S2_NumberParents = Convert(S2Der[,13], 1:2, 1:0) #Pos. = 13	Variable = BDHTYS00	Variable label = S2 DV Summary of Parents/Carers in Household
df0$S2_NaturalMotherinHH = S2Der[,14] #Pos. = 14	Variable = BDMINH00	Variable label = S2 DV Natural mother in HH  
df0$S2_NaturalFatherinHH = S2Der[,15] #Pos. = 15	Variable = BDFINH00	Variable label = S2 DV Natural father in HH  
 
df0$S2_Ethnicity_England_BDCEEAA0  = S2Der$BDCEEAA0 # BDCEEAA0 S2 DV Cohort Member Ethnic Group (England) – inc new families C1
df0$S2_Ethnicity_Wales_BDCEWAA0    = S2Der$BDCEWAA0
df0$S2_Ethnicity_Scotland_BDCESAA0 = S2Der$BDCESAA0
df0$S2_Ethnicity_NI_BDCENAA0       = S2Der$BDCENAA0


df0$S2_SES_PovertyIndicator_BDOEDP00 = S2Der$BDOEDP00
df0$S2_SES_FamilyIncomeBanded_BDHINC00 = S2Der$BDHINC00
df0$S2_SES_HousingTenure_BDROOW00 = S2Der$BDROOW00
df0$S2_SES_Main_Education_BMDNVQ00 = S2Der$BMDNVQ00



# table(duplicated(S2HouseholdGrid$MCSID))


#Cognitive Variables
S2Cog = fread("UKDA-5350-tab MCS2/tab/mcs2_child_assessment_data.tab", stringsAsFactors = FALSE, data.table = FALSE)
S2Cog = S2Cog[S2Cog$bhcnum00==1,]
S2Cog = S2Cog[match(df0$MCSID, S2Cog$mcsid),]


df0$S2_Sex_bhcsex00 = S2Cog$bhcsex00
df0$S2_Sex = swapsies(S2Cog[,3], 1:2, 0:1) #1 = female 0=male (no "not applicable" cases)
df0$S2_Age = as.numeric(setNA(S2Cog[6],-2)/365.25) #Age at interview in years. NA = No interview 
df0$S2_Age_bhcage00 = S2Cog$bhcage00

df0$S2_Main_Education_BMDNVQ00 = S2Der$BMDNVQ00
df0$S2_Part_Education_BPDNVQ00 = S2Der$BPDNVQ00 

df0$S2_Main_Identity_BMDRES00 = S2Der$BMDRES00
df0$S2_Part_Identity_BPDRES00 = S2Der$BPDRES00

#Interviewer Observations
#for below: 3 = Not Observed (set as NA)


df0$S2_HomeObs_IsHomeSafe = Convert(S2Cog[560],1:2,0:1)            #Pos. = 560	Variable = bcenvi00	Variable label = S2 COG Child's in-home play environment safe
df0$S2_HomeObs_ParentProvideToys =  Convert(S2Cog[561],1:2,0:1)    #Pos. = 561	Variable = bctoys00	Variable label = S2 COG Parent provided toys during visit
df0$S2_HomeObs_KeptChildInVision = Convert(S2Cog[562],1:2,0:1)     #Pos. = 562	Variable = bcseec00	Variable label = S2 COG Parent kept child in vision
df0$S2_HomeObs_IsHomeDark =Convert(S2Cog[564],1:2,0:1)            # Pos. = 564	Variable = bcdark00	Variable label = S2 COG Interior of home dark
df0$S2_HomeObs_IsHomeClean = Convert(S2Cog[565],1:2,1:0)          # Pos. = 565	Variable = bcrcle00	Variable label = S2 COG House/flat reasonably clean
df0$S2_HomeObs_IsHomeUncluttered = Convert(S2Cog[566],1:2,1:0)    # Pos. = 566	Variable = bcuncl00	Variable label = S2 COG House/flat reasonably uncluttered
df0$S2_HomeObs_IsHomeNoisy = Convert(S2Cog[567],1:3,3:1)          #Pos. = 567	Variable = bctvns00	Variable label = S2 COG Noise from tv/radio 3=noise, 2= moderate noise, 1= no noise


#Higher = better
df0$S2_ParentObs_MotherPositive = Convert(S2Cog[582],1:2,1:0)         #Pos. = 582	Variable = bcspea00	Variable label = S2 COG Mother's voice positive when speaking to child
df0$S2_ParentObs_MotherTalksToChild = Convert(S2Cog[583],1:2,1:0)     #Pos. = 583	Variable = bcmcon00	Variable label = S2 COG Mother converses at least twice with child
df0$S2_ParentObs_MotherAnswersChild = Convert(S2Cog[584],1:2,1:0)     #Pos. = 584	Variable = bcansw00	Variable label = S2 COG Mother answers child's questions verbally
df0$S2_ParentObs_MotherPraisesChild = Convert(S2Cog[585],1:2,1:0)     #Pos. = 585	Variable = bcprai00	Variable label = S2 COG Mother praises child spontaneously
df0$S2_ParentObs_MotherKissesChild = Convert(S2Cog[586],1:2,1:0)      #Pos. = 586	Variable = bckiss00	Variable label = S2 COG Mother caresses or kisses child
df0$S2_ParentObs_MotherIntroducesChild = Convert(S2Cog[587],1:2,1:0)  #Pos. = 587	Variable = bcinti00	Variable label = S2 COG Mother introduces interviewer to child
df0$S2_ParentObs_MotherScoldsChild = Convert(S2Cog[588],1:2,1:0)      #Pos. = 588	Variable = bcscol00	Variable label = S2 COG Mother scolded child more than once
df0$S2_ParentObs_MotherRestrain = Convert(S2Cog[589],1:2,1:0)         #Pos. = 589	Variable = bcphys00	Variable label = S2 COG Mother used physical restraint on child
df0$S2_ParentObs_MotherSlapped = Convert(S2Cog[590],1:2,1:0)          #Pos. = 590	Variable = bcslap00	Variable label = S2 COG Mother slapped or spanked child

#Parent Questionnaires
S2Par = fread("UKDA-5350-tab MCS2/tab/mcs2_parent_interview.tab", stringsAsFactors = FALSE, data.table = FALSE)
S2Par = S2Par[match(df0$MCSID, S2Par$mcsid),]

#Home Langauge Use
df0$S2_Misc_LanguagesSpokenAtHome = Convert(S2Par[34],1:3,1:3) #1=just eng 2= multiple 3=only other language Pos. = 34	Variable = bhhlan00	Variable label = S2 HHQ Language spoken at home
df0$S2_Misc_EngSpokenHome = Convert(S2Par[34],1:3,c(1,1,0))
df0$S2_Misc_EngAndOtherLangSpokenHome = Convert(S2Par[34],1:3,c(0,1,0))
df0$S2_Misc_EnglishMonolingualHome = Convert(S2Par[34],1:3,c(1,0,0)) #Pos. = 34	Variable = bhhlan00	Variable label = S2 HHQ Language spoken at home

#Home learning environment
df0$S2_Parenting_ReadToChild = Convert(S2Par[352],1:6,6:1) #Pos. = 352	Variable = bmofrea0	Variable label = S2 MAIN How often do you read to the child C1
df0$S2_Parenting_TakeToLibrary = Convert(S2Par[356],1:4,1:4) #Pos. = 356	Variable = bmoflia0	Variable label = S2 MAIN How often child taken to library C1
df0$S2_Parenting_TakeToLibrary[S2Par[355]==2]=0
table(df0$S2_Parenting_TakeToLibrary)
#df0$S2_Parenting_HelpWithSport = setNA(S2Par[357],-1)
df0$S2_Parenting_HelpWithAlphabet =  Convert(S2Par[359],1:7,1:7) #Pos. = 359	Variable = bmofaba0	Variable label = S2 MAIN How often help child learn alphabet C1
df0$S2_Parenting_HelpWithAlphabet[S2Par[358]==2]=0
df0$S2_Parenting_HelpWithCounting = Convert(S2Par[361],1:7,1:7) #Pos. = 361	Variable = bmofcoa0	Variable label = S2 MAIN How often at home try to teach child counting C1
df0$S2_Parenting_HelpWithCounting[S2Par[360]==2]=0
df0$S2_Parenting_TeachSongsPoems = Convert(S2Par[363],1:7,1:7) #Pos. = 363	Variable = bmofsoa0	Variable label = S2 MAIN How often teach child songs/poems/rhymes C1
df0$S2_Parenting_TeachSongsPoems[S2Par[262]==2]=0
df0$S2_Parenting_EatenWithFamily = Convert(S2Par[366],1:2,1:0) #Pos. = 366	Variable = bmeatwa0	Variable label = S2 MAIN Child eaten with family past week C1

#Parenting beliefs
df0$S2_Parenting_Competence = setNA(S2Par[2866], c(-1,6)) #I feel that I am: 1 Notverygoodatbeingaparent 2 Apersonwhohassometroublebeingaparent 3 Anaverageparent 4 Abetterthanaverageparent 5 Averygoodparent 6 Can’tsay

#Recreational drug use
df0$S2_Parent_RecreationalDrugUse = Convert(S2Par[,1859], 1:3,c(0,0,1)) #Pos. = 1859	Variable = bmdrug00	Variable label = S2 MAIN Used recreational drugs
#ProblemDrinking
df0$S2_Parent_Alcoholism_FeltShouldCutDown = Convert(S2Par[,1860], 1:2,0:1) #Pos. = 1860	Variable = bmcaam00	Variable label = S2 MAIN Ever felt should cut down on drinking
df0$S2_Parent_Alcoholism_Criticised = Convert(S2Par[,1861], 1:2,0:1) #Pos. = 1861	Variable = bmcacr00	Variable label = S2 MAIN People annoyed you by criticising your drinking
df0$S2_Parent_Alcoholism_FeltGuilty = Convert(S2Par[,1862], 1:2,0:1) #Pos. = 1862	Variable = bmcagu00	Variable label = S2 MAIN Ever felt bad/guilty about drinking
df0$S2_Parent_Alcoholism_MorningDrink = Convert(S2Par[,1863], 1:2,0:1) #Pos. = 1863	Variable = bmcamo00	Variable label = S2 MAIN Ever had drink first thing in morning

#Strengths and Difficulties Questionnaire

SDQ = convertSDQ(S2Par[,1652:1676], varnames="S2_SDQ_")
df0 = cbind.data.frame(df0,SDQ)

#Random Personality Questions - like SDQ 1=NOT TRUE 3- CERTAINLY TRUE
  df0$S2_Behav_WorkThingsOutForSelf = setNA(S2Par[1642], c(-1:-9, 4))
  df0$S2_Behav_MoodSwings = setNA(S2Par[1643], c(-1:-9, 4))
  df0$S2_Behav_DoesNotNeedMuchHelpWithTasks = setNA(S2Par[1644], c(-1:-9, 4))
  df0$S2_Behav_GetsOverExcited = setNA(S2Par[1645], c(-1:-9, 4))
  df0$S2_Behav_ChoosesActivitiesOnTheirOwn = setNA(S2Par[1646], c(-1:-9, 4))
  df0$S2_Behav_EasilyFustrated = setNA(S2Par[1647], c(-1:-9, 4))
  df0$S2_Behav_GetsOverBeingUpset = setNA(S2Par[1648], c(-1:-9, 4))
  df0$S2_Behav_PersistsDifficultTasks = setNA(S2Par[1649], c(-1:-9, 4))
  df0$S2_Behav_MoveToNewActivity = setNA(S2Par[1650], c(-1:-9, 4))
  df0$S2_Behav_ActsImpulsively = setNA(S2Par[1651], c(-1:-9, 4))

#Parenting Values - This is a scale that was developed at Institute of Education. This question was asked to the main respondent only.
  #not used due to VERY POOR reliability!
  S2_ParentingValues = S2Par[,370:376]
  apply(S2_ParentingValues,2,table)
  df0$S2_Parenting_Values_Obedience = Convert(S2Par[370],1:2,1:0) #Pos. = 370	Variable = bmobrea0	Variable label = S2 MAIN Values to instil obedience and respect C1
  df0$S2_Parenting_Values_Negotiation = Convert(S2Par[371],1:2,1:0)
  df0$S2_Parenting_Values_Respect = Convert(S2Par[372],1:2,1:0)
  df0$S2_Parenting_Values_School = Convert(S2Par[373],1:2,1:0)
  df0$S2_Parenting_Values_Religion = Convert(S2Par[374],1:2,1:0)
  df0$S2_Parenting_Values_Rules = Convert(S2Par[375],1:2,1:0)
  df0$S2_Parenting_Values_Enforcement = Convert(S2Par[376],1:2,1:0)
  
#Discipline Practices 
  #Other questions in the Strauss conflict scale were not of interest! 
  df0$S2_HarshParenting_Ignore = Convert(S2Par[1772],1:5,5:1) #Pos. = 1772	Variable = bmdiig00	Variable label = S2 MAIN Ignore child if being naughty
  df0$S2_HarshParenting_Smack = Convert(S2Par[1773],1:5,5:1) #Pos. = 1773	Variable = bmdism00	Variable label = S2 MAIN Smack child if being naughty
  df0$S2_HarshParenting_Shouting = Convert(S2Par[1774],1:5,5:1) #Pos. = 1774	Variable = bmdish00	Variable label = S2 MAIN Shout at child if being naughty

# Mental Health Kessler K6
  
  S2_Kessler = S2Par[,1779:1784]
  S2_Kessler = apply(S2_Kessler,2,function(x) Convert(x, 1:5,1:5))
  colnames(S2_Kessler) = paste0("S2_ParentMentalHealth_",1:6)
  
  
  df0=cbind.data.frame(df0,S2_Kessler)
  rm(S2_Kessler)

#Neighbourhood Assessment 
S2Neighbourhood = fread("UKDA-5350-tab MCS2/tab/mcs2_neighbourhood_assessment_data.tab", stringsAsFactors = FALSE, data.table=FALSE)
#Because this was taken multiple times per family i've had to preprocess it first then average across

S2Neighbourhood$S2_NeighRating_Condition = swapsies(setNA(S2Neighbourhood[5],8), 1:4, 4:1) #All recoded so higher is better
S2Neighbourhood$S2_NeighRating_Blinds = swapsies(setNA(S2Neighbourhood[6],8), 1:3, 3:1)
S2Neighbourhood$S2_NeighRating_TrafficCalm = swapsies(setNA(S2Neighbourhood[7],8), 1:2, 2:1)
S2Neighbourhood$S2_NeighRating_TrafficVol = swapsies(setNA(S2Neighbourhood[8],8), 1:4, 4:1)
S2Neighbourhood$S2_NeighRating_Burntcars = setNA(S2Neighbourhood[9],8)
S2Neighbourhood$S2_NeighRating_Litter = swapsies(setNA(S2Neighbourhood[10],8), 1:3, 3:1)
S2Neighbourhood$S2_NeighRating_DogMess = swapsies(setNA(S2Neighbourhood[11],8), 1:3, 3:1)
S2Neighbourhood$S2_NeighRating_Graffiti = swapsies(setNA(S2Neighbourhood[12],8), 1:3, 3:1)
S2Neighbourhood$S2_NeighRating_Vandalism = swapsies(setNA(S2Neighbourhood[13],8), 1:2, 2:1)
S2Neighbourhood$S2_NeighRating_Fighting = swapsies(setNA(S2Neighbourhood[14],8), 1:4, 4:1)
S2Neighbourhood$S2_NeighRating_Feel = swapsies(setNA(S2Neighbourhood[15],8), 1:6, 6:1)

#Average scores over multiple visits (everytime a interviewer visited a place they would score)

df1=split(S2Neighbourhood[,17:27],S2Neighbourhood$mcsid)
df2=as.data.frame(t(as.data.frame(lapply(df1, function(df) apply(df, 2, function(x) mean(x, na.rm=TRUE))))), stringsAsFactors=FALSE)
df2 = df2[match(df0$MCSID, rownames(df2)),]

df0=cbind(df0,df2)

df0$S2_NeighRating_FactorScore = fa(df2)$scores

#Remove datasets
rm(S2Par,S2Neighbourhood,S2Cog,df1,df2)

# 
# #######################################################################################################################
# ####################################################     MCS 3     #################################################### 
# #######################################################################################################################
# 
# 
#MC3 - SES variables

#Miscealleanous
S3Ses = fread("UKDA-5795-tab MCS3/tab/mcs3_parent_interview.tab", stringsAsFactors = FALSE, data.table = FALSE)
S3Ses = S3Ses[match(df0$MCSID, S3Ses$mcsid),]

#SDQ - only completed by "main" 

S3_SDQ = convertSDQ(S3Ses[,2994:3018])
colnames(S3_SDQ) = paste0("S3_Parent_",colnames(S3_SDQ))

df0 = cbind.data.frame(df0,S3_SDQ)


rm(S3Ses, S3_SDQ)



# 
# df0$S3_SES_FeePayingSchool =       swapsies(setNA(S3Ses[817], -1:-9), input=1:2, replacement = 1:0) #0=no, 1=yes [switched]
# df0$S3_SES_EverAttendedChildcare = swapsies(setNA(S3Ses[822], -1:-9), input=1:6, replacement=c(1,1,1,1,1,0)) #1=yes, 2=no
# 
# df0$S3_SES_FreeSchoolMeal_DontUse = Convert(S3Ses[,1908],1:2,1:0)  #Pos. = 1908	Variable = cmfrema0	Variable label = S3 MAIN: Whether school meals paid for or free C1 !!! Parent report - also not clear if the question differentiates between kids who are ELIGIBLE for free school meals but do not take them.
# 
# table(S3Ses[,1908])
# #Parenting - Help with school
# 
# df0$S3_Parenting_HelpReading = swapsies(setNA(S3Ses[1336], -1:-9), 1:6, 6:1) #Recoded - Variable = cmalwha0	Variable label = S3 MAIN How often CM helped with reading C1
# df0$S3_Parenting_HelpReading[S3Ses[,1335]==2]=1 #Set response to 1 if parent previously responded that no help is given.
# 
# df0$S3_Parenting_HelpWriting = swapsies(setNA(S3Ses[1338], -1:-9), 1:6, 6:1) #Recoded - Variable = cmalwha0	Variable label = S3 MAIN How often CM helped with reading C1
# df0$S3_Parenting_HelpWriting[S3Ses[,1337]==2]=1 #Set response to 1 if parent previously responded that no help is given.
# 
# df0$S3_Parenting_HelpMath = swapsies(setNA(S3Ses[1340], -1:-9), 1:6, 6:1) #Recoded - Variable = cmalwha0	Variable label = S3 MAIN How often CM helped with reading C1
# df0$S3_Parenting_HelpMath[S3Ses[,1339]==2]=1 #Set response to 1 if parent previously responded that no help is given.
# 
# df0$S3_Parenting_HelpSchool_FSN = Normalise(calcFactorScore(var=c("S3_Parenting_HelpReading","S3_Parenting_HelpWriting","S3_Parenting_HelpMath"), verbose=TRUE))
# 
# #Parenting - Activities
# df0$S3_Parenting_Activities_Stories = swapsies(setNA(S3Ses[,1601],-1:-9),1:6,6:1) #Variable = cmreofa0	Variable label = S3 MAIN: How often reads to CM C1
# df0$S3_Parenting_Activities_Music = swapsies(setNA(S3Ses[,1602],-1:-9),1:6,6:1)
# df0$S3_Parenting_Activities_Draw = swapsies(setNA(S3Ses[,1603],-1:-9),1:6,6:1)
# df0$S3_Parenting_Activities_PhysicalPlay = swapsies(setNA(S3Ses[,1604],-1:-9),1:6,6:1)
# df0$S3_Parenting_Activities_ToyPlay = swapsies(setNA(S3Ses[,1605],-1:-9),1:6,6:1)
# df0$S3_Parenting_Activities_ParkPlay = swapsies(setNA(S3Ses[,1606],-1:-9),1:6,6:1)
# 
# df0$S3_Parenting_Activities_FSN = Normalise(calcFactorScore(var=c("S3_Parenting_Activities_Stories","S3_Parenting_Activities_Music","S3_Parenting_Activities_Draw","S3_Parenting_Activities_PhysicalPlay","S3_Parenting_Activities_ToyPlay","S3_Parenting_Activities_ParkPlay"), verbose=TRUE))
# 
# #Parent neighbourhood rating
# 
# df0$S3_ParentQ_Neigh_GoodAreaChildren = Convert(S3Ses[,2935],1:5, 5:1) #Pos. = 2935	Variable = cmargd00	Variable label = S3 MAIN: Whether good area for raising children
# df0$S3_ParentQ_Neigh_FriendsOtherParents = Convert(S3Ses[,2936],1:2, 1:0)
# df0$S3_ParentQ_Neigh_FriendsLiveInArea = Convert(S3Ses[,2937],1:4, c(1,1,1,0))
# df0$S3_ParentQ_Neigh_Safe = Convert(S3Ses[,2938],1:5, 5:1)
# df0$S3_ParentQ_Neigh_Racism = Convert(S3Ses[,2939],1:4, 1:4)
# df0$S3_ParentQ_Neigh_ReligiousAttacks = Convert(S3Ses[,2940],1:4,1:4) #Lots of missing data so not included in fa
# df0$S3_ParentQ_Neigh_FSN = calcFactorScore(var=c("S3_ParentQ_Neigh_GoodAreaChildren","S3_ParentQ_Neigh_Safe","S3_ParentQ_Neigh_Racism"),verbose=TRUE)
# cronbach.alpha(na.omit(df0[,c("S3_ParentQ_Neigh_GoodAreaChildren","S3_ParentQ_Neigh_Safe","S3_ParentQ_Neigh_Racism")]))
# 
# #Mini household choas scale
# 
# df0$S3_Parenting_Chaos_Disorganised = Convert(S3Ses[,2932],1:5, 1:5)#Pos. = 2932	Variable = cmhodi00	Variable label = S3 MAIN: Home is really disorganised
# df0$S3_Parenting_Chaos_CantHearYrself = Convert(S3Ses[,2933],1:5, 1:5) #Pos. = 2933	Variable = cmhoth00	Variable label = S3 MAIN: Cannot hear yourself think at home
# df0$S3_Parenting_Chaos_CalmAtmosphere = Convert(S3Ses[,2934],1:5, 5:1) #Pos. = 2934	Variable = cmhoca00	Variable label = S3 MAIN: Atmosphere is calm at home
# 
# df0$S3_Parenting_Chaos_FSN = Normalise(calcFactorScore(var=c("S3_Parenting_Chaos_Disorganised","S3_Parenting_Chaos_CantHearYrself","S3_Parenting_Chaos_CalmAtmosphere")))
# cronbach.alpha(na.omit(df0[,c("S3_Parenting_Chaos_Disorganised","S3_Parenting_Chaos_CantHearYrself","S3_Parenting_Chaos_CalmAtmosphere")]))
# 
# #Child Health
# 
# df0$S3_Health_GeneralHealth =  Convert(S3Ses[,1622], 1:5, 5:1) #Variable = cmcghea0	Variable label = S3 MAIN: CM's general level of health C1
# df0$S3_Health_LongIllness = Convert(S3Ses[,1623], 1:2, 1:2) #Pos. = 1623	Variable = cmclsia0	Variable label = S3 MAIN: Whether CM has longstanding illness C1
# 
# df0$S3_Health_FruitPortions = Convert(S3Ses[,1962],0:3, 0:3)
# df0$S3_Health_DaysEatsBreakfast = Convert(S3Ses[,1906], 0:7, 0:7)
# 
# df0$S3_Health_FSN = Normalise(calcFactorScore(var=c("S3_Health_GeneralHealth","S3_Health_LongIllness"), verbose=TRUE))
# 
# 
# #Alpha .45 cronbach.alpha(na.omit(df0[,c("S3_Health_GeneralHealth","S3_Health_LongIllness")]))
# 
# table(S3Ses[,1904],S3Ses[,1905])
# 
# 
# #Index Multiple Deprivation
# S3Neigh = fread("UKDA-5795-tab MCS3/tab/mcs3_geographically_linked_data.tab", stringsAsFactors = FALSE, data.table = FALSE)
# S3Neigh= S3Neigh[match(df0$MCSID, S3Neigh$mcsid),]
# df0$S3_IMD = c(t(S3Neigh[11])) #1 = most deprived decile 10=least deprived decile
# 
# #Marital state - adapted from  Rust, J, Bennun, I., Crowe, M. & Golombok, S. (1990). The Grims: a psychometric instrument for the assessment of marital discord. Journal of Family Therapy, 12, 45-57)
# df0$S3_MaritalState1=setNA(S3Ses[3268],c(-1:-9,6)) #1=strongly agree 5=strongly disagree
# df0$S3_MaritalState2=setNA(S3Ses[3269],c(-1:-9,6)) 
# df0$S3_MaritalState3=setNA(S3Ses[3270],c(-1:-9,6))
# df0$S3_MaritalState4=setNA(S3Ses[3271],c(-1:-9,6)) 
# df0$S3_MaritalState5=setNA(S3Ses[3272],c(-1:-9,6)) 
# df0$S3_MaritalState5=setNA(S3Ses[3273],c(-1:-9,6)) 
# df0$S3_MaritalState5=setNA(S3Ses[3274],c(-1:-9,6)) 
# df0$S3_MaritalState5=setNA(S3Ses[3275],c(-1:-9,6)) 
# 
# #Life Satisfaction, happiness
# df0$S3_LifeSatisfaction = setNA(S3Ses[3342], c(-1:-9,11)) #10 - completely satisfied 1- complete dissatisfied
# 
# #SES - income and education
# S3Derived = fread("UKDA-5795-tab MCS3/tab/mcs3_derived_variables.tab", stringsAsFactors = FALSE, data.table = FALSE) #mcsids are not duplicated here
# S3Derived = S3Derived[match(df0$MCSID, S3Derived$MCSID),]
# 
# df0$S3_SES_TotalIncome2Par = (setNA(S3Derived[40],-1:-9)) #Variable = CDINCC00	Variable label = S3 DV Total Income (banded, TWO PARENT) 
# df0$S3_SES_TotalIncome1Par = (setNA(S3Derived[41],-1:-9)) #Variable = CDINCS00	Variable label = S3 DV Total Income (banded, LONE PARENT)
# df0$S3_SES_TotalIncome = (setNA(S3Derived[42],-1)) #YEARLY INCOME Variable = CDTOTI00	Variable label = S3 DV Total Income 
# 
# 
# df0$S3_SES_EquivIncome = setNA(S3Derived[,32],-1)
# df0$S3_SES_EquivIncome_Norm = Normalise(df0$S3_SES_EquivIncome)
# 
# df0$S3_SES_HomeOwnership = swapsies(setNA(S3Derived[36],-1:-9), 1:10, c(1,1,1,0,0,0,0,0,0,0)) #1 = Own or part own property 0 = do not own
# 
# df0$S3_SES_MainLastJob = swapsies(setNA(S3Derived[76], -1), 1:7, 6:0) #Variable = CMD13C00	Variable label = S3 MAIN DV NS-SEC major categories (last known job) 
# df0$S3_SES_PartnerLastJob = swapsies(setNA(S3Derived[105], -1), 1:7, 6:0) #Variable = CPD07C00	Variable label = S3 PARTNER DV NS-SEC 7 classes (last known job) 
# df0$S3_SES_AverageLastJob = apply(df0[,c("S3_SES_MainLastJob","S3_SES_PartnerLastJob")],1, function(x) mean(x, na.rm=TRUE))
# 
# #Education
# df0$S3_SES_EducationMain = swapsies(setNA(S3Derived[63], c(-1, 95)),c(96,1,2,3,4,5), 0:5) #Higher is better - CMDNVQ00	Variable label = S3 MAIN DV Respondent NVQ Highest Level (across sweeps 1,2 and 3)   
# df0$S3_SES_EducationPartner = swapsies(setNA(S3Derived[92], c(-1, 95)),c(96,1,2,3,4,5), 0:5)
# df0$S3_SES_EducationAvg = apply(df0[,c("S3_SES_EducationMain","S3_SES_EducationPartner")],1, function(x) mean(x, na.rm=TRUE))
# 
# #HOUSEHOLD INCOME
# df0$S3_SES_Income_TotalHousehold_JOINT = Convert(S3Ses[,2670],2:20, c(15,45,75,105,135,175,225,275,325,375,450,550,650,750,850,950,1275,1726))
# df0$S3_SES_Income_TotalHousehold_NOPARTNER = Convert(S3Ses[,2671],2:20, c(10,30,50,70,90,117.5,150,182.5,217.5,250,300,367.5,432.5,500,567.5,632.5,832.5,1135))
# df0$S3_SES_Income_TotalHousehold = df0$S3_SES_Income_TotalHousehold_JOINT
# df0[which(!is.na(df0$S3_SES_Income_TotalHousehold_NOPARTNER)),"S3_SES_Income_TotalHousehold"]=df0$S3_SES_Income_TotalHousehold_NOPARTNER[which(!is.na(df0$S3_SES_Income_TotalHousehold_NOPARTNER))]
# 
# 
# #Derived SES   
# df0$S3_SES_FA = calcFactorScore(var=c("S3_SES_EquivIncome_Norm","S3_SES_AverageLastJob","S3_SES_EducationAvg"))
# 
# #psych::factor.scores(df0[,CA2],SESFA, method="tenBerge", impute="mean")
# 
# #Cog/Education outcomes
# S3Cog = fread("UKDA-5795-tab MCS3/tab/mcs3_child_assessment_data.tab", stringsAsFactors = FALSE, data.table = FALSE)
# S3Cog = S3Cog[S3Cog$chcnum00==1,]
# S3Cog = S3Cog[match(df0$MCSID, S3Cog$mcsid),]
# 
# df0$S3_Cog_PatternRawScore = setNA(S3Cog[229], -1 )
# df0$S3_Cog_PatternRawScore_Norm = Normalise (df0$S3_Cog_PatternRawScore)
# df0$S3_Cog_PatternAbilityScore = setNA(S3Cog[230], -1 )
# 
# 
# df0$S3_Cog_PictureSimilaritiesRaw = setNA(S3Cog[52],-1:-9)
# df0$S3_Cog_PictureSimilaritiesNorm = Normalise(df0$S3_Cog_PictureSimilaritiesRaw)
# df0$S3_Cog_PictureSimilaritiesAbility = setNA(S3Cog[53],-1:-9)
# 
# df0$S3_Cog_NamingVocabRaw = setNA(S3Cog[115],-1:-9)
# df0$S3_Cog_NamingVocabNorm = Normalise(df0$S3_Cog_NamingVocabRaw)
# df0$S3_Cog_NamingVocabAbility= setNA(S3Cog[116],-1:-9)
# 
# #Height and Weight
# df0$S3_Health_Height = setNA(S3Cog[,345],-1:-9)
# df0$S3_Health_Height_Correct = S3Cog[,348]
# 
# 
# table(df0$S3_Health_Height<0, df0$S3_Health_Height_Correct )
# 
# 
# #Delete Files
# rm(S3Derived, S3Neigh, S3Cog, S3Ses)
# 
# #Teacher Ratings -- Only 3,000 cases so not performed! 
# #S3Teach = fread("UKDA-6847-tab MCS3 Teacher /tab/mcs3_teacher.tab", stringsAsFactors = FALSE, data.table = FALSE)
# #S3Teach = S3Derived[match(df0$MCSID, S3Derived$MCSID),]
# 
# 
# #######################################################################################################################
# ####################################################     MCS 4     #################################################### 
# #######################################################################################################################
# 
# #Parent Questionnaire
S4Par = fread("UKDA-6411-tab MCS4/tab/mcs4_parent_interview.tab", stringsAsFactors = FALSE, data.table=FALSE)
# table(duplicated(S4Par$mcsid))
S4Par = S4Par[match(df0$MCSID, S4Par$mcsid),]

#SDQ Data for Edwin

S4_SDQ = convertSDQ(S4Par[,4255:4279])
colnames(S4_SDQ) = paste0("S4_Parent_",colnames(S4_SDQ))


df0 = cbind.data.frame(df0, S4_SDQ)

rm(S4_SDQ,S4Par)




#######################################################################################################################
####################################################     MCS 5     #################################################### 
#######################################################################################################################

#Parent Interview
# S5ParInt = fread("UKDA-7464-tab MCS5/tab/mcs5_parent_interview.tab", stringsAsFactors = FALSE, data.table = FALSE) #mcsids are not duplicated here
# S5ParInt = S5ParInt[S5ParInt$EPNUM00==1,] #remove PARTNER responses (no duplicates of MCSID after this!)
# S5ParInt = S5ParInt[match(df0$MCSID, S5ParInt$MCSID),]

#Derived Data
S5Derived = fread("UKDA-7464-tab MCS5/tab/mcs5_family_derived.tab", stringsAsFactors = FALSE, data.table = FALSE) #mcsids are not duplicated here
S5Derived = S5Derived[match(df0$MCSID, S5Derived$MCSID),]

#PARENT derived data
# S5ParDerived = fread("UKDA-7464-tab MCS5/tab/mcs5_parent_derived.tab", stringsAsFactors = FALSE, data.table = FALSE) #mcsids are not duplicated here
# S5ParDerivedMain = S5ParDerived[S5ParDerived$EELIG00==1,]
# S5ParDerivedMain = S5ParDerivedMain[match(df0$MCSID, S5ParDerivedMain$MCSID),]
# S5ParDerivedPartner = S5ParDerived[S5ParDerived$EELIG00==2,]
# S5ParDerivedPartner = S5ParDerivedPartner[match(df0$MCSID, S5ParDerivedPartner$MCSID),]
# S5ParDerived = S5ParDerived[S5ParDerived$EELIG00<3,]
# S5ParDerived = S5ParDerived[match(df0$MCSID, S5ParDerived$MCSID),]

#Child Questionnaires
S5CQ = fread("UKDA-7464-tab MCS5/tab/mcs5_cm_interview.tab", stringsAsFactors = FALSE, data.table = FALSE)
S5CQ = S5CQ[S5CQ$ECNUM00 ==1,]
S5CQ = S5CQ[match(df0$MCSID, S5CQ$MCSID),]

#Teacher rated outcomes
S5Teach = fread("UKDA-7464-tab MCS5/tab/mcs5_cm_teacher_survey.tab", stringsAsFactors = FALSE, data.table = FALSE)
S5Teach = S5Teach[S5Teach$ECNUM00==1,]
S5Teach = S5Teach[match(df0$MCSID, S5Teach$MCSID),]

#Parent interview about cm
S5ParCMint = fread("UKDA-7464-tab MCS5/tab/mcs5_parent_cm_interview.tab", stringsAsFactors = FALSE, data.table = FALSE)
S5ParCMint = S5ParCMint[S5ParCMint$EELIG00 ==1,] #Only keep main responder! 
S5ParCMint = S5ParCMint[S5ParCMint$ECNUM00 ==1,]
S5ParCMint = S5ParCMint[match(df0$MCSID, S5ParCMint$MCSID),]

#checks
# done_SDQ = S5ParCMint[,890]!=-1
# 
# table(S5ParCMint[,890]>0)
# 
# table(S5ParCMint$EPNUM00,done_SDQ)


#Cognition
S5Cog = fread("UKDA-7464-tab MCS5/tab/mcs5_cm_assessment.tab",stringsAsFactors = FALSE, data.table=FALSE)
S5Cog = S5Cog[S5Cog$ECNUM00==1,]
S5Cog = S5Cog[match(df0$MCSID, S5Cog$MCSID),]

#Geographic Data
S5Geo = fread("UKDA-7464-tab MCS5/tab/mcs5_geographically_linked_data.tab", stringsAsFactors = FALSE, data.table = FALSE) #mcsids are not duplicated here
S5Geo = S5Geo[match(df0$MCSID, S5Geo$MCSID),]


# New demographic things added
df0$S5_Sex_ECCSEX00 = S5Cog$ECCSEX00
df0$S5_Age_ECCAGE00 = S5Cog$ECCAGE00

#random
df0$S5_ChildQ_SportsGames = Convert(S5CQ[,14],1:5,5:1)

#Child-rated Feelings
df0$S5_ChildQ_Feelings_Happy = Convert(S5CQ[,106],1:5,1:5)
df0$S5_ChildQ_Feelings_Worry = Convert(S5CQ[,107],1:5,5:1)
df0$S5_ChildQ_Feelings_Sad   = Convert(S5CQ[,108],1:5,5:1)
df0$S5_ChildQ_Feelings_Afraid= Convert(S5CQ[,109],1:5,5:1)
df0$S5_ChildQ_Feelings_Laugh = Convert(S5CQ[,110],1:5,1:5)
df0$S5_ChildQ_Feelings_Angry = Convert(S5CQ[,111],1:5,5:1)





#Academic Achievement 
df0$S5_Teach_AA_Eng = Convert(S5Teach[13], 1:5,5:1) #Swapped coding so that 5= well above average, 1= well below average
df0$S5_Teach_AA_Math = Convert(S5Teach[15], 1:5,5:1)
df0$S5_Teach_AA_Science = Convert(S5Teach[16], 1:5,5:1)
df0$S5_Teach_AA_Art = Convert(S5Teach[17], 1:5,5:1) 
df0$S5_Teach_AA_Music = Convert(S5Teach[18], 1:5,5:1)
df0$S5_Teach_AA_PE = Convert(S5Teach[19], 1:5,5:1) 
df0$S5_Teach_AA_ICT = Convert(S5Teach[20], 1:5,5:1) 

# S5TeachIRT = IRTcheck(df1=df0,vars="S5_Teach_AA", guess_param = 1/5,mirtoutput =TRUE, mirt_modeltype="nominal", nCores=1, mirt_Method="EM", mirt_Optimizer="BFGS")
# 
# 
# df0$S5_Teach_AA_IRTScore = S5TeachIRT$mirt_scores
# 
# score = calcFactorScore(var=c("S5_Teach_AA_Eng","S5_Teach_AA_Math","S5_Teach_AA_Science","S5_Teach_AA_Art","S5_Teach_AA_Music","S5_Teach_AA_PE","S5_Teach_AA_ICT"))

# df0$S5_Teach_EngMathSciAvg = apply(df0[,c("S5_Teach_Eng","S5_Teach_Math","S5_Teach_Science")], 1, function(x) mean(x, na.rm=TRUE))
# df0$S5_Teach_EngMathSciAvg_Norm = Normalise(df0$S5_Teach_EngMathSciAvg)

#Q18-19ab,b - teacher predictions about child's future 
df0$S5_Teach_Pred_PreparedForSecondarySchool = swapsies(setNA(S5Teach[128],c(-8,5)), 1:4,4:1) #Variable = EQ18	Variable label = S5 TS CM Prepared for secondary school  
df0$S5_Teach_Pred_StayFullTimeEducation = swapsies(setNA(S5Teach[129],-8), 1:4,4:1) #	Variable = EQ19A	Variable label = S5 TS How likely CM will stay on after age 16   
df0$S5_Teach_Pred_GoUniversity = swapsies(setNA(S5Teach[130],-8), 1:4,4:1) #Variable = EQ19B	Variable label = S5 TS How likely CM will go to university   

#Parent Interest

df0$S5_Teach_MotherInterest = Convert(S5Teach[,131],1:5,5:1)
df0$S5_Teach_FatherInterest = Convert(S5Teach[,132],1:5,5:1)

df0$S5_Teach_ParentInterest_MeanNorm = Normalise(as.numeric(apply(df0[,c("S5_Teach_MotherInterest","S5_Teach_FatherInterest")],1,function(x) mean(x, na.rm=TRUE))))
df0$S5_Teach_ParentInterest_Binary = as.numeric(df0$S5_Teach_ParentInterest_MeanNorm>0)

# df0$S45_Teach_ParentInterest_Averaged = Normalise(as.numeric(apply(df0[,c("S4_Teach_MotherInterest","S4_Teach_FatherInterest","S5_Teach_MotherInterest","S5_Teach_FatherInterest")],1,function(x) mean(x, na.rm=TRUE))))
# df0$S45_Teach_ParentInterest_Binary = as.numeric(df0$S45_Teach_ParentInterest_Averaged>-.20)

#SDQ - Teacher 
# SDQ = S5Teach[,27:51]
# SDQ[SDQ==-8]=NA
# colnames(SDQ) = paste0("S5_Teach_SDQ",1:25)
# 
# SDQ[7] = swapsies(SDQ[,7], 1:3, 3:1) #reverse code certain items!!
# SDQ[11] = swapsies(SDQ[,11], 1:3, 3:1)
# SDQ[14] = swapsies(SDQ[,14], 1:3, 3:1)
# SDQ[21] = swapsies(SDQ[,21], 1:3, 3:1)
# SDQ[25] = swapsies(SDQ[,25], 1:3, 3:1)
# 
# df0$S5_Teach_SDQ_Emotion = apply(SDQ[,c(3,8,13,16,24)], 1, function(x) mean(x, na.rm=TRUE)) 
# df0$S5_Teach_SDQ_Conduct = apply(SDQ[,c(5,7,12,18,22)], 1, function(x) mean(x, na.rm=TRUE)) 
# df0$S5_Teach_SDQ_Hyper = apply(SDQ[,c(2,10,15,21,25)], 1, function(x) mean(x, na.rm=TRUE)) 
# df0$S5_Teach_SDQ_Peer = apply(SDQ[, c(6,11,14,19,23)], 1, function(x) mean(x, na.rm=TRUE)) 
# df0$S5_Teach_SDQ_Prosocial = apply(SDQ[, c(1,4,9,17,20)], 1, function(x) mean(x, na.rm=TRUE)) 
# df0=cbind(df0, SDQ)
# rm(SDQ)
# #Attitude to school
# df0$S5_Teach_Attitude_enjoy = swapsies(setNA(S5Teach[,21],-8), 1:4,4:1)
# df0$S5_Teach_Attitude_bored = setNA(S5Teach[,22],-8)
# df0$S5_Teach_Attitude_tries = swapsies(setNA(S5Teach[,23],-8), 1:4,4:1)
# df0$S5_Teach_Attitude_misb = setNA(S5Teach[,24],-8)
# df0$S5_Teach_Attitude_late = setNA(S5Teach[,25],-8)
# df0$S5_Teach_Attitude_indep = swapsies(setNA(S5Teach[,26],-8), 1:4, 4:1)
# 
# df0$S5_Teach_Attitude_NormFactorScore = Normalise(fa(df0[,grepl("S5_Teach_Attitude", colnames(df0))])$scores)
# 
# rm(SDQ)

#SDQ - Parent & Teacher


S5_Parent_SDQ = convertSDQ(S5ParCMint[,890:914], varnames = "S5_Parent_SDQ_")

S5_Teach_SDQ = convertSDQ(S5Teach[,27:51], varnames = "S5_Teach_SDQ_")

df0=cbind.data.frame(df0,S5_Parent_SDQ, S5_Teach_SDQ)


#SEN info
df0$S5_teach_SEN_Ever = S5Teach[,57]
df0$S5_teach_SEN_FullStatement = S5Teach[,58]

df0$S5_Teach_SEN_Dyslexia = S5Teach[59] 
df0$S5_Teach_SEN_DyspraxiaDyscalculia = S5Teach[60]
df0$S5_Teach_SEN_ADHD = S5Teach[61]
df0$S5_Teach_SEN_Autism = S5Teach[62]
df0$S5_Teach_SEN_Hyperactivity = S5Teach[63]
df0$S5_Teach_SEN_SpeechLanguage = S5Teach[64]
#Some are excluded here!
df0$S5_Teach_SEN_Depression = S5Teach[69]

#Miscellaneous 
df0$S5_Teach_FriendsBehav = setNA(S5Teach[124], -8)
df0$S5_Teach_FriendsAbili = swapsies(setNA(S5Teach[125], -8:-9),1:3,3:1)
df0$S5_Teach_Bullied = setNA(S5Teach[126],-8)
df0$S5_Teach_BullyOthers = setNA(S5Teach[127],-8)

#Verbal Similarities 
df0$S5_Cog_VS_Raw =  setNA(S5Cog[115], -1:-9)
df0$S5_Cog_VS_Ability =  setNA(S5Cog[116], -1:-9) #This does not asjust for age!
df0$S5_Cog_VS_Standard =  setNA(S5Cog[117], -1:-9)

#CANTAB spatial working memory task
df0$S5_Cog_SWM_BetweenErrors4 = setNA(S5Cog[,166],-1:-9)
df0$S5_Cog_SWM_BetweenErrors48 = setNA(S5Cog[,167],-1:-9)
df0$S5_Cog_SWM_DoubleErrors = setNA(S5Cog[,168],-1:-9)
df0$S5_Cog_SWM_BetweenErrors4 = setNA(S5Cog[,169],-1:-9)


df0$S5_Cog_SWM_Strategy = setNA(S5Cog[,170],-1:-9)*-1 #Looking at the table there are some outliers before 18! #reverse coded!    Pos. = 170	Variable = SWMSTRAT	Variable label = SWM Strategy

df0$S5_Cog_SWM_TimeFirstResponse = setNA(S5Cog[,171],-1:-9)
df0$S5_Cog_SWM_MeanTokenSearchPreparationTime = setNA(S5Cog[,172],-1:-9)
df0$S5_Cog_SWM_MeanTimeToLastResponse = setNA(S5Cog[,173],-1:-9)

df0$S5_Cog_SWM_TotalErrors4 = setNA(S5Cog[,174],-1:-9)
df0$S5_Cog_SWM_TotalErrors48 = setNA(S5Cog[,175],-1:-9)*-1 #reverse coded!  Pos. = 175	Variable = SWMTE8BX	Variable label = SWM Total errors 4 to 8 boxes   
df0$S5_Cog_SWM_WithinErrors = setNA(S5Cog[,176],-1:-9)
df0$S5_Cog_SWM_WithinErrors4 = setNA(S5Cog[,177],-1:-9)
df0$S5_Cog_SWM_WithinErrors48 = setNA(S5Cog[,178],-1:-9)

#CANTAB- Cambridge Gamling Task 

S5_CGT = S5Cog[,159:164]
S5_CGT = data.frame(apply(S5_CGT, 2, function(x) setNA(x, -9)))
colnames(S5_CGT) = c("S5_Cog_CGT_DelayAversion",
                     "S5_Cog_CGT_DeliberationTime",
                     "S5_Cog_CGT_OverallProportionBet",
                     "S5_Cog_CGT_QualityDecision",
                     "S5_Cog_CGT_RiskAdjustment",
                     "S5_Cog_CGT_RiskTaking"
)

df0 = cbind.data.frame(df0,S5_CGT)

df0$S5_Cog_CGT_RiskTaking = df0$S5_Cog_CGT_RiskTaking*-1 #recode so higher means less risk taking


rm(S5_CGT)


#Income and Employment 
#There is no "pure" income measure in this sweep :'(

#df0$S5_SES_Income_Predicted  This variable DOESN'T SEEM TO EXIST IN THIS SWEEP!
# 
# df0$S5_SES_EquivIncome = setNA(S5Derived[,30],-1) #Variable label = S5 DV OECD equiv weekly family income   
# df0$S5_SES_EquivIncome_Norm = Normalise(df0$S5_SES_EquivIncome)
# df0$S5_SES_EquivIncomeQuintile = S5Derived[,32] #Variable = EOECDUK0	Variable label = S5 DV OECD Equivalised income quintiles - UK whole  
# 
# df0$S5_SES_ParentWorking = (S5Derived[26]) #Variable = EDCWRK00	Variable label = S5 DV Combined labour market status 
# df0$S5_SES_ParentWorking2 = swapsies(setNA(S5Derived[26],9:11), 1:6, c(3,2,2,1,2,1) ) #3- both parents in work 2-one parent in work 1- no parents in work
# df0$S5_SES_ParentWorking3 = swapsies(setNA(S5Derived[26],9:11), 1:6, c(1,1,1,0,1,0) ) #1- both or one parent employed or on leave 0- both parents unemployed
# 
# df0$S5_SES_MainCurrentJob = Convert(S5ParDerivedMain[18], 1:13, 13:1) # Pos. = 18	Variable = EDD13S00	Variable label = S5 DV NS-SEC 13 category (current job) 
# df0$S5_SES_PartnerCurrentJob = Convert(S5ParDerivedPartner[18], 1:13, 13:1)
# df0$S5_SES_AverageCurrentJob = apply(df0[,c("S5_SES_MainCurrentJob","S5_SES_PartnerCurrentJob")],1, function(x) mean(x, na.rm=TRUE))
# df0$S5_SES_AverageCurrentJob_Norm = Normalise(df0$S5_SES_AverageCurrentJob)
# 
# #Raw income data
# df0$S5_SES_Income_TotalHousehold_JOINT = Convert(S5ParInt[,746],2:20, c(30,100,170,220,255,280,335,415,490,560,625,715,845,960,1100,1400,1900,2550,3250))
# df0$S5_SES_Income_TotalHousehold_NOPARTNER = Convert(S5ParInt[,747],2:20, c(10,80,165,205,225,240,280,340,400,465,535,620,755,885,1000,1225,1690,2050,2150))
# df0$S5_SES_Income_TotalHousehold = df0$S5_SES_Income_TotalHousehold_JOINT
# df0[which(!is.na(df0$S5_SES_Income_TotalHousehold_NOPARTNER)),"S5_SES_Income_TotalHousehold"]=df0$S5_SES_Income_TotalHousehold_NOPARTNER[which(!is.na(df0$S5_SES_Income_TotalHousehold_NOPARTNER))]
# 
# 
# #Education
# df0$S5_SES_EducationMain = swapsies(setNA(S5ParDerivedMain[,13], c(-1, 95)),c(96,1,2,3,4,5), 0:5) #Higher is better - Variable = EDDNVQ00	Variable label = S5 DV Respondent NVQ Highest Level (all sweeps) 
# df0$S5_SES_EducationPartner = swapsies(setNA(S5ParDerivedPartner[,13], c(-1, 95)),c(96,1,2,3,4,5), 0:5)
# df0$S5_SES_EducationAvg = apply(df0[,c("S5_SES_EducationMain","S5_SES_EducationPartner")],1, function(x) mean(x, na.rm=TRUE))
# df0$S5_SES_EducationAvg_Norm = Normalise(df0$S5_SES_EducationAvg)
# 
# #Income+ 
# df0$S5_SES_Subj_ManagingFinanciallyMAIN = Convert(S5ParInt[,760],1:5,5:1) #Pos. = 2960	Variable = dmmafi00	Variable label = S4 MAIN How well managing financially   
# df0$S5_SES_Subj_ManagingFinancially_ComparedtoLastTime_MAIN = Convert(S5ParInt[,761],1:5,5:1) #Pos. = 2960	Variable = dmmafi00	Variable label = S4 MAIN How well managing financially   
# 
# #Other
# df0$S5_SES_HomeOwnership = swapsies(setNA(S5Derived[23], -1:-9), 1:10, c(1,1,1,0,0,0,0,0,0,0)) #1 = Own or part own property 0 = do not own
# df0$S5_SES_IMD = S5Geo[,10] #Just england
# df0$S5_SES_WIMD = S5Geo[,18] #Just wales
# df0$S5_SES_SIMD = S5Geo[,27] #Just scotland
# df0$S5_SES_NIIMD = S5Geo[,36] #Just northern ireland
# df0$S5_SES_IMD_Combined = (apply(df0[,c("S5_SES_IMD","S5_SES_WIMD","S5_SES_SIMD","S5_SES_NIIMD")], 1, function(x) mean(x, na.rm=TRUE) ) )
# 
# df0$S5_SES_FA = calcFactorScore(var=c("S5_SES_EquivIncome_Norm","S5_SES_AverageLastJob","S5_SES_EducationAvg"))
# df0$S5_SES_FSN = Normalise(calcFactorScore(var=c("S5_SES_EquivIncome_Norm","S5_SES_AverageLastJob","S5_SES_EducationAvg")))


#Misc

S5cmDerived = fread("UKDA-7464-tab MCS5/tab/mcs5_cm_derived.tab", stringsAsFactors = FALSE, data.table = FALSE) #mcsids are not duplicated here
S5cmDerived = S5cmDerived[S5cmDerived$ECNUM00==1,]
S5cmDerived = S5cmDerived[match(df0$MCSID, S5cmDerived$MCSID),]

S5cmDerived$year = S5cmDerived[,4]
S5cmDerived$month = as.character(S5cmDerived[,3])
S5cmDerived$month[which(nchar(S5cmDerived$month)==1)]=paste0("0",S5cmDerived$month[which(nchar(S5cmDerived$month)==1)])
S5cmDerived$month=paste0("01/",S5cmDerived$month)

InterviewDate = as.Date(c(t(apply(S5cmDerived[,c("year","month")],1, function(x) paste(x, collapse="/")))), format="%Y/%d/%m")

S5cmDerived$byear = S5cmDerived[,7]
S5cmDerived$bmonth = as.character(S5cmDerived[,6])
S5cmDerived$bmonth[which(nchar(S5cmDerived$bmonth)==1)]=paste0("0",S5cmDerived$bmonth[which(nchar(S5cmDerived$bmonth)==1)])
S5cmDerived$bmonth=paste0("01/",S5cmDerived$bmonth)

BirthDate = as.Date(c(t(apply(S5cmDerived[,c("byear","bmonth")],1, function(x) paste(x, collapse="/")))), format="%Y/%d/%m")



df0$S5_AgeAtInterview = as.numeric(InterviewDate-BirthDate)/365.25




rm(S5CQ,S5Cog, S5Derived, S5Geo, S5ParDerived, S5ParDerivedMain, S5ParDerivedPartner, InterviewDate, BirthDate,S5cmDerived)


#######################################################################################################################
####################################################     MCS 6     #################################################### 
#######################################################################################################################
# Child Questionnaire
S6CMint = fread("UKDA-8156-tab MCS6/tab/mcs6_cm_interview.tab", stringsAsFactors = FALSE, data.table = FALSE)
S6CMint = S6CMint[S6CMint$FCNUM00==1,]
S6CMint = S6CMint[match(df0$MCSID, S6CMint$MCSID),]

S6Der = fread("UKDA-8156-tab MCS6/tab/mcs6_cm_derived.tab", stringsAsFactors = FALSE, data.table = FALSE)
S6Der = S6Der[S6Der$FCNUM00==1,]
S6Der = S6Der[match(df0$MCSID, S6Der$MCSID),]

S6Cog = fread("UKDA-8156-tab MCS6/tab/mcs6_cm_assessment.tab", stringsAsFactors = FALSE, data.table = FALSE)
S6Cog = S6Cog[S6Cog$FCNUM00==1,]
S6Cog = S6Cog[match(df0$MCSID, S6Cog$MCSID),]

S6ParInt = fread("UKDA-8156-tab MCS6/tab/mcs6_parent_interview.tab", stringsAsFactors = FALSE, data.table = FALSE)
S6ParInt = S6ParInt[S6ParInt$FELIG00==1,] #Just keep main responses 
S6ParInt = S6ParInt[match(df0$MCSID, S6ParInt$MCSID),]

S6ParIntCM = fread("UKDA-8156-tab MCS6/tab/mcs6_parent_cm_interview.tab", stringsAsFactors = FALSE, data.table = FALSE)
S6ParIntCM = S6ParIntCM[S6ParIntCM$FELIG00==1,] #Just keep main responses 
S6ParIntCM = S6ParIntCM[match(df0$MCSID, S6ParIntCM$MCSID),]

S6FamDer = fread("UKDA-8156-tab MCS6/tab/mcs6_family_derived.tab", stringsAsFactors = FALSE, data.table = FALSE)
S6FamDer = S6FamDer[match(df0$MCSID, S6FamDer$MCSID),]

S6ParDerived = fread("UKDA-8156-tab MCS6/tab/mcs6_parent_derived.tab", stringsAsFactors = FALSE, data.table = FALSE)
S6ParDerivedMain = S6ParDerived[S6ParDerived$FELIG00==1,]
  S6ParDerivedMain = S6ParDerivedMain[match(df0$MCSID , S6ParDerivedMain$MCSID),]
S6ParDerivedPart = S6ParDerived[S6ParDerived$FELIG00==2,]
  S6ParDerivedPart = S6ParDerivedPart[match(df0$MCSID , S6ParDerivedPart$MCSID),]


# IMD
  IMD_Data = list()
  IMD_Data[["E"]] = fread("UKDA-8156-tab MCS6/tab/mcs_sweep6_imd_e_2004.tab", stringsAsFactors = FALSE, data.table = FALSE)
  IMD_Data[["N"]] = fread("UKDA-8156-tab MCS6/tab/mcs_sweep6_imd_n_2004.tab", stringsAsFactors = FALSE, data.table = FALSE)
  IMD_Data[["S"]] = fread("UKDA-8156-tab MCS6/tab/mcs_sweep6_imd_s_2004.tab", stringsAsFactors = FALSE, data.table = FALSE)
  IMD_Data[["W"]] = fread("UKDA-8156-tab MCS6/tab/mcs_sweep6_imd_w_2004.tab", stringsAsFactors = FALSE, data.table = FALSE)
  
  S6_IMD = as.numeric(unlist(sapply(IMD_Data, function(x) x[,3])))
  S6_IMD_PpsID = as.character(unlist(sapply(IMD_Data, function(x) x[,1])))
  
  
  df0$S6_SES_IMD_Combined = S6_IMD[match(df0$MCSID,S6_IMD_PpsID)]
  
  
# Demographics
  
  

# SES
df0$S6_SES_EquivIncome = setNA(S6FamDer[,29],-1) #Pos. = 29	Variable = FOEDE000	Variable label = S6 DV OECD equiv weekly family income   
df0$S6_SES_HomeOwnership_FDROOW00 = S6FamDer$FDROOW00
df0$S6_SES_HomeOwnership = Convert(S6FamDer[,27],1:10, c(1,1,1,0,0, 0,0,0,0,0)) #Pos. = 27	Variable = FDROOW00	Variable label = S6 DV Housing Tenure
df0$S6_SES_CuurentJob = Convert(S6ParDerivedMain[,12], 1:13,13:1) #  Pos. = 12	Variable = FD13S00	Variable label = S6 DV NS-SEC 13 category (current job)  
df0$S6_SES_EducationMain = Convert(S6ParDerivedMain[,7],1:5,1:5) #Pos. = 7	Variable = FDNVQ00	Variable label = S6 DV Respondent NVQ Highest Level (all sweeps) 






# Parent Rated SDQ
  S6_SDQ = S6ParIntCM[,382:406]
  S6_SDQ = convertSDQ(S6_SDQ, varnames="S6_Parent_SDQ_")
  df0 = cbind.data.frame(df0,S6_SDQ)


# Child Questionnaire Data
  
# Truancy
df0$S6_ChildQ_Truancy_Ever = Convert(S6CMint[,88], 1:2,0:1) #Pos. = 88	Variable = FCTRUA00	Variable label = Has CM missed school without parents' permission
df0$S6_ChildQ_Truancy_Freq = Convert(S6CMint[,89], c(-1,1:6),7:1) #Pos. = 89	Variable = FCTRUF00	Variable label = How often does CM miss school without permission

#Bullying
df0$S6_ChildQ_Bullying_Siblings = Convert(S6CMint[,206],1:6,1:6) #Pos. = 206	Variable = FCBULP00	Variable label = How often CM hurts or picks on brothers or sisters  
df0$S6_ChildQ_Bullying_Others = Convert(S6CMint[,208],1:6,1:6) #Pos. = 208	Variable = FCPCKP00	Variable label = How often CM hurts or picks on other children   
df0$S6_ChildQ_Bullying_Online = Convert(S6CMint[,210], 1:6,1:6) #Pos. = 210	Variable = FCCYBO00	Variable label = How often CM bullied other children online  

# x=calcFactorScore(var=findMCS("S6_ChildQ_Bully"), verbose=TRUE)

#Antisocial Behaviour - past 12 months
df0$S6_ChildQ_BB_RudePublic = Convert(S6CMint[,218],1:2,0:1) #Pos. = 218	Variable = FCRUDE00	Variable label = Past 12 months:CM has been complained for being rude/noisy in public?   
df0$S6_ChildQ_BB_ShopLift = Convert(S6CMint[,221],1:2,0:1) #Pos. = 221	Variable = FCSTOL00	Variable label = Past 12months: has CM taken something from a shop without paying
df0$S6_ChildQ_BB_Vandalism = Convert(S6CMint[,224],1:2,0:1) #Pos. = 224	Variable = FCSPRY00	Variable label = Past 12 months: has CM written/spray painted somewhere they shouldn't   
df0$S6_ChildQ_BB_Damage = Convert(S6CMint[,227],1:2,0:1) #Pos. = 227	Variable = FCDAMG00	Variable label = Past 12 months: has CM damaged something that didn't belong to them 
df0$S6_ChildQ_BB_Knife = Convert(S6CMint[,230],1:2,0:1) #Pos. = 230	Variable = FCKNIF00	Variable label = Has CM ever carried a knife or other weapon 
df0$S6_ChildQ_BB_Robbery = Convert(S6CMint[,231],1:2,0:1) #Pos. = 231	Variable = FCROBH00	Variable label = Has CM ever entered smn's home to steal or damage something 

#Seperate set of Antisocial behaviour questions
df0$S6_ChildQ_BB_HitSomeone = Convert(S6CMint[,232],1:2,0:1) #Pos. = 232	Variable = FCHITT00	Variable label = ASBOgrid: CM Pushed or shoved/hit/slapped/punched someone?  
df0$S6_ChildQ_BB_HitWeapon = Convert(S6CMint[,233],1:2,0:1) #Pos. = 233	Variable = FCWEPN00	Variable label = ASBOgrid: CM Used or hit someone with a weapon?   
df0$S6_ChildQ_BB_Steal = Convert(S6CMint[,234],1:2,0:1) #Pos. = 234	Variable = FCSTLN00	Variable label = ASBOgrid: CM stolen something from someone eg mobile phone, money  
#Not including this: Pos. = 235	Variable = FCPOLS00	Variable label = Has CM been stopped or questioned by police 
df0$S6_ChildQ_BB_PoliceWarning = Convert(S6CMint[,236],1:2,0:1) #Pos. = 236	Variable = FCCAUT00	Variable label = Has CM ever been given a formal warning or caution from police  
df0$S6_ChildQ_BB_Arrested = Convert(S6CMint[,237],1:2,0:1) #Pos. = 237	Variable = FCARES00	Variable label = Has CM ever been arrested    
df0$S6_ChildQ_BB_StreetGang = Convert(S6CMint[,238],1:3,c(0,1,0)) #Pos. = 238	Variable = FCGANG00	Variable label = Is CM a member of a street gang    
df0$S6_ChildQ_BB_CompHack = Convert(S6CMint[,239],1:2,0:1) # Pos. = 239	Variable = FCHACK00	Variable label = Past 12 months: has CM hacked into smn elses computer/online account
df0$S6_ChildQ_BB_SentCompVirus = Convert(S6CMint[,242],1:2,0:1) # Pos. = 242	Variable = FCVIRS00	Variable label = Past 12 months: has CM sent viruses to damage/infect other computers

# x=Normalise(calcFactorScore(var=findMCS("S6_ChildQ_BB"), verbose=TRUE))
# fa.parallel(df0[,findMCS("S6_ChildQ_BB")], error.bars = TRUE)
# fa(df0[,findMCS("S6_ChildQ_BB")], nfactors = 1)

#Well-being - uknown origins

WellbeingGrid = S6CMint[,282:287]
WellbeingGrid = apply(WellbeingGrid,2,function(x) Convert(x, 1:7,7:1))
colnames(WellbeingGrid) = paste0("S6_ChildQ_Wellbeing_",1:6)
df0 = cbind.data.frame(df0,WellbeingGrid)

#Self-Esteem - adapted from Rosenberg Self-Esteem Measure

RosenbergGrid = S6CMint[,288:292]
RosenbergGrid = apply(RosenbergGrid,2,function(x) Convert(x,1:4,4:1))
colnames(RosenbergGrid) = paste0("S6_ChildQ_SelfEsteem_",1:5)
df0 = cbind.data.frame(df0, RosenbergGrid)

rm(WellbeingGrid,RosenbergGrid)

#Mental Health
df0$S6_ChildQ_Feelings_Unhappy = Convert(S6CMint[,293], 1:3,3:1) #Pos. = 293	Variable = FCMDSA00	Variable label = FeelingsGrid: I felt miserable or unhappy   
df0$S6_ChildQ_Feelings_Anhedonia = Convert(S6CMint[,294], 1:3,3:1) #Pos. = 294	Variable = FCMDSB00	Variable label = FeelingsGrid: I didn't enjoy anything at all
df0$S6_ChildQ_Feelings_Tired = Convert(S6CMint[,295], 1:3,3:1) #Pos. = 295	Variable = FCMDSC00	Variable label = FeelingsGrid: I felt so tired I just sat around and did nothing 
df0$S6_ChildQ_Feelings_Restless = Convert(S6CMint[,296], 1:3,3:1) #Pos. = 296	Variable = FCMDSD00	Variable label = FeelingsGrid: I was very restless   
df0$S6_ChildQ_Feelings_NoGood = Convert(S6CMint[,297], 1:3,3:1) #Pos. = 297	Variable = FCMDSE00	Variable label = FeelingsGrid: I felt I was no good any more
df0$S6_ChildQ_Feelings_Cried = Convert(S6CMint[,298], 1:3,3:1) #Pos. = 298	Variable = FCMDSF00	Variable label = FeelingsGrid: I cried a lot
df0$S6_ChildQ_Feelings_Concentrate = Convert(S6CMint[,299], 1:3,3:1) #Pos. = 299	Variable = FCMDSG00	Variable label = FeelingsGrid: I found it hard to think properly or concentrate
df0$S6_ChildQ_Feelings_HateSelf = Convert(S6CMint[,300], 1:3,3:1) #Pos. = 300	Variable = FCMDSH00	Variable label = FeelingsGrid: I hated myself
df0$S6_ChildQ_Feelings_BadPerson = Convert(S6CMint[,301], 1:3,3:1) #Pos. = 301	Variable = FCMDSI00	Variable label = FeelingsGrid: I was a bad person
df0$S6_ChildQ_Feelings_Lonely = Convert(S6CMint[,302], 1:3,3:1) # Pos. = 302	Variable = FCMDSJ00	Variable label = FeelingsGrid: I felt lonely 
df0$S6_ChildQ_Feelings_Unloved = Convert(S6CMint[,303], 1:3,3:1) # Pos. = 303	Variable = FCMDSK00	Variable label = FeelingsGrid: I thought nobody really loved me  
df0$S6_ChildQ_Feelings_NotGood = Convert(S6CMint[,304], 1:3,3:1) # Pos. = 304	Variable = FCMDSL00	Variable label = FeelingsGrid: I thought I could never be as good as other kids  
df0$S6_ChildQ_Feelings_Wrong = Convert(S6CMint[,305], 1:3,3:1) # Pos. = 305	Variable = FCMDSM00	Variable label = FeelingsGrid: I did everything wrong

df0$S6_ChildQ_SelfHarm = Convert(S6CMint[,306], 1:2,0:1) #Pos. = 306 Variable = FCHARM00 Variable label = In the past year has CM self-harmed 



#Drug Experimentation

df0$S6_RiskyBehaviours_Cig =      Convert(S6CMint[,185], 1:6,c(1,0,0,0,0,0)) #Pos. = 185	Variable = FCSMOK00	Variable label = How often CM smokes cigarettes  
df0$S6_RiskyBehaviours_ECig =     Convert(S6CMint[,187],1:4,c(1,0,0,0)) #Pos. = 187	Variable = FCECIG00	Variable label = How often CM smokes e-cigarettes
df0$S6_RiskyBehaviours_Alcohol =  Convert(S6CMint[,189], 1:2,0:1) #Pos. = 189	Variable = FCALCD00	Variable label = Has CM ever had an alcoholic drink  
df0$S6_RiskyBehaviours_Cannabis = Convert(S6CMint[,197], 1:2,0:1) #Pos. = 197	Variable = FCCANB00	Variable label = DruGrid: Cannabis (also known as weed, marijuana, dope, hash or skunk)? 
#df0$S6_RiskyBehaviours_OtherDrug= Convert(S6CMint[,198], 1:2,1:2) #Pos. = 198	Variable = FCOTDR00	Variable label = DruGrid: Any other illegal drug (such as ecstasy, cocaine, speed)?  



#SES metrics
#Raw income data
df0$S6_SES_Income_TotalHousehold_JOINT = Convert(S6ParInt[,655],2:20, c(50,155,250,305,335,360,410,485,555,625,705,800,925,1050,1200,1450,1850,2400,3000))
df0$S6_SES_Income_TotalHousehold_NOPARTNER = Convert(S6ParInt[,656],2:20, c(30,95,155,190,210,225,255,300,335,370,405,445,495,535,575,650,780,980,1220))
df0$S6_SES_Income_TotalHousehold = df0$S6_SES_Income_TotalHousehold_JOINT
df0[which(!is.na(df0$S6_SES_Income_TotalHousehold_NOPARTNER)),"S6_SES_Income_TotalHousehold"]=df0$S6_SES_Income_TotalHousehold_NOPARTNER[which(!is.na(df0$S6_SES_Income_TotalHousehold_NOPARTNER))]


#Word Score Data
df0$S6_Cog_WordScore = setNA(S6Cog[75], -1:-9)
df0$S6_Cog_WordScoreNorm = Normalise(df0$S6_Cog_WordScore)
df0$S6_Cog_WordScoreLanguage = S6Cog[42]

WordDf = S6Cog[20:39]
colnames(WordDf)=paste0("S6_Cog_Word_",1:20)
WordDf[WordDf==-3 | WordDf==-1]=NA
CorrectAnswer=c(5,5,2,2,5,
                3,1,5,5,4,
                2,2,5,4,3,
                5,1,1,3,3)

for(i in 1:20){
  WordDf[i]=as.numeric(WordDf[i]==CorrectAnswer[i])
}
df0=cbind(df0, WordDf)



rm(S6WORD,WordDf)

#Cambridge Gambling Task
  S6_CGT = S6Der[,24:29]
  S6_CGT = data.frame(apply(S6_CGT, 2, function(x) setNA(x, -9)))
  colnames(S6_CGT) = c("S6_Cog_CGT_DelayAversion",
                       "S6_Cog_CGT_DeliberationTime",
                       "S6_Cog_CGT_OverallProportionBet",
                       "S6_Cog_CGT_QualityDecision",
                       "S6_Cog_CGT_RiskAdjustment",
                       "S6_Cog_CGT_RiskTaking"
                       )
  
  df0 = cbind.data.frame(df0, S6_CGT)
  df0$S6_Cog_CGT_RiskTaking = df0$S6_Cog_CGT_RiskTaking*-1 #recode so higher means less risk taking
  


#Age
S6Derived = fread("UKDA-8156-tab MCS6/tab/mcs6_cm_derived.tab", stringsAsFactors = FALSE, data.table = FALSE)
S6Derived = S6Derived[S6Derived$FCNUM00==1,]
S6Derived = S6Derived[match(df0$MCSID, S6Derived$MCSID),]

# age

InterviewDateMonth = S6Derived[,3] #No NAs (-1:-9)
InterviewDateYear  = S6Derived[,4] #No NAs (-1:-9)
InterviewDate = paste0("15","/",InterviewDateMonth,"/",InterviewDateYear)   # Assume interview was in the middle of the year
InterviewDate[grepl("NA",InterviewDate)] = NA
InterviewDate = as.Date(InterviewDate, format="%d/%m/%Y")

BirthMonth = S6Derived[,6] #No NAs
BirthYear = S6Derived[,7] #No NAs
BirthDate = paste0("15","/",BirthMonth,"/",BirthYear)   # Assume interview was in the middle of the year
BirthDate[grepl("NA",BirthDate)] = NA
BirthDate = as.Date(BirthDate, format="%d/%m/%Y")

df0$S6_Age_Est = as.numeric(InterviewDate-BirthDate)/365.25

df0$S6_Age = setNA(S6Derived[17], -1)
#WordScore Corrected for Age
df0$S6_Cog_WordScoreResidNorm = Normalise(residuals(lm(S6_Cog_WordScore ~ S6_Age, data=df0, na.action = na.exclude)))
#BMI 
df0$S6_BMI = setNA(S6Derived[18],-1)

rm(S6Cog,S6Derived, S6_CGT, InterviewDate, InterviewDateMonth, InterviewDateYear, BirthDate, BirthYear, BirthMonth)

#######################################################################################################################
####################################################     Other     #################################################### 
#######################################################################################################################


######### Air Polution ############


AirPollutionData = list() #List of dataframes with airpollution data per sweep

#Read Air Pollution Dataframes, Only include variables of Interest
for (i in 1:6){
  AirPollutionData[[i]] = fread(file=paste0("UKDA-8153-tab Air Pollution/tab/medix_deciles_mcs",i,".tab"), stringsAsFactors = FALSE, data.table = FALSE)
  AirPollutionData[[i]] = AirPollutionData[[i]][match(df0$MCSID, AirPollutionData[[i]]$MCSID),c(3:6)]
  colnames(AirPollutionData[[i]]) = c(         paste0("S",i,"_AirPollution_Sulphur"),paste0("S",i,"_AirPollution_Particularate"),
                                               paste0("S",i,"_AirPollution_Nitrogen"),paste0("S",i,"_AirPollution_CarbonMonoxide"))
}

AirPollutionDataMatrix = data.frame(do.call("cbind.data.frame",AirPollutionData))

AirPollutionDataMatrix = data.frame(apply(AirPollutionDataMatrix, 2, function(x) Convert(x,1:10,10:1)))

#Create 4 seperate dataframes for each type of pollutant seperately 

AirPol_SeperatedByType = lapply(1:4, function(i) AirPollutionDataMatrix[4*(0:5)+i])
AirPol_Avg=
  sapply(AirPol_SeperatedByType, function(df) 
    apply(df,1, function(x) mean(x, na.rm = TRUE)))
  colnames(AirPol_Avg) = c("S_AirPollution_Sulphur","S_AirPollution_Particularate","S_AirPollution_Nitrogen","S_AirPollution_CarbonMonoxide")

AirPollutionDataMatrix = cbind.data.frame(AirPollutionDataMatrix, AirPol_Avg)

#plotmat(cor(AirPollutionDataMatrix, use="pairwise.complete.obs"), axis_labels = colnames(AirPollutionDataMatrix))

df0 = cbind.data.frame(df0,AirPollutionDataMatrix)

rm(AirPol_Avg, AirPol_SeperatedByType,AirPollutionData, AirPollutionDataMatrix)


######### Other Datasets ######### 
#Harmonised height, weight info

SBMI = fread("UKDA-8340-tab harmonised MBI/tab/mcs_closer_wp1.tab", stringsAsFactors = FALSE, data.table = FALSE) 

table(SBMI$wtself) #Check that all assessments are measured weight 
table(SBMI$wtimp) #Some measures may not be imperial? 
table(SBMI$wtself)
table(SBMI$htimp)

#Create different datasets for each sweep
SBMI_S0 = SBMI[SBMI$visitage==0,] 
SBMI_S0 = SBMI_S0[match(df0$MCSID, SBMI_S0$mcsid),]

SBMI_S1 = SBMI[SBMI$visitage==1,] 
SBMI_S1 = SBMI_S1[match(df0$MCSID, SBMI_S1$mcsid),]

SBMI_S2 = SBMI[SBMI$visitage==3,]       
SBMI_S2 = SBMI_S2[match(df0$MCSID, SBMI_S2$mcsid),]

SBMI_S3 = SBMI[SBMI$visitage==5,] 
SBMI_S3 = SBMI_S3[match(df0$MCSID, SBMI_S3$mcsid),]

SBMI_S4 = SBMI[SBMI$visitage==7,] 
SBMI_S4 = SBMI_S4[match(df0$MCSID, SBMI_S4$mcsid),]

SBMI_S5 = SBMI[SBMI$visitage==11,] 
SBMI_S5 = SBMI_S5[match(df0$MCSID, SBMI_S5$mcsid),]

#Add in data
df0$S0_Health_Height = SBMI_S0$wt
df0$S0_Health_Weight = SBMI_S0$ht #Looks like no height measurements were taken 
df0$S0_Health_Age = SBMI_S0$xage #Looks like no height measurements were taken 

df0$S1_Health_Height = SBMI_S1$wt
df0$S1_Health_Weight = SBMI_S1$ht
df0$S1_Health_Age = SBMI_S1$xage #Looks like no height measurements were taken 

df0$S2_Health_Height = SBMI_S2$wt
df0$S2_Health_Weight = SBMI_S2$ht
df0$S2_Health_Age = SBMI_S2$xage #Looks like no height measurements were taken 

df0$S3_Health_Height = SBMI_S3$wt
df0$S3_Health_Weight = SBMI_S3$ht
df0$S3_Health_Age = SBMI_S3$xage #Looks like no height measurements were taken 

df0$S4_Health_Height = SBMI_S4$wt
df0$S4_Health_Weight = SBMI_S4$ht
df0$S4_Health_Age = SBMI_S4$xage #Looks like no height measurements were taken 

df0$S5_Health_Height = SBMI_S5$wt
df0$S5_Health_Weight = SBMI_S5$ht
df0$S5_Health_Age = SBMI_S5$xage #Looks like no height measurements were taken 


df0$S34_Health_HeightDiff = df0$S4_Health_Height - df0$S3_Health_Height 
df0$S45_Health_HeightDiff = df0$S5_Health_Height - df0$S4_Health_Height 
df0$S35_Health_HeightDiff = df0$S5_Health_Height - df0$S3_Health_Height 



rm(SBMI_S0,SBMI_S1,SBMI_S2,SBMI_S3,SBMI_S4,SBMI_S5,SBMI,S6CMint)


# #Variable calculations
# df0$S45_Health_HeightDiff_Norm = Normalise(df0$S45_Health_HeightDiff)
# df0$S5_Health_Height_Norm = Normalise(df0$S5_Health_Height)
# df0$S45_Teach_EngMathSciAvg_Norm_Diff = df0$S5_Teach_EngMathSciAvg_Norm - df0$S4_Teach_EngMathSciAvg_Norm
# 
# #Changes in SES
# df0$S45_SES_FSN = df0$S5_SES_FSN - df0$S4_SES_FSN
# df0$S15_SES_FSN = df0$S5_SES_FSN - df0$S1_SES_FSN


#######################################################################################################################
#################################################     Data Export     ################################################# 
#######################################################################################################################

rownames(df0) = NULL
save(df0, file=file.path(OUPUTDATA_LOCATION,"MCS_MasterDataset_FINAL.Rdata"))
write.csv(apply(df0, 2, as.character), file=file.path(OUPUTDATA_LOCATION,"MCS_MasterDataset_CSV_FINAL.csv"), na="NA")


#Split into two groups. Make sure roughly equal number of teacher responses in each
df0$S5_Teach_IsThereData = apply(df0[,findMCS("S5_Teach_AA")],1,function(x) length(which(is.na(x)))<7)

set.seed(10)
df0_Rand = df0[sample(nrow(df0)),]

df0_SplitTeach = split(df0_Rand, df0_Rand$S5_Teach_IsThereData)

group1 = c(df0_SplitTeach[[1]][1:floor(nrow(df0_SplitTeach[[1]])/2),"MCSID"],
           df0_SplitTeach[[2]][1:floor(nrow(df0_SplitTeach[[2]])/2),"MCSID"])
group2 = c(df0_SplitTeach[[1]][(floor(nrow(df0_SplitTeach[[1]])/2)+1):nrow(df0_SplitTeach[[1]]),"MCSID"],
           df0_SplitTeach[[2]][(floor(nrow(df0_SplitTeach[[2]])/2)+1):nrow(df0_SplitTeach[[2]]),"MCSID"])
           
df0_group1 = df0[match(group1,df0$MCSID),]
df0_group2 = df0[match(group2,df0$MCSID),]

df0_group1$group = 1
df0_group2$group = 2

table(duplicated(c(df0_group1$MCSID,df0_group2$MCSID)))

save(df0_group1, file=file.path(OUPUTDATA_LOCATION,"MCS_MasterDataset_group1_FINAL.Rdata"))
save(df0_group2, file=file.path(OUPUTDATA_LOCATION,"MCS_MasterDataset_group2_FINAL.Rdata"))


# apply(df0[,findMCS("Teach")],2,function(x) length(which(!is.na(x))))


#### Notes

#Remember when looking at parent responses, ELIG should be used to find who is the main responder for a given sweep, but i think pnum is attached to the same person across sweeps!

###