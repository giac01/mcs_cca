
#Load file
rm(list=ls())
gc()
setwd("/imaging/gb01/Paper 2 - Cumulative Risk/Paper 2 - Cumulative Risk")
source("Script 0 - Load Useful things for Analyses.R")

RelCheck(vars_vector = c("S2_Parenting_ReadToChild","S2_Parenting_HelpWithAlphabet","S2_Parenting_HelpWithCounting"))
RelCheck(vars = c("S1_ParentSelfEsteem"))
RelCheck(vars = c("S1_ParentMentalHealth"))
RelCheck(vars = c("S2_ParentMentalHealth"))
RelCheck(vars = c("Locus"))
RelCheck(vars = c("S1_Parent_Neighbourhood"))
RelCheck(vars = c("S1_ParentRelationship"))

#Parent SDQ check 
RelCheck(vars_vector = paste0("S6_Parent_SDQ_",c(3,8,13,16,24))) #Emotion
RelCheck(vars_vector = paste0("S6_Parent_SDQ_",c(5,7,12,18,22))) #Conduct
RelCheck(vars_vector = paste0("S6_Parent_SDQ_",c(2,10,15,21,25))) #Hyper
RelCheck(vars_vector = paste0("S6_Parent_SDQ_",c(6,11,14,19,23))) #Peer
RelCheck(vars_vector = paste0("S6_Parent_SDQ_",c(1,4,9,17,20))) #Prosocial

#Teacher SDQ check 
RelCheck(vars_vector = paste0("S5_Teach_SDQ_",c(3,8,13,16,24))) #Emotion
RelCheck(vars_vector = paste0("S5_Teach_SDQ_",c(5,7,12,18,22))) #Conduct
RelCheck(vars_vector = paste0("S5_Teach_SDQ_",c(2,10,15,21,25))) #Hyper
RelCheck(vars_vector = paste0("S5_Teach_SDQ_",c(6,11,14,19,23))) #Peer
RelCheck(vars_vector = paste0("S5_Teach_SDQ_",c(1,4,9,17,20))) #Prosocial



#Theory-Based Coding
Emotion = apply(SDQ[,c(3,8,13,16,24)], 1, function(x) mean(x, na.rm=TRUE)) 
Conduct = apply(SDQ[,c(5,7,12,18,22)], 1, function(x) mean(x, na.rm=TRUE)) 
Hyper = apply(SDQ[,c(2,10,15,21,25)], 1, function(x) mean(x, na.rm=TRUE)) 
Peer = apply(SDQ[,c(6,11,14,19,23)], 1, function(x) mean(x, na.rm=TRUE)) 
Prosocial = apply(SDQ[,c(1,4,9,17,20)], 1, function(x) mean(x, na.rm=TRUE)) 


RelCheck(vars_vector  = c("S5_Teach_AA_Eng","S5_Teach_AA_Math","S5_Teach_AA_Science"))
RelCheck(vars = c("S6_ChildQ_SelfEsteem_"))
RelCheck(vars = c("S6_ChildQ_Well"))
RelCheck(vars_vector =  c("S6_ChildQ_Feelings_NoGood","S6_ChildQ_Feelings_Cried","S6_ChildQ_Feelings_HateSelf","S6_ChildQ_Feelings_BadPerson","S6_ChildQ_Feelings_Lonely","S6_ChildQ_Feelings_Unloved","S6_ChildQ_Feelings_NotGood","S6_ChildQ_Feelings_Wrong"))
RelCheck(vars_vector  = c("S6_ChildQ_Feelings_Unhappy","S6_ChildQ_Feelings_Anhedonia","S6_ChildQ_Feelings_Tired","S6_ChildQ_Feelings_Restless","S6_ChildQ_Feelings_Concentrate"))

#Reliability check vocab
Vocab_DF = na.omit(df0[,findMCS("S6_Cog_Word_[0-9]")])
apply(Vocab_DF,2, table)
RelCheck(vars="S6_Cog_Word_[0-9]")
psych::alpha(Vocab_DF)
IRT_Vocab = mirt::mirt(Vocab_DF,1,itemtype="3PL", method="EM", optimizer = "BFGS", technical = list("NCYCLES"=50000), guess=1/5, upper=1)
plot(IRT_Vocab, type="rxx", theta_lim=c(-3,3))
plot(IRT_Vocab, type="trace", theta_lim=c(-2,2))


#Short mood and feeling questionnaire 

SMFQ = na.omit(df0[,findMCS("S6_ChildQ_Feelings")])
apply(SMFQ,2,table)

library(mirt)


IRT_SMFQ_1 = mirt::mirt(SMFQ,1,itemtype="gpcm", method="EM", optimizer = "BFGS" )
IRT_SMFQ_2 = mirt::mirt(SMFQ,2,itemtype="gpcm", method="EM", optimizer = "BFGS" )
IRT_SMFQ_3 = mirt::mirt(SMFQ,3,itemtype="gpcm", method="EM", optimizer = "BFGS" )

plot(IRT_SMFQ_1, type="rxx", theta_lim=c(-2,2))
plot(IRT_SMFQ_2, type="trace", theta_lim=c(-2,2))

summary(IRT_SMFQ_2)

#Other 


RelCheck(vars = "S6_ChildQ_BB|S6_ChildQ_Truancy_Ever") #Antisocial behaviour
Antisocial_IRT = mirt::mirt(na.omit(df0[,findMCS("S6_ChildQ_BB|S6_ChildQ_Truancy_Ever")]),1,itemtype="3PL", method="EM", optimizer = "BFGS", technical = list("NCYCLES"=50000), guess=0, upper=1)
plot(Antisocial_IRT, type="rxx", theta_lim=c(-3,3))


RelCheck(vars = "Q_Bully")
RelCheck(vars = "S1_Air|S2_Air")
cor(df0[,findMCS("S1_Air|S2_Air")], use="pairwise.complete.obs")

RelCheck(vars = c())
RelCheck(vars = c())
RelCheck(vars = c())
RelCheck(vars = c())
RelCheck(vars = c())
RelCheck(vars = c())
RelCheck(vars = c())


S2_ParentMentalHealth