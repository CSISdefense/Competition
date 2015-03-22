require(ggplot2)
Path<-"K:\\2007-01 PROFESSIONAL SERVICES\\R scripts and data\\"
source(paste(Path,"helper.r",sep=""))
source(paste(Path,"lookups.r",sep=""))
setwd("K:\\Development\\Competition")
source("statistics_aggregators.R")

options(error=recover)
options(warn=1)



mcc.competition.combined<-competition.statistics(
    "",
    "data\\defense_Office_SP_DefenseCompetitionHistoryBucketPlatformSubCustomerMajorCommand.csv",
    "MajorCommandID"
  )
# debug(competition.statistics.pt2)
mcc.competition.combined<-plyr::join(mcc.competition.combined,competition.statistics.pt2(
    "",
    "data\\defense_Office_SP_DefenseCompetitionPricingVehicleHistoryMajorCommand.csv",
    "MajorCommandID"
)
)
  
mcc.competition.combined<-plyr::join(mcc.competition.combined,competition.statistics.pt3(""
                                                                                   ,"data\\Defense_Office_SP_ContractSizeforCompetitionStudy.csv"
                                                                                   ,"MajorCommandID"
)
)


MCCshortList<-subset(mcc.competition.combined,!MajorCommandID %in% c("ORG-2841",
                                               "ORG-2776",
                                               "ORG-4793",
                                               "ORG-2849",
                                               "ORG-2762",
                                               "ORG-4020",
                                               "ORG-2840",
                                               "ORG-2757")
)

MCC.fit <- lm(pEffectiveComp ~ 
                  pService:pProduct+ 
                  pAir  +
                  pMnS +
                  pWnA +
                  pEnC  +
                  pIDV +
                  pService +
                  pService:pIDV +
                  pProduct:pService:pIDV
              , data=MCCshortList)
summary(MCC.fit) 
plot(MCC.fit)


mcc.competition.combined<-cbind(mcc.competition.combined,predict(fit,mcc.competition.combined,interval="prediction",level=0.95))
subset(mcc.competition.combined,lwr>pEffectiveComp|upr<pEffectiveComp)$MajorCommandID


write.table(mcc.competition.combined
  ,file="data\\defense_mcc_competition_combined.csv"
#   ,header=TRUE
  , sep=","
  , row.names=FALSE
  , append=FALSE
         )



mcc.competition.2014<-competition.statistics(
    "",
    "data\\2014DefenseCompetitionByPlatformAreaMajorCommand.csv",
    "MajorCommandID",
    2014
)


mcc.competition.2014<-plyr::join(mcc.competition.2014,competition.statistics.pt2a(
    "",
    "data\\2014DefenseCompetitionVehicleByPlatformAreaMajorCommand.csv",
    "MajorCommandID",
    2014
)
)

mcc.competition.2014<-cbind(mcc.competition.2014,
                              predict(MCC.fit,
                                      mcc.competition.2014,
                                      interval="prediction",
                                      level=0.95)
)
subset(mcc.competition.2014,lwr>pEffectiveComp|upr<pEffectiveComp)$MajorCommandID




write.table(mcc.competition.2014
            ,file="Output\\2014_defense_MCC_competition_combined.csv"
            #   ,header=TRUE
            , sep=","
            , row.names=FALSE
            , append=FALSE
)



write.table(mcc.competition.combined
            ,file="Output\\2000_2013_defense_mcc_competition_combined.csv"
            #   ,header=TRUE
            , sep=","
            , row.names=FALSE
            , append=FALSE
)


state.competition.combined<-competition.statistics(""
                                       ,"data\\defense_Location_SP_DefenseCompetitionStatePoPHistoryBucketPlatform.csv"
                                       ,"PoPstateCode"
)
# debug(competition.statistics.pt2)
state.competition.combined<-plyr::join(state.competition.combined,competition.statistics.pt2(""
                                                                                   ,"data\\defense_Location_SP_DefenseCompetitionPricingVehiclePoPstateHistory.csv"
                                                                                   ,"PoPstateCode"
)
)

state.competition.combined<-plyr::join(state.competition.combined,competition.statistics.pt3(""
                                                                                       ,"data\\Defense_Location_SP_ContractSizeforCompetitionStudy.csv"
                                                                                       ,"PoPstateCode"
)
)



ShortStateList <-subset(state.competition.combined ,!PoPstateCode %in% c("VI",
                                                   "AS",
                                                   "MP",
                                                   "AE",
                                                   "PW",
                                                   "US",
                                                   "UK",
                                                   "GM",
                                                   "GB",
                                                   "JA",
                                                   "ON",
                                                   "QC",
                                                   "   ",
                                                   "BC"))



StateFit <- lm(pEffectiveComp ~
                   pAir  +
                   pMnS +
                   pEnC +
                   pIDV+ 
                   pVessel+
                   pProduct+
                   pEnC:pProduct, 
               data=ShortStateList)
summary(StateFit) 
plot(StateFit)


write.table(state.competition.combined
            ,file="data\\defense_state_competition_combined.csv"
            #   ,header=TRUE
            , sep=","
            , row.names=FALSE
            , append=FALSE
)


state.competition.combined<-cbind(state.competition.combined,predict(StateFit,state.competition.combined,interval="prediction",level=0.95))
subset(state.competition.combined,lwr>pEffectiveComp|upr<pEffectiveComp)$PoPstateCode



state.competition.2014<-competition.statistics(
    "",
    "data\\2014DefenseCompetitionByPlatformAreaPoP.csv",
    "PoPstateCode",
    2014
)


state.competition.2014<-plyr::join(state.competition.2014,competition.statistics.pt2a(""
                                                                                             ,"data\\2014DefenseCompetitionVehicleByPlatformAreaPoP.csv"
                                                                                             ,"PoPstateCode"
                                                                                             ,2014
)
)

state.competition.2014<-cbind(state.competition.2014,
                              predict(StateFit,
                                      state.competition.2014,
                                      interval="prediction",
                                      level=0.95)
                              )
subset(state.competition.2014,lwr>pEffectiveComp|upr<pEffectiveComp)$PoPstateCode




write.table(state.competition.2014
            ,file="Output\\2014_defense_state_competition_combined.csv"
            #   ,header=TRUE
            , sep=","
            , row.names=FALSE
            , append=FALSE
)

write.table(state.competition.combined
            ,file="Output\\2000_2013_defense_state_competition_combined.csv"
            #   ,header=TRUE
            , sep=","
            , row.names=FALSE
            , append=FALSE
)


# state.competition.massive <-read.csv(
#   paste(Path,"data\\defense_Location_SP_DefenseCompetitionStatePoPHistoryBucketPlatform.csv",sep=""),
#   header=TRUE, sep=",", dec=".", strip.white=TRUE, 
#   na.strings="NULL",
#   stringsAsFactors=TRUE
# )


# debug(apply_lookups)
# state.competition.massive<-apply_lookups(Path,state.competition.massive)

# state.competition.combined<-competition.statistics(state.competition.massive,"PoPstateCode")


# state.competition$SumOf

# 
# 
# #"defense_SumofObligatedAmount_SumOfbaseandexercisedoptionsvalue_StartFiscal_Year_exercisedweighted"
# 
# SpentvExpected<-qplot(log10(SumofObligatedAmount)
#                       ,log10(SumOfbaseandexercisedoptionsvalue)
#                       ,data=mcc.competition[sample(nrow(mcc.competition), size=15000, prob=abs(mcc.competition$SumofObligatedAmount)),]
#                       ,color=IsIDV
# )+facet_wrap(IsIDV~StartFiscal_Year)
# 
# png(
#   paste(Path
#         
#         ,"defense_SumofObligatedAmount_SumOfbaseandexercisedoptionsvalue_StartFiscal_Year_obligationweighted"
#         ,".png"
#         , sep=""
#   )
#   , width=6#VAR.width
#   , height=6#VAR.height
#   , units='in'
#   , res=300
# )
# 
# print(SpentvExpected)
# 
# if (!(dev.cur()[[1]]==1)){
#   dev.off()
# }
# 
# #"defense_SumofObligatedAmount_Sumofbaseandalloptionsvalue_StartFiscal_Year_obligationweighted"
# SpentvExpected<-qplot(log10(SumofObligatedAmount)
#                       ,log10(Sumofbaseandalloptionsvalue)
#                       ,data=mcc.competition[sample(nrow(mcc.competition), size=15000, prob=abs(mcc.competition$SumofObligatedAmount)),]
#                       ,color=IsIDV
# )+facet_wrap(StartFiscal_Year~IsIDV)
# 
# 
# png(
#   paste(Path
#         
#         ,"defense_SumofObligatedAmount_Sumofbaseandalloptionsvalue_StartFiscal_Year_obligationweighted"
#         ,".png"
#         , sep=""
#   )
#   , width=6#VAR.width
#   , height=6#VAR.height
#   , units='in'
#   , res=300
# )
# 
# print(SpentvExpected)
# 
# if (!(dev.cur()[[1]]==1)){
#   dev.off()
# }
# 
# mcc.competition<-subset(mcc.competition,!is.na(SumOfbaseandexercisedoptionsvalue)&!is.na(Sumofbaseandalloptionsvalue))
# 
# 
# #"defense_SumofObligatedAmount_SumOfbaseandexercisedoptionsvalue_StartFiscal_Year_exercisedweighted"
# SpentvExpected<-qplot(log10(SumofObligatedAmount)
#                       ,log10(SumOfbaseandexercisedoptionsvalue)
#                       ,data=mcc.competition[sample(nrow(mcc.competition), size=15000, prob=abs(mcc.competition$SumOfbaseandexercisedoptionsvalue)),]
#                       ,color=IsIDV
# )+facet_wrap(StartFiscal_Year~IsIDV)
# 
# 
# png(
#   paste(Path
#         
#         ,"defense_SumofObligatedAmount_SumOfbaseandexercisedoptionsvalue_StartFiscal_Year_exercisedweighted"
#         ,".png"
#         , sep=""
#   )
#   , width=6#VAR.width
#   , height=6#VAR.height
#   , units='in'
#   , res=300
# )
# 
# print(SpentvExpected)
# 
# if (!(dev.cur()[[1]]==1)){
#   dev.off()
# }
# 
# 
# #"defense_SumofObligatedAmount_Sumofbaseandalloptionsvalue_StartFiscal_Year_allweighted"
# SpentvExpected<-qplot(log10(SumofObligatedAmount)
#                       ,log10(Sumofbaseandalloptionsvalue)
#                       ,data=mcc.competition[sample(nrow(mcc.competition), size=15000, prob=abs(mcc.competition$Sumofbaseandalloptionsvalue)),]
#                       ,color=IsIDV
# )+facet_wrap(StartFiscal_Year~IsIDV)
# 
# 
# png(
#   paste(Path
#         
#         ,"defense_SumofObligatedAmount_Sumofbaseandalloptionsvalue_StartFiscal_Year_allweighted"
#         ,".png"
#         , sep=""
#   )
#   , width=6#VAR.width
#   , height=6#VAR.height
#   , units='in'
#   , res=300
# )
# 
# print(SpentvExpected)
# 
# if (!(dev.cur()[[1]]==1)){
#   dev.off()
# }