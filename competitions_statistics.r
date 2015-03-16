require(ggplot2)
Path<-"K:\\2007-01 PROFESSIONAL SERVICES\\R scripts and data\\"
source(paste(Path,"helper.r",sep=""))
source(paste(Path,"lookups.r",sep=""))

options(error=recover)
options(warn=1)



mcc.competition.combined<-competition.statistics(Path
    ,"data\\defense_Office_SP_DefenseCompetitionHistoryBucketPlatformSubCustomerMajorCommand.csv"
    ,"MajorCommandID"
  )
# debug(competition.statistics.pt2)
mcc.competition.combined<-join(mcc.competition.combined,competition.statistics.pt2(Path
                                                 ,"data\\defense_Office_SP_DefenseCompetitionPricingVehicleHistoryMajorCommand.csv"
                                                 ,"MajorCommandID"
)
)
  
mcc.competition.combined<-join(mcc.competition.combined,competition.statistics.pt3(Path
                                                                                   ,"data\\Defense_Office_SP_ContractSizeforCompetitionStudy.csv"
                                                                                   ,"MajorCommandID"
)
)

write.table(mcc.competition.combined
  ,file=paste(Path,"data\\defense_mcc_competition_combined.csv",sep="")
#   ,header=TRUE
  , sep=","
  , row.names=FALSE
  , append=FALSE
         )


state.competition.combined<-competition.statistics(Path
                                       ,"data\\defense_Location_SP_DefenseCompetitionStatePoPHistoryBucketPlatform.csv"
                                       ,"PoPstateCode"
)
# debug(competition.statistics.pt2)
state.competition.combined<-join(state.competition.combined,competition.statistics.pt2(Path
                                                                                   ,"data\\defense_Location_SP_DefenseCompetitionPricingVehiclePoPstateHistory.csv"
                                                                                   ,"PoPstateCode"
)
)

state.competition.combined<-join(state.competition.combined,competition.statistics.pt3(Path
                                                                                       ,"data\\Defense_Location_SP_ContractSizeforCompetitionStudy.csv"
                                                                                       ,"PoPstateCode"
)
)

write.table(state.competition.combined
            ,file=paste(Path,"data\\defense_state_competition_combined.csv",sep="")
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