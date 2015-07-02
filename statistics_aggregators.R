require(stringr)


statistics.pivot<-function(VAR.Path
                           ,competition.file.name
                           ,VAR.Attribute
                           ,VAR.UnitOfAnalysis
                           ,LimitFiscalYear=NA
                           ,UnlabeledValue=NA
                           ,ColumnPrefix="p"
                           
){
    
    
    competition.DF <-read.csv(
        competition.file.name,
        header=TRUE, sep=",", dec=".", strip.white=TRUE, 
        na.strings="NULL",
        stringsAsFactors=TRUE
    )
    
    if(!is.na(LimitFiscalYear)){
        competition.DF<-subset(competition.DF,fiscal_year==LimitFiscalYear)
    }
    
    
    competition.DF<-apply_lookups(VAR.Path,competition.DF)
    
    if(!is.na(UnlabeledValue)){
        competition.DF<-competition.DF[competition.DF[,VAR.Attribute]!=UnlabeledValue,]
        
    }
    
    competition.DF<-ddply(competition.DF
          , VAR.UnitOfAnalysis
          , transform
          , percent=Obligation.2014/sum(Obligation.2014)
    )
    
    competition.combined<-ddply(competition.DF
                          , VAR.UnitOfAnalysis
                          , summarise
                          , p=0
    )
    
    competition.combined<-subset(competition.combined,select=-c(p))
   
    
    for(f in unique(competition.DF[,VAR.Attribute])){
        competition.combined<-plyr::join(competition.combined
                                         ,ddply(.data=competition.DF[competition.DF[,VAR.Attribute]==f,]
                                                ,VAR.UnitOfAnalysis
                                                ,summarise
                                                ,p=sum(percent)
                                         )
        )
        competition.combined$p[is.na(competition.combined$p)]<-0
        competition.combined[,paste(ColumnPrefix,f,sep="")]<-competition.combined$p
        competition.combined<-subset(competition.combined,select=-c(p))
    }
    
    standardize_variable_names(VAR.Path,competition.combined)
}


# 
# competition.statistics<-function(VAR.Path
#                                  ,competition.file.name
#                                  ,VAR.UnitOfAnalysis
#                                  ,LimitFiscalYear=NULL
#                                  
# ){
#     
#     
#     competition.DF <-read.csv(
#         competition.file.name,
#         header=TRUE, sep=",", dec=".", strip.white=TRUE, 
#         na.strings="NULL",
#         stringsAsFactors=TRUE
#     )
#     
#     if(!is.null(LimitFiscalYear)){
#         competition.DF<-subset(competition.DF,fiscal_year==LimitFiscalYear)
#     }
#     
#     competition.DF<-apply_lookups(VAR.Path,competition.DF)
#     
#     
#     
#     labeled.competition.DF<-ddply(subset(competition.DF
#                                          ,Competition.sum!="Unlabeled"
#     )
#     , VAR.UnitOfAnalysis
#     , transform
#     , percent=Obligation.2014/sum(Obligation.2014)
#     )
#     
#     
#     competition.combined<-ddply(.data=subset(labeled.competition.DF
#                                              ,Competition.effective.only=="Effective Comp.")
#                                 ,VAR.UnitOfAnalysis
#                                 ,summarise
#                                 ,pEffectiveComp=sum(percent)
#     )
#     # 
#     #   ddply(subset(labeled.competition.DF
#     #                     ,Competition.effective.only=="Effective Comp.")
#     #         ,.(MajorCommandID)
#     #         ,summarise
#     #         ,pEffectiveComp=sum(percent)
#     #   )
#     
#     
#     competition.combined$pEffectiveComp[is.na(competition.combined$pEffectiveComp)]<-0
#     rm(labeled.competition.DF)
#     
#     prodservsimple<-ddply(subset(competition.DF
#                                  ,ServicesCategory.sum!="Unlabeled"
#     )
#     , VAR.UnitOfAnalysis
#     , transform
#     , percent=Obligation.2014/sum(Obligation.2014)
#     )
#     #                            .(Fiscal.Year)
#     #                            , transform, p=Obligation.2014/sum(Obligation.2014))
#     # attach(mcc.competition.massive)
#     competition.combined<-plyr::join(competition.combined
#                                      ,ddply(subset(prodservsimple
#                                                    ,ServicesCategory.sum=="Services (Non-R&D)")
#                                             ,VAR.UnitOfAnalysis
#                                             ,summarise
#                                             ,pService=sum(percent)
#                                      )
#     )
#     competition.combined$pService[is.na(competition.combined$pService)]<-0
#     competition.combined<-plyr::join(competition.combined
#                                      ,ddply(subset(prodservsimple
#                                                    ,ServicesCategory.sum=="Products (All)")
#                                             ,VAR.UnitOfAnalysis
#                                             ,summarise
#                                             ,pProduct=sum(percent)
#                                      )
#     )
#     competition.combined$pProduct[is.na(competition.combined$pService)]<-0
#     competition.combined<-plyr::join(competition.combined
#                                      ,ddply(subset(prodservsimple
#                                                    ,ServicesCategory.sum=="R&D")
#                                             ,VAR.UnitOfAnalysis
#                                             ,summarise
#                                             ,pRnD=sum(percent)
#                                      )
#     )
#     competition.combined$pRnD[is.na(competition.combined$pRnD)]<-0
#     #   competition.combined<-plyr::join(competition.combined
#     #                              ,ddply(subset(prodservsimple
#     #                                            ,ProductsCategory.detail=="Fuels")
#     #                                     ,VAR.UnitOfAnalysis
#     #                                     ,summarise
#     #                                     ,pFuels=sum(percent)
#     #                              )
#     #   )
#     #   competition.combined$pFuels[is.na(competition.combined$pFuels)]<-0
#     
#     rm(prodservsimple)
#     
#     
#     comp.platformportfolio<-ddply(subset(competition.DF
#                                          ,PlatformPortfolio.sum!="Unlabeled"
#     )
#     , VAR.UnitOfAnalysis
#     , transform
#     , percent=Obligation.2014/sum(Obligation.2014)
#     )
#     #                            .(Fiscal.Year)
#     #                            , transform, p=Obligation.2014/sum(Obligation.2014))
#     # attach(mcc.competition.massive)
#     competition.combined<-plyr::join(competition.combined,ddply(subset(comp.platformportfolio
#                                                                        ,PlatformPortfolio.sum=="Aircraft and Drones"|
#                                                                            PlatformPortfolio.sum=="Unmanned")
#                                                                 ,VAR.UnitOfAnalysis
#                                                                 ,summarise
#                                                                 ,pAir=sum(percent)
#     )
#     )
#     competition.combined$pAir[is.na(competition.combined$pAir)]<-0
#     competition.combined<-plyr::join(competition.combined
#                                      ,ddply(subset(comp.platformportfolio
#                                                    ,PlatformPortfolio.sum=="Ships & Submarines")
#                                             ,VAR.UnitOfAnalysis
#                                             ,summarise
#                                             ,pVessel=sum(percent)
#                                      )
#     )
#     competition.combined$pVessel[is.na(competition.combined$pVessel)]<-0
#     competition.combined<-plyr::join(competition.combined
#                                      ,ddply(subset(comp.platformportfolio
#                                                    ,PlatformPortfolio.sum=="Land Vehicles")
#                                             ,VAR.UnitOfAnalysis
#                                             ,summarise
#                                             ,pLand=sum(percent)
#                                      )
#     )
#     competition.combined$pLand[is.na(competition.combined$pLand)]<-0
#     competition.combined<-plyr::join(competition.combined
#                                      ,ddply(subset(comp.platformportfolio
#                                                    ,PlatformPortfolio.sum=="Missile and Space Systems")
#                                             ,VAR.UnitOfAnalysis
#                                             ,summarise
#                                             ,pMnS=sum(percent)
#                                      )
#     )
#     competition.combined$pMnS[is.na(competition.combined$pMnS)]<-0
#     competition.combined<-plyr::join(competition.combined
#                                      ,ddply(subset(comp.platformportfolio
#                                                    ,PlatformPortfolio.sum=="Weapons and Ammunition")
#                                             ,VAR.UnitOfAnalysis
#                                             ,summarise
#                                             ,pWnA=sum(percent)
#                                      )
#     )
#     competition.combined$pWnA[is.na(competition.combined$pWnA)]<-0
#     competition.combined<-plyr::join(competition.combined
#                                      ,ddply(subset(comp.platformportfolio
#                                                    ,PlatformPortfolio.sum=="Facilities and Construction")
#                                             ,VAR.UnitOfAnalysis
#                                             ,summarise
#                                             ,pFnC=sum(percent)
#                                      )
#     )
#     competition.combined$pFnC[is.na(competition.combined$pFnC)]<-0
#     competition.combined<-plyr::join(competition.combined
#                                      ,ddply(subset(comp.platformportfolio
#                                                    ,PlatformPortfolio.sum=="Electronics and Communications")
#                                             ,VAR.UnitOfAnalysis
#                                             ,summarise
#                                             ,pEnC=sum(percent)
#                                      )
#     )
#     competition.combined$pEnC[is.na(competition.combined$pEnC)]<-0
#     
#     rm(comp.platformportfolio)
#     competition.combined
# }

wavg<-function(x,wt) x %*% wt/sum(wt)



competition.statistics.pt2<-function(VAR.Path
                                     ,competition.file.name
                                     ,VAR.UnitOfAnalysis
                                     
){
    
    
    competition.DF <-read.csv(
        competition.file.name,
        header=TRUE, sep=",", dec=".", strip.white=TRUE, 
        na.strings="NULL",
        stringsAsFactors=TRUE
    )
    
    competition.DF<-apply_lookups(VAR.Path,competition.DF)
    
    
    
    competition.combined<-ddply(subset(competition.DF
                                       ,!Competition.sum %in% c("Unlabeled","No Comp.")&
                                           NumberOfOffersReceived>0
    )
    , VAR.UnitOfAnalysis
    , summarise
    , wavOffers=wavg(NumberOfOffersReceived,abs(Obligation.2014))
    , avOffers=mean(NumberOfOffersReceived)
    , lwavOffers=wavg(log1p(NumberOfOffersReceived),abs(Obligation.2014))
    , lavOffers=mean(log1p(NumberOfOffersReceived))
    )
    
    
    
    pricing.DF<-ddply(subset(competition.DF
                             ,competition.DF$Pricing.Mechanism.hypothesis !="Unlabeled"
    )
    , VAR.UnitOfAnalysis
    , transform
    , percent=Obligation.2014/sum(Obligation.2014)
    )
    #                            .(Fiscal.Year)
    #                            , transform, p=Obligation.2014/sum(Obligation.2014))
    # attach(mcc.competition.massive)
    competition.combined<-plyr::join(competition.combined
                                     ,ddply(subset(pricing.DF
                                                   ,Pricing.Mechanism.hypothesis =="Fixed")
                                            ,VAR.UnitOfAnalysis
                                            ,summarise
                                            ,pFixed=sum(percent)
                                     )
    )
    competition.combined$pFixed[is.na(competition.combined$pFixed)]<-0
    competition.combined<-plyr::join(competition.combined
                                     ,ddply(subset(pricing.DF
                                                   ,Pricing.Mechanism.hypothesis =="Cost+")
                                            ,VAR.UnitOfAnalysis
                                            ,summarise
                                            ,pCostPlus=sum(percent)
                                     )
    )
    competition.combined$pCostPlus[is.na(competition.combined$pCostPlus)]<-0
    competition.combined<-plyr::join(competition.combined
                                     ,ddply(subset(pricing.DF
                                                   ,Pricing.Mechanism.Incentive =="TRUE")
                                            ,VAR.UnitOfAnalysis
                                            ,summarise
                                            ,pIncentFee=sum(percent)
                                     )
    )
    competition.combined$pIncentFee[is.na(competition.combined$pIncentFee)]<-0
    
    rm(pricing.DF)
    
    
    comp.vehicle<-ddply(subset(competition.DF
                               ,Vehicle.sum!="Unlabeled"
    )
    , VAR.UnitOfAnalysis
    , transform
    , percent=Obligation.2014/sum(Obligation.2014)
    )
    #                            .(Fiscal.Year)
    #                            , transform, p=Obligation.2014/sum(Obligation.2014))
    # attach(mcc.competition.massive)
    #   competition.combined<-plyr::join(competition.combined,ddply(subset(comp.vehicle
    #                                                                ,Vehicle.IDVorAward=="Aircraft and Drones")
    #                                                         ,VAR.UnitOfAnalysis
    #                                                         ,summarise
    #                                                         ,pAward=sum(percent)
    #   )
    #   )
    #   competition.combined$pAward[is.na(competition.combined$pAward)]<-0
    #   competition.combined<-plyr::join(competition.combined
    #                              ,ddply(subset(comp.vehicle
    #                                            ,PlatformPortfolio.sum=="Ships & Submarines")
    #                                     ,VAR.UnitOfAnalysis
    #                                     ,summarise
    #                                     ,pIDV=sum(percent)
    #                              )
    #   )
    #   competition.combined$pIDV[is.na(competition.combined$pIDV)]<-0
    competition.combined<-plyr::join(competition.combined
                                     ,ddply(subset(comp.vehicle
                                                   ,Vehicle.IDVorAward=="IDV")
                                            ,VAR.UnitOfAnalysis
                                            ,summarise
                                            ,pIDV=sum(percent)
                                     )
    )
    competition.combined$pIDV[is.na(competition.combined$pIDV)]<-0
    
    rm(comp.vehicle)
    competition.combined
}


competition.statistics.pt2a<-function(VAR.Path
                                      ,competition.file.name
                                      ,VAR.UnitOfAnalysis
                                      ,LimitFiscalYear=NULL
                                      
                                      
){
    
    
    competition.DF <-read.csv(
        competition.file.name,
        header=TRUE, sep=",", dec=".", strip.white=TRUE, 
        na.strings="NULL",
        stringsAsFactors=TRUE
    )
    
    if(!is.null(LimitFiscalYear)){
        competition.DF<-subset(competition.DF,fiscal_year==LimitFiscalYear)
    }
    
    
    
    
    
    competition.DF<-apply_lookups(VAR.Path,competition.DF)
    
    
    
    competition.combined<-ddply(subset(competition.DF
                                       ,!Competition.sum %in% c("Unlabeled","No Comp.")&
                                           NumberOfOffersReceived>0
    )
    , VAR.UnitOfAnalysis
    , summarise
    , wavOffers=wavg(NumberOfOffersReceived,abs(Obligation.2014))
    , avOffers=mean(NumberOfOffersReceived)
    , lwavOffers=wavg(log1p(NumberOfOffersReceived),abs(Obligation.2014))
    , lavOffers=mean(log1p(NumberOfOffersReceived)))
    #     , TotalCount=sum(CountOfPIID)
    #     , avSize=sum(Obligation.2014)/sum(CountOfPIID)
    #     , lavSize=(sum(log1p(abs(Obligation.2014)))/sum(CountOfPIID))
    #     )
    
    
    competition.combined<-plyr::join(competition.combined,ddply(competition.DF
                                                                , VAR.UnitOfAnalysis
                                                                ,summarise
                                                                , TotalValue=sum(Obligation.2014,na.rm=TRUE)
                                                                #     , TotalCount=sum(CountOfPIID)
                                                                #     , avSize=sum(Obligation.2014)/sum(CountOfPIID)
                                                                #     , lavSize=(sum(log1p(abs(Obligation.2014)))/sum(CountOfPIID))
    )
    )
    
    
    #     
    #     
    #     pricing.DF<-ddply(subset(competition.DF
    #                              ,competition.DF$Pricing.Mechanism.hypothesis !="Unlabeled"
    #     )
    #     , VAR.UnitOfAnalysis
    #     , transform
    #     , percent=Obligation.2014/sum(Obligation.2014)
    #     )
    #                            .(Fiscal.Year)
    #                            , transform, p=Obligation.2014/sum(Obligation.2014))
    # attach(mcc.competition.massive)
    #     competition.combined<-plyr::join(competition.combined
    #                                      ,ddply(subset(pricing.DF
    #                                                    ,Pricing.Mechanism.hypothesis =="Fixed")
    #                                             ,VAR.UnitOfAnalysis
    #                                             ,summarise
    #                                             ,pFixed=sum(percent)
    #                                      )
    #     )
    #     competition.combined$pFixed[is.na(competition.combined$pFixed)]<-0
    #     competition.combined<-plyr::join(competition.combined
    #                                      ,ddply(subset(pricing.DF
    #                                                    ,Pricing.Mechanism.hypothesis =="Cost+")
    #                                             ,VAR.UnitOfAnalysis
    #                                             ,summarise
    #                                             ,pCostPlus=sum(percent)
    #                                      )
    #     )
    #     competition.combined$pCostPlus[is.na(competition.combined$pCostPlus)]<-0
    #     competition.combined<-plyr::join(competition.combined
    #                                      ,ddply(subset(pricing.DF
    #                                                    ,Pricing.Mechanism.Incentive =="TRUE")
    #                                             ,VAR.UnitOfAnalysis
    #                                             ,summarise
    #                                             ,pIncentFee=sum(percent)
    #                                      )
    #     )
    #     competition.combined$pIncentFee[is.na(competition.combined$pIncentFee)]<-0
    #     
    #     rm(pricing.DF)
    #     
    
    comp.vehicle<-ddply(subset(competition.DF
                               ,Vehicle.sum!="Unlabeled"
    )
    , VAR.UnitOfAnalysis
    , transform
    , percent=Obligation.2014/sum(Obligation.2014)
    )
    #                            .(Fiscal.Year)
    #                            , transform, p=Obligation.2014/sum(Obligation.2014))
    # attach(mcc.competition.massive)
    #   competition.combined<-plyr::join(competition.combined,ddply(subset(comp.vehicle
    #                                                                ,Vehicle.IDVorAward=="Aircraft and Drones")
    #                                                         ,VAR.UnitOfAnalysis
    #                                                         ,summarise
    #                                                         ,pAward=sum(percent)
    #   )
    #   )
    #   competition.combined$pAward[is.na(competition.combined$pAward)]<-0
    #   competition.combined<-plyr::join(competition.combined
    #                              ,ddply(subset(comp.vehicle
    #                                            ,PlatformPortfolio.sum=="Ships & Submarines")
    #                                     ,VAR.UnitOfAnalysis
    #                                     ,summarise
    #                                     ,pIDV=sum(percent)
    #                              )
    #   )
    #   competition.combined$pIDV[is.na(competition.combined$pIDV)]<-0
    competition.combined<-plyr::join(competition.combined
                                     ,ddply(subset(comp.vehicle
                                                   ,Vehicle.IDVorAward=="IDV")
                                            ,VAR.UnitOfAnalysis
                                            ,summarise
                                            ,pIDV=sum(percent)
                                     )
    )
    competition.combined$pIDV[is.na(competition.combined$pIDV)]<-0
    
    rm(comp.vehicle)
    competition.combined
}


competition.statistics.pt3<-function(VAR.Path
                                     ,competition.file.name
                                     ,VAR.UnitOfAnalysis
                                     
){
    
    
    competition.DF <-read.csv(
        paste(VAR.Path,competition.file.name,sep="")
        ,header=TRUE
        , sep=","
        , dec="."
        , strip.white=TRUE, 
        na.strings="NULL",
        stringsAsFactors=TRUE
    )
    
    competition.DF<-apply_lookups(VAR.Path,competition.DF)
    competition.DF$Obligation.2014<-competition.DF$Obligation.2014*1000000000
    competition.combined<-ddply(competition.DF
                                , VAR.UnitOfAnalysis
                                , summarise
                                , TotalValue=sum(Obligation.2014)
                                , TotalCount=sum(CountOfPIID)
                                , avSize=sum(Obligation.2014)/sum(CountOfPIID)
                                , lavSize=(sum(log1p(abs(Obligation.2014)))/sum(CountOfPIID))
    )
    
    competition.combined<-plyr::join(competition.combined,
                                     ddply(ddply(competition.DF
                                                 , c(VAR.UnitOfAnalysis,"Fiscal.Year")
                                                 , summarise
                                                 , AnnualValue=sum(Obligation.2014)
                                     ),
                                     VAR.UnitOfAnalysis,
                                     summarise,
                                     MaxAnnualValue=max(AnnualValue)
                                     )
    )
    
    #   competition.combined<-ddply(competition.DF
    #                         , VAR.UnitOfAnalysis
    #   , transform
    #   , avSize=sum(Obligation.2014)/sum(CountOfPIID)
    #   , lavSize=log1p(sum(Obligation.2014)/sum(CountOfPIID))
    #   )
    
    
    #   
    #   pricing.DF<-ddply(subset(competition.DF
    #                            ,Pricing.Mechanism.hypothesis !="Unlabeled"
    #   )
    #   , VAR.UnitOfAnalysis
    #   , transform
    #   , percent=Obligation.2014/sum(Obligation.2014)
    #   )
    #   #                            .(Fiscal.Year)
    #   #                            , transform, p=Obligation.2014/sum(Obligation.2014))
    #   # attach(mcc.competition.massive)
    #   competition.combined<-plyr::join(competition.combined
    #                              ,ddply(subset(pricing.DF
    #                                            ,Pricing.Mechanism.hypothesis =="Fixed")
    #                                     ,VAR.UnitOfAnalysis
    #                                     ,summarise
    #                                     ,pFixed=sum(percent)
    #                              )
    #   )
    #   competition.combined$pFixed[is.na(competition.combined$pFixed)]<-0
    #   competition.combined<-plyr::join(competition.combined
    #                              ,ddply(subset(pricing.DF
    #                                            ,Pricing.Mechanism.hypothesis =="Cost+")
    #                                     ,VAR.UnitOfAnalysis
    #                                     ,summarise
    #                                     ,pCostPlus=sum(percent)
    #                              )
    #   )
    #   competition.combined$pCostPlus[is.na(competition.combined$pCostPlus)]<-0
    #   competition.combined<-plyr::join(competition.combined
    #                              ,ddply(subset(pricing.DF
    #                                            ,Pricing.Mechanism.Incentive =="TRUE")
    #                                     ,VAR.UnitOfAnalysis
    #                                     ,summarise
    #                                     ,pIncentFee=sum(percent)
    #                              )
    #   )
    #   competition.combined$pIncentFee[is.na(competition.combined$pIncentFee)]<-0
    #   
    #   rm(pricing.DF)
    #   
    #   
    #   comp.vehicle<-ddply(subset(competition.DF
    #                              ,Vehicle.sum!="Unlabeled"
    #   )
    #   , VAR.UnitOfAnalysis
    #   , transform
    #   , percent=Obligation.2014/sum(Obligation.2014)
    #   )
    #   #                            .(Fiscal.Year)
    #   #                            , transform, p=Obligation.2014/sum(Obligation.2014))
    #   # attach(mcc.competition.massive)
    #   #   competition.combined<-plyr::join(competition.combined,ddply(subset(comp.vehicle
    #   #                                                                ,Vehicle.IDVorAward=="Aircraft and Drones")
    #   #                                                         ,VAR.UnitOfAnalysis
    #   #                                                         ,summarise
    #   #                                                         ,pAward=sum(percent)
    #   #   )
    #   #   )
    #   #   competition.combined$pAward[is.na(competition.combined$pAward)]<-0
    #   #   competition.combined<-plyr::join(competition.combined
    #   #                              ,ddply(subset(comp.vehicle
    #   #                                            ,PlatformPortfolio.sum=="Ships & Submarines")
    #   #                                     ,VAR.UnitOfAnalysis
    #   #                                     ,summarise
    #   #                                     ,pIDV=sum(percent)
    #   #                              )
    #   #   )
    #   #   competition.combined$pIDV[is.na(competition.combined$pIDV)]<-0
    #   competition.combined<-plyr::join(competition.combined
    #                              ,ddply(subset(comp.vehicle
    #                                            ,Vehicle.IDVorAward=="IDV")
    #                                     ,VAR.UnitOfAnalysis
    #                                     ,summarise
    #                                     ,pIDV=sum(percent)
    #                              )
    #   )
    #   competition.combined$pIDV[is.na(competition.combined$pIDV)]<-0
    #   
    #   rm(comp.vehicle)
    competition.combined
}




fixed.price.statistics<-function(Path
                                 ,fixed.price.DF
                                 ,VAR.UnitOfAnalysis
                                 
){
    
    #TO DO
    
    #   
    #   fixed.price.DF$IsFixedPriceCode<-fixed.price.DF$IsFixedPrice
    fixed.price.DF<-apply_lookups(VAR.Path,fixed.price.DF)
    #   colnames(fixed.price.DF)[colnames(fixed.price.DF)==VAR.UnitOfAnalysis]<-"UnitOfAnalysis"
    fixed.price.combined<-ddply(fixed.price.DF
                                , VAR.UnitOfAnalysis
                                , summarise
                                #                               , TotalValue=sum(Obligation.2014)
                                , TotalCount=length(CSIScontractID)
                                #                               , avSize=sum(Obligation.2014)/sum(CountOfPIID)
                                #                               , lavSize=log1p(sum(Obligation.2014)/sum(CountOfPIID))
    )
    
    #*****pIsFixedPrice   
    IsFixedPrice.DF<-ddply(subset(fixed.price.DF
                                  ,!is.na(IsFixedPrice)
    )
    , VAR.UnitOfAnalysis
    , transform
    , percent=1/length(CSIScontractID)
    )
    #   #                            .(Fiscal.Year)
    #   #                            , transform, p=Obligation.2014/sum(Obligation.2014))
    #   # attach(mcc.competition.massive)
    fixed.price.combined<-plyr::join(fixed.price.combined
                                     ,ddply(subset(IsFixedPrice.DF
                                                   ,IsFixedPrice =="Fixed Price")
                                            ,VAR.UnitOfAnalysis
                                            ,summarise
                                            ,pFixedPrice=sum(percent)
                                     )
    )
    fixed.price.combined$pFixedPrice[is.na(fixed.price.combined$pFixedPrice)]<-0
    rm(IsFixedPrice.DF)
    
    
    #*****pTerminations   
    terminations.DF<-ddply(subset(fixed.price.DF
                                  ,!is.na(IsTerminated)
    )
    , VAR.UnitOfAnalysis
    , transform
    , percent=1/length(CSIScontractID)
    )
    #   #                            .(Fiscal.Year)
    #   #                            , transform, p=Obligation.2014/sum(Obligation.2014))
    #   # attach(mcc.competition.massive)
    fixed.price.combined<-plyr::join(fixed.price.combined
                                     ,ddply(subset(terminations.DF
                                                   ,IsTerminated =="Terminated")
                                            ,VAR.UnitOfAnalysis
                                            ,summarise
                                            ,pTerminated=sum(percent)
                                     )
    )
    fixed.price.combined$pTerminated[is.na(fixed.price.combined$pTerminated)]<-0
    rm(terminations.DF)
    
    #   pMaxOfisChangeOrder
    Changed.DF<-ddply(subset(fixed.price.DF
                             ,!is.na(MaxOfisChangeOrder)
    )
    , VAR.UnitOfAnalysis
    , transform
    , percent=1/length(CSIScontractID)
    )
    #   #                            .(Fiscal.Year)
    #   #                            , transform, p=Obligation.2014/sum(Obligation.2014))
    #   # attach(mcc.competition.massive)
    fixed.price.combined<-plyr::join(fixed.price.combined
                                     ,ddply(subset(Changed.DF
                                                   ,MaxOfisChangeOrder =="Change Order(s)")
                                            ,VAR.UnitOfAnalysis
                                            ,summarise
                                            ,pChanged=sum(percent)
                                            ,avNumberOfChangeOrders=mean(SumOfisChangeOrder,na.rm=TRUE)
                                            ,avChangeOrderGrowth=mean(pChangeOrderVsUnmodifiedBaseAndAll,na.rm=TRUE)
                                     )
    )
    
    
    
    fixed.price.combined$pChanged[is.na(fixed.price.combined$pChanged)]<-0
    rm(Changed.DF)
    
    
    #   pSomeCompetition
    labeled.competition.DF<-ddply(subset(fixed.price.DF
                                         ,IsSomeCompetition!="Unlabeled"
    )
    , VAR.UnitOfAnalysis
    , transform
    , percent=1/length(CSIScontractID)
    )
    
    
    fixed.price.combined<-plyr::join(fixed.price.combined
                                     ,ddply(subset(labeled.competition.DF
                                                   ,IsSomeCompetition=="Comp.")
                                            ,VAR.UnitOfAnalysis
                                            ,summarise
                                            ,pSomeCompetition=sum(percent)
                                     )
    )
    
    fixed.price.combined$pSomeCompetition[is.na(fixed.price.combined$pSomeCompetition)]<-0
    
    #pSingleOffer , avOffers
    labeled.competition.DF<-ddply(subset(fixed.price.DF
                                         ,IsSomeCompetition!="Unlabeled"&
                                             !is.na(numberofoffersreceived)&
                                             numberofoffersreceived>0
    )
    , VAR.UnitOfAnalysis
    , transform
    , percent=1/length(CSIScontractID)
    
    )
    
    fixed.price.combined<-plyr::join(fixed.price.combined
                                     ,ddply(labeled.competition.DF
                                            ,VAR.UnitOfAnalysis
                                            ,summarise
                                            , avOffers=mean(numberofoffersreceived,na.rm=TRUE)
                                     )
    )
    
    
    fixed.price.combined<-plyr::join(fixed.price.combined
                                     ,ddply(subset(labeled.competition.DF
                                                   ,numberofoffersreceived==1)
                                            ,VAR.UnitOfAnalysis
                                            ,summarise
                                            ,pSingleOffer=sum(percent)
                                     )
    )
    
    
    fixed.price.combined$pSingleOffer[is.na(fixed.price.combined$pSingleOffer)]<-0
    
    
    rm(labeled.competition.DF)
    
    
    #   fixed.price.combined<-plyr::join(fixed.price.combined
    #                              ,ddply(subset(pricing.DF
    #                                            ,IsTerminated =="Cost+")
    #                                     ,VAR.UnitOfAnalysis
    #                                     ,summarise
    #                                     ,pCostPlus=sum(percent)
    #                              )
    #   )
    #   fixed.price.combined$pCostPlus[is.na(fixed.price.combined$pCostPlus)]<-0
    #   fixed.price.combined<-plyr::join(fixed.price.combined
    #                              ,ddply(subset(pricing.DF
    #                                            ,Pricing.Mechanism.Incentive =="TRUE")
    #                                     ,VAR.UnitOfAnalysis
    #                                     ,summarise
    #                                     ,pIncentFee=sum(percent)
    #                              )
    #   )
    #   fixed.price.combined$pIncentFee[is.na(fixed.price.combined$pIncentFee)]<-0
    #   
    #   rm(pricing.DF)
    #   
    #   
    #   comp.vehicle<-ddply(subset(fixed.price.DF
    #                              ,Vehicle.sum!="Unlabeled"
    #   )
    #   , VAR.UnitOfAnalysis
    #   , transform
    #   , percent=Obligation.2014/sum(Obligation.2014)
    #   )
    #   #                            .(Fiscal.Year)
    #   #                            , transform, p=Obligation.2014/sum(Obligation.2014))
    #   # attach(mcc.competition.massive)
    #   #   fixed.price.combined<-plyr::join(fixed.price.combined,ddply(subset(comp.vehicle
    #   #                                                                ,Vehicle.IDVorAward=="Aircraft and Drones")
    #   #                                                         ,VAR.UnitOfAnalysis
    #   #                                                         ,summarise
    #   #                                                         ,pAward=sum(percent)
    #   #   )
    #   #   )
    #   #   fixed.price.combined$pAward[is.na(fixed.price.combined$pAward)]<-0
    #   #   fixed.price.combined<-plyr::join(fixed.price.combined
    #   #                              ,ddply(subset(comp.vehicle
    #   #                                            ,PlatformPortfolio.sum=="Ships & Submarines")
    #   #                                     ,VAR.UnitOfAnalysis
    #   #                                     ,summarise
    #   #                                     ,pIDV=sum(percent)
    #   #                              )
    #   #   )
    #   #   fixed.price.combined$pIDV[is.na(fixed.price.combined$pIDV)]<-0
    #   fixed.price.combined<-plyr::join(fixed.price.combined
    #                              ,ddply(subset(comp.vehicle
    #                                            ,Vehicle.IDVorAward=="IDV")
    #                                     ,VAR.UnitOfAnalysis
    #                                     ,summarise
    #                                     ,pIDV=sum(percent)
    #                              )
    #   )
    #   fixed.price.combined$pIDV[is.na(fixed.price.combined$pIDV)]<-0
    #   
    #   rm(comp.vehicle)
    # colnames(fixed.price.combined)[colnames(fixed.price.combined)=="UnitOfAnalysis"]<-VAR.UnitOfAnalysis
    # colnames(fixed.price.combined)[colnames(fixed.price.combined)=="IsFixedPriceCode"]<-"IsFixedPrice"
    
    fixed.price.combined
}