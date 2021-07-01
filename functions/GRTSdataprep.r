#=======================
#GRTS data prep function
#=======================
GRTSdataprep<-function(GRTSpops,data.dir,db.dir,accdb.name,years, write){
  GRTSdat_list<-list(NULL)
  length(GRTSdat_list)<-length(years)
  names(GRTSdat_list)<-years
  for(t in 1:length(years)){
    yr=years[t]
    #======================
    #create final data folder
    #=====================
    if(write==T){
      subDir<-paste("GRTSdata","_",Sys.Date(),sep="")
      if (!file.exists(subDir)){
        dir.create(file.path(home.dir,data.dir, subDir))
      }
    }
    #=================================================================
    GRTSdat<-GRTSpops[,colnames(GRTSpops)%in%c("GRTS_Population","Order","F_miles")]
    #####################################################
    #sites,pops
    #####################
    dat<-data.frame(odbc(db.dir=db.dir,accdb.name = accdb.name, dsn = "MS Access Database", sqtable = "qry_GRTS_Location", fields ="*"))
    dat<-dat[dat$GRTS_Population%in%GRTSdat$GRTS_Population & dat$Return_Yr==yr,]
    dat<-merge(x=dat,y=GRTSdat,by="GRTS_Population",all.y = TRUE)
    dat<-dat[order(dat$Order),]
    #site 
    sites<-NULL
    sites<-as.data.frame(dat%>%filter(!is.na(HEADER_STREAM_REACH_Code))
                            %>%count(GRTS_Population))
    colnames(sites)[2]<-"sites"
    GRTSdat<-merge(GRTSdat,sites,all.x=TRUE)
    GRTSdat$sites[is.na(GRTSdat$sites)]<-0
    #pops
    pops<-NULL
    pops<-length(unique(GRTSdat$GRTS_Population))
    ######################################################
    #F,AS,H,MS #mark sample is currently clip sample only!
    #########################################################
    dat<- odbc(db.dir=db.dir,accdb.name = accdb.name, dsn = "MS Access Database", sqtable = "qry_GRTS_Coho_FishCounts", fields ="*")    
    dat<-dat[dat$GRTS_Population%in%GRTSdat$GRTS_Population & dat$Return_Yr==yr,]
    dat<-merge(x=dat,y=GRTSdat,by.x="GRTS_Population",by.y="GRTS_Population",all.y=T,all.x=T)
    dat<-dat[order(dat$Order),]
    #F (females)
    Carcs<-NULL
    Carcs<-as.data.frame(dat%>%group_by(GRTS_Population)%>%summarise(
      Fm=sum(Females),
      AS=sum(Adults),
      H=sum(HOS),
      MS=sum(MS_ClipSampled)))
    Carcs[is.na(Carcs)]<-0
    GRTSdat<-merge(Carcs,GRTSdat,"GRTS_Population")
    ######################################################
    #INDEX F,AS,H,MS #mark sample is currently clip sample only!
    #########################################################
    dat<- odbc(db.dir=db.dir,accdb.name = accdb.name, dsn = "MS Access Database", sqtable = "qry_INDEX_Coho_FishCounts2", fields ="*")    
    dat<-dat[dat$GRTS_Population%in%GRTSdat$GRTS_Population & dat$Return_Yr==yr,]
    dat<-merge(x=dat,y=GRTSdat,by.x="GRTS_Population",by.y="GRTS_Population",all.y=T,all.x=T)
    dat<-dat[order(dat$Order),]
    #F (females)
    I.Carcs<-NULL
    I.Carcs<-as.data.frame(dat%>%group_by(GRTS_Population)%>%summarise(
      I.Fm=sum(Females),
      I.AS=sum(Adults),
      I.H=sum(HOS),
      I.MS=sum(MS_ClipSampled)))
    I.Carcs[is.na(I.Carcs)]<-0
    GRTSdat<-merge(I.Carcs,GRTSdat,"GRTS_Population")
    ########################
    #I_miles
    #########################
    dat<- odbc(db.dir=db.dir,accdb.name = accdb.name ,dsn = "MS Access Database", sqtable = "qry_INDEX_Location", fields ="*") 
    dat<-dat[dat$GRTS_Population%in%GRTSdat$GRTS_Population & dat$Return_Yr==yr,]
    dat<-merge(x=dat,y=GRTSdat,by.x="GRTS_Population",by.y="GRTS_Population",all.y=T,all.x=T)
    dat<-dat[order(dat$Order),]
    dat$Upper_River_Mile_Meas[is.na(dat$Upper_River_Mile_Meas)]<-0
    dat$Lower_River_Mile_Meas[is.na(dat$Lower_River_Mile_Meas)]<-0
    dat$Reach_Length<-dat$Upper_River_Mile_Meas-dat$Lower_River_Mile_Meas
    #I_miles
    I_miles<-NULL
    I_miles<-as.data.frame(dat%>%group_by(GRTS_Population)%>%summarise(I_miles=sum(Reach_Length)))
    GRTSdat<-merge(GRTSdat,I_miles)
    ########################
    #GRTS_miles (TOTAL)
    #######################
    dat<- odbc(db.dir=db.dir,accdb.name = accdb.name, dsn = "MS Access Database", sqtable = "qry_GRTS_Location", fields ="*")    
    dat<-dat[dat$GRTS_Population%in%GRTSdat$GRTS_Population & dat$Return_Yr==yr,]
    dat<-merge(x=dat,y=GRTSdat,by.x="GRTS_Population",by.y="GRTS_Population",all.y=T,all.x=T)
    dat<-dat[order(dat$Order),]
    dat$Upper_River_Mile_Meas[is.na(dat$Upper_River_Mile_Meas)]<-0
    dat$Lower_River_Mile_Meas[is.na(dat$Lower_River_Mile_Meas)]<-0
    dat$Reach_Length<-dat$Upper_River_Mile_Meas-dat$Lower_River_Mile_Meas
    #GRTS_miles (miles in Indexes)
    GRTS_miles<-NULL
    GRTS_miles<-as.data.frame(dat%>%group_by(GRTS_Population)%>%summarise(GRTS_miles=sum(Reach_Length)))
    GRTSdat<-merge(GRTSdat,GRTS_miles)
    #g matrix of new redds by GRTS reach
    g<-NULL
    #y<-data.frame(matrix(NA,pops,max(GRTSdat$sites[!is.na(GRTSdat$sites)])))
    g<-data.frame(matrix(NA,pops,50))
    colnames(g)<-paste("g",1:50,sep=".")
    #olnames(g)<-paste("g",1:max(GRTSdat$sites[!is.na(GRTSdat$sites)]),sep=".")
    for (i in 1:length(unique(GRTSdat$Order))){
      tdat<-NULL
      tdat<-dat[which(dat$Order==i),]
      if(nrow(tdat[!is.na(tdat$HEADER_STREAM_REACH_Code),])>0){
        tdat$Reach_Num<-as.numeric(as.factor(tdat$HEADER_STREAM_REACH_Code))
        tdat<-tdat[order(tdat$Reach_Num),]
        for(j in 1:length(unique(tdat$Reach_Num))){
          ttdat<-NULL
          ttdat<-tdat[which(tdat$Reach_Num==unique(tdat$Reach_Num)[j]),]
          g[i,j]<-ttdat$Reach_Length
        }
      }
    } 
    ########################
    #Y,y,J,OC
    #########################
    dat<- odbc(db.dir=db.dir,accdb.name = accdb.name, dsn = "MS Access Database", sqtable = "qry_Coho_GRTS_NewRedds", fields ="*")   
    dat<-dat[dat$GRTS_Population%in%GRTSdat$GRTS_Population & dat$Return_Yr==yr,]
    dat<-merge(x=dat,y=GRTSdat,by.x="GRTS_Population",by.y="GRTS_Population",all.y=T,all.x=T)
    dat<-dat[order(dat$Order),]
    dat$NewRedds[is.na(dat$NewRedds)]<-0
    #Y (sum of new redds in GRTS reaches for each pop)
    Y<-as.data.frame(dat%>%group_by(GRTS_Population)%>%summarise(Y=sum(NewRedds)))
    GRTSdat<-merge(GRTSdat,Y)
    GRTSdat<-GRTSdat[order(as.numeric(as.character(GRTSdat$Order))),]
    #y matrix of new redds by GRTS reach
    y<-NULL
    #y<-data.frame(matrix(NA,pops,max(GRTSdat$sites[!is.na(GRTSdat$sites)])))
    y<-data.frame(matrix(NA,pops,50))
    colnames(y)<-paste("y",1:50,sep=".")
    #olnames(y)<-paste("y",1:max(GRTSdat$sites[!is.na(GRTSdat$sites)]),sep=".")
    for (i in 1:length(unique(GRTSdat$Order))){
      tdat<-dat[which(dat$Order==i),]
      if(nrow(tdat[!is.na(tdat$HEADER_STREAM_REACH_Code),])>0){
        tdat$Reach_Num<-as.numeric(as.factor(tdat$HEADER_STREAM_REACH_Code))
        tdat<-tdat[order(tdat$Reach_Num),]
        for(j in 1:length(unique(tdat$Reach_Num))){
          ttdat<-tdat[which(tdat$Reach_Num==unique(tdat$Reach_Num)[j]),]
          y[i,j]<-sum(ttdat$NewRedds)
        }
      }
    } 
    # #J (maximum season total redd count in a GRTS reach +1)
    # J<-y
    # J[is.na(J)]<-0
    # J<-apply(J,1,max)+1
    # GRTSdat<-as.data.frame(cbind(GRTSdat,J))
    #OC (NUMBER OF NON-ZERO new redd count reaches per pop)
    dat<-dat[dat$NewRedds>0,]
    OC<-as.data.frame(dat%>%group_by(GRTS_Population,HEADER_STREAM_REACH_Code)%>%summarize(Y=sum(NewRedds))%>%count(GRTS_Population))
    colnames(OC)[2]<-"OC"
    GRTSdat<-merge(GRTSdat,OC,all.x=TRUE)
    GRTSdat$OC[is.na(GRTSdat$OC)]<-0
    ########################
    #Y2
    #########################
    dat<- odbc(db.dir=db.dir,accdb.name = accdb.name, dsn = "MS Access Database", sqtable = "qry_Coho_INDEX_NewRedds", fields ="*")    
    #dat$GRTS_Population[dat$GRTS_Population=="SalmonCreek"]<-"Salmon Creek"
    dat<-dat[dat$GRTS_Population%in%GRTSdat$GRTS_Population & dat$Return_Yr==yr,]
    dat<-merge(x=dat,y=GRTSdat,by.x="GRTS_Population",by.y="GRTS_Population",all.y=T,all.x=T)
    dat<-dat[order(dat$Order),]
    dat$NewRedds[is.na(dat$NewRedds)]<-0
    #Y2 (sum of new redds in NON GRTS index reaches for each pop)
    Y2<-as.data.frame(dat%>%group_by(GRTS_Population)%>%summarise(Y2=sum(NewRedds)))
    GRTSdat<-merge(GRTSdat,Y2,all.x=TRUE)
    GRTSdat<-GRTSdat[order(as.numeric(as.character(GRTSdat$Order))),]
    ###################
    #mis miles (unsurveyed miles in frame to expand for)
    ###################
    GRTSdat$mis_miles<-pmax(GRTSdat$F_miles-GRTSdat$I_miles-GRTSdat$GRTS_miles,0)
    #Year
    GRTSdat$Year<-yr
    #merge in y and g data
    y$GRTS_Population<-GRTSdat$GRTS_Population
    g$GRTS_Population<-GRTSdat$GRTS_Population
    GRTSdat<-merge(GRTSdat,y)
    GRTSdat<-merge(GRTSdat,g)
    GRTSdat<-GRTSdat[order(GRTSdat$Order),]
    #print out
    outfile<-file.path(data.dir,subDir,paste0(yr,"GRTSdat_",Sys.Date(),".csv"))
    print(outfile)
    if(write==T){write.csv(GRTSdat,outfile,row.names = FALSE)}
    GRTSdat_list[[t]]<-GRTSdat
  }
  return(GRTSdat_list)
}
