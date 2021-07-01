GRTSdataprep2011<-function(data.dir,frame_file.name){
  #======================
  #create final data folder
  #=====================
  if(write==T){
    subDir<-paste("GRTSdata","_",Sys.Date(),sep="")
    if (!file.exists(subDir)){
      dir.create(file.path(home.dir,data.dir, subDir))
    }
  }
  #2011 data
  GRTS_Population<-c("Grays_Chinook",  "Elochoman_Skamokawa",	"Lower Cowlitz",	"Delameter_Above Weir", "Lacamas_Above Weir", "Olequa_Above Weir", "Ostrander_Above Weir","Coweeman",	"NF_MS Toutle",	"Green",	"SF Toutle",	"Kalama",	"Lower Lewis",	"Cedar",	"EF Lewis",	"Washougal","Lower Gorge","Salmon Creek","Duncan_Above Weir")
  sites=c(15,20,13,2,0,4,1,18,6,17,12,3,5,24,10,5,8,8,0)#edited 5.29.2018LB
  Y2=c(114,9,0,0,0,57,0,0,0,0,0,0,0,0,0,3,14,0,21)#edited 5.29.2018LB
  Y=c(157,77,13,6,0,57,0,297,4,16,25,7,58,85,41,17,40,18,0) #edited 5.29.2018LB
  #J=c(118,39,13,97,6,5,10,4,26,12,8,8,24,14,1)#max redds plus 1
  GRTS_miles=c(14.65,18.94,12.17,2,0,4,1,16.25,5.8,14.78,10.92,3.11,5.13,21.62,10.23,4.55,6.71,8,0) #edited 5.29.2018LB
  I_miles=c(6.35,10.5,0,0,0,3.7,0,0,0,0,0,0,0,0,4.5,5.67,3.06,0,1.62)#Checked 5.29.2018LB
  #mis_miles=c(147,70,397,44,42,37,48,30,41,19,73,44,26,167) #edited 12.19.2013
  I.Fm=c(0,0,0,0,0,12,0,0,0,0,0,0,0,0,0,0,3,0,5) #total female carcs in all index reaches in each pop
  I.H=c(417,1,0,0,0,0,0,0,0,0,0,0,0,0,4,1,0,0,0)  #total Hatchery carcs in all index reaches in each pop
  I.MS=c(455,8,0,0,0,29,0,0,0,0,0,0,0,0,5,1,10,0,7) #total Hatchery + Wild carcs in all index reaches in each pop
  I.AS=c(0,0,0,0,0,29,0,0,0,0,0,0,0,0,0,0,7,0,7) #total Adult carcs in all index reaches in each pop (often is same as I.MS)
  Fm=c(5,1,1,1,0,1,0,33,1,2,2,0,20,14,10,5,5,2,0)#edited 5.29.2018LB
  AS=c(14,8,2,3,0,4,0,64,2,4,7,0,24,34,15,11,6,3,0) #needed non zero kalama #edited 5.29.2018LB
  H=c(90,12,0,0,0,0,0,3,1,0,1,0,6,0,0,0,0,0,0) #needed non zero kalama #edited 5.29.2018LB
  MS=c(92,21,3,3,0,4,0,69,2,3,7,0,27,36,15,11,6,3,0) #needed non zero kalama #edited 5.29.2018LB
  OC=c(5,13,9,2,0,3,0,17,1,10,6,2,4,15,9,3,5,4,0) #edited 5.29.2018LB
  y=t(structure(.Data=c(#redd counts by GRTS reach #columns is 24after transposing #19 pops.
    0,0,0,0,0,15,0,0,0,0,0,5,8,0,0,0,0,117,17,NA,NA,NA,NA,NA,
    38,3,10,0,4,0,1,2,0,6,0,1,4,1,0,1,5,0,0,1,0,NA,NA,NA,
    0,2,0,0,0,1,2,1,4,1,1,2,1,0,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
    3,3,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
    NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
    0,3,12,1,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
    0,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
    29,19,0,27,20,19,0,2,4,1,1,9,16,11,12,14,21,13,96,NA,NA,NA,NA,NA,
    0,0,0,0,0,0,5,0,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
    0,0,1,4,4,1,1,0,1,0,0,1,0,1,2,0,0,1,NA,NA,NA,NA,NA,NA,
    0,9,1,0,0,0,0,3,5,0,2,5,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
    0,3,0,4,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
    0,0,6,12,25,15,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
    1,3,7,1,2,2,1,2,3,5,0,0,1,0,2,5,0,4,1,1,1,0,9,0,
    9,1,5,0,4,3,4,7,4,4,0,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
    5,5,0,7,0,0,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
    0,6,5,7,23,1,0,0,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
    0,0,1,0,0,2,2,13,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
    NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA
  ),.Dim=c(24,length(GRTS_Population))))
  y<-cbind(y,matrix(NA,length(GRTS_Population),50-ncol(y)))
  colnames(y)<-paste("y",c(1:ncol(y)),sep=".")
  g=t(structure(.Data=c(#reach length of GRTS reaches #columns is 24after transposing #19 pops.
    0.42,0.71,1,1,1,1,1,1,1,1,1,0.34,1,1,1,1,1,NA,NA,NA,NA,NA,NA,NA,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,0.65,0.8,1,0.47,1,1,1,NA,NA,NA,
    0.83,1,1,0.42,0.98,1,1,1,1,1,1,1,1,1,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
    1,1,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
    NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
    1,1,1,1,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
    1,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
    1,1,0.46,1,1,1,1,1,1,1,1,1,1,1.01,1,1,1,0.62,1,NA,NA,NA,NA,NA,
    0.77,1,1,1,1,0.25,1,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA, 
    1,1,1,1,1,1,1,1,1,1,1,1,0.31,1,1,1,0.38,1,NA,NA,NA,NA,NA,NA,
    1,1,1,1,1,1,1,1,1,1,1,1,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
    0.21,0.9,1,1,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
    1,1,1,0.64,1,0.49,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
    1,1,1,1,1,1,1,1,1,1,0.99,1,1,1,1,1,1,1,1.65,0.85,1,1,1,1,
    1,0.23,1,1,1,1,1,1,1,1,1,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
    1,1,1,1,1,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
    1,1,1,1,1,1,1,1,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
    1,1,1,1,1,1,1,1,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
    NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA
  ),.Dim=c(24,length(GRTS_Population))))
  g<-cbind(g,matrix(NA,length(GRTS_Population),50-ncol(g)))
  colnames(g)<-paste("g",c(1:ncol(g)),sep=".")
  dat<-data.frame(cbind(GRTS_Population,Fm,AS,H,MS,sites,I_miles,GRTS_miles,Y,OC,Y2,y,g))
  GRTSpops<-data.frame(read.csv(file.path(data.dir,frame_file.name)))
  GRTSdat<-merge(GRTSpops[,colnames(GRTSpops)%in%c("GRTS_Population","Order","F_miles")],dat,all=T)
  GRTSdat[,!colnames(GRTSdat)%in%c(colnames(y),colnames(g))] [is.na(GRTSdat[,!colnames(GRTSdat)%in%c(colnames(y),colnames(g))])]<-0
  GRTSdat$mis_miles<-as.numeric(as.character(GRTSdat$F_miles))-as.numeric(as.character(GRTSdat$I_miles))-as.numeric(as.character(GRTSdat$GRTS_miles))
  GRTSdat$I.Fm<-rep(0,dim(GRTSdat)[1])
  GRTSdat$I.H<-rep(0,dim(GRTSdat)[1])
  GRTSdat$I.MS<-rep(0,dim(GRTSdat)[1])
  GRTSdat$I.AS<-rep(0,dim(GRTSdat)[1])
  GRTSdat$Year<-rep(2011,dim(GRTSdat)[1])
  GRTSdat<-GRTSdat[order(GRTSdat$Order),]
  #write.csv(GRTSdat,paste("2011GRTSdat_",Sys.Date(),".csv",sep=""),row.names=FALSE)
  outfile<-file.path(data.dir,subDir,paste0(2011,"GRTSdat_",Sys.Date(),".csv"))
  write.csv(GRTSdat,outfile,row.names = FALSE)
}
