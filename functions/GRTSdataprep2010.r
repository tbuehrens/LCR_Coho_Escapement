GRTSdataprep2010<-function(data.dir,frame_file.name,WriteGRTSdata){
  #======================
  #create final data folder
  #=====================
  if(WriteGRTSdata==T){
    subDir<-paste("GRTSdata","_",Sys.Date(),sep="")
    if (!file.exists(subDir)){
      dir.create(file.path(home.dir,data.dir, subDir))
    }
  }
  #2010 GRTS dat
  GRTS_Population<-c("Grays_Chinook",  "Elochoman_Skamokawa",	"Lower Cowlitz",	"Delameter_Above Weir", "Lacamas_Above Weir", "Olequa_Above Weir", "Ostrander_Above Weir", "Coweeman",	"NF_MS Toutle",	"Green",	"SF Toutle",	"Kalama",	"Lower Lewis",	"Cedar",	"EF Lewis",	"Washougal","Lower Gorge","Duncan_Above Weir")
  sites=c(19, 19, 14, 1, 0, 4, 3, 20, 5, 22, 11, 8, 2, 24, 11, 7, 7, 0)#1 mile reaches surveyed for redds per population 
  Y2=c(68,30,0, 0, 0, 44,0,0,0,0,0,0,0,0, 2,4,56,17)#redds in index Chinook and Chum surveys
  Y=c(25,99, 55, 1, 0, 11, 12, 227, 2, 260, 82, 17, 32, 76, 48, 38, 10,0)# obs redds from GRTS surveys for each pop 
  #mis_miles=c(148,80, 405, 417, 418, 414, 415, 43, 45, 35, 49, 27, 26, 13, 74, 43, 25, 0) #unsampled miles in each pop updated 05.14.2018
  GRTS_miles=c(17.31,16.69,13.57,1, 0, 4, 2.91, 20,4.48,22,10.85,7.21,2,23.17,9.55,7,7,0)#updated 05.14.2018
  I_miles=c(7.47,9.6,0,0,0,3.7,0,0, 0, 0, 0, 0,0,0,4.5,6.7,2.99,1.62)#updated 05.24.2018.
  I.Fm=c(6,0,0,0,0,3,0,0,0,0,0,0,0,0,6,4,15,3) #total female carcs in all index reaches in each pop
  I.H=c(243,7,0,0,0,0,0,0,0,0,0,0,0,0,39,5,12,0)  #total Hatchery carcs in all index reaches in each pop
  I.MS=c(255,8,0,0,0,9,0,0,0,0,0,0,0,0,53,12,40,10) #total Hatchery + Wild carcs in all index reaches in each pop
  I.AS=c(13,1,0,0,0,3,0,0,0,0,0,0,0,0,19,8,32,10) #total Adult carcs in all index reaches in each pop (often is same as I.MS)
  Fm=c(23,12,9, 0, 0, 6, 1, 32, 0, 116, 41, 4, 1, 24, 9, 6, 22,0)# number of females carcasess in each pop
  AS=c(51,35, 12, 0, 0, 13, 1, 73,0, 230, 100, 9, 7, 62, 17, 21, 38,0)#number adults carcsses in each pop
  H=c(39,28,1, 0, 0, 2,1, 7,0,163,21, 52, 0, 0, 5, 11, 11,0)#number mass marked carcasses in each pop
  MS=c(48,38,13, 0, 0, 14, 2, 75,0,244, 102, 52, 7, 63, 16, 25, 38,0) #number known origin carcasses sampled in each pop
  OC=c(8, 14,8, 1, 0, 4, 2, 18, 1, 21, 5, 2, 2, 19, 7, 4, 1,0)# reaches that had at least one redd 
  y=t(structure(.Data=c(#counts per reach for the 18 populations
    0,0,0,0,0,5,1,1,0,0,4,0,0,2,0,0,2,4,6,NA,NA,NA,NA,NA,
    12,3,37,2,2,5,3,0,11,7,11,9,0,0,0,0,1,1,2,NA,NA,NA,NA,NA,
    5,2,6,16,0,0,20,0,0,4,0,0,1,1,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
    1,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
    NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
    1,4,4,2,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
    0,5,7,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
    28,0,17,18,13,12,0,1,5,10,16,6,4,23,24,12,13,23,22,19,NA,NA,NA,NA,
    0,0,0,0,2,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
    13,11,0,28,20,19,75,7,7,13,21,15,1,11,6,2,17,4,3,3,1,20,NA,NA,
    0,17,20,14,0,0,30,0,1,0,0,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
    0,0,3,16,0,0,0,0,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
    8,24,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
    0,3,3,4,1,1,2,7,8,2,1,3,8,12,8,4,0,0,0,4,4,0,2,9,
    0,0,6,0,2,3,0,6,7,18,6,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
    5,0,10,2,0,21,0,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
    0,0,10,0,0,0,0,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
    NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA
  ),.Dim=c(24,length(GRTS_Population))))
  y<-cbind(y,matrix(NA,length(GRTS_Population),50-ncol(y)))
  colnames(y)<-paste("y",c(1:ncol(y)),sep=".")
  g=t(structure(.Data=c(#GRTS reach lengths for the 18 populations
    0.41,0.3,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0.6,NA,NA,NA,NA,NA,
    1,1,1,1,1,1,1,0.5,1,1,1,1,1,0.4,0.7,0.7,0.5,1,1,NA,NA,NA,NA,NA,
    1,1,1,1,0.93,1,1,0.86,1,1,1,1,0.78,1,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
    1,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
    NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
    1,1,1,1,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
    1,1,0.91,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,NA,NA,NA,NA,
    0.8,1,1,0.7,1,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,NA,NA,
    1,1,1,0.9,1,1,1,1,1,1,1,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
    0.21,1,1,1,1,1,1,1,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
    1,1,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
    1,1,1,1,0.3,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0.9,1,1,1,1,
    0.32,1,1,0.23,1,1,1,1,1,1,1,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
    1,1,1,1,1,1,1,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
    1,1,1,1,1,1,1,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
    NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA
  ),.Dim=c(24,length(GRTS_Population))))
  g<-cbind(g,matrix(NA,length(GRTS_Population),50-ncol(g)))
  colnames(g)<-paste("g",c(1:ncol(g)),sep=".")
  dat<-data.frame(cbind(GRTS_Population,Fm,AS,H,MS,sites,I_miles,GRTS_miles,Y,OC,Y2,y,g))
  GRTSpops<-data.frame(read.csv(file.path(data.dir,frame_file.name)))
  #GRTSpops<-data.frame(read.csv("2010GRTSdat_2017-11-13.csv"))
  GRTSdat<-merge(GRTSpops[,colnames(GRTSpops)%in%c("GRTS_Population","Order","F_miles")],dat,all=T)
  GRTSdat[,!colnames(GRTSdat)%in%c(colnames(y),colnames(g))] [is.na(GRTSdat[,!colnames(GRTSdat)%in%c(colnames(y),colnames(g))])]<-0
  GRTSdat$mis_miles<-as.numeric(as.character(GRTSdat$F_miles))-as.numeric(as.character(GRTSdat$I_miles))-as.numeric(as.character(GRTSdat$GRTS_miles))
  GRTSdat$I.Fm<-rep(0,dim(GRTSdat)[1])
  GRTSdat$I.H<-rep(0,dim(GRTSdat)[1])
  GRTSdat$I.MS<-rep(0,dim(GRTSdat)[1])
  GRTSdat$I.AS<-rep(0,dim(GRTSdat)[1])
  GRTSdat$Year<-rep(2010,dim(GRTSdat)[1])
  GRTSdat<-GRTSdat[order(GRTSdat$Order),]
  #write.csv(GRTSdat,paste("2010GRTSdat_",Sys.Date(),".csv",sep=""),row.names=FALSE)
  outfile<-file.path(data.dir,subDir,paste0(2010,"GRTSdat_",Sys.Date(),".csv"))
  write.csv(GRTSdat,outfile,row.names = FALSE)
}