#plotfunction========================================================
JAGSplot<-function(dir,JAGS.sims.list,results.dir){
  dat<-JAGS.sims.list
  pdf(file.path(dir,"JAGSplots.pdf"))
  for(i in 1:length(names(dat))){
    tdat<-dat[[i]]
    if(length(dim(tdat))==3){
      for(j in 1:dim(tdat)[3]){
        plotdat<-as.matrix(tdat[,,j])
        index<-j
        bp<-boxplot.matrix(plotdat,outline=FALSE,plot=FALSE)
        bp$stats[1,]<-apply(plotdat,2,function(x) quantile(x,0.025))
        bp$stats[2,]<-apply(plotdat,2,function(x) quantile(x,0.25))
        bp$stats[3,]<-apply(plotdat,2,function(x) quantile(x,0.5))
        bp$stats[4,]<-apply(plotdat,2,function(x) quantile(x,0.75))
        bp$stats[5,]<-apply(plotdat,2,function(x) quantile(x,0.975))
        bp$out<-NULL
        bxp(bp,outline=F)
        mtext(3,text=paste(names(dat)[[i]],index))
      }
    }else{plotdat<-tdat;index<-""
    bp<-boxplot.matrix(plotdat,outline=FALSE,plot=FALSE)
    bp$stats[1,]<-apply(plotdat,2,function(x) quantile(x,0.025))
    bp$stats[2,]<-apply(plotdat,2,function(x) quantile(x,0.25))
    bp$stats[3,]<-apply(plotdat,2,function(x) quantile(x,0.5))
    bp$stats[4,]<-apply(plotdat,2,function(x) quantile(x,0.75))
    bp$stats[5,]<-apply(plotdat,2,function(x) quantile(x,0.975))
    bp$out<-NULL
    bxp(bp,outline=F)
    mtext(3,text=paste(names(dat)[[i]],index))
    }
  }
  dev.off()
}