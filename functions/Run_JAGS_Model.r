Run_JAGS_Model<-function(models.dir,
                         results.dir,
                         model.file,
                         pars,
                         chains,
                         thin,
                         burn,
                         iter){
  start.time<-Sys.time()
  print(start.time)
  jagsfit <- jags.parallel(jagsdat,
                           #inits=initials(jagsdat),
                           inits=NULL,
                           #inits=parallel.inits(initials,4),
                           #model.file="C:\\data\\BPA Projects\\BPA CWT\\Lower Columbia Coho Escapement\\Analysis\\2013-15\\2013CohoEscapement_Model_9_20_2017.txt",
                           model.file=file.path(models.dir,model.file),
                           # n.chains=chains,
                           # n.thin=thin,
                           # n.burnin=burn,
                           # n.iter=iter,#60000 takes 20 mins
                           n.chains=4,
                           n.thin=1,
                           n.burnin=100,
                           n.iter=200,#60000 takes 20 mins
                           parameters.to.save=pars)
  end.time<-Sys.time()
  print(end.time-start.time)
  
  JAGS.sims.list<-jagsfit$BUGSoutput$sims.list
  
  #============
  #save results
  #============
  subDir<-paste("Coho_Results",Sys.Date(),sep="_")
  if (!file.exists(subDir)){
    dir.create(file.path(results.dir, subDir))
  }
  JAGSplot(dir=file.path(results.dir,subDir),jagsfit$BUGSoutput$sims.list)
  write.csv(jagsfit$BUGSoutput$summary,file.path(results.dir,subDir,"summary.csv"))
  save(jagsfit,file=file.path(results.dir,subDir,"modelfit.RData"))
  setwd(home.dir)
  return(jagsfit)
}