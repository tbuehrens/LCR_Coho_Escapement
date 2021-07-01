library(tidyverse)
library(rstan)
library(rstanarm)
library(mgcv)
library(ggplot2)
library(gridExtra)
library(brms)


f_year<-2020 #this is the year we want to predict prelim escapement for
#===========================
#Data update links and files
#===========================
# LCR Totals = sum of medians by year Coho_TRT_Adults.csv
# LCR Wild = sum of medians by year Coho_TRT_UM_Ad.csv
# Bonneville Dam: http://www.cbr.washington.edu/dart/wrapper?type=php&fname=adultannual_1613075788_519.php
# Willamette Dam: http://www.cbr.washington.edu/dart/wrapper?type=php&fname=adultannual_1613076456_104.php
# Clackamas (early + late wild): https://portlandgeneral.com/about/fish-wildlife-habitats/fish-counts-fish-runs/clackamas-daily-fish-counts
# cowlitz ...open 10 year table https://www.mytpu.org/community-environment/fish-wildlife-environment/cowlitz-fish-report/
# washougal get the type N adult Hatchery total: https://wdfw.wa.gov/fishing/management/hatcheries/escapement#weekly-reports

dat<-read_csv("cohocorr.csv")%>%
  mutate_at(.vars=c("Bonneville.total","Willamette.total", "Clackamas.w", "Cowlitz.barrier.total", "Washougal.hatchery.late.h"),.funs=function(x) c(scale(log(x))))

knots<-length(unique(dat$Yr))-1

m1<-stan_gamm4(log(LCR.wild) ~ 
             s(Yr,bs="ps") +
             Bonneville.total + 
             Willamette.total +
             Clackamas.w +
             Cowlitz.barrier.total +
             Washougal.hatchery.late.h,
           
             #prior=laplace(),
             prior=hs(
             ),
           
           data=dat%>%filter(Yr<f_year),
           prior_intercept = cauchy(0,10),
           chains=4,
           cores=4,
           iter=10000,
           warmup=8000,
           control=list(adapt_delta=0.9995)
)

preds<-apply(posterior_predict(m1,newdata = data.frame(dat%>%select(-c("LCR.wild","LCR.total"))),draws=1000),2,function(x) quantile(x,c(0.025,0.25,0.5,0.75,0.975)))
results<-data.frame(dat,exp(t(preds)))

p<-ggplot(results,aes(x=Yr,y=LCR.wild))+
  geom_point(size=2)+
  geom_ribbon(aes(ymin=X2.5.,ymax=X97.5.),alpha=0.2)+
  geom_ribbon(aes(ymin=X25.,ymax=X75.),alpha=0.2)+
  geom_line(aes(x=Yr,y=X50.))+
  geom_point(data=results%>%filter(Yr==f_year),aes(x=Yr,y=X50.),color="forestgreen",size=3)



m2<-stan_gamm4(log(LCR.total) ~ 
                 s(Yr,bs="ps") +
                 Bonneville.total + 
                 Willamette.total +
                 Clackamas.w +
                 Cowlitz.barrier.total +
                 Washougal.hatchery.late.h,
               
               #prior=laplace(),
               prior=hs(
               ),
               
               data=dat%>%filter(Yr<f_year),
               prior_intercept = cauchy(0,10),
               chains=4,
               cores=4,
               iter=10000,
               warmup=8000,
               control=list(adapt_delta=0.9995)
)

preds2<-apply(posterior_predict(m2,newdata = data.frame(dat%>%select(-c("LCR.wild","LCR.total"))),draws=1000),2,function(x) quantile(x,c(0.025,0.25,0.5,0.75,0.975)))
results2<-data.frame(dat,exp(t(preds2)))

p2<-ggplot(results2,aes(x=Yr,y=LCR.total))+
  geom_point(size=2)+
  geom_ribbon(aes(ymin=X2.5.,ymax=X97.5.),alpha=0.2)+
  geom_ribbon(aes(ymin=X25.,ymax=X75.),alpha=0.2)+
  geom_line(aes(x=Yr,y=X50.))+
  geom_point(data=results2%>%filter(Yr==f_year),aes(x=Yr,y=X50.),color="forestgreen",size=3)

print("Wild Escapement for whole LCR:")
print(exp(preds[,dim(preds)[2]]))
print("Totals Escapement for whole LCR:")
print(exp(preds2[,dim(preds2)[2]]))
grid.arrange(p, p2, ncol=2)
