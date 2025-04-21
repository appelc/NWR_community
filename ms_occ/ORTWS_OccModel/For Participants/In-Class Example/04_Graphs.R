## Set your working directory to the "Simple Example Folder"
##     Session -> Set Working Directory ->  Choose Directory
##     Then navigate to where you saved the full example folder

#Load Packages
library(R2jags)
library(ggplot2)

## Load workspace with model run saved
load("SimpleExample_ModelRun.Rdata")

invlogit<-function(p){exp(p)/(1+exp(p))}

## Summary Table - rename for easier access
Example.summary<-Example$BUGSoutput$summary


## Occupancy Effect Plot ##
species<-c("ACGR","ANQU","ANTE","GACA","HYCH", "HYCI","HYFE","HYSQ","LICA",
                  "LICL","LISP","LIVI","PSBR","PSCR","PSOC")

p1<-as.data.frame(Example.summary[48:62,c(1,3,7)])
p1$Species<-species
colnames(p1)<-c("mean","lower","upper","Species")

ggplot(p1,aes(mean,Species)) + theme_bw() + geom_point(cex=4) + geom_errorbarh(aes(xmax=upper,xmin=lower),height=.4) + 
  labs(title="Species Specific YSDM Coefficient Estimates",x="a.YSDM (95% Credible Interval)") + theme(plot.title=element_text(size=18)) + 
  theme(axis.title.y = element_text(size=16)) + theme(axis.text.y = element_text(size=14)) + 
  theme(axis.title.x = element_text(size=16)) + theme(axis.text.x = element_text(size=14))

## With dashed line showing mean YSDM coefficient ##
ggplot(p1,aes(mean,Species)) + theme_bw() + geom_point(cex=4) + 
  geom_errorbarh(aes(xmax=upper,xmin=lower),height=.4) + geom_vline(xintercept=Example.summary[95,1],color="grey",linetype="dashed") +
  labs(title="Species Specific YSDM Coefficient Estimates",x="a.YSDM (95% Credible Interval)") + theme(plot.title=element_text(size=18)) + 
  theme(axis.title.y = element_text(size=16)) + theme(axis.text.y = element_text(size=14)) + 
  theme(axis.title.x = element_text(size=16)) + theme(axis.text.x = element_text(size=14))


## Species Richness plot ##
site.year<-factor(c("0.1.2012","0.2.2012","0.3.2012","0.4.2012",
             "5.1.2012","5.2.2012","5.3.2012","5.4.2012",
             "10.1.2012","10.2.2012","10.3.2012","10.4.2012",
             "15.1.2012","15.2.2012","15.3.2012","15.4.2012",
             "0.1.2013","0.2.2013","0.3.2013","0.4.2013",
             "5.1.2013","5.2.2013","5.3.2013","5.4.2013",
             "10.1.2013","10.2.2013","10.3.2013","10.4.2013",
             "15.1.2013","15.2.2013","15.3.2013","15.4.2013"),
             levels=c("0.1.2012","0.2.2012","0.3.2012","0.4.2012",
             "5.1.2012","5.2.2012","5.3.2012","5.4.2012",
             "10.1.2012","10.2.2012","10.3.2012","10.4.2012",
             "15.1.2012","15.2.2012","15.3.2012","15.4.2012",
             "0.1.2013","0.2.2013","0.3.2013","0.4.2013",
             "5.1.2013","5.2.2013","5.3.2013","5.4.2013",
             "10.1.2013","10.2.2013","10.3.2013","10.4.2013",
             "15.1.2013","15.2.2013","15.3.2013","15.4.2013"))

p2<-as.data.frame(Example.summary[1:32,c(1,3,7)])
p2$SiteYear<-site.year
colnames(p2)<-c("mean","lower","upper","SiteYear")

ggplot(p2,aes(SiteYear,mean)) + theme_bw() + geom_point(cex=4) + geom_errorbar(aes(ymax=upper,ymin=lower),width=.4) + 
  labs(title="Species Richness by Site.Year",y="Species Richness (95% Credible Interval)") + theme(plot.title=element_text(size=18)) + 
  theme(axis.title.y = element_text(size=16)) + theme(axis.text.y = element_text(size=14)) + 
  theme(axis.title.x = element_text(size=16)) + theme(axis.text.x = element_text(size=14,angle=-90,vjust=.5))


## Rescale y-axis to show 0-15 species
ggplot(p2,aes(SiteYear,mean)) + theme_bw() + ylim(0,15) + geom_point(cex=4) + geom_errorbar(aes(ymax=upper,ymin=lower),width=.4) + 
  labs(title="Species Richness by Site.Year",y="Species Richness (95% Credible Interval)") + theme(plot.title=element_text(size=18)) + 
  theme(axis.title.y = element_text(size=16)) + theme(axis.text.y = element_text(size=14)) + 
  theme(axis.title.x = element_text(size=16)) + theme(axis.text.x = element_text(size=14,angle=-90,vjust=.5))



## Detection Probability Plot
d1<-as.data.frame(Example.summary[63:77,c(1,3,7)])
d1$Species<-species
colnames(d1)<-c("mean","lower","upper","Species")
d1[,1]<-invlogit(d1[,1])
d1[,2]<-invlogit(d1[,2])
d1[,3]<-invlogit(d1[,3])


ggplot(d1,aes(mean,Species)) + theme_bw() + geom_point(cex=4) + geom_errorbarh(aes(xmax=upper,xmin=lower),height=.4) + 
  labs(title="Species Specific Detection Probabilities - 0 inches Rain",x="Prob(Detection) (95% Credible Interval)") + theme(plot.title=element_text(size=18)) + 
  theme(axis.title.y = element_text(size=16)) + theme(axis.text.y = element_text(size=14)) + 
  theme(axis.title.x = element_text(size=16)) + theme(axis.text.x = element_text(size=14)) + xlim(0,1)



## a.0 distribution plot
mu.a.0<-Example.summary[94,1]
sig.a.0<-Example.summary[98,1]
a.0<-Example.summary[33:47,1]

x<-seq(-4.5,5.5,0.01)
y<-dnorm(x,mean=mu.a.0,sd=sig.a.0)
dens<-as.data.frame(cbind(x,y))
ggplot(dens,aes(x,y)) + theme_bw() + geom_point() + 
  geom_segment(x=a.0[1],xend=a.0[1],y=-1,yend=.265,linetype="dashed") + geom_segment(x=a.0[2],xend=a.0[2],y=-1,yend=.265,linetype="dashed") + 
  geom_segment(x=a.0[3],xend=a.0[3],y=-1,yend=.265,linetype="dashed") + geom_segment(x=a.0[4],xend=a.0[4],y=-1,yend=.265,linetype="dashed") + 
  geom_segment(x=a.0[5],xend=a.0[5],y=-1,yend=.265,linetype="dashed") + geom_segment(x=a.0[6],xend=a.0[6],y=-1,yend=.265,linetype="dashed") + 
  geom_segment(x=a.0[7],xend=a.0[7],y=-1,yend=.265,linetype="dashed") + geom_segment(x=a.0[8],xend=a.0[8],y=-1,yend=.265,linetype="dashed") + 
  geom_segment(x=a.0[9],xend=a.0[9],y=-1,yend=.265,linetype="dashed") + geom_segment(x=a.0[10],xend=a.0[10],y=-1,yend=.265,linetype="dashed") + 
  geom_segment(x=a.0[11],xend=a.0[11],y=-1,yend=.265,linetype="dashed") + geom_segment(x=a.0[12],xend=a.0[12],y=-1,yend=.265,linetype="dashed") + 
  geom_segment(x=a.0[13],xend=a.0[13],y=-1,yend=.265,linetype="dashed") + geom_segment(x=a.0[14],xend=a.0[14],y=-1,yend=.265,linetype="dashed") + 
  geom_segment(x=a.0[15],xend=a.0[15],y=-1,yend=.265,linetype="dashed") + 
  labs(title="Distribution of a.0 coefficients",x="a.0 Value",y="Probability") + theme(plot.title=element_text(size=18)) + 
  theme(axis.title.y = element_text(size=16)) + theme(axis.text.y = element_text(size=14)) + 
  theme(axis.title.x = element_text(size=16)) + theme(axis.text.x = element_text(size=14))

