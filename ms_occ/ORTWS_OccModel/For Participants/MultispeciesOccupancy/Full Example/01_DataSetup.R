## Set your working directory to the "Simple Example Folder"
##     Session -> Set Working Directory ->  Choose Directory
##     Then navigate to where you saved the full example folder

#Load Packages
library(reshape)

#Read in Data
#Presence/Absence data
data.occ<-read.csv("PA_SiteYear_SeasonVisit.csv",head=T)
data.occ$Site.Year<-factor(data.occ$Site.Year,levels=c("0.1.2012","0.2.2012","0.3.2012","0.4.2012",
                                                       "5.1.2012","5.2.2012","5.3.2012","5.4.2012",
                                                       "10.1.2012","10.2.2012","10.3.2012","10.4.2012",
                                                       "15.1.2012","15.2.2012","15.3.2012","15.4.2012",
                                                       "0.1.2013","0.2.2013","0.3.2013","0.4.2013",
                                                       "5.1.2013","5.2.2013","5.3.2013","5.4.2013",
                                                       "10.1.2013","10.2.2013","10.3.2013","10.4.2013",
                                                       "15.1.2013","15.2.2013","15.3.2013","15.4.2013"))

#Creat array of occupancy
temp.melt<-melt(data.occ, id.var=c("Season.Visit","Site.Year","Species"), measure.var="PA")
X<-cast(temp.melt,Species~Site.Year~Season.Visit)
rm(temp.melt)
#X[i,j,k]=P/A for species i Site.Year j on Season.Visit k

#Occupancy Covariates
Occ.CoVar<-read.csv("OccupancyCovariates.csv",head=T)

## Center and scale YSDM for use in the occupancy model
YSDM<-scale(Occ.CoVar$YSDMValue)[1:32,1]


#Detection Covariates
Rain<-read.csv("Rain.csv",head=T)
Rain<-Rain[,-1]
#Rain[j,k]=rainfall for Site.Year j at Season.Visit k

#Loop sizes
nspecies<-15
nsite.year<-32
nseason.visit<-8

nseason.visit.1<-rep(c(5,1),each=16)

save.image("SimpleExample_DataSetUp.RData")
