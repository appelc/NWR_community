## Set your working directory to the "Simple Example Folder"
##     Session -> Set Working Directory ->  Choose Directory
##     Then navigate to where you saved the full example folder

#Load Packages
library(R2jags)

## Load workspace with model run saved
load("SimpleExample_ModelRun.Rdata")

invlogit<-function(p){exp(p)/(1+exp(p))}

## Summary Table - rename for easier access
Example.summary<-Example$BUGSoutput$summary

##  Notice what the dimentions of this summary table are, 101 rows x 9 columns.  The 101 rows 
##     correspond to the 101 parameters we asked JAGS to save
dim(Example.summary)

## Look at first few rows to see what columns show, and rownames to see what values we saved
head(Example.summary)
rownames(Example.summary)

Example.summary

## Assessing convergence - look at Rhat values
hist(Example.summary[,8], main="Rhat Values")
summary(Example.summary[,8]) 

## Assessing cpmbergemce - look at Trace Plots (If you have a lot of parameters this can be hard to do)
par(mfrow=c(4,4), mar=c(3.1,2.1,2.1,2.1), cex=.5)    
traceplot(Example, mfrow = c(4, 4), ask = F, cex=.5)


## Pull out the saved MCMC chains and save them in a more accessable fashion for use later
Example.mcmc<-as.mcmc(Example)
params.Example<-as.data.frame(rbind(Example.mcmc[[1]],Example.mcmc[[2]],Example.mcmc[[3]]))
     ## Notice the dimentions of the parameters file: 3000 rows by 101 columns
     ##     101 matches the number of rows from the summary table, this is the number of parameters we 
     ##     saved.  The 3000 rows comes from the fact that each of our 3 chians saved 1000 draws from 
     ##     the posterior distribution.  Each chain was 100000 steps long, but with a burn-in period of
     ##     50000 and a thinning factor of 50, meaning (100000-50000)/50 = 1000 draws were left.

## Finally save the summary output as a CSV file
write.csv(Example.summary,"ExampleSummary.csv")
