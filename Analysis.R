#Libraries

library(brms)
library(marginaleffects)
library(performance)
library(tidyverse)
library(bayesplot)
#Read in Data

recordings<-read.csv("recordings.csv")

sensorinfo<-read.csv("sensorinfo.csv")


#Model Distribution - Gamma with grouping
recordings$watertemp<-as.numeric(recordings$watertemp)
recordings$sensorid<-as.character(recordings$sensorid)
recordings$dayid<-as.character(recordings$dayid)


song.mod<-brm(songlength~boatnoise+watertemp+boatactivity, data = recordings, family = "Gamma"(link=log))

summary(song.mod)
mcmc_plot(song.mod)
plot(song.mod)
#Model fit
plot_predictions(song.mod, condition = c("boatnoise")) + theme_bw()
predictions(song.mod)

mcmc_plot(song.mod, pars="^b_")

exp(-0.16)
exp(2.48)
#Model distribution - Poisson or Negative binomial
hist(recordings$totsongs)

totsongs<-brm(totsongs~boatnoise+watertemp+boatactivity, data = recordings, family = "negbinomial" (link = "log"))

summary(totsongs)
mcmc_plot(totsongs)

ppc_stat(y = recordings$totsongs,  
         # Compare the dispersion in the real data (y)
         yrep = posterior_predict(totsongs, 
                                  draws = 1000), 
         
         stat="dispersion")
