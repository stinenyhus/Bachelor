setwd("/Users/kennethenevoldsen/Desktop/Bachelor/Bachelor_code/Bachelor/")
library(pacman)
p_load(plyr, tidyverse, raster, reshape2, knitr, brms, boot, rethinking)



##ToM agent
#see Devaine, et al. (2017) https://doi.org/10.1371/journal.pcbi.1005833

U <- function(a.self, a.op){ # U(): utility function
  # takes two arguments
  # a.self: choice of self
  # a.op: choice of opponnent
  # returns the point reward
  a.self*a.op*5 + (1-a.self)*3 #if both choose 1 you get 5 points and if you choose 0 you get 3 points
}


softmax <- function(e_payoff_diff, beta){
  # Takes two arguments
    # expected payoff difference
    # a beta also called bahaivoural temperature - higher make the agent choice more random
  # Returns probability self choosing 1
  1/(1 + exp(-(e_payoff_diff/beta)))
}

payoff_difference <- function(prop_a.op1){
  prop_a.op1*(U(1,1)-U(0,1))+(1-prop_a.op1)*(U(1,0)-U(0,0))
}

prop_a.op1 <- function(mu, variance, sigma){
  # takes in three arguments
    # mu is approximate mean of 0-ToM posterior distribution
    # variance is subjective uncertainty of the mean 
    # sigma #?#
  # returns probability of opponent choosing 1 
  inv.logit(mu/sqrt(1+(variance+sigma)*3/pi^2))
}

mu_update <- function(mu, variance, a.op){
  # takes 3 arguments
    # mu from the round before
    # variance form the round before
    # a.op, which is the opponents choice last round
  # returns an updated mu
  mu + variance * (a.op - inv.logit(mu))
}

variance_update <- function(mu, variance, sigma) {
  # takes 3 arguments
    # mu from last round
    # variance from last round
    # sigma: higher values mean lower updating
  #returns an updated variance
  1 / (1 / (variance + sigma) + inv.logit(mu) * (1 - inv.logit(mu)))
}




ToM0 <- function(a.op, 
                 beta,
                 sigma,
                 mu, 
                 variance){
  #update parameters based on previous choices
  variance <- variance_update(mu, variance, sigma) #!# make this into an update variable (måske)
  mu <- mu_update(mu, variance, a.op)
  
  
  e_prop_a.op1 <- prop_a.op1(mu, variance, sigma) #estimated probability of opponent chosing 1 in prop
  e_payoff_diff <- payoff_difference(e_prop_a.op1) #estimated payoff difference
  prop_a.self1 <- softmax(e_payoff_diff, beta) #probability of self chosing 1
  
  return(choice)
}

ToM0(a.op = 1, 
     beta = 0.5,
     sigma = 0.2,
     mu = 0.0, #in logodds 
     variance = 1)


a.op = 1
beta = 0.5
sigma = 0.2 #should vary across agents - volatility
mu = 0.0
variance = 1 #?# kan man sætte denne til noget neutrals ligesom ved mean? 


result_df <- result_df[1:2,] #for testing (example round one)


#?# question for eq S2 - is the Ut-1 two round before the current rounds (e.g. is it not the same round as aopt?)

#adding ToM for k>=1

