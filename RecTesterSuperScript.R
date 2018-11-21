
###______________________ REC TEST FUNCTION ______________________###

recTest <- function(prev_hidden_states, level){
  new_hidden_states = list()
  if (level == 0){
    variance_basic <- prev_hidden_states$own_hidden_states$variance_basic + 1
    mean_basic <- prev_hidden_states$own_hidden_states$mean_basic + 2
    own_hidden_states <- list(variance_basic = variance_basic, mean_basic = mean_basic)
  } else {
    p_k <- prev_hidden_states$own_hidden_states$p_k
    variance <- prev_hidden_states$own_hidden_states$variance * p_k
    parameter_mean <- prev_hidden_states$own_hidden_states$parameter_mean * p_k
    
    mean = NULL
  
    
    for (level_index in 1:level){
      op_level = level_index - 1
      new_hidden_states[[level_index]] <- recTest(prev_hidden_states[[level_index]], op_level)
      mean[level_index] = mean(new_hidden_states[[level_index]]$own_hidden_states$parameter_mean)
    }
    
    own_hidden_states <- list(p_k = p_k, variance = variance, parameter_mean = parameter_mean, mean = mean)
  }

  new_hidden_states$own_hidden_states <- own_hidden_states
  return(new_hidden_states)
}

###______________________ NEW FUNCTION ______________________###

prepare_kToM <- function(k){
  new_hidden_states = list()
  if (level == 0){
    variance_basic <- 0.5
    mean_basic <- 0.2
    own_hidden_states <- list(variance_basic = variance_basic, mean_basic = mean_basic)
  } else {
    
    behavioral_temp = -1 #note: exp(-1) = 0.37
    volatility = -2 #note exp(-2) = 0.14 #!# assumed from matlab code where theta = -2
    
    p_k <- rep(1, level)/level
    variance <- 0
    parameter_mean <-0 #Note 
    
    mean = NULL
    
    for (level_index in 1:level){
      op_level = level_index - 1
      new_hidden_states[[level_index]] <- prepare_op(level = op_level)
      mean[level_index] = 2
    }
    
    own_hidden_states <- list(p_k = p_k, variance = variance, parameter_mean = parameter_mean, mean = mean)
  }
  
  new_hidden_states$own_hidden_states <- own_hidden_states
  return(new_hidden_states)
}
prev_hidden_states <- prepare_op(3)

prev_hidden_states[[2]]
tester <- recTest(prev_hidden_states, 3)

###______________________ OLD FUNCTION ______________________###

prepare_kToM <- function(k){ 
  #!# note in the following the sd=0, but the intention is to add it using an argument
  behavioral_temp = rnorm(1, mean = 0.37, sd = 0.0) #note: exp(-1) = 0.37
  sigma = rnorm(1, mean = 0.14, sd = 0.0) #, note exp(-2) = 0.14 #!# assumed from matlab code where theta = -2
  
  if (k == 0){
    variance = rnorm(1, mean = 1, sd = 0.0) #note exp(0) = 1
    mu = rnorm(1, mean = 0.5, sd = 0.0) #note inv.logit(0.5) = 0
    
    params <- list(behavioral_temp = behavioral_temp, 
                   variance = variance, 
                   mu = mu, 
                   sigma = sigma, 
                   k = k)
    
  } else if (k > 0){
    p_k <- rep(1/k, k) #probabilty for each possible sophistication level of the opponent 
    #(note: 1-ToM's opponent == 0-ToM, e.g. p_k = 1) 
    mu <- rep(0.5, k) #equal chance of choosing both strategies (naive starting point)
    variance <- rnorm(k, mean = 1, sd = 0.0)
    
    params <- list(behavioral_temp = behavioral_temp, 
                   variance = variance, 
                   mu = mu, 
                   sigma = sigma,
                   p_k = p_k, 
                   k = k)
  }
  params <- list(params)
  return(params)
}

###______________________ THE END ______________________##



