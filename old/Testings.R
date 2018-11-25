####Questions:

#Where does 1ToM (and KToM) get the volatility param and behavioural param used by the opponent from ? 
#Does it just assume it to be constant?

#What should be the constraints for behavioural temperature and volatility?

#Same starting prior for opponents probability for all ToM models?

#There seems to be problems with the wsls brms model

####TO DO

#Find alternative to arrays for storing estimates etc

#Make the grepl for finding k-level from the string name k-tom

#Make the function that decides between o-ToM, 1-ToM and k-ToM and puts into a ToM-function

#Put that ToM-function into where the agent selects his function

#Rename so they're not called predict_ but instead estimate_ or something like that

#Put the new function inputs into where the 

#Make 0ToM and KToM update function the same


#==============================================================================
#=============================== TESTINGS =====================================
#==============================================================================



####### --------- Making all different ToM into one function -------- #######
###Do seperately for A and B
if (sophistication_level == 0) {
  TOMCHOICE = function() {
    TOM0CHOICE (prev_predict_mean,
                prev_predict_variance,
                prev_choice_opponent,
                volatility_param,
                behavioural_temperature_param, 
                player) 
    }
} else if (sophistication_level == 1) {
  TOMCHOICE = function() {
    TOM1CHOICE (opponent_prev_predict_mean,
                opponent_prev_predict_variance,
                prev_choice_self,
                assumed_volatility_param_opponent,
                assumed_behavioural_temperature_param_opponent, 
                behavioural_temperature_param,
                player) 
  }
} else {
  TOMCHOICE = function() {
    TOMKCHOICE () 
  }
}


#==============================================================================
#============================= Preparation ====================================
#==============================================================================


library(pacman); p_load(brms, tidyverse, stringr)
#==============================================================================
#====================== Data Simulation: Settings =============================
#==============================================================================

#Simulation parameters
ntrials = 100 #number of trials each pair plays
npairs = 10 #number of different pairs simulated

#Set condition
#"Cooperative", "Competitive"
Condition = "Competitive" #Which payoff matrix to use - in competitive, they have different objectives

#Select strategies used in the loop
#"Nash", "WSLS", "0ToM", "1ToM"
A_strat = "0ToM" #strategy of first agent
B_strat = "Nash" #strategy of second agent


##### -------------------- Underlying Distribution for Parameters -------------------- #####
#All parameter values in logodds, where 0 is the mean of the possible parameter values for that parameter.
#A value of 3 would be 0.95 if possible values are 0 to 1.
#Parameter value sampled from a normal distribution. The first number is the mean, the second is the standard deviation,

#For Biased Nash Equilibrium
#Bias of choice parameter 
nash_bias_dist = c(0, 1) #will be constrained between 0 and 1

#For Win-Stay-Lose-Switch
#Probability to stay when winning
WS_prob_dist = c(0, 1) #will be constrained between 0.5 and 1
#Probability to switch when losing
LS_prob_dist = c(0, 1) #will be constrained between 0.5 and 1

#For 0ToM
#Higher volatility makes the agent update less because it expects more noise
volatility_dist = c(0, 1) #will be constrained between 0 and 1
#Higher behavioural temperature makes the agents' choices more random
behavioural_temperature_dist = c(0, 1) #will be constrained between 0 and 1

#Starting prior for opponents choice probability
opponent_choice_probability_prior = c(0, 1) #the mean and the variance of the starting prior


#==============================================================================
#=================== Data simulation: The Function ============================
#==============================================================================
#Save the imulation as a function so it can be run later in different conditions
SIMULATE = function() {
  
  ###### ------ Defining the value function for implementing the payoff matrix ------ ######
  #In competitive condition, the two players have different payoff matrices
  #Player A wins if they choose the same, player B wins if they choose differently
  #In cooperative condition, they both win if they choose the same
  #Values for player input: 0 is player A, 1 is player B
  #Choice inputs are either 0 or 1
  if (Condition == "Competitive") {
    VALUE = function (choice_self, choice_opponent, player) {
        (1-player) * (choice_self * choice_opponent + (1 - choice_self) * (1 - choice_opponent)) + 
        player * (choice_self * (1 - choice_opponent) + (1 - choice_self) * choice_opponent)
    }
  } else if (Condition == "Cooperative") {
    VALUE = function (choice_self, choice_opponent, player) {
      choice_self * choice_opponent + (1 - choice_self) * (1 - choice_opponent)
    }
  }
  
  ##### -------------------- Define sub-functions for strategy functions -------------------- #####
  #Using the equations from the Reading Wilds Minds paper
  
  ###### ------ Updating 0-ToM's uncertainty of estimate ------ ######
  #The volatility parameter reduces the updating
  TOM0_VARIANCE_UPDATE = function(possible_opponents_amount,
                                  prev_predict_mean,
                                  prev_predict_variance,
                                  volatility_param) {
    
    #Create empty list for filling out
    output = NULL
    #Follow the equation
    for (level in 1:(possible_opponents_amount)) {
      
      output[level] = 
        1 / (
          1 / (prev_predict_variance[level] + volatility_param) +
            inv_logit_scaled(prev_predict_mean[level]) 
          * (1 - inv_logit_scaled(prev_predict_mean[level])))
    }
    return (output)
  }
  
  ###### ------ Updating k-ToM's estimate of opponent's uncertainty  ------ ######
  TOMK_VARIANCE_UPDATE = function (possible_opponents_amount,
                                   prev_predict_variance,
                                   volatility_param, 
                                   prev_predict_mean,
                                   level_probability) {
    #Create empty lists for filling out
    output = NULL 
    #Follow the equation
    for (level in 1:(possible_opponents_amount)) {
      
      output[level] = 
        1/((prev_predict_variance[level] + volatility_param)^(-1) + 
           inv_logit_scaled(prev_predict_mean[level]) *
             (1-inv_logit_scaled(prev_predict_mean[level])) * level_probability[level])
    }
    return (output)
  }
  
  ###### ------ Updating 0-ToM's estimate of opponents choice probability  ------ ######
  #Based on the previous choice by the opponent
  TOM0_MEAN_UPDATE = function (possible_opponents_amount,
                               prev_predict_mean,
                               predict_variance,
                               prev_choice_opponent) {
    #Create empty list for filling out
    output = NULL
    #Follow the equation
    for (level in 1:(possible_opponents_amount)) {
      
      output[level] = 
        prev_predict_mean[level] +
        predict_variance[level] * (prev_choice_opponent - inv_logit_scaled(prev_predict_mean[level]))
    }
    return (output)
  }
  
  
  ###### ------ Updating k-ToM's estimate of opponent's estimate of own probability  ------ ######
  TOMK_MEAN_UPDATE = function (possible_opponents_amount,
                               prev_predict_mean,
                               probability_level,
                               predict_variance,
                               prev_choice_opponent) {
    #Create empty list for filling out
    output = NULL
    #Follow the equation
    for (level in 1:(possible_opponents_amount)) {
      
      output[level] = 
        prev_predict_mean[level] + 
        probability_level[level] * 
        predict_variance[level] * 
        (prev_choice_opponent - inv_logit_scaled(prev_predict_mean[level]))
    }
    return (output)
  }
  
  
  ###### ------ Updating k-ToM's estimate of probability of each of opoonent's possible sophistication levels  ------ ######
  TOMK_LEVEL_PROBABILITY_UPDATE = function (possible_opponents_amount,
                                            prev_probability_level,
                                            probability_opponent,
                                            prev_choice_opponent) {
    #Create empty list for filling out
    output = NULL
    #Follow the equation
    for (level in 1:(possible_opponents_amount)) {
      
      output[level] = 
        prev_choice_opponent* #If opponent chose 1
        (prev_probability_level[level]*probability_opponent[level])/
           sum(prev_probability_level*probability_opponent) +
        (1-prev_choice_opponent)* #If opponent chose 2
        (prev_probability_level[level]*(1-probability_opponent[level]))/
           sum(prev_probability_level*(1-probability_opponent))
    }
    return (output)
  }
  
  ###### ------ Calculating 0-ToM's probability of own choice based on estimate of opponent ------ ######
  #Based on mean and variance of estimate
  TOM0_PROBABILITY = function (possible_opponents_amount,
                               predict_mean,
                               predict_variance,
                               volatility_param) {
    #Create empty list for filling out
    output = NULL
    #Follow the equation
    for (level in 1:(possible_opponents_amount)) {
      
      output[level] = 
        inv_logit_scaled(predict_mean[level] / sqrt(1 + (predict_variance[level] + volatility_param) * 3 /pi^2))
    }
    return (output)
  }
  
  TOMK_PROBABILITY_APPROX = function (possible_opponents_amount,
                                      predict_mean,
                                      predict_variance) {
    #This is an approximation derived in Daunizeau (2017), which is used in the Matlab code
    
    #Take the given values
    a = 0.205
    b = 0.319
    c = 0.781
    d = 0.870
    
    #Create empty list for filling out
    output = NULL
    #Follow the equation
    for (level in 1:(possible_opponents_amount)) {
    
    log(
      inv_logit_scaled(
        (predict_mean[level] + b * predict_variance[level]^c)/
          sqrt(1 + a * predict_variance[level]^d)))
    }
    return(output)
  }
  
  ###### ------ Calculating expected value of choosing 1 ------ ######
  #Depending on probability of opponent choosing 1 and on the payoff matrix
  EXPECTED_VALUE = function (possible_opponents_amount,
                             probability_opponent,
                             player) {
    #Create empty list for filling out
    output = NULL
    #Follow the equation
    for (level in 1:(possible_opponents_amount)) {
      
      output[level] = 
        probability_opponent[level] * (VALUE(1, 1, player) - VALUE(0, 1, player)) +
        (1 - probability_opponent[level]) * (VALUE(1, 0, player) - VALUE(0, 0, player))
    }
    return (output)
  }
  
  
  ###### ------ The softmax function makes expected value into choice probability ------ ######
  #Larger payoff -> higher probability to choose 1, controlled by the temperature parameter
  SOFTMAX = function (possible_opponents_amount,
                      expected_value,
                      behavioural_temperature_param) {
    #Create empty list for filling out
    output = NULL
    #Follow the equation
    for (level in 1:(possible_opponents_amount)) {
      
      output[level] = 
        1 / (1 + exp(-(expected_value[level] / behavioural_temperature_param)))
    }
    return (output)
  }
  
  ##### -------------------- Define final strategy functions -------------------- #####
  #Outputs of all functions is 
  #[[1]] the choice 
  #[[2]] the mean of the 0ToM prediction
  #[[3]] the uncertainty of the 0ToM prediction
  #[[4]] the estimated probability of opponent choosing 1
  #[[5]] the probabilities of opponent's different sophistication levels
  #When irrelevant, functions just output a 0
  #All outputs are lists with values for each possible sophistication level
  
  ###### ------ Biased Nash Equlibrium ------ ######
  NASHCHOICE = function(bias_parameter) {
    #Make choice 1 with a probability of the parameter
    choice = rbinom(1, 1, bias_parameter)
    #Output the choice
    return(list(choice, 0, 0, 0, 0))
  }
  
  ###### ------ Win-Stay-Lose-Switch ------ ######
  WSLSCHOICE = function(prev_win,
                        prev_choice_self,
                        ws_param,
                        ls_param) {
    #If previous choice was a win, repeat own previous choice with a probability
    #If previous choice was a loss, do the opposite of own previous choice with a different probability
    choice = rbinom(1, 1, ws_param) * prev_win * prev_choice_self +
      rbinom(1, 1, ls_param) * (1 - prev_win) * (1 - prev_choice_self)
    #Output the choice
    return(list(choice, 0, 0, 0, 0))
  }
  
  ###### ------ 0-ToM ------ ######
  TOM0CHOICE = function (prev_predict_mean,
                   prev_predict_variance,
                   prev_choice_opponent,
                   volatility_param,
                   behavioural_temperature_param, 
                   player) {
    #First update the uncertainty of 0ToM's estimate of opponent probability
    predict_variance = TOM0_VARIANCE_UPDATE(prev_predict_mean, prev_predict_variance, volatility_param)
    #Then update the mean of 0ToM's estimate
    predict_mean = TOM0_MEAN_UPDATE (prev_predict_mean, predict_variance, prev_choice_opponent)
    #Then calculate the estimated probability of 0ToM's opponent selecting 1
    probability_opponent = TOM0_PROBABILITY(predict_mean, predict_variance, volatility_param)
    #Then calculate the payoff of choosing 1, given the opponents probability of choosing 1
    expected_value = EXPECTED_VALUE(probability_opponent, player)
    #Then insert that into the softmax to find the probability of 0ToM choosing 1
    probability_self = SOFTMAX(expected_value, behavioural_temperature_param)
    #And then make the choice with the calculated probability
    choice_self = rbinom(1, 1, probability_self)
    #Output the choice, and also the new mean and variance of estimate, for updating next turn
    return (list(choice_self, predict_mean, predict_variance, 0, 0))
  }

  ###### ------ 1-ToM ------ ######
  TOM1CHOICE = function (prev_predict_mean,
                         prev_predict_variance,
                         prev_choice_self,
                         assumed_volatility_param_opponent,
                         assumed_behavioural_temperature_param_opponent, 
                         behavioural_temperature_param,
                         player) {
    ## -- Simulate a 0ToM process from the opponents perspective -- ##
    #First update the uncertainty of opponent 0ToM's estimate of own probability
    predict_variance = TOM0_VARIANCE_UPDATE(prev_predict_mean, prev_predict_variance, assumed_volatility_param_opponent)
    #Then update the mean of opponent 0ToM's estimate
    predict_mean = TOM0_MEAN_UPDATE (prev_predict_mean, predict_variance, prev_choice_self)
    #Then calculate opponent 0ToM's estimate of own probability of selecting 1
    opponent_probability_self = TOM0_PROBABILITY(predict_mean, predict_variance, assumed_volatility_param_opponent)
    #Then calculate opponent's expected value of selecting 1
    opponent_expected_value = EXPECTED_VALUE(opponent_probability_self, 1-player)
    #Then calculate opponent's probability of selecting 1
    probability_opponent = SOFTMAX(opponent_expected_value, assumed_behavioural_temperature_param_opponent)
    
    ## -- Then make own decision -- ##
    #Then calculate the payoff of choosing 1, given the opponents probability of choosing 1
    expected_value = EXPECTED_VALUE(probability_opponent, player)
    #Then insert that into the softmax to find own probability of choosing 1
    probability_self = SOFTMAX(expected_value, behavioural_temperature_param)
    #And then make the choice with the calculated probability
    choice_self = rbinom(1, 1, probability_self)
    #Output the choice, and also the new mean and variance of estimate, for updating next turn
    return (list(choice_self, predict_mean, predict_variance, 0, 0))
  }
  
  ###### ------ K-ToM ------ ######
  TOMKCHOICE = function (possible_opponents_amount,
                         prev_predict_variance,
                         prev_predict_mean,
                         prev_probability_level,
                         prev_probability_opponent,
                         prev_choice_opponent,
                         prev_choice_self,
                         volatility_param,
                         behavioural_temperature_param,
                         player) {
    
    #First update probabilities for each possible sophistication level
    probability_level = TOMK_LEVEL_PROBABILITY_UPDATE(
      possible_opponents_amount,
      prev_probability_level,
      prev_probability_opponent,
      prev_choice_opponent)
    #Then use that to update the variance
    predict_variance = TOMK_VARIANCE_UPDATE(
      possible_opponents_amount,
      prev_predict_variance,
      volatility_param,
      prev_probability_opponent,
      probability_level)
    #Then use those to update the mean estimate
    predict_mean = TOMK_MEAN_UPDATE(
      possible_opponents_amount,
      prev_predict_mean,
      probability_level,
      predict_variance,
      prev_choice_opponent,
      prev_probability_opponent)
    #Then calculate opponent's estimate of own probability of selecting 1
    opponent_probability = TOMK_PROBABILITY_APPROX(
      possible_opponents_amount,
      predict_mean,
      predict_variance)
    #Then make a mean of probabilitie of opponent choosing 1 for each possible sophistication level
    #Weighted by the probability of the sophistication levels
    probability_opponent_weighted = sum(probability_opponent*probability_level)
    #Then calculate the payoff of choosing 1, given the opponents probability of choosing 1
    expected_value = EXPECTED_VALUE(
      probability_opponent,
      player)
    #Then insert that into the softmax to find the probability of 0ToM choosing 1
    probability_self = SOFTMAX(
      1,
      expected_value,
      behavioural_temperature_param)
    #And then make the choice with the calculated probability
    choice_self = rbinom(1, 1, probability_self)
    #Output the choice, the estimated opponent's estimate, uncertainty and choice probability, and the probabilities for each sophistication level
    return (list(choice_self, predict_mean, predict_variance, probability_opponent, probability_level))
  }
  
##### -------------------- Select Used Strategies -------------------- #####
  #Define the choice function as a strategy depending on which condition was selected.
  #The choice function takes all possible inputs, and outputs the relevant ones into the strategy function
  #This is done for both players
  
  ### --- For A --- ###
  if (A_strat == "Nash") {
    ACHOICE = function(bias_parameter,
                       prev_win,
                       prev_choice_self,
                       ws_param,
                       ls_param,
                       prev_predict_mean,
                       prev_predict_variance,
                       prev_choice_opponent,
                       volatility_param,
                       behavioural_temperature_param,
                       player) {
      NASHCHOICE(bias_parameter)
    }
  } else if (A_strat == "WSLS") {
    ACHOICE = function(bias_parameter,
                       prev_win,
                       prev_choice_self,
                       ws_param,
                       ls_param,
                       prev_predict_mean,
                       prev_predict_variance,
                       prev_choice_opponent,
                       volatility_param,
                       behavioural_temperature_param,
                       player) {
      WSLSCHOICE(prev_win, prev_choice_self, ws_param, ls_param)
    } 
  } else if (A_strat == "0ToM") {
    ACHOICE = function(bias_parameter,
                       prev_win,
                       prev_choice_self,
                       ws_param,
                       ls_param,
                       prev_predict_mean,
                       prev_predict_variance,
                       prev_choice_opponent,
                       volatility_param,
                       behavioural_temperature_param,
                       player) {
      TOM0CHOICE(prev_predict_mean, prev_predict_variance, prev_choice_opponent, volatility_param, behavioural_temperature_param, player)
    } 
  } else if (grepl("ToM", A_strat)) {
    ACHOICE = function(bias_parameter,
                       prev_win,
                       prev_choice_self,
                       ws_param,
                       ls_param,
                       prev_predict_mean,
                       prev_predict_variance,
                       prev_choice_opponent,
                       volatility_param,
                       behavioural_temperature_param,
                       player) {
      TOMCHOICE(prev_predict_mean, prev_predict_variance, prev_choice_opponent, volatility_param, behavioural_temperature_param, player)
    }
    A_sophistication_level <- str_extract(A_strat, "[0-9]")
    A_possible_opponents_amount = max(1, A_sophistication_level - 1)
  }
  
  ### --- For B --- ###
  if (B_strat == "Nash") {
    BCHOICE = function(bias_parameter,
                       prev_win,
                       prev_choice_self,
                       ws_param,
                       ls_param,
                       prev_predict_mean,
                       prev_predict_variance,
                       prev_choice_opponent,
                       volatility_param,
                       behavioural_temperature_param,
                       player) {
      NASHCHOICE(bias_parameter)
    }
  } else if (B_strat == "WSLS") {
    BCHOICE = function(bias_parameter,
                       prev_win,
                       prev_choice_self,
                       ws_param,
                       ls_param,
                       prev_predict_mean,
                       prev_predict_variance,
                       prev_choice_opponent,
                       volatility_param,
                       behavioural_temperature_param,
                       player) {
      WSLSCHOICE(prev_win, prev_choice_self, ws_param, ls_param)
    }
  } else if (B_strat == "0ToM") {
    BCHOICE = function(bias_parameter,
                       prev_win,
                       prev_choice_self,
                       ws_param,
                       ls_param,
                       prev_predict_mean,
                       prev_predict_variance,
                       prev_choice_opponent,
                       volatility_param,
                       behavioural_temperature_param,
                       player) {
      TOM0CHOICE(prev_predict_mean, prev_predict_variance, prev_choice_opponent, volatility_param, behavioural_temperature_param, player)
    }
  } else if (grepl("ToM", B_strat)) {
    BCHOICE = function(bias_parameter,
                       prev_win,
                       prev_choice_self,
                       ws_param,
                       ls_param,
                       prev_predict_mean,
                       prev_predict_variance,
                       prev_choice_opponent,
                       volatility_param,
                       behavioural_temperature_param,
                       player) {
      TOMCHOICE(prev_predict_mean, prev_predict_variance, prev_choice_opponent, volatility_param, behavioural_temperature_param, player)
    }
    B_sophistication_level <- str_extract(B_strat, "[0-9]")
    B_possible_opponents_amount = max(1, B_sophistication_level - 1)
  }
  
  ##### -------------------- Sample parameter values for each agent -------------------- #####
  #For each player in each pair, a parameter value in logodds is sampled from a normal distribution
  #Then that value is transformed logistically into normal values, but can be constrained to a certain parameter space
  
  ###Biased Nash Equlibirum Strategy
  #The bias parameter is constrained between 0 and 1
  A_nash_bias_params = inv_logit_scaled(rnorm(npairs, nash_bias_dist[1], nash_bias_dist[2]), 0, 1)
  B_nash_bias_params = inv_logit_scaled(rnorm(npairs, nash_bias_dist[1], nash_bias_dist[2]), 0, 1)
  
  ###Win-Stay-Lose-Switch Strategy
  #Probabilities of staying and switching are both constrained between 0.5 and 1
  A_WS_prob_params = inv_logit_scaled(rnorm(npairs, WS_prob_dist[1], WS_prob_dist[2]), 0.5, 1)
  A_LS_prob_params = inv_logit_scaled(rnorm(npairs, LS_prob_dist[1], LS_prob_dist[2]), 0.5, 1)
  B_WS_prob_params = inv_logit_scaled(rnorm(npairs, WS_prob_dist[1], WS_prob_dist[2]), 0.5, 1)
  B_LS_prob_params = inv_logit_scaled(rnorm(npairs, LS_prob_dist[1], LS_prob_dist[2]), 0.5, 1)
  
  ###0ToM Strategy
  #The volatility parameter is constrained between 0 and 1
  #The behavioural temperature parameter is constrained between 0 and 1
  #Here is sampled both for the agents' own parameters, but also for their assumption of opponent parameter
  A_volatility_params = inv_logit_scaled(rnorm(npairs, volatility_dist[1], volatility_dist[2]), 0, 1)
  A_behavioural_temperature_params = inv_logit_scaled(rnorm(npairs, behavioural_temperature_dist[1], behavioural_temperature_dist[2]), 0, 1)
  B_volatility_params = inv_logit_scaled(rnorm(npairs, volatility_dist[1], volatility_dist[2]), 0, 1)
  B_behavioural_temperature_params = inv_logit_scaled(rnorm(npairs, behavioural_temperature_dist[1], behavioural_temperature_dist[2]), 0, 1)
  
  ##### -------------------- Setup for the loop -------------------- #####
  
  #Create empty output dataframe
  results = setNames(
    data.frame(matrix(ncol = 6, nrow = 1)),
    c("trial", "pairnr", "A_choice", "B_choice", "A_win", "B_win")
  )
  
  #Empty arrays for filling in data
  A_choice = array(0, c(npairs, ntrials)) #The choice of each agent each round
  B_choice = array(0, c(npairs, ntrials))
  A_win = array(0, c(npairs, ntrials)) #Whether each agent won the round
  B_win = array(0, c(npairs, ntrials))
  A_predict_mean = as_data_frame(array(0, c(npairs, ntrials))) #The mean of 0ToM's estimate. Also used for KToM's estimate of opponents estimate
  B_predict_mean = as_data_frame(array(0, c(npairs, ntrials)))
  A_predict_variance = as_data_frame(array(0, c(npairs, ntrials))) #The variance of 0ToM's estimate. Also used for KToM's estimate of opponents uncertainty
  B_predict_variance = as_data_frame(array(0, c(npairs, ntrials)))
  A_probability_opponent = as_data_frame(array(0, c(npairs, ntrials))) #KToM's estimated probability of opponent choosing 1
  B_probability_opponent = as_data_frame(array(0, c(npairs, ntrials)))
  A_level_probability = as_data_frame(array(0, c(npairs, ntrials))) #KToM's calculated probability of opponent having each possible sophistication level
  B_level_probability = as_data_frame(array(0, c(npairs, ntrials)))
  
  ###### ------ Run the loop ------ ######
  
  #Go through trials one by one, for each pair, one by one
  for (pair in 1:npairs) {
    for (trial in 1:ntrials) {
      ###First trial setup
      if (trial == 1) {
        #Both agents make a random choice in the first trial
        A_choice[pair, trial] = rbinom(1, 1, 0.5)
        B_choice[pair, trial] = rbinom(1, 1, 0.5)
        #The mean and variance of the prediction is set according to the settings
        A_predict_mean[[pair, trial]] = list(rep(opponent_choice_probability_prior[1], A_possible_opponents_amount))
        B_predict_mean[[pair, trial]] = list(rep(opponent_choice_probability_prior[1], B_possible_opponents_amount))
        A_predict_variance[[pair, trial]] = list(rep(opponent_choice_probability_prior[2], A_possible_opponents_amount))
        B_predict_variance[[pair, trial]] = list(rep(opponent_choice_probability_prior[2], B_possible_opponents_amount))
        #KToM is agnostic in first round
        A_probability_opponent[[pair, trial]] = list(rep(0.5, A_possible_opponents_amount))
        B_probability_opponent[[pair, trial]] =list(rep( 0.5, B_possible_opponents_amount))
        #All sophistication levels are equally probable in first round
        A_level_probability[[pair, trial]] = list(rep(1/possible_opponents_amount, A_possible_opponents_amount))
        B_level_probability[[pair, trial]] = list(rep(1/possible_opponents_amount, B_possible_opponents_amount))
        
      } else {
        ###All other trials
        #For both players
        #Use the choice function, which inputs the relevant information into the selected strategy function
        #Outputs are
        #[1] Agents choice
        #[2] Mean of agents probability prediction
        #[3] Variance of agents probability prediction
        A_strategy_output = ACHOICE(
          player = 0,
          sophistication_level = A_sophistication_level,
          possible_opponents_amount = A_possible_opponents_amount,
          prev_choice_self = A_choice[pair, trial - 1],
          prev_choice_opponent = B_choice[[pair, trial - 1]],
          prev_win = A_win[pair, trial - 1],
          prev_predict_mean = A_predict_mean[[pair, trial - 1]],
          prev_predict_variance = A_predict_variance[[pair, trial - 1]],
          prev_probability_opponent = A_probability_opponent[[pair, trial]],
          prev_level_probability = A_level_probability[[pair, trial]],
          bias_parameter = A_nash_bias_params[pair],
          ws_param = A_WS_prob_params[pair],
          ls_param = A_LS_prob_params[pair],
          volatility_param = A_volatility_params[pair],
          behavioural_temperature_param = A_behavioural_temperature_params[pair]
        )
        
        B_strategy_output = BCHOICE(
          player = 1,
          sophistication_level = B_sophistication_level,
          possible_opponents_amount = B_possible_opponents_amount,
          prev_choice_self = B_choice[pair, trial - 1],
          prev_choice_opponent = A_choice[pair, trial - 1],
          prev_win = B_win[pair, trial - 1],
          prev_predict_mean = B_predict_mean[[pair, trial - 1]],
          prev_predict_variance = B_predict_variance[[pair, trial - 1]],
          prev_probability_opponent = B_probability_opponent[[pair, trial]],
          prev_level_probability = B_level_probability[[pair, trial]],
          bias_parameter = B_nash_bias_params[pair],
          ws_param = B_WS_prob_params[pair],
          ls_param = B_LS_prob_params[pair],
          volatility_param = B_volatility_params[pair],
          behavioural_temperature_param = B_behavioural_temperature_params[pair]
        )
        
        #Save the agents' choices for this round
        A_choice[pair, trial] = A_strategy_output[[1]]
        B_choice[pair, trial] = B_strategy_output[[1]]
        
        #Save the agents' prediction mean at this round
        A_predict_mean[pair, trial] = A_strategy_output[[2]]
        B_predict_mean[pair, trial] = B_strategy_output[[2]]
        
        #Save the agents' prediciton variance at this round
        A_predict_variance[pair, trial] = A_strategy_output[[3]]
        B_predict_variance[pair, trial] = B_strategy_output[[3]]
        
        #Save the agents' prediciton variance at this round
        A_probability_opponent[pair, trial] = A_strategy_output[[4]]
        B_probability_opponent[pair, trial] = B_strategy_output[[4]]
        
        #Save the agents' prediciton variance at this round
        A_level_probability[pair, trial] = A_strategy_output[[5]]
        B_level_probability[pair, trial] = B_strategy_output[[5]]
      }
      
      #Save the scores of each player for this round
      #Uses the value function, which implements the payoff matrix
      A_win[pair, trial] = VALUE(A_choice[pair, trial], B_choice[pair, trial], player = 0)
      B_win[pair, trial] = VALUE(B_choice[pair, trial], A_choice[pair, trial], player = 1)
      
      #Append the results of the trial to the output dataframe
      #Choices and scores are saved
      #Do not save the first trial
      if (trial != 1) {
        results = rbind(results,
                        c(trial, pair, A_choice[pair, trial], B_choice[pair, trial],
                          A_win[pair, trial], B_win[pair, trial]))
      }
    }
  }
  #For technical reasons: exclude the first row, which otherwise is just full of NA's
  #Otherwise, output the now full dataframe
  return (results[-1, ])
}

#==============================================================================
#============== Data simulation: Running different conditions =================
#==============================================================================

#0ToM vs nash
Condition = "Competitive"
A_strat = "0ToM"
B_strat = "Nash"
Comp.0ToM.nash = SIMULATE()
