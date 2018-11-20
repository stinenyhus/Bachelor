
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

prepare_op <- function(level){
  new_hidden_states = list()
  if (level == 0){
    variance_basic <- 0.5
    mean_basic <- 0.2
    own_hidden_states <- list(variance_basic = variance_basic, mean_basic = mean_basic)
  } else {
    p_k <- rep(1, level)/level
    variance <- rep(1, level) * 0.5
    parameter_mean <- rep(1, level) * 0.2
    
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




