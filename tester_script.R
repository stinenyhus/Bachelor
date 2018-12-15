
agent_df$player1 #1-ToM
agent_df$player2 #0-ToM


agent_df$params_p1[[1]]
agent_df$hidden_states_p1[[1]]

output <- k_ToM(params = agent_df$params_p1[[1]], agent_df$hidden_states_p1[[1]], player = 0, level = NULL, p_matrix = penny_competitive_matrix, return_hidden_states = T)
output[[1]] # choice X

output <- k_ToM(params = agent_df$params_p1[[1]], agent_df$hidden_states_p1[[1]], player = 0, level = NULL,  choice_self = 1, choice_op = 0,
                p_matrix = penny_competitive_matrix, return_hidden_states = T)
output[[1]] # choice 0

rec_learning_function(
  prev_hidden_states = agent_df$hidden_states_p1[[1]],
  params = agent_df$params_p1[[1]],
  choices = c(1,0),
  level = 1,
  player = 0,
  p_matrix = penny_competitive_matrix)
  
  
  
t(exp(c(0,0))) %*% c(0,0)^2 

