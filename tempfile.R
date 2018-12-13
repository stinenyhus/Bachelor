
savething <- result_df
result_df <- filter(result_df, sim == 1)
result_df$hidden_states[[2]]$own_hidden_states$param_mean[,1]


  #extracting mu / variance for 1-ToM
for (i in seq(2, nrow(result_df), by = 2)){
  mu <- result_df$hidden_states[[i]]$own_hidden_states$param_mean
  var <- result_df$hidden_states[[i]]$own_hidden_states$variance
  if(i == 2){
    mu_matrix <- t(matrix(mu))
    var_matrix <- t(matrix(var))
  } else {
    mu_matrix <- rbind(mu_matrix, mu)
    var_matrix <- rbind(var_matrix, mu)
  }
}

tbl <- data_frame(mu_1_1tom = mu_matrix[,1], mu_2_1tom = mu_matrix[,2], var_1_1tom = var_matrix[,1], var_2_1tom = var_matrix[,2])


  #extracting mu / variance for 0-ToM
for (i in seq(1, nrow(result_df), by = 2)){
  mu <- result_df$hidden_states[[i]]$own_hidden_states$mean_basic
  var <- result_df$hidden_states[[i]]$own_hidden_states$variance_basic
  if(i == 1){
     params <- t(matrix(c(mu, var)))
  } else {
    params <- rbind(params, c(mu, var))
  }
}

tbl$mu_0tom <- params[,1]
tbl$var_0tom <- params[,2]

ggplot()

result_df$hidden_states

