
p_load(RColorBrewer)

#loading large datasets
load(file = "result_df_full.rda")
load(file = "result_df_full_selfsim.rda")
load(file = "result_df_distribution.rda")
#loading 1v1 datasets
load(file = "result_df_RB_0ToM.rda")
load(file = "result_df_0ToM_2ToM.rda")
load(file = "result_df_2ToM_5ToM.rda")

result_df = result_df_2ToM_5ToM

### CREATING PLOTTING FUNCTIONS ###,l

  #add ID function
add_ID <- function(result_df, add_op = F){
  #DESCRIPTION
  #INPUT
  #OUTPUT
  
  n_rounds <- max(result_df$round_nr)
  n_players <- nrow(result_df)/n_rounds
  result_df$ID <- as.numeric( #as. numeric since sapply returns a matrix
                    sapply(seq(1, n_players, by = 2), #creating a list of the first agent in each pair (jumping by two since it is a pair) 
                    function(x) rep(c(x, x+1), n_rounds))) #creating ID for each pair equal to x and x+1 and repeating for each round
  
  if (add_op){
    result_df$op <- sapply(seq(nrow(result_df)), 
                          function(x) 
                          str_split(result_df$pair[x], " / ")[[1]][str_split(result_df$pair[x], " / ")[[1]] !=  result_df$player[x]])
  }
  
  
  return(result_df)
}


result_df <- add_ID(result_df, add_op = T)
result_sum <- result_df %>% 
  group_by(ID, player, op) %>% 
  summarise(total_point = sum(points), mean_choice = mean(choice)) %>% 
  arrange(desc(total_point))

result_sum <- result_df %>% 
  group_by(sim, player, op) %>% 
  summarise(total_point = sum(points), mean_choice = mean(choice)) %>% 
  arrange(desc(total_point))

result_sum

result_sum_sub <- subset(result_sum, !(op %in% c("RB", "SoftmaxTitTat", "WSLS")) & !(player %in% c("RB", "SoftmaxTitTat", "WSLS")) )

  #create a compete matrix
quick_heatmap <- function(result_df, return_plot_df = F){
  #DESCRIPTION
  #INPUT
  #OUTPUT
  
  tmp_list <- NULL; i = 1
  for (c_pair in unique(result_df$pair)){ #loop through all the pairs
    p_subset <- subset(result_df, pair == c_pair)
    
      #Player 1
    player <- p_subset$player[1]
    op <- p_subset$player[2]
    points <- sum(p_subset$points[seq(1, nrow(p_subset), by = 2)]) #calculate points
    
    tmp_list[[i]] <- data_frame(player = player, op = op, points = points) #save the variables
    
      #Player 2
    player <- p_subset$player[2]
    op <- p_subset$player[1]
    points <- sum(p_subset$points[seq(2, nrow(p_subset), by = 2)]) #calculate points
    
    tmp_list[[i+1]] <- data_frame(player = player, op = op, points = points) #save the variables
    i = i + 2
  }
  i
  tmp_df <- tmp_list %>% bind_rows() #save the variables
  
  unique(tmp_df$player)
  
  p <- ggplot(tmp_df, aes(player, op)) + 
      geom_tile(aes(fill = points), colour = "white") + 
      scale_fill_gradient(low = "lightsteelblue1", high = "steelblue") + 
      theme_classic()
  
  if (return_plot_df){
    return(list(plot = p, plot_df = tmp_df))
  } else{
    return(p)
  }
}

quick_heatmap(result_df, return_plot_df = F)


result_df_sub <- subset(result_df, !(op %in% c("RB", "TFT", "WSLS")) & !(player %in% c("RB", "TFT", "WSLS")))
quick_heatmap(result_df_sub, return_plot_df = F)

#

#parameter values plots #!#

#P-K plot
  #INPUTS
quick_p_k_plot <- function(result_df, ID, blue = T){
  
  p_k_v <- c()
  hidden_states <- result_df$hidden_states[result_df$ID == ID]
  for (i in 1:length(hidden_states)){
    p_k_v <- c(p_k_v, hidden_states[[i]]$own_hidden_states$p_k)
  }
  level <- length(p_k_v)/length(hidden_states)
  plot_df <- data_frame(round = seq(length(hidden_states)))
  
  for (i in 1:level){
    plot_df[,i+1] <- p_k_v[seq(i, length(p_k_v), by= level)]
    colnames(plot_df)[i+1] <- paste("P(", i-1, ")", sep = "")
  }
  
  plot_df1 <- melt(plot_df ,  id.vars = 'round', variable.name = "level")
  p <- ggplot(plot_df1, aes(round, value, color = level)) + geom_line()
  
  if (blue){ #add color palette
    p <- p + scale_color_brewer(palette="Blues", direction = -1) + theme_classic()
  }
  return(p)
  
}

quick_p_k_plot(result_df, ID = 2, blue = T)


#estimation errors
  #we need to save prob of choosing 1
  #and estimate for prob of op choosing 1
result_df = result_df
ID1 = 28
ID2 = 27

ID1_hidden_states <- result_df$hidden_states[result_df$ID == ID1]
ID2_hidden_states = result_df$hidden_states[result_df$ID == ID2]
e_p_op_1 <- c()
a_p_op_1 <- c()
for (i in 1:length(hidden_states)){
  e_p_op_1 <- c(e_p_op_1, p_op_1_k_fun(ID1_hidden_states[[i]])) #estimate probability of op. choosing 1
  a_p_op_1 <- c(a_p_op_1, basic_p_op_1_fun(ID2_hidden_states[[i]])) #actual probability of op. choosing 1
}

plot(seq(length(hidden_states)), #the trials
     a_p_op_1-e_p_op_1 #the predictions error
)
length(a_p_op_1)
length(e_p_op_1)
dens(p_op_1)


#0-ToM plots
  #Variance
variance_basic <- c()
hidden_states <- result_df$hidden_states[result_df$ID == "6"]
for (i in 1:length(hidden_states)){
  variance_basic <- c(variance_basic, result_df$hidden_states[result_df$ID == "6"][[i]]$own_hidden_states$variance_basic)
}

plot(seq(length(hidden_states)), #the trials
     exp(variance_basic)
)

  #mean
mean_basic <- c()
for (i in 1:length(hidden_states)){
  mean_basic <- c(mean_basic, result_df$hidden_states[result_df$ID == "6"][[i]]$own_hidden_states$mean_basic)
}

plot(seq(length(hidden_states)), #the trials
     inv.logit(mean_basic)
)

  #p_op_1
e_p_op_1 <- c()
for (i in 1:length(hidden_states)){
  e_p_op_1 <- c(e_p_op_1, basic_p_op_1_fun(hidden_states[[i]])) 
}

plot(seq(length(hidden_states)), #the trials
     (e_p_op_1)
)


test_2 <- "images_someartists\\somegenre_somenumber.png"
strsplit(test_2, "[_\\ ]+")
