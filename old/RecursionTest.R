
# A rec function
RecOp <- function(k){
  print(k)
  for (j in 1:k){
    op <- seq(j)
    level = j
    if(j == 1){
      op_stats = "None"
    } else {
      op_stats <- RecOp(j-1)
    }
    output <- list(op = op, level = level, op_stats = op_stats)
  }
  return(output)
}


output <- RecOp(3) #IT WORKS!

output
#fetch variables
  #this is a bit messy, but you could fetch variables recursively but it seems quite intense. 
output$op_stats$op_stats$op_stats
tester <- unlist(output)
tester[5]
unnest(output)
lapply(output, unlist)
get_function(list)