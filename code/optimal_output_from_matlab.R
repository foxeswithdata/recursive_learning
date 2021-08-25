files <- list.files("code/matlab/", pattern = "^opt_detail" )

all_sims <- lapply(files, function(file_name){
  simulation <- read.csv(file = paste("code/matlab/", file_name, collapse = "", sep = ""), header = FALSE, col.names = c("alloc", "biomass", 
                                                                                                                         "storage", "water"))
  file_parts <- unlist(str_split(substr(file_name, 1, (nchar(file_name) - 4)), pattern = "_"))
  simulation$goal_func <- as.numeric(file_parts[7])
  simulation$sim_num <- as.numeric(file_parts[3])
  simulation$k <- as.numeric(file_parts[5])
  simulation$time <- 1:nrow(simulation)
  return(simulation)
})

all_sims_df <- plyr::rbind.fill(all_sims)

all_sims_df <- all_sims_df %>%
  dplyr::filter(goal_func %in% c(0, 0.45, 0.5, 0.55, 0.6, 0.65, 0.75, 1) & sim_num <= 5)

goal_funcs <- unique(all_sims_df$goal_func)
rainfall_nums <- unique(all_sims_df$k) 

out_matlab <- plyr::rbind.fill(lapply(rainfall_nums, function(k_val){
  plyr::rbind.fill(lapply(goal_funcs, function(goal_func_val){
    sim_subset <- all_sims_df %>%
      dplyr::filter(goal_func == goal_func_val & k == k_val)
    if(nrow(sim_subset) == 0){
      return(data.frame())
    }
    sim_nums <- unique(sim_subset$sim_num)
    max_val <- sim_nums[which.max(sapply(sim_nums, function(ind){
      goal_sim_sub <- subset(sim_subset, sim_num == ind)
      objective = sum(goal_func_val * goal_sim_sub$M) + sum((1-goal_func_val) * goal_sim_sub$S)
      return(objective)
    }))]
    final_goal_sim_sub <- subset(sim_subset, sim_num == max_val)
    return(final_goal_sim_sub)
  }))
}))

