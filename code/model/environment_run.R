generate_rainfall <- function(Tend, rain_MC, rain_ave, rain_sd){
  
  currR <- ifelse(runif(1) > 0.5, 'R', 'D')
  
  rain_seq <- vector()
  for (i in 1:(Tend+1)){
    if(currR == 'R'){
      rain_seq[i] <- (rnorm(1, rain_ave, rain_sd))
      rain_seq[i] <- ifelse(rain_seq[i] < 0, 0, rain_seq[i])
    }
    else{
      rain_seq[i] <- 0
    }
    move = runif(1)
     
    if(move < rain_MC[currR,'R']){
       currR <- 'R'
     }
     else{
       currR <- 'D'
     }
     
  }
  return(rain_seq)
}

generate_rainfall_gamma <- function(Tend, rain_MC, rain_shape, rain_rate){
  
  currR <- ifelse(runif(1) > 0.5, 'R', 'D')
  
  rain_seq <- vector()
  for (i in 1:(Tend+1)){
    if(currR == 'R'){
      rain_seq[i] <- (rgamma(1, rain_shape, rain_rate))
      rain_seq[i] <- ifelse(rain_seq[i] < 0, 0, rain_seq[i])
    }
    else{
      rain_seq[i] <- 0
    }
    move = runif(1)
    
    if(move < rain_MC[currR,'R']){
      currR <- 'R'
    }
    else{
      currR <- 'D'
    }
    
  }
  return(rain_seq)
}

generate_rainfall_chi <- function(Tend, rain_MC, rain_df){
  
  currR <- ifelse(runif(1) > 0.5, 'R', 'D')
  
  rain_seq <- vector()
  for (i in 1:(Tend+1)){
    if(currR == 'R'){
      rain_seq[i] <- (rchisq(1, rain_df))
      rain_seq[i] <- ifelse(rain_seq[i] < 0, 0, rain_seq[i])
    }
    else{
      rain_seq[i] <- 0
    }
    move = runif(1)
    
    if(move < rain_MC[currR,'R']){
      currR <- 'R'
    }
    else{
      currR <- 'D'
    }
    
  }
  return(rain_seq)
}

generate_rainfall_two_seasons_chi <- function(SeasonEnd, Tend, rain_MC_1, rain_MC_2, rain_df){
  
  currR <- ifelse(runif(1) > 0.5, 'R', 'D')
  
  rain_seq <- vector()
  for (i in 1:(SeasonEnd)){
    if(currR == 'R'){
      rain_seq[i] <- (rchisq(1, rain_df))
      rain_seq[i] <- ifelse(rain_seq[i] < 0, 0, rain_seq[i])
    }
    else{
      rain_seq[i] <- 0
    }
    move = runif(1)
    
    if(move < rain_MC_1[currR,'R']){
      currR <- 'R'
    }
    else{
      currR <- 'D'
    }
  }
  
  for (i in (SeasonEnd+1):(Tend+1)){
    if(currR == 'R'){
      rain_seq[i] <- (rchisq(1, rain_df))
      rain_seq[i] <- ifelse(rain_seq[i] < 0, 0, rain_seq[i])
    }
    else{
      rain_seq[i] <- 0
    }
    move = runif(1)
    
    if(move < rain_MC_2[currR,'R']){
      currR <- 'R'
    }
    else{
      currR <- 'D'
    }
  }
  return(rain_seq)
}

run_model <- function(plant, rainfall, tw, Tend, W0, goal_func, deltat){
  time_seq <- seq(from = deltat, to = (Tend-deltat), by = deltat)
  runlength = length(time_seq)
  
  plant <- reset(plant, W0+rainfall[1])
  
  ut_seq = vector(length=runlength)
  W_seq = vector(length=runlength)
  M_seq = vector(length=runlength)
  S_seq = vector(length=runlength)
  rain_prediction = vector(length=runlength)
  predicted = list()
  
  for(t in seq_along(time_seq)){
    print(t)
    out = optim_alloc(plant, rainfall[t]-plant$E, tw, goal_func, deltat)
    ut_seq[t] = out$ut[2];
    
    plant <- apply_control(plant, ut_seq[t], rainfall[t+1])
    W_seq[t] = plant$W
    S_seq[t] = plant$S
    M_seq[t] = plant$M
    rain_prediction[t] = rainfall[t]-plant$E
    
    predicted[[t]] = out
    
    if(plant$S < 0){
      break;
    }
  }
  
  return(list(t = time_seq,
              u = ut_seq,
              M = M_seq,
              S = S_seq,
              W = W_seq, 
              plant = plant,
              rain_prediction = rain_prediction,
              predicted = predicted))
  
}

run_model_simple_pred <- function(plant, rainfall, tw, Tend, W0, goal_func, deltat){
  time_seq <- seq(from = deltat, to = (Tend-deltat), by = deltat)
  runlength = length(time_seq)
  
  plant <- reset(plant, W0+rainfall[1])
  
  ut_seq = vector(length=runlength)
  W_seq = vector(length=runlength)
  M_seq = vector(length=runlength)
  S_seq = vector(length=runlength)
  rain_prediction = vector(length=runlength)
  predicted = list()
  
  for(t in seq_along(time_seq)){
    print(t)
    file = paste0("temp", t, ".csv", collapse = "", sep = "")
    out = optim_alloc(plant, rainfall[t], tw, goal_func, file, deltat)
    ut_seq[t] = out$ut[2];
    
    plant <- apply_control(plant, ut_seq[t], rainfall[t+1])
    W_seq[t] = plant$W
    S_seq[t] = plant$S
    M_seq[t] = plant$M
    rain_prediction[t] = rainfall[t]
    
    predicted[[t]] = out
    
    if(plant$S < 0){
      break;
    }
  }
  
  return(list(t = time_seq,
              u = ut_seq,
              M = M_seq,
              S = S_seq,
              W = W_seq, 
              plant = plant,
              rain_prediction = rain_prediction,
              predicted = predicted))
  
}

run_model_memory <- function(plant, rainfall, memory, tw, Tend, W0, goal_func, deltat, fileprefix = "temp"){
  time_seq <- seq(from = deltat, to = (Tend-deltat), by = deltat)
  runlength = length(time_seq)
  
  plant <- reset(plant, W0+rainfall[1])
  
  ut_seq = vector(length=runlength)
  W_seq = vector(length=runlength)
  M_seq = vector(length=runlength)
  S_seq = vector(length=runlength)
  rain_prediction = vector(length=runlength)
  predicted = list()
  
  rain_memory = c()
  
  for(t in seq_along(time_seq)){
    print(t)
    rain_memory = c(rain_memory, rainfall[t])
    if(length(rain_memory) > memory){
      rain_memory = rain_memory[2:length(rain_memory)]  
    }
    print(rain_memory)
    
    file = paste0(fileprefix, t, "rain_in.csv", collapse = "", sep = "")
    
    out = optim_alloc_memory(plant, rain_memory, tw, goal_func, file, deltat)
    ut_seq[t] = out$ut[2];
    
    plant <- apply_control(plant, ut_seq[t], rainfall[t+1])
    W_seq[t] = plant$W
    S_seq[t] = plant$S
    M_seq[t] = plant$M
    rain_prediction[t] = rainfall[t]
    
    out$rain_prediction = rep(rain_memory, length.out = tw)
    predicted[[t]] = out
    
    if(plant$S < 0){
      break;
    }
  }
  
  return(list(t = time_seq,
              u = ut_seq,
              M = M_seq,
              S = S_seq,
              W = W_seq, 
              plant = plant,
              rain_prediction = rain_prediction,
              predicted = predicted))
  
}

run_model_memory_learning <- function(plant, rainfall, memory, tw, Tend, W0, goal_func, deltat){
  time_seq <- seq(from = deltat, to = (Tend-deltat), by = deltat)
  runlength = length(time_seq)
  
  error_factor = 0.2
  
  plant <- reset(plant, W0+rainfall[1])
  
  ut_seq = vector(length=runlength)
  W_seq = vector(length=runlength)
  M_seq = vector(length=runlength)
  S_seq = vector(length=runlength)
  rain_error_seq = vector(length=runlength)
  rain_prediction = list()
  predicted = list()
  
  rain_memory = c()
  
  rain_error = 0
  
  for(t in seq_along(time_seq)){
    print(t)
    rain_memory = c(rain_memory, rainfall[t])
    if(length(rain_memory) > memory){
      rain_memory = rain_memory[2:length(rain_memory)]  
    }
    print(rain_memory)
    print(rain_error)
    rain_memory - rain_error * error_factor
    print(rain_memory)
    
    file = paste0("temp", t, "rain_in_learn_mem.csv", collapse = "", sep = "")
    
    out = optim_alloc_memory(plant, rain_memory, tw, goal_func, file, deltat)
    ut_seq[t] = out$ut[2];
    
    plant <- apply_control(plant, ut_seq[t], rainfall[t+1])
    W_seq[t] = plant$W
    S_seq[t] = plant$S
    M_seq[t] = plant$M
    
    rain_prediction[t] = rain_memory
    
    rain_error = rainfall[t+1] - rain_memory[1]
    rain_error_seq[t] = rain_error
    rain_error = mean(rain_error_seq)
    
    out$rain_prediction = rep(rain_memory, length.out = tw)
    predicted[[t]] = out
    
    if(plant$S < 0){
      break;
    }
  }
  
  return(list(t = time_seq,
              u = ut_seq,
              M = M_seq,
              S = S_seq,
              W = W_seq, 
              rain_error = rain_error_seq,
              plant = plant,
              rain_prediction = rain_prediction,
              predicted = predicted))
  
}