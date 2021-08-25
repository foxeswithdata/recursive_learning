rm(list = ls())

library(tidyverse)

source("code/model/plant_class.R")
source("code/model/environment_run.R")
source("code/util/foxes_pallettes.R")

out_dir <- "out/simulations/"
dir.create(out_dir)

out_dir_temp <- "out/simulations/variable_season"
dir.create(out_dir_temp)


### Model a single year 

Tend = 250

### SET UP WATER ENVIRONMENT

## spending approximately 1/3 of the year in rain
MC_water <- matrix(c(0.6, 0.4, 0.2,  0.8), nrow = 2, byrow = TRUE)
colnames(MC_water) <- c('D', 'R')
rownames(MC_water) <- c('D', 'R')
MC_water

## find the steady state for each 
steady_state <- c(MC_water[2,1]/(MC_water[1,2] + (MC_water[2,1])), 
                  MC_water[1,2]/(MC_water[1,2] + (MC_water[2,1])))

names(steady_state) <- c('D', 'R')
steady_state

# Chi alternative
water_ave <- 40
water_sd <- 0.75

MC_dry <- matrix(c(0.85, 0.15, 0.45, 0.55), nrow=2, byrow = TRUE)
colnames(MC_dry) <- c('D', 'R')
rownames(MC_dry) <- c('D', 'R')
MC_dry

steady_state <- c(MC_dry[2,1]/(MC_dry[1,2] + (MC_dry[2,1])), 
                  MC_dry[1,2]/(MC_dry[1,2] + (MC_dry[2,1])))

names(steady_state) <- c('D', 'R')
steady_state


sim_list = list()
k = c(200, 250)
k = c(200)
goal_func = c(0, 0.65, 1)
time_window = c(10, 25)
W0 = 50

rainfall_list <- lapply(k, function(x){
  generate_rainfall_two_seasons_chi(SeasonEnd = x, Tend = Tend, rain_MC_1 = MC_water, rain_MC_2 = MC_dry, rain_df = water_ave)
})

filename <- tempfile(paste("rainfall", sep = "_"), tmpdir = out_dir_temp, fileext = ".RData")

rainfall_150 <- rainfall_list[[3]]

load("out/simulations/variable_season/rainfall17b03c99e58.RData")
rainfall_list <- rainfall_list[2:3]
# rainfall_list[[1]] <- rainfall_150
# 
# save(rainfall_list, file = filename)

sim_list <- lapply(k, function(ind){
  rain_ind = which(k == ind)
  
  print("sim:") 
  print(ind)
  
  rainfall <- rainfall_list[[rain_ind]]
  print(rainfall)
  rainfall_strict <- rainfall
  rainfall_strict[ind:length(rainfall_strict)] = 0
  
  
  sims_memory <- (lapply(goal_func, function(goal){
    return(lapply(time_window, function(tw){
      file_name_prefix = paste("simulation", "memory", "k", ind,
                               "goal", goal, "time_window", tw, sep = "_")
      files = list.files(path = out_dir_temp, pattern = file_name_prefix)
      print(files)
      if(length(files) > 0){
        return()
      }
      filename <- tempfile(paste("simulation", "memory", "k", ind,
                                 "goal", goal, "time_window", tw, sep = "_"), tmpdir = out_dir_temp, fileext = ".RData")

      plant <- new_plant(W0 = W0);
      simulation <- run_model_memory(plant, rainfall, 5, tw, Tend, W0, goal, 1, fileprefix = "time_memory_season")
      simulation$time_window = tw
      simulation$goal_func = goal
      simulation$Tend = Tend
      simulation$W0 = W0
      simulation$memory = 5
      simulation$rainfall = rainfall
      simulation$plant = plant

      filename <- tempfile(file_name_prefix, tmpdir = out_dir_temp, fileext = ".RData")

      save(simulation, file = filename)

    }))
  }))

  sims_drought_strict <- (lapply(goal_func, function(goal){
    return(lapply(time_window, function(tw){
      file_name_prefix = paste("simulation", "drought_strict", "k", ind, 
                               "goal", goal, "time_window", tw, sep = "_")
      files = list.files(path = out_dir_temp, pattern = file_name_prefix)
      print(files)
      if(length(files) > 0){
        return()
      }
      filename <- tempfile(paste("simulation", "drought_strict", "k", ind, 
                                 "goal", goal, "time_window", tw, sep = "_"), tmpdir = out_dir_temp, fileext = ".RData")
      
      plant <- new_plant(W0 = W0);
      simulation <- run_model_memory(plant, rainfall_strict, 5, tw, Tend, W0, goal, 1, fileprefix = "time_memory_season")
      simulation$time_window = tw
      simulation$goal_func = goal
      simulation$Tend = Tend
      simulation$W0 = W0
      simulation$memory = 5
      simulation$rainfall = rainfall_strict
      simulation$plant = plant
      
      filename <- tempfile(file_name_prefix, tmpdir = out_dir_temp, fileext = ".RData")
      
      save(simulation, file = filename)
      
    }))
  }))
  
  output <- list()
  output$simulations_drought_strict = sims_drought_strict
  output$simulations_memory = sims_memory
  output$rainfall = rainfall
  output$rainfall_strict = rainfall_strict
  
  filename <- tempfile(paste("simulation", "all", "k", ind, sep = "_"), tmpdir = out_dir_temp, fileext = ".RData")
  
  save(output, file = filename)
  
  return(output)
})

