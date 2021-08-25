rm(list = ls())

library(tidyverse)
library(cowplot)
library(patchwork)
library(ggnewscale)

source("code/model/plant_class.R")
source("code/model/environment_run.R")
source("code/util/foxes_pallettes.R")
source("code/model/water_model_opt.R")

out_folder <- "out/figures/fitness"
dir.create(out_folder)

files <- list.files("out/simulations/check_strategy/", pattern = "simulation_k_1")

all_sims <- lapply(files, function(file_name){
  load(file = paste("out/simulations/check_strategy/", file_name, collapse = "", sep = ""))
  return(simulation)
})

sims_of_interest <- all_sims[sapply(all_sims, function(sim){
  if(sim$time_window == 25 & sim$goal_func %in% c(0.65, 1)){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
})]

out <- lapply(sims_of_interest, function(sim){
  data <- data.frame(time = sim$t,
                     alloc = sim$u,
                     biomass = sim$M,
                     storage = sim$S,
                     water = sim$W,
                     rainfall = sim$rain_prediction)
  data$time_window = sim$time_window
  data$Tend = sim$Tend
  data$goal_func = sim$goal_func
  data$W0 = sim$W0
  data$memory = sim$memory
  
  data$goal_equiv = sapply(sim$predicted, function(pred){
    return(which.min(c(sum(abs(pred$ut - pred$ut.maxS)), sum(abs(pred$ut - pred$ut.maxM)))) - 1)
  })
  
  return(data)
})

out_df <- plyr::rbind.fill(out)

out_no_mem_maxM <- subset(out_df, goal_func == 1 & time_window == 25)
out_no_mem_max65 <- subset(out_df, goal_func == 0.65 & time_window == 25)

unaligned <- which(!near(out_no_mem_max65$alloc, out_no_mem_maxM$alloc, tol = 0.01))
out_no_mem_max65$alloc

prediction_65_2 <- sims_of_interest[[1]]$predicted[[unaligned[2]]]
prediction_1_2 <- sims_of_interest[[2]]$predicted[[unaligned[2]]]

sum(prediction_65_2$biomass * 0.65 + prediction_65_2$storage * 0.35)
sum(prediction_65_2$biomass.maxM * 0.65 + prediction_65_2$storage.maxM * 0.35)
sum(prediction_65_2$biomass.maxS * 0.65 + prediction_65_2$storage.maxS * 0.35)

sum(prediction_1_2$biomass * 0.65 + prediction_1_2$storage * 0.35)
sum(prediction_1_2$biomass.maxM * 0.65 + prediction_1_2$storage.maxM * 0.35)
sum(prediction_1_2$biomass.maxS * 0.65 + prediction_1_2$storage.maxS * 0.35)

sum(prediction_1_2$biomass)
sum(prediction_1_2$biomass.maxM)
sum(prediction_1_2$biomass.maxS)

sum(prediction_65_2$biomass)
sum(prediction_65_2$biomass.maxM)

prediction_1_2

