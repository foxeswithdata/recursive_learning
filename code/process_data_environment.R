rm(list = ls())

library(tidyverse)
library(cowplot)
library(stringr)
library(patchwork)
library(ggnewscale)

source("code/model/plant_class.R")
source("code/model/environment_run.R")
source("code/util/foxes_pallettes.R")
source("code/model/water_model_opt.R")

out_folder <- "out/figures/environment"
dir.create(out_folder)

files <- list.files("out/simulations/variable_season/", pattern = "simulation_(d|m)")

all_sims <- lapply(files, function(file_name){
  load(file = paste("out/simulations/variable_season/", file_name, collapse = "", sep = ""))
  name_parts <- unlist(str_split(file_name, pattern = "_"))
  if(name_parts[3] == "strict"){
    simulation$type = "strict_drought"
    simulation$season_length = as.numeric(name_parts[5])
  }
  else{
    simulation$type = "regular_drought"
    simulation$season_length = as.numeric(name_parts[4])
  }
  return(simulation)
})

out <- lapply(all_sims, function(sim){
  if(length(sim$predicted) < length(sim$rain_prediction)){
    sim$rain_prediction <- sim$rain_prediction[1:length(sim$predicted)]
    sim$t <- sim$t[1:length(sim$predicted)]
    sim$u <- sim$u[1:length(sim$predicted)]
    sim$M <- sim$M[1:length(sim$predicted)]
    sim$S <- sim$S[1:length(sim$predicted)]
    sim$W <- sim$W[1:length(sim$predicted)]
  }
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
  data$drought_type = sim$type
  data$season_length = sim$season_length
  
  data$goal_equiv = sapply(sim$predicted, function(pred){
    return(which.min(c(sum(abs(pred$ut - pred$ut.maxS)), sum(abs(pred$ut - pred$ut.maxM)))) - 1)
  })
  return(data)
})

out_simple <- plyr::rbind.fill(out)

out_simple <- pivot_longer(out_simple, cols = c("biomass", "storage"), names_to = "carbon_pool_type", values_to = "carbon_pool_val")
out_simple <- pivot_longer(out_simple, cols = c("water"), names_to = "water_pool_type", values_to = "water_pool_val")

out_simple_sub <- subset(out_simple, time_window == 25 & goal_func == 0)

coeff = 0.2
p_mpc_maxM <- ggplot(out_simple_sub, aes(x=time, y=carbon_pool_val)) +
  geom_hline(yintercept=0, color="#083855", size = 0.5,linetype="dashed") + 
  geom_line(mapping=aes(y=water_pool_val/coeff, fill = water_pool_type), 
            size=1, linetype="longdash", alpha=0.5, color=foxes_palettes$main[5]) + 
  geom_line(mapping=aes(colour=carbon_pool_type), size = 1, show.legend=TRUE) + 
  scale_y_continuous("Carbon Pool Size (gC)", 
                     sec.axis = sec_axis(~.*coeff, name = expression(paste("Available Water (kg", H[2], "O)")))) +
  scale_color_manual("Carbon Pool:",
                     values=foxes_palette(n=2, name="main"),
                     guide=guide_legend(order=1)) +
  scale_fill_manual(name  ="Water Pool:", values = rep(1, 3),
                    guide=guide_legend(override.aes = list(colour=foxes_palettes$main[5], alpha = 0.5, linetype = "longdash") , order=2),
                    labels=c("Available Soil Water")) +
  labs(x = "Time (day)") +
  facet_grid(cols = vars(drought_type), rows = vars(season_length), space= "fixed") +
  foxes_theme + 
  theme(legend.position = "bottom", 
        plot.margin = margin(t = 0,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 0))
p_mpc_maxM

file_figure <- tempfile(paste("sim", "env", "time_window", 25, "goal_func", 1, sep = "_"), tmpdir = out_folder, fileext = ".png")
ggsave(file_figure, plot = p_mpc_maxM, device = NULL, path = NULL,
       scale = 1, width = 260, height = 180, dpi = 300, limitsize = TRUE,
       units =  "mm")


#### nums of days

goals <- unique(out_simple$goal_func)
time_windows <- unique(out_simple$time_window)
types <-unique(out_simple$drought_type)
lengths <- unique(out_simple$season_length)

analysis <- lapply(time_windows, function(tw){
  return(plyr::rbind.fill(lapply(goals, function(goal){
    return(plyr::rbind.fill(lapply(types, function(type){
      return(plyr::rbind.fill(lapply(lengths, function(l){
        maxM_simple <- subset(out_simple, goal_func == 1 & time_window == tw & drought_type == type & season_length == l)
        maxS_simple <- subset(out_simple, goal_func == 0 & time_window == tw & drought_type == type & season_length == l)
        
        temp = subset(out_simple, goal_func == goal & time_window == tw & drought_type == type & season_length == l)
        
        num_maxM_days <- sum(temp$goal_equiv == 1)
        num_maxS_days <- sum(temp$goal_equiv == 0)
        
        error_M = sum(abs(maxM_simple$biomass - temp$biomass) + abs(maxM_simple$storage - temp$storage))
        error_S = sum(abs(maxS_simple$biomass - temp$biomass) + abs(maxS_simple$storage - temp$storage))
        
        df_out <- data.frame(time_window = tw, 
                             goal_func = goal,
                             drought_type = type, 
                             season_length = l,
                             num_maxM_days = num_maxM_days,
                             num_maxS_days = num_maxS_days,
                             error_M = error_M,
                             error_S = error_S)
        return(df_out)
      })))
    })))
  })))
})

analysis_df <- plyr::rbind.fill(analysis)

analysis_df$error <- analysis_df$error_M - analysis_df$error_S

analysis_df$total_equivalent <- ifelse(analysis_df$error >= 0, "MaxS", "MaxM")

analysis_df <- select(analysis_df, c("time_window", "season_length", "drought_type", "goal_func","num_maxM_days","num_maxS_days", "total_equivalent"))

analysis_df_next <- analysis_df %>%
  dplyr::mutate(percent_maxM = 100 * num_maxM_days/(num_maxM_days + num_maxS_days))

p <- ggplot(analysis_df_next, aes(x = goal_func, y = percent_maxM, 
                                  color = as.factor(time_window),
                                  shape = as.factor(drought_type),
                                  alpha = season_length)) + 
  geom_jitter(height = 0) + 
  scale_alpha_continuous(range = c(0.2, 1)) +
  scale_shape_discrete("Memory (days):") + 
  scale_color_manual("Time Window:",
                     values=foxes_palettes$extra[c(1,3)]) +
  labs(x = expression(paste("Goal Function ", k[f])),
       y = "Percent of MaxM Equivalent days (%)") +
  foxes_theme + 
  theme(legend.position = "bottom")
p

file_figure <- tempfile(paste("summary", "env", "percent_maxm", sep = "_"), tmpdir = out_folder, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 180, height = 120, dpi = 150, limitsize = TRUE,
       units =  "mm")


file_csv <- tempfile(paste("sim", "env", "analysis", sep = "_"), tmpdir = out_folder, fileext = ".csv")
write.csv(analysis_df, file = file_csv)



## UT AND RAIN AND WATER

out_water_alloc <- subset(out_simple, time_window == 25 & drought_type == "regular_drought")
out_water_alloc$season <- ifelse(out_water_alloc$time < out_water_alloc$season_length, yes = "rain", no = "dry")

## Rainfall 

p_rain <- ggplot(out_water_alloc, aes(rainfall, alloc, color = goal_func)) + 
  geom_jitter(alpha = 0.25) + 
  scale_color_gradientn(expression(paste("Goal Function ", k[f], ":")), 
                        colors = foxes_palettes$main[c(2, 4, 3, 1)],
                        breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c("MaxS", "0.25", "0.5", "0.75", "MaxM")) +
  scale_y_continuous(expression(paste("Allocation parameter ", u[t], " gC", g^-1, "Cda", y^-1 )),
                     breaks = c(0, 0.06), labels = c(0, expression(k[s])))+
  scale_x_continuous(expression(paste("Rainfall ", W[t], " kg", H[2], "O da", y^-1))) +
  facet_grid(cols = vars(season), rows = vars(season_length)) +
  foxes_theme + 
  theme(legend.position = "bottom", 
        plot.margin = margin(t = 5,  # Top margin
                             r = 5,  # Right margin
                             b = 5,  # Bottom margin
                             l = 5))
p_rain

file_figure <- tempfile(paste("allocation", "rainfall", "env", "regular", "tw", 25, sep = "_"), tmpdir = out_folder, fileext = ".png")
ggsave(file_figure, plot = p_rain, device = NULL, path = NULL,
       scale = 1, width = 260, height = 180, dpi = 200, limitsize = TRUE,
       units =  "mm")

out_water_alloc <- subset(out_simple, time_window == 25 & drought_type == "strict_drought")
out_water_alloc$season <- ifelse(out_water_alloc$time < out_water_alloc$season_length, yes = "rain", no = "dry")

p_rain <- ggplot(out_water_alloc, aes(rainfall, alloc, color = goal_func)) + 
  geom_jitter(alpha = 0.25) + 
  scale_color_gradientn(expression(paste("Goal Function ", k[f], ":")), 
                        colors = foxes_palettes$main[c(2, 4, 3, 1)],
                        breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c("MaxS", "0.25", "0.5", "0.75", "MaxM")) +
  scale_y_continuous(expression(paste("Allocation parameter ", u[t], " gC", g^-1, "Cda", y^-1 )),
                     breaks = c(0, 0.06), labels = c(0, expression(k[s])))+
  scale_x_continuous(expression(paste("Rainfall ", W[t], " kg", H[2], "O da", y^-1))) +
  facet_grid(cols = vars(season), rows = vars(season_length)) +
  foxes_theme + 
  theme(legend.position = "bottom", 
        plot.margin = margin(t = 5,  # Top margin
                             r = 5,  # Right margin
                             b = 5,  # Bottom margin
                             l = 5))
p_rain

file_figure <- tempfile(paste("allocation", "rainfall", "env", "strict", "tw", 25, sep = "_"), tmpdir = out_folder, fileext = ".png")
ggsave(file_figure, plot = p_rain, device = NULL, path = NULL,
       scale = 1, width = 260, height = 180, dpi = 200, limitsize = TRUE,
       units =  "mm")



#### Allocation over time

out_simple_tw_25 <- subset(out_simple, time_window == 25 & goal_func %in% c(0.65))
out_all <- rbind(out_simple_tw_25)

p <- ggplot(out_all, aes(time, alloc)) + 
  geom_line(color = "#083855") + 
  scale_y_continuous(expression(paste("Allocation parameter ", u[t], " gC", g^-1, "Cda", y^-1 )),
                     breaks = c(0, 0.06), labels = c(0, expression(k[s])))+
  scale_x_continuous("Time (days)") +
  facet_grid(rows = vars(season_length), cols = vars(drought_type), space= "fixed") +
  foxes_theme
p

file_figure <- tempfile(paste("allocation", "time", "environment", sep = "_"), tmpdir = out_folder, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 260, height = 180, dpi = 200, limitsize = TRUE,
       units =  "mm")









