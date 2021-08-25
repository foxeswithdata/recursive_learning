rm(list = ls())

library(tidyverse)
library(cowplot)
library(patchwork)
library(ggnewscale)

source("code/model/plant_class.R")
source("code/model/environment_run.R")
source("code/util/foxes_pallettes.R")
source("code/model/water_model_opt.R")

out_folder <- "out/figures/time_windows"
dir.create(out_folder)


files <- list.files("out/simulations/check_strategy/", pattern = "simulation_memory_k_3")

all_sims <- lapply(files, function(file_name){
  load(file = paste("out/simulations/check_strategy/", file_name, collapse = "", sep = ""))
  return(simulation)
})

out <- lapply(all_sims, function(sim){
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
out_df$memory = 5

out_df <- pivot_longer(out_df, cols = c("biomass", "storage"), names_to = "carbon_pool_type", values_to = "carbon_pool_val")
out_df <- pivot_longer(out_df, cols = c("water"), names_to = "water_pool_type", values_to = "water_pool_val")

out_df_sub <- subset(out_df, goal_func %in% c(0.5, 1))


coeff = 0.2
p_mpc_maxM <- ggplot(out_df_sub, aes(x=time, y=carbon_pool_val)) +
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
  facet_grid(cols = vars(time_window), rows = vars(goal_func), space= "fixed") +
  foxes_theme + 
  theme(legend.position = "bottom", 
        plot.margin = margin(t = 0,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 0))
p_mpc_maxM

file_figure <- tempfile(paste("sim", "timewindow", sep = "_"), tmpdir = out_folder, fileext = ".png")
ggsave(file_figure, plot = p_mpc_maxM, device = NULL, path = NULL,
       scale = 1, width = 260, height = 180, dpi = 300, limitsize = TRUE,
       units =  "mm")

out_df_sub <- subset(out_df, goal_func %in% c(0, 0.5, 0.65, 1))

goals <- unique(out_df_sub$goal_func)
time_windows <- unique(out_df_sub$time_window)
memories <-unique(out_df_sub$memory)

analysis <- lapply(time_windows, function(tw){
  maxM_simple <- subset(out_df_sub, goal_func == 1 & time_window == tw)
  maxS_simple <- subset(out_df_sub, goal_func == 0 & time_window == tw)
  return(plyr::rbind.fill(lapply(goals, function(goal){
    return(plyr::rbind.fill(lapply(memories, function(memory){
      temp = subset(out_df_sub, goal_func == goal & time_window == tw & memory == memory)
      num_maxM_days <- sum(temp$goal_equiv == 1)
      num_maxS_days <- sum(temp$goal_equiv == 0)
      
      error_M = sum(abs(maxM_simple$biomass - temp$biomass) + abs(maxM_simple$storage - temp$storage))
      error_S = sum(abs(maxS_simple$biomass - temp$biomass) + abs(maxS_simple$storage - temp$storage))
      
      df_out <- data.frame(time_window = tw, 
                           goal_func = goal,
                           memory = memory, 
                           num_maxM_days = num_maxM_days,
                           num_maxS_days = num_maxS_days,
                           error_M = error_M,
                           error_S = error_S)
      return(df_out)
    })))
  })))
})

analysis_df <- plyr::rbind.fill(analysis)

analysis_df$error <- analysis_df$error_M - analysis_df$error_S

analysis_df$total_equivalent <- ifelse(analysis_df$error >= 0, "MaxS", "MaxM")

analysis_df <- select(analysis_df, c("time_window","goal_func","num_maxM_days","num_maxS_days", "total_equivalent"))

analysis_df_next <- analysis_df %>%
  dplyr::mutate(percent_maxM = 100 * num_maxM_days/(num_maxM_days + num_maxS_days))

p <- ggplot(analysis_df_next, aes(x = goal_func, y = percent_maxM, 
                                  color = as.factor(time_window))) + 
  geom_jitter() + 
  scale_color_manual("Time Window:",
                     values=foxes_palettes$main[c(1,2,3,4)]) +
  labs(x = expression(paste("Goal Function ", k[f])),
       y = "Percent of MaxM Equivalent days (%)") +
  foxes_theme + 
  theme(legend.position = "bottom")
p

file_figure <- tempfile(paste("summary", "time_window", "percent_maxm", sep = "_"), tmpdir = out_folder, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 180, height = 120, dpi = 150, limitsize = TRUE,
       units =  "mm")

file_csv <- tempfile(paste("sim", "time_window", "analysis", sep = "_"), tmpdir = out_folder, fileext = ".csv")
write.csv(analysis_df, file = file_csv)



#### Allocation parameters

out_water_alloc <- out_df
out_water_alloc$season <- ifelse(out_water_alloc$time < 150, yes = "rain", no="dry")

p_water <- ggplot(out_memory_water_alloc, aes(water, alloc, color = goal_func)) + 
  geom_jitter(alpha = 0.25) + 
  scale_color_gradientn(expression(paste("Goal Function ", k[f], ":")), 
                        colors = foxes_palettes$main[c(2, 4, 3, 1)],
                        breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c("MaxS", "0.25", "0.5", "0.75", "MaxM")) +
  scale_y_continuous(expression(paste("Allocation parameter ", u[t], " gC", g^-1, "Cda", y^-1 )),
                     breaks = c(0, 0.06), labels = c(0, expression(k[s])))+
  scale_x_continuous(expression(paste("Water Availability ", W[t], " kg", H[2], "O")))+
  facet_grid(cols = vars(season)) +
  foxes_theme + 
  theme(legend.position = "bottom", 
        plot.margin = margin(t = 5,  # Top margin
                             r = 5,  # Right margin
                             b = 5,  # Bottom margin
                             l = 5))
p_water

file_figure <- tempfile(paste("allocation", "water", "tw", 25, sep = "_"), tmpdir = out_folder, fileext = ".png")
ggsave(file_figure, plot = p_water, device = NULL, path = NULL,
       scale = 1, width = 260, height = 180, dpi = 200, limitsize = TRUE,
       units =  "mm")

p <- ggplot(out_sub_rain_season, aes(water, alloc, color = goal_func)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_gradientn(expression(paste("Goal Function ", k[f], ":")), 
                        colors = foxes_palettes$main[c(2, 4, 3, 1)],
                        breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c("MaxS", "0.25", "0.5", "0.75", "MaxM")) +
  scale_y_continuous(expression(paste("Allocation parameter ", u[t], " gC", g^-1, "Cda", y^-1 )),
                     breaks = c(0, 0.06), labels = c(0, expression(k[s])))+
  scale_x_continuous(expression(paste("Water Availability ", W[t], " kg", H[2], "O")))+
  foxes_theme 
p

file_figure <- tempfile(paste("allocation", "water", "for_legend", "tw", 25, sep = "_"), tmpdir = out_folder, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 260, height = 180, dpi = 200, limitsize = TRUE,
       units =  "mm")

## Rainfall 

p_rain <- ggplot(out_water_alloc, aes(rainfall, alloc, color = goal_func)) + 
  geom_jitter(alpha = 0.25) + 
  scale_color_gradientn(expression(paste("Goal Function ", k[f], ":")), 
                        colors = foxes_palettes$main[c(2, 4, 3, 1)],
                        breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c("MaxS", "0.25", "0.5", "0.75", "MaxM")) +
  scale_y_continuous(expression(paste("Allocation parameter ", u[t], " gC", g^-1, "Cda", y^-1 )),
                     breaks = c(0, 0.06), labels = c(0, expression(k[s])))+
  scale_x_continuous(expression(paste("Rainfall ", W[t], " kg", H[2], "O da", y^-1))) +
  facet_grid(cols = vars(season), rows = vars(time_window)) +
  foxes_theme + 
  theme(legend.position = "bottom", 
        plot.margin = margin(t = 5,  # Top margin
                             r = 5,  # Right margin
                             b = 5,  # Bottom margin
                             l = 5))
p_rain

file_figure <- tempfile(paste("allocation", "rainfall", "timewindow", "tw", sep = "_"), tmpdir = out_folder, fileext = ".png")
ggsave(file_figure, plot = p_rain, device = NULL, path = NULL,
       scale = 1, width = 260, height = 180, dpi = 200, limitsize = TRUE,
       units =  "mm")

#### Allocation over time

out_memory_tw_25_1 <- subset(out_memory, time_window == 25 & goal_func %in% c(1))
out_simple_tw_25_1 <- subset(out_simple, time_window == 25 & goal_func %in% c(1))
out_memory_tw_25_55 <- subset(out_memory, time_window == 25 & goal_func %in% c(0.55))
out_simple_tw_25_55 <- subset(out_simple, time_window == 25 & goal_func %in% c(0.55))
out_all <- rbind(out_memory_tw_25, out_simple_tw_25)

p <- ggplot(out_df_sub, aes(time, alloc)) + 
  geom_line(color = "#083855") + 
  scale_y_continuous(expression(paste("Allocation parameter ", u[t], " gC", g^-1, "Cda", y^-1 )),
                     breaks = c(0, 0.06), labels = c(0, expression(k[s])))+
  scale_x_continuous("Time (days)") +
  facet_grid(rows = vars(goal_func), cols = vars(time_window), space= "fixed") +
  foxes_theme
p

file_figure <- tempfile(paste("allocation", "time", sep = "_"), tmpdir = out_folder, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 260, height = 180, dpi = 200, limitsize = TRUE,
       units =  "mm")


num_alloc_days <- out_df_sub %>% 
  dplyr::group_by(time_window, goal_func) %>%
  dplyr::summarise(num_alloc_days = sum(alloc > 0.01))


file_csv <- tempfile(paste("num", "alloc", "days", "time_window", sep = "_"), tmpdir = out_folder, fileext = ".csv")
write.csv(num_alloc_days, file = file_csv)


out_df_sub <- subset(out_df, goal_func %in% c(0.5, 1))

out_mpc_sums <- out_df_sub %>% 
  dplyr::group_by(goal_func, time_window) %>%
  dplyr::summarise(all_s = sum(storage)/max(time),
                   all_m = sum(biomass)/max(time)) %>%
  dplyr::mutate(out_goal = all_m * goal_func + all_s * (1-goal_func))

out_mpc_sums$type = paste0("MPC", out_mpc_sums$time_window)

out_all_sums <- out_mpc_sums
out_all_sums <- select(out_all_sums, c("goal_func", "type", "out_goal"))
out_all_sums <- pivot_wider(out_all_sums, names_from = type, values_from = out_goal)
View(out_all_sums)

out_mpc_final <- out_df_sub %>% 
  filter(time == max(time)) %>% 
  dplyr::group_by(goal_func, time_window) %>%
  dplyr::summarise(s = storage,
                   m = biomass) %>%
  dplyr::mutate(out_goal = m * goal_func + s * (1-goal_func))

out_mpc_final$type = paste0("MPC", out_mpc_final$time_window)

out_all_final <- out_mpc_final
out_all_final <- select(out_all_final, c("goal_func", "type", "out_goal"))
out_all_final <- pivot_wider(out_all_final, names_from = type, values_from = out_goal)
View(out_all_final)

file_csv <- tempfile(paste("sim", "time_window", "all", "final_val", "analysis", sep = "_"), tmpdir = out_folder, fileext = ".csv")
write.csv(out_all_final, file = file_csv)

file_csv <- tempfile(paste("sim", "time_window", "all", "sum", "analysis", sep = "_"), tmpdir = out_folder, fileext = ".csv")
write.csv(out_all_sums, file = file_csv)










