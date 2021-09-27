rm(list = ls())

library(tidyverse)
library(cowplot)
library(patchwork)
library(ggnewscale)
library(gridExtra)

source("code/model/plant_class.R")
source("code/model/environment_run.R")
source("code/util/foxes_pallettes.R")
source("code/model/water_model_opt.R")

out_folder <- "out/figures/fitness_new"
dir.create(out_folder)

files <- list.files("out/simulations/check_strategy/", pattern = "simulation_k_")

all_sims <- lapply(files, function(file_name){
  load(file = paste("out/simulations/check_strategy/", file_name, collapse = "", sep = ""))
  simulation$k = str_extract(file_name, "[:digit:]")
  simulation$memory = 0;
  return(simulation)
})

sim_test <- all_sims[[3]]

files_mem <- list.files("out/simulations/check_strategy/", pattern = "simulation_memory_k_")

all_sims_mem <- lapply(files_mem, function(file_name){
  load(file = paste("out/simulations/check_strategy/", file_name, collapse = "", sep = ""))
  simulation$k = str_extract(file_name, "[:digit:]")
  simulation$memory = 5;
  return(simulation)
})

all_sims_full <- c(all_sims, all_sims_mem)

out <- lapply(all_sims_full, function(sim){
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
  data$k = sim$k
  
  data$goal_equiv = sapply(sim$predicted, function(pred){
    return(which.min(c(sum(abs(pred$ut - pred$ut.maxS)), sum(abs(pred$ut - pred$ut.maxM)))) - 1)
  })
  data$goal_maxS_mean_err_sq = sapply(sim$predicted, function(pred){
    return(mean(pred$ut - pred$ut.maxS)^2)
  })
  data$goal_maxM_mean_err_sq = sapply(sim$predicted, function(pred){
    return(mean(pred$ut - pred$ut.maxM)^2)
  })
  
  return(data)
})

out_df <- plyr::rbind.fill(out)

# out_df <- pivot_longer(out_df, cols = c("biomass", "storage"), names_to = "carbon_pool_type", values_to = "carbon_pool_val")
# out_df <- pivot_longer(out_df, cols = c("water"), names_to = "water_pool_type", values_to = "water_pool_val")

rainfall_sims <- unique(out_df$k)
goals <- unique(out_df$goal_func)
memories <- unique(out_df$memory)
time_windows <- unique(out_df$time_window)

analysis <- lapply(rainfall_sims, function(sim_num){
  return(plyr::rbind.fill(lapply(memories, function(m){
  return(plyr::rbind.fill(lapply(time_windows, function(tw){
    maxM_simple <- subset(out_df, goal_func == 1 & time_window == tw & k == sim_num & memory == m)
    maxS_simple <- subset(out_df, goal_func == 0 & time_window == tw & k == sim_num & memory == m)
    return(plyr::rbind.fill(lapply(goals, function(goal){
      temp = subset(out_df, goal_func == goal & time_window == tw & k == sim_num & memory == m)
      num_maxM_days <- sum(temp$goal_equiv == 1)
      num_maxS_days <- sum(temp$goal_equiv == 0)
      
      error_M = sum(abs(maxM_simple$biomass - temp$biomass) + abs(maxM_simple$storage - temp$storage))
      error_S = sum(abs(maxS_simple$biomass - temp$biomass) + abs(maxS_simple$storage - temp$storage))
      
      df_out <- data.frame(time_window = tw, 
                           goal_func = goal,
                           num_maxM_days = num_maxM_days,
                           num_maxS_days = num_maxS_days,
                           error_M = error_M,
                           error_S = error_S, 
                           k = sim_num,
                           memory = m)
      return(df_out)
    })))
  })))
  })))
})

goals <- c(0, 0.45, 0.5, 0.55, 0.6, 0.65, 0.75, 1)

analysis_df <- plyr::rbind.fill(analysis)

analysis_df$error <- analysis_df$error_M - analysis_df$error_S

analysis_df$total_equivalent <- ifelse(analysis_df$error >= 0, "MaxS", "MaxM")

analysis_df <- select(analysis_df, c("time_window","goal_func","num_maxM_days","memory", "num_maxS_days", "total_equivalent", "k"))

analysis_df_next <- analysis_df %>%
  dplyr::filter(k == 1 & memory == 0 & time_window %in% c(10,25)) %>%
  dplyr::mutate(percent_maxM = 100 * num_maxM_days/(num_maxM_days + num_maxS_days)) %>%
  dplyr::group_by(time_window, goal_func, memory) %>%
  dplyr::summarise(percent_maxM_ave = mean(percent_maxM),
                percent_maxM_sd = sd(percent_maxM))

p_full_scope <- ggplot(analysis_df_next, aes(x = goal_func, 
                                  y = percent_maxM_ave, 
                                  color = as.factor(time_window))) + 
  geom_point() + 
  scale_color_manual("Time Window:",
                     values=foxes_palettes$extra[c(1,3)]) +
  labs(x = expression(paste("Goal Function ", k[f])),
       y = "Percent of MaxM Equivalent days (%)") +
  foxes_theme + 
  theme(legend.position = "bottom")
p_full_scope

file_figure <- tempfile(paste("summary", "no_mem", "percent_maxm", sep = "_"), tmpdir = out_folder, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 180, height = 120, dpi = 150, limitsize = TRUE,
       units =  "mm")

file_csv <- tempfile(paste("sim", "fitness", "no", "memory", "analysis", sep = "_"), tmpdir = out_folder, fileext = ".csv")
write.csv(analysis_df, file = file_csv)

### Plot differences between time window and Percentage of MaxM equivalent days at "ledge"

analysis_df_next <- analysis_df %>%
  dplyr::filter(goal_func %in% c(0.55, 0.6, 0.65, 0.75)) %>%
  dplyr::mutate(percent_maxM = 100 * num_maxM_days/(num_maxM_days + num_maxS_days)) %>%
  dplyr::group_by(time_window, memory) %>%
  dplyr::summarise(percent_maxM_ave = mean(percent_maxM),
                   percent_maxM_sd = sd(percent_maxM))


p_summary <- ggplot(analysis_df_next, aes(x = time_window, 
                                  y = percent_maxM_ave, 
                                  fill = as.factor(memory))) + 
  geom_bar(colour="#FFFFFF", 
           position="dodge", stat="identity") + 
  geom_errorbar(aes(ymin = percent_maxM_ave - percent_maxM_sd, ymax = percent_maxM_ave + percent_maxM_sd),
                colour = foxes_palettes$dark[5], position="dodge") +
  scale_fill_manual(expression("Memory Window ", t[m], " (d)"),
                    values = foxes_palettes$main[c(1,2)]) +
  labs(x = expression(paste("Time Window ", t[w], " (d)")),
       y = "Percent of MaxM Equivalent days (%)") +
  foxes_theme + 
  theme(legend.position = "bottom")
p_summary

analysis_df_aov <- analysis_df %>%
  dplyr::filter(goal_func %in% c(0.55, 0.6, 0.65, 0.75)) %>%
  dplyr::mutate(percent_maxM = 100 * num_maxM_days/(num_maxM_days + num_maxS_days))

analysis_aov <- aov(percent_maxM ~ (as.factor(time_window) * as.factor(memory)), data=analysis_df_aov)
summary(analysis_aov)
TukeyHSD(analysis_aov)

HSD.test(analysis_aov, "as.factor(time_window)", console=TRUE)
HSD.test(analysis_aov, "as.factor(memory)", console=TRUE)

analysis_aov <- aov(percent_maxM ~ (as.factor(time_window) + as.factor(memory)), data=analysis_df_aov)
summary(analysis_aov)
TukeyHSD(analysis_aov)

library(agricolae)
HSD.test(analysis_aov, "as.factor(time_window)", console=TRUE)
HSD.test(analysis_aov, "as.factor(memory)", console=TRUE)

p <- plot_grid(p_full_scope, p_summary, labels = c('A', 'B'), label_size = 14, 
               rel_widths = c(3,2),
               label_fontfamily = "Lato Light",
               label_fontface = "bold",
               label_colour = "#083855")
p

file_figure <- tempfile(paste("maxm_figure_full", sep = "_"), tmpdir = out_folder, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 220, height = 140, dpi = 300, limitsize = TRUE,
       units =  "mm")


### Plot results for all of the simulations

## load values: 
source("code/process_data_opt.R")
source("code/optimal_output_from_matlab.R")

out_mpc <- out_df %>%
  dplyr::filter(goal_func %in% c(0, 0.45, 0.5, 0.55, 0.6, 0.65, 0.75, 1))

out_optim <- pivot_wider(out_optim, names_from = carbon_pool_type, values_from = carbon_pool_val)
cols_out_optims <- colnames(out_optim)
cols_out_optims[cols_out_optims == "goal"] = "goal_func"
cols_out_optims[cols_out_optims == "Biomass"] = "biomass"
cols_out_optims[cols_out_optims == "Storage"] = "storage"
cols_out_optims[cols_out_optims == "water_pool_val"] = "water"
colnames(out_optim) <- cols_out_optims

out_matlab <- out_matlab %>%
  dplyr::filter(time <= 249)

out_optim$type = "OCT"
out_mpc$type = "MPC"
out_matlab$type = "Brute-Force"
View(out_matlab)
out_all <- plyr::rbind.fill(out_optim, out_mpc, out_matlab)

out_all_sums <- out_all %>% 
  dplyr::group_by(goal_func, time_window, memory, k, type) %>%
  dplyr::summarise(all_s = sum(storage)/max(time),
                   all_m = sum(biomass)/max(time)) %>%
  dplyr::mutate(out_goal = all_m * goal_func + all_s * (1-goal_func)) %>%
  dplyr::group_by(goal_func, time_window, memory, type) %>%
  dplyr::summarise(out_goal_ave = mean(out_goal, na.rm= TRUE),
                   out_goal_sd = sd(out_goal, na.rm= TRUE),
                   out_s_ave = mean(all_s, na.rm= TRUE),
                   out_s_sd = sd(all_s, na.rm= TRUE),
                   out_m_ave = mean(all_m, na.rm= TRUE),
                   out_m_sd = sd(all_m, na.rm= TRUE)) 

out_all_sums$type_det <- paste0(out_all_sums$type, "mem", out_all_sums$memory, "tw", out_all_sums$time_window)
out_all_sums$type <- paste0(out_all_sums$type, "mem", out_all_sums$memory)
out_all_sums$type <- factor(out_all_sums$type, levels=c('OCTmemNA', 'Brute-ForcememNA', 'MPCmem0', 'MPCmem5'))

View(out_all_sums)

out_all_final <- out_all %>% 
  filter(time == max(time)) %>% 
  dplyr::group_by(goal_func, time_window, memory, k, type) %>%
  dplyr::summarise(s = storage,
                   m = biomass) %>%
  dplyr::mutate(out_goal = m * goal_func + s * (1-goal_func),
                storage_buffer = s / (m+s)) %>%
  dplyr::group_by(goal_func, time_window, memory, type) %>%
  dplyr::summarise(out_goal_ave =  mean(out_goal, na.rm= TRUE),
                   out_goal_sd = sd(out_goal, na.rm= TRUE),
                   out_s_ave = mean(s, na.rm= TRUE),
                   out_s_sd = sd(s, na.rm= TRUE),
                   out_m_ave = mean(m, na.rm= TRUE),
                   out_m_sd = sd(m, na.rm= TRUE),
                   storage_buff_ave = mean(storage_buffer, na.rm= TRUE),
                   storage_buff_sd = sd(storage_buffer, na.rm= TRUE))

out_all_final$type_det <- paste0(out_all_final$type, "mem", out_all_final$memory, "tw", out_all_sums$time_window)
out_all_final$type <- paste0(out_all_final$type, "mem", out_all_final$memory)
out_all_final$type <- factor(out_all_final$type, levels=c('OCTmemNA', 'Brute-ForcememNA', 'MPCmem0', 'MPCmem5'))

out_all_sums_sub <- out_all_sums %>%
  dplyr::filter(goal_func %in% c(0, 0.65, 0.75, 1))
out_all_sums_sub$goal_func <- factor(out_all_sums_sub$goal_func)
out_all_sums_sub$goal_func <- plyr::revalue(out_all_sums_sub$goal_func, c("0"="MaxS", "0.65"="kf=0.65", "0.75"="kf=0.75", "1"="MaxM"))

out_all_final_sub <- out_all_final %>%
  dplyr::filter(goal_func %in% c(0, 0.65, 0.75, 1))
out_all_final_sub$goal_func <- factor(out_all_final_sub$goal_func)
out_all_final_sub$goal_func <- plyr::revalue(out_all_final_sub$goal_func, c("0"="MaxS", "0.65"="kf=0.65", "0.75"="kf=0.75", "1"="MaxM"))

p_sums <- ggplot(out_all_sums_sub, aes(fill=type, 
                              y=out_goal_ave, 
                              x=as.factor(type), 
                              group = type_det, 
                              alpha = time_window)) + 
  geom_bar(position="dodge", stat="identity", colour = "#ffffff") + 
  geom_errorbar(aes(ymin = out_goal_ave - out_goal_sd, ymax = out_goal_ave + out_goal_sd), position="dodge") +
  scale_fill_manual("Model Type", 
                    labels = c("OCT", "Brute-Force", 
                               "MPC \nNo Memory", 
                               "MPC \n5d Memory"),
                    values = foxes_palettes$main[c(5,4,1,2)]) + 
  scale_alpha_continuous("Time Window\n(d)", range = c(0.5, 1)) +
  labs(x = expression(paste(" ")),
       y = "Day-Normalized Fitness Output (kgC)") +
  scale_x_discrete(labels = c("OCT", "Brute-Force", 
                              "MPC No Memory", 
                              "MPC 5d Memory")) +
  facet_wrap(vars(goal_func), nrow = 1, scales = "free_x") +
  foxes_theme + 
  guides(fill = "none") + 
  theme(legend.position =  "right", axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(t = 10,  # Top margin
                             r = 10,  # Right margin
                             b = 10,  # Bottom margin
                             l = 10))
p_sums

p_final <- ggplot(out_all_final_sub, aes(fill=type, 
                                  y=out_goal_ave, 
                                  x=as.factor(type), 
                                  group = type_det, 
                                  alpha = time_window)) + 
  geom_bar(position="dodge", stat="identity", colour = "#ffffff") + 
  geom_errorbar(aes(ymin = out_goal_ave - out_goal_sd, ymax = out_goal_ave + out_goal_sd), position="dodge") +
  scale_fill_manual("Model Type", 
                    labels = c("OCT", "Brute-Force", 
                               "MPC \nNo Memory", 
                               "MPC \n5d Memory"),
                    values = foxes_palettes$main[c(5,4,1,2)]) + 
  scale_alpha_continuous("Time Window\n(d)", range = c(0.5, 1)) +
  labs(x = expression(paste("Model Type ")),
       y = "Fitness Output (kgC)") +
  scale_x_discrete(labels = c("OCT", "Brute-Force", 
                              "MPC No Memory", 
                              "MPC 5d Memory")) +
  facet_wrap(vars(goal_func), nrow = 1, scales = "free_x") +
  foxes_theme + 
  guides(alpha = "none") +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(t = 10,  # Top margin
                             r = 10,  # Right margin
                             b = 10,  # Bottom margin
                             l = 10))
p_final

p <- plot_grid(p_sums,p_final, labels = c('A', 'B'), label_size = 14, 
               nrow = 2, 
               label_fontfamily = "Lato Light",
               label_fontface = "bold",
               label_colour = "#083855")
p

file_figure <- tempfile(paste("fitnessoutputs", sep = "_"), tmpdir = out_folder, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 330, height = 200, dpi = 300, limitsize = TRUE,
       units =  "mm")
 





p_sums <- ggplot(out_all_sums_sub, aes(fill=type, 
                                       y=out_m_ave, 
                                       x=as.factor(type), 
                                       group = type_det, 
                                       alpha = time_window)) + 
  geom_bar(position="dodge", stat="identity", colour = "#ffffff") + 
  geom_errorbar(aes(ymin = out_m_ave - out_m_sd, ymax = out_m_ave + out_m_sd), position="dodge") +
  scale_fill_manual("Model Type", 
                    labels = c("OCT", "Brute-Force", 
                               "MPC \nNo Memory", 
                               "MPC \n5d Memory"),
                    values = foxes_palettes$main[c(5,4,1,2)]) + 
  scale_alpha_continuous("Time Window\n(d)", range = c(0.5, 1)) +
  labs(x = expression(paste(" ")),
       y = "Average Daily Biomass Size (kgC)") +
  scale_x_discrete(labels = c("OCT", "Brute-Force", 
                              "MPC No Memory", 
                              "MPC 5d Memory")) +
  facet_wrap(vars(goal_func), nrow = 1, scales = "free_x") +
  foxes_theme + 
  guides(fill = "none") + 
  theme(legend.position =  "right", axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(t = 10,  # Top margin
                             r = 10,  # Right margin
                             b = 10,  # Bottom margin
                             l = 10))
p_sums

p_final <- ggplot(out_all_final_sub, aes(fill=type, 
                                         y=out_m_ave, 
                                         x=as.factor(type), 
                                         group = type_det, 
                                         alpha = time_window)) + 
  geom_bar(position="dodge", stat="identity", colour = "#ffffff") + 
  geom_errorbar(aes(ymin = out_m_ave - out_m_sd, ymax = out_m_ave + out_m_sd), position="dodge") +
  scale_fill_manual("Model Type", 
                    labels = c("OCT", "Brute-Force", 
                               "MPC \nNo Memory", 
                               "MPC \n5d Memory"),
                    values = foxes_palettes$main[c(5,4,1,2)]) + 
  scale_alpha_continuous("Time Window\n(d)", range = c(0.5, 1)) +
  labs(x = expression(paste("Model Type ")),
       y = "Final Biomass Size (kgC)") +
  scale_x_discrete(labels = c("OCT", "Brute-Force", 
                              "MPC No Memory", 
                              "MPC 5d Memory")) +
  facet_wrap(vars(goal_func), nrow = 1, scales = "free_x") +
  foxes_theme + 
  guides(alpha = "none") +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(t = 10,  # Top margin
                             r = 10,  # Right margin
                             b = 10,  # Bottom margin
                             l = 10))
p_final

p <- plot_grid(p_sums,p_final, labels = c('A', 'B'), label_size = 14, 
               nrow = 2, 
               label_fontfamily = "Lato Light",
               label_fontface = "bold",
               label_colour = "#083855")
p

file_figure <- tempfile(paste("biomass_output", sep = "_"), tmpdir = out_folder, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 330, height = 200, dpi = 300, limitsize = TRUE,
       units =  "mm")






p_sums <- ggplot(out_all_sums_sub, aes(fill=type, 
                                       y=out_s_ave, 
                                       x=as.factor(type), 
                                       group = type_det, 
                                       alpha = time_window)) + 
  geom_bar(position="dodge", stat="identity", colour = "#ffffff") + 
  geom_errorbar(aes(ymin = out_s_ave - out_s_sd, ymax = out_s_ave + out_s_sd), position="dodge") +
  scale_fill_manual("Model Type", 
                    labels = c("OCT", "Brute-Force", 
                               "MPC \nNo Memory", 
                               "MPC \n5d Memory"),
                    values = foxes_palettes$main[c(5,4,1,2)]) + 
  scale_alpha_continuous("Time Window\n(d)", range = c(0.5, 1)) +
  labs(x = expression(paste(" ")),
       y = "Average Daily Storage Size (kgC)") +
  scale_x_discrete(labels = c("OCT", "Brute-Force", 
                              "MPC No Memory", 
                              "MPC 5d Memory")) +
  facet_wrap(vars(goal_func), nrow = 1, scales = "free_x") +
  foxes_theme + 
  guides(fill = "none") + 
  theme(legend.position =  "right", axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(t = 10,  # Top margin
                             r = 10,  # Right margin
                             b = 10,  # Bottom margin
                             l = 10))
p_sums

p_final <- ggplot(out_all_final_sub, aes(fill=type, 
                                         y=out_s_ave, 
                                         x=as.factor(type), 
                                         group = type_det, 
                                         alpha = time_window)) + 
  geom_bar(position="dodge", stat="identity", colour = "#ffffff") + 
  geom_errorbar(aes(ymin = out_s_ave - out_s_sd, ymax = out_s_ave + out_s_sd), position="dodge") +
  scale_fill_manual("Model Type", 
                    labels = c("OCT", "Brute-Force", 
                               "MPC \nNo Memory", 
                               "MPC \n5d Memory"),
                    values = foxes_palettes$main[c(5,4,1,2)]) + 
  scale_alpha_continuous("Time Window\n(d)", range = c(0.5, 1)) +
  labs(x = expression(paste("Model Type ")),
       y = "Final Storage Size (kgC)") +
  scale_x_discrete(labels = c("OCT", "Brute-Force", 
                              "MPC No Memory", 
                              "MPC 5d Memory")) +
  facet_wrap(vars(goal_func), nrow = 1, scales = "free_x") +
  foxes_theme + 
  guides(alpha = "none") +
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(t = 10,  # Top margin
                             r = 10,  # Right margin
                             b = 10,  # Bottom margin
                             l = 10))
p_final

p <- plot_grid(p_sums,p_final, labels = c('A', 'B'), label_size = 14, 
               nrow = 2, 
               label_fontfamily = "Lato Light",
               label_fontface = "bold",
               label_colour = "#083855")
p

file_figure <- tempfile(paste("storage_output", sep = "_"), tmpdir = out_folder, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 330, height = 200, dpi = 300, limitsize = TRUE,
       units =  "mm")



out_all_final_sub <- out_all_final_sub %>%
  dplyr::filter(goal_func == "MaxM" & type %in% c("MPCmem0", "MPCmem5"))

p_final <- ggplot(out_all_final_sub, aes(fill=type, 
                                         y=storage_buff_ave, 
                                         x=as.factor(type), 
                                         group = type_det, 
                                         alpha = time_window)) + 
  geom_bar(position="dodge", stat="identity", colour = "#ffffff") + 
  geom_errorbar(aes(ymin = storage_buff_ave - storage_buff_sd, ymax = storage_buff_ave + storage_buff_sd), position="dodge") +
  scale_fill_manual("Model Type", 
                    labels = c("MPC \nNo Memory", 
                               "MPC \n5d Memory"),
                    values = foxes_palettes$main[c(1,2)]) + 
  scale_alpha_continuous("Time Window\n(d)", range = c(0.5, 1)) +
  labs(x = expression(paste("Model Type ")),
       y = "Final Storage Concentration (kgC/kgC)") +
  scale_x_discrete(labels = c("MPC No Memory", 
                              "MPC 5d Memory")) +
  foxes_theme + 
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(t = 10,  # Top margin
                             r = 10,  # Right margin
                             b = 10,  # Bottom margin
                             l = 10))
p_final

file_figure <- tempfile(paste("storage_buffer", sep = "_"), tmpdir = out_folder, fileext = ".png")
ggsave(file_figure, plot = p_final, device = NULL, path = NULL,
       scale = 1, width = 260, height = 180, dpi = 300, limitsize = TRUE,
       units =  "mm")

#### Evaluate Ut Values

out_mpc <- out_df %>%
  dplyr::filter(goal_func %in% c(0,0.65,1) & memory == 5 & time_window == 25 & k == 3)
out_optim <- out_optim[,c(1:7,9:12)]
out_opt = out_optim %>%
  dplyr::filter(goal_func %in% c(0,0.65,1) & k == 3)
out_matlab_new = out_matlab %>%
  dplyr::filter(goal_func %in% c(0,0.65,1) & k == 3)

out_opt_update <- out_opt %>%
  group_by(goal_func) %>%
  mutate(alloc = ifelse(time <= ts, 0.06, 0))


out_opt$type = "OCT"
out_mpc$type = "MPC"
out_matlab_new$type = "Brute-Force"

out_all <- plyr::rbind.fill(out_opt_update, out_mpc, out_matlab_new)
out_all$type <- factor(out_all$type, levels = c("OCT", "Brute-Force", "MPC"))


p_ut_group <- ggplot(out_all, aes(y=alloc,
                                  x=time,
                                  group = k)) + 
  geom_line(alpha=0.85, colour = "#083855") + 
  labs(x = "Time (d)",
       y = expression(paste("Allocation Parameter ", u[t], " (kgCk", g^-1, "C)" ))) +
  facet_grid(type~goal_func, labeller = labeller(goal_func = c("0" = "MaxS",
                                                               "0.65" = "0.65",
                                                               "1" = "MaxM"))) +
  foxes_theme + 
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(t = 10,  # Top margin
                             r = 10,  # Right margin
                             b = 10,  # Bottom margin
                             l = 10))
p_ut_group

file_figure <- tempfile(paste("ut_models", sep = "_"), tmpdir = out_folder, fileext = ".png")
ggsave(file_figure, plot = p_ut_group, device = NULL, path = NULL,
       scale = 1, width = 260, height = 180, dpi = 300, limitsize = TRUE,
       units =  "mm")

out_mpc <- out_df %>%
  dplyr::filter(goal_func %in% c(1) & k == 4)

p_ut_group <- ggplot(out_mpc, aes(y=alloc,
                                  x=time,
                                  group = k)) + 
  geom_line(alpha=0.85, colour = "#083855") + 
  labs(x = "Time (d)",
       y = expression(paste("Allocation Parameter ", u[t], " (kgCk", g^-1, "C)" ))) +
  facet_grid(time_window~memory, labeller = labeller(memory = c("0" = "No Memory",
                                                                "5" = "5 day Memory"),
                                                     time_window = c("10" = "tw=10d",
                                                                     "15" = "tw=15d",
                                                                     "20" = "tw=20d",
                                                                     "25" = "tw=25d"))) +
  foxes_theme + 
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(t = 10,  # Top margin
                             r = 10,  # Right margin
                             b = 10,  # Bottom margin
                             l = 10))
p_ut_group

file_figure <- tempfile(paste("ut_mpc", sep = "_"), tmpdir = out_folder, fileext = ".png")
ggsave(file_figure, plot = p_ut_group, device = NULL, path = NULL,
       scale = 1, width = 260, height = 180, dpi = 300, limitsize = TRUE,
       units =  "mm")

### percentage storage of total biomass 

out_all_st <- out_all %>% 
  dplyr::filter(goal_func == 1) %>%
  dplyr::group_by(k, type, time_window, memory, time) %>%
  dplyr::summarise(st_perc = storage / (biomass+storage)) %>%
  dplyr::group_by(k, type, time_window, memory) %>%
  dplyr::summarise(st_min = min(st_perc) * 100,
                   st_max = max(st_perc) * 100)

View(out_all_st)
out_all_st_mpc <- out_all_st %>% filter(type == "MPC")
