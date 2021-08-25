rm(list = ls())

library(tidyverse)
library(cowplot)
library(patchwork)
library(ggnewscale)

source("code/model/plant_class.R")
source("code/model/environment_run.R")
source("code/util/foxes_pallettes.R")
source("code/model/water_model_opt.R")

out_folder <- "out/figures/fitness_new"
dir.create(out_folder)

files <- list.files("out/simulations/check_strategy/", pattern = "simulation_k_")

all_sims <- lapply(files, function(file_name){
  load(file = paste("out/simulations/check_strategy/", file_name, collapse = "", sep = ""))
  return(simulation)
})

sim_test <- all_sims[[3]]


W0 = 50
rain_df <- data.frame(rainfall = sim_test$rainfall, time = 1:length(sim_test$rainfall))
rain_df$rainfall[1] = W0
cumul_df = rain_df[2:(nrow(rain_df)),]
current_rain = W0
for(i in 1:nrow(cumul_df)){
  current_rain = cumul_df$rainfall[i] + current_rain
  cumul_df$rainfall[i] = current_rain
}

p1 <- ggplot(rain_df, aes(x=time, y=rainfall)) +
  geom_bar(stat = "identity", fill=foxes_palettes$light[5]) +
  scale_y_continuous(
    name =  "Water Input [kgH20/plant]"
  ) +
  scale_x_continuous("Time [day]") +
  foxes_theme
p1


p2 <- ggplot(cumul_df, aes(x=time, y=rainfall)) +
  geom_line(color=foxes_palettes$light[5]) +
  scale_y_continuous(
    name =  "Cumulative Water Availability [kgH20/plant]"
  ) +
  scale_x_continuous("Time [day]") +
  foxes_theme
p2

p <- plot_grid(p1, p2, labels = c('A', 'B'), label_size = 14, 
               label_fontfamily = "Lato Light",
               label_fontface = "bold",
               label_colour = "#083855")
p

file_figure <- tempfile(paste("rainfall", sep = "_"), tmpdir = out_folder, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 220, height = 140, dpi = 300, limitsize = TRUE,
       units =  "mm")




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
  data$goal_maxS_mean_err_sq = sapply(sim$predicted, function(pred){
    return(mean(pred$ut - pred$ut.maxS)^2)
  })
  data$goal_maxM_mean_err_sq = sapply(sim$predicted, function(pred){
    return(mean(pred$ut - pred$ut.maxM)^2)
  })
  
  return(data)
})

out_df <- plyr::rbind.fill(out)

out_df <- pivot_longer(out_df, cols = c("biomass", "storage"), names_to = "carbon_pool_type", values_to = "carbon_pool_val")
out_df <- pivot_longer(out_df, cols = c("water"), names_to = "water_pool_type", values_to = "water_pool_val")

out_no_mem_maxM <- subset(out, goal_func == 1 & time_window == 25)
out_no_mem_maxS <- subset(out, goal_func == 0 & time_window == 25)
out_no_mem_max45 <- subset(out, goal_func == 0.45 & time_window == 25)
out_no_mem_max65 <- subset(out, goal_func == 0.65 & time_window == 25)

coeff = 0.4
p_mpc_maxM <- ggplot(out_no_mem_maxM, aes(x=time, y=carbon_pool_val)) +
  geom_hline(yintercept=0, color="#083855", size = 0.5,linetype="dashed") + 
  geom_line(mapping=aes(y=water_pool_val/coeff, fill = water_pool_type), 
            size=1, linetype="longdash", alpha=0.5, color=foxes_palettes$main[5]) + 
  geom_line(mapping=aes(colour=carbon_pool_type), size = 1, show.legend=TRUE) + 
  scale_y_continuous("Carbon Pool Size (gC)", 
                     sec.axis = sec_axis(~.*coeff, name = NULL)) +
  scale_color_manual("Carbon Pool:",
                     values=foxes_palette(n=2, name="main"),
                     guide=guide_legend(order=1)) +
  scale_fill_manual(name  ="Water Pool:", values = rep(1, 3),
                    guide=guide_legend(override.aes = list(colour=foxes_palettes$main[5], alpha = 0.5, linetype = "longdash") , order=2),
                    labels=c("Available Soil Water")) +
  labs(x = "Time (day)") +
  foxes_theme + 
  theme(legend.position = "none", 
        plot.margin = margin(t = 0,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 0))
p_mpc_maxM

p_mpc_maxS <- ggplot(out_no_mem_maxS, aes(x=time, y=carbon_pool_val)) +
  geom_hline(yintercept=0, color="#083855", size = 0.5,linetype="dashed") + 
  geom_line(mapping=aes(y=water_pool_val/coeff, fill = water_pool_type), 
            size=1, linetype="longdash", alpha=0.5, color=foxes_palettes$main[5]) + 
  geom_line(mapping=aes(colour=carbon_pool_type), size = 1, show.legend=TRUE) + 
  scale_y_continuous("Carbon Pool Size (gC)", 
                     sec.axis = sec_axis(~.*coeff, )) +
  scale_color_manual("Carbon Pool:",
                     values=foxes_palette(n=2, name="main"),
                     guide=guide_legend(order=1)) +
  scale_fill_manual(name  ="Water Pool:", values = rep(1, 3),
                    guide=guide_legend(override.aes = list(colour=foxes_palettes$main[5], alpha = 0.5, linetype = "longdash") , order=2),
                    labels=c("Available Soil Water")) +
  labs(x = NULL) +
  foxes_theme + 
  theme(legend.position = "none", 
        plot.margin = margin(t = 0,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 0))
p_mpc_maxS

p_mpc_max45 <- ggplot(out_no_mem_max45, aes(x=time, y=carbon_pool_val)) +
  geom_hline(yintercept=0, color="#083855", size = 0.5,linetype="dashed") + 
  geom_line(mapping=aes(y=water_pool_val/coeff, fill = water_pool_type), 
            size=1, linetype="longdash", alpha=0.5, color=foxes_palettes$main[5]) + 
  geom_line(mapping=aes(colour=carbon_pool_type), size = 1, show.legend=TRUE) + 
  scale_y_continuous(NULL, 
                     sec.axis = sec_axis(~.*coeff, name = expression(paste("Available Water (kg", H[2], "O)")))) +
  scale_color_manual("Carbon Pool:",
                     values=foxes_palette(n=2, name="main"),
                     guide=guide_legend(order=1)) +
  scale_fill_manual(name  ="Water Pool:", values = rep(1, 3),
                    guide=guide_legend(override.aes = list(colour=foxes_palettes$main[5], alpha = 0.5, linetype = "longdash") , order=2),
                    labels=c("Available Soil Water")) +
  labs(x = NULL) +
  foxes_theme + 
  theme(legend.position = "none", 
        plot.margin = margin(t = 0,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 0))
p_mpc_max45

p_mpc_max65 <- ggplot(out, aes(x=time, y=carbon_pool_val)) +
  geom_hline(yintercept=0, color="#083855", size = 0.5,linetype="dashed") + 
  geom_line(mapping=aes(y=water_pool_val/coeff, fill = water_pool_type), 
            size=1, linetype="longdash", alpha=0.5, color=foxes_palettes$main[5]) + 
  geom_line(mapping=aes(colour=carbon_pool_type), size = 1, show.legend=TRUE) + 
  scale_y_continuous(NULL, 
                     sec.axis = sec_axis(~.*coeff, name = expression(paste("Available Water (kg", H[2], "O)")))) +
  scale_color_manual("Carbon Pool:",
                     values=foxes_palette(n=2, name="main"),
                     guide=guide_legend(order=1)) +
  scale_fill_manual(name  ="Water Pool:", values = rep(1, 3),
                    guide=guide_legend(override.aes = list(colour=foxes_palettes$main[5], alpha = 0.5, linetype = "longdash") , order=2),
                    labels=c("Available Soil Water")) +
  labs(x = "Time (day)") +
  foxes_theme + 
  theme(legend.position = "none", 
        plot.margin = margin(t = 0,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 0))
p_mpc_max65





file_figure <- tempfile(paste("sim", "fitness", "no", "memory", sep = "_"), tmpdir = out_folder, fileext = ".png")
ggsave(file_figure, plot = p1, device = NULL, path = NULL,
       scale = 1, width = 260, height = 340, dpi = 300, limitsize = TRUE,
       units =  "mm")

coeff = 0.4
out_df_p1 <- subset(out_df, goal_func %in% c(0, 0.5, 0.55, 0.6, 0.65, 0.75, 1))

p_mpc_all <- ggplot(out_df_p1, aes(x=time, y=carbon_pool_val)) +
  geom_hline(yintercept=0, color="#083855", size = 0.5,linetype="dashed") + 
  geom_line(mapping=aes(y=water_pool_val/coeff, fill = water_pool_type), 
            size=1, linetype="longdash", alpha=0.5, color=foxes_palettes$main[5]) + 
  geom_line(mapping=aes(colour=carbon_pool_type), size = 1, show.legend=TRUE) + 
  scale_y_continuous(NULL, 
                     sec.axis = sec_axis(~.*coeff, name = expression(paste("Available Water (kg", H[2], "O)")))) +
  scale_color_manual("Carbon Pool:",
                     values=foxes_palette(n=2, name="main"),
                     guide=guide_legend(order=1)) +
  scale_fill_manual(name  ="Water Pool:", values = rep(1, 3),
                    guide=guide_legend(override.aes = list(colour=foxes_palettes$main[5], alpha = 0.5, linetype = "longdash") , order=2),
                    labels=c("Available Soil Water")) +
  labs(x = "Time (day)") +
  facet_grid(rows = vars(goal_func), cols = vars(time_window), space= "fixed", labeller = labeller(goal_func = c("0" = "MaxS",
                                                                                       "0.45" = "0.45",
                                                                                       "0.55" = "0.55",
                                                                                       "1" = "MaxM"))) +
  foxes_theme + 
  theme(legend.position = "none", 
        plot.margin = margin(t = 0,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 0))
p_mpc_all





file_figure <- tempfile(paste("sim", "fitness", "no", "memory", sep = "_"), tmpdir = out_folder, fileext = ".png")
ggsave(file_figure, plot = p1, device = NULL, path = NULL,
       scale = 1, width = 260, height = 340, dpi = 300, limitsize = TRUE,
       units =  "mm")



goals <- unique(out$goal_func)
time_windows <- unique(out$time_window)

analysis <- lapply(time_windows, function(tw){
  maxM_simple <- subset(out, goal_func == 1 & time_window == tw)
  maxS_simple <- subset(out, goal_func == 0 & time_window == tw)
  return(plyr::rbind.fill(lapply(goals, function(goal){
    temp = subset(out, goal_func == goal & time_window == tw)
    num_maxM_days <- sum(temp$goal_equiv == 1)
    num_maxS_days <- sum(temp$goal_equiv == 0)
    
    error_M = sum(abs(maxM_simple$biomass - temp$biomass) + abs(maxM_simple$storage - temp$storage))
    error_S = sum(abs(maxS_simple$biomass - temp$biomass) + abs(maxS_simple$storage - temp$storage))
    
    df_out <- data.frame(time_window = tw, 
                         goal_func = goal,
                         num_maxM_days = num_maxM_days,
                         num_maxS_days = num_maxS_days,
                         error_M = error_M,
                         error_S = error_S)
    return(df_out)
  })))
})

analysis_df <- plyr::rbind.fill(analysis)

analysis_df$error <- analysis_df$error_M - analysis_df$error_S

analysis_df$total_equivalent <- ifelse(analysis_df$error >= 0, "MaxS", "MaxM")

analysis_df <- select(analysis_df, c("time_window","goal_func","num_maxM_days","num_maxS_days", "total_equivalent"))

analysis_df_next <- analysis_df %>%
  dplyr::mutate(percent_maxM = 100 * num_maxM_days/(num_maxM_days + num_maxS_days))

p <- ggplot(analysis_df_next, aes(x = goal_func, y = percent_maxM, color = as.factor(time_window))) + 
  geom_point() + 
  scale_color_manual("Time Window:",
                     values=foxes_palettes$extra[c(1,3)]) +
  labs(x = expression(paste("Goal Function ", k[f])),
       y = "Percent of MaxM Equivalent days (%)") +
  foxes_theme + 
  theme(legend.position = "bottom")
p

file_figure <- tempfile(paste("summary", "no_mem", "percent_maxm", sep = "_"), tmpdir = out_folder, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 180, height = 120, dpi = 150, limitsize = TRUE,
       units =  "mm")


file_csv <- tempfile(paste("sim", "fitness", "no", "memory", "analysis", sep = "_"), tmpdir = out_folder, fileext = ".csv")
write.csv(analysis_df, file = file_csv)






p1 <- ggplot(out, aes(x=time, y=alloc)) +
  geom_hline(yintercept=0, color="#083855", size = 0.5,linetype="dashed") + 
  # geom_line(mapping=aes(y=water_pool_val/coeff, fill = water_pool_type), 
  #           size=1, linetype="longdash", alpha=0.5, color=foxes_palettes$main[5]) + 
  geom_line(mapping=aes(colour=carbon_pool_type), size = 1, show.legend=TRUE) +
  # scale_y_continuous("Carbon Pool Size (gC)",
                     # sec.axis = sec_axis(~.*coeff, name = expression(paste("Available Water (kg", H[2], "O)")))) +
  # scale_color_manual("Carbon Pool:",
  #                    values=foxes_palette(n=2, name="main"),
  #             +       guide=guide_legend(order=1)) +
  # scale_fill_manual(name  ="Water Pool:", values = rep(1, 3),
  #                   guide=guide_legend(override.aes = list(colour=foxes_palettes$main[5], alpha = 0.5, linetype = "longdash") , order=2),
  #                   labels=c("Available Soil Water")) +
  labs(x = "Time (day)") +
  facet_grid(cols = vars(time_window), rows = vars(goal_func), space= "fixed") +
  foxes_theme
p1




file_figure <- tempfile(paste("sim", "memory", sep = "_"), tmpdir = out_folder, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 220, height = 520, dpi = 300, limitsize = TRUE,
       units =  "mm")


p1 <- ggplot(out_orig, aes(x=time, y=carbon_pool_val)) +
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
  foxes_theme
p1

file_figure <- tempfile(paste("sim",  sep = "_"), tmpdir = out_folder, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 220, height = 520, dpi = 300, limitsize = TRUE,
       units =  "mm")







#### TEST OPTIMALITY DETERMINISTIC OCT

Water_input_total = sum(sim_test$rainfall) + sim_test$plant$W
plant <- sim_test$plant
Tend = 249

kf = 0
ts_maxs <- find_opt_ts(plant, Water_input_total, Tend, kf, deltat = 1)
kf = 0.45
ts_max45 <- find_opt_ts(plant, Water_input_total, Tend, kf, deltat = 1)
kf = 0.50
ts_max5 <- find_opt_ts(plant, Water_input_total, Tend, kf, deltat = 1)
kf = 0.55
ts_max55 <- find_opt_ts(plant, Water_input_total, Tend, kf, deltat = 1)
kf = 0.60
ts_max6 <- find_opt_ts(plant, Water_input_total, Tend, kf, deltat = 1)
kf = 0.65
ts_max65 <- find_opt_ts(plant, Water_input_total, Tend, kf, deltat = 1)
kf = 0.75
ts_max75 <- find_opt_ts(plant, Water_input_total, Tend, kf, deltat = 1)
kf = 1
ts_maxm <- find_opt_ts(plant, Water_input_total, Tend, kf, deltat = 1)
ts_mid <- find_mid_ts(plant, Water_input_total, Tend)




outmaxM$carbon_pool_val[nrow(outmaxM)]

outmaxM <- plant_out_df(plant, W0 = Water_input_total, ts = ts_maxm, Tend = Tend, kf = 1, deltat = 1)
outmax45 <- plant_out_df(plant, W0 = Water_input_total, ts = ts_max45, Tend = Tend, kf = 0.45, deltat = 1)
outmax50 <- plant_out_df(plant, W0 = Water_input_total, ts = ts_max5, Tend = Tend, kf = 0.5, deltat = 1)
outmax55 <- plant_out_df(plant, W0 = Water_input_total, ts = ts_max55, Tend = Tend, kf = 0.55, deltat = 1)
outmax60 <- plant_out_df(plant, W0 = Water_input_total, ts = ts_max6, Tend = Tend, kf = 0.6, deltat = 1)
outmax65 <- plant_out_df(plant, W0 = Water_input_total, ts = ts_max65, Tend = Tend, kf = 0.65, deltat = 1)
outmax75 <- plant_out_df(plant, W0 = Water_input_total, ts = ts_max75, Tend = Tend, kf = 0.75, deltat = 1)
outmaxS <- plant_out_df(plant, W0 = Water_input_total, ts = ts_maxs, Tend = Tend, kf = 0, deltat = 1)

out_optim_all <- rbind(outmaxM, outmax45,outmax50, outmax55, outmax60, outmax65,outmax75, outmaxS)
out_optim <- rbind(outmaxM, outmax60, outmax65, outmaxS)

mean_ts <- data.frame(
  ts = c(outmaxM$ts[1], outmax60$ts[1],  outmax65$ts[1], outmaxS$ts[1]), 
  goal=c(1, 0.60, 0.65, 0), 
  initial_water=c(Water_input_total, Water_input_total, Water_input_total, Water_input_total)
)

mean_tcrit <- data.frame(
  tcrit = c(outmaxM$tcrit[1], outmax60$tcrit[1],  outmax65$tcrit[1],  outmaxS$tcrit[1]), 
  goal=c(1, 0.60, 0.65, 0), 
  initial_water=c(Water_input_total, Water_input_total, Water_input_total, Water_input_total)
)

mean_ts_maxM <- data.frame(
  ts = c(outmaxM$ts[1]), 
  goal=c(1), 
  initial_water=c(Water_input_total)
)

mean_ts_maxS <- data.frame(
  ts = c(outmaxS$ts[1]), 
  goal=c(0), 
  initial_water=c(Water_input_total)
)

mean_tcrit_maxM <- data.frame(
  tcrit = c(outmaxM$tcrit[1]), 
  goal=c(1), 
  initial_water=c(Water_input_total)
)

mean_tcrit_maxS <- data.frame(
  tcrit = c(outmaxS$tcrit[1]), 
  goal=c(0), 
  initial_water=c(Water_input_total)
)

coeff <- 0.4
p_oct_maxM <- ggplot(outmaxM, aes(x=time, y=carbon_pool_val)) +
  geom_vline(aes(xintercept=ts), mean_ts_maxM, color=foxes_palettes$dark[3], size = 0.5,linetype="dashed") +
  geom_vline(aes(xintercept=tcrit), mean_tcrit_maxM, color=foxes_palettes$main[4], size = 0.5,linetype="dashed") +
  geom_line(mapping=aes(y=water_pool_val/coeff, fill = water_pool_type), 
            size=1, linetype="longdash", alpha=0.5, color=foxes_palettes$light[5]) + 
  geom_line(mapping=aes(colour=carbon_pool_type), size = 1, show.legend=TRUE) + 
  geom_hline(yintercept=0, color="#083855", size = 0.5,linetype="dashed") + 
  scale_y_continuous(NULL, 
                     sec.axis = sec_axis(~.*coeff, name = NULL)) + 
  scale_color_manual("Carbon Pool:",
                     values=foxes_palette(n=2, name="main"),
                     guide=guide_legend(order=1)) +
  scale_fill_manual(name  ="Water Pool:", values = rep(1, 3),
                    guide=guide_legend(override.aes = list(colour=foxes_palettes$main[5], alpha = 0.5, linetype = "longdash") , order=2),
                    labels=c("Available Soil Water")) +
  labs(x = NULL) +
  foxes_theme + 
  theme(legend.position = "none", 
        plot.margin = margin(t = 0,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 0))
p_oct_maxM


p_oct_maxS <- ggplot(outmaxS, aes(x=time, y=carbon_pool_val)) +
  geom_line(mapping=aes(y=water_pool_val/coeff, fill = water_pool_type), 
            size=1, linetype="longdash", alpha=0.5, color=foxes_palettes$light[5]) + 
  geom_line(mapping=aes(colour=carbon_pool_type), size = 1, show.legend=TRUE) + 
  geom_hline(yintercept=0, color="#083855", size = 0.5,linetype="dashed") + 
  geom_vline(aes(xintercept=ts), mean_ts_maxS, color=foxes_palettes$dark[3], size = 0.5,linetype="dashed") +
  geom_vline(aes(xintercept=tcrit), mean_tcrit_maxS, color=foxes_palettes$main[4], size = 0.5,linetype="dashed") +
  scale_y_continuous(NULL, 
                     sec.axis = sec_axis(~.*coeff, name = NULL)) +
  scale_color_manual("Carbon Pool:",
                     values=foxes_palette(n=2, name="main"),
                     guide=guide_legend(order=1)) +
  scale_fill_manual(name  ="Water Pool:", values = rep(1, 3),
                    guide=guide_legend(override.aes = list(colour=foxes_palettes$main[5], alpha = 0.5, linetype = "longdash") , order=2),
                    labels=c("Available Soil Water")) +
  labs(x = NULL) +
  foxes_theme + 
  theme(legend.position = "none", 
        plot.margin = margin(t = 0,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 0))
p_oct_maxS

file_figure <- tempfile(paste("OCT", "maxM", sep = "_"), tmpdir = out_folder, fileext = ".png")
ggsave(file_figure, plot = p_maxM, device = NULL, path = NULL,
       scale = 1, width = 130, height = 90, dpi = 300, limitsize = TRUE,
       units =  "mm")

file_figure <- tempfile(paste("OCT", "maxS", sep = "_"), tmpdir = out_folder, fileext = ".png")
ggsave(file_figure, plot = p_maxS, device = NULL, path = NULL,
       scale = 1, width = 130, height = 90, dpi = 300, limitsize = TRUE,
       units =  "mm")

max_s_store <- outmaxS$carbon_pool_val[outmaxS$carbon_pool_type == "Storage"]
max_s_bio <- outmaxS$carbon_pool_val[outmaxS$carbon_pool_type == "Biomass"]
max_s_store[length(max_s_store)]
max_s_bio[length(max_s_bio)]

max_m_store <- outmaxM$carbon_pool_val[outmaxM$carbon_pool_type == "Storage"]
max_m_bio <- outmaxM$carbon_pool_val[outmaxM$carbon_pool_type == "Biomass"]
max_m_store[length(max_m_store)]
max_m_bio[length(max_m_bio)]


##### Optimal From Matlab

matlab_opt <- pivot_longer(out_obj, cols = c("biomass", "storage"), names_to = "carbon_pool_type", values_to = "carbon_pool_val")
matlab_opt <- pivot_longer(matlab_opt, cols = c("water"), names_to = "water_pool_type", values_to = "water_pool_val")

matlab_opt_maxM <- subset(matlab_opt, goal_func %in% c(1))
matlab_opt_maxS <- subset(matlab_opt, goal_func %in% c(0))

coeff <- 0.4
p_matlab_maxM <- ggplot(matlab_opt_maxM, aes(x=time, y=carbon_pool_val)) +
  geom_line(mapping=aes(y=water_pool_val/coeff, fill = water_pool_type), 
            size=1, linetype="longdash", alpha=0.5, color=foxes_palettes$light[5]) + 
  geom_line(mapping=aes(colour=carbon_pool_type), size = 1, show.legend=TRUE) + 
  geom_hline(yintercept=0, color="#083855", size = 0.5,linetype="dashed") + 
  scale_y_continuous(NULL, 
                     sec.axis = sec_axis(~.*coeff, name = NULL)) +
  scale_color_manual("Carbon Pool:",
                     values=foxes_palette(n=2, name="main"),
                     guide=guide_legend(order=1)) +
  scale_fill_manual(name  ="Water Pool:", values = rep(1, 3),
                    guide=guide_legend(override.aes = list(colour=foxes_palettes$main[5], alpha = 0.5, linetype = "longdash") , order=2),
                    labels=c("Available Soil Water")) +
  labs(x = "Time (day)") +
  foxes_theme + 
  theme(legend.position = "none", 
        plot.margin = margin(t = 0,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 0))
p_matlab_maxM


p_matlab_maxS <- ggplot(matlab_opt_maxS, aes(x=time, y=carbon_pool_val)) +
  geom_line(mapping=aes(y=water_pool_val/coeff, fill = water_pool_type), 
            size=1, linetype="longdash", alpha=0.5, color=foxes_palettes$light[5]) + 
  geom_line(mapping=aes(colour=carbon_pool_type), size = 1, show.legend=TRUE) + 
  geom_hline(yintercept=0, color="#083855", size = 0.5,linetype="dashed") + 
  scale_y_continuous(NULL, sec.axis = sec_axis(~.*coeff)) + 
  scale_color_manual("Carbon Pool:",
                     values=foxes_palette(n=2, name="main"),
                     guide=guide_legend(order=1)) +
  scale_fill_manual(name  ="Water Pool:", values = rep(1, 3),
                    guide=guide_legend(override.aes = list(colour=foxes_palettes$main[5], alpha = 0.5, linetype = "longdash") , order=2),
                    labels=c("Available Soil Water")) +
  labs(x = NULL) +
  foxes_theme + 
  theme(legend.position = "none", 
        plot.margin = margin(t = 0,  # Top margin
                             r = 0,  # Right margin
                             b = 0,  # Bottom margin
                             l = 0))
p_matlab_maxS

file_figure <- tempfile(paste("Opt", "Matlab", sep = "_"), tmpdir = out_folder, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 260, height = 180, dpi = 300, limitsize = TRUE,
       units =  "mm")

sum(rain_df$rainfall > 0)
sum(rain_df$rainfall == 0)

rain_df$rainfall[rain_df$time >= 150]
sum(rain_df$rainfall[rain_df$time >= 150] == 0)

matlab_opt_maxm <- subset(matlab_opt, goal_func == 1)
matlab_opt_maxs <- subset(matlab_opt, goal_func == 0)
matlab_opt_max65 <- subset(matlab_opt, goal_func == 0.65)

max_m_store <- matlab_opt_maxm$carbon_pool_val[matlab_opt_maxm$carbon_pool_type == "storage"]
max_m_bio <- matlab_opt_maxm$carbon_pool_val[matlab_opt_maxm$carbon_pool_type == "biomass"]
max_m_store[length(max_m_store)]
max_m_bio[length(max_m_bio)]

max_s_store <- matlab_opt_maxs$carbon_pool_val[matlab_opt_maxs$carbon_pool_type == "storage"]
max_s_bio <- matlab_opt_maxs$carbon_pool_val[matlab_opt_maxs$carbon_pool_type == "biomass"]
max_s_store[length(max_s_store)]
max_s_bio[length(max_s_bio)]

max_65_store <- matlab_opt_max65$carbon_pool_val[matlab_opt_max65$carbon_pool_type == "storage"]
max_65_bio <- matlab_opt_max65$carbon_pool_val[matlab_opt_max65$carbon_pool_type == "biomass"]
max_65_store[length(max_65_store)]
max_65_bio[length(max_65_bio)]

0.65 * max_65_bio[length(max_65_bio)] + 0.35 * max_65_store[length(max_65_store)]





p_all <- (p_mpc_maxS | (p_oct_maxS / p_matlab_maxS) | p_mpc_max45) /
  (p_mpc_maxM | (p_oct_maxM / p_matlab_maxM) | p_mpc_max65) +
  plot_layout(widths = c(2, 1, 2))
p_all
file_figure <- tempfile(paste("Opt", "compiled", sep = "_"), tmpdir = out_folder, fileext = ".png")
ggsave(file_figure, plot = p_all, device = NULL, path = NULL,
       scale = 1, width = 260, height = 180, dpi = 300, limitsize = TRUE,
       units =  "mm")




out_sub_rain_season <- subset(out, time_window == 25 & time < 150)
out_sub_dry_season <- subset(out, time_window == 25 & time >= 150)
p_water <- ggplot(out_sub_rain_season, aes(water, alloc, color = goal_func)) + 
  geom_point(alpha = 0.25) + 
  scale_color_gradientn(expression(paste("Goal Function ", k[f], ":")), 
                        colors = foxes_palettes$main[c(2, 4, 3, 1)],
                        breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c("MaxS", "0.25", "0.5", "0.75", "MaxM")) +
  # geom_jitter(data = out_sub_dry_season,
  #             mapping = aes(water, alloc, color = goal_func), 
  #             alpha = 0.25) + 
  # scale_color_gradientn(expression(paste("Goal Function ", k[f], ":")), 
  #                       colors = foxes_palettes$dark[c(2, 4, 3, 1)],
  #                       breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c("MaxS", "0.25", "0.5", "0.75", "MaxM")) +
  scale_y_continuous(expression(paste("Allocation parameter ", u[t], " gC", g^-1, "Cda", y^-1 )),
                     breaks = c(0, 0.06), labels = c(0, expression(k[s])))+
  scale_x_continuous(expression(paste("Water Availability ", W[t], " kg", H[2], "O")))+
  foxes_theme #+ 
  # theme(legend.position = "none", 
  #       plot.margin = margin(t = 5,  # Top margin
  #                            r = 5,  # Right margin
  #                            b = 5,  # Bottom margin
  #                            l = 5))
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


p_rain <- ggplot(out_sub_rain_season, aes(rainfall, alloc, color = goal_func)) + 
  geom_jitter(alpha = 0.25) + 
  scale_color_gradientn(expression(paste("Goal Function ", k[f], ":")), 
                        colors = foxes_palettes$light[c(2, 4, 3, 1)],
                        breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c("MaxS", "0.25", "0.5", "0.75", "MaxM")) +
  new_scale_color() +
  geom_jitter(data = out_sub_dry_season,
              mapping = aes(rainfall, alloc, color = goal_func), 
              alpha = 0.25) + 
  scale_color_gradientn(expression(paste("Goal Function ", k[f], ":")), 
                        colors = foxes_palettes$dark[c(2, 4, 3, 1)],
                        breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c("MaxS", "0.25", "0.5", "0.75", "MaxM")) +
  scale_y_continuous(NULL, breaks = NULL)+
  scale_x_continuous(expression(paste("Rainfall ", W[t], " kg", H[2], "O da", y^-1)))+
  foxes_theme + 
  theme(legend.position = "none", 
        plot.margin = margin(t = 5,  # Top margin
                             r = 5,  # Right margin
                             b = 5,  # Bottom margin
                             l = 5))
p_rain

file_figure <- tempfile(paste("allocation", "rainfall", "tw", 25, sep = "_"), tmpdir = out_folder, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 260, height = 180, dpi = 200, limitsize = TRUE,
       units =  "mm")

p <- ggplot(out_sub_rain_season, aes(rainfall, alloc, color = goal_func)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_gradientn(expression(paste("Goal Function ", k[f], ":")), 
                        colors = foxes_palettes$light[c(2, 4, 3, 1)],
                        breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c("MaxS", "0.25", "0.5", "0.75", "MaxM")) +
  new_scale_color() +
  geom_jitter(data = out_sub_dry_season,
              mapping = aes(rainfall, alloc, color = goal_func), 
              alpha = 0.5) + 
  scale_color_gradientn(expression(paste("Goal Function ", k[f], ":")), 
                        colors = foxes_palettes$main[c(2, 4, 3, 1)],
                        breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c("MaxS", "0.25", "0.5", "0.75", "MaxM")) +
  scale_y_continuous(expression(paste("Allocation parameter ", u[t], " gC", g^-1, "Cda", y^-1 )),
                     breaks = c(0, 0.06), labels = c(0, expression(k[s])))+
  scale_x_continuous(expression(paste("Rainfall ", W[t], " kg", H[2], "O da", y^-1)))+
  foxes_theme + 
  theme(legend.position = "bottom", 
        legend.key.width = unit(2.5, "cm"),
        legend.text = element_text(angle = 45, hjust = 1),
        legend.box = "vertical")
p

file_figure <- tempfile(paste("allocation", "rainfall", "for_legend", "tw", 25, sep = "_"), tmpdir = out_folder, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 260, height = 180, dpi = 200, limitsize = TRUE,
       units =  "mm")


p_water_dims <- get_dim(p_water)
p_rain_aligned <- set_dim(p_rain, p_water_dims)
p_alloc <- p_water + p_rain
p_alloc

file_figure <- tempfile(paste("allocation", "water", "rainfall", sep = "_"), tmpdir = out_folder, fileext = ".png")
ggsave(file_figure, plot = p_alloc, device = NULL, path = NULL,
       scale = 1, width = 260, height = 120, dpi = 200, limitsize = TRUE,
       units =  "mm")



#### Allocation over time

out_tw_25 <- subset(out, time_window == 25 & goal_func %in% c(0, 0.45, 0.55, 1))
out_tw_25$goal_func <- as.factor(out_tw_25$goal_func)
p <- ggplot(out_tw_25, aes(time, alloc)) + 
  geom_line(color = "#083855") + 
  scale_y_continuous(expression(paste("Allocation parameter ", u[t], " gC", g^-1, "Cda", y^-1 )),
                     breaks = c(0, 0.06), labels = c(0, expression(k[s])))+
  scale_x_continuous("Time (days)") +
  facet_grid(rows = vars(goal_func), space= "fixed", labeller = labeller(goal_func = c("0" = "MaxS",
                                                                                  "0.45" = "0.45",
                                                                                  "0.55" = "0.55",
                                                                                  "1" = "MaxM"))) +
  foxes_theme
p

file_figure <- tempfile(paste("allocation", "time", sep = "_"), tmpdir = out_folder, fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 260, height = 180, dpi = 200, limitsize = TRUE,
       units =  "mm")

#### TABLE WITH OUT VALUES

out_optim_all <- pivot_wider(out_optim_all, names_from = carbon_pool_type, values_from = carbon_pool_val)

cols_out_optims <- colnames(out_optim_all)
cols_out_optims[cols_out_optims == "goal"] = "goal_func"
colnames(out_optim_all) <- cols_out_optims

out_oct_sums <- out_optim_all %>% 
  dplyr::group_by(goal_func) %>%
  dplyr::summarise(all_s = sum(Storage)/max(time),
                   all_m = sum(Biomass)/max(time)) %>%
  dplyr::mutate(out_goal = all_m * goal_func + all_s * (1-goal_func))

out_mpc_sums <- out %>% 
  dplyr::group_by(goal_func, time_window) %>%
  dplyr::summarise(all_s = sum(storage)/max(time),
                   all_m = sum(biomass)/max(time)) %>%
  dplyr::mutate(out_goal = all_m * goal_func + all_s * (1-goal_func))

matlab_opt_all <- pivot_wider(matlab_opt, names_from = carbon_pool_type, values_from = carbon_pool_val)
out_opt_sums <- matlab_opt_all %>% 
  dplyr::group_by(goal_func) %>%
  dplyr::summarise(all_s = sum(storage)/max(time),
                   all_m = sum(biomass)/max(time)) %>%
  dplyr::mutate(out_goal = all_m * goal_func + all_s * (1-goal_func))

out_oct_sums$type = "OCT"
out_opt_sums$type = "OPT"
out_mpc_sums$type = paste0("MPC", out_mpc_sums$time_window)

out_all_sums <- plyr::rbind.fill(out_oct_sums, out_opt_sums, out_mpc_sums)
out_all_sums <- select(out_all_sums, c("goal_func", "type", "out_goal"))
out_all_sums <- pivot_wider(out_all_sums, names_from = type, values_from = out_goal)
View(out_all_sums)


out_oct_final<- out_optim_all %>% 
  filter(time == max(time)) %>%
  dplyr::group_by(goal_func) %>%
  dplyr::summarise(s = Storage,
                   m = Biomass) %>%
  dplyr::mutate(out_goal = m * goal_func + s * (1-goal_func))

out_mpc_final <- out %>% 
  filter(time == max(time)) %>% 
  dplyr::group_by(goal_func, time_window) %>%
  dplyr::summarise(s = storage,
                   m = biomass) %>%
  dplyr::mutate(out_goal = m * goal_func + s * (1-goal_func))

matlab_opt_all <- pivot_wider(matlab_opt, names_from = carbon_pool_type, values_from = carbon_pool_val)
out_opt_final <- matlab_opt_all %>% 
  filter(time == max(time)) %>% 
  dplyr::group_by(goal_func) %>%
  dplyr::summarise(s = storage,
                   m = biomass) %>%
  dplyr::mutate(out_goal = m * goal_func + s * (1-goal_func))

out_oct_final$type = "OCT"
out_opt_final$type = "OPT"
out_mpc_final$type = paste0("MPC", out_mpc_sums$time_window)

out_all_final <- plyr::rbind.fill(out_oct_final, out_opt_final, out_mpc_final)
out_all_final <- select(out_all_final, c("goal_func", "type", "out_goal"))
out_all_final <- pivot_wider(out_all_final, names_from = type, values_from = out_goal)
View(out_all_final)



file_csv <- tempfile(paste("sim", "fitness", "all", "final_val", "analysis", sep = "_"), tmpdir = out_folder, fileext = ".csv")
write.csv(out_all_final, file = file_csv)

file_csv <- tempfile(paste("sim", "fitness", "all", "sum", "analysis", sep = "_"), tmpdir = out_folder, fileext = ".csv")
write.csv(out_all_sums, file = file_csv)



(out_all_sums$MPC10 / out_all_sums$OCT)

