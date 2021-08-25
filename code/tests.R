rm(list = ls())

library(tidyverse)

source("code/model/plant_class.R")
source("code/model/environment_run.R")
source("code/util/foxes_pallettes.R")

### Model a single year 

Tend = 150

### SET UP WATER ENVIRONMENT

water_ave <- 10
water_sd <- 5

## spending approximately 1/3 of the year in rain
MC_water <- matrix(c(0.6, 0.4, 0.2,  0.8), nrow = 2, byrow = TRUE)
colnames(MC_water) <- c('R', 'D')
rownames(MC_water) <- c('R', 'D')
MC_water

## find the steady state for each 
steady_state <- c(MC_water[2,1]/(MC_water[1,2] + (MC_water[2,1])), 
                  MC_water[1,2]/(MC_water[1,2] + (MC_water[2,1])))

names(steady_state) <- c('R', 'D')
steady_state

# KG of water per year
steady_state[1] * 365 * water_ave

rainfall <- generate_rainfall(Tend, MC_water, water_ave, water_sd)

time = 1:length(rainfall)
rain_df <- data.frame(time = time, rainfall = rainfall)

p <- ggplot(rain_df, aes(x=time, y=rainfall)) +
  geom_bar(stat = "identity", fill="blue") +
  scale_y_continuous(
    # Features of the first axis
    name =  "Water Input [kgH20]"
  ) +
  theme_bw()
p

sum(rainfall)

# Gamma alternative
water_ave <- 4
water_sd <- 0.75

rainfall <- generate_rainfall_gamma(Tend, MC_water, water_ave, water_sd)
sum(rainfall)

time = 1:length(rainfall)
rain_df <- data.frame(time = time, rainfall = rainfall)

p <- ggplot(rain_df, aes(x=time, y=rainfall)) +
  geom_bar(stat = "identity", fill="blue") +
  scale_y_continuous(
    # Features of the first axis
    name =  "Water Input [kgH20]"
  ) +
  theme_bw()
p

sum(rainfall)

# Chi alternative
water_ave <- 40
water_sd <- 0.75

rainfall <- generate_rainfall_chi(Tend, MC_water, water_ave)
sum(rainfall)

time = 1:length(rainfall)
rain_df <- data.frame(time = time, rainfall = rainfall)

p <- ggplot(rain_df, aes(x=time, y=rainfall)) +
  geom_bar(stat = "identity", fill="blue") +
  scale_y_continuous(
    # Features of the first axis
    name =  "Water Input [kgH20]"
  ) +
  theme_bw()
p

sum(rainfall)


### SET UP THE PLANT
W0 <- 50;
plant <- new_plant(W0 = W0);

### GENERATE AN OUTPUT

goal_func = 0
outputs_10_0 <- run_model(plant,rainfall, 10, Tend, W0, goal_func, 1)
goal_func = 0.25
outputs_10_25 <- run_model(plant,rainfall, 10, Tend, W0, goal_func, 1)
goal_func = 0.5
outputs_10_50 <- run_model(plant,rainfall, 10, Tend, W0, goal_func, 1)
goal_func = 0.75
outputs_10_75 <- run_model(plant,rainfall, 10, Tend, W0, goal_func, 1)
goal_func = 1
outputs_10_1 <- run_model(plant,rainfall, 10, Tend, W0, goal_func, 1)

goal_func = 0
outputs_25_0 <- run_model(plant,rainfall, 25, Tend, W0, goal_func, 1)
goal_func = 0.25
outputs_25_25 <- run_model(plant,rainfall, 25, Tend, W0, goal_func, 1)
goal_func = 0.5
outputs_25_50 <- run_model(plant,rainfall, 25, Tend, W0, goal_func, 1)
goal_func = 0.75
outputs_25_75 <- run_model(plant,rainfall, 25, Tend, W0, goal_func, 1)
goal_func = 1
outputs_25_1 <- run_model(plant,rainfall, 25, Tend, W0, goal_func, 1)




outputs_15_025 <- run_model(plant,rainfall, 15, Tend, W0, goal_func, 1)
outputs_20_025 <- run_model(plant,rainfall, 20, Tend, W0, goal_func, 1)
outputs_25_025 <- run_model(plant,rainfall, 25, Tend, W0, goal_func, 1)

goal_func = 1

outputs_10_075 <- run_model(plant,rainfall, 10, Tend, W0, goal_func, 1)
outputs_15_075 <- run_model(plant,rainfall, 15, Tend, W0, goal_func, 1)
outputs_20_075 <- run_model(plant,rainfall, 20, Tend, W0, goal_func, 1)
outputs_25_075 <- run_model(plant,rainfall, 25, Tend, W0, goal_func, 1)

# Generate the optimum based on total water during the year

save.image(file = "sim6.RData")

load("sim2.RData")

simlength = length(outputs_10_0$M)
t_seq = 1:simlength
# 
# out_df_1 <- data.frame(carbon_pool_type = as.factor(rep(c("Biomass", "Storage"), each = simlength)),
#                        carbon_pool_val = c(outputs_10_025$M, outputs_10_025$S),
#                        water_pool_val = c(rep(outputs_10_025$W, times = 2)),
#                        water_pool_type = as.factor(rep("Soil Water Available", times = simlength * 2)),
#                        time = rep(t_seq, times = 2),
#                        goal_func = c(as.factor(c(rep("0", times = simlength * 2)))),
#                        prediction_window = rep(10, times = 2 * simlength))

out_df_1 <- data.frame(carbon_pool_type = as.factor(rep(c("Biomass", "Storage"), each = simlength)),
                       carbon_pool_val = c(outputs_10_0$M, outputs_10_0$S),
                       water_pool_val = c(rep(outputs_10_0$W, times = 2)),
                       water_pool_type = as.factor(rep("Soil Water Available", times = simlength * 2)),
                       time = rep(t_seq, times = 2),
                       goal_func = c(as.factor(c(rep("0", times = simlength * 2)))),
                       prediction_window = rep(10, times = 2 * simlength))

out_df_1 <- data.frame(carbon_pool_type = as.factor(rep(c("Biomass", "Storage", 
                                                          "Biomass", "Storage", 
                                                          "Biomass", "Storage", 
                                                          "Biomass", "Storage", 
                                                          "Biomass", "Storage"), each = simlength)),
                       carbon_pool_val = c(outputs_10_0$M, outputs_10_0$S, 
                                           outputs_10_25$M, outputs_10_25$S, 
                                           outputs_10_50$M, outputs_10_50$S, 
                                           outputs_10_75$M, outputs_10_75$S, 
                                           outputs_10_1$M, outputs_10_1$S),
                       water_pool_val = c(rep(outputs_10_0$W, times = 2), 
                                          rep(outputs_10_25$W, times = 2), 
                                          rep(outputs_10_50$W, times = 2), 
                                          rep(outputs_10_75$W, times = 2), 
                                          rep(outputs_10_1$W, times = 2)),
                       water_pool_type = as.factor(rep("Soil Water Available", times = simlength * 10)),
                       time = rep(t_seq, times = 10),
                       goal_func = c(rep("0", times = simlength * 2), 
                                     rep("025", times = simlength * 2), 
                                     rep("050", times = simlength * 2), 
                                     rep("075", times = simlength * 2), 
                                     rep("1", times = simlength * 2)),
                       prediction_window = rep(10, times = 10 * simlength))

simlength = length(outputs_25_0$M)

out_df_2 <- data.frame(carbon_pool_type = as.factor(rep(c("Biomass", "Storage", 
                                                          "Biomass", "Storage", 
                                                          "Biomass", "Storage", 
                                                          "Biomass", "Storage", 
                                                          "Biomass", "Storage"), each = simlength)),
                       carbon_pool_val = c(outputs_25_0$M, outputs_25_0$S, 
                                           outputs_25_25$M, outputs_25_25$S, 
                                           outputs_25_50$M, outputs_25_50$S, 
                                           outputs_25_75$M, outputs_25_75$S, 
                                           outputs_25_1$M, outputs_25_1$S),
                       water_pool_val = c(rep(outputs_25_0$W, times = 2), 
                                          rep(outputs_25_25$W, times = 2), 
                                          rep(outputs_25_50$W, times = 2), 
                                          rep(outputs_25_75$W, times = 2), 
                                          rep(outputs_25_1$W, times = 2)),
                       water_pool_type = as.factor(rep("Soil Water Available", times = simlength * 10)),
                       time = rep(t_seq, times = 10),
                       goal_func = c(rep("0", times = simlength * 2), 
                                     rep("025", times = simlength * 2), 
                                     rep("050", times = simlength * 2), 
                                     rep("075", times = simlength * 2), 
                                     rep("1", times = simlength * 2)),
                       prediction_window = rep(25, times = 10 * simlength))

simlength = length(outputs_15_025$M)
t_seq = 1:simlength
out_df_2 <- data.frame(carbon_pool_type = as.factor(rep(c("Biomass", "Storage", "Biomass", "Storage"), each = simlength)),
                       carbon_pool_val = c(outputs_15_025$M, outputs_15_025$S,outputs_15_075$M, outputs_15_075$S),
                       water_pool_val = c(rep(outputs_15_025$W, times = 2), rep(outputs_15_075$W, times = 2)),
                       water_pool_type = as.factor(rep("Soil Water Available", times = simlength * 4)),
                       time = rep(t_seq, times = 4),
                       goal_func = c(as.factor(c(rep("0", times = simlength * 2), rep("1", times = simlength * 2)))),
                       prediction_window = rep(15, times = 4 * simlength))

simlength = length(outputs_20_025$M)
t_seq = 1:simlength
out_df_3 <- data.frame(carbon_pool_type = as.factor(rep(c("Biomass", "Storage", "Biomass", "Storage"), each = simlength)),
                       carbon_pool_val = c(outputs_20_025$M, outputs_20_025$S,outputs_20_075$M, outputs_20_075$S),
                       water_pool_val = c(rep(outputs_20_025$W, times = 2), rep(outputs_20_075$W, times = 2)),
                       water_pool_type = as.factor(rep("Soil Water Available", times = simlength * 4)),
                       time = rep(t_seq, times = 4),
                       goal_func = c(as.factor(c(rep("0", times = simlength * 2), rep("1", times = simlength * 2)))),
                       prediction_window = rep(20, times = 4 * simlength))

simlength = length(outputs_25_025$M)
t_seq = 1:simlength
out_df_4 <- data.frame(carbon_pool_type = as.factor(rep(c("Biomass", "Storage", 
                                                          "Biomass", "Storage"), each = simlength)),
                       carbon_pool_val = c(outputs_25_025$M, outputs_25_025$S, outputs_25_075$M, outputs_25_075$S),
                       water_pool_val = c(rep(outputs_25_025$W, times = 2), rep(outputs_25_075$W, times = 2)),
                       water_pool_type = as.factor(rep("Soil Water Available", times = simlength * 4)),
                       time = rep(t_seq, times = 4),
                       goal_func = c(as.factor(c(rep("0", times = simlength * 2), rep("1", times = simlength * 2)))),
                       prediction_window = rep(25, times = 4 * simlength))


out_df <- plyr::rbind.fill(out_df_1, out_df_2)


coeff <- 0.4

p1 <- ggplot(out_df_1, aes(x=time, y=carbon_pool_val)) +
  geom_line(mapping=aes(colour=carbon_pool_type), show.legend=TRUE) + 
  geom_line(mapping=aes(y=water_pool_val/coeff, 
                        fill = water_pool_type), size=1.5, alpha=0.5, color = "blue") + 
  
  geom_hline(yintercept=0, color="#000000", size = 0.25,linetype="dashed") + 
  scale_y_continuous(
    limits = c(-625,4000),
    # Features of the first axis
    name = "Pool Size (gC)",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name = expression(paste("Available Water (kg", H[2], "O)")))
  ) +
  scale_color_hue("Carbon Pool", guide=guide_legend(order=1)) +
  scale_fill_manual(name  ="Water Pool", values = rep(1, 3),
                    guide=guide_legend(
                      override.aes = list(colour=c("blue")) , order=2),
                    labels=c("Available Soil Water")) +
  
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

p1



p <- ggplot(out_df, aes(x=time, y=carbon_pool_val)) +
  geom_line(mapping=aes(y=water_pool_val/coeff, fill = water_pool_type), 
            size=1, linetype="longdash", alpha=0.5, color=foxes_palettes$light[5]) + 
  geom_line(mapping=aes(colour=carbon_pool_type), size = 1, show.legend=TRUE) + 
  geom_hline(yintercept=0, color="#083855", size = 0.5,linetype="dashed") + 
  scale_y_continuous("Carbon Pool Size (gC)", 
                     sec.axis = sec_axis(~.*coeff, name = expression(paste("Available Water (kg", H[2], "O)")))) +
  scale_color_manual("Carbon Pool:",
                     values=foxes_palette(n=2, name="main"),
                     guide=guide_legend(order=1)) +
  scale_fill_manual(name  ="Water Pool:", values = rep(1, 3),
                    guide=guide_legend(override.aes = list(colour=foxes_palettes$main[5], alpha = 0.5, linetype = "longdash") , order=2),
                    labels=c("Available Soil Water")) +
  labs(x = "Time (day)") +
  facet_grid(goal_func ~ prediction_window, labeller = labeller(goal_func = c("0" = "MaxS",
                                                                       "025" = "0.25",
                                                                       "050" = "0.50",
                                                                       "075" = "0.75",
                                                                       "1" = "MaxM"))) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        legend.position = "bottom",
        plot.margin = margin(1,1,1,1, "cm"),
        text=element_text(size=12, color = "#083855", family="Lato Light"),
        axis.text=element_text(size=8, color = "#083855", family="Lato Light"), 
        # text=element_text(size=16, color = "#083855", family="Lato"),
        # axis.text=element_text(size=16, color = "#083855", family="Lato"), 
        strip.background = element_blank())
p







out_df_predicted <- out_df_1[out_df_1$time < 50,]
out_df_rest <- out_df_1[out_df_1$time >= 50,]

predicted_plant <- outputs$predicted[[25]]

predicted_25 <- data.frame(carbon_pool_type = as.factor(rep(c("Storage", "Biomass"), each = tw+1)),
                           carbon_pool_val = c(t(predicted_plant$storage), t(predicted_plant$biomass)),
                           water_pool_val = rep(seq(from=outputs$W[25], by=outputs$rain_prediction[25], length.out = tw+1), times = 2),
                           water_pool_type = as.factor(rep("Soil Water Available", times = (tw+1) * 2)),
                           time = rep(t_seq[25] + predicted_plant$time, times = 2))


predicted_plant <- outputs$predicted[[50]]

predicted_50 <- data.frame(carbon_pool_type = as.factor(rep(c("Storage", "Biomass"), each = tw+1)),
                           carbon_pool_val = c(t(predicted_plant$x[2,]), t(predicted_plant$x[1,])),
                           water_pool_val = rep(seq(from=outputs$W[50], by=outputs$rain_prediction[50], length.out = tw+1), times = 2),
                           water_pool_type = as.factor(rep("Soil Water Available", times = (tw+1) * 2)),
                           time = rep(t_seq[50] + predicted_plant$time, times = 2))

predicted_plant <- outputs$predicted[[100]]

predicted_100 <- data.frame(carbon_pool_type = as.factor(rep(c("Storage", "Biomass"), each = tw+1)),
                           carbon_pool_val = c(t(predicted_plant$x[2,]), t(predicted_plant$x[1,])),
                           water_pool_val = rep(seq(from=outputs$W[100], by=outputs$rain_prediction[100], length.out = tw+1), times = 2),
                           water_pool_type = as.factor(rep("Soil Water Available", times = (tw+1) * 2)),
                           time = rep(t_seq[100] + predicted_plant$time, times = 2))


p1 <- ggplot(out_df_predicted, aes(x=time, y=carbon_pool_val)) +
  geom_line(mapping=aes(colour=carbon_pool_type), show.legend=TRUE) + 
  geom_line(mapping=aes(y=water_pool_val/coeff, 
                        fill = water_pool_type), size=1.5, alpha=0.5, color = "blue") + 
  geom_line(mapping=aes(x=time, y=carbon_pool_val,colour=carbon_pool_type), 
            data = out_df_rest, show.legend=TRUE, alpha=0.25) + 
  geom_line(mapping=aes(x=time, y=water_pool_val/coeff, 
                        fill = water_pool_type), data = out_df_rest, size=1.5, alpha=0.125, color = "blue") +
  geom_hline(yintercept=0, color="#000000", size = 0.25,linetype="dashed") + 
  scale_y_continuous(
    limits = c(-625,4000),
    # Features of the first axis
    name = "Pool Size (gC)",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name = expression(paste("Available Water (kg", H[2], "O)")))
  ) +
  scale_color_hue("Carbon Pool", guide=guide_legend(order=1)) +
  scale_fill_manual(name  ="Water Pool", values = rep(1, 3),
                    guide=guide_legend(
                      override.aes = list(colour=c("blue")) , order=2),
                    labels=c("Available Soil Water")) +
  
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

p1

p1 <- ggplot(predicted_25, aes(x=time, y=carbon_pool_val)) +
  geom_line(mapping=aes(colour=carbon_pool_type), show.legend=TRUE) + 
  geom_line(mapping=aes(y=water_pool_val/coeff, 
                        fill = water_pool_type), size=1.5, alpha=0.5, color = "blue") + 
  # geom_line(mapping=aes(x=time, y=carbon_pool_val,colour=carbon_pool_type), 
  #           data = out_df_rest, show.legend=TRUE, alpha=0.25) + 
  # geom_line(mapping=aes(x=time, y=water_pool_val/coeff, 
  #                       fill = water_pool_type), data = out_df_rest, size=1.5, alpha=0.125, color = "blue") +
  geom_hline(yintercept=0, color="#000000", size = 0.25,linetype="dashed") +
scale_y_continuous(
  limits = c(-625,4000),
  # Features of the first axis
  name = "Pool Size (gC)",
  # Add a second axis and specify its features
  sec.axis = sec_axis(~.*coeff, name = expression(paste("Available Water (kg", H[2], "O)")))
) +
  scale_color_hue("Carbon Pool", guide=guide_legend(order=1)) +
  scale_fill_manual(name  ="Water Pool", values = rep(1, 3),
                    guide=guide_legend(
                      override.aes = list(colour=c("blue")) , order=2),
                    labels=c("Available Soil Water")) +
  
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

p1
6.598684