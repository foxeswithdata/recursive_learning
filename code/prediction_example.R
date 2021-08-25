rm(list=ls())

library(tidyverse)
library(ggnewscale)
library(cowplot)

source("code/model/plant_class.R")
source("code/model/environment_run.R")
source("code/util/foxes_pallettes.R")


prediction_example_t <- data.frame(time = c(1:11),
                                 control = c(0.3, 0.33, 0.34, 0.35, 0.40, 0.39, 0.37, 0.29, 0.24, 0.23, 0.18),
                                 prediction = c("real", "pred", "pred", "pred", "pred", "pred", "pred", "pred", "pred", "pred", "pred"),
                                 rect_start = rep(1.5, times = 11),
                                 rect_end = rep(11.5, times = 11),
                                 time_point = rep("t", times = 11))

prediction_example_t_1 <- data.frame(time = c(1:12),
                                   control = c(0.3, 0.33, 0.22, 0.24, 0.23, 0.22, 0.27, 0.30, 0.36, 0.40, 0.44, 0.46),
                                   prediction = c("real", "real", "pred", "pred", "pred", "pred", "pred", "pred", "pred", "pred", "pred", "pred"),
                                   rect_start = rep(2.5, times = 12),
                                   rect_end = rep(12.5, times = 12),
                                   time_point = rep("t+1", times = 12))

prediction_example_t_2 <- data.frame(time = c(1:13),
                                   control = c(0.3, 0.33, 0.22, 0.24, 0.19, 0.20, 0.22, 0.21, 0.24, 0.22, 0.15, 0.13, 0.15),
                                   prediction = c("real", "real", "real", "pred", "pred", "pred", "pred", "pred", "pred", "pred", "pred", "pred", "pred"),
                                   rect_start = rep(3.5, times = 13),
                                   rect_end = rep(13.5, times = 13),
                                   time_point = rep("t+2", times = 13))

prediction_example <- plyr::rbind.fill(prediction_example_t, prediction_example_t_1, prediction_example_t_2)

goal_example_t <- data.frame(time = c(1:2),
                             return = c(0.2, 0.3), 
                             prediction = c("real", "pred"),
                             time_point = rep("t", times = 2))

goal_example_t_1 <- data.frame(time = c(1,2, 2, 3),
                             return = c(0.2, 0.23, 0.3, 0.14), 
                             prediction = c("done", "real", "pred", "pred"),
                             time_point = rep("t+1", times = 4))

goal_example_t_2 <- data.frame(time = c(1,2, 3, 3, 4),
                               return = c(0.2, 0.23, 0.16, 0.14, 0.35),  
                               prediction = c("done", "done", "real", "pred", "pred"),
                               time_point = rep("t+2", times = 5))

goal_example <- plyr::rbind.fill(goal_example_t, goal_example_t_1, goal_example_t_2)

p_pred <- ggplot(prediction_example, aes(time, control, color = prediction)) +
  geom_rect(aes(xmin = rect_start, xmax = rect_end, ymin = -3, ymax = 3), color = "#FFFFFF", fill = "#F9AF2C", alpha = 0.005) + 
  geom_point(size = 2) + 
  scale_x_continuous("Time", breaks = c(1, 2, 3, 4, 6, 11), labels = c("t", "t+1", "t+2", "t+3", "t+5", "t+10")) + 
  scale_y_continuous("Control u", breaks = NULL) + 
  scale_color_manual("",
                     values=foxes_palettes$main[c(2,4)],
                     labels = c("Predicted", "Realised")) +
  coord_cartesian(xlim = c(1, 13), ylim = c(0, 0.5)) +
  facet_grid(rows = vars(time_point)) +
  foxes_theme + 
  theme(legend.position = "bottom",
        plot.margin = margin(t=10, r=15, b=10, l=10, unit = "pt"))
p_pred


p_goal <- ggplot(goal_example, aes(time, return, color = prediction)) +
  geom_point(size = 2) + 
  scale_x_continuous("Time", breaks = c(1, 2, 3, 4), labels = c("t", "t+1", "t+2", "t+3")) + 
  scale_y_continuous("Return", breaks = NULL) + 
  scale_color_manual("",
                     values=c(foxes_palettes$main[c(2,4)], "#987C8C"),
                     labels = c("Predicted", "Realised", "Past")) +
  coord_cartesian(xlim = c(1, 4), ylim = c(0, 0.4)) +
  facet_grid(rows = vars(time_point)) +
  foxes_theme + 
  theme(legend.position = "bottom",
        plot.margin = margin(t=10, r=10, b=10, l=20, unit = "pt"))
p_goal

p <- plot_grid(p_pred, p_goal, label_size = 14, 
               label_fontfamily = "Lato Light",
               label_fontface = "bold",
               label_colour = "#083855",
               rel_widths = c(3, 2))
p

file_figure <- tempfile(paste("MPC", "prediction", "example", sep = "_"), tmpdir = "out/figures/example_pred", fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 220, height = 160, dpi = 300, limitsize = TRUE,
       units =  "mm")





