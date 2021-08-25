rain_vals = seq(from = 10, to = 100, by = 0.01)

rain_df <- 40

probs <- pchisq(rain_vals, rain_df)
density <- dchisq(rain_vals, rain_df)


chi_sq_dist_df <- data.frame(rain = rain_vals,
                             probability = probs,
                             density = density)


p <- ggplot(chi_sq_dist_df, aes(x = rain, y = density)) + 
  geom_line(color = "#083855", size = 1) + 
  scale_y_continuous("Probability Density") +
  labs(x = expression(paste("Rain Input [kg", H[2], "O/day]"))) +
  foxes_theme
p

file_figure <- tempfile("rain_prob_density_chi_sq", tmpdir = "out/figures/Environment_Methods", fileext = ".png")
ggsave(file_figure, plot = p, device = NULL, path = NULL,
       scale = 1, width = 120, height = 80, dpi = 300, limitsize = TRUE,
       units =  "mm")
