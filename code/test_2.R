load("sim6.RData")

Tend_orig = 150
Tend = 150

goal_func = 0
outputs_10_0 <- run_model_simple_pred(plant,rainfall, 10, Tend, W0, goal_func, 1)
goal_func = 0.5
outputs_10_50 <- run_model_simple_pred(plant,rainfall, 10, Tend, W0, goal_func, 1)
goal_func = 1
outputs_10_1 <- run_model_simple_pred(plant,rainfall, 10, Tend, W0, goal_func, 1)

goal_func = 0
outputs_25_0 <- run_model_simple_pred(plant,rainfall, 25, Tend, W0, goal_func, 1)
goal_func = 0.5
outputs_25_50 <- run_model_simple_pred(plant,rainfall, 25, Tend, W0, goal_func, 1)
goal_func = 1
outputs_25_1 <- run_model_simple_pred(plant,rainfall, 25, Tend, W0, goal_func, 1)

save(outputs_10_1, outputs_25_1, rainfall, plant, file = "simulation_150d.RData")
save(outputs_10_0, outputs_10_1, outputs_25_1, rainfall, plant, file = "simulation_150d.RData")
save(outputs_10_0, outputs_10_1, outputs_25_0, outputs_25_1, rainfall, plant, file = "simulation_150d.RData")
save(outputs_10_0, outputs_10_50, outputs_10_1, outputs_25_0, outputs_25_1, rainfall, plant, file = "simulation_150d.RData")
save(outputs_10_0, outputs_10_50, outputs_10_1, outputs_25_0, outputs_25_50, outputs_25_1, rainfall, plant, file = "simulation_150d.RData")

save.image("sim7.RData")
