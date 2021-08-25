load("out/simulations/check_strategy/rainfall358735a45ed.RData")

Water_input_total = sapply(rainfall_list, sum) + sim_test$plant$W
plant <- sim_test$plant
Tend = 249
kf = c(0, 0.45, 0.5, 0.55, 0.6, 0.65, 0.75, 1)

out_optim <- plyr::rbind.fill(lapply(Water_input_total, (function(water_in){
  return(plyr::rbind.fill(lapply(kf, function(kf_val){
    ts_opt <- find_opt_ts(plant, water_in, Tend, kf_val, deltat = 1)
    out_plant <- plant_out_df(plant, W0 = water_in, ts = ts_opt, Tend = Tend, kf = kf_val, deltat = 1)
    out_plant$goal_func = kf_val
    out_plant$k = which(Water_input_total == water_in)
    return(out_plant)
  })))
})))

