library(nloptr)


new_plant <- function(S0 = 550,
                      M0 = 3300,
                      kp = 0.014,
                      ks = 0.06,
                      kr = 0.004,
                      kw = 0.4,
                      kk = 0.00001,
                      kd = 0,
                      W0 = 1000){
  structure(list(kp = kp,
                 kr = kr,
                 kw = kw,
                 ks = ks,
                 S0 = S0,
                 M0 = M0,
                 kk = kk,
                 S = S0,
                 M = M0,
                 W = W0,
                 E = 0), 
            class = "plant_class")
}

optim_alloc <- function(plant, ...){
  UseMethod("optim_alloc", plant)
}

optim_alloc.plant_class <- function(plant, rain_input, timewindow, goal_func, file_name, deltat){
  
  matlab_command <- paste("matlab", "-nodesktop", "-nosplash", "-r", "\"", 
                          "M0=", plant$M, ";",
                          "S0=", plant$S, ";",
                          "W0=", plant$W, ";",
                          "ks=", plant$ks, ";",
                          "kk=", plant$kk, ";",
                          "kr=", plant$kr, ";",
                          "kp=", plant$kp, ";",
                          "kw=", plant$kw, ";",
                          "kf=", goal_func, ";",
                          "tw=", timewindow, ";",
                          "ri=", rain_input, ";",
                          "delta_t=", deltat, ";",
                          "ks_start=", (plant$ks/2), ";",
                          "kd=", 0, ";",
                          "file_name=", paste0("'",file_name,"'", sep = "", collapse = ""), ";",
                          "command= 'code/matlab/call_MPC.m';",
                          "run(command);",
                          "exit;",
                          "\"")
  
  out <- system(matlab_command, intern = TRUE)
  # print(out)
  # print(paste0("code/matlab/", file_name, sep = "", collapse = ""))
  out_df <- read.csv(paste0("code/matlab/", file_name, sep = "", collapse = ""), header = FALSE, col.names =  c("ut", "biomass", "storage", "water",
                                                                                                                "ut.maxS", "biomass.maxS", "storage.maxS", "water.maxS",
                                                                                                                "ut.maxM", "biomass.maxM", "storage.maxM", "water.maxM"))
  return(out_df)
}

optim_alloc_memory <- function(plant, ...){
  UseMethod("optim_alloc_memory", plant)
}

optim_alloc_memory <- function(plant, rain_input, timewindow, goal_func, file_name, deltat){
  
  rain_command = paste0("[", paste(rain_input, collapse = ",")  ,"]", collapse = "")
  
  matlab_command <- paste("matlab", "-nodesktop", "-nosplash", "-r", "\"", 
                          "M0=", plant$M, ";",
                          "S0=", plant$S, ";",
                          "W0=", plant$W, ";",
                          "ks=", plant$ks, ";",
                          "kk=", plant$kk, ";",
                          "kr=", plant$kr, ";",
                          "kp=", plant$kp, ";",
                          "kw=", plant$kw, ";",
                          "kf=", goal_func, ";",
                          "tw=", timewindow, ";",
                          "ri=", rain_command, ";",
                          "delta_t=", deltat, ";",
                          "ks_start=", (plant$ks/2), ";",
                          "kd=", 0, ";",
                          "file_name=", paste0("'",file_name,"'", sep = "", collapse = ""), ";",
                          "command= 'code/matlab/call_MPC_rain_in.m';",
                          "run(command);",
                          "exit;",
                          "\"")
  
  out <- system(matlab_command, intern = TRUE)
  # print(out)
  # print(paste0("code/matlab/", file_name, sep = "", collapse = ""))
  out_df <- read.csv(paste0("code/matlab/", file_name, sep = "", collapse = ""), header = FALSE, col.names =  c("ut", "biomass", "storage", "water",
                                                                                                   "ut.maxS", "biomass.maxS", "storage.maxS", "water.maxS",
                                                                                                   "ut.maxM", "biomass.maxM", "storage.maxM", "water.maxM"))
  return(out_df)
}


apply_control <- function(plant,...){
  UseMethod("apply_control",plant)
}

apply_control.plant_class <- function(plant, ut, rainfall){
  R = plant$M * plant$kr;
  G = plant$S * ut; 
  P = ifelse(plant$W+rainfall >= plant$kp*plant$kw*plant$M, 
             yes = plant$kp * plant$M, 
             no = 0)
  E = P * plant$kw;
  K = plant$M * plant$kk;
  
  plant$W = plant$W + rainfall - E;
  plant$M = plant$M + G - K;
  plant$S = plant$S + P - R - G;
  plant$E = E;
  
  return(plant)
}

reset <- function(plant,...){
  UseMethod("reset",plant)
}

reset.plant_class <- function(plant, W0){
  plant$W = W0;
  plant$M = plant$M0;
  plant$S = plant$S0;
  return(plant)
}
