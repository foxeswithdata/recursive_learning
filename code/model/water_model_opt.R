find_mid_ts <- function(plant, W0, Tend, deltat = 1){
  maxM_ts <- find_opt_ts(plant, W0, Tend, kf=1, deltat)
  maxS_ts <- find_opt_ts(plant, W0, Tend, kf=0, deltat)
  
  outmaxM <- plant_out_df(plant, W0 = Water_input_total, ts = maxM_ts, Tend = Tend, kf = 1, deltat = 1)
  outmaxS <- plant_out_df(plant, W0 = Water_input_total, ts = maxS_ts, Tend = Tend, kf = 0, deltat = 1)
  
  maxM_st <-outmaxM$carbon_pool_val[outmaxM$time == max(outmaxM$time) & outmaxM$carbon_pool_type == "Storage"]
  maxM_bi <-outmaxM$carbon_pool_val[outmaxM$time == max(outmaxM$time) & outmaxM$carbon_pool_type == "Biomass"]
  maxS_st <-outmaxS$carbon_pool_val[outmaxS$time == max(outmaxS$time) & outmaxS$carbon_pool_type == "Storage"]
  maxS_bi <-outmaxS$carbon_pool_val[outmaxS$time == max(outmaxS$time) & outmaxS$carbon_pool_type == "Biomass"]
  
  find_mid <- function(x){
    difference <- (x * maxM_bi + (1-x) * maxM_st) - (x * maxS_bi + (1-x) * maxS_st)
    return(difference)
  }
  
  out <- uniroot(find_mid, c(0,1))
  return(out$root)
}

find_opt_ts <- function(plant, W0, Tend, kf, deltat = 1){
  plant$W = W0
  plant$W0 = W0
  plant$Tend = Tend
  
  t_seq = seq(from=0, to=Tend, by=deltat)
  
  simlength <- length(t_seq)
  
  S_out <- vector(length=simlength)
  M_out <- vector(length=simlength)
  
  k=1
  for (i in t_seq){
    # Find the output values of simulation for the specific ts value
    
    out <- simple_water_model_sim_time_breaks_decay(plant, i, deltat)
    
    # Save the values of S[Tend], M[Tend], W[Tend] and total photosynthesis A
    S_out[k] <- out$S[simlength]
    M_out[k] <- out$M[simlength]
    
    if(sum(out$S<0) > 0){
      S_out[k] <- NA
      M_out[k] <- NA
    }
    
    k = k+1
  }
  
  ms_sum <- colSums(rbind (kf * M_out, (1 - kf) * S_out))
  ts_ind <- which.max(ms_sum)
  ts_out = t_seq[ts_ind]
  
  return(ts_out)
}

plant_out_df <- function(plant, W0, ts, Tend, kf, deltat = 1){
  
  plant$W = W0
  plant$W0 = W0
  plant$Tend = Tend
  out <- simple_water_model_sim_time_breaks_decay(plant, ts, deltat)
  t_seq <- out$t
  simlength = length(t_seq)
  tcrit_out <- t_seq[which.min(out$W)]
  
  out_df <- data.frame(carbon_pool_type = as.factor(rep(c("Storage", "Biomass"), each = simlength)),
                              carbon_pool_val = c(out$S, out$M),
                              water_pool_val = rep(out$W, times = 2),
                              water_pool_type = as.factor(rep("Soil Water Available", times = simlength * 2)),
                              initial_water = rep(plant$W0, simlength * 2),
                              ts = (rep(ts, times = simlength * 2)),
                              tcrit = (rep(tcrit_out, times = simlength * 2)),
                              goal = rep(kf, simlength * 2),
                              time = rep(t_seq, times = 2))
  return(out_df)
}

simple_water_model_sim <- function(M0, S0, W0, Tend, kp, kr, u, wue){
  # INITIALISE OUTPUT VECTORS
  # browser() 
  M <- rep(M0, times=Tend)
  S <- vector(length=Tend)
  WU <- vector(length=Tend)
  W <- vector(length=Tend)
  A <- vector(length=Tend)
  
  # INITIALISE FIRST VALUE
  M[1] = M0 + S0*u[1]
  A[1] = kp * M0
  WU[1] = A[1] * wue;
  if(WU[1] > W0){
    A[1] = 0;
    WU[1] = 0;
    kp = 0;
  }
  W[1] = W0 - WU[1]
  S[1] = S0 + A[1] - M0 * kr - S0*u[1]
  
  # RUN MODEL FOR REST OF THE PERIOD
  for(i in 2:Tend){
    A[i] = kp * M[i-1]
    WU[i] = A[i] * wue;
    if(WU[i] > W[i-1]){
      WU[i] = W[i-1];
      A[i] = W[i]/wue;
      kp = 0;
    }
    W[i] = W[i-1] - WU[i]
    M[i] = M[i-1] + S[i-1]*u[i]
    S[i] = S[i-1] + A[i] - M[i-1] * kr - S[i-1]*u[i]
    if(S[i] < 0){
      break;
    }
  }
  return(list(S=c(S0,S),M=c(M0, M),A=c(0,A), WU = c(0,WU), W=c(W0,W), t=0:Tend))
}

simple_water_model_sim_time_breaks <- function(param, ts, deltat){
  
  # INITIALISE Timeline settings
  t = seq(from=0, to=param$Tend, by=deltat)
  simlength = length(t)
  # delta t = h
  h = param$Tend/(simlength-1)
  h2 = h/2
  
  # INITIALISE OUTPUT VECTORS
  M <- vector(length=simlength)
  S <- vector(length=simlength)
  WU <- vector(length=simlength)
  W <- vector(length=simlength)
  A <- vector(length=simlength)
  u <- c(rep(param$ks, times = sum(t<=ts)), rep(0, times = sum(t>ts)))
  
  # INITIALISE FIRST VALUE
  M[1] = param$M0
  A[1] = 0
  WU[1] = 0
  W[1] = param$W0
  S[1] = param$S0
  
  # RUN MODEL FOR REST OF THE PERIOD
  for(i in 2:simlength){
    if(W[i-1] <=0){
      kp = 0
    }
    else{
      kp = param$kp
    }
    k1a = kp * M[i-1]
    k1wu = kp * M[i-1] * param$kw
    k1m = u[i-1] * S[i-1]
    k1s = ((kp - param$kr) * M[i-1] - u[i-1]*S[i-1])
    k1w = - kp * M[i-1] * param$kw
    
    k2a = (kp * (M[i-1] + h2*k1m))
    k2wu = (kp * (M[i-1] + h2*k1m) * param$kw)
    k2m = ((0.5 * (u[i-1] + u[i])) * (S[i-1] + h2*k1s))
    k2s = ((kp - param$kr) * (M[i-1] + h2*k1m) - (0.5*(u[i-1] + u[i])) * (S[i-1] + h2*k1s)) # might need to play around a bit with u
    k2w = - (kp * (M[i-1] + h2*k1m) * param$kw)
    
    k3a = (kp * (M[i-1] + h2*k2m))
    k3wu = (kp * (M[i-1] + h2*k2m) * param$kw)
    k3m = ((0.5 * (u[i-1] + u[i])) * (S[i-1] + h2*k2s))
    k3s = ((kp - param$kr) * (M[i-1] + h2*k2m) - (0.5 * (u[i-1] + u[i])) * (S[i-1] + h2*k2s)) # might need to play around a bit with u
    k3w = - (kp * (M[i-1] + h2*k2m) * param$kw)
    
    k4a = (kp * (M[i-1] + h*k3m))
    k4wu = (kp * (M[i-1] + h*k3m) * param$kw)
    k4m = (u[i] * (S[i-1] + h*k3s))
    k4s = ((kp - param$kr) * (M[i-1] +h*k3m) - u[i] * (S[i-1] + h*k3s)) # might need to play around a bit with u
    k4w = -(kp *(M[i-1] + h*k3m) * param$kw)
    
    A[i] = h/6 * (k1a + 2 * k2a + 2 * k3a + k4a) 
    WU[i] = h/6 * (k1wu + 2 * k2wu + 2 * k3wu + k4wu) 
    M[i] = M[i-1] + h/6 * (k1m + 2 * k2m + 2 * k3m + k4m)
    S[i] = S[i-1] + h/6 * (k1s + 2 * k2s + 2 * k3s + k4s)
    W[i] = W[i-1] + h/6 * (k1w + 2 * k2w + 2 * k3w + k4w)
    
  }
  return(list(S=S,M=M,A=A,WU=WU,W=W,t=t))
}

simple_water_model_sim_time_breaks_decay <- function(param, ts, deltat){
  
  # INITIALISE Timeline settings
  t = seq(from=0, to=param$Tend, by=deltat)
  simlength = length(t)
  # delta t = h
  h = param$Tend/(simlength-1)
  h2 = h/2
  
  # INITIALISE OUTPUT VECTORS
  M <- vector(length=simlength)
  S <- vector(length=simlength)
  WU <- vector(length=simlength)
  W <- vector(length=simlength)
  A <- vector(length=simlength)
  u <- c(rep(param$ks, times = sum(t<=ts)), rep(0, times = sum(t>ts)))
  
  # INITIALISE FIRST VALUE
  M[1] = param$M0
  A[1] = 0
  WU[1] = 0
  W[1] = param$W0
  S[1] = param$S0
  
 
  # RUN MODEL FOR REST OF THE PERIOD
  for(i in 2:simlength){
    if(W[i-1] <=0){
      kp = 0
    }
    else{
      kp = param$kp
    }
    k1a = kp * M[i-1]
    k1wu = kp * M[i-1] * param$kw
    k1m = u[i-1] * S[i-1] - param$kk * M[i-1]
    k1s = ((kp - param$kr) * M[i-1] - u[i-1]*S[i-1])
    k1w = - kp * M[i-1] * param$kw
    
    k2a = (kp * (M[i-1] + h2*k1m))
    k2wu = (kp * (M[i-1] + h2*k1m) * param$kw)
    k2m = ((0.5 * (u[i-1] + u[i])) * (S[i-1] + h2*k1s)) - (param$kk * (M[i-1] + h2 * k1m))
    k2s = ((kp - param$kr) * (M[i-1] + h2*k1m) - (0.5*(u[i-1] + u[i])) * (S[i-1] + h2*k1s)) # might need to play around a bit with u
    k2w = - (kp * (M[i-1] + h2*k1m) * param$kw)
    
    k3a = (kp * (M[i-1] + h2*k2m))
    k3wu = (kp * (M[i-1] + h2*k2m) * param$kw)
    k3m = ((0.5 * (u[i-1] + u[i])) * (S[i-1] + h2*k2s)) - (param$kk * (M[i-1] + h2 * k2m))
    k3s = ((kp - param$kr) * (M[i-1] + h2*k2m) - (0.5 * (u[i-1] + u[i])) * (S[i-1] + h2*k2s)) # might need to play around a bit with u
    k3w = - (kp * (M[i-1] + h2*k2m) * param$kw)
    
    k4a = (kp * (M[i-1] + h*k3m))
    k4wu = (kp * (M[i-1] + h*k3m) * param$kw)
    k4m = (u[i] * (S[i-1] + h*k3s)) - (param$kk * (M[i-1] + h * k3m))
    k4s = ((kp - param$kr) * (M[i-1] +h*k3m) - u[i] * (S[i-1] + h*k3s)) # might need to play around a bit with u
    k4w = -(kp *(M[i-1] + h*k3m) * param$kw)
    
    A[i] = h/6 * (k1a + 2 * k2a + 2 * k3a + k4a) 
    WU[i] = h/6 * (k1wu + 2 * k2wu + 2 * k3wu + k4wu) 
    M[i] = M[i-1] + h/6 * (k1m + 2 * k2m + 2 * k3m + k4m)
    S[i] = S[i-1] + h/6 * (k1s + 2 * k2s + 2 * k3s + k4s)
    W[i] = W[i-1] + h/6 * (k1w + 2 * k2w + 2 * k3w + k4w)
    
  }
  return(list(S=S,M=M,A=A,WU=WU,W=W,t=t))
}

simple_water_model_adjuncts_sim_time_breaks <- function(param, lambda, ts, tcrit, mu, deltat){
  
  # INITIALISE Timeline settings
  t = seq(from=0, to=param$Tend, by=deltat)
  simlength = length(t)
  # delta t = h
  h = param$Tend/(simlength-1)
  h2 = h/2
  
  # INITIALISE OUTPUT VECTORS
  lamM <- vector(length=simlength)
  lamS <- vector(length=simlength)
  lamW <- vector(length=simlength)
  u <- c(rep(param$ks, times = sum(t<=ts)), rep(0, times = sum(t>ts)))
  kp <- c(rep(param$kp, times = sum(t<=tcrit)), rep(0, times = sum(t>tcrit)))
  
  # INITIALISE LAST VALUE
  lamM[simlength] = lambda[1]
  lamS[simlength] = lambda[2]
  lamW[simlength] = lambda[3]
  
  # RUN MODEL FOR REST OF THE PERIOD
  for(j in (simlength-1):1){
    mu_p1 <- mu
    if(kp[j+1] >0){
      mu_p1 = 0
    }
    mu_p0 <- mu
    if(kp[j] > 0){
      mu_p0 = 0
    }
    
    km1 = -lamS[j+1] * (kp[j+1] - param$kr) + kp[j+1] * param$kw  * lamW[j+1];
    ks1 = (lamS[j+1] - lamM[j+1]) * u[j+1];
    kw1 = mu_p1
    
    km2 = -(lamS[j+1]-h2*ks1) * (0.5*(kp[j+1]+kp[j]) - param$kr) - 0.5*(kp[j+1]+kp[j]) * param$kw * (lamW[j+1]-h2*kw1);
    ks2 = ((lamS[j+1]-h2*ks1) - (lamM[j+1]-h2*km1)) * 0.5 * (u[j]+u[j+1]);
    kw2 = 0.5 * (mu_p1 + mu_p0)
    
    km3 = -(lamS[j+1]-h2*ks2) * (0.5*(kp[j+1]+kp[j]) - param$kr) - 0.5*(kp[j+1]+kp[j]) * param$kw * (lamW[j+1]-h2*kw2);
    ks3 = ((lamS[j+1]-h2*ks2) - (lamM[j+1]-h2*km2)) * 0.5 * (u[j]+u[j+1]);
    kw3 = 0.5 * (mu_p1 + mu_p0)
    
    km4 = -(lamS[j+1]-h*ks3) * (kp[j] - param$kr) - kp[j] * param$kw * (lamW[j+1]-h*kw3);
    ks4 = ((lamS[j+1]-h*ks3) - (lamM[j+1]-h*km3)) * u[j];
    kw4 = mu_p0
    
    lamM[j] = lamM[j+1] - (h/6)*(km1 + 2*km2 + 2*km3 + km4);
    lamS[j] = lamS[j+1] - (h/6)*(ks1 + 2*ks2 + 2*ks3 + ks4);
    lamW[j] = lamW[j+1] - (h/6)*(kw1 + 2*kw2 + 2*kw3 + kw4);
    
  }
  return(list(lamM = lamM, lamS = lamS, lamW= lamW, u = u, kp = kp, t=t))
}






water_model_log <- function(param, ts, deltat){
  
  # INITIALISE Timeline settings
  t = seq(from=0, to=param$Tend, by=deltat)
  simlength = length(t)
  # delta t = h
  h = param$Tend/(simlength-1)
  h2 = h/2
  
  # INITIALISE OUTPUT VECTORS
  M <- vector(length=simlength)
  S <- vector(length=simlength)
  WU <- vector(length=simlength)
  W <- vector(length=simlength)
  A <- vector(length=simlength)
  u <- c(rep(param$ks, times = sum(t<=ts)), rep(0, times = sum(t>ts)))
  
  # INITIALISE FIRST VALUE
  M[1] = param$M0
  A[1] = 0
  WU[1] = 0
  W[1] = param$W0
  S[1] = param$S0
  
  # RUN MODEL FOR REST OF THE PERIOD
  for(i in 2:simlength){
    k1p = (param$kp / (1 + exp(-param$klog * (W[i-1] - param$w50))))
    k1a = k1p  * M[i-1]
    k1wu = k1p * M[i-1] * param$kw
    k1m = u[i-1] * S[i-1]
    k1s = ((k1p - param$kr) * M[i-1] - u[i-1]*S[i-1])
    k1w = - k1p * M[i-1] * param$kw
    
    k2p = (param$kp / (1 + exp(-param$klog * ((W[i-1] + h2 * k1w) - param$w50))))
    k2a = (k2p * (M[i-1] + h2*k1m))
    k2wu = (k2p * (M[i-1] + h2*k1m) * param$kw)
    k2m = ((0.5 * (u[i-1] + u[i])) * (S[i-1] + h2*k1s))
    k2s = ((k2p - param$kr) * (M[i-1] + h2*k1m) - (0.5*(u[i-1] + u[i])) * (S[i-1] + h2*k1s)) # might need to play around a bit with u
    k2w = - (k2p * (M[i-1] + h2*k1m) * param$kw)
    
    k3p = (param$kp / (1 + exp(-param$klog * ((W[i-1] + h2 * k2w) - param$w50))))
    k3a = (k3p * (M[i-1] + h2*k2m))
    k3wu = (k3p * (M[i-1] + h2*k2m) * param$kw)
    k3m = ((0.5 * (u[i-1] + u[i])) * (S[i-1] + h2*k2s))
    k3s = ((k3p - param$kr) * (M[i-1] + h2*k2m) - (0.5 * (u[i-1] + u[i])) * (S[i-1] + h2*k2s)) # might need to play around a bit with u
    k3w = - (k3p * (M[i-1] + h2*k2m) * param$kw)
    
    k4p = (param$kp / (1 + exp(-param$klog * ((W[i-1] + h * k3w) - param$w50))))
    k4a = (k4p * (M[i-1] + h*k3m))
    k4wu = (k4p * (M[i-1] + h*k3m) * param$kw)
    k4m = (u[i] * (S[i-1] + h*k3s))
    k4s = ((k4p - param$kr) * (M[i-1] +h*k3m) - u[i] * (S[i-1] + h*k3s)) # might need to play around a bit with u
    k4w = -(k4p *(M[i-1] + h*k3m) * param$kw)
    
    A[i] = h/6 * (k1a + 2 * k2a + 2 * k3a + k4a) 
    WU[i] = h/6 * (k1wu + 2 * k2wu + 2 * k3wu + k4wu) 
    M[i] = M[i-1] + h/6 * (k1m + 2 * k2m + 2 * k3m + k4m)
    S[i] = S[i-1] + h/6 * (k1s + 2 * k2s + 2 * k3s + k4s)
    W[i] = W[i-1] + h/6 * (k1w + 2 * k2w + 2 * k3w + k4w)
    
  }
  return(list(S=S,M=M,A=A,WU=WU,W=W,t=t,u=u))
}
