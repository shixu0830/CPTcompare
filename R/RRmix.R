RRmix <-
function(dat){
  ccc = as.numeric(dat$CPT[dat$site==1])
  fit= glmer(count ~ site + offset(log(offset)) + (1+ site | block/CPT),
             family = "poisson", data = dat)
  lp_mix = predict(fit)
  
  off1 = dat$offset[which(dat$site==1)][1]
  off0 = dat$offset[which(dat$site==0)][1]
  rrr_mix = (lp_mix[dat$site==1] - log(off1))  -
    (lp_mix[dat$site==0] - log(off0))
  return(rrr_mix)
}
