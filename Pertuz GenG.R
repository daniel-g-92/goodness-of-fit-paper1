### UPDATED SIMULAITON APPROACH ### June 2019


# the major change is how the results are stored. All estimates will be stored without any selection of best fitting models being done
# this is the bug spotting version of the code, without suppressing of any errors or warnings

## setup for exponential and dacomitinib


# prelim

setwd("//gobo/USER40/u/u1474467/Documents/PHD/AIC paper/Trials/digitised data/output")
path<-"//gobo/USER40/u/u1474467/Documents/PHD/AIC paper/Trials/digitised data/"
library("flexsurv", lib.loc="\\\\GOBO/USER40/u/u1474467/Documents/R/win-library/3.5")
library("survminer", lib.loc="\\\\GOBO/USER40/u/u1474467/Documents/R/win-library/3.5")
h <- function(w) if( any( grepl( "NaNs",w) ) ) invokeRestart("muffleWarning") # this creates the funciton to ignore NaNs

options(warn=1) # get warnings to appear when they occur
set.seed(147)

# open target data + set trial specific values
##### parameters specfic to a trial
sampsize <- 2400   # trial sample size. # change for each trial &&&
pertimemax <- 48    # max followup time  # change for each trial &&&
perrectimemax <- 21   # length of recruitment period # change for each trial &&&
timehorizon <- 624    # time horizon of economic model # change for each trial based on time horizon &&&
lambdarec <- 0.1      # change for each trial, exponential parameter for recruitment &&&
lambdacens <- 0.002233 # lambda of low underlying censoring rate
dpertu <- read.delim("//gobo/USER40/u/u1474467/Documents/PHD/AIC paper/Trials/digitised data/IPD pertuz.txt") #change &&&



# fit required model to obtain parameters
fit <- surv_fit(Surv(V1, V2) ~ 1, data=dpertu )
exper <- flexsurvreg(Surv(V1, V2) ~ 1, data=dpertu, dist = "gengamma") #### change here if alternate dist and lines 49-50 + 210, 211 215 &&&


## set additional parameters here

nsim <- 10000       # number of simulations to run.
pars <- matrix(, nrow=8, ncol=12)
results <- data.frame(id = 1:nsim)   # start of results matrix


# step 1 - estimate true mean and create results matrix
closeAllConnections()
console_output <- file("Pertuz_GenG_Log.txt") ## change &&&


results$truemean <- rmst_gengamma(timehorizon,mu = exper$res[1,1], sigma = exper$res[2,1], Q = exper$res[3,1])    #### change if using alternate distribution &&&
results$truedist <- "GenG"                             #### change if using alternate distribution &&&
results$nocensaicexp <- NA # this is column 4 of results
results$nocensbicexp <- NA
results$nocensllexp <- NA
results$nocensmeanexp <- NA
results$nocensmedexp <- NA
results$nocensaicwei <- NA
results$nocensbicwei <- NA # this is column 10
results$nocensllwei <- NA
results$nocensmeanwei <- NA
results$nocensmedwei <- NA
results$nocensaiclnorm <- NA
results$nocensbiclnorm <- NA
results$nocenslllnorm <- NA
results$nocensmeanlnorm <- NA
results$nocensmedlnorm <- NA
results$nocensaicllog <- NA
results$nocensbicllog <- NA # this is column 20
results$nocensllllog <- NA
results$nocensmeanllog <- NA
results$nocensmedllog <- NA
results$nocensaicgam <- NA
results$nocensbicgam <- NA
results$nocensllgam <- NA
results$nocensmeangam <- NA
results$nocensmedgam <- NA
results$nocensaicgeng <- NA
results$nocensbicgeng <- NA # this is column 30
results$nocensllgeng <- NA
results$nocensmeangeng <- NA
results$nocensmedgeng <- NA
results$nocensaicgomp <- NA
results$nocensbicgomp <- NA
results$nocensllgomp <- NA
results$nocensmeangomp <- NA
results$nocensmedgomp <- NA
results$nocensaicgenf <- NA
results$nocensbicgenf <- NA # this is column 40
results$nocensllgenf <- NA
results$nocensmeangenf <- NA
results$nocensmedgenf <- NA

results$censaicexp <- NA
results$censbicexp <- NA
results$censllexp <- NA
results$censmeanexp <- NA
results$censmedexp <- NA
results$censaicwei <- NA
results$censbicwei <- NA # this is column 50
results$censllwei <- NA
results$censmeanwei <- NA
results$censmedwei <- NA
results$censaiclnorm <- NA
results$censbiclnorm <- NA
results$censlllnorm <- NA
results$censmeanlnorm <- NA
results$censmedlnorm <- NA
results$censaicllog <- NA
results$censbicllog <- NA # this is column 60
results$censllllog <- NA
results$censmeanllog <- NA
results$censmedllog <- NA
results$censaicgam <- NA
results$censbicgam <- NA
results$censllgam <- NA
results$censmeangam <- NA
results$censmedgam <- NA
results$censaicgeng <- NA
results$censbicgeng <- NA # this is column 70
results$censllgeng <- NA
results$censmeangeng <- NA
results$censmedgeng <- NA
results$censaicgomp <- NA
results$censbicgomp <- NA
results$censllgomp <- NA
results$censmeangomp <- NA
results$censmedgomp <- NA
results$censaicgenf <- NA
results$censbicgenf <- NA # this is column 80
results$censllgenf <- NA
results$censmeangenf <- NA
results$censmedgenf <- NA

results$trialaicexp <- NA
results$trialbicexp <- NA
results$trialllexp <- NA
results$trialmeanexp <- NA
results$trialmedexp <- NA
results$trialaicwei <- NA
results$trialbicwei <- NA # this is column 90
results$trialllwei <- NA
results$trialmeanwei <- NA
results$trialmedwei <- NA
results$trialaiclnorm <- NA
results$trialbiclnorm <- NA
results$triallllnorm <- NA
results$trialmeanlnorm <- NA
results$trialmedlnorm <- NA
results$trialaicllog <- NA
results$trialbicllog <- NA # this is column 100
results$trialllllog <- NA
results$trialmeanllog <- NA
results$trialmedllog <- NA
results$trialaicgam <- NA
results$trialbicgam <- NA
results$trialllgam <- NA
results$trialmeangam <- NA
results$trialmedgam <- NA
results$trialaicgeng <- NA
results$trialbicgeng <- NA # this is column 110
results$trialllgeng <- NA
results$trialmeangeng <- NA
results$trialmedgeng <- NA
results$trialaicgomp <- NA
results$trialbicgomp <- NA
results$trialllgomp <- NA
results$trialmeangomp <- NA
results$trialmedgomp <- NA
results$trialaicgenf <- NA
results$trialbicgenf <- NA # this is column 120
results$trialllgenf <- NA
results$trialmeangenf <- NA
results$trialmedgenf <- NA

results$exp_rate_1 <- NA # this is column 124
results$exp_rate_2 <- NA
results$exp_rate_3 <- NA
results$wei_shape_1 <- NA # this is column 127
results$wei_scale_1 <- NA
results$wei_shape_2 <- NA
results$wei_scale_2 <- NA
results$wei_shape_3 <- NA
results$wei_scale_3 <- NA
results$geng_mu_1 <- NA # this is column 133
results$geng_sigma_1 <- NA 
results$geng_Q_1 <- NA
results$geng_mu_2 <- NA
results$geng_sigma_2 <- NA 
results$geng_Q_2 <- NA
results$geng_mu_3 <- NA
results$geng_sigma_3 <- NA 
results$geng_Q_3 <- NA

results$true_param_1 <- NA # this is column 142
results$true_param_2 <- NA
results$true_param_3 <- NA  

results$sim_median_1 <- NA


########################################################################
parafits <- c("exponential", "weibull", "lnorm", "llog", "gamma", "gengamma", "gompertz", "genf") # 
#these names match the commands to calculate the means survival.
meanfits <- c("rmst_exp", "rmst_weibull", "rmst_lnorm",  "rmst_llogis", "rmst_gamma", "rmst_gengamma", "rmst_gompertz", "rmst_genf") # 


for (k in 1:nsim){ 
  
  #change for each starting distribution
  recper <- log(1 + (exp(perrectimemax*lambdarec)-1)*runif(sampsize))/lambdarec 
  results[k,142] <- exper$res[1,1] # change for each starting distribution 
  results[k,143] <- exper$res[2,1]
  results[k,144] <- exper$res[3,1]
  timecens <- rexp(sampsize, lambdacens)
  
  #### starting with data setup ####
  
  # set up for  run 1 - no censoring
  survperexp <- data.frame(rgengamma(sampsize, mu = exper$res[1,1], sigma = exper$res[2,1], Q = exper$res[3,1]), recper,timecens) # change for each distribution
  colnames(survperexp) <- c("stime1", "rtime", "ctime")
  survperexp$run1ev1cens0 <- 1
  survmean <- c(0,0,0,0,0,0,0,0)
  km_fit1 <- survfit(Surv(stime1, run1ev1cens0)~1, data=survperexp)
  sim_median <- as.data.frame(surv_median(km_fit1))[2]
  results[k,145] <- sim_median
  
  
  # set up for for run 2 - random loss to followup
  survperexp$stime2 <-  survperexp$stime1
  survperexp$run2ev1cens0[survperexp$ctime<survperexp$stime1] <- 0
  survperexp$run2ev1cens0[survperexp$ctime>=survperexp$stime1] <- 1
  survperexp$stime2[survperexp$run2ev1cens0==0] <- survperexp$ctime[survperexp$run2ev1cens0==0]
  
  # set up for run 3 - trial censoring
  survperexp$stime3 <-  survperexp$stime2
  survperexp$run3ev1cens0 <- survperexp$run2ev1cens0
  survperexp$ftime <- pertimemax - survperexp$rtime  
  survperexp$run3ev1cens0[survperexp$ftime < survperexp$stime3] <- 0
  survperexp$stime3[survperexp$ftime < survperexp$stime3] <- survperexp$ftime[survperexp$ftime < survperexp$stime3]
  
  
  ## model fitting for run 1
  
  for (i in 1:8) {
    if (k == 1 & i==1) {
      sink(console_output,split = F)
      sink(console_output, type = "message", split = F)
    } 
    #   print(c( k, 1, i))
    tryCatch(expr = {
      m1 <- flexsurvreg(Surv(stime1, run1ev1cens0) ~ 1, data=survperexp, dist = parafits[i], sr.control = survreg.control(maxit = 1000))
      if (i == 1) {
        results[k,124] <- m1$res[1,1]
      }
      if (i == 2){
        results[k,127] <- m1$res[1,1]
        results[k,128] <- m1$res[2,1]
      }
      if (i == 6){
        results[k,133] <- m1$res[1,1]
        results[k,134] <- m1$res[2,1]
        results[k,135] <- m1$res[3,1]
      }
      
      results[k,5*i-1] <- m1$AIC
      results[k,5*i] <- log(m1$N)*m1$npars - 2*m1$loglik  
      results[k,5*i+1] <- -m1$loglik
      pars[i,] <- m1$res[,1]
      if (i != 8) {
        med1 <- as.data.frame(summary(m1, type = "median"))[1]
      }
      results[k,5*i+3] <- med1
      if (m1$npars == 4){
        P1 <- pars[i,1]
        P2 <- pars[i,2]
        P3 <- pars[i,3]
        P4 <- pars[i,4]
        try(results[k,5*i+2] <- get(paste(meanfits[i]))(timehorizon,P1,P2,P3,P4))
      }
      if (m1$npars == 3){
        P1 <- pars[i,1]
        P2 <- pars[i,2]
        P3 <- pars[i,3]
        try(results[k,5*i+2] <- get(paste(meanfits[i]))(timehorizon,P1,P2,P3))
      }
      if (m1$npars == 2){
        P1 <- pars[i,1]
        P2 <- pars[i,2]
        try(results[k,5*i+2] <- get(paste(meanfits[i]))(timehorizon,P1,P2))
      }
      if (m1$npars == 1){
        P1 <- pars[i,1]
        try(results[k,5*i+2] <- get(paste(meanfits[i]))(timehorizon,P1))
      }
      
    }, 
    error=function(e){ 
      message("Error"," ",k,"  ", 1, "  ", i)
      message(e)
      return(NA)
    }, 
    warning=function(w){
      message("Warning"," ", k,"  ", 1, "  ", i)
      message(w)
    },
    finally = {
    } )
    
    
    rm(m1)}
  
  ## model fitting for run 2
  for (i in 1:8) {
    
    #   print(c(k, 2, i))
    tryCatch({
      if (i!=5){
        m2 <- flexsurvreg(Surv(stime2, run2ev1cens0) ~ 1, data=survperexp, dist = parafits[i], sr.control = survreg.control(maxit = 1000))
      }
      if (i==5){
        m2 <-  withCallingHandlers(flexsurvreg(Surv(stime2, run2ev1cens0) ~ 1, data=survperexp, dist = parafits[i], sr.control = survreg.control(maxit = 1000)),warning = h)
      }
      if (i == 1) {
        results[k,125] <- m2$res[1,1]
      }
      if (i ==2){
        results[k,129] <- m2$res[1,1]
        results[k,130] <- m2$res[2,1]
      }
      if (i == 6){
        results[k,136] <- m2$res[1,1]
        results[k,137] <- m2$res[2,1]
        results[k,138] <- m2$res[3,1]
      }
      
      results[k,5*i+39] <- m2$AIC
      results[k,5*i+40] <- log(m2$N)*m2$npars - 2*m2$loglik  
      results[k,5*i+41] <- -m2$loglik
      pars[i,] <- m2$res[,1]
      med2 <- NA
      if (i != 8) {
        med2 <- as.data.frame(summary(m2, type = "median"))[1]
      }
      results[k,5*i+43] <- med2
      
      if (m2$npars == 4){
        P1 <- pars[i,1]
        P2 <- pars[i,2]
        P3 <- pars[i,3]
        P4 <- pars[i,4]
        try(results[k,5*i+42] <- get(paste(meanfits[i]))(timehorizon,P1,P2,P3,P4))
      }
      if (m2$npars == 3){
        P1 <- pars[i,1]
        P2 <- pars[i,2]
        P3 <- pars[i,3]
        try(results[k,5*i+42] <- get(paste(meanfits[i]))(timehorizon,P1,P2,P3))
      }
      if (m2$npars == 2){
        P1 <- pars[i,1]
        P2 <- pars[i,2]
        try(results[k,5*i+42] <- get(paste(meanfits[i]))(timehorizon,P1,P2))
      }
      if (m2$npars == 1){
        P1 <- pars[i,1]
        try(results[k,5*i+42] <- get(paste(meanfits[i]))(timehorizon,P1))
      }
    },
    error=function(e){ 
      message("Error"," ",k,"  ", 2, "  ", i)
      message(e)
      return(NA)
    }, 
    warning=function(w){
      message("Warning"," ", k,"  ", 2, "  ", i)
      message(w)
    },
    finally = {
    } )
    
    
    rm(m2) }
  
  ## model fitting for run 3
  for (i in 1:8) {
    
    #   print(c(k, 3, i))
    
    tryCatch({
      if (i!=5){
        m3 <- flexsurvreg(Surv(stime3, run3ev1cens0) ~ 1, data=survperexp, dist = parafits[i], sr.control = survreg.control(maxit = 1000))
      }
      if (i==5){
        m3 <-  withCallingHandlers(flexsurvreg(Surv(stime3, run3ev1cens0) ~ 1, data=survperexp, dist = parafits[i], sr.control = survreg.control(maxit = 1000)),warning = h)
      }
      if (i == 1){
        results[k,126] <- m3$res[1,1]
      }
      if (i ==2){
        results[k,131] <- m3$res[1,1]
        results[k,132] <- m3$res[2,1]
      }
      if (i == 6){
        results[k,139] <- m3$res[1,1]
        results[k,140] <- m3$res[2,1]
        results[k,141] <- m3$res[3,1]
      }
      
      results[k,5*i+79] <- m3$AIC
      results[k,5*i+80] <- log(m3$N)*m3$npars - 2*m3$loglik  
      results[k,5*i+81] <- -m3$loglik
      pars[i,] <- m3$res[,1]
      med3 <- NA
      if (i != 8) {
        med3 <- as.data.frame(summary(m3, type = "median"))[1]
      }
      results[k,5*i+83] <- med3
      
      if (m3$npars == 4){
        P1 <- pars[i,1]
        P2 <- pars[i,2]
        P3 <- pars[i,3]
        P4 <- pars[i,4]
        try(results[k,5*i+82] <- get(paste(meanfits[i]))(timehorizon,P1,P2,P3,P4))
      }
      if (m3$npars == 3){
        P1 <- pars[i,1]
        P2 <- pars[i,2]
        P3 <- pars[i,3]
        try(results[k,5*i+82] <- get(paste(meanfits[i]))(timehorizon,P1,P2,P3))
      }
      if (m3$npars == 2){
        P1 <- pars[i,1]
        P2 <- pars[i,2]
        try(results[k,5*i+82] <- get(paste(meanfits[i]))(timehorizon,P1,P2))
      }
      if (m3$npars == 1){
        P1 <- pars[i,1]
        try(results[k,5*i+82] <- get(paste(meanfits[i]))(timehorizon,P1))
      }
    }, 
    error=function(e){ 
      message("Error"," ",k,"  ", 3, "  ", i)
      message(e)
      return(NA)
    }, 
    warning=function(w){
      message("Warning"," ", k,"  ", 3, "  ", i)
      message(w)
    },
    finally = {
    } )
    rm(m3) }
  
}

write.csv(results,"Pertuz_GenG_Results.csv")

