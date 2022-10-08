##########################################
###only for ARMA model!!!
##########################################
rm(list = ls()) 
setwd("~/Dropbox/Tjasulka/MAGISTERIJ/Bayes_2018/Trading_Bitcoin")
source("functions.R")
source("TrainTime.R")
source("TestTime.R")
source("source_trading.R")
source("betanalpha_functions.R")
# dev.off()

# input0 <- "predicted_ModelsARMA_bayes_a01598_trainTime4_dRange30_100_ARMApqARMA_1_2_delta80.rds"
# input0 <- "predicted_ModelsARMA_bayes_a01598_trainTime4_dRange30_100_ARMApqARMA_1_2_delta99.rds"
input0 <- "predicted_ModelsARMA_bayes_a01598_trainTime120_dRange30_50_80_100_ARMApqMA_1.rds"
# input0 <- NULL
n.chains.plot=1
model.num <- 4
Day=4 
m.name="MA_1"

# train.time0.plot = 4
# d_range1.x.plot = c(30,100)
# d_range1.xx = 30
# ARMApq.bayes.x.plot = list("ARMA_1_2")
# n.iter.plot=7000
# n.warmup.plot=4000
# delta.plot=0.99
# n.chains.plot=4
# Day=4 #4 for MA1, 1 for ARMA12
# m.name="ARMA_1_2"


if(m.name == "MA_1"){
  pars.1 <- c("mu","theta","sigma","predy")
}else if(m.name == "ARMA_1_2"){
  pars.1 <- c("alpha","phi","theta[1]","theta[2]","sigma","predy")
}
# train.time0 <- train.time0.plot
# d_range1.x <- d_range1.x.plot
# ARMApq.bayes.x <- ARMApq.bayes.x.plot
###
# n.iter <- n.iter.plot;
# n.warmup <- n.warmup.plot;
# delta <- delta.plot;
n.chains <- n.chains.plot

# if(is.null(input0)){
#   do_grid_search_ARMA_bayes <- TRUE
#   s.time0 <- Sys.time()
#   model.name <- "ARMA_bayes"
#   funk_run_trainTime(model.name)
#   s.time1 <- Sys.time()-s.time0; s.time1
# }
setwd("~/Dropbox/Tjasulka/MAGISTERIJ/Bayes_2018/Trading_Bitcoin/train_days")
r1 <- readRDS(input0)
names(r1)
setwd("./divergent_plots/")
###########################################################
# model.num <- 0
# a2 <- c()
# model.num <- model.num+1
# for(i in 1:train.time0.plot){
#   Day <- i
#   fit_cp.xxx <- r1[[model.num]][[Day]]
#   a1 <- check_div(fit_cp.xxx)
#   a2 <- rbind(a2,a1)
# }
# length(a2)
###########################################################
# model.num <- which(names(r1) == paste("TrainDays=",train.time0,
#                                       "_model=",m.name,"_d=",d_range1.xx,sep=""))
fit_cp <- r1[[model.num]][[Day]]
check_div(fit_cp)
###
#########################################################

params_cp <- as.data.frame(extract(fit_cp, permuted=FALSE))
posterior_cp <- as.array(fit_cp)

# available_mcmc(pattern = "_nuts_")
lp_cp <- log_posterior(fit_cp) #log posterior
np_cp <- nuts_params(fit_cp) #nuts
###
###
######################################################################################################
if(m.name %in% c("MA_1","MA_2")){
  Mu <- unlist(extract(fit_cp, 'mu'), use.names=FALSE) 
}else{
  Alpha <- unlist(extract(fit_cp, 'alpha'), use.names=FALSE)
}
##############
if("phi" %in% names(fit_cp)){
  Phi1 <- unlist(extract(fit_cp, 'phi'), use.names=FALSE)
}
if("phi[1]" %in% names(fit_cp)){
  Phi1 <- unlist(extract(fit_cp, 'phi[1]'), use.names=FALSE)
}
if("phi[2]" %in% names(fit_cp)){
  Phi2 <- unlist(extract(fit_cp, 'phi[2]'), use.names=FALSE)
}
if("theta" %in% names(fit_cp)){
  Theta1 <- unlist(extract(fit_cp, 'theta'), use.names=FALSE)
}
if("theta[1]" %in% names(fit_cp)){
  Theta1 <- unlist(extract(fit_cp, 'theta[1]'), use.names=FALSE)
}
if("theta[2]" %in% names(fit_cp)){
  Theta2 <- unlist(extract(fit_cp, 'theta[2]'), use.names=FALSE)
}
##############
Sigma <- unlist(extract(fit_cp, 'sigma'), use.names=FALSE)
y_pred <- unlist(extract(fit_cp, 'predy'), use.names=FALSE)
if(m.name %in% c("MA_1","MA_2")){
  x.lim.mu <- c(min(curve(dnorm(x, 0, 10))[1][[1]],density(Mu)[1]$x %>% min(),na.rm=TRUE),
                max(curve(dnorm(x, 0, 10))[1][[1]],density(Mu)[1]$x %>% max(),na.rm=TRUE))
  y.lim.mu <- c(min(curve(dnorm(x, 0, 10))[2][[1]],density(Mu)[2]$y %>% min(),na.rm=TRUE),
                max(curve(dnorm(x, 0, 10))[2][[1]],density(Mu)[2]$y %>% max(),na.rm=TRUE))
}else{
  x.lim.alpha <- c(min(curve(dnorm(x, 0, 10))[1][[1]],density(Alpha)[1]$x %>% min(),na.rm=TRUE),
                   max(curve(dnorm(x, 0, 10))[1][[1]],density(Alpha)[1]$x %>% max(),na.rm=TRUE))
  y.lim.alpha <- c(min(curve(dnorm(x, 0, 10))[2][[1]],density(Alpha)[2]$y %>% min(),na.rm=TRUE),
                   max(curve(dnorm(x, 0, 10))[2][[1]],density(Alpha)[2]$y %>% max(),na.rm=TRUE))
}
x.lim.sigma <- c(min(curve(dtruncnorm(x, mean=0, sd=10))[1][[1]],density(Sigma)[1]$x %>% min(),na.rm=TRUE),
                 max(curve(dtruncnorm(x, mean=0, sd=10))[1][[1]],density(Sigma)[1]$x %>% max(),na.rm=TRUE))
y.lim.sigma <- c(min(curve(dtruncnorm(x, mean=0, sd=10))[2][[1]],density(Sigma)[2]$y %>% min(),na.rm=TRUE),
                 max(curve(dtruncnorm(x, mean=0, sd=10))[2][[1]],density(Sigma)[2]$y %>% max(),na.rm=TRUE))
#######################################################################################################
c_dark <- c("#DCBCBC")
c_light_highlight <- c("#C79999")
c_mid <- c("#B97C7C")
c_mid_highlight <- c("#A25050")
c_light <- c("#8F2727")
c_dark_highlight <- c("#7C0000")
# posterior curve
#########################################################
cairo_pdf("./priorvsposterior.pdf",width=8,height=8)
if(length(pars.1) > 4){
  par(mfrow=c(2,3))
}else{
  par(mfrow=c(2,2))
}

if(m.name %in% c("MA_1","MA_2")){
  hist(Mu,
       xlab=expression(mu), ylab = "Gostota", col=c_dark,
       main = "", freq = FALSE, xlim = x.lim.mu, ylim = y.lim.mu)
  curve(dnorm(x, 0, 10),
        add=TRUE, col=c_light,lwd=1.5)
  legend("topleft", col = c(c_light,c_dark), lty=c(1,1), bty="n",
         legend=c("Apriorna","Aposteriorna"),seg.len	= .8, y.intersp=.8)
}else{
  hist(Alpha,
       xlab=expression(alpha), ylab = "Gostota", col=c_dark,
       main = "", freq = FALSE, xlim = x.lim.alpha, ylim=y.lim.alpha)
  curve(dnorm(x, 0, 10),
        add=TRUE, col=c_light,lwd=1.5)
  legend("topleft", col = c(c_light,c_dark), lty=c(1,1), bty="n",
         legend=c("Apriorna","Aposteriorna"),seg.len	= .8, y.intersp=.8)
}


if("phi" %in% names(fit_cp) | "phi[1]" %in% names(fit_cp)){
  hist(Phi1, 
       xlab=expression(phi[1]), ylab = "Gostota", col=c_dark,
       main = "", freq = FALSE, xlim=c(-1,1))
  # Analytical posterior parameter
  curve(dunif(x, -1, 1),
        add=TRUE, col=c_light,lwd=1.5)
  legend("topleft", col = c(c_light,c_dark), lty=c(1,1), bty="n",
         legend=c("Apriorna","Aposteriorna"),seg.len	= .8, y.intersp=.8)
}

if("phi[2]" %in% names(fit_cp)){
  hist(Phi2,
       xlab=expression(phi[2]), ylab = "Gostota", col=c_dark,
       main = "", freq = FALSE, xlim=c(-1,1))
  # Analytical posterior parameter
  curve(dunif(x, -1, 1),
        add=TRUE, col=c_light,lwd=1.5)
  legend("topleft", col = c(c_light,c_dark), lty=c(1,1), bty="n",
         legend=c("Apriorna","Aposteriorna"),seg.len	= .8, y.intersp=.8)
}

if("theta" %in% names(fit_cp) | "theta[1]" %in% names(fit_cp)){
  hist(Theta1,
       xlab=expression(theta[1]), ylab = "Gostota", col=c_dark,
       main = "", freq = FALSE, xlim=c(-1,1))
  # Analytical posterior parameter
  curve(dunif(x, -1, 1),
        add=TRUE, col=c_light,lwd=1.5)
  legend("topleft", col = c(c_light,c_dark), lty=c(1,1), bty="n",
         legend=c("Apriorna","Aposteriorna"),seg.len	= .8, y.intersp=.8)
}

if("theta[2]" %in% names(fit_cp)){
  hist(Theta2,
       xlab=expression(theta[2]), ylab = "Gostota", col=c_dark,
       main = "", freq = FALSE, xlim=c(-1,1))
  # Analytical posterior parameter
  curve(dunif(x, -1, 1),
        add=TRUE, col=c_light,lwd=1.5)
  legend("topleft", col = c(c_light,c_dark), lty=c(1,1), bty="n",
         legend=c("Apriorna","Aposteriorna"),seg.len	= .8, y.intersp=.8)
}

hist(Sigma,
     xlab=expression(sigma), ylab = "Gostota", col=c_dark,
     main = "", freq = FALSE, xlim=x.lim.sigma, ylim=y.lim.sigma)
curve(dtruncnorm(x, mean=0, sd=10),
      add=TRUE, col=c_light,lwd=1.5)
legend("topleft", col = c(c_light,c_dark), lty=c(1,1), bty="n",
       legend=c("Apriorna","Aposteriorna"),seg.len	= .8, y.intersp=.8)

hist(y_pred,
     xlab=expression(hat(y)), ylab = "Gostota", col=c_dark,
     main = "")
legend("topleft", col = c(c_dark), lty=c(1), bty="n",
       legend=c("Napoved"),seg.len	= .8, y.intersp=.8)

gar <- dev.off()

