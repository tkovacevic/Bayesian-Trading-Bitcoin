rm(list = ls()) 
setwd("~/Dropbox/Tjasulka/MAGISTERIJ/Bayes_2018/Trading_Bitcoin")
source("functions.R")
source("TrainTime.R")
source("TestTime.R")
source("source_trading.R")
source("betanalpha_functions.R")
dev.off()

# input0 <- NULL
#1 veriga, 2000 iteracij, 1000 warmup, 120 days
input0 <- "./train_days/predicted_ModelsLR_bayes_a01598_trainTime120_dRange30_50_80_100_ddRange80_110.rds"
n.chains.plot=1

# train.time0.plot = 10
# dd_range1.x.plot = 110
# d_range1.x.plot = 30
# d_range1.xx = 30
# n.iter.plot=7000
# n.warmup.plot=4000
# delta.plot=0.8
# n.chains.plot=2
Day=4 #4 for MA1, 1 for ARMA12

# train.time0 <- train.time0.plot
# d_range1.x <- d_range1.x.plot
# dd_range1.x <- dd_range1.x.plot
# gr.name <- c(FALSE,TRUE,rep(FALSE,3))
# ###
# n.iter <- n.iter.plot;
# n.warmup <- n.warmup.plot;
# delta <- delta.plot;
n.chains <- n.chains.plot
#################################################
# if(is.null(input0)){
#   s.time0 <- Sys.time()
#   for(i in 1:length(models.x)){ #we have 5 models
#     if(gr.name[i]){
#       model.name <- models.x[i]
#       funk_run_trainTime(model.name)
#     }
#   }
#   s.time1 <- Sys.time()-s.time0; s.time1
# }
#################################################
xxi <- readRDS(input0)
ss <- matrix(0,ncol=1,nrow=4)
s.summary <- c()
for(i in 1:length(xxi[["TrainDays=120_dd=111_d=50"]])){
  si <- summary(xxi[["TrainDays=120_dd=111_d=50"]][[i]], pars = c("alpha","beta1","beta2","beta3"))$summary[,c("mean"),drop=FALSE]
  s.summary <- cbind(s.summary,si)
}

povzetek <- t(s.summary) %>% summary(); povzetek

Day=4
fit_cp <- xxi[["TrainDays=120_dd=111_d=50"]][[Day]]
posterior_cp <- as.array(fit_cp)
setwd("~/Dropbox/Tjasulka/MAGISTERIJ/Bayes_2018/Trading_Bitcoin/train_days/LR_plots_d50_D110")
####################################
check_div(fit_cp)
###
#########################################################
pars.1 <- c("alpha","beta1","beta2","beta3","y_test")
###
params_cp <- as.data.frame(extract(fit_cp, permuted=FALSE))
posterior_cp <- as.array(fit_cp)
###
# available_mcmc(pattern = "_nuts_")
lp_cp <- log_posterior(fit_cp) #log posterior
np_cp <- nuts_params(fit_cp) #nuts
###
###
c_dark <- c("#DCBCBC")
c_light_highlight <- c("#C79999")
c_mid <- c("#B97C7C")
c_mid_highlight <- c("#A25050")
c_light <- c("#8F2727")
c_dark_highlight <- c("#7C0000")
###
Alpha <- unlist(extract(fit_cp, 'alpha'), use.names=FALSE)
Beta1 <- unlist(extract(fit_cp, 'beta1'), use.names=FALSE)
Beta2 <- unlist(extract(fit_cp, 'beta2'), use.names=FALSE)
Beta3 <- unlist(extract(fit_cp, 'beta3'), use.names=FALSE)
y_pred <- unlist(extract(fit_cp, 'y_test'), use.names=FALSE)
###
cairo_pdf("./priorvsposterior.pdf",width=8,height=8)
par(mfrow=c(2,2))
hist(Alpha,
     xlab=expression(alpha), ylab = "Gostota", col=c_dark,
     main = "", freq = FALSE)
curve(dunif(x, -Inf, Inf),
      add=TRUE, col=c_light,lwd=1.5)
legend("topleft", col = c(c_light,c_dark), lty=c(1,1), bty="n",
       legend=c("Apriorna","Aposteriorna"),seg.len	= .8, y.intersp=.8)

hist(Beta1,
     xlab=expression(beta[1]), ylab = "Gostota", col=c_dark,
     main = "", freq = FALSE)
curve(dnorm(x, 0, 10),
      add=TRUE, col=c_light,lwd=1.5)
legend("topleft", col = c(c_light,c_dark), lty=c(1,1), bty="n",
       legend=c("Apriorna","Aposteriorna"),seg.len	= .8, y.intersp=.8)

hist(Beta2,
     xlab=expression(beta[2]), ylab = "Gostota", col=c_dark,
     main = "", freq = FALSE)
curve(dnorm(x, 0, 10),
      add=TRUE, col=c_light,lwd=1.5)
legend("topleft", col = c(c_light,c_dark), lty=c(1,1), bty="n",
       legend=c("Apriorna","Aposteriorna"),seg.len	= .8, y.intersp=.8)

hist(Beta3,
     xlab=expression(beta[3]), ylab = "Gostota", col=c_dark,
     main = "", freq = FALSE)
curve(dnorm(x, 0, 10),
      add=TRUE, col=c_light,lwd=1.5)
legend("topleft", col = c(c_light,c_dark), lty=c(1,1), bty="n",
       legend=c("Apriorna","Aposteriorna"),seg.len	= .8, y.intersp=.8)

gar <- dev.off()
