

set.seed(0)

library(lattice)
set.seed(0)
sim_n <- 1000
n <- 40
lam <- 0.2

# # explore LLN
# a<-10^seq(2,7,by = 0.01)
# # lapply(a,function(i) {mean(rexp(i,lam))})
# plot(a,lapply(a,function(i) {mean(rexp(i,lam))})
#      ,type="l"
#      ,lwd=1
#      ,col = "blue"
#      ,log = "x"
#      ,xlab = "Sample Size"
#      ,ylab = "Means: rexp() w/ Lambda = 0.2"
#      ,main = "Exploration of Law of Large Numbers"
# )
# abline(h = 5)
a<-10^seq(2,7,by = 0.01)
re<-function(i) {mean(rexp(i,lam))}
LLN <- data.frame(x = a, mean = mean(rexp(a,lam)), stdev = sd(rexp(a,lam)))
# # explore LLN
# a<-10^seq(2,7,by = 0.01)
# # lapply(a,function(i) {mean(rexp(i,lam))})
# plot(a,lapply(a,function(i) {mean(rexp(i,lam))})
#      ,type="l"
#      ,lwd=1
#      ,col = "blue"
#      ,log = "x"
#      ,xlab = "Sample Size"
#      ,ylab = "Means: rexp() w/ Lambda = 0.2"
#      ,main = "Exploration of Law of Large Numbers"
# )
# abline(h = 5)







# Create Data Sets raw and measured
control <- rexp(sim_n,lam)
samples <- data.frame(obs=NULL,rexp=NULL)
measures <- data.frame(obs=NULL
                      ,mean=NULL
                      ,stdev=NULL
                      ,sterr=NULL
                      )

  for (i in 1:sim_n) {
    tmp <- rexp(n,lam)
    samples <- rbind(samples,data.frame(obs=i,rexp=tmp))
    measures <- rbind(measures, data.frame(obs=i,mean=mean(tmp),stdev=sd(tmp),sterr=sd(tmp)/sqrt(n)))
  }





# # Histograms of raw data
# hist(control)
# hist(samples$rexp[samples$obs==1])
# hist(mean_dev$mean)
# hist(mean_dev$stdev)

# Control
hist(control, col="light blue"
        , border = "dark gray"
        ,xlab = NULL
        , main="1000 Random Eponentials with Rate 0.2")



# Mean
hist(measures$mean, col="light blue", border = "dark gray", xlab="Mean", main="Mean vs Normal")
  xfit<-seq(min(measures$mean),max(measures$mean),length=500)
  yfit<-dnorm(xfit,mean=mean(measures$mean),sd=sd(measures$mean))
  yfit <- yfit*diff(h$mids[1:2])*length(measures$mean)
  confint <- round(mean(measures$mean) + c(-1,1) * qnorm(0.975) * sd(measures$mean)/sqrt(length(measures$mean)),3)
    lines(xfit, yfit, col="black", lwd=2)
    abline(v = 5,col = "black", lwd = 2)
    abline(v = confint,col = "red", lwd = 1.5)

# Standard Deviation
    hist(measures$stdev, col="light blue", border = "dark gray", xlab="Mean", main="Mean vs Normal")
    xfit<-seq(min(measures$stdev),max(measures$stdev),length=500)
    yfit<-dnorm(xfit,mean=mean(measures$stdev),sd=sd(measures$stdev))
    yfit <- yfit*diff(h$mids[1:2])*length(measures$stdev)
    confint <- round(mean(measures$stdev) + c(-1,1) * qnorm(0.975) * sd(measures$stdev)/sqrt(length(measures$stdev)),3)
    lines(xfit, yfit, col="black", lwd=2)
    abline(v = 5,col = "black", lwd = 2)
    abline(v = confint,col = "red", lwd = 1.5)

# Standard Error
    hist(measures$sterr, col="light blue", border = "dark gray", xlab="Mean", main="Mean vs Normal")
    xfit<-seq(min(measures$sterr),max(measures$sterr),length=500)
    yfit<-dnorm(xfit,mean=mean(measures$sterr),sd=sd(measures$sterr))
    yfit <- yfit*diff(h$mids[1:2])*length(measures$sterr)
    confint <- round(mean(measures$sterr) + c(-1,1) * qnorm(0.975) * sd(measures$sterr)/sqrt(length(measures$sterr)),3)
    lines(xfit, yfit, col="black", lwd=2)
    abline(v = 5,col = "black", lwd = 2)
    abline(v = confint,col = "red", lwd = 1.5)


mlims <- mean(measures$mean) + c(-1,1) * qnorm(0.975) * sd(measures$mean)/sqrt(sim_n)
sdlims <- mean(measures$stdev) + c(-1,1) * qnorm(0.975) * sd(measures$stdev)/sqrt(sim_n)


plot(measures$mean,measures$stdev,xlim = c(3,9),ylim = c(3,9))
polygon(c(mlims[1],mlims[1],mlims[2],mlims[2],mlims[1])
        ,c(0,10,10,0,0)
        , col = rgb(0,0,0,0.1)
        , border = FALSE)
abline(v = mean(measures$mean),col = rgb(0,0,0))
abline(v = 5,col = rgb(1,0,0))
polygon(c(0,0,10,10,0)
        ,c(sdlims[1],sdlims[2],sdlims[2],sdlims[1],sdlims[1])
        , col = rgb(0,0,0,0.1)
        , border = FALSE)
abline(h = mean(measures$stdev),col = rgb(0,0,0))
abline(h = 5,col = rgb(1,0,0))
