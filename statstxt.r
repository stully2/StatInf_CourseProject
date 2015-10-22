

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

# Create Data Sets raw and measured
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
# hist(samples$rexp[samples$obs==1])
# hist(mean_dev$mean)
# hist(mean_dev$stdev)


# # standard
# x <- seq(-3,3,length=1000)
# y <- dnorm(x,mean=0,sd=1)
# plot(x,y,type="l",lwd=1)
