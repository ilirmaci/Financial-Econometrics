# rollingPortfolios.r				Examples of rolling analysis of portfolios
#
# author: Eric Zivot
# created: November 26, 2006
# updated: November 16, 2008
#
# notes:
# 1. requires portfolio functions in portfolio.r
# 2. data are imported from singleIndexPrices.csv 
# which contains monthly closing prices on the following assets
#
# sp500, sbux, msft, nord, boeing
#
# over the period 10/98 - 10/03
#
library(zoo)

singleIndexPrices.df = read.csv("C:/finBook/EXCEL/singleIndexPrices.csv",
                  stringsAsFactors=F)
colnames(singleIndexPrices.df)
#
# 2. Create zooreg object from data and dates in singleIndexPrices.df
#
singleIndexPrices.z = zooreg(data=singleIndexPrices.df[,-1], 
                              start=c(1998,10), end=c(2003,10),
                              frequency=12)
plot(singleIndexPrices.z)
#
# 3. create returns
#
si.z = diff(log(singleIndexPrices.z))
si.df = as.data.frame(coredata(si.z))
# returns excluding market
ret.mat = as.matrix(si.df[,-1])

# plot returns over full sample
plot(si.z)

# estimate parameters of constant expected return
# model

options(digits=4)
nobs = length(si.z[,1])
muhat.vals = colMeans(ret.mat)
sigmahat.vals = apply(ret.mat,2,sd)
cov.mat = var(ret.mat)
cor.mat = cor(ret.mat)

se.muhat = sigmahat.vals/sqrt(nobs)
se.sigmahat = sigmahat.vals/sqrt(2*nobs)
mu.lower = muhat.vals - 2*se.muhat
mu.upper = muhat.vals + 2*se.muhat
sigma.lower = sigmahat.vals - 2*se.sigmahat
sigma.upper = sigmahat.vals + 2*se.sigmahat


plot(sigmahat.vals, muhat.vals, type="n", 
     xlab = "sigma", ylab="mu",
     xlim=range(c(sigma.lower,sigma.upper)),
     ylim=range(c(mu.lower,mu.upper)))
text(sigmahat.vals[c(1,4)], muhat.vals[c(1,4)], 
     labels=names(muhat.vals[c(1,4)]))


#
# compute efficient portfolios
#

# global minimum variance portfolio
gmin.4 = globalMin.portfolio(er=muhat.vals,cov.mat=cov.mat)
gmin.4

# efficient portfolio with target return = 0.015
eport.015 = efficient.portfolio(er=muhat.vals,cov.mat=cov.mat,
                                target.return=0.015)
eport.015
plot(eport.015)

# compute efficient frontier
ef.4 = efficient.frontier(er=muhat.vals,cov.mat=cov.mat)
ef.4
plot(ef.4, plot.assets=T)

#
# rolling global min portfolios
#

rollGmin = function(x) {
	mu.hat = colMeans(x)
	cov.hat = var(x)
	gmin = globalMin.portfolio(er=mu.hat,cov.mat=cov.hat)
	ans = c(gmin$er,gmin$sd,gmin$weights)
	names(ans)[1:2] = c("er","sd")
	ans
}
rollefficient = function(x,target=0.015) {
	mu.hat = colMeans(x)
	cov.hat = var(x)
	eport = efficient.portfolio(er=mu.hat,cov.mat=cov.hat,
	                            target.return=target)
	ans = c(eport$er,eport$sd,eport$weights)
	names(ans)[1:2] = c("er","sd")
	ans
}

# rolling global minimum variance portfolios
roll.gmin = rollapply(si.z[,-1],width=24,
                      by.column=F,align="right",
                      FUN=rollGmin)

plot(roll.gmin[,3:6],main="Rolling weights in global min portfolio",
     plot.type="single", col=1:4, lwd=3, ylab="weight")    
abline(h=0)
legend(x="bottomleft",legend=colnames(si.z[,-1]),
       lty=rep(1,4),col=1:4,lwd=3)

plot(roll.gmin[,1:2],plot.type="single",ylab="percent",
     main="Rolling means and sds on global min portfolio",
     col=c("black","orange"),lwd=3)
abline(h=0)
legend(x="topleft",legend=colnames(roll.gmin[,1:2]),
       lty=rep(1,2),col=c("black","orange"),lwd=3)

# rolling efficient portfolios with target = 0.015
roll.eport = rollapply(si.z[,-1],width=24,
                       by.column=F,align="right",
                       FUN=rollefficient)

plot(roll.eport[,3:6],main="Rolling weights in efficient portfolio with target=0.015",
     plot.type="single", ylab="weight", col=1:4,lwd=3)
abline(h=0)
legend(x="bottomleft",legend=colnames(si.z[,-1]),
       lty=rep(1,4),col=1:4,lwd=3)

plot(roll.eport[,1:2], plot.type="single", ylab="percent",
     main="Rolling means and sds on efficient portfolio with target=0.015",
     col=c("black","orange"),lwd=3)
abline(h=0)
legend(x="topleft",legend=colnames(roll.eport[,1:2]),
       lty=rep(1,2),col=c("black","orange"),lwd=3)
