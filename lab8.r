# lab8.r		script file lab 8
#
# author: Eric Zivot
# created: November 9, 2009
# revised: July 31, 2012
#   fixed problem with yearmon date object

options(digits=4, width=70)

# load packages
library("PerformanceAnalytics")
library("zoo")

################################################################################
# introduction to portfolio theory
################################################################################

# source portfolio functions
source(file="http://spark-public.s3.amazonaws.com/compfinance/R%20code/portfolio.r")

# load data from class website
lab8returns.df = read.csv(file="http://spark-public.s3.amazonaws.com/compfinance/R%20code/lab8returns.csv",
                          stringsAsFactors=FALSE)
# 7/31/12: fix to problem with the yearmon class
dates = seq(as.Date("1992-07-01"), as.Date("2000-10-01"), by="months")
lab8returns.df$Date = dates
# create zoo object
lab8returns.z = zoo(lab8returns.df[,-1], lab8returns.df$Date)
plot(lab8returns.z, lwd=2, col="blue")
                          
# compute estimates of CER model and annualize
muhat.annual = apply(lab8returns.z,2,mean)*12   
sigma2.annual = apply(lab8returns.z,2,var)*12
sigma.annual = sqrt(sigma2.annual)
covmat.annual = cov(lab8returns.z)*12 
covhat.annual = cov(lab8returns.z)[1,2]*12   
rhohat.annual = cor(lab8returns.z)[1,2]

mu.b = muhat.annual["rboeing"]
mu.m = muhat.annual["rmsft"]
sig2.b =  sigma2.annual["rboeing"]
sig2.m = sigma2.annual["rmsft"]
sig.b = sigma.annual["rboeing"]
sig.m = sigma.annual["rmsft"]
sig.bm = covhat.annual
rho.bm = rhohat.annual


#
# 1. create portfolios and plot
#
x.b = seq(from=-1, to=2, by=0.1)
x.m = 1 - x.b
mu.p = x.b*mu.b + x.m*mu.m
sig2.p = x.b^2 * sig2.b + x.m^2 * sig2.m + 2*x.b*x.m*sig.bm
sig.p = sqrt(sig2.p)

# min variance portfolio, tip of Markowitz bullet
x.b.min = (sig2.m - sig.bm)/(sig2.m + sig2.b - 2*sig.bm)
x.m.min = 1 - x.b.min
mu.p.min = x.b.min*mu.b + x.m.min*mu.m

# dot color vector, green for efficient
col.p = rep("", length(mu.p)) # initiate empty
col.p[mu.p >= mu.p.min] <- "green"
col.p[mu.p < mu.p.min] <- "red"


plot(sig.p, mu.p, type="b", pch=16, ylim=c(0, max(mu.p)), xlim=c(0, max(sig.p)),
     xlab=expression(sigma[p]), ylab=expression(mu[p]),
     col=col.p)
text(x=sig.b, y=mu.b, labels="Boeing", pos=4)
text(x=sig.m, y=mu.m, labels="MSFT", pos=4)

# now compute portfolios with assets and T-bills as well as Sharpe slopes

r.f = 0.03
# T-bills + Boeing
x.b = seq(from=0, to=2, by=0.1)
mu.p.b = r.f + x.b*(mu.b - r.f)
sig.p.b = x.b*sig.b


# T-bills + MSFT
x.m = seq(from=0, to=2, by=0.1)
mu.p.m = r.f + x.m*(mu.m - r.f)
sig.p.m = x.m*sig.m


plot(sig.p, mu.p, type="b", pch=16, ylim=c(0, max(mu.p)), xlim=c(0, max(sig.p)),
     xlab=expression(sigma[p]), ylab=expression(mu[p]),
     col=col.p)
text(x=sig.b, y=mu.b, labels="Boeing", pos=4)
text(x=sig.m, y=mu.m, labels="MSFT", pos=4)
points(sig.p.b, mu.p.b, type="b", col="blue")
points(sig.p.m, mu.p.m, type="b", col="orange")


#
# 2. compute global minimum variance portfolio
#

gmin.port = globalMin.portfolio(muhat.annual,
                                covmat.annual) 
gmin.port
summary(gmin.port, risk.free=0.03)
pie(gmin.port$weights)
plot(gmin.port)

# computation using matrix algebra
cov.mat.inv = solve(covmat.annual)
rownames(cov.mat.inv) = names(muhat.annual) #naming assets
gmin.port.x = rowSums(cov.mat.inv)/sum(cov.mat.inv) #sum of each row / total
gmin.port.x

gmin.port.ret = crossprod(gmin.port.x, muhat.annual)
gmin.port.sd = sqrt(gmin.port.x %*% covmat.annual %*% gmin.port.x)
gmin.port.ret
gmin.port.sd


plot(sig.p, mu.p, type="b", pch=16, ylim=c(0, max(mu.p)), xlim=c(0, max(sig.p)),
     xlab=expression(sigma[p]), ylab=expression(mu[p]),
     col=col.p)
text(x=sig.b, y=mu.b, labels="Boeing", pos=4)
text(x=sig.m, y=mu.m, labels="MSFT", pos=4)
text(x=gmin.port.sd, y=gmin.port.ret, labels="Global min", pos=2)

#
# 3. compute tangency portfolio
#

tan.port = tangency.portfolio(muhat.annual,
                              covmat.annual,
                              risk.free=0.03) 
tan.port
summary(tan.port,risk.free=0.03)
plot(tan.port)
pie(tan.port$weights)

# computation using matrix algebra
r.f = 0.03
cov.mat.inv = solve(covmat.annual)
tan.port.x = cov.mat.inv %*% (muhat.annual - r.f) # unweighted shares
tan.port.x = as.numeric(tan.port.x/sum(tan.port.x)) #scaling total of 1
names(tan.port.x) = names(muhat.annual) #naming assets
tan.port.x

tan.port.ret = crossprod(tan.port.x, muhat.annual)
tan.port.sd = sqrt(tan.port.x %*% covmat.annual %*% tan.port.x)

# T-bills + tangency
x.t = seq(from=0, to=2, by=0.1)
mu.p.t = r.f + x.t*(tan.port.ret - r.f)
sig.p.t = x.t*tan.port.sd



plot(sig.p, mu.p, type="b", pch=16, ylim=c(0, max(mu.p)), xlim=c(0, max(sig.p)),
     xlab=expression(sigma[p]), ylab=expression(mu[p]),
     col=col.p)
text(x=sig.b, y=mu.b, labels="Boeing", pos=4)
text(x=sig.m, y=mu.m, labels="MSFT", pos=4)
text(x=tan.port.sd, y=tan.port$er, labels="Tangency", pos=2)
points(sig.p.t, mu.p.t, type="b", col="blue", pch=16)

#
# 4 and 5
#

x.t = 0.1
x.f = 1 - x.t

mu.e = r.f + x.t*(tan.port$er - r.f)
sd.e = x.t*tan.port$sd


plot(sig.p, mu.p, type="b", pch=16, ylim=c(0, max(mu.p)), xlim=c(0, max(sig.p)),
     xlab=expression(sigma[p]), ylab=expression(mu[p]),
     col=col.p)
text(x=sig.b, y=mu.b, labels="Boeing", pos=4)
text(x=sig.m, y=mu.m, labels="MSFT", pos=4)
text(x=tan.port$sd, y=tan.port$er, labels="Tangency", pos=2)
points(sig.p.t, mu.p.t, type="b", col="blue", pch=16)
points(sd.e, mu.e, type="p", col="orange", pch=16, cex=2)
text(x=sd.e, y=mu.e, labels="Efficient Portfolio with 10% Tangency", pos=4, cex=0.75)

# efficient port with same risk as msft
x.t = sig.m/tan.port$sd
mu.e = r.f + x.t*(tan.port$er - r.f)
sd.e = x.t*tan.port$sd


plot(sig.p, mu.p, type="b", pch=16, ylim=c(0, max(mu.p)), xlim=c(0, max(sig.p)),
     xlab=expression(sigma[p]), ylab=expression(mu[p]),
     col=c(rep("green", 18), rep("red", 13)))
text(x=sig.b, y=mu.b, labels="Boeing", pos=4)
text(x=sig.m, y=mu.m, labels="MSFT", pos=4)
text(x=tan.port$sd, y=tan.port$er, labels="Tangency", pos=2)
points(sig.p.t, mu.p.t, type="b", col="blue", pch=16)
points(sd.e, mu.e, type="p", col="orange", pch=16, cex=2)
text(x=sd.e, y=mu.e, labels="Efficient Portfolio with Same SD as MSFT", pos=2, cex=0.75)