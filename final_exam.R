muhat = c(0.0043, 0.0049, 0.0128)
names(muhat) = c('vpacx', 'vbltx', 'veiex')
muhat

rf = (0.0008)

sighat = c(0.0559, 0.029, 0.0845)
names(sighat) = names(muhat)

sig2hat = sighat^2

sigma.mat = diag(sighat)
colnames(sigma.mat) = names(muhat)
rownames(sigma.mat) = names(muhat)

sigma2.mat = diag(sig2hat)
colnames(sigma2.mat) = names(muhat)
rownames(sigma2.mat) = names(muhat)

cov.mat.inv = solve(sigma2.mat)

# find efficient portfolio using zero-cov matrix
# target return = 0.01
# incorrect for the exam

target.ret = 0.01

A.top = cbind(2*sigma2.mat, muhat, 1)
A.bot = rbind(c(muhat, 0, 0), c(rep(1,3), 0, 0))
A = rbind (A.top, A.bot)

b = c(rep(0, 3), target, 1)

eff.01.x = solve(A, b)
eff.01.x = as.numeric(eff.01.x[1:3])
names(eff.01.x) = names(muhat)


# compute efficient frontier as linear combination
# of global min and tangency portfolios
target.ret = 0.01

gm.x = c(0.23, 0.87, -0.1)
tan.x = c(-1.97, 1.51, 1.45)
names(gm.x) <- names(tan.x) <- names(muhat)

gm.ret = as.numeric(crossprod(gm.x, muhat))
tan.ret = as.numeric(crossprod(tan.x, muhat))

alpha = as.numeric((target.ret - tan.ret)/(gm.ret - tan.ret))

eff.01.x = alpha*gm.x + (1-alpha)*tan.x
names(eff.01.x) = names(muhat)


# compute mix of T-bills and tangency portfolio
# to achieve return 0.01
target.ret = 0.01
alpha = as.numeric((target.ret-tan.ret)/(rf-tan.ret)) #share of T-bills

tan.se = 0.0653
as.numeric((1-alpha)*tan.se)

c(alpha, (1-alpha)*tan.x)

# portfolio beta
betahat = c(1.00139, 0.16639, 1.5262)
crossprod(gm.x, betahat)
