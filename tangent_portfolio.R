mu.usa = 1.6367
mu.cvr = 1.4741
mu.sprint = 1.4231
mu.hfc = 0.9834

muhat = c(mu.usa, mu.cvr, mu.sprint, mu.hfc)
names(muhat) = c("usa", "cvr", "sprint", "hfc")


sig2.usa = 0.0361^2 * 250
sig2.cvr = 0.0205^2 * 250
sig2.sprint = 0.0321^2 * 250
sig2.hfc = 0.0215^2 * 250

sig.usa = sqrt(sig2.usa)
sig.cvr = sqrt(sig2.cvr)
sig.sprint = sqrt(sig2.sprint)
sig.hfc = sqrt(sig2.hfc)

cov.usacvr = 250 * 0.0001114067
cov.usasprint = 250 * 0.0001430057
cov.usahfc = 250 * 0.000066
cov.cvrsprint = 250 * 0.000079487
cov.cvrhfc = 250 * 0.0002214
cov.sprinthfc = 250 * 0.000099499

sigma2 = matrix(c(sig2.usa, cov.usacvr, cov.usasprint, cov.usahfc,
                  cov.usacvr, sig2.cvr, cov.cvrsprint, cov.cvrhfc,
                  cov.usasprint, cov.cvrsprint, sig2.sprint, cov.sprinthfc,
                  cov.usahfc, cov.cvrhfc, cov.sprinthfc, sig2.sprint), 4, 4)

rownames(sigma2) = c("usa", "cvr", "sprint", "hfc")
colnames(sigma2) = c("usa", "cvr", "sprint", "hfc")

cov.inv = solve(sigma2)
weights = cov.inv %*% muhat