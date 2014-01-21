#smodel1.1 is the base strucural model for SEHC
smodel1.1 <- smat1~edges+mutual+gwidegree(fixed=T)+gwodegree(fixed=T)+gwesp(.5,fixed=T)+gwdsp(.5,fixed=TRUE)
sresults1.1 <- ergm(smodel1.1,burnin=200000,MCMCsamplesize=30000,parallel=4,constraints=~bd(maxout=10))
summary(sresults1.1)
mcmc.diagnostics(sresults1.1,vars.per.page=6)
dev.off()

#lowers sample size
smodel1.1a <- smat1~edges+mutual+gwidegree(fixed=T)+gwodegree(fixed=T)+gwesp(.5,fixed=T)+gwdsp(.5,fixed=TRUE)
sresults1.1a <- ergm(smodel1.1a,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(sresults1.1a)
mcmc.diagnostics(sresults1.1a,vars.per.page=6)
dev.off()

#removes .5 from gwesp and gwdsp
smodel1.1b <- smat1~edges+mutual+gwidegree(fixed=T)+gwodegree(fixed=T)+gwesp(fixed=T)+gwdsp(fixed=TRUE)
sresults1.1b <- ergm(smodel1.1b,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(sresults1.1b)
mcmc.diagnostics(sresults1.1b,vars.per.page=6)
dev.off()

fit1.1b=gof(results1.1b)
summary(fit1.1b)
plot(fit1.1b)

fit1.1b.5=gof(results1.1b~distance+espartners,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(fit1.1b.5)
plot(fit1.1b.5)




#model1.2 updates the parameters from 1.1b and then increases burnin
sresults1.2 <- ergm(smodel1.1b,theta0=sresults1.1b$coef,burnin=500000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(sresults1.2)
mcmc.diagnostics(sresults1.2,vars.per.page=6)



#same as model1.1a but updates those coefficients and then increases the burnin
sresults1.2a <- ergm(smodel1.1a,theta0=sresults1.1a$coef,burnin=500000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(sresults1.2a)
mcmc.diagnostics(sresults1.2,vars.per.page=6)

#same as 1.2a but increases sample size to 30000
sresults1.2b <- ergm(smodel1.1a,theta0=sresults1.1a$coef,burnin=500000,MCMCsamplesize=30000,parallel=4,constraints=~bd(maxout=10))
summary(sresults1.2b)
mcmc.diagnostics(sresults1.2,vars.per.page=6)

c(results1.2a$mle.lik,results1.2b$mle.lik,results1.2c$mle.lik,results1.2c$mle.lik)


#updates coefficients from 1.1a but updates the MCMC to use the TNT feature
sresults1.2c <- ergm(smodel1.1a,theta0=sresults1.1a$coef,burnin=500000,MCMCsamplesize=10000,parallel=4,MCMC.interval=1000,constraints=~bd(maxout=10),control=control.ergm(prop.weights='TNT'))
summary(sresults1.2c)
mcmc.diagnostics(sresults1.2,vars.per.page=6)


#updates coefficients from 1.1a and includes same terms from 1.2c but increases interval of 5000
sresults1.2d <- ergm(smodel1.1a,theta0=sresults1.1a$coef,burnin=500000,MCMCsamplesize=10000,parallel=4,MCMC.interval=5000,constraints=~bd(maxout=10),control=control.ergm(prop.weights='TNT'))
summary(sresults1.2d)
mcmc.diagnostics(sresults1.2,vars.per.page=6)


#same as 1.2c but raises the burn in to 1000000 and sample size to 50000
sresults1.2e <- ergm(smodel1.1a,theta0=sresults1.1a$coef,burnin=1000000,MCMCsamplesize=50000,parallel=4,MCMC.interval=1000,constraints=~bd(maxout=10),control=control.ergm(prop.weights='TNT'))
summary(sresults1.2e)
mcmc.diagnostics(sresults1.2e,vars.per.page=6)


c(sresults1.1$mle.lik,sresults1.1a$mle.lik,sresults1.1b$mle.lik,sresults1.2$mle.lik,sresults1.2a$mle.lik,sresults1.2b$mle.lik,sresults1.2c$mle.lik,sresults1.2d$mle.lik,sresults1.2e$mle.lik)
