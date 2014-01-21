smodel3 <- smat1~edges+mutual+gwesp(fixed=T)+gwdsp(fixed=TRUE)+nodematch('Gender')+nodeifactor('Gender')+nodeofactor('Gender')
sresults3 <- ergm(smodel3,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(sresults3)

mcmc.diagnostics(sresults3,vars.per.page=7)


fit3=gof(sresults3~distance+espartners,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(fit3)
plot(fit3)



#############################################
smodel3.0 <- smat1~edges+mutual+gwesp(.5,fixed=T)+gwdsp(.5,fixed=TRUE)+nodematch('Gender')+nodeifactor('Gender')+nodeofactor('Gender')
sresults3.0 <- ergm(smodel3.0,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(sresults3.0)

mcmc.diagnostics(sresults3.0,vars.per.page=7)


fit3.0=gof(sresults3.0~distance+espartners,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(fit3.0)
plot(fit3.0)







################################################

smodel3.1 <- smat1~edges+mutual+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('Gender')+nodeifactor('Gender')+nodeofactor('Gender')
sresults3.1 <- ergm(smodel3.1,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(sresults3.1)

mcmc.diagnostics(sresults3.1,vars.per.page=7)


fit3.1=gof(sresults3.1~distance+espartners,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(fit3.1)
plot(fit3.1)



#######################################################
smodel3.2 <- smat1~edges+mutual+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('Gender')+nodeifactor('Gender')+nodeofactor('Gender')
sresults3.2 <- ergm(smodel3.2,burnin=500000,MCMCsamplesize=100000,parallel=4,constraints=~bd(maxout=10))
summary(sresults3.2)

mcmc.diagnostics(sresults3.2,vars.per.page=7)


fit3.2=gof(sresults3.2~distance+espartners,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(fit3.2)
plot(fit3.2)

#########################################################
smodel3.3 <- smat1~edges+mutual+gwesp(.15,fixed=T)+gwdsp(.15,fixed=TRUE)+nodematch('Gender')+nodeifactor('Gender')+nodeofactor('Gender')
sresults3.3 <- ergm(smodel3.3,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(sresults3.3)

mcmc.diagnostics(sresults3.3,vars.per.page=7)


fit3.3=gof(sresults3.3~distance+espartners,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(fit3.3)
plot(fit3.3)

########################################################
c(sresults2$mle.lik,sresults2.1$mle.lik,sresults2.2$mle.lik,sresults2.3$mle.lik,sresults2.4$mle.lik,sresults2.5$mle.lik,sresults3$mle.lik,sresults3.0$mle.lik,sresults3.1$mle.lik,sresults3.2$mle.lik,sresults3.3$mle.lik,sresults3.4$mle.lik,sresults3.5$mle.lik)

##########################################################

smodel3.4 <- smat1~edges+mutual+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('Gender')+nodeifactor('Gender')+nodeofactor('Gender')
sresults3.4 <- ergm(smodel3.4,burnin=500000,MCMCsamplesize=300000,parallel=4,constraints=~bd(maxout=10))
summary(sresults3.4)

mcmc.diagnostics(sresults3.4,vars.per.page=7)


fit3.4=gof(sresults3.4~distance+espartners,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(fit3.4)
plot(fit3.4)


####################################

sresults3.5 <- ergm(smodel3.4,theta0=sresults3.4$coef,burnin=500000,MCMCsamplesize=300000,parallel=4,constraints=~bd(maxout=10))
summary(sresults3.5)

mcmc.diagnostics(sresults3.5,vars.per.page=7)


fit3.5=gof(sresults3.5~distance+espartners,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(fit3.5)
plot(fit3.5)
