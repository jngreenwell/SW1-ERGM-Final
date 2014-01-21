smodel5 <- smat1~edges+mutual+gwidegree(fixed=T)+gwodegree(fixed=T)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('PersonSmoke')+nodeifactor('PersonSmoke',base=1)+nodeofactor('PersonSmoke',base=1)
sresults5 <- ergm(smodel5,burnin=500000,MCMCsamplesize=50000,parallel=4,constraints=~bd(maxout=10))
summary(sresults5)
dev.off()
mcmc.diagnostics(sresults5,vars.per.page=5)


fit5=gof(sresults5~distance+espartners,nsim=40,burnin=500000)
dev.off()
par(mfrow=c(2,2))
summary(fit5)
plot(fit5)

####################################
smodel5.1 <- smat1~edges+mutual('PersonSmoke')+asymmetric('PersonSmoke')+gwidegree(fixed=T)+gwodegree(fixed=T)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('PersonSmoke')+nodeifactor('PersonSmoke',base=1)+nodeofactor('PersonSmoke',base=1)

sresults5.1 <- ergm(smodel5.1,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(sresults5.1)
dev.off()
mcmc.diagnostics(sresults5.1,vars.per.page=5)


fit5.1=gof(sresults5.1~distance+espartners,nsim=40,burnin=500000)
dev.off()
par(mfrow=c(2,2))
summary(fit5.1)
plot(fit5.1)

####################################################
#updates 5.1 parameters
smodel5.2 <- smat1~edges+mutual('PersonSmoke')+asymmetric('PersonSmoke')+gwidegree(fixed=T)+gwodegree(fixed=T)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('PersonSmoke')+nodeifactor('PersonSmoke',base=1)+nodeofactor('PersonSmoke',base=1)

sresults5.2 <- ergm(smodel5.1,theta0=sresults5.1$coef,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(sresults5.2)
dev.off()
mcmc.diagnostics(sresults5.2,vars.per.page=5)


fit5.2=gof(sresults5.2~distance+espartners,nsim=40,burnin=500000)
dev.off()
par(mfrow=c(2,2))
summary(fit5.2)
plot(fit5.2)

##################################
#recreates 5.1 but increases burnin and sample size

smodel5.3 <- smat1~edges+mutual('PersonSmoke')+asymmetric('PersonSmoke')+gwidegree(fixed=T)+gwodegree(fixed=T)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('PersonSmoke')+nodeifactor('PersonSmoke',base=1)+nodeofactor('PersonSmoke',base=1)

sresults5.3 <- ergm(smodel5.3,burnin=500000,MCMCsamplesize=100000,parallel=4,constraints=~bd(maxout=10))
summary(sresults5.3)
dev.off()
mcmc.diagnostics(sresults5.3,vars.per.page=5)


fit5.3=gof(sresults5.3~distance+espartners,nsim=40,burnin=500000)
dev.off()
par(mfrow=c(2,2))
summary(fit5.3)
plot(fit5.3)

#################################################################
#makes the nodematch parameter differential
smodel5.4 <- smat1~edges+mutual('PersonSmoke')+asymmetric('PersonSmoke')+gwidegree(fixed=T)+gwodegree(fixed=T)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('PersonSmoke',diff=T)+nodeifactor('PersonSmoke',base=1)+nodeofactor('PersonSmoke',base=1)

sresults5.4 <- ergm(smodel5.4,burnin=500000,MCMCsamplesize=50000,parallel=4,constraints=~bd(maxout=10))
summary(sresults5.4)
dev.off()
mcmc.diagnostics(sresults5.4,vars.per.page=5)


fit5.4=gof(sresults5.4~distance+espartners,nsim=40,burnin=500000)
dev.off()
par(mfrow=c(2,2))
summary(fit5.4)
plot(fit5.4)

##########################################
#similar to Model5.1 but adds more node attribute parameters
smodel5.5 <- smat1~edges+mutual('PersonDrink')+asymmetric('PersonDrink')+gwidegree(fixed=T)+gwodegree(fixed=T)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('PersonSmoke')+nodeifactor('PersonSmoke',base=1)+nodeofactor('PersonSmoke',base=1)+nodematch('PersonDrink')+nodeifactor('PersonDrink',base=1)+nodeofactor('PersonDrink',base=1)

sresults5.5 <- ergm(smodel5.5,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(sresults5.5)
dev.off()
mcmc.diagnostics(sresults5.5,vars.per.page=5)


fit5.5=gof(sresults5.5~distance+espartners,nsim=40,burnin=500000)
dev.off()
par(mfrow=c(2,2))
summary(fit5.5)
plot(fit5.5)

###############################################

smodel5.6 <- smat1~edges+mutual+asymmetric('PersonSmoke')+gwidegree(fixed=T)+gwodegree(fixed=T)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('PersonSmoke')+nodeifactor('PersonSmoke',base=1)+nodeofactor('PersonSmoke',base=1)

sresults5.6 <- ergm(smodel5.6,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(sresults5.6)
dev.off()
mcmc.diagnostics(sresults5.6,vars.per.page=5)


fit5.6=gof(sresults5.6~distance+espartners,nsim=40,burnin=500000)
dev.off()
par(mfrow=c(2,2))
summary(fit5.6)
plot(fit5.6)