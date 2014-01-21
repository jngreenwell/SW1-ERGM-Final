smodel6 <- smat1~edges+mutual('PersonDrink')+asymmetric('PersonDrink')+gwidegree(fixed=T)+gwodegree(fixed=T)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('PersonDrink')+nodeifactor('PersonDrink',base=1)+nodeofactor('PersonDrink',base=1)

sresults6 <- ergm(smodel6,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(sresults6)
dev.off()
mcmc.diagnostics(sresults6,vars.per.page=5)


fit6=gof(sresults6~distance+espartners,nsim=40,burnin=500000)
dev.off()
par(mfrow=c(2,2))
summary(fit6)
plot(fit6)

############################
#adds a twopath term
smodel6.1 <- smat1~edges+twopath+mutual('PersonDrink')+asymmetric('PersonDrink')+gwidegree(fixed=T)+gwodegree(fixed=T)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('PersonDrink')+nodeifactor('PersonDrink',base=1)+nodeofactor('PersonDrink',base=1)

sresults6.1 <- ergm(smodel6.1,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(sresults6.1)
dev.off()
mcmc.diagnostics(sresults5,vars.per.page=5)


fit5.1=gof(sresults5~distance+espartners,nsim=40,burnin=500000)
dev.off()
par(mfrow=c(2,2))
summary(fit5)
plot(fit5)