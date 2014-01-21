smodel2 <- smat1~edges+mutual+gwidegree(fixed=T)+gwodegree(fixed=T)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('Gender')+nodeicov('Gender')+nodeocov('Gender')
sresults2 <- ergm(smodel2,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(sresults2)

dev.off()
mcmc.diagnostics(sresults2,vars.per.page=9)




##########################################
smodel2.1 <- smat1~edges+mutual+gwidegree(fixed=T)+gwodegree(fixed=T)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('Gender')+nodeifactor('Gender')+nodeofactor('Gender')
sresults2.1 <- ergm(smodel2.1,burnin=500000,MCMCsamplesize=100000,parallel=4,constraints=~bd(maxout=10))
summary(sresults2.1)

mcmc.diagnostics(sresults2.1,vars.per.page=5)


fit2.1=gof(sresults2.1~distance+espartners,nsim=40,burnin=500000)
dev.off()
par(mfrow=c(2,2))
summary(fit2.1)
plot(fit2.1)

####################################

smodel2.2 <- smat1~edges+mutual+gwidegree(fixed=T)+gwodegree(fixed=T)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('Gender')+nodeifactor('Gender')+nodeofactor('Gender')
sresults2.2 <- ergm(smodel2.2,burnin=500000,MCMCsamplesize=300000,parallel=4,constraints=~bd(maxout=10))
summary(sresults2.2)

mcmc.diagnostics(sresults2.2,vars.per.page=5)


fit2.2=gof(sresults2.2~distance+espartners,nsim=40,burnin=500000)
dev.off()
par(mfrow=c(2,2))
summary(fit2.2)
plot(fit2.2)

###################################

smodel2.3 <- smat1~edges+mutual+gwidegree(fixed=T)+gwodegree(fixed=T)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('Gender')+nodeifactor('Gender',base=1)+nodeofactor('Gender',base=1)
sresults2.3 <- ergm(smodel2.3,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(sresults2.3)
dev.off()
mcmc.diagnostics(sresults2.3,vars.per.page=5)


fit2.3=gof(sresults2.3~distance+espartners,nsim=40,burnin=500000)
dev.off()
par(mfrow=c(2,2))
summary(fit2.3)
plot(fit2.3)

######################################

smodel2.4 <- smat1~edges+mutual+gwidegree(fixed=T)+gwodegree(fixed=T)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('Gender')+nodeifactor('Gender',base=1)+nodeofactor('Gender',base=1)
sresults2.4 <- ergm(smodel2.4,burnin=500000,MCMCsamplesize=30000,parallel=4,constraints=~bd(maxout=10))
summary(sresults2.4)
dev.off()
mcmc.diagnostics(sresults2.4,vars.per.page=5)


fit2.4=gof(sresults2.4~distance+espartners,nsim=40,burnin=500000)
dev.off()
par(mfrow=c(2,2))
summary(fit2.4)
plot(fit2.4)


######################################

smodel2.5 <- smat1~edges+mutual+gwidegree(fixed=T)+gwodegree(fixed=T)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('Gender')+nodeifactor('Gender',base=1)+nodeofactor('Gender',base=1)

sresults2.5 <- ergm(smodel2.5,burnin=600000,MCMCsamplesize=50000,parallel=4,constraints=~bd(maxout=10))

summary(sresults2.5)
dev.off()
mcmc.diagnostics(sresults2.5,vars.per.page=5)


fit2.5=gof(sresults2.5~distance+espartners,nsim=40,burnin=500000)
dev.off()
par(mfrow=c(2,2))
summary(fit2.5)
plot(fit2.5)



########################################
