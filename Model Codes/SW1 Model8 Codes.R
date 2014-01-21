###################################
#Model8 is the base model for the smoking attribute and is based on Model7.8

smodel8 <- smat1~edges+mutual('PersonSmoke')+asymmetric('PersonSmoke')+gwidegree(fixed=F)+gwodegree(fixed=F)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('Gender')+nodeifactor('Gender')+nodeofactor('Gender')+nodematch('Ethnic')+nodeifactor('Ethnic')+nodeofactor('Ethnic')+nodematch('PersonSmoke')+nodeifactor('PersonSmoke',base=1)+nodeofactor('PersonSmoke',base=1)

sresults8 <- ergm(smodel8,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(sresults8)
dev.off()
mcmc.diagnostics(sresults8,vars.per.page=8)

sfit8=gof(sresults8~distance+espartners+triadcensus,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(sfit8)
plot(sfit8)

##############################################################
#########################################
#adds an additional unattributed mutual term to the base model
smodel8.1 <- smat1~edges+mutual('PersonSmoke')+asymmetric('PersonSmoke')+mutual+gwidegree(fixed=F)+gwodegree(fixed=F)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('Gender')+nodeifactor('Gender')+nodeofactor('Gender')+nodematch('Ethnic')+nodeifactor('Ethnic')+nodeofactor('Ethnic')+nodematch('PersonSmoke')+nodeifactor('PersonSmoke',base=1)+nodeofactor('PersonSmoke',base=1)

sresults8.1 <- ergm(smodel8.1,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(sresults8.1)

dev.off()
mcmc.diagnostics(sresults8.1,vars.per.page=9)

sfit8.1=gof(sresults8.1~distance+espartners+triadcensus,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(sfit8.1)
plot(sfit8.1)

#############################

#adds the ctriple parameter to 8.1

smodel8.2 <- smat1~edges+ctriple+mutual('PersonSmoke')+asymmetric('PersonSmoke')+mutual+gwidegree(fixed=F)+gwodegree(fixed=F)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('Gender')+nodeifactor('Gender')+nodeofactor('Gender')+nodematch('Ethnic')+nodeifactor('Ethnic')+nodeofactor('Ethnic')+nodematch('PersonSmoke')+nodeifactor('PersonSmoke',base=1)+nodeofactor('PersonSmoke',base=1)

sresults8.2 <- ergm(smodel8.2,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(sresults8.2)
dev.off()
mcmc.diagnostics(sresults8.2,vars.per.page=9)

sfit8.2=gof(sresults8.2~distance+espartners+triadcensus,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(sfit8.2)
plot(sfit8.2)
######################################
#same as 8.1 but removes the ethnic parameter
smodel8.3 <- smat1~edges+mutual('PersonSmoke')+asymmetric('PersonSmoke')+mutual+gwidegree(fixed=F)+gwodegree(fixed=F)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('Gender')+nodeifactor('Gender')+nodeofactor('Gender')+nodematch('Ethnic')+nodeifactor('Ethnic')+nodeofactor('Ethnic')+nodematch('PersonSmoke')+nodeifactor('PersonSmoke',base=1)+nodeofactor('PersonSmoke',base=1)

sresults8.3 <- ergm(smodel8.3,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(sresults8.3)
dev.off()
mcmc.diagnostics(sresults8.3,vars.per.page=9)

sfit8.3=gof(sresults8.3~distance+espartners+triadcensus,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(sfit8.3)
plot(sfit8.3)

#######################################

#replaces the ethnic parameter but also adds the twopath parameter

smodel8.4 <- smat1~edges+twopath+mutual('PersonSmoke')+asymmetric('PersonSmoke')+mutual+gwidegree(fixed=F)+gwodegree(fixed=F)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('Gender')+nodeifactor('Gender')+nodeofactor('Gender')+nodematch('Ethnic')+nodeifactor('Ethnic')+nodeofactor('Ethnic')+nodematch('PersonSmoke')+nodeifactor('PersonSmoke',base=1)+nodeofactor('PersonSmoke',base=1)

sresults8.4 <- ergm(smodel8.4,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(sresults8.4)
dev.off()
mcmc.diagnostics(sresults8.4,vars.per.page=9)

sfit8.4=gof(sresults8.4~distance+espartners+triadcensus,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(sfit8.4)
plot(sfit8.4)