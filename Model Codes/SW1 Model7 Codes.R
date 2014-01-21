## This model7 is a larger model with a modest burnin 
smodel7 <- smat1~edges+mutual('PersonDrink')+asymmetric('PersonDrink')+gwidegree(fixed=T)+gwodegree(fixed=T)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('Gender')+nodeifactor('Gender')+nodeofactor('Gender')+nodematch('Ethnic')+nodeifactor('Ethnic')+nodeofactor('Ethnic')+nodematch('PersonDrink')+nodeifactor('PersonDrink',base=1)+nodeofactor('PersonDrink',base=1)

sresults7 <- ergm(smodel7,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(sresults7)
dev.off()
mcmc.diagnostics(sresults7,vars.per.page=5)

sfit7=gof(sresults7~distance+espartners+triadcensus,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(sfit7)
plot(sfit7)

######################################
#same as base model but adds an edges constraint 
smodel7.1 <- smat1~edges+mutual('PersonDrink')+asymmetric('PersonDrink')+gwidegree(fixed=T)+gwodegree(fixed=T)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('Gender')+nodeifactor('Gender')+nodeofactor('Gender')+nodematch('Ethnic')+nodeifactor('Ethnic')+nodeofactor('Ethnic')+nodematch('PersonDrink')+nodeifactor('PersonDrink',base=1)+nodeofactor('PersonDrink',base=1)

sresults7.1 <- ergm(smodel7.1,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~edges+bd(maxout=10))
summary(sresults7.1)
dev.off()
mcmc.diagnostics(sresults7.1,vars.per.page=5)

sfit7.1=gof(sresults7.1~distance+espartners,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(sfit7.1)
plot(sfit7.1)
#######################################

#like the base model but adds a triangle term with special attribute attention
smodel7.2 <- smat1~edges+triangle('PersonDrink')+mutual('PersonDrink')+asymmetric('PersonDrink')+gwidegree(fixed=T)+gwodegree(fixed=T)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('Gender')+nodeifactor('Gender')+nodeofactor('Gender')+nodematch('Ethnic')+nodeifactor('Ethnic')+nodeofactor('Ethnic')+nodematch('PersonDrink')+nodeifactor('PersonDrink',base=1)+nodeofactor('PersonDrink',base=1)

sresults7.2 <- ergm(smodel7.2,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(sresults7.2)
dev.off()
mcmc.diagnostics(sresults7.2,vars.per.page=5)

sfit7.2=gof(sresults7.2~distance+espartners+triadcensus,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(sfit7.2)
plot(sfit7.2)

################################
#Same as 7.1 but update the coefficients from 7.1. maintaining the edges constraint


sresults7.3 <- ergm(smodel7.1,theta0=sresults7.1$coef,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~edges+bd(maxout=10))
summary(sresults7.3)
dev.off()
mcmc.diagnostics(sresults7.3,vars.per.page=5)

sfit7.3=gof(sresults7.3~distance+espartners+triadcensus,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(sfit7.3)
plot(sfit7.3)

####################################
#updates model7
sresults7.4 <- ergm(smodel7,theta0=sresults7$coef,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(sresults7.4)
dev.off()
mcmc.diagnostics(sresults7.4,vars.per.page=5)

sfit7.4=gof(sresults7.4~distance+espartners+triadcensus,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(sfit7.4)
plot(sfit7.4)

######################################
#updates model7.2 parameters

#Same as 7.1 but update the coefficients from 7.1. maintaining the edges constraint and triangle parameter


sresults7.5 <- ergm(smodel7.2,theta0=sresults7.2$coef,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~edges+bd(maxout=10))
summary(sresults7.5)
dev.off()
mcmc.diagnostics(sresults7.5,vars.per.page=5)

sfit7.5=gof(sresults7.5~distance+espartners+triadcensus,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(sfit7.5)
plot(sfit7.5)

######################
#same as base model but unfixes the gwi and gwo degrees and also changes the alpha for qwesp and qwdsp to .4 from .25

smodel7.6 <- smat1~edges+mutual('PersonDrink')+asymmetric('PersonDrink')+gwidegree(fixed=F)+gwodegree(fixed=F)+gwesp(.4,fixed=T)+gwdsp(.4,fixed=TRUE)+nodematch('Gender')+nodeifactor('Gender')+nodeofactor('Gender')+nodematch('Ethnic')+nodeifactor('Ethnic')+nodeofactor('Ethnic')+nodematch('PersonDrink')+nodeifactor('PersonDrink',base=1)+nodeofactor('PersonDrink',base=1)

sresults7.6 <- ergm(smodel7.6,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(sresults7.6)
dev.off()
mcmc.diagnostics(sresults7.6,vars.per.page=5)

sfit7.6=gof(sresults7.6~distance+espartners+triadcensus,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(sfit7.6)
plot(sfit7.6)

############################################################
#same as base but fixes gwi and gwo and leaves teh gwesp at .4 and gwdsp at .4
smodel7.7 <- smat1~edges+mutual('PersonDrink')+asymmetric('PersonDrink')+gwidegree(fixed=T)+gwodegree(fixed=T)+gwesp(.4,fixed=T)+gwdsp(.4,fixed=TRUE)+nodematch('Gender')+nodeifactor('Gender')+nodeofactor('Gender')+nodematch('Ethnic')+nodeifactor('Ethnic')+nodeofactor('Ethnic')+nodematch('PersonDrink')+nodeifactor('PersonDrink',base=1)+nodeofactor('PersonDrink',base=1)

sresults7.7 <- ergm(smodel7.7,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(sresults7.7)
dev.off()
mcmc.diagnostics(sresults7.7,vars.per.page=5)

sfit7.7=gof(sresults7.7~distance+espartners+triadcensus,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(sfit7.7)
plot(sfit7.7)

###################################
#Same as base but unfixes gwi/gwodegree and replaces the gwesp and gwdsp back to .25

smodel7.8 <- smat1~edges+mutual('PersonDrink')+asymmetric('PersonDrink')+gwidegree(fixed=F)+gwodegree(fixed=F)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('Gender')+nodeifactor('Gender')+nodeofactor('Gender')+nodematch('Ethnic')+nodeifactor('Ethnic')+nodeofactor('Ethnic')+nodematch('PersonDrink')+nodeifactor('PersonDrink',base=1)+nodeofactor('PersonDrink',base=1)

sresults7.8 <- ergm(smodel7.8,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(sresults7.8)
dev.off()
mcmc.diagnostics(sresults7.8,vars.per.page=5)

sfit7.8=gof(sresults7.8~distance+espartners+triadcensus,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(sfit7.8)
plot(sfit7.8)

##############################################################
#same as 7.8 but with a larger burnin

smodel7.9 <- smat1~edges+mutual('PersonDrink')+asymmetric('PersonDrink')+gwidegree(fixed=F)+gwodegree(fixed=F)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('Gender')+nodeifactor('Gender')+nodeofactor('Gender')+nodematch('Ethnic')+nodeifactor('Ethnic')+nodeofactor('Ethnic')+nodematch('PersonDrink')+nodeifactor('PersonDrink',base=1)+nodeofactor('PersonDrink',base=1)

sresults7.9 <- ergm(smodel7.9,burnin=500000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(sresults7.9)
dev.off()
mcmc.diagnostics(sresults7.9,vars.per.page=5)

sfit7.9=gof(sresults7.9~distance+espartners+triadcensus,nsim=40,burnin=500000)
dev.off()
par(mfrow=c(2,2))
summary(sfit7.9)
plot(sfit7.9)

sfit7.9.5=gof(sresults7.9,nsim=40,burnin=500000)
dev.off()
par(mfrow=c(2,2))
summary(sfit7.9.5)
plot(sfit7.9.5)

#########################################
#adds an additional mutual term to 7.9
smodel7.10 <- smat1~edges+mutual('PersonDrink')+asymmetric('PersonDrink')+mutual+gwidegree(fixed=F)+gwodegree(fixed=F)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('Gender')+nodeifactor('Gender')+nodeofactor('Gender')+nodematch('Ethnic')+nodeifactor('Ethnic')+nodeofactor('Ethnic')+nodematch('PersonDrink')+nodeifactor('PersonDrink',base=1)+nodeofactor('PersonDrink',base=1)

sresults7.10 <- ergm(smodel7.10,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(sresults7.10)
dev.off()
mcmc.diagnostics(sresults7.10,vars.per.page=5)

sfit7.10=gof(sresults7.10~distance+espartners+triadcensus,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(sfit7.10)
plot(sfit7.10)

#############################

#################################
#increases burnin and sample size to model 7.10
smodel7.12 <- smat1~edges+mutual('PersonDrink')+asymmetric('PersonDrink')+mutual+gwidegree(fixed=F)+gwodegree(fixed=F)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('Gender')+nodeifactor('Gender')+nodeofactor('Gender')+nodematch('Ethnic')+nodeifactor('Ethnic')+nodeofactor('Ethnic')+nodematch('PersonDrink')+nodeifactor('PersonDrink',base=1)+nodeofactor('PersonDrink',base=1)

sresults7.12 <- ergm(smodel7.12,burnin=500000,MCMCsamplesize=50000,parallel=4,constraints=~bd(maxout=10))
summary(sresults7.12)
dev.off()
mcmc.diagnostics(sresults7.12,vars.per.page=5)

sfit7.12=gof(sresults7.12~distance+espartners+triadcensus,nsim=40,burnin=500000)
dev.off()
par(mfrow=c(2,2))
summary(sfit7.12)
plot(sfit7.12)

#####################################

smodel7.13 <- smat1~edges+twopath+mutual('PersonDrink')+asymmetric('PersonDrink')+gwidegree(fixed=F)+gwodegree(fixed=F)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('Gender')+nodeifactor('Gender')+nodeofactor('Gender')+nodematch('Ethnic')+nodeifactor('Ethnic')+nodeofactor('Ethnic')+nodematch('PersonDrink')+nodeifactor('PersonDrink',base=1)+nodeofactor('PersonDrink',base=1)

sresults7.13 <- ergm(smodel7.13,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(sresults7.13)
dev.off()
mcmc.diagnostics(sresults7.13,vars.per.page=5)

sfit7.13=gof(sresults7.13~distance+espartners+triadcensus,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(sfit7.13)
plot(sfit7.13)