smodel4 <- smat1~edges+mutual+gwidegree(fixed=T)+gwodegree(fixed=T)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('Gender')+nodeifactor('Gender',base=1)+nodeofactor('Gender',base=1)
sresults4 <- ergm(smodel4,burnin=500000,MCMCsamplesize=30000,parallel=4,constraints=~bd(maxout=10))
summary(sresults2.4)
dev.off()
mcmc.diagnostics(sresults2.4,vars.per.page=5)


fit2.4=gof(sresults2.4~distance+espartners,nsim=40,burnin=500000)
dev.off()
par(mfrow=c(2,2))
summary(fit2.4)
plot(fit2.4)