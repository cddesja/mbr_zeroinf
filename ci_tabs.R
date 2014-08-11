library(lsr)

#
# Confidence Interval
#

nbh.result.length = list()
nbh.result.coverage = list()
zinb.result.length = list()
zinb.result.coverage = list()

## Putting together the CI tables
for(j in 1:2){
  for(l in 1:2){
    for(m in 1:2){
      for(k in 1:6){
        fsumstatNBH=paste("/Users/chris/Dropbox/um/phd/analysis/Simulation/nbhmodels/confidenceintervals/nbhCI-",j,k,l,m,".rdata", sep="")
        fsumstatZINB=paste("/Users/chris/Dropbox/um/phd/analysis/Simulation/zinbmodels/confidenceintervals/zinbCI-",j,k,l,m,".rdata", sep="")
        load(fsumstatNBH)
        nbh.result.length[[paste(j,k,l,m)]] <- LengthCI  
        nbh.result.coverage[[paste(j,k,l,m)]] <- ContainsTrue
        rm(LengthCI,ContainsTrue)

        load(fsumstatZINB)
        zinb.result.length[[paste(j,k,l,m)]] <- LengthCI  
        zinb.result.coverage[[paste(j,k,l,m)]] <- ContainsTrue
        rm(LengthCI,ContainsTrue)
      }
    }
  }
}

length.nbh = matrix(ncol=18,nrow=48)
coverage.nbh = matrix(ncol=18,nrow=48)
length.zinb = matrix(ncol=18,nrow=48)
coverage.zinb = matrix(ncol=18,nrow=48)

for(i in 1:48){
length.nbh[i,] = c(nbh.result.length[[i]][,2],nbh.result.length[[i]][,3],nbh.result.length[[i]][,4],nbh.result.length[[i]][,5],nbh.result.length[[i]][,6],nbh.result.length[[i]][,7])
coverage.nbh[i,] = c(nbh.result.coverage[[i]][,2],nbh.result.coverage[[i]][,3],nbh.result.coverage[[i]][,4],nbh.result.coverage[[i]][,5],nbh.result.coverage[[i]][,6],nbh.result.coverage[[i]][,7])
length.zinb[i,] = c(zinb.result.length[[i]][,2],zinb.result.length[[i]][,3],zinb.result.length[[i]][,4],zinb.result.length[[i]][,5],zinb.result.length[[i]][,6],zinb.result.length[[i]][,7])
coverage.zinb[i,] = c(zinb.result.coverage[[i]][,2],zinb.result.coverage[[i]][,3],zinb.result.coverage[[i]][,4],zinb.result.coverage[[i]][,5],zinb.result.coverage[[i]][,6],zinb.result.coverage[[i]][,7])
}

NAMES = c("b0","b0","b0","b1","b1","b1","b2","b2","b2","g0","g0","g0","g1","g1","g1","g2","g2","g2")

colnames(length.nbh) = NAMES
colnames(coverage.nbh) = NAMES
colnames(length.zinb) = NAMES
colnames(coverage.zinb) = NAMES

length.nbh = as.data.frame(length.nbh)
coverage.nbh = as.data.frame(coverage.nbh)
length.zinb = as.data.frame(length.zinb)
coverage.zinb = as.data.frame(coverage.zinb)

length.nbh$col = c(rep(0,24),rep(.3,24))
length.nbh$theta = c(rep(1/4,4),rep(1/2,4),rep(1,4),rep(2,4),rep(5,4),rep(10,4))
length.nbh$beta1 = rep(c(0,.5),24)
length.nbh$gamma1 = rep(c(rep(0,2),rep(-.5,2)),12)

coverage.nbh$col = c(rep(0,24),rep(.3,24))
coverage.nbh$theta = c(rep(1/4,4),rep(1/2,4),rep(1,4),rep(2,4),rep(5,4),rep(10,4))
coverage.nbh$beta1 = rep(c(0,.5),24)
coverage.nbh$gamma1 = rep(c(rep(0,2),rep(-.5,2)),12)

length.zinb$col = c(rep(0,24),rep(.3,24))
length.zinb$theta = c(rep(1/4,4),rep(1/2,4),rep(1,4),rep(2,4),rep(5,4),rep(10,4))
length.zinb$beta1 = rep(c(0,.5),24)
length.zinb$gamma1 = rep(c(rep(0,2),rep(-.5,2)),12)

coverage.zinb$col = c(rep(0,24),rep(.3,24))
coverage.zinb$theta = c(rep(1/4,4),rep(1/2,4),rep(1,4),rep(2,4),rep(5,4),rep(10,4))
coverage.zinb$beta1 = rep(c(0,.5),24)
coverage.zinb$gamma1 = rep(c(rep(0,2),rep(-.5,2)),12)

marg.length.nbh =rbind(length.nbh[,c(1,4,7,10,13,16,19:22)],length.nbh[,c(2,5,8,11,14,17,19:22)],length.nbh[,c(3,6,9,12,15,18,19:22)])
marg.length.nbh$n <- c(rep(100, 48), rep(250, 48), rep(500, 48))
marg.coverage.nbh =rbind(coverage.nbh[,c(1,4,7,10,13,16,19:22)],coverage.nbh[,c(2,5,8,11,14,17,19:22)],coverage.nbh[,c(3,6,9,12,15,18,19:22)])
marg.coverage.nbh$n <- c(rep(100, 48), rep(250, 48), rep(500, 48))
marg.length.zinb =rbind(length.zinb[,c(1,4,7,10,13,16,19:22)],length.zinb[,c(2,5,8,11,14,17,19:22)],length.zinb[,c(3,6,9,12,15,18,19:22)])
marg.length.zinb$n <- c(rep(100, 48), rep(250, 48), rep(500, 48))
marg.coverage.zinb =rbind(coverage.zinb[,c(1,4,7,10,13,16,19:22)],coverage.zinb[,c(2,5,8,11,14,17,19:22)],coverage.zinb[,c(3,6,9,12,15,18,19:22)])
marg.coverage.zinb$n <- c(rep(100, 48), rep(250, 48), rep(500, 48))

length.ci <- rbind(marg.length.nbh, marg.length.zinb)
length.ci$model <- c(rep("NBH", 144), rep("ZINB", 144))
length.ci$col <- length.ci$col
length.ci$theta <- length.ci$theta
length.ci$beta1 <- length.ci$beta1
length.ci$gamma1 <- length.ci$gamma1
length.ci$n <- length.ci$n
length.ci$model <- as.factor(length.ci$model)

coverage.ci <- rbind(marg.coverage.nbh, marg.coverage.zinb)
coverage.ci$model <- c(rep("NBH", 144), rep("ZINB", 144))

dist <- abs(coverage.ci[,1:6] - .95)
names(dist) <- paste("dist", names(dist), sep = "")
coverage.ci <- cbind(coverage.ci,dist)

# Confidence Interval Length
ci_l_aov <- list()
for(i in 1:6){
  ci_l_aov[[i]] <- aov(length.ci[,i]~ col*theta*beta1*gamma1*model*n,
                        length.ci)
}

step_ci_l <- list()
for(i in 1:6){
  step_ci_l[[i]] <- step(ci_l_aov[[i]], k = log(288))  # log(288), BIC
}

etas_ci_l <- list()
for(i in 1:6){
  etas_ci_l[[i]] <- etaSquared(step_ci_l[[i]], anova = TRUE)
}

lengths <- NULL
for(i in 1:6){
  lengths <- rbind(lengths,as.data.frame(etas_ci_l[[i]]))
}

write(print(xtable(lengths), type = "html"), file = "ci.html")

#
# Confidence Interval Coverage
# 

ci_c_aov <- list()
for(i in 1:6){
  ci_c_aov[[i]] <- aov(coverage.ci[,i+12]~ col*theta*beta1*gamma1*model*n,
                       coverage.ci)
}

step_ci_c <- list()
for(i in 1:6){
  step_ci_c[[i]] <- step(ci_c_aov[[i]], k = log(288))  # log(288), BIC
}

etas_ci_c <- list()
for(i in 1:6){
  etas_ci_c[[i]] <- etaSquared(step_ci_c[[i]], anova = TRUE)
}

coverage <- NULL
for(i in 1:6){
  coverage <- rbind(coverage,as.data.frame(etas_ci_c[[i]]))
}

write(print(xtable(coverage), type = "html"), file = "coverage.html")

#
# Structural Zeros
#

zinb.sz.results = list()

setwd("/Users/chris/Dropbox/um/phd/analysis/Simulation/zinbmodels/")
for(j in 1:2){
  for(k in 1:6){
    for(l in 1:2){
      for(m in 1:2){
        fsumstat=paste("struczeros/zinbSZ-",j,k,l,m,".rdata", sep="")
        load(fsumstat)
        zinb.sz.results[[paste(j,k,l,m)]] <- StrZeroTab
      }
    }
  }
}

sz = matrix(ncol=3,nrow=48)
for(i in 1:48){
  sz[i,] = zinb.sz.results[[i]][,2]
}

zeros = c(sz[,1],sz[,2],sz[,3])
N = c(rep(100,48),rep(250,48),rep(500,48))
col = rep(c(rep(0,24),rep(.3,24)),3)
theta = rep(c(rep(.25,4),rep(.5,4),rep(1,4),rep(2,4),rep(5,4),rep(10,4)),6)
beta1 = rep(c(0,.5),24)
gamma1 = rep(c(rep(0,2),rep(-.5,2)),12)

s.zeros = data.frame(zeros,N,col,theta,beta1,gamma1)




