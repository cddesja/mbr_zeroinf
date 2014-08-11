# Required libraries
library(pscl)
library(bindata)
library(xtable)

# Data-generating mechanism NBH
data.gen.nbh=function(prob,mu.nb,size.nb)
{
  y.bin=rbinom(1,size=1,prob)
  if( y.bin == 1)
  {
    y.int=rnbinom(1, mu=mu.nb, size=size.nb)
    while(y.int==0)
    {
      y.int=rnbinom(1, mu=mu.nb, size=size.nb)
    }
    y=y.int
  } else
  {
    y=y.bin
  }
  return(y)
}

# Data-generating mechanism ZINB
data.gen.zinb = function(prob, mu.nb,size.nb)
{
  y.unif = runif(1)
  if(y.unif <= prob)
  {
    y = 0
    # Is it a structural zero?
    y.struct = 1
  }
  else
  {
    y.samp = rnbinom(1, mu=mu.nb, size=size.nb)
    y = y.samp
    # Is it a structural zero?
    y.struct = 0
  }
  return(c(y,y.struct))  
}
 
# Setting the Conditions #
NOBS=c(100,250,500) 
multicol=c(0,.3) 
p = rep(.5,4) 
theta=c(1/4,1/2,1,2,5,10) 
gamma.weights=list(gamma1=c(1.5,0,-2),
                   gamma2=c(1.5,-.5,-2))
gamma.names = c("First","Second")
beta.weights=list(beta1=c(-1.5,0,2),
                  beta2=c(-1.5,.5,2))
beta.names = c("First","Second")
nsims=2000

# Generate the predictor data, where the components are uncorrelated in the population and that within a component the correlation is either 0 or 0.3
G=list()
cor.tmp=list()
for(rho in 1:2){
  r=multicol[rho]
  rho.cor=cbind(c(1,r,0,0),c(r,1,0,0),c(0,0,1,r),c(0,0,r,1))
    for(n in 1:3){
      cor.tmp[[n]] = rmvbin(NOBS[n],p,bincorr=rho.cor)
}
  G[[rho]]=cor.tmp
}

####################################
# -------------------------------- #
# GENERATE DATA FROM NBH AND ZINB  #
# -------------------------------- #
####################################

# ------------ #
#  NBH MODELS  #                       
# ------------ #

### 
# hurdle.data
# First list corresponds to sample size: 25,50,100
# Second list corresponds to multicollineary: 0, .30
# Third list corresponds to dispersion: 1/2, 1/5, 1/10
# Fourth list corresponds to gamma weights - see above
# Five list corresponds to beta weights
# Sixth list is the simulates!
###

# Start with empty lists
hurdle.data=list()
tmp.gamma = list()
tmp.beta = list()
simulates = list()  
tmp.multicol = list()
tmp.dis = list()


# Generate the data from the NBH model #
# This data is available in hurdle.Rdata

# system.time(for(n in 1:3){
#   sample.size = NOBS[n]
#     for(multi.col in 1:2){
#       X = model.matrix(~G[[multi.col]][[n]][,3] + G[[multi.col]][[n]][,4]) 
#       Z = model.matrix(~G[[multi.col]][[n]][,1] + G[[multi.col]][[n]][,2])
#         for(k in 1:6){
#           size.nb = theta[k]
#             for(gweights in 1:2){
#               gamma = gamma.weights[[gweights]]
#               for(bweights in 1:2){
#                 beta = beta.weights[[bweights]]
#                   for(sims in 1:nsims){
#                     mu.nb = exp(X%*%gamma)
#                     prob = plogis(Z%*%beta)
#                     y=NULL
#                       for(i in 1:sample.size){
#                           y[i] = data.gen.nbh(prob=prob[i],mu.nb=mu.nb[i],size.nb=size.nb)
#                       }
#             simulates[[sims]]= y
#                   }
#           tmp.beta[[bweights]]=simulates
#               }
#         tmp.gamma[[gweights]]=tmp.beta
#             }
#     tmp.dis[[k]] = tmp.gamma
#         }
#   tmp.multicol[[multi.col]]=tmp.dis
#     }
#   hurdle.data[[n]] = tmp.multicol
# }
# )
# 
# ########
# # SAVE #
# ########
# 
# ## Save response data and G-matrix
# save(hurdle.data,G,file = "hurdleData.rdata") 
# 
# # ------------- #
# #  ZINB MODELS  #                       
# # ------------- #
# 
# # zinb.data
# # First list corresponds to sample size: 25,50,100
# # Second list corresponds to multicollineary: 0, .30
# # Third list corresponds to dispersion: 2, 5, 10
# # Fourth list corresponds to gamma weights - see above
# # Five list corresponds to beta weights
# # Sixth list is the simulates!
# # First column corresponds to the response and second to the structural zero.
# ### 

# Start with empty lists
zinb.data=list()
tmp.gamma = list()
tmp.beta = list()
simulates = list()  
tmp.multicol = list()
tmp.dis = list()
 
# # Generate the response data #
# This data is available in zinbData.Rdata

# # A little over 2 minutes
# system.time(for(n in 1:3){
#   sample.size = NOBS[n]
#   for(multi.col in 1:2){
#     X = model.matrix(~G[[multi.col]][[n]][,3] + G[[multi.col]][[n]][,4]) 
#     Z = model.matrix(~G[[multi.col]][[n]][,1] + G[[multi.col]][[n]][,2])
#     for(k in 1:6){
#       size.nb = theta[k]
#       for(gweights in 1:2){
#         gamma = gamma.weights[[gweights]]
#         for(bweights in 1:2){
#           beta = beta.weights[[bweights]]
#           for(sims in 1:nsims){
#             mu.nb = exp(X%*%gamma)
#             prob = plogis(Z%*%beta)
#             y=matrix(ncol=2,nrow=sample.size)
#             for(i in 1:sample.size){
#               y[i,] = data.gen.zinb(prob=prob[i],mu.nb=mu.nb[i],size.nb=size.nb)
#             }
#             simulates[[sims]]= y
#           }
#           tmp.beta[[bweights]]=simulates
#         }
#         tmp.gamma[[gweights]]=tmp.beta
#       }
#       tmp.dis[[k]] = tmp.gamma
#     }
#     tmp.multicol[[multi.col]]=tmp.dis
#   }
#   zinb.data[[n]] = tmp.multicol
# }
# )
# 
# ########
# # SAVE #
# ########
# 
# ## Save response data and G-matrix
# save(zinb.data,G,file = "zinbData.rdata") 

################################
# ---------------------------- #
# FIT THE NBH AND ZINB MODELS  #
# ---------------------------- #
################################


# ----------------- # 
# Run the NBH Model #
# ----------------- #

## Load data
load(file = "hurdleData.rdata") 

## Run the NBH model and save them in nbh.models
# Takes a very long (greater than 4 hours)
system.time(
  for(i in 1:3){
    for(j in 1:2){
      for(k in 1:6){
        for(l in 1:2){
          for(m in 1:2){
            y.model=list()
            for(s in 1:nsims){
              print(sprintf("Sample Size-%s Collinearity-%s Phi-%s Gamma-%s Beta-%s Sim-%s",
                            NOBS[i], multicol[j], theta[k],gamma.names[l],beta.names[m], s))
              y.model[[s]]=hurdle(hurdle.data[[i]][[j]][[k]][[l]][[m]][[s]]~G[[j]][[i]][,3] + G[[j]][[i]][,4] | G[[j]][[i]][,1] + G[[j]][[i]][,2], dist="negbin")
             # y.model[[s]]=try(hurdle(hurdle.data[[i]][[j]][[k]][[l]][[m]][[s]]~G[[j]][[i]][,3] + G[[j]][[i]][,4] | G[[j]][[i]][,1] + G[[j]][[i]][,2], dist="negbin"))
            }
            # i corresponds to sample size
            # j corresponds to multicollinearity
            # k corresponds dispersion
            # l corresponds to Count parameters - Gamma
            # m corresponds to Zero parameters - Beta
            fname=paste("nbhcond-",i,j,k,l,m,".rdata", sep="")
            save(y.model,file=fname)
            rm(y.model)
          }
        }
      }
    }
  }
)


# ------------------ # 
# Run the ZINB Model #
# ------------------ #

## Load data
load(file = "zinbData.rdata") 

## Run the ZINB model and save them in zinb.models
# The try function keeps track of which models fail to converge
# Takes a very long (greater than 4 hours)
system.time(
for(i in 1:3){
  for(j in 1:2){
    for(k in 4:6){
      for(l in 1:2){
        for(m in 1:2){
            y.model=list()
          for(s in 1:nsims){
            print(sprintf("Sample Size-%s Collinearity-%s Phi-%s Gamma-%s Beta-%s Sim-%s",
                          NOBS[i], multicol[j], theta[k],gamma.names[l],beta.names[m], s))
          y.model[[s]]=zeroinfl(zinb.data[[i]][[j]][[k]][[l]][[m]][[s]][,1]~G[[j]][[i]][,3] + G[[j]][[i]][,4] | G[[j]][[i]][,1] + G[[j]][[i]][,2], dist="negbin")
          }
            # i corresponds to sample size
            # j corresponds to multicollinearity
            # k corresponds dispersion
            # l corresponds to Count parameters - Gamma
            # m corresponds to Zero parameters - Beta
          fname=paste("zinb_cond-",i,j,k,l,m,".rdata", sep="")
          save(y.model,file=fname)
          rm(y.model)
        }
      }
    }
  }
}
)

########################
# -------------------- #
#  PARAMETER RECOVERY  #                       
# -------------------- #
########################

#########
## NBH ##
#########

N = c(100,250,500)# Takes a very long (greater than 4 hours)
system.time(
for(j in 1:2){
    for(k in 1:6){
      for(l in 1:2){
        for(m in 1:2){
          cint.mean = NULL
          cint.median = NULL
          cint.sd = NULL
          
          cgamma1.mean = NULL
          cgamma1.median = NULL
          cgamma1.sd = NULL

          cgamma2.mean = NULL
          cgamma2.median = NULL
          cgamma2.sd = NULL

          zint.mean = NULL
	        zint.median = NULL
          zint.sd = NULL

          zbeta1.mean = NULL
	        zbeta1.median = NULL
          zbeta1.sd = NULL

          zbeta2.mean = NULL
	        zbeta2.median = NULL
          zbeta2.sd = NULL
          
            for(i in 1:3){
              fname=paste("nbhcond-",i,j,k,l,m,".rdata", sep="")
              load(fname)
              count.int = NULL
              count.gamma1 = NULL
              count.gamma2 = NULL
	            zero.int = NULL
              zero.beta1 = NULL
              zero.beta2 = NULL
              
              for(s in 1:2000){
                count.int[s] = coef(y.model[[s]])[1]
                count.gamma1[s] = coef(y.model[[s]])[2]
                count.gamma2[s] =coef(y.model[[s]])[3]
                zero.int[s] =  coef(y.model[[s]])[4]
                zero.beta1[s] = coef(y.model[[s]])[5]
                zero.beta2[s] = coef(y.model[[s]])[6]
              }
              
              cint.tmp = count.int
 	            cint.mean[i] = mean(cint.tmp)
	            cint.median[i] = median(cint.tmp)
	            cint.sd[i] = sd(cint.tmp)

              cgamma1.tmp = count.gamma1
              cgamma1.mean[i] = mean(cgamma1.tmp)
	            cgamma1.median[i] = median(cgamma1.tmp)
              cgamma1.sd[i] = sd(cgamma1.tmp)

              cgamma2.tmp = as.numeric(count.gamma2)
              cgamma2.mean[i] = mean(cgamma2.tmp)
	            cgamma2.median[i] = median(cgamma2.tmp)
              cgamma2.sd[i] = sd(cgamma2.tmp)

	            zint.tmp = zero.int
              zint.mean[i] = mean(zint.tmp)
        	    zint.median[i] = median(zint.tmp)
              zint.sd[i] = sd(zint.tmp)

	            zbeta1.tmp = zero.beta1
              zbeta1.mean[i] = mean(zbeta1.tmp)
	            zbeta1.median[i] = median(zbeta1.tmp)
              zbeta1.sd[i] = sd(zbeta1.tmp)

	            zbeta2.tmp = zero.beta2
              zbeta2.mean[i] = mean(zbeta2.tmp)
	            zbeta2.median[i] = median(zbeta2.tmp)
              zbeta2.sd[i] = sd(zbeta2.tmp)
              rm(y.model)
            }
          print(sprintf("Collinearity-%s Phi-%s Gamma-%s Beta-%s",
                        j, k,l,m))
          tableNames = c("N","Beta0","Beta1","Beta2","Gamma0","Gamma1","Gamma2")
          nbhMeans = cbind(N,zint.mean,zbeta1.mean,zbeta2.mean,cint.mean,cgamma1.mean,cgamma2.mean)
          colnames(nbhMeans) = tableNames
          nbhMedians = cbind(N,zint.median,zbeta1.median,zbeta2.median,cint.median,cgamma1.median,cgamma2.median)
          colnames(nbhMedians) = tableNames
          nbhSDs = cbind(N,zint.sd,zbeta1.sd,zbeta2.sd,cint.sd,cgamma1.sd,cgamma2.sd)
          colnames(nbhSDs) = tableNames
          fsumstat=paste("nbhsumstat-",j,k,l,m,".rdata", sep="")
          save(nbhMeans,nbhMedians,nbhSDs,file=fsumstat)
          rm(nbhMeans,nbhMedians,nbhSDs,tableNames)
        }
      }
    }
  }
)


#######
# MSE #
#######

N = c(100,250,500)
system.time(
  for(j in 1:2){
    for(k in 1:6){
      for(l in 1:2){
        for(m in 1:2){
             truth = NULL
            truth = c(gamma.weights[[l]],beta.weights[[m]])
             cgamma0.mse = NULL
             cgamma1.mse = NULL
             cgamma2.mse = NULL
             zbeta0.mse = NULL
             zbeta1.mse = NULL
             zbeta2.mse = NULL
            for(i in 1:3){
              fname=paste("nbhcond-",i,j,k,l,m,".rdata", sep="")
              load(fname)
              count.int = NULL
              count.gamma1 = NULL
              count.gamma2 = NULL
	            zero.int = NULL
              zero.beta1 = NULL
              zero.beta2 = NULL
              
              for(s in 1:2000){
                count.int[s] = coef(y.model[[s]])[1]
                count.gamma1[s] = coef(y.model[[s]])[2]
                count.gamma2[s] =coef(y.model[[s]])[3]
                zero.int[s] =  coef(y.model[[s]])[4]
                zero.beta1[s] = coef(y.model[[s]])[5]
                zero.beta2[s] = coef(y.model[[s]])[6]
              }
              
 	            cgamma0.mse[i] = mean((count.int - truth[1])^2) + var(count.int)
              cgamma1.mse[i] = mean((count.gamma1 - truth[2])^2) + var(count.gamma1)
              cgamma2.mse[i] = mean((count.gamma2 - truth[3])^2) + var(count.gamma2)
              zbeta0.mse[i] = mean((zero.int - truth[4])^2) + var(zero.int)
              zbeta1.mse[i] = mean((zero.beta1 - truth[5])^2) + var(zero.beta1)
              zbeta2.mse[i] = mean((zero.beta2 - truth[6])^2) + var(zero.beta2)
              rm(y.model)
            }
          print(sprintf("Collinearity-%s Phi-%s Gamma-%s Beta-%s",
                        j, k,l,m))
          tableNames = c("N","Beta0","Beta1","Beta2","Gamma0","Gamma1","Gamma2")
          mse = cbind(N,zbeta0.mse,zbeta1.mse,zbeta2.mse, cgamma0.mse,cgamma1.mse,cgamma2.mse)
          colnames(mse) = tableNames
          fsumstat=paste("nbhmse-",j,k,l,m,".rdata", sep="")
          save(mse,file=fsumstat)
          rm(mse,tableNames)
        }
      }
    }
  }
)


##########
## ZINB ##
##########

N = c(100,250,500)
system.time(
  for(j in 1:2){
    for(k in 1:6){
      for(l in 1:2){
        for(m in 1:2){
          cint.mean = NULL
          cint.median = NULL
          cint.sd = NULL
          
          cgamma1.mean = NULL
          cgamma1.median = NULL
          cgamma1.sd = NULL
          
          cgamma2.mean = NULL
          cgamma2.median = NULL
          cgamma2.sd = NULL
          
          zint.mean = NULL
          zint.median = NULL
          zint.sd = NULL
          
          zbeta1.mean = NULL
          zbeta1.median = NULL
          zbeta1.sd = NULL
          
          zbeta2.mean = NULL
          zbeta2.median = NULL
          zbeta2.sd = NULL
          
          for(i in 1:3){
            fname=paste("zinb_cond-",i,j,k,l,m,".rdata", sep="")
            load(fname)
            count.int = NULL
            count.gamma1 = NULL
            count.gamma2 = NULL
            zero.int = NULL
            zero.beta1 = NULL
            zero.beta2 = NULL
            
            for(s in 1:2000){
              count.int[s] = coef(y.model[[s]])[1]
              count.gamma1[s] = coef(y.model[[s]])[2]
              count.gamma2[s] =coef(y.model[[s]])[3]
              zero.int[s] =  coef(y.model[[s]])[4]
              zero.beta1[s] = coef(y.model[[s]])[5]
              zero.beta2[s] = coef(y.model[[s]])[6]
            }
            
            cint.tmp = count.int
            cint.mean[i] = mean(cint.tmp)
            cint.median[i] = median(cint.tmp)
            cint.sd[i] = sd(cint.tmp)
            
            cgamma1.tmp = count.gamma1
            cgamma1.mean[i] = mean(cgamma1.tmp)
            cgamma1.median[i] = median(cgamma1.tmp)
            cgamma1.sd[i] = sd(cgamma1.tmp)
            
            cgamma2.tmp = count.gamma2
            cgamma2.mean[i] = mean(cgamma2.tmp)
            cgamma2.median[i] = median(cgamma2.tmp)
            cgamma2.sd[i] = sd(cgamma2.tmp)
            
            zint.tmp = zero.int
            zint.mean[i] = mean(zint.tmp)
            zint.median[i] = median(zint.tmp)
            zint.sd[i] = sd(zint.tmp)
            
            zbeta1.tmp = zero.beta1
            zbeta1.mean[i] = mean(zbeta1.tmp)
            zbeta1.median[i] = median(zbeta1.tmp)
            zbeta1.sd[i] = sd(zbeta1.tmp)
            
            zbeta2.tmp = zero.beta2
            zbeta2.mean[i] = mean(zbeta2.tmp)
            zbeta2.median[i] = median(zbeta2.tmp)
            zbeta2.sd[i] = sd(zbeta2.tmp)
            rm(y.model)
          }
          print(sprintf("Collinearity-%s Phi-%s Gamma-%s Beta-%s",
                        j, k,l,m))
          tableNames = c("N","Beta0","Beta1","Beta2","Gamma0","Gamma1","Gamma2")
          zinbMeans = cbind(N,zint.mean,zbeta1.mean,zbeta2.mean,cint.mean,cgamma1.mean,cgamma2.mean)
          colnames(zinbMeans) = tableNames
          zinbMedians = cbind(N,zint.median,zbeta1.median,zbeta2.median,cint.median,cgamma1.median,cgamma2.median)
          colnames(zinbMedians) = tableNames
          zinbSDs = cbind(N,zint.sd,zbeta1.sd,zbeta2.sd,cint.sd,cgamma1.sd,cgamma2.sd)
          colnames(zinbSDs) = tableNames
          fsumstat=paste("zinbsumstat-",j,k,l,m,".rdata", sep="")
          save(zinbMeans,zinbMedians,zinbSDs,file=fsumstat)
          rm(zinbMeans,zinbMedians,zinbSDs,tableNames)
        }
      }
    }
  }
)

#######
# MSE #
#######

N = c(100,250,500)
system.time(
  for(j in 1:2){
    for(k in 1:6){
      for(l in 1:2){
        for(m in 1:2){
          truth = NULL
          truth = c(gamma.weights[[l]],beta.weights[[m]])
          cgamma0.mse = NULL
          cgamma1.mse = NULL
          cgamma2.mse = NULL
          zbeta0.mse = NULL
          zbeta1.mse = NULL
          zbeta2.mse = NULL
          for(i in 1:3){
            fname=paste("zinb_cond-",i,j,k,l,m,".rdata", sep="")
            load(fname)
            count.int = NULL
            count.gamma1 = NULL
            count.gamma2 = NULL
            zero.int = NULL
            zero.beta1 = NULL
            zero.beta2 = NULL
            
            for(s in 1:2000){
              count.int[s] = coef(y.model[[s]])[1]
              count.gamma1[s] = coef(y.model[[s]])[2]
              count.gamma2[s] =coef(y.model[[s]])[3]
              zero.int[s] =  coef(y.model[[s]])[4]
              zero.beta1[s] = coef(y.model[[s]])[5]
              zero.beta2[s] = coef(y.model[[s]])[6]
            }
            
            cgamma0.mse[i] = mean((count.int - truth[1])^2) + var(count.int)
            cgamma1.mse[i] = mean((count.gamma1 - truth[2])^2) + var(count.gamma1)
            cgamma2.mse[i] = mean((count.gamma2 - truth[3])^2) + var(count.gamma2)
            zbeta0.mse[i] = mean((zero.int - truth[4])^2) + var(zero.int)
            zbeta1.mse[i] = mean((zero.beta1 - truth[5])^2) + var(zero.beta1)
            zbeta2.mse[i] = mean((zero.beta2 - truth[6])^2) + var(zero.beta2)
            rm(y.model)
          }
          print(sprintf("Collinearity-%s Phi-%s Gamma-%s Beta-%s",
                        j, k,l,m))
          tableNames = c("N","Beta0","Beta1","Beta2","Gamma0","Gamma1","Gamma2")
          mse = cbind(N,zbeta0.mse,zbeta1.mse,zbeta2.mse, cgamma0.mse,cgamma1.mse,cgamma2.mse)
          colnames(mse) = tableNames
          fsumstat=paste("zinbmse-",j,k,l,m,".rdata", sep="")
          save(mse,file=fsumstat)
          rm(mse,tableNames)
        }
      }
    }
  }
)


##########################
# ---------------------- #
#  CONFIDENCE INTERVALS  #                       
# ---------------------- #
##########################

#########
## NBH ##
#########
NOBS = c(100,250,500)
system.time(
  for(j in 1:2){
    for(k in 1:6){
      for(l in 1:2){
        for(m in 1:2){
            truth = NULL
            truth = c(gamma.weights[[l]],beta.weights[[m]])
            
            LenGamma0 = NULL
            LenGamma1 = NULL
            LenGamma2 = NULL
            LenBeta0 = NULL
            LenBeta1 = NULL
            LenBeta2 = NULL
            
            trueGamma0 = NULL
            trueGamma1 = NULL
            trueGamma2 = NULL
            
            trueBeta0 = NULL
            trueBeta1 = NULL
            trueBeta2 = NULL
            for(i in 1:3){
              fname=paste("nbhcond-",i,j,k,l,m,".rdata", sep="")
              load(fname)
              
              g0.length = NULL
              g1.length = NULL
              g2.length = NULL
            
              b0.length = NULL
              b1.length = NULL
              b2.length = NULL
            
              g0.truth = NULL
              g1.truth = NULL
              g2.truth = NULL
            
              b0.truth = NULL
              b1.truth = NULL
              b2.truth = NULL
            
            for(s in 1:2000){
                g0.length[s] = confint(y.model[[s]])[1,2] - confint(y.model[[s]])[1,1]
                g1.length[s] = confint(y.model[[s]])[2,2] - confint(y.model[[s]])[2,1]
                g2.length[s] = confint(y.model[[s]])[3,2] - confint(y.model[[s]])[3,1]
                b0.length[s] = confint(y.model[[s]])[4,2] - confint(y.model[[s]])[4,1]
                b1.length[s] = confint(y.model[[s]])[5,2] - confint(y.model[[s]])[5,1]
                b2.length[s] = confint(y.model[[s]])[6,2] - confint(y.model[[s]])[6,1]
                g0.truth[s]= confint(y.model[[s]])[1,1] < truth[1] & truth[1] < confint(y.model[[s]])[1,2]
                g1.truth[s] = confint(y.model[[s]])[2,1] < truth[2] & truth[2] < confint(y.model[[s]])[2,2]  
                g2.truth[s] = confint(y.model[[s]])[3,1] < truth[3] & truth[3] < confint(y.model[[s]])[3,2] 
                b0.truth[s] = confint(y.model[[s]])[4,1] < truth[4] & truth[4] < confint(y.model[[s]])[4,2]
               b1.truth[s] = confint(y.model[[s]])[5,1] < truth[5] & truth[5] < confint(y.model[[s]])[5,2]
                b2.truth[s] = confint(y.model[[s]])[6,1] < truth[6] & truth[6] < confint(y.model[[s]])[6,2]
              }
  			LenGamma0[i] = mean(g0.length,na.rm=T)
            LenGamma1[i] = mean(g1.length,na.rm=T)
            LenGamma2[i] = mean(g2.length,na.rm=T)
            LenBeta0[i] = mean(b0.length,na.rm=T)
            LenBeta1[i] = mean(b1.length,na.rm=T)
            LenBeta2[i] = mean(b2.length,na.rm=T)
            
            trueGamma0[i] = sum(g0.truth,na.rm=T)/length(na.omit(g0.truth,na.rm=T))
            trueGamma1[i] = sum(g1.truth,na.rm=T)/length(na.omit(g1.truth,na.rm=T))
            trueGamma2[i] = sum(g2.truth,na.rm=T)/length(na.omit(g2.truth,na.rm=T))
            trueBeta0[i] = sum(b0.truth,na.rm=T)/length(na.omit(b0.truth,na.rm=T))
            trueBeta1[i] = sum(b1.truth,na.rm=T)/length(na.omit(b1.truth,na.rm=T))
            trueBeta2[i] = sum(b2.truth,na.rm=T)/length(na.omit(b2.truth,na.rm=T))
            rm(y.model)
            }
          print(sprintf("Collinearity-%s Phi-%s Gamma-%s Beta-%s",
                        j, k,l,m))
          tableNames = c("N","Beta0","Beta1","Beta2","Gamma0","Gamma1","Gamma2")
          LengthCI = cbind(NOBS,LenBeta0,LenBeta1,LenBeta2,LenGamma0,LenGamma1,LenGamma2)
          colnames(LengthCI) = tableNames
          ContainsTrue = cbind(NOBS,trueBeta0,trueBeta1,trueBeta2,trueGamma0,trueGamma1,trueGamma2)
          colnames(ContainsTrue) = tableNames
          fsumstat=paste("nbhCI-",j,k,l,m,".rdata", sep="")
          save(LengthCI,ContainsTrue,file=fsumstat)
          rm(tableNames,LengthCI,ContainsTrue)
        }
      }
    }
  }
)

##########
## ZINB ##
##########
NOBS = c(100,250,500)
system.time(
  for(j in 1:2){
    for(k in 1:6){
      for(l in 1:2){
        for(m in 1:2){
            truth = NULL
            truth = c(gamma.weights[[l]],beta.weights[[m]])
            
            LenGamma0 = NULL
            LenGamma1 = NULL
            LenGamma2 = NULL
            LenBeta0 = NULL
            LenBeta1 = NULL
            LenBeta2 = NULL
            
            trueGamma0 = NULL
            trueGamma1 = NULL
            trueGamma2 = NULL
            
            trueBeta0 = NULL
            trueBeta1 = NULL
            trueBeta2 = NULL
            for(i in 1:3){
              fname=paste("zinb_cond-",i,j,k,l,m,".rdata", sep="")
              load(fname)
              
              g0.length = NULL
              g1.length = NULL
              g2.length = NULL
            
              b0.length = NULL
              b1.length = NULL
              b2.length = NULL
            
              g0.truth = NULL
              g1.truth = NULL
              g2.truth = NULL
            
              b0.truth = NULL
              b1.truth = NULL
              b2.truth = NULL
            
            for(s in 1:2000){
                g0.length[s] =confint(y.model[[s]])[1,2] - confint(y.model[[s]])[1,1]
                g1.length[s] =confint(y.model[[s]])[2,2] - confint(y.model[[s]])[2,1]
                g2.length[s] =confint(y.model[[s]])[3,2] - confint(y.model[[s]])[3,1]
                b0.length[s] =confint(y.model[[s]])[4,2] - confint(y.model[[s]])[4,1]
                b1.length[s] =confint(y.model[[s]])[5,2] - confint(y.model[[s]])[5,1]
                b2.length[s] =confint(y.model[[s]])[6,2] - confint(y.model[[s]])[6,1]
                g0.truth[s]=confint(y.model[[s]])[1,1] < truth[1] & truth[1] < confint(y.model[[s]])[1,2]
                g1.truth[s] = confint(y.model[[s]])[2,1] < truth[2] & truth[2] < confint(y.model[[s]])[2,2]   
                g2.truth[s] = confint(y.model[[s]])[3,1] < truth[3] & truth[3] < confint(y.model[[s]])[3,2]
                b0.truth[s] = confint(y.model[[s]])[4,1] < truth[4] & truth[4] < confint(y.model[[s]])[4,2]
               b1.truth[s] = confint(y.model[[s]])[5,1] < truth[5] & truth[5] < confint(y.model[[s]])[5,2]
                b2.truth[s] = confint(y.model[[s]])[6,1] < truth[6] & truth[6] < confint(y.model[[s]])[6,2]
              }
            LenGamma0[i] = mean(g0.length,na.rm=T)
            LenGamma1[i] = mean(g1.length,na.rm=T)
            LenGamma2[i] = mean(g2.length,na.rm=T)
            LenBeta0[i] = mean(b0.length,na.rm=T)
            LenBeta1[i] = mean(b1.length,na.rm=T)
            LenBeta2[i] = mean(b2.length,na.rm=T)
            
            trueGamma0[i] = sum(g0.truth,na.rm=T)/length(na.omit(g0.truth,na.rm=T))
            trueGamma1[i] = sum(g1.truth,na.rm=T)/length(na.omit(g1.truth,na.rm=T))
            trueGamma2[i] = sum(g2.truth,na.rm=T)/length(na.omit(g2.truth,na.rm=T))
            trueBeta0[i] = sum(b0.truth,na.rm=T)/length(na.omit(b0.truth,na.rm=T))
            trueBeta1[i] = sum(b1.truth,na.rm=T)/length(na.omit(b1.truth,na.rm=T))
            trueBeta2[i] = sum(b2.truth,na.rm=T)/length(na.omit(b2.truth,na.rm=T))
            rm(y.model)
            }
          print(sprintf("Collinearity-%s Phi-%s Gamma-%s Beta-%s",
                        j, k,l,m))
          tableNames = c("N","Beta0","Beta1","Beta2","Gamma0","Gamma1","Gamma2")
          LengthCI = cbind(NOBS,LenBeta0,LenBeta1,LenBeta2,LenGamma0,LenGamma1,LenGamma2)
          colnames(LengthCI) = tableNames
          ContainsTrue = cbind(NOBS,trueBeta0,trueBeta1,trueBeta2,trueGamma0,trueGamma1,trueGamma2)
          colnames(ContainsTrue) = tableNames
          fsumstat=paste("zinbCI-",j,k,l,m,".rdata", sep="")
          save(LengthCI,ContainsTrue,file=fsumstat)
          rm(tableNames,LengthCI,ContainsTrue)
        }
      }
    }
  }
)


#####################
# ----------------- #
#  STRUCTURAL ZEROS #                       
# ----------------- #
#####################

load("zinbData.rdata")
NOBS = c(100,250,500)
system.time(
  for(j in 1:2){
    for(k in 1:6){
      for(l in 1:2){
        for(m in 1:2){
            StrZero=NULL
          for(i in 1:3){
              fname=paste("zinb_cond-",i,j,k,l,m,".rdata", sep="")
              #zeros=NULL
              str.zero=NULL
              load(fname)
              for(s in 1:2000){
              str.zero[s]=sum(zinb.data[[i]][[j]][[k]][[l]][[m]][[s]][,2] == round(predict(y.model[[s]],type="zero")))/length(zinb.data[[i]][[j]][[k]][[l]][[m]][[s]][,2])
}
              StrZero[i]=mean(str.zero)
              rm(y.model)
          }
          print(sprintf("Collinearity-%s Phi-%s Gamma-%s Beta-%s",
                        j, k,l,m))
          tableNames = c("N","Structural Zeros")
          StrZeroTab = cbind(NOBS,StrZero)
          fsumstat=paste("zinbSZ-",j,k,l,m,".rdata", sep="")
          save(StrZeroTab,file=fsumstat)
          rm(tableNames,StrZeroTab)
        }
      }
    }
  }
)    

##################
# -------------- #
#  TYPE I ERROR  #                       
# -------------- #
##################

##########
## ZINB ##
##########
nsims=2000
NOBS = c(100,250,500)

# No  Gamma 1 #
l = 1
system.time(
  for(j in 1:2){
    for(k in 1:6){
        for(m in 1:2){
	singular = NULL
	p.value = NULL
	Pvalues = list()
	typeI.error = NULL
		for(i in 1:3){
		fname=paste("zinb_cond-",i,j,k,l,m,".rdata", sep="")
			 			load(fname)
          		for(s in 1:nsims){
			p.value[s] =summary(y.model[[s]])$coef$count[2,4]
			}
		p.valueNA = na.omit(p.value)
		singular[i] = length(p.value) - length(p.valueNA)	
		Pvalues[[i]] = p.valueNA
		typeI.error[i] = sum(p.valueNA<.05)/length(p.valueNA)
		rm(y.model)		
		}
		   print(sprintf("Collinearity-%s Phi-%s Gamma-%s Beta-%s",
                        j, k,l,m))
		typeItab = data.frame(NOBS,typeI.error)
          	fsumstat=paste("zinbTypeIG1-",j,k,l,m,".rdata", sep="")
          	save(singular,typeItab,Pvalues,file=fsumstat)
          	rm(typeI.error,Pvalues,typeItab,singular)
      	}
    }
  }
)

# No  Beta 1 #
m = 1
system.time(
  for(j in 1:2){
    for(k in 1:6){
        for(l in 1:2){
	singular = NULL
	p.value = NULL
	Pvalues = list()
	typeI.error = NULL
		for(i in 1:3){
		fname=paste("zinb_cond-",i,j,k,l,m,".rdata", sep="")
			 			load(fname)
          		for(s in 1:nsims){
			p.value[s] =summary(y.model[[s]])$coef$zero[2,4]
			}
		p.valueNA = na.omit(p.value)
		singular[i] = length(p.value) - length(p.valueNA)	
		Pvalues[[i]] = p.valueNA
		typeI.error[i] = sum(p.valueNA<.05)/length(p.valueNA)
		rm(y.model)		
		}
		   print(sprintf("Collinearity-%s Phi-%s Gamma-%s Beta-%s",
                        j, k,l,m))
		typeItab = data.frame(NOBS,typeI.error)
          	fsumstat=paste("zinbTypeIB1-",j,k,l,m,".rdata", sep="")
          	save(singular,typeItab,Pvalues,file=fsumstat)
          	rm(typeI.error,Pvalues,typeItab,singular)
      	}
    }
  }
)


#########
## NBH ##
#########
nsims=2000
NOBS = c(100,250,500)

# No Gamma 1 #
l = 1
system.time(
  for(j in 1:2){
    for(k in 1:6){
        for(m in 1:2){
	singular = NULL
	p.value = NULL
	Pvalues = list()
	typeI.error = NULL
		for(i in 1:3){
		fname=paste("nbhcond-",i,j,k,l,m,".rdata", sep="")
			 			load(fname)
          		for(s in 1:nsims){
			p.value[s] =summary(y.model[[s]])$coef$count[2,4]
			}
		p.valueNA = na.omit(p.value)
		singular[i] = length(p.value) - length(p.valueNA)	
		Pvalues[[i]] = p.valueNA
		typeI.error[i] = sum(p.valueNA<.05)/length(p.valueNA)
		rm(y.model)		
		}
		   print(sprintf("Collinearity-%s Phi-%s Gamma-%s Beta-%s",
                        j, k,l,m))
		typeItab = data.frame(NOBS,typeI.error)
          	fsumstat=paste("nbhTypeIG1-",j,k,l,m,".rdata", sep="")
          	save(singular,typeItab,Pvalues,file=fsumstat)
          	rm(typeI.error,Pvalues,typeItab,singular)
      	}
    }
  }
)


# No Beta 1 #
m = 1
system.time(
  for(j in 1:2){
    for(k in 1:6){
        for(l in 1:2){
	singular = NULL
	p.value = NULL
	Pvalues = list()
	typeI.error = NULL
		for(i in 1:3){
		fname=paste("nbhcond-",i,j,k,l,m,".rdata", sep="")
			 			load(fname)
          		for(s in 1:nsims){
			p.value[s] =summary(y.model[[s]])$coef$zero[2,4]
			}
		p.valueNA = na.omit(p.value)
		singular[i] = length(p.value) - length(p.valueNA)	
		Pvalues[[i]] = p.valueNA
		typeI.error[i] = sum(p.valueNA<.05)/length(p.valueNA)
		rm(y.model)		
		}
		   print(sprintf("Collinearity-%s Phi-%s Gamma-%s Beta-%s",
                        j, k,l,m))
		typeItab = data.frame(NOBS,typeI.error)
          	fsumstat=paste("nbhTypeIB1-",j,k,l,m,".rdata", sep="")
          	save(singular,typeItab,Pvalues,file=fsumstat)
          	rm(typeI.error,Pvalues,typeItab,singular)
      	}
    }
  }
)

#########
# ----- #
#  BIAS #                       
# ----- #
#########

#########
## NBH ##
#########

# -------- #
# E(Y | X) #
# -------- #
 
for(j in 1:2){
    for(k in 1:6){
      for(l in 1:2){
        for(m in 1:2){
          for(i in 1:3){

            fname=paste("nbhcond-",i,j,k,l,m,".rdata",sep="")
            load(fname)

            ### Hurdle Code
            X= model.matrix(y.model[[1]],model = "count")
            Z= model.matrix(y.model[[1]],model = "zero")
            coefc = gamma.weights[[l]]
            coefz = beta.weights[[m]]
            phi <- plogis(Z %*% coefz)
            p0_zero <- log(phi)
            mu = exp(X%*%coefc)
            p0_count = pnbinom(0, size = theta[k], mu = mu, lower.tail = FALSE,log.p=TRUE) 
            
            # Calculate the mean function
            EY <- exp((p0_zero - p0_count) + log(mu))
           
            # For the design matrix
            mm = data.frame(X,Z)
            design = unique(mm)
            
            # Setting up Bias
            BIAS = matrix(ncol=2000,nrow=nrow(design))
            
            for(s in 1:nsims){
              EY.hat = fitted(y.model[[s]])
              bias = (EY.hat - EY)/EY
              mm.bias = data.frame(bias,mm)
              BIAS[,s] = unique(mm.bias)[,1]
            }
            print(sprintf("Collinearity-%s Phi-%s Gamma-%s Beta-%s N-%s",j, k,l,m,i))
            meanBIAS = apply(BIAS,1,mean)
            medianBIAS = apply(BIAS,1,median)
            print("Mean Bias is:")
            print(meanBIAS)
            sdBIAS = apply(BIAS,1,sd)
            fname=paste("EYbiasNBH-",i,j,k,l,m,".rdata", sep="")
            save(meanBIAS,medianBIAS,sdBIAS,design,file=fname)
            rm(y.model,X,Z,mm,design,coefc,coefz,phi,p0_zero,mu,EY,meanBIAS, medianBIAS,sdBIAS)
          }
        }
      }
    }
  }
  )

# ----------- #
#  Pr(Y = 0)  #
# ----------- #
system.time(
  for(j in 1:2){
    for(k in 1:6){
      for(l in 1:2){
        for(m in 1:2){
          for(i in 1:3){

            fname=paste("nbhcond-",i,j,k,l,m,".rdata",sep="")
            load(fname)

            ### Hurdle Code
            Z= model.matrix(y.model[[1]],model = "zero")
            X= model.matrix(y.model[[1]],model = "count")
            coefz = beta.weights[[m]]
            phi <- plogis(Z %*% coefz)
                       
            # Pr(Y = 0)
            P = 1 - phi

            # For the design matrix
            mm = data.frame(X,Z)
            design = unique(mm)
            
            # Setting up Bias
            BIAS = matrix(ncol=2000,nrow=nrow(design))
            Pr.hat = NULL

            for(s in 1:nsims){
            Pr.hat = predict(y.model[[s]],type="prob")[,1]
            bias = (Pr.hat - P)/P
            mm.bias = data.frame(bias,mm)
            BIAS[,s] = unique(mm.bias)[,1]
            }
            print(sprintf("Collinearity-%s Phi-%s Gamma-%s Beta-%s N-%s",j, k,l,m,i))
            meanBIAS = apply(BIAS,1,mean)
            medianBIAS = apply(BIAS,1,median)
            print("Mean Bias is:")
            print(meanBIAS)
            sdBIAS = apply(BIAS,1,sd)
            fname=paste("bias/PrbiasNBH-",i,j,k,l,m,".rdata", sep="")
            save(meanBIAS,medianBIAS,sdBIAS,design,file=fname)
            rm(y.model,X,Z,mm,design,phi,coefz,P,EY,meanBIAS, medianBIAS,sdBIAS)
          }
        }
      }
    }
  }
  )


##########
## ZINB ##
##########

# -------- #
# E(Y | X) #
# -------- #
system.time(
  for(j in 1:2){
    for(k in 1:6){
      for(l in 1:2){
        for(m in 1:2){
          for(i in 1:3){

            fname=paste("zinb_cond-",i,j,k,l,m,".rdata",sep="")
            load(fname)

            X = model.matrix(y.model[[1]],model="count")
            Z = model.matrix(y.model[[1]],model="zero")            
            mm = data.frame(X,Z)
            design = unique(mm)
            
            gamma = gamma.weights[[l]]
            beta = beta.weights[[m]]
            mu = exp(X%*%gamma)
            p = plogis(Z%*%beta)

            # Calculate the mean function
            EY = (1 - p) * mu
            BIAS = matrix(ncol=2000,nrow=nrow(design))
            
            for(s in 1:nsims){
              EY.hat = fitted(y.model[[s]])
              bias = (EY.hat - EY)/EY
              mm.bias = data.frame(bias,mm)
              BIAS[,s] = unique(mm.bias)[,1]
            }
            print(sprintf("Collinearity-%s Phi-%s Gamma-%s Beta-%s N-%s",j, k,l,m,i))
            meanBIAS = apply(BIAS,1,mean)
            print("Mean Bias is:")
            print(meanBIAS)
            sdBIAS = apply(BIAS,1,sd)
            fname=paste("EYbiasZINB-",i,j,k,l,m,".rdata", sep="")
            save(meanBIAS,sdBIAS,design,file=fname)
            rm(y.model,X,Z,mm,design,gamma,beta,mu,p,EY,meanBIAS,sdBIAS)
          }
        }
      }
    }
  }
  )

# ----------- #
#  Pr(Y = 0)  #
# ----------- #
system.time(
  for(j in 1:2){
    for(k in 1:6){
      for(l in 1:2){
        for(m in 1:2){
          for(i in 1:3){

            fname=paste("zinb_cond-",i,j,k,l,m,".rdata",sep="")
            load(fname)
            X = model.matrix(y.model[[1]],model="count")
            Z = model.matrix(y.model[[1]],model="zero")
            mm = data.frame(X,Z)
            design = unique(mm)
            coefc = gamma.weights[[l]]
            coefz = beta.weights[[m]]           
            mu = exp(X%*%coefc)
            phi <- plogis(Z %*% coefz)
            P <- phi + (1-phi) * dnbinom(0, mu = mu, size = theta[k])

            BIAS = matrix(ncol=2000,nrow=nrow(design))
            for(s in 1:nsims){
              P.hat = predict(y.model[[s]],type="prob")[,1]
              bias = (P.hat - P)/P
              mm.bias = data.frame(bias,mm)
              BIAS[,s] = unique(mm.bias)[,1]
            }
            print(sprintf("Collinearity-%s Phi-%s Gamma-%s Beta-%s N-%s",j, k,l,m,i))
            meanBIAS = apply(BIAS,1,mean)
            print("Mean Bias is:")
            print(meanBIAS)
            sdBIAS = apply(BIAS,1,sd)
            fname=paste("PrbiasZINB-",i,j,k,l,m,".rdata", sep="")
            save(meanBIAS,sdBIAS,design,file=fname)
            rm(y.model,X,Z,mm,design,beta,mu,P,P.hat,meanBIAS,medianBIAS,sdBIAS)
          }
        }
      }
    }
  }
  )


############################
# Question #2     ##########
############################

################################
# ---------------------------- #
# FIT THE NBH AND ZINB MODELS  #
# ---------------------------- #
################################


# ---------------------------------------------- # 
# Run the NBH with ZINB data                     #
# -----------------------------------------------#

## Load data
load(file = "zinbData.rdata") 

i=3
system.time(
    for(j in 1:2){
      for(k in 1:6){
        for(l in 1:2){
          for(m in 1:2){
            y.model=list()
            for(s in 1:nsims){
              print(sprintf("Sample Size-%s Collinearity-%s Phi-%s Gamma-%s Beta-%s Sim-%s",
                            NOBS[i], multicol[j], theta[k],gamma.names[l],beta.names[m], s))
              y.model[[s]]=hurdle(zinb.data[[i]][[j]][[k]][[l]][[m]][[s]][,1]~G[[j]][[i]][,3] + G[[j]][[i]][,4] | G[[j]][[i]][,1] + G[[j]][[i]][,2], dist="negbin")
             # y.model[[s]]=try(hurdle(hurdle.data[[i]][[j]][[k]][[l]][[m]][[s]]~G[[j]][[i]][,3] + G[[j]][[i]][,4] | G[[j]][[i]][,1] + G[[j]][[i]][,2], dist="negbin"))
            }
            # i corresponds to sample size
            # j corresponds to multicollinearity
            # k corresponds dispersion
            # l corresponds to Count parameters - Gamma
            # m corresponds to Zero parameters - Beta
            fname=paste("zinbNBHcond-",i,j,k,l,m,".rdata", sep="")
            save(y.model,file=fname)
            rm(y.model)
          }
        }
      }
    }
)

########################
## Summary Statistics ##
########################
N = 500
i = 3
system.time(
for(j in 1:2){
    for(k in 1:6){
      for(l in 1:2){
        for(m in 1:2){
          cint.mean = NULL
          cint.median = NULL
          cint.sd = NULL
          
          cgamma1.mean = NULL
          cgamma1.median = NULL
          cgamma1.sd = NULL

          cgamma2.mean = NULL
          cgamma2.median = NULL
          cgamma2.sd = NULL

          zint.mean = NULL
          zint.median = NULL
          zint.sd = NULL

          zbeta1.mean = NULL
          zbeta1.median = NULL
          zbeta1.sd = NULL

          zbeta2.mean = NULL
          zbeta2.median = NULL
          zbeta2.sd = NULL
          
              fname=paste("zinbNBHcond-",i,j,k,l,m,".rdata", sep="")
              load(fname)
              count.int = NULL
              count.gamma1 = NULL
              count.gamma2 = NULL
              zero.int = NULL
              zero.beta1 = NULL
              zero.beta2 = NULL
              
              for(s in 1:2000){
                count.int[s] = coef(y.model[[s]])[1]
                count.gamma1[s] = coef(y.model[[s]])[2]
                count.gamma2[s] =coef(y.model[[s]])[3]
                zero.int[s] =  coef(y.model[[s]])[4]
                zero.beta1[s] = coef(y.model[[s]])[5]
                zero.beta2[s] = coef(y.model[[s]])[6]
              }
              
              cint.tmp = count.int
              cint.mean = mean(cint.tmp)
              cint.median = median(cint.tmp)
              cint.sd = sd(cint.tmp)

              cgamma1.tmp = count.gamma1
              cgamma1.mean = mean(cgamma1.tmp)
              cgamma1.median   = median(cgamma1.tmp)
              cgamma1.sd   = sd(cgamma1.tmp)

              cgamma2.tmp = as.numeric(count.gamma2)
              cgamma2.mean   = mean(cgamma2.tmp)
              cgamma2.median   = median(cgamma2.tmp)
              cgamma2.sd   = sd(cgamma2.tmp)

              zint.tmp = zero.int
              zint.mean   = mean(zint.tmp)
              zint.median   = median(zint.tmp)
              zint.sd   = sd(zint.tmp)

              zbeta1.tmp = zero.beta1
              zbeta1.mean   = mean(zbeta1.tmp)
              zbeta1.median   = median(zbeta1.tmp)
              zbeta1.sd   = sd(zbeta1.tmp)

              zbeta2.tmp = zero.beta2
              zbeta2.mean   = mean(zbeta2.tmp)
              zbeta2.median   = median(zbeta2.tmp)
              zbeta2.sd   = sd(zbeta2.tmp)
              rm(y.model)
          print(sprintf("Collinearity-%s Phi-%s Gamma-%s Beta-%s",
                        j, k,l,m))
          tableNames = c("N","Beta0","Beta1","Beta2","Gamma0","Gamma1","Gamma2")
          nbhMeans = cbind(N,zint.mean,zbeta1.mean,zbeta2.mean,cint.mean,cgamma1.mean,cgamma2.mean)
          colnames(nbhMeans) = tableNames
          nbhMedians = cbind(N,zint.median,zbeta1.median,zbeta2.median,cint.median,cgamma1.median,cgamma2.median)
          colnames(nbhMedians) = tableNames
          nbhSDs = cbind(N,zint.sd,zbeta1.sd,zbeta2.sd,cint.sd,cgamma1.sd,cgamma2.sd)
          colnames(nbhSDs) = tableNames
          fsumstat=paste("ZINBnbhsumstat-",j,k,l,m,".rdata", sep="")
          save(nbhMeans,nbhMedians,nbhSDs,file=fsumstat)
          rm(nbhMeans,nbhMedians,nbhSDs,tableNames)
        }
      }
    }
  }
)

#######
# MSE #
#######

i = 3
N =500
system.time(
  for(j in 1:2){
    for(k in 1:6){
      for(l in 1:2){
        for(m in 1:2){
          truth = NULL
          truth = c(gamma.weights[[l]],beta.weights[[m]])
          cgamma0.mse = NULL
          cgamma1.mse = NULL
          cgamma2.mse = NULL
          zbeta0.mse = NULL
          zbeta1.mse = NULL
          zbeta2.mse = NULL
            fname=paste("zinbNBHcond-",i,j,k,l,m,".rdata", sep="")
            load(fname)
            count.int = NULL
            count.gamma1 = NULL
            count.gamma2 = NULL
            zero.int = NULL
            zero.beta1 = NULL
            zero.beta2 = NULL
            
            for(s in 1:2000){
              count.int[s] = coef(y.model[[s]])[1]
              count.gamma1[s] = coef(y.model[[s]])[2]
              count.gamma2[s] =coef(y.model[[s]])[3]
              zero.int[s] =  coef(y.model[[s]])[4]
              zero.beta1[s] = coef(y.model[[s]])[5]
              zero.beta2[s] = coef(y.model[[s]])[6]
            }
            
            cgamma0.mse = mean((count.int - truth[1])^2) + var(count.int)
            cgamma1.mse = mean((count.gamma1 - truth[2])^2) + var(count.gamma1)
            cgamma2.mse = mean((count.gamma2 - truth[3])^2) + var(count.gamma2)
            zbeta0.mse = mean((zero.int - truth[4])^2) + var(zero.int)
            zbeta1.mse = mean((zero.beta1 - truth[5])^2) + var(zero.beta1)
            zbeta2.mse = mean((zero.beta2 - truth[6])^2) + var(zero.beta2)
            rm(y.model)
         
          print(sprintf("Collinearity-%s Phi-%s Gamma-%s Beta-%s",
                        j, k,l,m))
          tableNames = c("N","Beta0","Beta1","Beta2","Gamma0","Gamma1","Gamma2")
          mse = cbind(N,zbeta0.mse,zbeta1.mse,zbeta2.mse, cgamma0.mse,cgamma1.mse,cgamma2.mse)
          colnames(mse) = tableNames
          fsumstat=paste("ZINBnbhmse-",j,k,l,m,".rdata", sep="")
          save(mse,file=fsumstat)
          rm(mse,tableNames)
        }
      }
    }
  }
)




##################################
## Confidence Interval Coverage ##
##################################
NOBS = c(500)
system.time(
  for(j in 1:2){
    for(k in 1:6){
      for(l in 1:2){
        for(m in 1:2){
            truth = NULL
            truth = c(gamma.weights[[l]],beta.weights[[m]])
            
            LenGamma0 = NULL
            LenGamma1 = NULL
            LenGamma2 = NULL
            LenBeta0 = NULL
            LenBeta1 = NULL
            LenBeta2 = NULL
            
            trueGamma0 = NULL
            trueGamma1 = NULL
            trueGamma2 = NULL
            
            trueBeta0 = NULL
            trueBeta1 = NULL
            trueBeta2 = NULL
            
              fname=paste("zinbNBHcond-",i,j,k,l,m,".rdata", sep="")
              load(fname)
            
              
              g0.length = NULL
              g1.length = NULL
              g2.length = NULL
            
              b0.length = NULL
              b1.length = NULL
              b2.length = NULL
            
              g0.truth = NULL
              g1.truth = NULL
              g2.truth = NULL
            
              b0.truth = NULL
              b1.truth = NULL
              b2.truth = NULL
            
            for(s in 1:2000){
                g0.length[s] = confint(y.model[[s]])[1,2] - confint(y.model[[s]])[1,1]
                g1.length[s] = confint(y.model[[s]])[2,2] - confint(y.model[[s]])[2,1]
                g2.length[s] = confint(y.model[[s]])[3,2] - confint(y.model[[s]])[3,1]
                b0.length[s] = confint(y.model[[s]])[4,2] - confint(y.model[[s]])[4,1]
                b1.length[s] = confint(y.model[[s]])[5,2] - confint(y.model[[s]])[5,1]
                b2.length[s] = confint(y.model[[s]])[6,2] - confint(y.model[[s]])[6,1]
                g0.truth[s]= confint(y.model[[s]])[1,1] < truth[1] & truth[1] < confint(y.model[[s]])[1,2]
                g1.truth[s] = confint(y.model[[s]])[2,1] < truth[2] & truth[2] < confint(y.model[[s]])[2,2]  
                g2.truth[s] = confint(y.model[[s]])[3,1] < truth[3] & truth[3] < confint(y.model[[s]])[3,2] 
                b0.truth[s] = confint(y.model[[s]])[4,1] < truth[4] & truth[4] < confint(y.model[[s]])[4,2]
               b1.truth[s] = confint(y.model[[s]])[5,1] < truth[5] & truth[5] < confint(y.model[[s]])[5,2]
                b2.truth[s] = confint(y.model[[s]])[6,1] < truth[6] & truth[6] < confint(y.model[[s]])[6,2]
              }
        LenGamma0  = mean(g0.length,na.rm=T)
            LenGamma1  = mean(g1.length,na.rm=T)
            LenGamma2  = mean(g2.length,na.rm=T)
            LenBeta0  = mean(b0.length,na.rm=T)
            LenBeta1  = mean(b1.length,na.rm=T)
            LenBeta2  = mean(b2.length,na.rm=T)
            
            trueGamma0  = sum(g0.truth,na.rm=T)/length(na.omit(g0.truth,na.rm=T))
            trueGamma1  = sum(g1.truth,na.rm=T)/length(na.omit(g1.truth,na.rm=T))
            trueGamma2  = sum(g2.truth,na.rm=T)/length(na.omit(g2.truth,na.rm=T))
            trueBeta0  = sum(b0.truth,na.rm=T)/length(na.omit(b0.truth,na.rm=T))
            trueBeta1  = sum(b1.truth,na.rm=T)/length(na.omit(b1.truth,na.rm=T))
            trueBeta2  = sum(b2.truth,na.rm=T)/length(na.omit(b2.truth,na.rm=T))
            rm(y.model)
          print(sprintf("Collinearity-%s Phi-%s Gamma-%s Beta-%s",
                        j, k,l,m))
          tableNames = c("N","Beta0","Beta1","Beta2","Gamma0","Gamma1","Gamma2")
          LengthCI = cbind(NOBS,LenBeta0,LenBeta1,LenBeta2,LenGamma0,LenGamma1,LenGamma2)
          colnames(LengthCI) = tableNames
          ContainsTrue = cbind(NOBS,trueBeta0,trueBeta1,trueBeta2,trueGamma0,trueGamma1,trueGamma2)
          colnames(ContainsTrue) = tableNames
          fsumstat=paste("ZINBnbhCI-",j,k,l,m,".rdata", sep="")
          save(LengthCI,ContainsTrue,file=fsumstat)
          rm(tableNames,LengthCI,ContainsTrue)
        }
      }
    }
  }
)

##################
## Type I error ##
##################
nsims=2000
NOBS = 500

# No  Gamma 1 #
l = 1
i=3
system.time(
  for(j in 1:2){
    for(k in 1:6){
        for(m in 1:2){
  singular = NULL
  p.value = NULL
  Pvalues = list()
  typeI.error = NULL

    fname=paste("zinbNBHcond-",i,j,k,l,m,".rdata", sep="")
                  load(fname)
              for(s in 1:nsims){
      p.value[s] =summary(y.model[[s]])$coef$count[2,4]
      }
    p.valueNA = na.omit(p.value)
    singular = length(p.value) - length(p.valueNA) 
    Pvalues = p.valueNA
    typeI.error = sum(p.valueNA<.05)/length(p.valueNA)
    rm(y.model)   
       print(sprintf("Collinearity-%s Phi-%s Gamma-%s Beta-%s",
                        j, k,l,m))
    typeItab = data.frame(NOBS,typeI.error)
            fsumstat=paste("NBHzinbTypeI-",j,k,l,m,".rdata", sep="")
            save(singular,typeItab,Pvalues,file=fsumstat)
            rm(typeI.error,Pvalues,typeItab,singular)
        }
    }
  }
)

# No  Beta 1 #
m = 1
system.time(
  for(j in 1:2){
    for(k in 1:6){
        for(l in 1:2){
  singular = NULL
  p.value = NULL
  Pvalues = list()
  typeI.error = NULL
   fname=paste("zinbNBHcond-",i,j,k,l,m,".rdata", sep="")
            load(fname)
              for(s in 1:nsims){
      p.value[s] =summary(y.model[[s]])$coef$zero[2,4]
      }
    p.valueNA = na.omit(p.value)
    singular= length(p.value) - length(p.valueNA) 
    Pvalues = p.valueNA
    typeI.error = sum(p.valueNA<.05)/length(p.valueNA)
    rm(y.model)   
       print(sprintf("Collinearity-%s Phi-%s Gamma-%s Beta-%s",
                        j, k,l,m))
    typeItab = data.frame(NOBS,typeI.error)
            fsumstat=paste("NBHzinbTypeI-",j,k,l,m,".rdata", sep="")
            save(singular,typeItab,Pvalues,file=fsumstat)
            rm(typeI.error,Pvalues,typeItab,singular)
        }
    }
  }
)

#########
# ----- #
#  BIAS #                       
# ----- #
#########

# E(Y | X)
i = 3
system.time(
  for(j in 1:2){
    for(k in 1:6){
      for(l in 1:2){
        for(m in 1:2){

            fname=paste("zinbNBHcond-",i,j,k,l,m,".rdata", sep="")
            load(fname)

            X = model.matrix(y.model[[1]],model="count")
            Z = model.matrix(y.model[[1]],model="zero")            
            mm = data.frame(X,Z)
            design = unique(mm)
            
            gamma = gamma.weights[[l]]
            beta = beta.weights[[m]]
            mu = exp(X%*%gamma)
            p = plogis(Z%*%beta)

            # Calculate the mean function
            EY = (1 - p) * mu
            BIAS = matrix(ncol=2000,nrow=nrow(design))
            
            for(s in 1:nsims){
              EY.hat = fitted(y.model[[s]])
              bias = (EY.hat - EY)/EY
              mm.bias = data.frame(bias,mm)
              BIAS[,s] = unique(mm.bias)[,1]
            }
            print(sprintf("Collinearity-%s Phi-%s Gamma-%s Beta-%s N-%s",j, k,l,m,i))
            meanBIAS = apply(BIAS,1,mean)
            print("Mean Bias is:")
            print(meanBIAS)
            sdBIAS = apply(BIAS,1,sd)
            fname=paste("EYbiasZINBnbh-",i,j,k,l,m,".rdata", sep="")
            save(meanBIAS,sdBIAS,design,file=fname)
            rm(y.model,X,Z,mm,design,gamma,beta,mu,p,EY,meanBIAS,sdBIAS)
        }
      }
    }
  }
  )


## Pr(Y = 0)
system.time(
  for(j in 1:2){
    for(k in 1:6){
      for(l in 1:2){
        for(m in 1:2){
            fname=paste("zinbNBHcond-",i,j,k,l,m,".rdata", sep="")

            load(fname)
            X = model.matrix(y.model[[1]],model="count")
            Z = model.matrix(y.model[[1]],model="zero")
            mm = data.frame(X,Z)
            design = unique(mm)
            coefc = gamma.weights[[l]]
            coefz = beta.weights[[m]]           
            mu = exp(X%*%coefc)
            phi <- plogis(Z %*% coefz)
            P <- phi + (1-phi) * dnbinom(0, mu = mu, size = theta[k])

            BIAS = matrix(ncol=2000,nrow=nrow(design))
            for(s in 1:nsims){
              P.hat = predict(y.model[[s]],type="prob")[,1]
              bias = (P.hat - P)/P
              mm.bias = data.frame(bias,mm)
              BIAS[,s] = unique(mm.bias)[,1]
            }
            print(sprintf("Collinearity-%s Phi-%s Gamma-%s Beta-%s N-%s",j, k,l,m,i))
            meanBIAS = apply(BIAS,1,mean)
            print("Mean Bias is:")
            print(meanBIAS)
            sdBIAS = apply(BIAS,1,sd)
            fname=paste("PrbiasZINBnbh-",i,j,k,l,m,".rdata", sep="")
            save(meanBIAS,sdBIAS,design,file=fname)
            rm(y.model,X,Z,mm,design,beta,mu,P,P.hat,meanBIAS,medianBIAS,sdBIAS)
        }
      }
    }
  }
  )


# ---------------------------------------------- # 
# Run the ZINB with NBH  data                    #
# -----------------------------------------------#

## Load data
load(file = "hurdleData.rdata") 

## Run the ZINB model and save them in zinb.models
i=3
system.time(
  for(j in 1:2){
    for(k in 1:6){
      for(l in 1:2){
        for(m in 1:2){
            y.model=list()
          for(s in 1:nsims){
            print(sprintf("Sample Size-%s Collinearity-%s Phi-%s Gamma-%s Beta-%s Sim-%s",
                          NOBS[i], multicol[j], theta[k],gamma.names[l],beta.names[m], s))
          y.model[[s]]=zeroinfl(hurdle.data[[i]][[j]][[k]][[l]][[m]][[s]]~G[[j]][[i]][,3] + G[[j]][[i]][,4] | G[[j]][[i]][,1] + G[[j]][[i]][,2], dist="negbin")
          }
            # i corresponds to sample size
            # j corresponds to multicollinearity
            # k corresponds dispersion
            # l corresponds to Count parameters - Gamma
            # m corresponds to Zero parameters - Beta
          fname=paste("NBHzinbcond-",i,j,k,l,m,".rdata", sep="")
          save(y.model,file=fname)
          rm(y.model)
        }
      }
    }
  }
)
########################
## Summary Statistics ##
########################
N = 500
i = 3
system.time(
for(j in 1:2){
    for(k in 1:6){
      for(l in 1:2){
        for(m in 1:2){
          cint.mean = NULL
          cint.median = NULL
          cint.sd = NULL
          
          cgamma1.mean = NULL
          cgamma1.median = NULL
          cgamma1.sd = NULL

          cgamma2.mean = NULL
          cgamma2.median = NULL
          cgamma2.sd = NULL

          zint.mean = NULL
          zint.median = NULL
          zint.sd = NULL

          zbeta1.mean = NULL
          zbeta1.median = NULL
          zbeta1.sd = NULL

          zbeta2.mean = NULL
          zbeta2.median = NULL
          zbeta2.sd = NULL
          
              fname=paste("NBHzinbcond-",i,j,k,l,m,".rdata", sep="")
              load(fname)
              count.int = NULL
              count.gamma1 = NULL
              count.gamma2 = NULL
              zero.int = NULL
              zero.beta1 = NULL
              zero.beta2 = NULL
              
              for(s in 1:2000){
                count.int[s] = coef(y.model[[s]])[1]
                count.gamma1[s] = coef(y.model[[s]])[2]
                count.gamma2[s] =coef(y.model[[s]])[3]
                zero.int[s] =  coef(y.model[[s]])[4]
                zero.beta1[s] = coef(y.model[[s]])[5]
                zero.beta2[s] = coef(y.model[[s]])[6]
              }
              
              cint.tmp = count.int
              cint.mean = mean(cint.tmp)
              cint.median = median(cint.tmp)
              cint.sd = sd(cint.tmp)

              cgamma1.tmp = count.gamma1
              cgamma1.mean = mean(cgamma1.tmp)
              cgamma1.median   = median(cgamma1.tmp)
              cgamma1.sd   = sd(cgamma1.tmp)

              cgamma2.tmp = as.numeric(count.gamma2)
              cgamma2.mean   = mean(cgamma2.tmp)
              cgamma2.median   = median(cgamma2.tmp)
              cgamma2.sd   = sd(cgamma2.tmp)

              zint.tmp = zero.int
              zint.mean   = mean(zint.tmp)
              zint.median   = median(zint.tmp)
              zint.sd   = sd(zint.tmp)

              zbeta1.tmp = zero.beta1
              zbeta1.mean   = mean(zbeta1.tmp)
              zbeta1.median   = median(zbeta1.tmp)
              zbeta1.sd   = sd(zbeta1.tmp)

              zbeta2.tmp = zero.beta2
              zbeta2.mean   = mean(zbeta2.tmp)
              zbeta2.median   = median(zbeta2.tmp)
              zbeta2.sd   = sd(zbeta2.tmp)
              rm(y.model)
          print(sprintf("Collinearity-%s Phi-%s Gamma-%s Beta-%s",
                        j, k,l,m))
          tableNames = c("N","Beta0","Beta1","Beta2","Gamma0","Gamma1","Gamma2")
          nbhMeans = cbind(N,zint.mean,zbeta1.mean,zbeta2.mean,cint.mean,cgamma1.mean,cgamma2.mean)
          colnames(nbhMeans) = tableNames
          nbhMedians = cbind(N,zint.median,zbeta1.median,zbeta2.median,cint.median,cgamma1.median,cgamma2.median)
          colnames(nbhMedians) = tableNames
          nbhSDs = cbind(N,zint.sd,zbeta1.sd,zbeta2.sd,cint.sd,cgamma1.sd,cgamma2.sd)
          colnames(nbhSDs) = tableNames
          fsumstat=paste("NBHzinbsumstat-",j,k,l,m,".rdata", sep="")
          save(nbhMeans,nbhMedians,nbhSDs,file=fsumstat)
          rm(nbhMeans,nbhMedians,nbhSDs,tableNames)
        }
      }
    }
  }
)

#######
# MSE #
#######

i = 3
N =500
system.time(
  for(j in 1:2){
    for(k in 1:6){
      for(l in 1:2){
        for(m in 1:2){
          truth = NULL
          truth = c(gamma.weights[[l]],beta.weights[[m]])
          cgamma0.mse = NULL
          cgamma1.mse = NULL
          cgamma2.mse = NULL
          zbeta0.mse = NULL
          zbeta1.mse = NULL
          zbeta2.mse = NULL
          fname=paste("NBHzinbcond-",i,j,k,l,m,".rdata", sep="")
          load(fname)
          count.int = NULL
          count.gamma1 = NULL
          count.gamma2 = NULL
          zero.int = NULL
          zero.beta1 = NULL
          zero.beta2 = NULL
          
          for(s in 1:2000){
            count.int[s] = coef(y.model[[s]])[1]
            count.gamma1[s] = coef(y.model[[s]])[2]
            count.gamma2[s] =coef(y.model[[s]])[3]
            zero.int[s] =  coef(y.model[[s]])[4]
            zero.beta1[s] = coef(y.model[[s]])[5]
            zero.beta2[s] = coef(y.model[[s]])[6]
          }
          
          cgamma0.mse = mean((count.int - truth[1])^2) + var(count.int)
          cgamma1.mse = mean((count.gamma1 - truth[2])^2) + var(count.gamma1)
          cgamma2.mse = mean((count.gamma2 - truth[3])^2) + var(count.gamma2)
          zbeta0.mse = mean((zero.int - truth[4])^2) + var(zero.int)
          zbeta1.mse = mean((zero.beta1 - truth[5])^2) + var(zero.beta1)
          zbeta2.mse = mean((zero.beta2 - truth[6])^2) + var(zero.beta2)
          rm(y.model)
          
          print(sprintf("Collinearity-%s Phi-%s Gamma-%s Beta-%s",
                        j, k,l,m))
          tableNames = c("N","Beta0","Beta1","Beta2","Gamma0","Gamma1","Gamma2")
          mse = cbind(N,zbeta0.mse,zbeta1.mse,zbeta2.mse, cgamma0.mse,cgamma1.mse,cgamma2.mse)
          colnames(mse) = tableNames
          fsumstat=paste("NBHzinbmse-",j,k,l,m,".rdata", sep="")
          save(mse,file=fsumstat)
          rm(mse,tableNames)
        }
      }
    }
  }
)

##################################
## Confidence Interval Coverage ##
###################################
library(car)
NOBS = c(500)
system.time(
  for(j in 1:2){
    for(k in 1:6){
      for(l in 1:2){
        for(m in 1:2){
            truth = NULL
            truth = c(gamma.weights[[l]],beta.weights[[m]])
            
            LenGamma0 = NULL
            LenGamma1 = NULL
            LenGamma2 = NULL
            LenBeta0 = NULL
            LenBeta1 = NULL
            LenBeta2 = NULL
            
            trueGamma0 = NULL
            trueGamma1 = NULL
            trueGamma2 = NULL
            
            trueBeta0 = NULL
            trueBeta1 = NULL
            trueBeta2 = NULL
            
              fname=paste("NBHzinbcond-",i,j,k,l,m,".rdata", sep="")
              load(fname)
            
              
              g0.length = NULL
              g1.length = NULL
              g2.length = NULL
            
              b0.length = NULL
              b1.length = NULL
              b2.length = NULL
            
              g0.truth = NULL
              g1.truth = NULL
              g2.truth = NULL
            
              b0.truth = NULL
              b1.truth = NULL
              b2.truth = NULL
            
            for(s in 1:2000){
                g0.length[s] = confint(y.model[[s]])[1,2] - confint(y.model[[s]])[1,1]
                g1.length[s] = confint(y.model[[s]])[2,2] - confint(y.model[[s]])[2,1]
                g2.length[s] = confint(y.model[[s]])[3,2] - confint(y.model[[s]])[3,1]
                b0.length[s] = confint(y.model[[s]])[4,2] - confint(y.model[[s]])[4,1]
                b1.length[s] = confint(y.model[[s]])[5,2] - confint(y.model[[s]])[5,1]
                b2.length[s] = confint(y.model[[s]])[6,2] - confint(y.model[[s]])[6,1]
                g0.truth[s]= confint(y.model[[s]])[1,1] < truth[1] & truth[1] < confint(y.model[[s]])[1,2]
                g1.truth[s] = confint(y.model[[s]])[2,1] < truth[2] & truth[2] < confint(y.model[[s]])[2,2]  
                g2.truth[s] = confint(y.model[[s]])[3,1] < truth[3] & truth[3] < confint(y.model[[s]])[3,2] 
                b0.truth[s] = confint(y.model[[s]])[4,1] < truth[4] & truth[4] < confint(y.model[[s]])[4,2]
               b1.truth[s] = confint(y.model[[s]])[5,1] < truth[5] & truth[5] < confint(y.model[[s]])[5,2]
                b2.truth[s] = confint(y.model[[s]])[6,1] < truth[6] & truth[6] < confint(y.model[[s]])[6,2]
              }
        LenGamma0  = mean(g0.length,na.rm=T)
            LenGamma1  = mean(g1.length,na.rm=T)
            LenGamma2  = mean(g2.length,na.rm=T)
            LenBeta0  = mean(b0.length,na.rm=T)
            LenBeta1  = mean(b1.length,na.rm=T)
            LenBeta2  = mean(b2.length,na.rm=T)
            
            trueGamma0  = sum(g0.truth,na.rm=T)/length(na.omit(g0.truth,na.rm=T))
            trueGamma1  = sum(g1.truth,na.rm=T)/length(na.omit(g1.truth,na.rm=T))
            trueGamma2  = sum(g2.truth,na.rm=T)/length(na.omit(g2.truth,na.rm=T))
            trueBeta0  = sum(b0.truth,na.rm=T)/length(na.omit(b0.truth,na.rm=T))
            trueBeta1  = sum(b1.truth,na.rm=T)/length(na.omit(b1.truth,na.rm=T))
            trueBeta2  = sum(b2.truth,na.rm=T)/length(na.omit(b2.truth,na.rm=T))
            rm(y.model)
          print(sprintf("Collinearity-%s Phi-%s Gamma-%s Beta-%s",
                        j, k,l,m))
          tableNames = c("N","Beta0","Beta1","Beta2","Gamma0","Gamma1","Gamma2")
          LengthCI = cbind(NOBS,LenBeta0,LenBeta1,LenBeta2,LenGamma0,LenGamma1,LenGamma2)
          colnames(LengthCI) = tableNames
          ContainsTrue = cbind(NOBS,trueBeta0,trueBeta1,trueBeta2,trueGamma0,trueGamma1,trueGamma2)
          colnames(ContainsTrue) = tableNames
          fsumstat=paste("NBHzinbCI-",j,k,l,m,".rdata", sep="")
          save(LengthCI,ContainsTrue,file=fsumstat)
          rm(tableNames,LengthCI,ContainsTrue)
        }
      }
    }
  }
)

nsims=2000
NOBS = 500
# No  Gamma 1 #
l = 1
i=3
system.time(
  for(j in 1:2){
    for(k in 1:6){
        for(m in 1:2){
  singular = NULL
  p.value = NULL
  Pvalues = list()
  typeI.error = NULL

    fname=paste("NBHzinbcond-",i,j,k,l,m,".rdata", sep="")
                  load(fname)
              for(s in 1:nsims){
      p.value[s] =summary(y.model[[s]])$coef$count[2,4]
      }
    p.valueNA = na.omit(p.value)
    singular = length(p.value) - length(p.valueNA) 
    Pvalues = p.valueNA
    typeI.error = sum(p.valueNA<.05)/length(p.valueNA)
    rm(y.model)   
       print(sprintf("Collinearity-%s Phi-%s Gamma-%s Beta-%s",
                        j, k,l,m))
    typeItab = data.frame(NOBS,typeI.error)
            fsumstat=paste("NBHzinbTypeI-",j,k,l,m,".rdata", sep="")
            save(singular,typeItab,Pvalues,file=fsumstat)
            rm(typeI.error,Pvalues,typeItab,singular)
        }
    }
  }
)

# No  Beta 1 #
m = 1
system.time(
  for(j in 1:2){
    for(k in 1:6){
        for(l in 1:2){
  singular = NULL
  p.value = NULL
  Pvalues = list()
  typeI.error = NULL
     fname=paste("NBHzinbcond-",i,j,k,l,m,".rdata", sep="")
            load(fname)
              for(s in 1:nsims){
      p.value[s] =summary(y.model[[s]])$coef$zero[2,4]
      }
    p.valueNA = na.omit(p.value)
    singular= length(p.value) - length(p.valueNA) 
    Pvalues = p.valueNA
    typeI.error = sum(p.valueNA<.05)/length(p.valueNA)
    rm(y.model)   
       print(sprintf("Collinearity-%s Phi-%s Gamma-%s Beta-%s",
                        j, k,l,m))
    typeItab = data.frame(NOBS,typeI.error)
            fsumstat=paste("NBHzinbTypeI-",j,k,l,m,".rdata", sep="")
            save(singular,typeItab,Pvalues,file=fsumstat)
            rm(typeI.error,Pvalues,typeItab,singular)
        }
    }
  }
)

# -------- #
# E(Y | X) #
# -------- #

system.time(
  for(j in 1:2){
    for(k in 1:6){
      for(l in 1:2){
        for(m in 1:2){
            fname=paste("NBHzinbcond-",i,j,k,l,m,".rdata", sep="")
            load(fname)

            ### Hurdle Code
            X= model.matrix(y.model[[1]],model = "count")
            Z= model.matrix(y.model[[1]],model = "zero")
            coefc = gamma.weights[[l]]
            coefz = beta.weights[[m]]
            phi <- plogis(Z %*% coefz)
            p0_zero <- log(phi)
            mu = exp(X%*%coefc)
            p0_count = pnbinom(0, size = theta[k], mu = mu, lower.tail = FALSE,log.p=TRUE) 
            
            # Calculate the mean function
            EY <- exp((p0_zero - p0_count) + log(mu))
           
            # For the design matrix
            mm = data.frame(X,Z)
            design = unique(mm)
            
            # Setting up Bias
            BIAS = matrix(ncol=2000,nrow=nrow(design))
            
            for(s in 1:nsims){
              EY.hat = fitted(y.model[[s]])
              bias = (EY.hat - EY)/EY
              mm.bias = data.frame(bias,mm)
              BIAS[,s] = unique(mm.bias)[,1]
            }
            print(sprintf("Collinearity-%s Phi-%s Gamma-%s Beta-%s N-%s",j, k,l,m,i))
            meanBIAS = apply(BIAS,1,mean)
            medianBIAS = apply(BIAS,1,median)
            print("Mean Bias is:")
            print(meanBIAS)
            sdBIAS = apply(BIAS,1,sd)
            fname=paste("EYbiasNBHzinb-",i,j,k,l,m,".rdata", sep="")
            save(meanBIAS,medianBIAS,sdBIAS,design,file=fname)
            rm(y.model,X,Z,mm,design,coefc,coefz,phi,p0_zero,mu,EY,meanBIAS, medianBIAS,sdBIAS)

        }
      }
    }
  }
  )

# ----------- #
#  Pr(Y = 0)  #
# ----------- #
system.time(
  for(j in 1:2){
    for(k in 1:6){
      for(l in 1:2){
        for(m in 1:2){

            fname=paste("NBHzinbcond-",i,j,k,l,m,".rdata", sep="")
            load(fname)

            ### Hurdle Code
            Z= model.matrix(y.model[[1]],model = "zero")
            X= model.matrix(y.model[[1]],model = "count")
            coefz = beta.weights[[m]]
            phi <- plogis(Z %*% coefz)
                       
            # Pr(Y = 0)
            P = 1 - phi

            # For the design matrix
            mm = data.frame(X,Z)
            design = unique(mm)
            
            # Setting up Bias
            BIAS = matrix(ncol=2000,nrow=nrow(design))
            Pr.hat = NULL

            for(s in 1:nsims){
            Pr.hat = predict(y.model[[s]],type="prob")[,1]
            bias = (Pr.hat - P)/P
            mm.bias = data.frame(bias,mm)
            BIAS[,s] = unique(mm.bias)[,1]
            }
            print(sprintf("Collinearity-%s Phi-%s Gamma-%s Beta-%s N-%s",j, k,l,m,i))
            meanBIAS = apply(BIAS,1,mean)
            medianBIAS = apply(BIAS,1,median)
            print("Mean Bias is:")
            print(meanBIAS)
            sdBIAS = apply(BIAS,1,sd)
            fname=paste("PrbiasNBHzinb-",i,j,k,l,m,".rdata", sep="")
            save(meanBIAS,medianBIAS,sdBIAS,design,file=fname)
            rm(y.model,X,Z,mm,design,phi,coefz,P,EY,meanBIAS, medianBIAS,sdBIAS)
          
        }
      }
    }
  }
  )