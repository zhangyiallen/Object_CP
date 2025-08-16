#same simulation as in section 3.2 of dubey 2023, 2 dim dist valued cp test
command.args = commandArgs(trailingOnly = TRUE) 
dt = as.numeric(command.args[1])##0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8
ite = as.numeric(command.args[2])##1...40
# library(MASS)
# # library(fda)
# library(expm)
# library(LaplacesDemon)
# library(pracma)
# library(e1071)

# dt=0.2
# ite=2

n=200
rho=0
# 
# ite=3
# rho=0.5

library(mvtnorm)
# install.packages("beepr")
#library(beepr)


# Generate MAR(1) model
MAR_MTS_A <- function(n, reptime, rho, p, sigma_cross,d1){
  ts_sim <- list()
  burnin <- 50
  for(index in 1:reptime){
    epsilon <- rmvnorm(n+burnin, mean=rep(0,p), sigma=sigma_cross%*%sigma_cross)
    inter <- matrix(0, n+burnin, p)
    for (j in 1:(n+burnin-1)){
      inter[j+1,] <- epsilon[j+1,]+rho*inter[j,]
    }
    ts_sim[[index]] <- cbind(t(inter[(burnin+1):(n/2+burnin),]),apply(t(inter[(burnin+n/2+1):(n+burnin),]),MARGIN = 2,function(X) X+c(d1,rep(0,p-1))))
  }
  return(ts_sim)
}




# Zdat= MAR_MTS_A(n,reptime,rho,p,sigma_cross,0)
# 
# qq1 = Zdat[[1]] ##2x300 matrix


# ShaoZhang(2010) for mean test, 1 dim
SN_test <- function(ts, k){
  n <- length(ts)
  mean1 <- mean(ts[1:k])
  mean2 <- mean(ts[(k+1):n])
  inter1 <- cumsum(ts[1:k])-(1:k)*mean1
  inter2 <- cumsum(ts[n:(k+1)])-(1:(n-k))*mean2
  M1 <- sum(inter1^2)/n^2
  M2 <- sum(inter2^2)/n^2
  test_SN <- sqrt(n)*(((n-k)*k/n^2)*(mean1-mean2))/sqrt(M1+M2) 
  return(test_SN)
}

###################generate bivariate normal cdf with range (-10,10), 2 dims are independent
generate_Gaussian = function(n=300, pp,Zdat){
  tempdat = list()

  # 
  grid = seq(from=-10,to=10,length.out=pp+1)
  cdf1 = t(sapply(Zdat[1,],function(x) pnorm(grid,mean = x,sd=0.5)))
  cdf2 = t(sapply(Zdat[2,],function(x) pnorm(grid,mean = x,sd=0.5))) 
  cdf=cbind(cdf1,cdf2)
  X=apply(cdf,1, function(x) as.vector(outer(x[1:(pp+1)],x[(pp+2):(2*pp+2)])))
  return(X)
}



##### JiangZhuShao2022 ##### 

compute_Tn = function(r,a,b, n, X, X.cumsum){
  nr = floor(r*n)
  na = floor(a*n)
  nb = floor(b*n)
  p = ncol(X)
  X0 = matrix(X[(na+1):nr,],nr - na,p)
  X1 = matrix(X[(nr+1):nb,],nb - nr,p)
  
  if (na == 0){
    mu0 = X.cumsum[nr,] / nr
  } else {
    mu0 = (X.cumsum[nr,] - X.cumsum[na,]) / (nr - na)
  }
  mu1 = (X.cumsum[nb,] - X.cumsum[nr,]) / (nb - nr)
  V0 = sum((X0 - cbind(rep(1,nr-na)) %*% rbind(mu0))^2) / (nr - na)
  V0c = sum((X0 - cbind(rep(1,nr-na)) %*% rbind(mu1))^2) / (nr - na)
  V1 = sum((X1 - cbind(rep(1,nb-nr)) %*% rbind(mu1))^2) / (nb - nr)
  V1c = sum((X1 - cbind(rep(1,nb-nr)) %*% rbind(mu0))^2) / (nb - nr)
  return((r-a)*(b-r)/(b-a) * c((V0-V1),(V0c-V0+V1c-V1)))
}

Jiang_test = function(X, eta1=0.15, eta2=0.05){
  
  res_Jiang = matrix(0,nrow=1,ncol=10)
  X.cumsum = apply(X,2,cumsum)
  n = nrow(X)
  grid = floor(n*eta1)
  trim = floor(n*eta2)
  Test_res = NULL
  for (k in (grid+1):(n-grid)){
    DS_res = compute_Tn(k/n, 0, 1, n, X, X.cumsum)
    Dn_num = (DS_res[1])^2
    Sn_num = (DS_res[2])^2
    
    Left = sapply(((trim+1):(k-trim))/n, compute_Tn, 0,k/n,n,X,X.cumsum)
    Right = sapply(((k+trim+1):(n-trim))/n, compute_Tn, k/n,1,n,X,X.cumsum)
    
    Dn_denorm = sum(Left[1,]^2)+sum(Right[1,]^2)
    Sn_denorm = sum(Left[2,]^2)+sum(Right[2,]^2)
    
    Tn = c(n*Dn_num/Dn_denorm, n*(Dn_num+Sn_num)/(Dn_denorm+Sn_denorm))
    Test_res = cbind(Test_res, Tn)
  }
  res_Jiang[1:2] = apply(Test_res, 1, max)
  return(res_Jiang)
}



##### DubeyMuller2019 ##### 

compute_Dubey = function(X,n, p, Ic=0.15){
  start_p = floor(Ic*n)
  end_p = n - floor(Ic*n)
  
  X.cumsum = apply(X, 2, cumsum)
  mu_hat = X.cumsum[n,] / n
  d_hat = apply((X - cbind(rep(1,n)) %*% rbind(mu_hat))^2, 1, sum) # d^2 with Euclidean metric
  sigma2_hat = mean(d_hat^2) - (mean(d_hat))^2
  
  Tn = numeric(length(start_p:end_p))
  for (i in start_p:end_p) {
    u = i/n
    X0 = matrix(X[1:i,],i,p)
    X1 = matrix(X[(i+1):n,],n-i,p)
    mu0 = X.cumsum[i,] / i
    mu1 = (X.cumsum[n,] - X.cumsum[i,]) / (n-i)
    V0 = mean(rowSums( (X0 - cbind(rep(1,i)) %*% rbind(mu0))^2 )) # Frechet variance of 1:n1
    V1 = mean(rowSums( (X1 - cbind(rep(1,n-i)) %*% rbind(mu1))^2 )) # Frechet variance of (n1+1):n
    V0c = mean(rowSums( (X0 - cbind(rep(1,i)) %*% rbind(mu1))^2 )) # contaminatedFrechet variance of 1:n1
    V1c = mean(rowSums( (X1 - cbind(rep(1,n-i)) %*% rbind(mu0))^2 )) # contaminatedFrechet variance of (n1+1):n
    
    Tn[i] = u*(1-u)/sigma2_hat * ((V0-V1)^2 + (V0c-V0+V1c-V1)^2) * n
    
  }
  return(max(Tn))
}
#########compute statistic from dubey 2023

inte_ecdf = function(x,ii){
  pp1 = sort(x)
  lefx = ecdf(x[1:ii])
  rightx = ecdf(x[(ii+1):n])
  dif1 = (lefx(pp1)-rightx(pp1))^2
  delt1 = pp1[2:n]-pp1[1:(n-1)]
  tempr = sum(dif1[1:(n-1)]*delt1)
  
  return(tempr)
}


inte_ecdf2 = function(x,sx,ii,n){
  
  tempr = numeric(n)
  for (kk in 1:n){
    pp1=sx[kk,]
    lefx = ecdf(x[kk,1:ii])
    rightx = ecdf(x[kk,(ii+1):n])
    dif1 = (lefx(pp1)-rightx(pp1))^2
    delt1 = pp1[2:n]-pp1[1:(n-1)]
    tempr[kk] = sum(dif1[1:(n-1)]*delt1)

    
  }
  
  return(mean(tempr))
}




compute_Dubey23 = function(disst,sortdisst,n, Ic=0.15){
  start_p = floor(Ic*n)
  end_p = n - floor(Ic*n)
  tempdub =numeric(length(start_p:end_p))
  for (i in start_p:end_p) {
    
    # temp_i = mean(apply(disst,1,inte_ecdf,i))
    temp_i = inte_ecdf2(disst,sortdisst,i,n)
    tempdub[i]=i*(n-i)*temp_i/n 
    
  }
  return(max(tempdub))
}
###############
Dubey_test = function(X,disst, n,  m.ratio=0.5, Ic=0.15, alpha=0.05, B=200){
  p=ncol(X)
  res_Dubey= compute_Dubey(X,n,p,Ic)
  sortdisst = t(apply(disst, 1, sort))
  res_Dubey23= compute_Dubey23(disst,sortdisst,n,Ic)  
  Tb = numeric(B)
  Tb23 = numeric(B)
  m = floor(n*m.ratio)
  for (b in 1:B){
    X_boot = matrix(X[sample(1:n,m,replace=TRUE),],m,p)
    res_boot = compute_Dubey(X_boot,m,p,Ic)
    Tb[b] = res_boot

    distB = as.matrix(dist(X[sample(1:n,n,replace=F),],method = "euclidean"))
    sortdistB = t(apply(distB, 1, sort))
    res_boot23 =compute_Dubey23(distB,sortdistB,n,Ic)  
    Tb23[b] = res_boot23
  }
  
  return(c(mean(Tb>res_Dubey),mean(Tb23>res_Dubey23)))
}

# sortdistm = t(apply(distm, 1, sort))
# 
# start.time <- Sys.time()
# # mean(apply(distm,1,inte_ecdf,10))
# compute_Dubey23(distm,sortdistm,n)
# # compute_Dubey(t(seqi),n,961)
# 
# # for (i in 1:n) {
# #   sort(distm[i,])
# #   
# # }
# end.time <- Sys.time()
# time.taken <- end.time - start.time
# time.taken



#################SSSN2
reptime <- 1000
# tau=1/3
b=0.15

sigma_cross = 0.5*sqrt(1-rho^2)*diag(c(1,1))
p=2


resultrawpl = c()
tempsumpl=rep(0,25)
#####Shao ZHU jIANG
result1 = c()
temp1=rep(0,25)
result2 = c()
temp2=rep(0,25)
####DM
resultDM = c()
tempDM=rep(0,25)
resultDM23 = c()
tempDM23=rep(0,25)



sum_squared_vals <- function(x) (sum(abs(x)^2))^(1/2)
set.seed(123)
dataZ = MAR_MTS_A(n,reptime,rho,p,sigma_cross,dt)

for (i in ((ite-1)*25+1):(ite*25)) {
  Zi_dat = dataZ[[i]]
  seqi= generate_Gaussian(n, pp=30,Zi_dat)
  seqihead = seqi[,1:floor(n*b)]
  seqimiddle = seqi[,(floor(n*b)+1):(n-floor(n*b))]
  seqitail = seqi[,(n-floor(n*b)+1):n]

  #####prepare the 1 dimensional data: hat Z_t

  Zt = rep(0,dim(seqimiddle)[2])
  for (t in 1:dim(seqimiddle)[2]){
    headZ = seqihead -  seqimiddle[,t]
    tailZ =  seqitail -  seqimiddle[,t]
    Zt[t] = sum(apply(tailZ, 2, sum_squared_vals))-sum(apply(headZ, 2, sum_squared_vals))
  }


  TNpl = rep(0,dim(seqimiddle)[2])
  for (k in 1:(dim(seqimiddle)[2]-1)) {
    TNpl[k] = SN_test(Zt,k)
  }

  tempsumpl[i-(ite-1)*25]=max(TNpl)

  # ####CALCULATE JIANG ZHU SHAO
  res11 = Jiang_test(t(seqi))
  temp1[i-(ite-1)*25] = res11[1,1]
  temp2[i-(ite-1)*25] =res11[1,2]
  ######CALCULATE dm
  distm = as.matrix(dist(t(seqi),method = "euclidean"))
  res110 = Dubey_test(X=t(seqi),disst = distm,n=n)
  res11 = res110[1]
  res23 = res110[2]
  tempDM[i-(ite-1)*25] = res11
  tempDM23[i-(ite-1)*25] = res23
  
  
}
resultrawpl = rbind(resultrawpl,c(n,rho,dt,tempsumpl))
result1 = rbind(result1,c(n,rho,dt,temp1))
result2 = rbind(result2,c(n,rho,dt,temp2))
resultDM = rbind(resultDM,c(n,rho,dt,tempDM))
resultDM23 = rbind(resultDM23,c(n,rho,dt,tempDM23))
write.csv(resultrawpl,paste0("./result/2dim_mod1_dist_SSSN2_power_SSSN_n",n,"_rho",rho,"_dt",dt,"_ite",ite,"_v1.csv"),row.names = F)
write.csv(result1,paste0("./result/2dim_mod1_dist_JiangZhuShao_SN1_power_n",n,"_rho",rho,"_dt",dt,"_ite",ite,"_v1.csv"),row.names = F)
write.csv(result2,paste0("./result/2dim_mod1_dist_JiangZhuShao_SN2_power_n",n,"_rho",rho,"_dt",dt,"_ite",ite,"_v1.csv"),row.names = F)
write.csv(resultDM,paste0("./result/2dim_mod1_dist_DM_power_n",n,"_rho",rho,"_dt",dt,"_ite",ite,"_v1.csv"),row.names = F)
write.csv(resultDM23,paste0("./result/2dim_mod1_dist_DM23_power_n",n,"_rho",rho,"_dt",dt,"_ite",ite,"_v1.csv"),row.names = F)





# 
# mean(tempsumpl>5.39)
# 
# 
# 





###95% quantile is 5.39 for SSSN


#####################################
###############plot the power 
##########################################

reptime=1000
d_sets = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8)

n=200
rho=0




data1=c()
data2=c()
data3=c()
data4=c()
data5=c()
for (dt in d_sets) {
  tempdata1 =c(n,rho,dt)
  tempdata2 =c(n,rho,dt)
  tempdata3 =c(n,rho,dt)
  tempdata4 =c(n,rho,dt)
  tempdata5 =c(n,rho,dt)
  for (ite in 1:40) {


  temp=read.csv(file=paste0("./result/2dim_dist/2dim_mod1_dist_SSSN2_power_SSSN_n",n,"_rho",rho,"_dt",dt,"_ite",ite,"_v1.csv"),header = F)
  temp=as.numeric(temp[2:length(temp[,1]),4:28])
  tempdata1=c(tempdata1,temp)

  temp=read.csv(file=paste0("./result/2dim_dist/2dim_mod1_dist_JiangZhuShao_SN1_power_n",n,"_rho",rho,"_dt",dt,"_ite",ite,"_v1.csv"),header = F)
  temp=as.numeric(temp[2:length(temp[,1]),4:28])
  tempdata2=c(tempdata2,temp)

  temp=read.csv(file=paste0("./result/2dim_dist/2dim_mod1_dist_JiangZhuShao_SN2_power_n",n,"_rho",rho,"_dt",dt,"_ite",ite,"_v1.csv"),header = F)
  temp=as.numeric(temp[2:length(temp[,1]),4:28])
  tempdata3=c(tempdata3,temp)

  temp=read.csv(file=paste0("./result/2dim_dist/2dim_mod1_dist_DM_power_n",n,"_rho",rho,"_dt",dt,"_ite",ite,"_v1.csv"),header = F)
  temp=as.numeric(temp[2:length(temp[,1]),4:28])
  tempdata4=c(tempdata4,temp)

  temp=read.csv(file=paste0("./result/2dim_dist/2dim_mod1_dist_DM23_power_n",n,"_rho",rho,"_dt",dt,"_ite",ite,"_v1.csv"),header = F)
  temp=as.numeric(temp[2:length(temp[,1]),4:28])
  tempdata5=c(tempdata5,temp)
  }
  data1 = rbind(data1,tempdata1)
  data2 = rbind(data2,tempdata2)
  data3 = rbind(data3,tempdata3)
  data4 = rbind(data4,tempdata4)
  data5 = rbind(data5,tempdata5)
}



# #####my power critic : rho=0.4
# critSN1 = 5.716788 ##SSSN
# critSN2 = 54.81250##sn1
# critSN3 = 28.26732#SN2
# critSN4 = 0.00000#DM
# critSN5 = 0.000 ###DM23
# #####my power critic : rho=0
critSN1 = 5.215607 ##SSSN
critSN2 = 46.74681##sn1
critSN3 = 29.38168#SN2
critSN4 = 0.08475#DM
critSN5 = 0.045 ###DM23
#####my power critic : rho=-0.4
# critSN1 = 5.122785 ##SSSN
# critSN2 = 47.46359##sn1
# critSN3 = 39.53046#SN2
# critSN4 = 0.04000#DM
# critSN5 = 0.045 ###DM23


Epower1 = c()
Epower2 = c()
Epower3 = c()
Epower4 = c()
Epower5 = c()

for (i in 1:length(data1[,1])) {
  r95n = sum(as.numeric(data1[i,4:length(data1[i,])])>critSN1)/reptime
  Epower1=rbind(Epower1,c(as.numeric(data1[i,3]),r95n))

  r95n = sum(as.numeric(data2[i,4:length(data2[i,])])>critSN2)/reptime
  Epower2=rbind(Epower2,c(as.numeric(data2[i,3]),r95n))

  r95n = sum(as.numeric(data3[i,4:length(data3[i,])])>critSN3)/reptime
  Epower3=rbind(Epower3,c(as.numeric(data3[i,3]),r95n))

  r95n = sum(as.numeric(data4[i,4:length(data4[i,])])<critSN4)/reptime
  Epower4=rbind(Epower4,c(as.numeric(data4[i,3]),r95n))

  r95n = sum(as.numeric(data5[i,4:length(data5[i,])])<critSN5)/reptime
  Epower5=rbind(Epower5,c(as.numeric(data5[i,3]),r95n))
}

Epower1= rbind(c(0,0.05),Epower1)
Epower2= rbind(c(0,0.05),Epower2)
Epower3= rbind(c(0,0.05),Epower3)
Epower4= rbind(c(0,0.05),Epower4)
Epower5= rbind(c(0,0.05),Epower5)



# 
par(mar = c(4.2, 4.2, 0.2, 0.2))
plot(Epower1[,1],Epower1[,2],type = "b",pch=1,col=1,lty=1,xlab = expression(delta),ylab = "Size Adjusted Power",ylim = c(0,1),cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,lwd = 1.9,cex=1.1)
lines(Epower1[,1],Epower2[,2],type="b",pch=2,col=1,lwd = 1.9,cex=1.1,lty=2)
lines(Epower1[,1],Epower3[,2],type="b",pch=3,col=1,lwd = 1.9,cex=1.1,lty=3)
lines(Epower1[,1],Epower4[,2],type="b",pch=7,col=1,lwd = 1.9,cex=1.1,lty=4)
lines(Epower1[,1],Epower5[,2],type="b",pch=5,col=1,lwd = 1.9,cex=1.1,lty=5)
legend( legend = c(expression(paste("SS","-","SN")),
                   expression(SN[1]),
                   expression(SN[2]),
                   expression(paste("DM")),
                   expression(paste("DZ"))),
        pch=c(1,2,3,7,5),col=c(1,1,1,1,1),lty=c(1,2,3,4,5),bty = "n",x=0.55,y=0.59,cex=1.2,pt.cex = 1.2,y.intersp = 0.6,lwd=1.9,seg.len=4)



#####plot for rho=0.4, without dz, dm
# par(mar = c(4.2, 4.2, 0.2, 0.2))
# plot(Epower1[,1],Epower1[,2],type = "b",pch=1,col=1,lty=1,xlab = expression(delta),ylab = "Size Adjusted Power",ylim = c(0,1),cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,lwd = 1.9,cex=1.1)
# lines(Epower1[,1],Epower2[,2],type="b",pch=2,col=1,lwd = 1.9,cex=1.1,lty=2)
# lines(Epower1[,1],Epower3[,2],type="b",pch=3,col=1,lwd = 1.9,cex=1.1,lty=3)
# # lines(Epower1[,1],Epower4[,2],type="b",pch=7,col=1,lwd = 1.7,cex=1.1,lty=4)
# # lines(Epower1[,1],Epower5[,2],type="b",pch=5,col=1,lwd = 1.7,cex=1.1,lty=5)
# legend( legend = c(expression(paste("SS","-","SN")),
#                    expression(SN[1]),
#                    expression(SN[2])),
#         pch=c(1,2,3),col=c(1,1,1),lty=c(1,2,3),bty = "n",x=0.55,y=0.59,cex=1.2,pt.cex = 1.2,y.intersp = 0.6,lwd=1.9,seg.len=4)
# 
# 




