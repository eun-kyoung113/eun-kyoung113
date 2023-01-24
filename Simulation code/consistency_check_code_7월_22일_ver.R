## consistency check of IPTW ATT Variance estimator - 7/22 update ver##
### 1. consistency assumption check for mu1 and mu0 except unobservable "U" variable###

library(boot)
library(tableone)
library(survey)
library(Hmisc)
library(tidyverse)
library(Matching)
library(clubSandwich)
library(geex)
library(geepack)

##########################################################################
## weight generation function ##
weight_make<-function(var_treat,var_cov,estimate,data){
  result<-list()
  mydata<-data[c(var_treat,var_cov)]
  
  myformula<-as.formula(sprintf("%s~.",var_treat))
  fit<-glm(formula=myformula,data=mydata,family='binomial')
  ps<-fit$fitted.values
  
  result$model<-fit
  result$ps<-ps
  #ps<-predict(psmodel,type='response')
  
  if(estimate=="ATE"){
    weight<-ifelse(data[,var_treat]==1,1/ps,1/(1-ps))
  }
  else if(estimate=='ATT'){
    weight<-ifelse(data[,var_treat]==1,1,ps/(1-ps))
  }
  else{
    weight<-ifelse(data[,var_treat]==1,(1-ps)/ps,1)
  }
  result$untrimmed<-weight
  result$trimmed<-ifelse(weight>quantile(weight,prob=0.99),quantile(weight,prob=0.99),weight)
  return(result)}


################################################################
###################### true mu_0, mu_1 #########################
#### Want to calculate : P(B=1|E=1) & E[C|E=1] ####

B_sample<-rep(0,1000000)
C_sample<-rep(0,1000000)
#U_sample<-rep(0,1000000)
E_sample<-rep(0,1000000)
Y_sample<-rep(0,1000000)


effect<-c(log(1.5),log(2),log(2))

# 10000000 replication
delta0<--2
delta_b<-0.01
delta_c<-0.01
sample<-replicate(1000000,expr={
  B<-rbinom(1,1,prob=0.5)
  C<-rnorm(1,0,1)
  #U<-runif(1)
  p_z<-inv.logit(delta0+delta_b*B+delta_c*C)
  E<-rbinom(1,1,prob=p_z)
  #c(B,C,U,E)
  c(B,C,E)
})

B_sample<-sample[1,]
C_sample<-sample[2,]
#U_sample<-sample[3,]
E_sample<-sample[3,]
X_sample<-cbind(B_sample, C_sample)

# Y random sampling - continuous
Y0<-as.matrix(cbind(X_sample,0))%*%effect+rnorm(nrow(X_sample),0,1)
Y1<-as.matrix(cbind(X_sample,1))%*%effect+rnorm(nrow(X_sample),0,1)
Y_sample<-Y0*(E_sample==0)+Y1*(E_sample==1)

df<-data.frame(E_sample, B_sample,C_sample, Y_sample,Y0,Y1)
colnames(df)<-c("E","B","C","Y_observed","Y0","Y1")

############################ true value ###################################################
## 1. true mu1, true mu0 ##
#true_mu0<-mean(df$Y0[df$E==1])
true_mu0<-0.2111845

#true_mu1<-mean(df$Y1[df$E==1])
true_mu1<-0.9025169

############### Consistency assumption check #############
mean(df$Y_observed[df$E==1]) # 0.9025169
mean(df$Y_observed[df$E==0])

log(1.5)*mean(df$B[df$E==1])+log(2)*mean(df$C[df$E==1]) # 0.2104158
########## mu0 is not same #######


############ Consistency assumption check about mu0 estimator with increasing exposure ratio###########
################ exposure ratio : 0.76 & # of obs = 1000000 #####################
B_sample<-rep(0,1000000)
C_sample<-rep(0,1000000)
#U_sample<-rep(0,100000)
E_sample<-rep(0,1000000)
Y_sample<-rep(0,1000000)

effect<-c(log(1.5),log(2),log(2))

# 1000000 replication
delta0<-1
delta_b<-0.5
delta_c<-0.5
sample<-replicate(1000000,expr={
  B<-rbinom(1,1,prob=0.5)
  C<-rnorm(1,0,1)
  #U<-runif(1)
  p_z<-inv.logit(delta0+delta_b*B+delta_c*C)
  E<-rbinom(1,1,prob=p_z)
  #c(B,C,U,E)
  c(B,C,E)
})

B_sample<-sample[1,]
C_sample<-sample[2,]
#U_sample<-sample[3,]
E_sample<-sample[3,]
X_sample<-cbind(B_sample, C_sample)

# Y random sampling - continuous
Y0<-as.matrix(cbind(X_sample,0))%*%effect+rnorm(nrow(X_sample),0,1)
Y1<-as.matrix(cbind(X_sample,1))%*%effect+rnorm(nrow(X_sample),0,1)
Y_sample<-Y0*(E_sample==0)+Y1*(E_sample==1)

df<-data.frame(E_sample, B_sample,C_sample, Y_sample,Y0,Y1)
colnames(df)<-c("E","B","C","Y_observed","Y0","Y1")

############################ true value ###################################################
## 1. true mu1, true mu0 ##
true_mu0<-mean(df$Y0[df$E==1]) # 0.2935713

#true_mu1<-mean(df$Y1[df$E==1])
#true_mu1<-0.9025169

############### Consistency assumption check #############
#mean(df$Y_observed[df$E==1]) # 0.9025169
log(1.5)*mean(df$B[df$E==1])+log(2)*mean(df$C[df$E==1]) # 0.2917725
mean(df$Y_observed[df$E==0])

#0.2935713-0.2917725
#[1] 0.0017988

##################################################################################
################ exposure ratio : 0.76 & # of obs = 10000000 #####################
B_sample<-rep(0,10000000)
C_sample<-rep(0,10000000)
#U_sample<-rep(0,100000)
E_sample<-rep(0,10000000)
Y_sample<-rep(0,10000000)

effect<-c(log(1.5),log(2),log(2))

# 10000000 replication
delta0<-1
delta_b<-0.5
delta_c<-0.5
sample<-replicate(10000000,expr={
  B<-rbinom(1,1,prob=0.5)
  C<-rnorm(1,0,1)
  #U<-runif(1)
  p_z<-inv.logit(delta0+delta_b*B+delta_c*C)
  E<-rbinom(1,1,prob=p_z)
  #c(B,C,U,E)
  c(B,C,E)
})

B_sample<-sample[1,]
C_sample<-sample[2,]
#U_sample<-sample[3,]
E_sample<-sample[3,]
X_sample<-cbind(B_sample, C_sample)

# Y random sampling - continuous
Y0<-as.matrix(cbind(X_sample,0))%*%effect+rnorm(nrow(X_sample),0,1)
Y1<-as.matrix(cbind(X_sample,1))%*%effect+rnorm(nrow(X_sample),0,1)
Y_sample<-Y0*(E_sample==0)+Y1*(E_sample==1)

df<-data.frame(E_sample, B_sample,C_sample, Y_sample,Y0,Y1)
colnames(df)<-c("E","B","C","Y_observed","Y0","Y1")

############################ true value ###################################################
## 1. true mu1, true mu0 ##
true_mu0<-mean(df$Y0[df$E==1]) #  0.2919322
true_mu1<-mean(df$Y1[df$E==1]) # 0.9851008
#true_mu1<-0.9025169

############### Consistency assumption check #############
#mean(df$Y_observed[df$E==1]) # 0.9025169
log(1.5)*mean(df$B[df$E==1])+log(2)*mean(df$C[df$E==1]) # 0.2917931

#log(1.5)*mean(df$B[df$E==1])+log(2)*mean(df$C[df$E==1])-true_mu0
#[1] -0.0001391367
#############################################################################







##################### Consistency assumption check of variance estimator for IPTW ATT########################
#################### with increasing exposure ratio ##################
#################### Version 1 : #of obs = 5000  x 100th data ##########################
B_sample<-matrix(0,nrow=5000,ncol=100)
C_sample<-matrix(0,nrow=5000,ncol=100)
U_sample<-matrix(0,nrow=5000,ncol=100)
E_sample<-matrix(0,nrow=5000,ncol=100)
Y_sample<-matrix(0,nrow=5000,ncol=100)

effect<-c(log(1.5),log(2),log(2))

# 100th replication data generating
for(repl in 1:100){
  delta0<-1
  delta_b<-0.5
  delta_c<-0.5
  set.seed(123*repl)
  sample<-replicate(5000,expr={
    B<-rbinom(1,1,prob=0.5)
    C<-rnorm(1,0,1)
    #U<-runif(1)
    p_z<-inv.logit(delta0+delta_b*B+delta_c*C)
    E<-rbinom(1,1,prob=p_z)
    c(B,C,E)
  })
  B_sample[,repl]<-sample[1,]
  C_sample[,repl]<-sample[2,]
  #U_sample[,repl]<-sample[3,]
  E_sample[,repl]<-sample[3,]
  X_sample<-cbind(B_sample[,repl],C_sample[,repl],E_sample[,repl])
  
  # Y random sampling - continuous
  Y_sample[,repl]<-as.matrix(X_sample)%*%effect+rnorm(nrow(X_sample),0,1)
  
  # Y_sample random sampling - binary                    
  #prob<-inv.logit(as.matrix(X_sample)%*%effect)
  #Y_sample_binary<-rbinom(n=nrow(X_sample), size=1, prob=prob)
}

## Data store ##
write.csv(B_sample,file="B_sample_5000_except_U.csv",row.names=FALSE)
write.csv(C_sample,file="C_sample_5000_except_U.csv",row.names=FALSE)
#write.csv(U_sample,file="U_sample.csv",row.names=FALSE)
write.csv(E_sample,file="E_sample_5000_except_U.csv",row.names=FALSE)
write.csv(Y_sample,file="Y_sample_5000_except_U.csv",row.names=FALSE)

## Data import ##
B_sample<-read.csv("B_sample_5000_except_U.csv",header=TRUE)
C_sample<-read.csv("C_sample_5000_except_U.csv",header=TRUE)
#U_sample<-read.csv("U_sample.csv",header=TRUE)
E_sample<-read.csv("E_sample_5000_except_U.csv",header=TRUE)
Y_sample<-read.csv("Y_sample_5000_except_U.csv",header=TRUE)

mu0_est_ver04_1<-rep(0,100)
mu1_est_ver04_1<-rep(0,100)

mu0_est_ver05_1<-rep(0,100)
mu1_est_ver05_1<-rep(0,100)

mu0_est_ver08_1<-rep(0,100)
mu1_est_ver08_1<-rep(0,100)

for(i in 1:100){
  E<-E_sample[,i]
  B<-B_sample[,i]
  #U<-U_sample[,1]
  C<-C_sample[,i]
  Y<-Y_sample[,i]
  data<-data.frame("E"=E,"B"=B,"C"=C,"Y"=Y)
  cov<-c("B","C")
  cov_type<-c("binary","continuous")
  weight_result<-weight_make("E",cov,estimate='ATT',data=data)
  
  # plus weight to column #
  data$weight_untrimmed<-weight_result$untrimmed 
  
  ATT_untrimmed<-lm(Y~E,data=data,weight=weight_result$untrimmed)
  
  mu0_est_ver04_1[i]<-log(1.5)*mean(data$B[data$E==1]) + log(2)*mean(data$C[data$E==1])
  mu1_est_ver04_1[i]<-log(1.5)*mean(data$B[data$E==1]) + log(2)*mean(data$C[data$E==1]) + log(2)
  
  mu0_est_ver05_1[i]<-mean(ATT_untrimmed$fitted.values[data$E==0])
  mu1_est_ver05_1[i]<-mean(ATT_untrimmed$fitted.values[data$E==1])
  
  mu0_est_ver08_1[i]<-sum(data$weight_untrimmed*(1-data$E)*data$Y)/sum(data$weight_untrimmed*(1-data$E))
  mu1_est_ver08_1[i]<-sum(data$weight_untrimmed*data$E*data$Y)/sum(data$weight_untrimmed*data$E)
}


############## Version 2 : #of obs : 20000 x 100th data ##############
B_sample<-matrix(0,nrow=20000,ncol=100)
C_sample<-matrix(0,nrow=20000,ncol=100)
#U_sample<-matrix(0,nrow=20000,ncol=100)
E_sample<-matrix(0,nrow=20000,ncol=100)
Y_sample<-matrix(0,nrow=20000,ncol=100)

effect<-c(log(1.5),log(2),log(2))

# 100th replication data generating
for(repl in 1:100){
  delta0<-1
  delta_b<-0.5
  delta_c<-0.5
  set.seed(123*repl)
  sample<-replicate(10000,expr={
    B<-rbinom(1,1,prob=0.5)
    C<-rnorm(1,0,1)
    #U<-runif(1)
    p_z<-inv.logit(delta0+delta_b*B+delta_c*C)
    E<-rbinom(1,1,prob=p_z)
    c(B,C,E)
  })
  B_sample[,repl]<-sample[1,]
  C_sample[,repl]<-sample[2,]
  #U_sample[,repl]<-sample[3,]
  E_sample[,repl]<-sample[3,]
  X_sample<-cbind(B_sample[,repl],C_sample[,repl],E_sample[,repl])
  
  # Y random sampling - continuous
  Y_sample[,repl]<-as.matrix(X_sample)%*%effect+rnorm(nrow(X_sample),0,1)
  
  # Y_sample random sampling - binary                    
  #prob<-inv.logit(as.matrix(X_sample)%*%effect)
  #Y_sample_binary<-rbinom(n=nrow(X_sample), size=1, prob=prob)
}

## Data store ##
write.csv(B_sample,file="B_sample_20000_except_U.csv",row.names=FALSE)
write.csv(C_sample,file="C_sample_20000_except_U.csv",row.names=FALSE)
#write.csv(U_sample,file="U_sample.csv",row.names=FALSE)
write.csv(E_sample,file="E_sample_20000_except_U.csv",row.names=FALSE)
write.csv(Y_sample,file="Y_sample_20000_except_U.csv",row.names=FALSE)

B_sample<-read.csv("B_sample_20000_except_U.csv",header=TRUE)
C_sample<-read.csv("C_sample_20000_except_U.csv",header=TRUE)
#U_sample<-read.csv("U_sample.csv",header=TRUE)
E_sample<-read.csv("E_sample_20000_except_U.csv",header=TRUE)
Y_sample<-read.csv("Y_sample_20000_except_U.csv",header=TRUE)

mu0_est_ver04_2<-rep(0,100)
mu1_est_ver04_2<-rep(0,100)

mu0_est_ver05_2<-rep(0,100)
mu1_est_ver05_2<-rep(0,100)

mu0_est_ver08_2<-rep(0,100)
mu1_est_ver08_2<-rep(0,100)

for(i in 1:100){
  E<-E_sample[,i]
  B<-B_sample[,i]
  #U<-U_sample[,1]
  C<-C_sample[,i]
  Y<-Y_sample[,i]
  data<-data.frame("E"=E,"B"=B,"C"=C,"Y"=Y)
  cov<-c("B","C")
  cov_type<-c("binary","continuous")
  weight_result<-weight_make("E",cov,estimate='ATT',data=data)
  
  # plus weight to column #
  data$weight_untrimmed<-weight_result$untrimmed 
  
  ATT_untrimmed<-lm(Y~E,data=data,weight=weight_result$untrimmed)
  
  mu0_est_ver04_2[i]<-log(1.5)*mean(data$B[data$E==1]) + log(2)*mean(data$C[data$E==1])
  mu1_est_ver04_2[i]<-log(1.5)*mean(data$B[data$E==1]) + log(2)*mean(data$C[data$E==1]) + log(2)
  
  mu0_est_ver05_2[i]<-mean(ATT_untrimmed$fitted.values[data$E==0])
  mu1_est_ver05_2[i]<-mean(ATT_untrimmed$fitted.values[data$E==1])
  
  mu0_est_ver08_2[i]<-sum(data$weight_untrimmed*(1-data$E)*data$Y)/sum(data$weight_untrimmed*(1-data$E))
  mu1_est_ver08_2[i]<-sum(data$weight_untrimmed*data$E*data$Y)/sum(data$weight_untrimmed*data$E)
}


############## Version 3 : #of obs : 80000 x 100th data ##############
# sample store
B_sample<-matrix(0,nrow=80000,ncol=100)
C_sample<-matrix(0,nrow=80000,ncol=100)
#U_sample<-matrix(0,nrow=80000,ncol=100)
E_sample<-matrix(0,nrow=80000,ncol=100)
Y_sample<-matrix(0,nrow=80000,ncol=100)

effect<-c(log(1.5),log(2),log(2))

# 100th replication data generating
for(repl in 1:100){
  delta0<-1
  delta_b<-0.5
  delta_c<-0.5
  set.seed(123*repl)
  sample<-replicate(80000,expr={
    B<-rbinom(1,1,prob=0.5)
    C<-rnorm(1,0,1)
    #U<-runif(1)
    p_z<-inv.logit(delta0+delta_b*B+delta_c*C)
    E<-rbinom(1,1,prob=p_z)
    c(B,C,E)
  })
  B_sample[,repl]<-sample[1,]
  C_sample[,repl]<-sample[2,]
  #U_sample[,repl]<-sample[3,]
  E_sample[,repl]<-sample[3,]
  X_sample<-cbind(B_sample[,repl],C_sample[,repl],E_sample[,repl])
  
  # Y random sampling - continuous
  Y_sample[,repl]<-as.matrix(X_sample)%*%effect+rnorm(nrow(X_sample),0,1)
  
  # Y_sample random sampling - binary                    
  #prob<-inv.logit(as.matrix(X_sample)%*%effect)
  #Y_sample_binary<-rbinom(n=nrow(X_sample), size=1, prob=prob)
}

## Data store ##
write.csv(B_sample,file="B_sample_80000_except_U.csv",row.names=FALSE)
write.csv(C_sample,file="C_sample_80000_except_U.csv",row.names=FALSE)
#write.csv(U_sample,file="U_sample.csv",row.names=FALSE)
write.csv(E_sample,file="E_sample_80000_except_U.csv",row.names=FALSE)
write.csv(Y_sample,file="Y_sample_80000_except_U.csv",row.names=FALSE)

## Data import ##
B_sample<-read.csv("B_sample_80000_except_U.csv",header=TRUE)
C_sample<-read.csv("C_sample_80000_except_U.csv",header=TRUE)
#U_sample<-read.csv("U_sample.csv",header=TRUE)
E_sample<-read.csv("E_sample_80000_except_U.csv",header=TRUE)
Y_sample<-read.csv("Y_sample_80000_except_U.csv",header=TRUE)

####################################################
mu0_est_ver04_3<-rep(0,100)
mu1_est_ver04_3<-rep(0,100)

mu0_est_ver05_3<-rep(0,100)
mu1_est_ver05_3<-rep(0,100)

mu0_est_ver08_3<-rep(0,100)
mu1_est_ver08_3<-rep(0,100)

for(i in 1:100){
  E<-E_sample[,i]
  B<-B_sample[,i]
  #U<-U_sample[,1]
  C<-C_sample[,i]
  Y<-Y_sample[,i]
  data<-data.frame("E"=E,"B"=B,"C"=C,"Y"=Y)
  cov<-c("B","C")
  cov_type<-c("binary","continuous")
  weight_result<-weight_make("E",cov,estimate='ATT',data=data)
  
  # plus weight to column #
  data$weight_untrimmed<-weight_result$untrimmed 
  
  ATT_untrimmed<-lm(Y~E,data=data,weight=weight_result$untrimmed)
  
  mu0_est_ver04_3[i]<-log(1.5)*mean(data$B[data$E==1]) + log(2)*mean(data$C[data$E==1])
  mu1_est_ver04_3[i]<-log(1.5)*mean(data$B[data$E==1]) + log(2)*mean(data$C[data$E==1]) + log(2)
  
  mu0_est_ver05_3[i]<-mean(ATT_untrimmed$fitted.values[data$E==0])
  mu1_est_ver05_3[i]<-mean(ATT_untrimmed$fitted.values[data$E==1])
  
  mu0_est_ver08_3[i]<-sum(data$weight_untrimmed*(1-data$E)*data$Y)/sum(data$weight_untrimmed*(1-data$E))
  mu1_est_ver08_3[i]<-sum(data$weight_untrimmed*data$E*data$Y)/sum(data$weight_untrimmed*data$E)
}

############## Version 4 : #of obs : 320000 x 100th data ##############
# sample store
B_sample<-matrix(0,nrow=320000,ncol=100)
C_sample<-matrix(0,nrow=320000,ncol=100)
#U_sample<-matrix(0,nrow=320000,ncol=100)
E_sample<-matrix(0,nrow=320000,ncol=100)
Y_sample<-matrix(0,nrow=320000,ncol=100)

effect<-c(log(1.5),log(2),log(2))

# 100th replication data generating
for(repl in 1:100){
  delta0<--2
  delta_b<-0.01
  delta_c<-0.01
  set.seed(123*repl)
  sample<-replicate(320000,expr={
    B<-rbinom(1,1,prob=0.5)
    C<-rnorm(1,0,1)
    #U<-runif(1)
    p_z<-inv.logit(delta0+delta_b*B+delta_c*C)
    E<-rbinom(1,1,prob=p_z)
    c(B,C,E)
  })
  B_sample[,repl]<-sample[1,]
  C_sample[,repl]<-sample[2,]
  #U_sample[,repl]<-sample[3,]
  E_sample[,repl]<-sample[3,]
  X_sample<-cbind(B_sample[,repl],C_sample[,repl],E_sample[,repl])
  
  # Y random sampling - continuous
  Y_sample[,repl]<-as.matrix(X_sample)%*%effect+rnorm(nrow(X_sample),0,1)
  
  # Y_sample random sampling - binary                    
  #prob<-inv.logit(as.matrix(X_sample)%*%effect)
  #Y_sample_binary<-rbinom(n=nrow(X_sample), size=1, prob=prob)
}

## Data store ##
write.csv(B_sample,file="B_sample_320000_except_U.csv",row.names=FALSE)
write.csv(C_sample,file="C_sample_320000_except_U.csv",row.names=FALSE)
#write.csv(U_sample,file="U_sample.csv",row.names=FALSE)
write.csv(E_sample,file="E_sample_320000_except_U.csv",row.names=FALSE)
write.csv(Y_sample,file="Y_sample_320000_except_U.csv",row.names=FALSE)

## Data import ##
B_sample<-read.csv("B_sample_320000_except_U.csv",header=TRUE)
C_sample<-read.csv("C_sample_320000_except_U.csv",header=TRUE)
#U_sample<-read.csv("U_sample.csv",header=TRUE)
E_sample<-read.csv("E_sample_320000_except_U.csv",header=TRUE)
Y_sample<-read.csv("Y_sample_320000_except_U.csv",header=TRUE)

####################################################
mu0_est_ver04_4<-rep(0,100)
mu1_est_ver04_4<-rep(0,100)

mu0_est_ver05_4<-rep(0,100)
mu1_est_ver05_4<-rep(0,100)

mu0_est_ver08_4<-rep(0,100)
mu1_est_ver08_4<-rep(0,100)

for(i in 1:100){
  E<-E_sample[,i]
  B<-B_sample[,i]
  #U<-U_sample[,1]
  C<-C_sample[,i]
  Y<-Y_sample[,i]
  data<-data.frame("E"=E,"B"=B,"C"=C,"Y"=Y)
  cov<-c("B","C")
  cov_type<-c("binary","continuous")
  weight_result<-weight_make("E",cov,estimate='ATT',data=data)
  
  # plus weight to column #
  data$weight_untrimmed<-weight_result$untrimmed 
  
  ATT_untrimmed<-lm(Y~E,data=data,weight=weight_result$untrimmed)
  
  mu0_est_ver04_4[i]<-log(1.5)*mean(data$B[data$E==1]) + log(2)*mean(data$C[data$E==1])
  mu1_est_ver04_4[i]<-log(1.5)*mean(data$B[data$E==1]) + log(2)*mean(data$C[data$E==1]) + log(2)
  
  mu0_est_ver05_4[i]<-mean(ATT_untrimmed$fitted.values[data$E==0])
  mu1_est_ver05_4[i]<-mean(ATT_untrimmed$fitted.values[data$E==1])
  
  mu0_est_ver08_4[i]<-sum(data$weight_untrimmed*(1-data$E)*data$Y)/sum(data$weight_untrimmed*(1-data$E))
  mu1_est_ver08_4[i]<-sum(data$weight_untrimmed*data$E*data$Y)/sum(data$weight_untrimmed*data$E)
}

###### result table ########
result1<-matrix(0,nrow=4,ncol=4)
rownames(result1)<-c("#of obs = 5000", "#of obs = 20000", "#of obs = 80000", "#of obs = 320000")
colnames(result1)<-c("bias of mu_0 estimator ver1","variance of mu_0 estimator ver1", 
                     "bias of mu_1 estimator ver1","variance of mu_1 estimator ver1")
result1[1,]<-c(mean(mu0_est_ver04_1)-true_mu0, var(mu0_est_ver04_1), mean(mu1_est_ver04_1)-true_mu1, var(mu1_est_ver04_1))
result1[2,]<-c(mean(mu0_est_ver04_2)-true_mu0, var(mu0_est_ver04_2), mean(mu1_est_ver04_2)-true_mu1, var(mu1_est_ver04_2))
result1[3,]<-c(mean(mu0_est_ver04_3)-true_mu0, var(mu0_est_ver04_3), mean(mu1_est_ver04_3)-true_mu1, var(mu1_est_ver04_3))
result1[4,]<-c(mean(mu0_est_ver04_4)-true_mu0, var(mu0_est_ver04_4), mean(mu1_est_ver04_4)-true_mu1, var(mu1_est_ver04_4))
result1


result2<-matrix(0,nrow=4,ncol=4)
rownames(result2)<-c("#of obs = 5000", "#of obs = 20000", "#of obs = 80000", "#of obs = 320000")
colnames(result2)<-c("bias of mu_0 estimator ver2","variance of mu_0 estimator ver2", 
                     "bias of mu_1 estimator ver2","variance of mu_1 estimator ver2")
result2[1,]<-c(mean(mu0_est_ver05_1)-true_mu0, var(mu0_est_ver05_1), mean(mu1_est_ver05_1)-true_mu1, var(mu1_est_ver05_1))
result2[2,]<-c(mean(mu0_est_ver05_2)-true_mu0, var(mu0_est_ver05_2), mean(mu1_est_ver05_2)-true_mu1, var(mu1_est_ver05_2))
result2[3,]<-c(mean(mu0_est_ver05_3)-true_mu0, var(mu0_est_ver05_3), mean(mu1_est_ver05_3)-true_mu1, var(mu1_est_ver05_3))
result2[4,]<-c(mean(mu0_est_ver05_4)-true_mu0, var(mu0_est_ver05_4), mean(mu1_est_ver05_4)-true_mu1, var(mu1_est_ver05_4))
result2

result3<-matrix(0,nrow=4,ncol=4)
rownames(result3)<-c("#of obs = 5000", "#of obs = 20000", "#of obs = 80000", "#of obs = 320000")
colnames(result3)<-c("bias of mu_0 estimator ver3","variance of mu_0 estimator ver3", 
                     "bias of mu_1 estimator ver3","variance of mu_1 estimator ver3")
result3[1,]<-c(mean(mu0_est_ver08_1)-true_mu0, var(mu0_est_ver08_1), mean(mu1_est_ver08_1)-true_mu1, var(mu1_est_ver08_1))
result3[2,]<-c(mean(mu0_est_ver08_2)-true_mu0, var(mu0_est_ver08_2), mean(mu1_est_ver08_2)-true_mu1, var(mu1_est_ver08_2))
result3[3,]<-c(mean(mu0_est_ver08_3)-true_mu0, var(mu0_est_ver08_3), mean(mu1_est_ver08_3)-true_mu1, var(mu1_est_ver08_3))
result3[4,]<-c(mean(mu0_est_ver08_4)-true_mu0, var(mu0_est_ver08_4), mean(mu1_est_ver08_4)-true_mu1, var(mu1_est_ver08_4))
result3

