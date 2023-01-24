## consistency check of IPTW ATT Variance estimator - 7/22 update ver##
### 1. consistency assumption check for mu1 and mu0 except unobservable "U" variable###
### 2. Change the distribution of "B" variable -> from binomial dist to normal

library(boot)
library(tableone)
library(survey)
library(Hmisc)




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


#################################################################################
############ Consistency assumption check about mu0 and mu1 estimator with increasing exposure ratio###########
B_sample<-matrix(0,nrow=100000,ncol=100)
C_sample<-matrix(0,nrow=100000,ncol=100)
#U_sample<-matrix(0,nrow=5000,ncol=100)
E_sample<-matrix(0,nrow=100000,ncol=100)
Y0_sample<-matrix(0,nrow=100000,ncol=100)
Y1_sample<-matrix(0,nrow=100000,ncol=100)
Y_sample<-matrix(0,nrow=100000,ncol=100)

effect<-c(log(1.5),log(2),log(2))

# 100th replication data generating
for(repl in 1:100){
  delta0<-1
  delta_b<-0.5
  delta_c<-0.5
  set.seed(123*repl)
  sample<-replicate(100000,expr={
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
  X_sample<-cbind(B_sample[,repl],C_sample[,repl])
  Y0_sample[,repl]<-as.matrix(cbind(X_sample,0))%*%effect+rnorm(nrow(X_sample),0,1)
  Y1_sample[,repl]<-as.matrix(cbind(X_sample,1))%*%effect+rnorm(nrow(X_sample),0,1)
  
  # Y random sampling - continuous
  Y_sample[,repl]<-Y0_sample[,repl]*(E_sample[,repl]==0)+Y1_sample[,repl]*(E_sample[,repl]==1)
}

cons_check_mu0<-matrix(0,nrow=100,ncol=4)
colnames(cons_check_mu0)<-c("ver01","ver04","ver05","ver08")

cons_check_mu1<-matrix(0,nrow=100,ncol=4)
colnames(cons_check_mu0)<-c("ver01","ver04","ver05","ver08")


for(i in 1:100){
  E<-E_sample[,i]
  B<-B_sample[,i]
  #U<-U_sample[,1]
  C<-C_sample[,i]
  Y0<-Y0_sample[,i]
  Y1<-Y1_sample[,i]
  Y<-Y_sample[,i]
  
  data<-data.frame("E"=E,"B"=B,"C"=C,"Y"=Y,"Y0"=Y0,"Y1"=Y1)
  true_mu0<-mean(data$Y0[data$E==1])
  true_mu1<-mean(data$Y1[data$E==1])
  
  cov<-c("B","C")
  cov_type<-c("binary","continuous")
  weight_result<-weight_make("E",cov,estimate='ATT',data=data)
  
  # plus weight to column #
  data$weight_untrimmed<-weight_result$untrimmed 
  
  ATT_untrimmed<-lm(Y~E,data=data,weight=weight_result$untrimmed)
  cons_check_mu0[i,1]<-mean(data$Y[data$E==0])-true_mu0
  cons_check_mu1[i,1]<-mean(data$Y[data$E==1])-true_mu1
  
  cons_check_mu0[i,2]<-log(1.5)*mean(data$B[data$E==1]) + log(2)*mean(data$C[data$E==1])-true_mu0
  cons_check_mu1[i,2]<-log(1.5)*mean(data$B[data$E==1]) + log(2)*mean(data$C[data$E==1]) + log(2)-true_mu1
  
  cons_check_mu0[i,3]<-mean(ATT_untrimmed$fitted.values[data$E==0])-true_mu0
  cons_check_mu1[i,3]<-mean(ATT_untrimmed$fitted.values[data$E==1])-true_mu1
  
  cons_check_mu0[i,4]<-sum(data$weight_untrimmed*(1-data$E)*data$Y)/sum(data$weight_untrimmed*(1-data$E))-true_mu0
  cons_check_mu1[i,4]<-sum(data$weight_untrimmed*data$E*data$Y)/sum(data$weight_untrimmed*data$E)-true_mu1
}

apply(cons_check_mu0,2,mean)
apply(cons_check_mu0,2,var)

apply(cons_check_mu1,2,mean)
apply(cons_check_mu1,2,var)

#apply(cons_check_mu0,2,mean)
#ver01         ver04         ver05         ver08 
#-0.3760882052  0.0009757375  0.0018798240  0.0018798240 

# apply(cons_check_mu0,2,var)
#ver01        ver04        ver05        ver08 
#8.071604e-05 1.036956e-05 7.747421e-05 7.747421e-05 

# apply(cons_check_mu1,2,mean)
#[1]  0.000000e+00  3.546110e-04 -5.528911e-16  4.440892e-18

# apply(cons_check_mu1,2,var)
#[1] 0.000000e+00 1.352759e-05 3.248597e-29 6.454316e-33






################################################################################
########### Consider true mu0 and mu1 be ver04 estimator ##########
cons_check_mu0_ver2<-matrix(0,nrow=100,ncol=3)
colnames(cons_check_mu0_ver2)<-c("ver01","ver05","ver08")

cons_check_mu1_ver2<-matrix(0,nrow=100,ncol=3)
colnames(cons_check_mu0_ver2)<-c("ver01","ver05","ver08")


for(i in 1:100){
  E<-E_sample[,i]
  B<-B_sample[,i]
  #U<-U_sample[,1]
  C<-C_sample[,i]
  Y0<-Y0_sample[,i]
  Y1<-Y1_sample[,i]
  Y<-Y_sample[,i]
  
  data<-data.frame("E"=E,"B"=B,"C"=C,"Y"=Y,"Y0"=Y0,"Y1"=Y1)
  true_mu0<-log(1.5)*mean(data$B[data$E==1]) + log(2)*mean(data$C[data$E==1])
  true_mu1<-true_mu0+log(2)
  
  cov<-c("B","C")
  cov_type<-c("binary","continuous")
  weight_result<-weight_make("E",cov,estimate='ATT',data=data)
  
  # plus weight to column #
  data$weight_untrimmed<-weight_result$untrimmed 
  
  ATT_untrimmed<-lm(Y~E,data=data,weight=weight_result$untrimmed)
  
  cons_check_mu0_ver2[i,1]<-mean(data$Y[data$E==0])-true_mu0
  cons_check_mu1_ver2[i,1]<-mean(data$Y[data$E==1])-true_mu1
  
  cons_check_mu0_ver2[i,2]<-mean(ATT_untrimmed$fitted.values[data$E==0])-true_mu0
  cons_check_mu1_ver2[i,2]<-mean(ATT_untrimmed$fitted.values[data$E==1])-true_mu1
  
  cons_check_mu0_ver2[i,3]<-sum(data$weight_untrimmed*(1-data$E)*data$Y)/sum(data$weight_untrimmed*(1-data$E))-true_mu0
  cons_check_mu1_ver2[i,3]<-sum(data$weight_untrimmed*data$E*data$Y)/sum(data$weight_untrimmed*data$E)-true_mu1
}

apply(cons_check_mu0_ver2,2,mean)
apply(cons_check_mu0_ver2,2,var)

apply(cons_check_mu1_ver2,2,mean)
apply(cons_check_mu1_ver2,2,var)

#apply(cons_check_mu0_ver2,2,mean)
#ver01         ver05         ver08 
#-0.3770639426  0.0009040866  0.0009040866 
# apply(cons_check_mu0_ver2,2,var)
#ver01        ver05        ver08 
#6.673565e-05 6.964828e-05 6.964828e-05 
# apply(cons_check_mu1_ver2,2,mean)
#[1] -0.000354611 -0.000354611 -0.000354611
# apply(cons_check_mu1_ver2,2,var)
#[1] 1.352759e-05 1.352759e-05 1.352759e-05












