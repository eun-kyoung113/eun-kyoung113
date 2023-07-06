## consistency check of IPTW ATT Variance estimator - 6/29 update ver##
### 1. true mu_1, mu_0 finding ###
### 2. Again consistency check ###

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


################################################
## robust variance calculation function - IPTW ATT ##
sandwich_var_ATT<-function(weight_obj,result_obj,data,cov,var_treat,var_y){
  ps<-weight_obj$ps
  fit<-weight_obj$model
  weight<-weight_obj$untrimmed
  
  w<-diag(ps*(1-ps))
  X<-model.matrix(fit)
  a11<-(-1)*crossprod(sqrt(w)%*%X)/nrow(data)
  #a12<-matrix(0,nrow=length(cov),ncol=2)
  #a22<-diag(rep(mean(data[,var_treat]),2))
  a21_1<-(-1)*rep(0,(length(data[,cov])+1))
  mu0_hat<-mean(result_obj$fitted.values[data[,var_treat]==0])
  a21_2<-((-1)*((data[,var_y]-mu0_hat)*(1-data[,var_treat])*(ps/(1-ps)))%*%X)/nrow(data)
  a21<-rbind(a21_1,a21_2)
  
  mu1_hat<-mean(result_obj$fitted.values[data[,var_treat]==1])
  b21_1<-(data[,var_treat]*(data[,var_y]-mu1_hat)*(data[,var_treat]-ps))%*%X
  b21_2<-((data[,var_y]-mu0_hat)*(1-data[,var_treat])*(ps/(1-ps))*(data[,var_treat]-ps))%*%X
  b21<-rbind(b21_1,b21_2)/nrow(data)
  
  psi_1<-weight*data[,var_treat]*(data[,var_y]-mu1_hat)
  psi_0<-weight*(data[,var_y]-mu0_hat)*(1-data[,var_treat])
  psi_1_0<-rbind(psi_1,psi_0)
  b22<-(psi_1_0%*%t(psi_1_0))/nrow(data)
  
  p1<-mean(data[,var_treat])
  v_1<-solve(a11)
  v_2<-(1/p1)*v_1%*%t(-a21+b21)
  v_3<-(1/p1)*(-a21+b21)%*%v_1
  v_4<-(1/p1)^2*{(a21-b21)%*%v_1%*%t(a21-b21)-b21%*%v_1%*%t(b21)+b22}
  V_total<-as.matrix(rbind(cbind(v_1,v_2),cbind(v_3,v_4)))
  
  g<-c(0,rep(0,length(cov)),1,-1)
  result<-t(g)%*%V_total%*%g
  return(result/nrow(data))
}


###########################################################
########### true value of IPTW ATT variance ##############
B_sample<-rep(0,10000)
C_sample<-rep(0,10000)
U_sample<-rep(0,10000)
E_sample<-rep(0,10000)
Y_sample<-rep(0,10000)
ps<-rep(0,10000)

effect<-c(log(1.2),log(1.5),log(2),log(2))

# 100000 row
delta0<--2
delta_b<-0.01
delta_c<-0.01
sample<-replicate(10000,expr={
  B<-rbinom(1,1,prob=0.5)
  C<-rnorm(1,0,1)
  U<-runif(1)
  p_z<-inv.logit(delta0+delta_b*B+delta_c*C)
  E<-rbinom(1,1,prob=p_z)
  c(p_z,B,C,U,E)
})
ps<-sample[1,]
B_sample<-sample[2,]
C_sample<-sample[3,]
U_sample<-sample[4,]
E_sample<-sample[5,]
X_sample<-cbind(B_sample, C_sample, U_sample, E_sample)

# Y random sampling - continuous
Y_sample<-as.matrix(X_sample)%*%effect+rnorm(nrow(X_sample),0,1)


## Data store ##
write.csv(ps,file="ps_true.csv",row.names=FALSE)
write.csv(B_sample,file="B_sample_true.csv",row.names=FALSE)
write.csv(C_sample,file="C_sample_true.csv",row.names=FALSE)
#write.csv(U_sample,file="U_sample.csv",row.names=FALSE)
write.csv(E_sample,file="E_sample_true.csv",row.names=FALSE)
write.csv(Y_sample,file="Y_sample_true.csv",row.names=FALSE)

## Data import ##
true_ps<-read.csv("ps_true.csv",header=FALSE)
B_sample<-read.csv("B_sample_true.csv",header=TRUE)
C_sample<-read.csv("C_sample_true.csv",header=TRUE)
#U_sample<-read.csv("U_sample.csv",header=TRUE)
E_sample<-read.csv("E_sample_true.csv",header=TRUE)
Y_sample<-read.csv("Y_sample_true.csv",header=TRUE)
# exposure ratio = 0.119615

################################################################
###################### true mu_0, mu_1 #########################
#### Want to calculate : P(B=1|E=1) & E[C|E=1] ####

B_sample<-rep(0,1000000)
C_sample<-rep(0,1000000)
U_sample<-rep(0,1000000)
E_sample<-rep(0,1000000)
Y_sample<-rep(0,1000000)


effect<-c(log(1.2),log(1.5),log(2),log(2))

# 10000000 replication
delta0<--2
delta_b<-0.01
delta_c<-0.01
sample<-replicate(1000000,expr={
  B<-rbinom(1,1,prob=0.5)
  C<-rnorm(1,0,1)
  U<-runif(1)
  p_z<-inv.logit(delta0+delta_b*B+delta_c*C)
  E<-rbinom(1,1,prob=p_z)
  c(B,C,U,E)
})

B_sample<-sample[1,]
C_sample<-sample[2,]
U_sample<-sample[3,]
E_sample<-sample[4,]
X_sample<-cbind(B_sample, C_sample, U_sample, E_sample)

# Y random sampling - continuous
Y_sample<-as.matrix(X_sample)%*%effect+rnorm(nrow(X_sample),0,1)
df<-data.frame(E_sample, B_sample, C_sample, Y_sample)
colnames(df)<-c("E","B","C","Y")

true_B_mean<-mean(df$B[df$E==1])
# 0.5038109
true_C_mean<-mean(df$C[df$E==1])
# 0.006597521

##############################################################################################
############################ true variance ###################################################
E<-E_sample
B<-B_sample
#U<-U_sample
C<-C_sample
Y<-Y_sample
data<-data.frame(E,B,C,Y)
colnames(data)<-c("E","B","C","Y")
cov<-c("B","C")
cov_type<-c("binary","continuous")


# plus weight to column #
data$ps<-true_ps$V1
weight<-ifelse(data$E==1,1,true_ps/(1-true_ps))
data$weight_untrimmed<-weight[[2]]
data$weight_trimmed<-ifelse(weight[[2]]>quantile(weight[[2]],prob=0.99),
                            quantile(weight[[2]],prob=0.99),weight[[2]])
head(data)

ps<-data$ps
X<-as.matrix(cbind(1,data[,cov]))
weight<-data$weight_untrimmed

w<-diag(ps*(1-ps))
a11<-(-1)*crossprod(sqrt(w)%*%X)/nrow(data)
#a12<-matrix(0,nrow=length(cov),ncol=2)
#a22<-diag(rep(mean(data[,var_treat]),2))
a21_1<-(-1)*rep(0,(length(data[,cov])+1))
mu0_hat<-log(1.2)*true_B_mean + log(1.5)*true_C_mean ###########
a21_2<-((-1)*((data[,"Y"]-mu0_hat)*(1-data[,"E"])*(ps/(1-ps)))%*%X)/nrow(data)
a21<-rbind(a21_1,a21_2)

mu1_hat<-log(1.2)*true_B_mean + log(1.5)*true_C_mean + log(2) ############
b21_1<-(data[,"E"]*(data[,"Y"]-mu1_hat)*(data[,"E"]-ps))%*%X
b21_2<-((data[,"Y"]-mu0_hat)*(1-data[,"E"])*(ps/(1-ps))*(data[,"E"]-ps))%*%X
b21<-rbind(b21_1,b21_2)/nrow(data)

psi_1<-weight*data[,"E"]*(data[,"Y"]-mu1_hat)
psi_0<-weight*(data[,"Y"]-mu0_hat)*(1-data[,"E"])
psi_1_0<-rbind(psi_1,psi_0)
b22<-(psi_1_0%*%t(psi_1_0))/nrow(data)

p1<-mean(data[,"E"])
v_1<-solve(a11)
v_2<-(1/p1)*v_1%*%t(-a21+b21)
v_3<-(1/p1)*(-a21+b21)%*%v_1
v_4<-(1/p1)^2*{(a21-b21)%*%v_1%*%t(a21-b21)-b21%*%v_1%*%t(b21)+b22}
V_total<-as.matrix(rbind(cbind(v_1,v_2),cbind(v_3,v_4)))

g<-c(0,rep(0,length(cov)),1,-1)
true_result<-t(g)%*%V_total%*%g
print(true_result)
#  4.886431


############################ version 1 : 100th row X  1000th data ################################\
## Data import ##
B_sample<-read.csv("B_sample.csv",header=TRUE)
C_sample<-read.csv("C_sample.csv",header=TRUE)
#U_sample<-read.csv("U_sample.csv",header=TRUE)
E_sample<-read.csv("E_sample.csv",header=TRUE)
Y_sample<-read.csv("Y_sample.csv",header=TRUE)
######### check #############################################################
var_est_ver1<-rep(0,1000)

for(i in 1:1000){
  E<-E_sample[,i]
  B<-B_sample[,i]
  #U<-U_sample[,1]
  C<-C_sample[,i]
  Y<-Y_sample[,i]
  data<-data.frame("E"=E,"B"=B,"C"=C,"Y"=Y)
  cov<-c("B","C")
  cov_type<-c("binary","continuous")
  #mydata<-data_export("E",cov,data)
  #head(mydata)
  
  weight_result<-weight_make("E",cov,estimate='ATT',data=data)
  
  # plus weight to column #
  data$ps<-weight_result$ps
  data$weight_untrimmed<-weight_result$untrimmed 
  data$weight_trimmed<-weight_result$trimmed
  #head(data)
  
  ATT_untrimmed<-lm(Y~E,data=data,weight=weight_result$untrimmed)
  estimate<-sandwich_var_ATT(weight_result,ATT_untrimmed,data,cov,"E","Y")*nrow(data)
  var_est_ver1[i]<-c(estimate)
  
}

plot(var_est_ver1,xlab="replication",ylab="IPTW ATT Variance estimator",main="#of observation=100")
abline(h=true_result,col='red')


################################## version 2 : total 1000th row X  100th data #####################################
# sample store
B_sample<-matrix(0,nrow=1000,ncol=100)
C_sample<-matrix(0,nrow=1000,ncol=100)
U_sample<-matrix(0,nrow=1000,ncol=100)
E_sample<-matrix(0,nrow=1000,ncol=100)
Y_sample<-matrix(0,nrow=1000,ncol=100)

effect<-c(log(1.2),log(1.5),log(2),log(2))

# 100th replication data generating
for(repl in 1:100){
  delta0<--2
  delta_b<-0.01
  delta_c<-0.01
  set.seed(123*repl)
  sample<-replicate(1000,expr={
    B<-rbinom(1,1,prob=0.5)
    C<-rnorm(1,0,1)
    U<-runif(1)
    p_z<-inv.logit(delta0+delta_b*B+delta_c*C)
    E<-rbinom(1,1,prob=p_z)
    c(B,C,U,E)
  })
  B_sample[,repl]<-sample[1,]
  C_sample[,repl]<-sample[2,]
  U_sample[,repl]<-sample[3,]
  E_sample[,repl]<-sample[4,]
  X_sample<-cbind(B_sample[,repl],C_sample[,repl],U_sample[,repl],E_sample[,repl])
  
  # Y random sampling - continuous
  Y_sample[,repl]<-as.matrix(X_sample)%*%effect+rnorm(nrow(X_sample),0,1)
  
  # Y_sample random sampling - binary                    
  #prob<-inv.logit(as.matrix(X_sample)%*%effect)
  #Y_sample_binary<-rbinom(n=nrow(X_sample), size=1, prob=prob)
}

## Data store ##
write.csv(B_sample,file="B_sample_1000.csv",row.names=FALSE)
write.csv(C_sample,file="C_sample_1000.csv",row.names=FALSE)
#write.csv(U_sample,file="U_sample.csv",row.names=FALSE)
write.csv(E_sample,file="E_sample_1000.csv",row.names=FALSE)
write.csv(Y_sample,file="Y_sample_1000.csv",row.names=FALSE)

## Data import ##
B_sample<-read.csv("B_sample_1000.csv",header=TRUE)
C_sample<-read.csv("C_sample_1000.csv",header=TRUE)
#U_sample<-read.csv("U_sample.csv",header=TRUE)
E_sample<-read.csv("E_sample_1000.csv",header=TRUE)
Y_sample<-read.csv("Y_sample_1000.csv",header=TRUE)

###############################################################################
var_est_ver2<-rep(0,100)

for(i in 1:100){
  E<-E_sample[,i]
  B<-B_sample[,i]
  #U<-U_sample[,1]
  C<-C_sample[,i]
  Y<-Y_sample[,i]
  data<-data.frame("E"=E,"B"=B,"C"=C,"Y"=Y)
  cov<-c("B","C")
  cov_type<-c("binary","continuous")
  #mydata<-data_export("E",cov,data)
  #head(mydata)
  
  weight_result<-weight_make("E",cov,estimate='ATT',data=data)
  
  # plus weight to column #
  data$ps<-weight_result$ps
  data$weight_untrimmed<-weight_result$untrimmed 
  data$weight_trimmed<-weight_result$trimmed
  
  ATT_untrimmed<-lm(Y~E,data=data,weight=weight_result$untrimmed)
  estimate<-sandwich_var_ATT(weight_result,ATT_untrimmed,data,cov,"E","Y")*nrow(data)
  var_est_ver2[i]<-estimate
  
}
plot(var_est_ver2,xlab="replication",ylab="IPTW ATT Variance estimator",ylim=c(0,20),main="#of observation=1000")
abline(h=true_result,col='red')

####################################################################################################################
################################## version 3 : total 10000th row X  100th data #####################################
# sample store
B_sample<-matrix(0,nrow=10000,ncol=100)
C_sample<-matrix(0,nrow=10000,ncol=100)
U_sample<-matrix(0,nrow=10000,ncol=100)
E_sample<-matrix(0,nrow=10000,ncol=100)
Y_sample<-matrix(0,nrow=10000,ncol=100)

effect<-c(log(1.2),log(1.5),log(2),log(2))

# 100th replication data generating
for(repl in 1:100){
  delta0<--2
  delta_b<-0.01
  delta_c<-0.01
  set.seed(123*repl)
  sample<-replicate(10000,expr={
    B<-rbinom(1,1,prob=0.5)
    C<-rnorm(1,0,1)
    U<-runif(1)
    p_z<-inv.logit(delta0+delta_b*B+delta_c*C)
    E<-rbinom(1,1,prob=p_z)
    c(B,C,U,E)
  })
  B_sample[,repl]<-sample[1,]
  C_sample[,repl]<-sample[2,]
  U_sample[,repl]<-sample[3,]
  E_sample[,repl]<-sample[4,]
  X_sample<-cbind(B_sample[,repl],C_sample[,repl],U_sample[,repl],E_sample[,repl])
  
  # Y random sampling - continuous
  Y_sample[,repl]<-as.matrix(X_sample)%*%effect+rnorm(nrow(X_sample),0,1)
  
  # Y_sample random sampling - binary                    
  #prob<-inv.logit(as.matrix(X_sample)%*%effect)
  #Y_sample_binary<-rbinom(n=nrow(X_sample), size=1, prob=prob)
}

## Data store ##
write.csv(B_sample,file="B_sample_10000.csv",row.names=FALSE)
write.csv(C_sample,file="C_sample_10000.csv",row.names=FALSE)
#write.csv(U_sample,file="U_sample.csv",row.names=FALSE)
write.csv(E_sample,file="E_sample_10000.csv",row.names=FALSE)
write.csv(Y_sample,file="Y_sample_10000.csv",row.names=FALSE)

## Data import ##
B_sample<-read.csv("B_sample_10000.csv",header=TRUE)
C_sample<-read.csv("C_sample_10000.csv",header=TRUE)
#U_sample<-read.csv("U_sample.csv",header=TRUE)
E_sample<-read.csv("E_sample_10000.csv",header=TRUE)
Y_sample<-read.csv("Y_sample_10000.csv",header=TRUE)

###############################################################################
var_est_ver3<-rep(0,100)

for(i in 1:100){
  E<-E_sample[,i]
  B<-B_sample[,i]
  #U<-U_sample[,1]
  C<-C_sample[,i]
  Y<-Y_sample[,i]
  data<-data.frame("E"=E,"B"=B,"C"=C,"Y"=Y)
  cov<-c("B","C")
  cov_type<-c("binary","continuous")
  #mydata<-data_export("E",cov,data)
  #head(mydata)
  
  weight_result<-weight_make("E",cov,estimate='ATT',data=data)
  
  # plus weight to column #
  data$ps<-weight_result$ps
  data$weight_untrimmed<-weight_result$untrimmed 
  data$weight_trimmed<-weight_result$trimmed
  
  ATT_untrimmed<-lm(Y~E,data=data,weight=weight_result$untrimmed)
  estimate<-sandwich_var_ATT(weight_result,ATT_untrimmed,data,cov,"E","Y")*nrow(data)
  var_est_ver3[i]<-estimate
  
}

plot(var_est_ver3,xlab="replication",ylab="IPTW ATT Variance estimator",ylim=c(0,16),main="#of observation=10000")
abline(h=true_result,col='red')

####################################################################################################################
################################## version 4 : total 20000th row X  100th data #####################################
# sample store
B_sample<-matrix(0,nrow=20000,ncol=100)
C_sample<-matrix(0,nrow=20000,ncol=100)
U_sample<-matrix(0,nrow=20000,ncol=100)
E_sample<-matrix(0,nrow=20000,ncol=100)
Y_sample<-matrix(0,nrow=20000,ncol=100)

effect<-c(log(1.2),log(1.5),log(2),log(2))

# 100th replication data generating
for(repl in 1:100){
  delta0<--2
  delta_b<-0.01
  delta_c<-0.01
  set.seed(123*repl)
  sample<-replicate(10000,expr={
    B<-rbinom(1,1,prob=0.5)
    C<-rnorm(1,0,1)
    U<-runif(1)
    p_z<-inv.logit(delta0+delta_b*B+delta_c*C)
    E<-rbinom(1,1,prob=p_z)
    c(B,C,U,E)
  })
  B_sample[,repl]<-sample[1,]
  C_sample[,repl]<-sample[2,]
  U_sample[,repl]<-sample[3,]
  E_sample[,repl]<-sample[4,]
  X_sample<-cbind(B_sample[,repl],C_sample[,repl],U_sample[,repl],E_sample[,repl])
  
  # Y random sampling - continuous
  Y_sample[,repl]<-as.matrix(X_sample)%*%effect+rnorm(nrow(X_sample),0,1)
  
  # Y_sample random sampling - binary                    
  #prob<-inv.logit(as.matrix(X_sample)%*%effect)
  #Y_sample_binary<-rbinom(n=nrow(X_sample), size=1, prob=prob)
}

## Data store ##
write.csv(B_sample,file="B_sample_20000.csv",row.names=FALSE)
write.csv(C_sample,file="C_sample_20000.csv",row.names=FALSE)
#write.csv(U_sample,file="U_sample.csv",row.names=FALSE)
write.csv(E_sample,file="E_sample_20000.csv",row.names=FALSE)
write.csv(Y_sample,file="Y_sample_20000.csv",row.names=FALSE)

## Data import ##
B_sample<-read.csv("B_sample_20000.csv",header=TRUE)
C_sample<-read.csv("C_sample_20000.csv",header=TRUE)
#U_sample<-read.csv("U_sample.csv",header=TRUE)
E_sample<-read.csv("E_sample_20000.csv",header=TRUE)
Y_sample<-read.csv("Y_sample_20000.csv",header=TRUE)

###############################################################################
var_est_ver4<-rep(0,100)

for(i in 1:100){
  E<-E_sample[,i]
  B<-B_sample[,i]
  #U<-U_sample[,1]
  C<-C_sample[,i]
  Y<-Y_sample[,i]
  data<-data.frame("E"=E,"B"=B,"C"=C,"Y"=Y)
  cov<-c("B","C")
  cov_type<-c("binary","continuous")
  #mydata<-data_export("E",cov,data)
  #head(mydata)
  
  weight_result<-weight_make("E",cov,estimate='ATT',data=data)
  
  # plus weight to column #
  data$ps<-weight_result$ps
  data$weight_untrimmed<-weight_result$untrimmed 
  data$weight_trimmed<-weight_result$trimmed
  
  ATT_untrimmed<-lm(Y~E,data=data,weight=weight_result$untrimmed)
  estimate<-sandwich_var_ATT(weight_result,ATT_untrimmed,data,cov,"E","Y")*nrow(data)
  var_est_ver4[i]<-estimate
  
}

plot(var_est_ver4,xlab="replication",ylab="IPTW ATT Variance estimator",ylim=c(0,15),main="#of observation=20000")
abline(h=true_result,col='red')


###### result table ########
result<-matrix(0,nrow=4,ncol=2)
rownames(result)<-c("#of obs = 100", "#of obs = 1000", "#of obs = 10000", "#of obs = 20000")
colnames(result)<-c("bias of estimator","variance of estimator")
result[1,]<-c(mean(var_est_ver1)-true_result,var(var_est_ver1))
result[2,]<-c(mean(var_est_ver2)-true_result,var(var_est_ver2))
result[3,]<-c(mean(var_est_ver3)-true_result,var(var_est_ver3))
result[4,]<-c(mean(var_est_ver4)-true_result,var(var_est_ver4))
result
