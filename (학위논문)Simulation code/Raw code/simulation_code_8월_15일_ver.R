########### Simulation code 8/15 version ##############
####### Update #########
## 1. DR Estimator function corrected
## 2. DR Variance estimator coding
########################

library(boot)
library(tableone)
library(survey)
library(Hmisc)
library(geepack)
library(geex)

##################### 1. Check IPW ATT Variance estimator #####################
#### Data generating ####
B_sample<-matrix(0,nrow=100,ncol=1000)
C_sample<-matrix(0,nrow=100,ncol=1000)
U_sample<-matrix(0,nrow=100,ncol=1000)
E_sample<-matrix(0,nrow=100,ncol=1000)
Y_sample<-matrix(0,nrow=100,ncol=1000)

effect<-c(log(1.2),log(1.5),log(2),log(2))

# 1000th replication data generating
for(repl in 1:1000){
  delta0<--2
  delta_b<-0.01
  delta_c<-0.01
  set.seed(123*repl)
  sample<-replicate(100,expr={
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
write.csv(B_sample,file="B_sample.csv",row.names=FALSE)
write.csv(C_sample,file="C_sample.csv",row.names=FALSE)
write.csv(U_sample,file="U_sample.csv",row.names=FALSE)
write.csv(E_sample,file="E_sample.csv",row.names=FALSE)
write.csv(Y_sample,file="Y_sample.csv",row.names=FALSE)

## Data import ##
B_sample<-read.csv("B_sample.csv",header=TRUE)
C_sample<-read.csv("C_sample.csv",header=TRUE)
#U_sample<-read.csv("U_sample.csv",header=TRUE)
E_sample<-read.csv("E_sample.csv",header=TRUE)
Y_sample<-read.csv("Y_sample.csv",header=TRUE)


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

###############################################################################
## balance check function - no function ##  
balance_check<-function(var_treat,var_cov,var_weight,cov_type,data){
  balance_table<-matrix(0,nrow=length(var_cov),ncol=2)
  colnames(balance_table)<-c("before_aSD","after_aSD")
  rownames(balance_table)<-var_cov
  
  cov<-data[,var_cov]
  weight<-data[,var_weight]
  
  for(i in 1:length(cov)){
    var_cov<-cov[,i]
    
    if(cov_type[i]=="continuous"){
      #std_var_cov = (var_cov - mean(var_cov))/sd(var_cov)
      simple_M1 = mean(var_cov[data[,var_treat]==1])
      simple_M0 = mean(var_cov[data[,var_treat]==0])
      simple_V1 = var(var_cov[data[,var_treat]==1])
      simple_V0 = var(var_cov[data[,var_treat]==0])
      wgted_M1 = sum((var_cov[data[,var_treat]==1])*(weight[data[,var_treat]==1]))/sum(weight[data[,var_treat]==1])
      wgted_M0 = sum((var_cov[data[,var_treat]==0])*(weight[data[,var_treat]==0]))/sum(weight[data[,var_treat]==0])
      wgted_V1 = sum(((var_cov[data[,var_treat]==1]-wgted_M1)^2)*(weight[data[,var_treat]==1]))*sum(weight[data[,var_treat]==1])/(sum(weight[data[,var_treat]==1])^2-sum(weight[data[,var_treat]==1]^2))
      wgted_V0 = sum(((var_cov[data[,var_treat]==0]-wgted_M0)^2)*(weight[data[,var_treat]==0]))*sum(weight[data[,var_treat]==0])/(sum(weight[data[,var_treat]==0])^2-sum(weight[data[,var_treat]==0]^2))
      
    }
    else{
      #std_var_cov = (var_cov - mean(var_cov))/sd(var_cov)
      simple_M1 = mean(var_cov[data[,var_treat]==1])
      simple_M0 = mean(var_cov[data[,var_treat]==0])
      simple_V1 = simple_M1*(1-simple_M1)
      simple_V0 = simple_M0*(1-simple_M0)
      wgted_M1 = sum((var_cov[data[,var_treat]==1])*(weight[data[,var_treat]==1]))/sum(weight[data[,var_treat]==1])
      wgted_M0 = sum((var_cov[data[,var_treat]==0])*(weight[data[,var_treat]==0]))/sum(weight[data[,var_treat]==0])
      wgted_V1 = wgted_M1*(1-wgted_M1)
      wgted_V0 = wgted_M0*(1-wgted_M0)
      
    }
    
    
    ## aSD calculation ##
    before_aSD<-(simple_M1-simple_M0)/sqrt((simple_V1+simple_V0)/2)
    after_aSD<-(wgted_M1-wgted_M0)/sqrt((wgted_V1+wgted_V0)/2)
    balance_table[i,1]<-before_aSD
    balance_table[i,2]<-after_aSD
  }
  return(balance_table)
}
######################################################################################

###################################################################################
## robust variance calculation function - IPTW ATE ##
sandwich_var_ATE<-function(obj,data){
  
  X = model.matrix(obj)
  weight<-weights(obj)
  new_X<-sqrt(weight)*X
  bread<-solve(crossprod(new_X))*as.vector(nrow(data))
  psi<-as.vector(ATE_untrimmed$residuals)*weight*X
  meat<-crossprod(as.matrix(psi))/nrow(psi)
  vr<-bread%*%meat%*%bread/nrow(data)
  
  return(vr)
  
}
##########################################################

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
  #mu0_hat<-mean(result_obj$fitted.values[data[,var_treat]==0])
  mu0_hat<-log(1.5)*mean(data$B[data[,var_treat]==1]) + log(2)*mean(data$C[data[,var_treat]==1])
  a21_2<-((-1)*((data[,var_y]-mu0_hat)*(1-data[,var_treat])*(ps/(1-ps)))%*%X)/nrow(data)
  a21<-rbind(a21_1,a21_2)
  
  mu1_hat<-mean(result_obj$fitted.values[data[,var_treat]==1])
  #ind<-which(data[,var_treat]==1)
  #mu1_hat<-mu0_hat+log(2)
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

#################################################################################

######### check #############################################################
## Data import ##
B_sample<-read.csv("B_sample.csv",header=TRUE)
C_sample<-read.csv("C_sample.csv",header=TRUE)
#U_sample<-read.csv("U_sample.csv",header=TRUE)
E_sample<-read.csv("E_sample.csv",header=TRUE)
Y_sample<-read.csv("Y_sample.csv",header=TRUE)

E<-E_sample[,1]
B<-B_sample[,1]
#U<-U_sample[,1]
C<-C_sample[,1]
Y<-Y_sample[,1]
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
head(data)

#balance_untrimmed_data<-balance_check("E",cov,"weight_untrimmed",cov_type,data)
#print(balance_untrimmed_data)

#balance_trimmed_data<-balance_check("E",cov,"weight_trimmed",cov_type,data)
#print(balance_trimmed_data)

ATE_untrimmed<-lm(Y~E,data=data,weight=weight_result$untrimmed)
sandwich_var_ATE(ATE_untrimmed,data)

robust_var<-vcovCR(ATE_untrimmed,cluster=as.numeric(rownames(data)),type="CR0")
robust_var

ATT_untrimmed<-lm(Y~E,data=data,weight=weight_result$untrimmed)
gg<-cbind(c(1,-1))
estfun<-function(data,model){
  E<-data$E
  Y<-data$Y
  IPW<-data$weight_untrimmed
  
  function(theta){
    ce1<-IPW*(E==1)*(Y-theta[1])
    ce0<-IPW*(E==0)*(Y-theta[2])
    c(ce1,ce0)
  }
}
fit<-geeglm(Y~E,data=data,weights=weight_untrimmed,id=1:nrow(data),corstr='independence')
mu1_hat<-mean(fit$fitted.values[fit$data$E==1])
mu0_hat<-mean(fit$fitted.values[fit$data$E==0])
result<-m_estimate(estFUN=estfun,data=data,roots=c(mu1_hat,mu0_hat),compute_roots = FALSE)
vcov_SEE<-vcov(result)[1:2,1:2]
sigma<-t(gg)%*%vcov_SEE%*%gg
#sigma=0.08249

sandwich_var_ATT(weight_result,ATT_untrimmed,data,cov,"E","Y")
# 0.09211579
#################################################################################


#############################################################################
##################### 2. DR ATT estimator ##########################
## Data import ##
B_sample<-read.csv("B_sample.csv",header=TRUE)
C_sample<-read.csv("C_sample.csv",header=TRUE)
#U_sample<-read.csv("U_sample.csv",header=TRUE)
E_sample<-read.csv("E_sample.csv",header=TRUE)
Y_sample<-read.csv("Y_sample.csv",header=TRUE)

################################################

#### 1-1.DR Estimator function -----------------------------------------------
DR_estimator<-function(estimate,data,var_treat,var_y,cov){
  
  result<-list()
  
  mydata<-data[c(var_treat,cov)]
  
  myformula<-as.formula(sprintf("%s~.",var_treat))
  fit<-glm(formula=myformula,data=mydata,family='binomial')
  ps<-fit$fitted.values
  
  data$ps<-ps
  result$ps<-ps
  
  myformula1<-as.formula(sprintf("%s~.",var_y))
  
  #ind_mu0<-which(data[,var_treat]==0)
  #mu0_df<-data[ind_mu1,c(var_y,cov)]
  
  out_model<-lm(formula=myformula1,data=data)
  new_dt_mu0<-model.matrix(out_model)
  new_dt_mu0[,2]<-rep(0,nrow(data))
  mu0_X<-coef(out_model)%*%t(new_dt_mu0)
  
  result$out_model<-out_model
  result$mu0_X<-mu0_X
  
  if(estimate=='ATE'){
    ind_mu1<-which(data[,var_treat]==1)
    #mu1_df<-data[ind_mu1,c(var_y,cov)]
    
    new_dt_mu1<-model.matrix(out_model)
    new_dt_mu1[,2]<-rep(1,nrow(data))
    
    mu1_X<-coef(out_model)%*%t(new_dt_mu1)
    result$mu1_X<-mu1_X
    
    mu1_dr<-mean(data[,var_treat]*data[,var_y]/data$ps - ((data[,var_treat]-data$ps)/data$ps)*(mu1_X))
    mu0_dr<-mean(mu0_X+((1-data[,var_treat])*(data[,var_y]-mu0_X))/(1-data$ps))
    
    result$est<-mu1_dr-mu0_dr
  }
  
  else if(estimate=='ATT'){
    ind_mu1<-which(data[,var_treat]==1)
    result$est<-sum(data[,var_y]*data[,var_treat]-((data[,var_y]*(1-data[,var_treat])*data$ps)+(mu0_X)*(data[,var_treat]-data$ps))/(1-data$ps))/length(ind_mu1) 
  }
  
  return(result)
}


###########################################################################
##### 1-2. Naive variance of DR estimator -------------------------------------
DR_Naive_var_estimator<-function(estimate,data,var_treat,var_y,cov){
  result<-DR_estimator(estimate,data,var_treat,var_y,cov)
  
  tau_dr<-result$est
  
  if(estimate=="ATE"){
    tau_i<-data[,var_treat]*data[,var_y]/result$ps - 
      ((data[,var_treat]-result$ps)/result$ps)*(result$mu1_X)-
      result$mu0_X+((1-data[,var_treat])*(data[,var_y]-result$mu0_X))/(1-result$ps)
    
    var<-sum((tau_i-tau_dr)^2)/(nrow(data)^2)
  }
  
  else if(estimate=="ATT"){
    tau_i<-(data[,var_y]*data[,var_treat])
    -(((data[,var_y]*(1-data[,var_treat])*result$ps)+result$mu0_X*(data[,var_treat]-result$ps))/(1-result$ps))
    
    var<-sum((tau_i-tau_dr)^2)/(length(data[,var_treat]==1)^2)
  }
  
  
  return(var)
  
}


#############################################################################
### 1-3. Sandwich robust variance estimator of DR Estimator -----------------


######## Doubly robust estimator by using package ########
### 1. ATE ###
library(drgee)
result<-drgee(oformula=formula(Y~B+C),
              eformula=formula(E~B+C),
              olink = 'identity',elink='logit',
              data=data,estimation.method = 'dr')

result$coefficients
result$vcov

### 2. ATT ###
library(DRDID)

data$D<-data$E
result_ATT<-drdid_imp_rc1(y=data$Y,post=data$E,D=data$D,covariates=data[,c("B","C")])
#############################################################
######### check ######################
E<-E_sample[,1]
B<-B_sample[,1]
#U<-U_sample[,1]
C<-C_sample[,1]
Y<-Y_sample[,1]
data<-data.frame("E"=E,"B"=B,"C"=C,"Y"=Y)
cov<-c("B","C")
cov_type<-c("binary","continuous")
#mydata<-data_export("E",cov,data)
#head(mydata)

DR_ATE<-DR_estimator("ATE",data,"E","Y",cov)
DR_ATT<-DR_estimator("ATT",data,"E","Y",cov)
print(c(DR_ATT$est,DR_ATE$est))

DR_Naive_var_estimator("ATE",data,"E","Y",cov)
DR_Naive_var_estimator("ATT",data,"E","Y",cov)
##############################################






###### 1-3. Consistency check of DR estimator ----------------------------------
result_check<-matrix(0,nrow=4,ncol=4)
rownames(result_check)<-c("#of obs=1000","#of obs=10000","#of obs=50000","#of obs=100000")
colnames(result_check)<-c("Bias of DR of ATE","Variance of DR of ATE","Bias of DR of ATT","Variance of DR of ATT")

### Version 1. #obs = 1000 ### 
#### Data generating ####
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
write.csv(U_sample,file="U_sample_1000.csv",row.names=FALSE)
write.csv(E_sample,file="E_sample_1000.csv",row.names=FALSE)
write.csv(Y_sample,file="Y_sample_1000.csv",row.names=FALSE)

## Data import ##
B_sample<-read.csv("B_sample_1000.csv",header=TRUE)
C_sample<-read.csv("C_sample_1000.csv",header=TRUE)
#U_sample<-read.csv("U_sample.csv",header=TRUE)
E_sample<-read.csv("E_sample_1000.csv",header=TRUE)
Y_sample<-read.csv("Y_sample_1000.csv",header=TRUE)

DR_ATE_1000<-c()
DR_ATT_1000<-c()

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
  ATE_result<-DR_estimator("ATE",data,"E","Y",cov)
  ATT_result<-DR_estimator("ATT",data,"E","Y",cov)
  
  DR_ATE_1000[i]<-ATE_result$est
  DR_ATT_1000[i]<-ATT_result$est
  
}

result_check[1,]<-c(mean(DR_ATE_1000)-log(2),var(DR_ATE_1000),mean(DR_ATT_1000)-log(2),var(DR_ATT_1000))


######################################################################
### Version 2. #obs = 10000 ### 
#### Data generating ####
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
write.csv(U_sample,file="U_sample_10000.csv",row.names=FALSE)
write.csv(E_sample,file="E_sample_10000.csv",row.names=FALSE)
write.csv(Y_sample,file="Y_sample_10000.csv",row.names=FALSE)

## Data import ##
B_sample<-read.csv("B_sample_10000.csv",header=TRUE)
C_sample<-read.csv("C_sample_10000.csv",header=TRUE)
#U_sample<-read.csv("U_sample.csv",header=TRUE)
E_sample<-read.csv("E_sample_10000.csv",header=TRUE)
Y_sample<-read.csv("Y_sample_10000.csv",header=TRUE)

DR_ATE_10000<-c()
DR_ATT_10000<-c()

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
  ATE_result<-DR_estimator("ATE",data,"E","Y",cov)
  ATT_result<-DR_estimator("ATT",data,"E","Y",cov)
  
  DR_ATE_10000[i]<-ATE_result$est
  DR_ATT_10000[i]<-ATT_result$est
  
}

result_check[2,]<-c(mean(DR_ATE_10000)-log(2),var(DR_ATE_10000),mean(DR_ATT_10000)-log(2),var(DR_ATT_10000))



######################################################################
### Version 3. #obs = 50000 ### 
#### Data generating ####
B_sample<-matrix(0,nrow=50000,ncol=100)
C_sample<-matrix(0,nrow=50000,ncol=100)
U_sample<-matrix(0,nrow=50000,ncol=100)
E_sample<-matrix(0,nrow=50000,ncol=100)
Y_sample<-matrix(0,nrow=50000,ncol=100)

effect<-c(log(1.2),log(1.5),log(2),log(2))

# 100th replication data generating
for(repl in 1:100){
  delta0<--2
  delta_b<-0.01
  delta_c<-0.01
  set.seed(123*repl)
  sample<-replicate(50000,expr={
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
write.csv(B_sample,file="B_sample_50000.csv",row.names=FALSE)
write.csv(C_sample,file="C_sample_50000.csv",row.names=FALSE)
write.csv(U_sample,file="U_sample_50000.csv",row.names=FALSE)
write.csv(E_sample,file="E_sample_50000.csv",row.names=FALSE)
write.csv(Y_sample,file="Y_sample_50000.csv",row.names=FALSE)

## Data import ##
B_sample<-read.csv("B_sample_50000.csv",header=TRUE)
C_sample<-read.csv("C_sample_50000.csv",header=TRUE)
#U_sample<-read.csv("U_sample.csv",header=TRUE)
E_sample<-read.csv("E_sample_50000.csv",header=TRUE)
Y_sample<-read.csv("Y_sample_50000.csv",header=TRUE)

DR_ATE_50000<-c()
DR_ATT_50000<-c()

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
  ATE_result<-DR_estimator("ATE",data,"E","Y",cov)
  ATT_result<-DR_estimator("ATT",data,"E","Y",cov)
  
  DR_ATE_50000[i]<-ATE_result$est
  DR_ATT_50000[i]<-ATT_result$est
  
}

result_check[3,]<-c(mean(DR_ATE_50000)-log(2),var(DR_ATE_50000),mean(DR_ATT_50000)-log(2),var(DR_ATT_50000))



######################################################################
### Version 4. #obs = 100000 ### 
#### Data generating ####
B_sample<-matrix(0,nrow=100000,ncol=100)
C_sample<-matrix(0,nrow=100000,ncol=100)
U_sample<-matrix(0,nrow=100000,ncol=100)
E_sample<-matrix(0,nrow=100000,ncol=100)
Y_sample<-matrix(0,nrow=100000,ncol=100)

effect<-c(log(1.2),log(1.5),log(2),log(2))

# 100th replication data generating
for(repl in 1:100){
  delta0<--2
  delta_b<-0.01
  delta_c<-0.01
  set.seed(123*repl)
  sample<-replicate(100000,expr={
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
write.csv(B_sample,file="B_sample_100000.csv",row.names=FALSE)
write.csv(C_sample,file="C_sample_100000.csv",row.names=FALSE)
#write.csv(U_sample,file="U_sample_100000.csv",row.names=FALSE)
write.csv(E_sample,file="E_sample_100000.csv",row.names=FALSE)
write.csv(Y_sample,file="Y_sample_100000.csv",row.names=FALSE)

## Data import ##
B_sample<-read.csv("B_sample_100000.csv",header=TRUE)
C_sample<-read.csv("C_sample_100000.csv",header=TRUE)
#U_sample<-read.csv("U_sample.csv",header=TRUE)
E_sample<-read.csv("E_sample_100000.csv",header=TRUE)
Y_sample<-read.csv("Y_sample_100000.csv",header=TRUE)

DR_ATE_100000<-c()
DR_ATT_100000<-c()

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
  ATE_result<-DR_estimator("ATE",data,"E","Y",cov)
  ATT_result<-DR_estimator("ATT",data,"E","Y",cov)
  
  DR_ATE_100000[i]<-ATE_result$est
  DR_ATT_100000[i]<-ATT_result$est
  
}

result_check[4,]<-c(mean(DR_ATE_100000,na.rm=TRUE)-log(2),var(DR_ATE_100000,na.rm=TRUE),mean(DR_ATT_100000,na.rm=TRUE)-log(2),var(DR_ATT_100000,na.rm=TRUE))

