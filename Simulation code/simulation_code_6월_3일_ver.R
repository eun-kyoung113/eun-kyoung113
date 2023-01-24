#### 6/3 update ver ####
## 1. robust variance calculation function update - IPTW ATT





library(boot)
#library(tableone)
library(survey)
library(Hmisc)
library(tidyverse)
library(Matching)
#library(geesmv)
library(clubSandwich)
library(geex)
library(geepack)
#delta<-seq(-2,-1,by=0.01) # delta0 sequence
#prevalence<-matrix(0,nrow=length(delta),ncol=1)

## prevalence calculate by changing delta0
#for(i in 1:length(delta)){
#  delta0<-delta[i]
#  delta_b<-0.01
#  delta_c<-0.01
#  #B,C,U,E generating
#  set.seed(123*i)
#  exposure_hat<-replicate(100000,expr={
#    #set.seed(delta0)
#    B<-rbinom(1,1,prob=0.5)
#    C<-rnorm(1,0,1)
#    U<-runif(1)
#    p_z<-inv.logit(delta0+delta_b*B+delta_c*C)
#    E<-rbinom(1,1,prob=p_z)
#    c(B,C,U,E)
#  })
#  prevalence[i,]<-mean(exposure_hat[4,])
#}

# exposure ratio
#prevalence
#min(prevalence)

## Data generating
#delta_rare<-delta[which(prevalence==min(prevalence))] # -2
#delta_rare

# sample store
#B_sample<-matrix(0,nrow=100,ncol=1000)
#C_sample<-matrix(0,nrow=100,ncol=1000)
#U_sample<-matrix(0,nrow=100,ncol=1000)
#E_sample<-matrix(0,nrow=100,ncol=1000)
#Y_sample<-matrix(0,nrow=100,ncol=1000)

#effect<-c(log(1.2),log(1.5),log(2),log(2))

# 1000th replication data generating
#for(repl in 1:1000){
#  delta0<-delta_rare
#  delta_b<-0.01
#  delta_c<-0.01
#  set.seed(123*repl)
#  sample<-replicate(100,expr={
#    B<-rbinom(1,1,prob=0.5)
#    C<-rnorm(1,0,1)
#    U<-runif(1)
#    p_z<-inv.logit(delta0+delta_b*B+delta_c*C)
#    E<-rbinom(1,1,prob=p_z)
#    c(B,C,U,E)
#  })
#  B_sample[,repl]<-sample[1,]
#  C_sample[,repl]<-sample[2,]
#  U_sample[,repl]<-sample[3,]
#  E_sample[,repl]<-sample[4,]
#  X_sample<-cbind(B_sample[,repl],C_sample[,repl],U_sample[,repl],E_sample[,repl])

# Y random sampling - continuous
#  Y_sample[,repl]<-as.matrix(X_sample)%*%effect+rnorm(nrow(X_sample),0,1)

# Y_sample random sampling - binary                    
#prob<-inv.logit(as.matrix(X_sample)%*%effect)
#Y_sample_binary<-rbinom(n=nrow(X_sample), size=1, prob=prob)
#}

## Data store ##
#write.csv(B_sample,file="B_sample.csv",row.names=FALSE)
#write.csv(C_sample,file="C_sample.csv",row.names=FALSE)
#write.csv(U_sample,file="U_sample.csv",row.names=FALSE)
#write.csv(E_sample,file="E_sample.csv",row.names=FALSE)
#write.csv(Y_sample,file="Y_sample.csv",row.names=FALSE)

## Data import ##
B_sample<-read.csv("B_sample.csv",header=TRUE)
C_sample<-read.csv("C_sample.csv",header=TRUE)
#U_sample<-read.csv("U_sample.csv",header=TRUE)
E_sample<-read.csv("E_sample.csv",header=TRUE)
Y_sample<-read.csv("Y_sample.csv",header=TRUE)


## Data export function - treat + covariance ##
#data_export<-function(var_treat,var_cov,data){
#  name<-c(var_treat,var_cov)
#  idx_name<-rep(0,length(name))
#  
#  for(n in 1:length(name)){
#    idx_name[n]<-which(colnames(data)==name[n])
#  }
#  mydata<-data[idx_name]
#  return(mydata)
#}

##################################################################################
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

## weighted data export ##
#weighteddata<-function(data,weight){
#  require(survey)
#  data<-svydesign(ids=~1,data=data,weights=~weight)
#  mydata<-data[[7]]
#  return(mydata)
#}
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
################################################
## result writing function ##
result_table<-function(glm_obj,lm_obj,var_treat,var_weight,var_y,cov,cov_type,data,estimator){
  result_table<-rep(0,4)
  balance<-balance_check(var_treat,cov,var_weight,cov_type,data)
  result_table[1]<-ifelse(sum(as.numeric(balance[,2]<0.1))==2,"OK","Not okay")
  result_table[2]<-obj$coefficients[var_treat]
  dt<-as.data.frame(summary(obj)$coefficients)
  result_table[3]<-dt[var_treat,"Std. Error"]
  
  if(estimator=="ATE"){
    robust_var<-sandwich_var_ATE(lm_obj,data)
    result_table[4]<-sqrt(robust_var[2,2])
  }
  else if(estimator=="ATT"){
    robust_var<-sandwich_var_ATT(glm_obj,data,cov,var_treat,var_y)
    result_table[4]<-sqrt(robust_var)
  }
  return(result_table)
  
}
#################################################################################

######### check #############################################################
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

balance_untrimmed_data<-balance_check("E",cov,"weight_untrimmed",cov_type,data)
print(balance_untrimmed_data)

balance_trimmed_data<-balance_check("E",cov,"weight_trimmed",cov_type,data)
print(balance_trimmed_data)

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
########################################################################


#### IPTW ATE & ATT ####
result_ATE_untrimmed<-matrix(0,nrow=1000,ncol=4)
colnames(result_ATE_untrimmed)<-c("balance_check_untrimmed","before_truncation_ATE_estimator",
                                  "SE_of_estimator","robust_SE_estimator")
result_ATE_untrimmed<-as.data.frame(result_ATE_untrimmed)

result_ATE_trimmed<-matrix(0,nrow=1000,ncol=4)
colnames(result_ATE_trimmed)<-c("balance_check_trimmed","after_truncation_ATE_estimator",
                                "SE_of_estimator","robust_SE_estimator")
result_ATE_trimmed<-as.data.frame(result_ATE_trimmed)

result_ATT_untrimmed<-matrix(0,nrow=1000,ncol=4)
colnames(result_ATT_untrimmed)<-c("balance_check_untrimmed","before_truncation_ATT_estimator",
                                  "SE_of_estimator","robust_SE_estimator")
result_ATT_untrimmed<-as.data.frame(result_ATT_untrimmed)

result_ATT_trimmed<-matrix(0,nrow=1000,ncol=4)
colnames(result_ATT_trimmed)<-c("balance_check_trimmed","after_truncation_ATT_estimator",
                                "SE_of_estimator","robust_SE_estimator")
result_ATT_trimmed<-as.data.frame(result_ATT_trimmed)

## 1000th simulated ##
for(j in 1:1000){
  E<-E_sample[,j]
  B<-B_sample[,j]
  C<-C_sample[,j]
  #U<-U_sample[,j]
  Y<-Y_sample[,j]
  data<-data.frame("E"=E,"B"=B,"C"=C,"Y"=Y)
  cov<-c("B","C")
  cov_type<-c("binary","continuous")
  weight_result_ATE<-weight_make("E",cov,estimate='ATE',data=data)
  weight_result_ATT<-weight_make("E",cov,estimate='ATT',data=data)
  
  # plus weight to column #
  data$ATE_ps<-weight_result_ATE$ps
  data$ATE_weight_untrimmed<-weight_result_ATE$untrimmed 
  data$ATE_weight_trimmed<-weight_result_ATE$trimmed
  
  data$ATT_ps<-weight_result_ATT$ps
  data$ATT_weight_untrimmed<-weight_result_ATT$untrimmed 
  data$ATT_weight_trimmed<-weight_result_ATT$trimmed
  
  ATE_untrimmed<-lm(Y~E,data=data,weight=weight_result_ATE$untrimmed)
  result_ATE_untrimmed[j,]<-result_table(ATE_untrimmed,"E","ATE_weight_untrimmed",cov,cov_type,data)
  
  ATE_trimmed<-lm(Y~E,data=data,weight=weight_result_ATE$trimmed)
  result_ATE_trimmed[j,]<-result_table(ATE_trimmed,"E","ATE_weight_trimmed",cov,cov_type,data)
  
  ATT_untrimmed<-lm(Y~E,data=data,weight=weight_result_ATT$untrimmed)
  result_ATT_untrimmed[j,]<-result_table(ATT_untrimmed,"E","ATT_weight_untrimmed",cov,cov_type,data)
  
  ATT_trimmed<-lm(Y~E,data=data,weight=weight_result_ATT$trimmed)
  result_ATT_trimmed[j,]<-result_table(ATT_trimmed,"E","ATT_weight_trimmed",cov,cov_type,data)
  
}
head(result_ATE_untrimmed)
head(result_ATE_trimmed)
head(result_ATT_untrimmed)
head(result_ATT_trimmed)

# plotting - ATE #
par(mfrow=c(2,1))
plot(result_ATE_untrimmed[,2],ylab="",main="ATE estimators using IPTW")
abline(h=log(2),col='red')
plot(result_ATE_trimmed[,2],ylab="",main="ATE estimators using IPTW_truncated version")
abline(h=log(2),col='red')

# plotting - ATT #
par(mfrow=c(2,1))
plot(result_ATT_untrimmed[,2],ylab="",main="ATT estimators using IPTW")
abline(h=log(2),col='red')
plot(result_ATT_trimmed[,2],ylab="",main="ATT estimators using IPTW_truncated version")
abline(h=log(2),col='red')


### Confidence interval & coverage probability ###
CI_coverage_make<-function(result_table,var_estimator,var_SE,true_value){
  
  estimator<-result_table[,var_estimator]
  SE_estimator<-result_table[,var_SE]
  CL<-estimator-1.96*SE_estimator
  CU<-estimator+1.96*SE_estimator
  coverage<-((CL<=true_value)&(CU>=true_value))
  
  result<-data.frame("SE_estimator"=SE_estimator,"Confidence_lower"=CL,
                     "Confidence_upper"=CU,"coverage"=coverage)
  
  return(result)
  
}


IPTW_Naive_variance_ATE<-list()
IPTW_Naive_variance_ATE$untrimmed<-CI_coverage_make(result_ATE_untrimmed,"before_truncation_ATE_estimator","SE_of_estimator",log(2))

IPTW_Naive_variance_ATE$trimmed<-CI_coverage_make(result_ATE_trimmed,"after_truncation_ATE_estimator","SE_of_estimator",log(2))
IPTW_Naive_variance_ATE


IPTW_Robust_variance_ATE<-list()
IPTW_Robust_variance_ATE$untrimmed<-CI_coverage_make(result_ATE_untrimmed,"before_truncation_ATE_estimator","robust_SE_estimator",log(2))

IPTW_Robust_variance_ATE$trimmed<-CI_coverage_make(result_ATE_trimmed,"after_truncation_ATE_estimator","robust_SE_estimator",log(2))
IPTW_Robust_variance_ATE


IPTW_Naive_variance_ATT<-list()
IPTW_Naive_variance_ATT$untrimmed<-CI_coverage_make(result_ATT_untrimmed,"before_truncation_ATT_estimator","SE_of_estimator",log(2))

IPTW_Naive_variance_ATT$trimmed<-CI_coverage_make(result_ATT_trimmed,"after_truncation_ATT_estimator","SE_of_estimator",log(2))
IPTW_Naive_variance_ATT


IPTW_Robust_variance_ATT<-list()
IPTW_Robust_variance_ATT$untrimmed<-CI_coverage_make(result_ATT_untrimmed,"before_truncation_ATE_estimator","robust_SE_estimator",log(2))

IPTW_Robust_variance_ATT$trimmed<-CI_coverage_make(result_ATT_trimmed,"after_truncation_ATE_estimator","robust_SE_estimator",log(2))
IPTW_Robust_variance_ATT

################################################################################
## matched index export ##
matchedindex<-function(var_treat,var_cov,data,PSM,M){
  require(Matching)
  if(PSM==TRUE){
    mydata<-data[c(var_treat,var_cov)]
    myformula<-as.formula(sprintf("%s~.",var_treat))
    ps<-glm(formula=myformula,data=mydata,family='binomial')$fitted.values
    m.out<-Match(Tr=data[,var_treat],M=M,X=ps,replace=FALSE)
  }
  else{
    m.out<-Match(Tr=data[,var_treat],M=M,X=data[,var_cov],replace=FALSE)
  }
  matched<-as.data.frame(cbind(m.out$index.treated,m.out$index.control))
  colnames(matched)<-c("treated",'control')
  return(matched)
}

############################### check #######################################
E<-E_sample[,1]
B<-B_sample[,1]
#U<-U_sample[,1]
C<-C_sample[,1]
Y<-Y_sample[,1]
data<-data.frame("E"=E,"B"=B,"C"=C,"Y"=Y)
cov<-c("B","C")
cov_type<-c("binary","continuous")

# matched data index check #
matchindex_greedy<-matchedindex("E",cov,data=data,PSM=FALSE,M=1)
matchindex_PSM<-matchedindex("E",cov,data=data,PSM=TRUE,M=1)

# Plus column to data defining matched #         
data$matched_greedy<-0
data$paired_number_greedy<-NA
data[c(matchindex_greedy$treated,matchindex_greedy$control),"matched_greedy"]<-1
data[c(matchindex_greedy$treated,matchindex_greedy$control),"paired_number_greedy"]<-rep(c(1:nrow(matchindex_greedy)),2)

data$matched_PSM<-0
data$paired_number_PSM<-NA
data[c(matchindex_PSM$treated,matchindex_PSM$control),"matched_PSM"]<-1
data[c(matchindex_PSM$treated,matchindex_PSM$control),"paired_number_PSM"]<-rep(c(1:nrow(matchindex_PSM)),2)
head(data)

# check balance #
balance_greedy<-balance_check("E",cov,"matched_greedy",cov_type,data)
print(balance_greedy)

balance_PSM<-balance_check("E",cov,"matched_PSM",cov_type,data)
print(balance_PSM)
################################################################################

#### PSM matching & greedy matching ####
result_PSM<-matrix(0,nrow=1000,ncol=2)
colnames(result_PSM)<-c("balance_check","ATE_PSM")

result_greedy<-matrix(0,nrow=1000,ncol=2)
colnames(result_greedy)<-c("balance_check","ATE_greedy")


## 1000th simulated ##
for(j in 1:1000){
  E<-E_sample[,j]
  B<-B_sample[,j]
  C<-C_sample[,j]
  #U<-U_sample[,j]
  Y<-Y_sample[,j]
  data<-data.frame("E"=E,"B"=B,"C"=C,"Y"=Y)
  cov<-c("B","C")
  cov_type<-c("binary","continuous")
  
  # matched data generation #
  matchindex_greedy<-matchedindex("E",cov,data=data,PSM=FALSE,M=1)
  matchindex_PSM<-matchedindex("E",cov,data=data,PSM=TRUE,M=1)
  
  data$matched_greedy<-0
  data[matchindex_greedy,"matched_greedy"]<-1
  
  data$matched_PSM<-0
  data[matchindex_PSM,"matched_PSM"]<-1
  
  greedy_balance<-balance_check("E",cov,"matched_greedy",cov_type,data)
  PSM_balance<-balance_check("E",cov,"matched_PSM",cov_type,data)
  
  matchdata_greedy<-data[matchindex_greedy,]
  matchdata_PSM<-data[matchindex_PSM,]
  
  result_PSM[j,1]<-ifelse(sum(as.numeric(PSM_balance[,2]<0.1))==2,"OK","Not okay")
  result_PSM[j,2]<-mean(matchdata_PSM$Y[matchdata_PSM$E==1])-mean(matchdata_PSM$Y[matchdata_PSM$E==0])
  
  result_greedy[j,1]<-ifelse(sum(as.numeric(greedy_balance[,2]<0.1))==2,"OK","Not okay")
  result_greedy[j,2]<-mean(matchdata_greedy$Y[matchdata_greedy$E==1])-mean(matchdata_greedy$Y[matchdata_greedy$E==0])
  
}
head(result_PSM)
head(result_greedy)

plot(result_PSM[,2],main="ATE estimators using PSM",ylab="")
abline(h=log(2),col='red')

plot(result_greedy[,2],main="ATE estimators using greedy matching",ylab="")
abline(h=log(2),col='red')

###############################################################################
#### Regression ####
ATE_regression<-matrix(0,nrow=1000,ncol=3)
colnames(ATE_regression)<-c("ATE_estimator","SE_of_estimator","robust_variance_estimator")

for(j in 1:1000){
  E<-E_sample[,j]
  B<-B_sample[,j]
  C<-C_sample[,j]
  #U<-U_sample[,j]
  Y<-Y_sample[,j]
  data<-data.frame("E"=E,"B"=B,"C"=C,"Y"=Y)
  cov<-c("B","C")
  
  #mydata<-data_export("E",cov,data)
  reg_fit<-lm(Y~.,data=data)
  dt7<-as.data.frame(summary(reg_fit)$coefficients)
  robust_var7<-vcovCR(reg_fit,cluster=as.numeric(rownames(data)),type="CR0")
  ATE_regression[j,1]<-reg_fit$coefficients['E']
  ATE_regression[j,2]<-dt7["E","Std. Error"]
  ATE_regression[j,3]<-robust_var7[2,2]
}

par(mfrow=c(1,1))
plot(ATE_regression[,1],main="ATE estimators using regression")
abline(h=log(2),col='red')

## Confidence interval & converge ##
regression_ATE_variance<-list()
regression_ATE_variance$Naive_var<-CI_coverage_make(ATE_regression,"ATE_estimator","SE_of_estimator",log(2))

regression_ATE_variance$robust_var<-CI_coverage_make(ATE_regression,"ATE_estimator","robust_SE_estimator",log(2))
regression_ATE_variance
###############################################################################
#### performance ####
performance_table<-matrix(0,nrow=7,ncol=2)
colnames(performance_table)<-c("Bias","Variance")
rownames(performance_table)<-c("IPTW_untrimmed_ATE","IPTW_trimmed_ATE","IPTW_untrimmed_ATT",
                               "IPTW_trimmed_ATT","Match_PSM","Match_greedy","Regression_ATE")
true_value<-log(2)
performance_table

estimator_data<-as.data.frame(cbind(result_ATE_untrimmed[,2],result_ATE_trimmed[,2],result_ATT_untrimmed[,2],
                                    result_ATT_trimmed[,2],result_PSM[,2],result_greedy[,2],ATE_regression[,1]))
colnames(estimator_data)<-c("IPTW_untrimmed_ATE","IPTW_trimmed_ATE","IPTW_untrimmed_ATT",
                            "IPTW_trimmed_ATT","Match_PSM","Match_greedy","Regression_ATE")
head(estimator_data)

for(i in 1:nrow(performance_table)){
  performance_table[i,1]<-mean(as.numeric(estimator_data[,i]))-true_value
  performance_table[i,2]<-var(as.numeric(estimator_data[,i]))
}

head(performance_table)


