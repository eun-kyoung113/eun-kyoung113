####### 5/6 update ver #######
### 1. balance check function rewrite (weight version & match version - combine ver.)
### 2. weighted mean / variance formula seperated by type of variable
### 3. IPTW ATE & ATT result table update : se / robust variance estimator
### 4. Regression ATE table update : se / robust variance estimator
### 5. confidence interval & converge list plus : except matching


library(boot)
#library(tableone)
library(survey)
library(Hmisc)
library(tidyverse)
library(Matching)
#library(geesmv)
library(clubSandwich)

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
  ps<-glm(formula=myformula,data=mydata,family='binomial')$fitted.values
  
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

## balance check function - function yes ##
#balance_check<-function(var_treat,var_cov,data){
#  require(tableone)
#  table<-CreateTableOne(vars=var_cov,strata=var_treat,data=data,test=FALSE)
#  print(table,smd=TRUE)
#}

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

weight_result<-weight_make("E",cov,estimate='ATE',data=data)

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
robust_var<-vcovCR(ATE_untrimmed,cluster=as.numeric(rownames(data)),type="CR0")
robust_var[2,2]
########################################################################


#### IPTW ATE & ATT ####
result_ATE_untrimmed<-matrix(0,nrow=1000,ncol=4)
colnames(result_ATE_untrimmed)<-c("balance_check_untrimmed","before_truncation_ATE_estimator",
                                  "SE_of_estimator","robust_variance_estimator")
result_ATE_untrimmed<-as.data.frame(result_ATE_untrimmed)

result_ATE_trimmed<-matrix(0,nrow=1000,ncol=4)
colnames(result_ATE_trimmed)<-c("balance_check_trimmed","after_truncation_ATE_estimator",
                                "SE_of_estimator","robust_variance_estimator")
result_ATE_trimmed<-as.data.frame(result_ATE_trimmed)

result_ATT_untrimmed<-matrix(0,nrow=1000,ncol=4)
colnames(result_ATT_untrimmed)<-c("balance_check_untrimmed","before_truncation_ATT_estimator",
                                  "SE_of_estimator","robust_variance_estimator")
result_ATT_untrimmed<-as.data.frame(result_ATT_untrimmed)

result_ATT_trimmed<-matrix(0,nrow=1000,ncol=4)
colnames(result_ATT_trimmed)<-c("balance_check_trimmed","after_truncation_ATT_estimator",
                                "SE_of_estimator","robust_variance_estimator")
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
  
  balance_untrimmed_ATE<-balance_check("E",cov,"ATE_weight_untrimmed",cov_type,data)
  result_ATE_untrimmed[j,1]<-ifelse(sum(as.numeric(balance_untrimmed_ATE[,2]<0.1))==2,"OK","Not okay")
  
  balance_trimmed_ATE<-balance_check("E",cov,"ATE_weight_trimmed",cov_type,data)
  result_ATE_trimmed[j,1]<-ifelse(sum(as.numeric(balance_trimmed_ATE[,2]<0.1))==2,"OK","Not okay")
  
  balance_untrimmed_ATT<-balance_check("E",cov,"ATT_weight_untrimmed",cov_type,data)
  result_ATT_untrimmed[j,1]<-ifelse(sum(as.numeric(balance_untrimmed_ATT[,2]<0.1))==2,"OK","Not okay")
  
  balance_trimmed_ATT<-balance_check("E",cov,"ATT_weight_trimmed",cov_type,data)
  result_ATT_trimmed[j,1]<-ifelse(sum(as.numeric(balance_trimmed_ATT[,2]<0.1))==2,"OK","Not okay")
  
  ATE_untrimmed<-lm(Y~E,data=data,weight=weight_result_ATE$untrimmed)
  dt1<-as.data.frame(summary(ATE_untrimmed)$coefficients)
  robust_var1<-vcovCR(ATE_untrimmed,cluster=as.numeric(rownames(data)),type="CR0")
  result_ATE_untrimmed[j,2]<-ATE_untrimmed$coefficients['E']
  result_ATE_untrimmed[j,3]<-dt1["E","Std. Error"]
  result_ATE_untrimmed[j,4]<-robust_var1[2,2]
  
  ATE_trimmed<-lm(Y~E,data=data,weight=weight_result_ATE$trimmed)
  dt2<-as.data.frame(summary(ATE_trimmed)$coefficients)
  robust_var2<-vcovCR(ATE_trimmed,cluster=as.numeric(rownames(data)),type="CR0")
  result_ATE_trimmed[j,2]<-ATE_trimmed$coefficients['E']
  result_ATE_trimmed[j,3]<-dt2["E","Std. Error"]
  result_ATE_trimmed[j,4]<-robust_var2[2,2]
  
  ATT_untrimmed<-lm(Y~E,data=data,weight=weight_result_ATT$untrimmed)
  dt3<-as.data.frame(summary(ATT_untrimmed)$coefficients)
  robust_var3<-vcovCR(ATT_untrimmed,cluster=as.numeric(rownames(data)),type="CR0")
  result_ATT_untrimmed[j,2]<-ATT_untrimmed$coefficients['E']
  result_ATT_untrimmed[j,3]<-dt3["E","Std. Error"]
  result_ATT_untrimmed[j,4]<-robust_var3[2,2]

  ATT_trimmed<-lm(Y~E,data=data,weight=weight_result_ATT$trimmed)
  dt4<-as.data.frame(summary(ATT_trimmed)$coefficients)
  robust_var4<-vcovCR(ATT_trimmed,cluster=as.numeric(rownames(data)),type="CR0")
  result_ATT_trimmed[j,2]<-ATT_trimmed$coefficients['E']
  result_ATT_trimmed[j,3]<-dt4["E","Std. Error"]
  result_ATT_trimmed[j,4]<-robust_var4[2,2]

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


### Confidence interval & converge probability ###
ATE_untrimmed_CL_Naive<-result_ATE_untrimmed[,2]-1.96*result_ATE_untrimmed[,3]
ATE_untrimmed_CU_Naive<-result_ATE_untrimmed[,2]+1.96*result_ATE_untrimmed[,3]
ATE_untrimmed_converge_Naive<-((ATE_untrimmed_CL_Naive<=log(2))&(ATE_untrimmed_CU_Naive>=log(2)))

ATE_untrimmed_CL_robust<-result_ATE_untrimmed[,2]-1.96*result_ATE_untrimmed[,4]
ATE_untrimmed_CU_robust<-result_ATE_untrimmed[,2]+1.96*result_ATE_untrimmed[,4]
ATE_untrimmed_converge_robust<-((ATE_untrimmed_CL_robust<=log(2))&(ATE_untrimmed_CU_robust>=log(2)))

ATE_trimmed_CL_Naive<-result_ATE_trimmed[,2]-1.96*result_ATE_trimmed[,3]
ATE_trimmed_CU_Naive<-result_ATE_trimmed[,2]+1.96*result_ATE_trimmed[,3]
ATE_trimmed_converge_Naive<-((ATE_trimmed_CL_Naive<=log(2))&(ATE_trimmed_CU_Naive>=log(2)))

ATE_trimmed_CL_robust<-result_ATE_trimmed[,2]-1.96*result_ATE_trimmed[,4]
ATE_trimmed_CU_robust<-result_ATE_trimmed[,2]+1.96*result_ATE_trimmed[,4]
ATE_trimmed_converge_robust<-((ATE_trimmed_CL_robust<=log(2))&(ATE_trimmed_CU_robust>=log(2)))

ATT_untrimmed_CL_Naive<-result_ATT_untrimmed[,2]-1.96*result_ATT_untrimmed[,3]
ATT_untrimmed_CU_Naive<-result_ATT_untrimmed[,2]+1.96*result_ATT_untrimmed[,3]
ATT_untrimmed_converge_Naive<-((ATT_untrimmed_CL_Naive<=log(2))&(ATT_untrimmed_CU_Naive>=log(2)))

ATT_untrimmed_CL_robust<-result_ATT_untrimmed[,2]-1.96*result_ATT_untrimmed[,4]
ATT_untrimmed_CU_robust<-result_ATT_untrimmed[,2]+1.96*result_ATT_untrimmed[,4]
ATT_untrimmed_converge_robust<-((ATT_untrimmed_CL_robust<=log(2))&(ATT_untrimmed_CU_robust>=log(2)))

ATT_trimmed_CL_Naive<-result_ATT_trimmed[,2]-1.96*result_ATT_trimmed[,3]
ATT_trimmed_CU_Naive<-result_ATT_trimmed[,2]+1.96*result_ATT_trimmed[,3]
ATT_trimmed_converge_Naive<-((ATT_trimmed_CL_Naive<=log(2))&(ATT_trimmed_CU_Naive>=log(2)))

ATT_trimmed_CL_robust<-result_ATT_trimmed[,2]-1.96*result_ATT_trimmed[,4]
ATT_trimmed_CU_robust<-result_ATT_trimmed[,2]+1.96*result_ATT_trimmed[,4]
ATT_trimmed_converge_robust<-((ATT_trimmed_CL_robust<=log(2))&(ATT_trimmed_CU_robust>=log(2)))


IPTW_Naive_variance_ATE<-list()
IPTW_Naive_variance_ATE$untrimmed<-data.frame("Naive_SE"=result_ATE_untrimmed[,3],"CL_Naive"=ATE_untrimmed_CL_Naive,
                                        "CU_Naive"=ATE_untrimmed_CU_Naive,"converge"=ATE_untrimmed_converge_Naive)

IPTW_Naive_variance_ATE$trimmed<-data.frame("Naive_SE"=result_ATE_trimmed[,3],"CL_Naive"=ATE_trimmed_CL_Naive,
                                         "CU_Naive"=ATE_trimmed_CU_Naive,"converge"=ATE_trimmed_converge_Naive)
IPTW_Naive_variance_ATE


IPTW_Robust_variance_ATE<-list()
IPTW_Robust_variance_ATE$untrimmed<-data.frame("Robust_SE"=result_ATE_untrimmed[,4],"CL_robust"=ATE_untrimmed_CL_robust,
                                         "CU_robust"=ATE_untrimmed_CU_robust,"converge"=ATE_untrimmed_converge_robust)

IPTW_Robust_variance_ATE$trimmed<-data.frame("Robust_SE"=result_ATE_trimmed[,4],"CL_robust"=ATE_trimmed_CL_robust,
                                       "CU_robust"=ATE_trimmed_CU_robust,"converge"=ATE_trimmed_converge_robust)
IPTW_Robust_variance_ATE


IPTW_Naive_variance_ATT<-list()
IPTW_Naive_variance_ATT$untrimmed<-data.frame("Naive_SE"=result_ATT_untrimmed[,3],"CL_Naive"=ATT_untrimmed_CL_Naive,
                                         "CU_Naive"=ATT_untrimmed_CU_Naive,"converge"=ATT_untrimmed_converge_Naive)

IPTW_Naive_variance_ATT$trimmed<-data.frame("Naive_SE"=result_ATT_trimmed[,3],"CL_Naive"=ATT_trimmed_CL_Naive,
                                       "CU_Naive"=ATT_trimmed_CU_Naive,"converge"=ATT_trimmed_converge_Naive)
IPTW_Naive_variance_ATT


IPTW_Robust_variance_ATT<-list()
IPTW_Robust_variance_ATT$untrimmed<-data.frame("Robust_SE"=result_ATT_untrimmed[,4],"CL_robust"=ATT_untrimmed_CL_robust,
                                          "CU_robust"=ATT_untrimmed_CU_robust,"converge"=ATT_untrimmed_converge_robust)

IPTW_Robust_variance_ATT$trimmed<-data.frame("Robust_SE"=result_ATT_trimmed[,4],"CL_robust"=ATT_trimmed_CL_robust,
                                        "CU_robust"=ATT_trimmed_CU_robust,"converge"=ATT_trimmed_converge_robust)
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
ATE_regression_CL_robust<-ATE_regression[,1]-1.96*ATE_regression[,2]
ATE_regression_CU_robust<-ATE_regression[,1]+1.96*ATE_regression[,2]
ATE_regression_converge_robust<-((ATE_regression_CL_robust<=log(2))&(ATE_regression_CU_robust>=log(2)))

ATE_regression_CL_Naive<-ATE_regression[,1]-1.96*ATE_regression[,3]
ATE_regression_CU_Naive<-ATE_regression[,1]+1.96*ATE_regression[,3]
ATE_regression_converge_Naive<-((ATE_regression_CL_Naive<=log(2))&(ATE_regression_CU_Naive>=log(2)))

regression_ATE_variance<-list()
regression_ATE_variance$Naive_var<-data.frame("Naive_SE"=ATE_regression[,2],"CL_Naive"=ATE_regression_CL_Naive,
                                              "CU_Naive"=ATE_regression_CU_Naive,"converge"=ATE_regression_converge_Naive)

regression_ATE_variance$robust_var<-data.frame("robust_SE"=ATE_regression[,3],"CL_robust"=ATE_regression_CL_robust,
                                              "CU_robust"=ATE_regression_CU_robust,"converge"=ATE_regression_converge_robust)
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

# Sensitivity analysis?

# error


