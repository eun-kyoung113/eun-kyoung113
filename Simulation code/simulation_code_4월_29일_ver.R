####### 4/29 update ver #######
### 1. weight make function update
### 2. generated weight & matched index -> plus column in data
### 3. balance check function rewrite (weight version & match version)
### 4. performance check



library(boot)
#library(tableone)
#library(survey)
library(Hmisc)
library(tidyverse)
library(Matching)

delta<-seq(-2,-1,by=0.01) # delta0 sequence
prevalence<-matrix(0,nrow=length(delta),ncol=1)

## prevalence calculate by changing delta0
for(i in 1:length(delta)){
  delta0<-delta[i]
  delta_b<-0.01
  delta_c<-0.01
  #B,C,U,E generating
  set.seed(123*i)
  exposure_hat<-replicate(100000,expr={
    #set.seed(delta0)
    B<-rbinom(1,1,prob=0.5)
    C<-rnorm(1,0,1)
    U<-runif(1)
    p_z<-inv.logit(delta0+delta_b*B+delta_c*C)
    E<-rbinom(1,1,prob=p_z)
    c(B,C,U,E)
  })
  prevalence[i,]<-mean(exposure_hat[4,])
}

# exposure ratio
prevalence
min(prevalence)

## Data generating
delta_rare<-delta[which(prevalence==min(prevalence))] # -2
delta_rare

# sample store
B_sample<-matrix(0,nrow=100,ncol=1000)
C_sample<-matrix(0,nrow=100,ncol=1000)
U_sample<-matrix(0,nrow=100,ncol=1000)
E_sample<-matrix(0,nrow=100,ncol=1000)
Y_sample<-matrix(0,nrow=100,ncol=1000)

effect<-c(log(1.2),log(1.5),log(2),log(2))

# 1000th replication data generating
for(repl in 1:1000){
  delta0<-delta_rare
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
#write.csv(U_sample,file="U_sample.csv",row.names=FALSE)
write.csv(E_sample,file="E_sample.csv",row.names=FALSE)
write.csv(Y_sample,file="Y_sample.csv",row.names=FALSE)

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
balance_check<-function(var_treat,var_cov,var_weight,data){
  balance_table<-matrix(0,nrow=length(var_cov),ncol=2)
  colnames(balance_table)<-c("before_aSD","after_aSD")
  rownames(balance_table)<-var_cov
  
  treat<-data[,var_treat]
  cov<-data[,var_cov]
  weight<-data[,var_weight]
  
  for(i in 1:length(cov)){
    var_cov<-cov[,i]
    std_var_cov = (var_cov - mean(var_cov))/sd(var_cov)
    simple_M1 = mean(std_var_cov[data[,var_treat]==1])
    simple_M0 = mean(std_var_cov[data[,var_treat]==0])
    simple_V1 = var(std_var_cov[data[,var_treat]==1])
    simple_V0 = var(std_var_cov[data[,var_treat]==0])
    wgted_M1 = Hmisc::wtd.mean(x=std_var_cov[data[,var_treat]==1],weights=weight[data[,var_treat]==1])
    wgted_M0 = Hmisc::wtd.mean(x=std_var_cov[data[,var_treat]==0],weights=weight[data[,var_treat]==0])
    wgted_V1 = Hmisc::wtd.var(x=std_var_cov[data[,var_treat]==1],weights=weight[data[,var_treat]==1])
    wgted_V0 = Hmisc::wtd.var(x=std_var_cov[data[,var_treat]==0],weights=weight[data[,var_treat]==0])
    
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
#mydata<-data_export("E",cov,data)
#head(mydata)

weight_result<-weight_make("E",cov,estimate='ATE',data=data)

# plus weight to column #
data$ps<-weight_result$ps
data$weight_untrimmed<-weight_result$untrimmed 
data$weight_trimmed<-weight_result$trimmed
head(data)

balance_untrimmed_data<-balance_check("E",cov,"weight_untrimmed",data)
print(balance_untrimmed_data)

balance_trimmed_data<-balance_check("E",cov,"weight_trimmed",data)
print(balance_trimmed_data)
########################################################################


#### IPTW ATE & ATT ####
result_ATE<-matrix(0,nrow=1000,ncol=4)
colnames(result_ATE)<-c("balance_check_untrimmed","balance_check_trimmed","before_truncation_ATE","after_truncation_ATE")
result_ATE<-as.data.frame(result_ATE)

result_ATT<-matrix(0,nrow=1000,ncol=4)
colnames(result_ATT)<-c("balance_check_untrimmed","balance_check_trimmed","before_truncation_ATE","after_truncation_ATE")
result_ATT<-as.data.frame(result_ATT)

## 1000th simulated ##
for(j in 1:1000){
  E<-E_sample[,j]
  B<-B_sample[,j]
  C<-C_sample[,j]
  #U<-U_sample[,j]
  Y<-Y_sample[,j]
  data<-data.frame("E"=E,"B"=B,"C"=C,"Y"=Y)
  cov<-c("B","C")
  
  weight_result_ATE<-weight_make("E",cov,estimate='ATE',data=data)
  weight_result_ATT<-weight_make("E",cov,estimate='ATT',data=data)
  
  # plus weight to column #
  data$ATE_ps<-weight_result_ATE$ps
  data$ATE_weight_untrimmed<-weight_result_ATE$untrimmed 
  data$ATE_weight_trimmed<-weight_result_ATE$trimmed

  data$ATT_ps<-weight_result_ATT$ps
  data$ATT_weight_untrimmed<-weight_result_ATT$untrimmed 
  data$ATT_weight_trimmed<-weight_result_ATT$trimmed
  
  balance_untrimmed_ATE<-balance_check("E",cov,"ATE_weight_untrimmed",data)
  result_ATE[j,1]<-ifelse(sum(as.numeric(balance_untrimmed_ATE[,2]<0.1))==2,"OK","Not okay")
  
  balance_trimmed_ATE<-balance_check("E",cov,"ATE_weight_trimmed",data)
  result_ATE[j,2]<-ifelse(sum(as.numeric(balance_trimmed_ATE[,2]<0.1))==2,"OK","Not okay")
  
  balance_untrimmed_ATT<-balance_check("E",cov,"ATT_weight_untrimmed",data)
  result_ATT[j,1]<-ifelse(sum(as.numeric(balance_untrimmed_ATT[,2]<0.1))==2,"OK","Not okay")
  
  balance_trimmed_ATT<-balance_check("E",cov,"ATT_weight_trimmed",data)
  result_ATT[j,2]<-ifelse(sum(as.numeric(balance_trimmed_ATT[,2]<0.1))==2,"OK","Not okay")
  
  result_ATE[j,3]<-lm(Y~E,data=data,weight=weight_result_ATE$untrimmed)$coef['E']
  result_ATE[j,4]<-lm(Y~E,data=data,weight=weight_result_ATE$trimmed)$coef['E']
  
  result_ATT[j,3]<-lm(Y~E,data=data,weight=weight_result_ATT$untrimmed)$coef['E']
  result_ATT[j,4]<-lm(Y~E,data=data,weight=weight_result_ATT$trimmed)$coef['E']
  
}
head(result_ATE)
head(result_ATT)

table(result_ATE$balance_check_untrimmed)
table(result_ATE$balance_check_trimmed)

table(result_ATT$balance_check_untrimmed)
table(result_ATT$balance_check_trimmed)

mean(result_ATE[,3])
mean(result_ATE[,4])
mean(result_ATT[,3])
mean(result_ATT[,4])
log(2)

# plotting - ATE #
par(mfrow=c(2,1))
plot(result_ATE[,3],ylab="",main="ATE estimators using IPTW")
abline(h=log(2),col='red')
plot(result_ATE[,4],ylab="",main="ATE estimators using IPTW_truncated version")
abline(h=log(2),col='red')

# plotting - ATT #
par(mfrow=c(2,1))
plot(result_ATT[,3],ylab="",main="ATT estimators using IPTW")
abline(h=log(2),col='red')
plot(result_ATT[,4],ylab="",main="ATT estimators using IPTW_truncated version")
abline(h=log(2),col='red')



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
  matched<-unlist(m.out[c("index.treated","index.control")])
  return(matched)
}


## matched data balance check function ##
matched_balance<-function(var_treat,var_cov,matched_index,data){
  balance_table<-matrix(0,nrow=length(var_cov),ncol=2)
  colnames(balance_table)<-c("before_aSD","after_aSD")
  rownames(balance_table)<-var_cov
  
  matched_data<-data[matched_index,]
  data_cov<-data[,cov]
  match_cov<-matched_data[,cov]
  
  for(i in 1:length(cov)){
    cov_dt<-data_cov[,i]
    cov_match<-match_cov[,i]
    
    std_cov_dt = (cov_dt - mean(cov_dt))/sd(cov_dt)
    simple_M1 = mean(std_cov_dt[data[,var_treat]==1])
    simple_M0 = mean(std_cov_dt[data[,var_treat]==0])
    simple_V1 = var(std_cov_dt[data[,var_treat]==1])
    simple_V0 = var(std_cov_dt[data[,var_treat]==0])
    
    std_cov_match<-(cov_match - mean(cov_match))/sd(cov_match)
    matched_M1 = mean(std_cov_match[matched_data[,var_treat]==1])
    matched_M0 = mean(std_cov_match[matched_data[,var_treat]==0])
    matched_V1 = var(std_cov_match[matched_data[,var_treat]==1])
    matched_V0 = var(std_cov_match[matched_data[,var_treat]==0])
    
    ## aSD calculation ##
    before_aSD<-(simple_M1-simple_M0)/sqrt((simple_V1+simple_V0)/2)
    after_aSD<-(matched_M1-matched_M0)/sqrt((matched_V1+matched_V0)/2)
    balance_table[i,1]<-before_aSD
    balance_table[i,2]<-after_aSD
  }
  return(balance_table)
}
############################### check #######################################
E<-E_sample[,1]
B<-B_sample[,1]
#U<-U_sample[,1]
C<-C_sample[,1]
Y<-Y_sample[,1]
data<-data.frame("E"=E,"B"=B,"C"=C,"Y"=Y)
cov<-c("B","C")

# matched data index check #
matchindex_greedy<-matchedindex("E",cov,data=data,PSM=FALSE,M=1)
matchindex_PSM<-matchedindex("E",cov,data=data,PSM=TRUE,M=1)

# Plus column to data defining matched #         
data$matched_greedy<-0
data[matchindex_greedy,"matched_greedy"]<-1


data$matched_PSM<-0
data[matchindex_PSM,"matched_greedy"]<-1
head(data)

# check balance #
balance_greedy<-matched_balance("E",cov,matchindex_greedy,data)
print(balance_greedy)

balance_PSM<-matched_balance("E",cov,matchindex_PSM,data)
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
  
  # matched data generation #
  matchindex_greedy<-matchedindex("E",cov,data=data,PSM=FALSE,M=1)
  matchindex_PSM<-matchedindex("E",cov,data=data,PSM=TRUE,M=1)
  
  greedy_balance<-matched_balance("E",cov,matchindex_greedy,data)
  PSM_balance<-matched_balance("E",cov,matchindex_PSM,data)
  
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
ATE_regression<-c()

for(j in 1:1000){
  E<-E_sample[,j]
  B<-B_sample[,j]
  C<-C_sample[,j]
  #U<-U_sample[,j]
  Y<-Y_sample[,j]
  data<-data.frame("E"=E,"B"=B,"C"=C,"Y"=Y)
  cov<-c("B","C")
  
  #mydata<-data_export("E",cov,data)
  ATE_regression[j]<-lm(Y~.,data=data)$coef['E']
}

par(mfrow=c(1,1))
plot(ATE_regression,main="ATE estimators using regression")
abline(h=log(2),col='red')

###############################################################################
#### performance ####
performance_table<-matrix(0,nrow=7,ncol=2)
colnames(performance_table)<-c("Bias","Variance")
rownames(performance_table)<-c("IPTW_untrimmed_ATE","IPTW_trimmed_ATE","IPTW_untrimmed_ATT",
                               "IPTW_trimmed_ATT","Match_PSM","Match_greedy","Regression_ATE")
true_value<-log(2)
performance_table

estimator_data<-as.data.frame(cbind(result_ATE[,3],result_ATE[,4],result_ATT[,3],result_ATT[,4],result_PSM[,2],result_greedy[,2],ATE_regression))
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


