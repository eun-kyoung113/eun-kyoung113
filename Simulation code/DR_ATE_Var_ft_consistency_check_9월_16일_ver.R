########### Simulation code 9/16 version ##############
####### Update #########
## 1. DR Sandwich Variance estimator consistency check
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

#effect<-c(log(1.2),log(1.5),log(2),log(2))
effect<-c(log(1.5),log(2),log(2))

# 1000th replication data generating
for(repl in 1:1000){
  delta0<--2
  delta_b<-0.01
  delta_c<-0.01
  set.seed(123*repl)
  sample<-replicate(100,expr={
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
  #X_sample<-cbind(B_sample[,repl],C_sample[,repl],U_sample[,repl],E_sample[,repl])
  X_sample<-cbind(B_sample[,repl],C_sample[,repl],E_sample[,repl])
  
  # Y random sampling - continuous
  Y_sample[,repl]<-as.matrix(X_sample)%*%effect+rnorm(nrow(X_sample),0,1)
  
  # Y_sample random sampling - binary                    
  #prob<-inv.logit(as.matrix(X_sample)%*%effect)
  #Y_sample_binary<-rbinom(n=nrow(X_sample), size=1, prob=prob)
}

## Data store ##
write.csv(B_sample,file="B_sample_consistency.csv",row.names=FALSE)
write.csv(C_sample,file="C_sample_consistency.csv",row.names=FALSE)
#write.csv(U_sample,file="U_sample.csv",row.names=FALSE)
write.csv(E_sample,file="E_sample_consistency.csv",row.names=FALSE)
write.csv(Y_sample,file="Y_sample_consistency.csv",row.names=FALSE)

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
#############################################################################

##################### DR ATT estimator consistency check ##########################
## Data import ##
B_sample<-read.csv("B_sample_consistency.csv",header=TRUE)
C_sample<-read.csv("C_sample_consistency.csv",header=TRUE)
#U_sample<-read.csv("U_sample.csv",header=TRUE)
E_sample<-read.csv("E_sample_consistency.csv",header=TRUE)
Y_sample<-read.csv("Y_sample_consistency.csv",header=TRUE)

################################################

#### 1-1.DR Estimator function -----------------------------------------------
DR_estimator<-function(estimate,data,var_treat,var_y,cov){
  
  result<-list()
  
  mydata<-data[c(var_treat,cov)]
  
  myformula<-as.formula(sprintf("%s~.",var_treat))
  fit<-glm(formula=myformula,data=mydata,family='binomial')
  result$fit<-fit
  
  ps<-fit$fitted.values
  
  myformula1<-as.formula(sprintf("%s~.",var_y))
  
  #ind_mu0<-which(data[,var_treat]==0)
  #mu0_df<-data[ind_mu1,c(var_y,cov)]
  
  out_model<-lm(formula=myformula1,data=data)
  new_dt_mu0<-model.matrix(out_model)
  new_dt_mu0[,which(colnames(new_dt_mu0)==var_treat)]<-rep(0,nrow(data))
  mu0_X<-coef(out_model)%*%t(new_dt_mu0)
  
  result$out_model<-out_model
  result$mu0_X<-mu0_X
  
  data$ps<-ps
  
  if(estimate=='ATE'){
    ind_mu1<-which(data[,var_treat]==1)
    #mu1_df<-data[ind_mu1,c(var_y,cov)]
    
    new_dt_mu1<-model.matrix(out_model)
    new_dt_mu1[,which(colnames(new_dt_mu0)==var_treat)]<-rep(1,nrow(data))
    
    mu1_X<-coef(out_model)%*%t(new_dt_mu1)
    result$mu1_X<-mu1_X
    
    mu1_dr<-mean(data[,var_treat]*data[,var_y]/data$ps - ((data[,var_treat]-data$ps)/data$ps)*(mu1_X))
    mu0_dr<-mean(mu0_X+((1-data[,var_treat])*(data[,var_y]-mu0_X))/(1-data$ps))
    
    result$mu1_dr<-mu1_dr
    result$est<-mu1_dr-mu0_dr
  }
  
  else if(estimate=='ATT'){
    ind_mu1<-which(data[,var_treat]==1)
    result$mu1_dr<-sum(data[,var_y]*data[,var_treat])/length(ind_mu1)
    result$est<-sum(data[,var_y]*data[,var_treat]-(((data[,var_y]*(1-data[,var_treat])*data$ps)+(mu0_X)*(data[,var_treat]-data$ps))/(1-data$ps)))/length(ind_mu1) 
  }
  
  return(result)
}


###########################################################################
##### 1-2. Naive variance of DR estimator -------------------------------------
DR_Naive_var_estimator<-function(estimate,data,var_treat,var_y,cov){
  result<-DR_estimator(estimate,data,var_treat,var_y,cov)
  
  tau_dr<-result$est
  fit<-result$fit
  
  if(estimate=="ATE"){
    tau_i<-data[,var_treat]*data[,var_y]/fit$fitted.values - 
      ((data[,var_treat]-fit$fitted.values)/fit$fitted.values)*(result$mu1_X)-
      result$mu0_X+((1-data[,var_treat])*(data[,var_y]-result$mu0_X))/(1-fit$fitted.values)
    
    var<-sum((tau_i-tau_dr)^2)/(nrow(data)^2)
  }
  
  else if(estimate=="ATT"){
    tau_i<-(data[,var_y]*data[,var_treat])
    -(((data[,var_y]*(1-data[,var_treat])*fit$fitted.values)+result$mu0_X*(data[,var_treat]-fit$fitted.values))/(1-fit$fitted.values))
    
    var<-sum((tau_i-tau_dr)^2)/(length(data[,var_treat]==1)^2)
  }
  
  
  return(var)
  
}


#############################################################################
### 1-3. Sandwich robust variance estimator of DR Estimator -----------------
sandwich_var_DR<-function(estimate,data,var_treat,var_y,cov){
  
  result<-DR_estimator(estimate,data,var_treat,var_y,cov)
  
  X_1<-as.matrix(cbind(1,1,data[,cov]))
  X_0<-as.matrix(cbind(1,0,data[,cov]))
  
  if(estimate=="ATE"){
    fit<-result$fit
    exp_beta_X<-fit$fitted.values / (1-fit$fitted.values)
    
    dim1<-length(coef(result$out_model))+length(coef(fit))+2
    nu_theta_array<-array(0,dim=c(dim1,dim1,nrow(data)))
    
    #### partial derivative ####
    for(i in 1:nrow(data)){
      nu1_3<-matrix(((data[,var_treat][i]*(1/fit$fitted.values[i]))-1)*X_1[i,],nrow=1)         
      nu1_4<-matrix(as.vector(2*(data[,var_treat][i]*(data[,var_y][i]- coef(result$out_model)%*%X_1[i,]))/exp_beta_X[i])*model.matrix(fit)[i,],nrow=1)
      nu1<-cbind(1,0,nu1_3,nu1_4)
      colnames(nu1)<-NULL
      
      nu2_3<-matrix(((1+exp_beta_X[i])*data[,var_treat][i]-exp_beta_X[i])*X_0[i,],nrow=1)
      nu2_4<- -as.vector(exp_beta_X[i]*((1-data[,var_treat][i])*(data[,var_y][i]+(coef(result$out_model)%*%X_0[i,]))))*model.matrix(fit)[i,]
      nu2<-cbind(0,1,matrix(nu2_3,nrow=1),matrix(nu2_4,nrow=1))
      colnames(nu2)<-NULL
      
      nu3_3<--model.matrix(result$out_model)[i,]%*%t(model.matrix(result$out_model)[i,])
      nu3<-cbind(0,0,nu3_3,
                 matrix(0,ncol=length(coef(fit)),nrow=dim(t(model.matrix(result$out_model))%*%model.matrix(result$out_model))[1]))
      colnames(nu3)<-NULL
      
      nu4_4<- -(fit$fitted.values[i])*(model.matrix(fit)[i,]%*%t(model.matrix(fit)[i,]))
      nu4<-cbind(0,0,matrix(0,ncol=length(coef(result$out_model)),nrow=dim(nu4_4)[1]),
                 nu4_4)
      colnames(nu4)<-NULL
      
      nu_theta<-rbind(nu1,nu2,nu3,nu4)
      nu_theta_array[,,i]<-nu_theta
      
    }
    nu_theta_total<-apply(nu_theta_array,c(1,2),sum)
    
    ######
    
    ### pi-hat ###
    dim1<-length(coef(result$out_model))+length(coef(fit))+2
    nu_hat_array<-array(0,dim=c(dim1,1,nrow(data)))
    
    for(j in 1:nrow(data)){
      
      nu1_hat<-result$mu1_dr-((data[,var_treat][j]*data[,var_y][j]-(data[,var_treat][j]-fit$fitted.values[j])*(coef(result$out_model)%*%X_1[j,]))/fit$fitted.values[j])[1]
      nu2_hat<-(result$mu1_dr-result$est)-(((1-data[,var_treat][j])*data[,var_y][j]-(data[,var_treat][j]-fit$fitted.values[j])*(coef(result$out_model)%*%X_0[j,]))/(1-fit$fitted.values[j]))[1]
      nu3_hat<-as.matrix((result$out_model$residuals[j])*model.matrix(result$out_model)[j,],ncol=1)
      nu4_hat<-as.matrix(fit$fitted.values[j]*model.matrix(fit)[j,],ncol=1)
      
      nu_hat<-as.matrix(rbind(nu1_hat,nu2_hat,nu3_hat,nu4_hat))
      nu_hat_array[,,j]<-nu_hat
      
    }
    nu_hat_total<-apply(nu_hat_array,c(1,2),sum)
    
    var_middle<-nu_hat_total%*%t(nu_hat_total)
    
    cov_var_matrix<-solve(nu_theta_total)%*%var_middle%*%t(solve(nu_theta_total))
    var_est<-(cov_var_matrix[1,1]+cov_var_matrix[2,2]-2*cov_var_matrix[1,2])
    
  }
  return(var_est)
}




###### 1-3. Consistency check of Variance estimator of DR estimator ----------------------------------
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
## ATT는 package가 존재하지 않음. ##
#############################################################


DR_ATE<-DR_estimator("ATE",data,"E","Y",cov)
sandwich_var_DR("ATE",data,"E","Y",cov)
  
true_ps<-inv.logit(-2 + 0.01*data$B + 0.01*data$C)
est_1<-DR_ATE$mu1_dr-((data$E*data$Y-(data$E-true_ps)*(log(1.5)*data$B + log(2)*data$C + log(2)))/true_ps)
est_2<-(DR_ATE$mu1_dr-DR_ATE$est)-(((1-data$E)*data$Y-(data$E-true_ps)*(log(1.5)*data$B + log(2)*data$C))/(1-true_ps))
est_3<-model.matrix(DR_ATE$out_model)*DR_ATE$out_model$residuals

fit<-DR_ATE$fit
est_4<-(data$E-fit$fitted.values)*model.matrix(fit)

estimating_equation_hat <- as.matrix(rbind(est_1,est_2,t(est_3),t(est_4)))

#apply(estimating_equation_hat,1,sum)
#        est_1         est_2   (Intercept)             E             B             C   (Intercept)             B             C 
#-2.769686e+01 -2.395313e+00  6.383782e-16  1.249001e-15  5.259682e-15 -3.457304e-15 -6.353792e-12 -3.246653e-12  7.200771e-12  






