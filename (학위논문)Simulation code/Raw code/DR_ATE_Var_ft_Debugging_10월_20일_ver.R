### 1. import library ----------------------------------------------------------
library(boot)
library(tableone)
library(survey)
library(Hmisc)
library(geepack)
library(geex)
library(drgee)


### 2. Hardcoding function made by Me ---------------------------------------
### 2-1) DR Estimation -----------------------------------------------------
### 2-1-1) DR Estimator function -----------------------------------------------
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


### -------------------------------------------------------------------------



### 2-1-2) Sandwich robust variance estimator of DR Estimator -----------------
sandwich_var_DR<-function(estimate,data,var_treat,var_y,cov){
  
  result<-DR_estimator(estimate,data,var_treat,var_y,cov)
  fit<-result$fit
  
  dim1<-length(coef(result$out_model))+length(coef(fit))+2
  nu_theta_array<-array(0,dim=c(dim1,dim1,nrow(data)))
  nu_hat_array<-array(0,dim=c(dim1,dim1,nrow(data)))
  
  X_1<-as.matrix(cbind(1,1,data[,cov]))
  X_0<-as.matrix(cbind(1,0,data[,cov]))
  
  if(estimate=="ATE"){
    exp_beta_X<-fit$fitted.values / (1-fit$fitted.values)
    
    #### partial derivative ####
    for(i in 1:nrow(data)){
      nu1_3<-matrix(((data[,var_treat][i]*(1/fit$fitted.values[i]))-1)*X_1[i,],nrow=1)         
      nu1_4<-matrix((-1)*as.vector((data[,var_treat][i]*(data[,var_y][i]- coef(result$out_model)%*%X_1[i,]))/exp_beta_X[i])*model.matrix(fit)[i,],nrow=1)
      nu1<-cbind(1,0,nu1_3,nu1_4)
      colnames(nu1)<-NULL
      
      nu2_3<-matrix(((-(1+exp_beta_X[i]))*data[,var_treat][i]+exp_beta_X[i])*X_0[i,],nrow=1)
      nu2_4<- as.vector((-1)*exp_beta_X[i]*((1-data[,var_treat][i])*(data[,var_y][i]-(coef(result$out_model)%*%X_0[i,]))))*model.matrix(fit)[i,]
      nu2<-cbind(0,1,matrix(nu2_3,nrow=1),matrix(nu2_4,nrow=1))
      colnames(nu2)<-NULL
      
      nu3_3<--model.matrix(result$out_model)[i,]%*%t(model.matrix(result$out_model)[i,])
      nu3<-cbind(0,0,nu3_3,
                 matrix(0,ncol=length(coef(fit)),nrow=dim(t(model.matrix(result$out_model))%*%model.matrix(result$out_model))[1]))
      colnames(nu3)<-NULL
      
      nu4_4<- -((fit$fitted.values[i])*(1-fit$fitted.values[i]))*(model.matrix(fit)[i,]%*%t(model.matrix(fit)[i,]))
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
    x_beta<-coef(fit)%*%t(model.matrix(fit))
    
    for(j in 1:nrow(data)){
      
      nu1_hat<-result$mu1_dr-((data[,var_treat][j]*data[,var_y][j]-(data[,var_treat][j]-(exp(x_beta[j])/(1+exp(x_beta[j]))))*(coef(result$out_model)%*%X_1[j,]))/(exp(x_beta[j])/(1+exp(x_beta[j]))))[1]
      nu2_hat<-(result$mu1_dr-result$est)-(((1-data[,var_treat][j])*data[,var_y][j]+(data[,var_treat][j]-(exp(x_beta[j])/(1+exp(x_beta[j]))))*(coef(result$out_model)%*%X_0[j,]))/(1-(exp(x_beta[j])/(1+exp(x_beta[j])))))[1]
      nu3_hat<-as.matrix((result$out_model$residuals[j])*model.matrix(result$out_model)[j,],ncol=1)
      nu4_hat<-as.matrix((exp(x_beta[j])/(1+exp(x_beta[j])))*model.matrix(fit)[j,],ncol=1)
      
      nu_hat<-as.matrix(rbind(nu1_hat,nu2_hat,nu3_hat,nu4_hat))
      nu_hat_array[,,j]<-nu_hat
      
    }
    nu_hat_total<-apply(nu_hat_array,c(1,2),sum)
    
    var_middle<-nu_hat_total%*%t(nu_hat_total)
    
    cov_var_matrix<-solve(nu_theta_total)%*%var_middle%*%t(solve(nu_theta_total))
    var_est<-(cov_var_matrix[1,1]+cov_var_matrix[2,2]-2*cov_var_matrix[1,2])
    
  }
  
  
  else if(estimate=="ATT"){
    exp_beta_X <- fit$fitted.values / (1-fit$fitted.values)
    
    #### partial derivative ####
    for(i in 1:nrow(data)){
      nu1_3<-matrix(rep(0,length(coef(result$out_model))),nrow=1)         
      nu1_4<-matrix(rep(0,length(coef(fit))),nrow=1)
      nu1<-cbind(1,0,nu1_3,nu1_4)
      colnames(nu1)<-NULL
      
      nu2_3<-(nrow(data)/sum(data[,var_treat]))*(matrix(((-(1+exp_beta_X[i]))*data[,var_treat][i]+exp_beta_X[i])*X_0[i,],nrow=1))
      nu2_4<-(nrow(data)/sum(data[,var_treat]))*(as.vector((-1)*exp_beta_X[i]*((1-data[,var_treat][i])*(data[,var_y][i]-(coef(result$out_model)%*%X_0[i,]))))*model.matrix(fit)[i,])
      nu2<-cbind(0,1,matrix(nu2_3,nrow=1),matrix(nu2_4,nrow=1))
      colnames(nu2)<-NULL
      
      nu3_3<--model.matrix(result$out_model)[i,]%*%t(model.matrix(result$out_model)[i,])
      nu3_4<-matrix(0,nrow=length(coef(result$out_model)),ncol=length(coef(fit)))
      nu3<-cbind(0,0,nu3_3,nu3_4)
      colnames(nu3)<-NULL
      
      nu4_3<-matrix(0,nrow=length(coef(fit)),ncol=length(coef(result$out_model)))
      nu4_4<- -((fit$fitted.values[i])*(1-fit$fitted.values[i]))*(model.matrix(fit)[i,]%*%t(model.matrix(fit)[i,]))
      nu4<-cbind(0,0,nu4_3,nu4_4)
      colnames(nu4)<-NULL
      
      nu_theta<-rbind(nu1,nu2,nu3,nu4)
      nu_theta_array[,,i]<-nu_theta
      
    }
    nu_theta_total<-apply(nu_theta_array,c(1,2),sum)
    
    ######
    
    ### theta-hat ###
    for(j in 1:nrow(data)){
      
      nu1_hat<-result$mu1_dr-((data[,var_y][j]*data[,var_treat][j])*(nrow(data)/sum(data[,var_treat])))
      nu2_hat<-(result$mu1_dr-result$est)-(nrow(data)/sum(data[,var_treat]))*(((1-data[,var_treat][j])*data[,var_y][j]*fit$fitted.values[j]+(coef(result$out_model)%*%X_0[j,])))/(1-fit$fitted.values[j])
      nu3_hat<-as.matrix((result$out_model$residuals[j])*model.matrix(result$out_model)[j,],ncol=1)
      nu4_hat<-as.matrix(fit$residuals[j]*model.matrix(fit)[j,],ncol=1)
      
      nu_hat<-as.matrix(rbind(nu1_hat,nu2_hat,nu3_hat,nu4_hat))
      nu_hat_array[,,j]<-nu_hat%*%t(nu_hat)
      
    }
    
    nu_hat_total<-apply(nu_hat_array,c(1,2),sum)
    
    cov_var_matrix<-solve(nu_theta_total)%*%nu_hat_total%*%solve(t(nu_theta_total))
    var_est<-(cov_var_matrix[1,1]+cov_var_matrix[2,2]-2*cov_var_matrix[1,2])
    
  }
  
  
  return(var_est)
}

### -------------------------------------------------------------------------


### 3. Debugging ------------------------------------------------------------

### 3-1) Data generation --------------------------------------------------
### - with # of obs = 100000 -----------------------------------------
B_monte<-matrix(0,nrow=100000,ncol=1)
C_monte<-matrix(0,nrow=100000,ncol=1)
U_monte<-matrix(0,nrow=100000,ncol=1)
E_monte<-matrix(0,nrow=100000,ncol=1)
Y_monte<-matrix(0,nrow=100000,ncol=1)

effect<-c(log(1.5),log(2),log(2))


### 1th replication data generating -----------------------------------------
delta0<--2
delta_b<-0.01
delta_c<-0.01
set.seed(123)
sample<-replicate(100000,expr={
  B<-rbinom(1,1,prob=0.5)
  C<-rnorm(1,0,1)
  p_z<-inv.logit(delta0+delta_b*B+delta_c*C)
  E<-rbinom(1,1,prob=p_z)
  c(B,C,E)
})

B_monte[,1]<-sample[1,]
C_monte[,1]<-sample[2,]
E_monte[,1]<-sample[3,]
X_monte<-cbind(B_monte[,1],C_monte[,1],E_monte[,1])

# Y random sampling - continuous
Y_monte[,1]<-as.matrix(X_monte)%*%effect+rnorm(nrow(X_monte),0,1)



### 3-2) true Expectation of crossprod of estimating_equation calculate --------------
### only nu1 part -----------------------------------------------------------
E<-E_monte[,1]
B<-B_monte[,1]
C<-C_monte[,1]
Y<-Y_monte[,1]
data<-data.frame("E"=E,"B"=B,"C"=C,"Y"=Y)
cov<-c("B","C")
cov_type<-c("binary","continuous")
var_treat<-"E"
var_y<-"Y"

X_1<-as.matrix(cbind(1,1,data[,cov]))
X_0<-as.matrix(cbind(1,0,data[,cov]))

true_effect <- c(log(2),log(1.5), log(2))
true_ps<-inv.logit(-2 + 0.01*data$B + 0.01*data$C)

nu_true_vec<-c()

for(j in 1:nrow(data)){
  
  est_1_true<-(log(1.5)*0.5 + log(2))-((data$E[j]*data$Y[j]-(data$E[j]-true_ps[j])*(as.vector(c(0, true_effect)) %*% X_1[j,]))/true_ps[j])
  nu_true_vec[j]<-(est_1_true)^2

}


### 3-3) Data generation : Comparison version --------------------------------------------------
### - with 1000 set which # of obs = 20000 -----------------------------------------
B_sample<-matrix(0,nrow=20000,ncol=1000)
C_sample<-matrix(0,nrow=20000,ncol=1000)
E_sample<-matrix(0,nrow=20000,ncol=1000)
Y_sample<-matrix(0,nrow=20000,ncol=1000)

effect<-c(log(1.5),log(2),log(2))

### 1000th replication data generating -----------------------------------------
for(repl in 1:1000){
  delta0<--2
  delta_b<-0.01
  delta_c<-0.01
  set.seed(123*repl)
  sample<-replicate(20000,expr={
    B<-rbinom(1,1,prob=0.5)
    C<-rnorm(1,0,1)
    p_z<-inv.logit(delta0+delta_b*B+delta_c*C)
    E<-rbinom(1,1,prob=p_z)
    c(B,C,E)
  })
  B_sample[,repl]<-sample[1,]
  C_sample[,repl]<-sample[2,]
  E_sample[,repl]<-sample[3,]
  X_sample<-cbind(B_sample[,repl],C_sample[,repl],E_sample[,repl])
  
  # Y random sampling - continuous
  Y_sample[,repl]<-as.matrix(X_sample)%*%effect+rnorm(nrow(X_sample),0,1)
  
}


### 3-3-1) Sample Variance of z_star_theta_true --------------
z_true <- matrix(0, nrow=1000, ncol=1)
true_effect <- c(log(2),log(1.5), log(2))

for(i in 1:1000){
  E<-E_sample[,i]
  B<-B_sample[,i]
  C<-C_sample[,i]
  Y<-Y_sample[,i]
  data<-data.frame("E"=E,"B"=B,"C"=C,"Y"=Y)
  cov<-c("B","C")
  cov_type<-c("binary","continuous")

  X_1<-as.matrix(cbind(1,1,data[,cov]))
  X_0<-as.matrix(cbind(1,0,data[,cov]))
  
  true_ps<-inv.logit(-2 + 0.01*data$B + 0.01*data$C)
  
  est_1_true<-(log(1.5)*0.5 + log(2))-((data$E*data$Y-(data$E-true_ps)*(as.vector(c(0, true_effect)) %*% t(X_1)))/true_ps)
  
  z_true[i,1]<-mean(est_1_true)
  
  
}

nu_theta<-var(z_true)*20000
abs(mean(nu_true_vec)-nu_theta)


### 3-4) J_theta true value & hat value approximation ------------------------------------------
### only nu0 part -----------------------------------------------------------

### about theta true --------------------------------------------------------
E<-E_monte[,1]
B<-B_monte[,1]
C<-C_monte[,1]
Y<-Y_monte[,1]
data<-data.frame("E"=E,"B"=B,"C"=C,"Y"=Y)
cov<-c("B","C")
cov_type<-c("binary","continuous")
var_treat<-"E"
var_y<-"Y"

X_1<-as.matrix(cbind(1,1,data[,cov]))
X_0<-as.matrix(cbind(1,0,data[,cov]))

true_effect <- c(0,log(2),log(1.5), log(2))
true_ps<-inv.logit(-2 + 0.01*data$B + 0.01*data$C)
exp_beta_X_true<-true_ps/(1-true_ps)
nu_true_array<-matrix(NA,nrow=nrow(data),ncol=9)

DR_ATE<-DR_estimator("ATE",data,"E","Y",cov)
fit<-DR_ATE$fit

for(j in 1:nrow(data)){
  
  nu2_3<-matrix(((-(1+exp_beta_X_true[j]))*data[,var_treat][j]+exp_beta_X_true[j])*X_0[j,],nrow=1)
  nu2_4<-(as.vector((-1)*exp_beta_X_true[j]*((1-data[,var_treat][j])*(data[,var_y][j]-(true_effect%*%X_0[j,]))))*model.matrix(fit)[j,])
  nu2<-cbind(0,1,matrix(nu2_3,nrow=1),matrix(nu2_4,nrow=1))
  colnames(nu2)<-NULL
  
  nu_true_array[j,]<-nu2
  
}

j_true<-apply(nu_true_array,2,mean)


### 3-4-1) Data generation --------------------------------------------------
### - # of obs = 10000 -----------------------------------------
B_sample<-matrix(0,nrow=20000,ncol=1)
C_sample<-matrix(0,nrow=20000,ncol=1)
U_sample<-matrix(0,nrow=20000,ncol=1)
E_sample<-matrix(0,nrow=20000,ncol=1)
Y_sample<-matrix(0,nrow=20000,ncol=1)

effect<-c(log(1.5),log(2),log(2))

### 1th replication data generating -----------------------------------------
delta0<--2
delta_b<-0.01
delta_c<-0.01
set.seed(123)

sample<-replicate(20000,expr={
  B<-rbinom(1,1,prob=0.5)
  C<-rnorm(1,0,1)
  p_z<-inv.logit(delta0+delta_b*B+delta_c*C)
  E<-rbinom(1,1,prob=p_z)
  c(B,C,E)
})

B_sample[,1]<-sample[1,]
C_sample[,1]<-sample[2,]
E_sample[,1]<-sample[3,]
X_sample<-cbind(B_sample[,1],C_sample[,1],E_sample[,1])

# Y random sampling - continuous
Y_sample[,1]<-as.matrix(X_sample)%*%effect+rnorm(nrow(X_sample),0,1)


### 4-1-4) check --------------
theta_true<-matrix(c(log(1.5)*0.5 + log(2), log(2), 0, log(2), log(1.5), log(2), -2, 0.01, 0.01),ncol=1)
E<-E_monte[,1]
B<-B_monte[,1]
C<-C_monte[,1]
Y<-Y_monte[,1]
data_monte<-data.frame("E"=E,"B"=B,"C"=C,"Y"=Y)
cov<-c("B","C")

X_1_monte<-as.matrix(cbind(1,1,data_monte[,cov]))
X_0_monte<-as.matrix(cbind(1,0,data_monte[,cov]))

E<-E_sample[,1]
B<-B_sample[,1]
C<-C_sample[,1]
Y<-Y_sample[,1]
data<-data.frame("E"=E,"B"=B,"C"=C,"Y"=Y)
cov<-c("B","C")

X_1<-as.matrix(cbind(1,1,data[,cov]))
X_0<-as.matrix(cbind(1,0,data[,cov]))

DR_ATE<-DR_estimator("ATE",data,"E","Y",cov)
fit<-DR_ATE$fit

true_ps<-inv.logit(-2 + 0.01*data_monte$B + 0.01*data_monte$C)
theta_hat<-matrix(c(DR_ATE$mu1_dr, DR_ATE$mu1_dr-DR_ATE$est, coef(DR_ATE$out_model), coef(DR_ATE$fit)), ncol=1)


### J_theta true value & hat value approximation -----------------
#### about theta_hat ---------
nu_theta_array<-matrix(NA,nrow=nrow(data_monte),ncol=9)
theta_hat<-matrix(c(DR_ATE$mu1_dr, DR_ATE$mu1_dr-DR_ATE$est, coef(DR_ATE$out_model), coef(DR_ATE$fit)), ncol=1)
exp_beta_monte<-coef(fit)%*%t(as.matrix(cbind(1,data_monte[,cov])))

for(j in 1:nrow(data_monte)){
  
  nu2_3<-matrix((-(1+exp_beta_monte[,j])*data_monte[j,var_treat]+exp_beta_monte[,j])*X_0_monte[j,],nrow=1)
  nu2_4<-as.vector((-1)*exp_beta_monte[,j]*((1-data_monte[j,var_treat])*(data_monte[j,var_y]-(coef(DR_ATE$out_model)%*%X_0_monte[j,]))))*as.matrix(cbind(1,data_monte[j,cov]))
  nu2<-cbind(0,1,matrix(nu2_3,nrow=1),matrix(nu2_4,nrow=1))
  colnames(nu2)<-NULL
  
  nu_theta_array[j,]<-nu2
  
}

j_hat<-apply(nu_theta_array,2,mean)

### Estimating equation : theta_true --------------------------------------------------------------
true_effect <- c(log(2),log(1.5), log(2))
true_ps<-inv.logit(-2 + 0.01*data$B + 0.01*data$C)

est_2_true<-(log(1.5)*0.5)-(((1-data$E)*data$Y+(data$E-true_ps)*(as.vector(c(0, true_effect))%*%t(X_0)))/(1-true_ps))

one<-sum(est_2_true)/sqrt(nrow(data))

three<--sqrt(nrow(data))*j_true*(theta_hat-theta_true)
two<--sqrt(nrow(data))*j_hat*(theta_hat-theta_true) 

abs(one-two)
abs(one-three)

