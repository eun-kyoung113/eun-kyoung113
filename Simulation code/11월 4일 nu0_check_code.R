library(boot)
library(tableone)
library(survey)
library(Hmisc)
library(geepack)
library(geex)
library(drgee)


### -----------------------------------------------------
### DR Estimator function -----------------------------------------------
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
    result$mu0_dr<-mu0_dr
    #####
    
    #####
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






### Data generation --------------------------------------------------
### - with # of obs = 100000 -----------------------------------------
N1<-100000
effect<-c(log(1.5),log(2),log(2))


### 1th replication data generating -----------------------------------------
delta0<--2
delta_b<-0.01
delta_c<-0.01
set.seed(123)
sample<-replicate(N1,expr={
  B<-rbinom(1,1,prob=0.5)
  C<-rnorm(1,0,1)
  p_z<-inv.logit(delta0+delta_b*B+delta_c*C)
  E<-rbinom(1,1,prob=p_z)
  c(B,C,E)
})

X_sample<-cbind(sample[1,],sample[2,],sample[3,])

# Y random sampling - continuous
Y_sample<-as.matrix(X_sample)%*%effect+rnorm(nrow(X_sample),0,1)


### -------------------------------------------------------------------------
E<-X_sample[,3]
B<-X_sample[,1]
C<-X_sample[,2]
Y<-Y_sample[,1]
data_L<-data.frame("E"=E,"B"=B,"C"=C,"Y"=Y)
cov<-c("B","C")
cov_type<-c("binary","continuous")

var_treat<-"E"
var_y<-"Y"


### J_theta true value & hat value approximation ------------------------------------------
### Define J_theta function -----------------------------------------------------------
J_function<-function(theta,data,var_treat,var_y,cov){
  
  nu1<-theta[1]
  nu0<-theta[2]
  outcome_model_coef<-theta[3:(length(cov)+2+3-1)]
  ps_model_coef<-theta[(length(theta)-length(cov)):length(theta)]
  
  dim1<-length(theta)
  nu_theta_array<-array(NA,dim=c(dim1,dim1,nrow(data)))
  
  X_P<-as.matrix(cbind(1,data[,cov]))
  X_out<-as.matrix(cbind(1,data[,var_treat],data[,cov]))
  X_0<-as.matrix(cbind(1,0,data[,cov]))
  X_1<-as.matrix(cbind(1,1,data[,cov]))
  
  exp_beta_X<-exp(ps_model_coef%*%t(X_P))
  fitted_ps<-exp_beta_X/(1+exp_beta_X)
  
  for(i in 1:nrow(data)){
    
    nu1_3<-matrix(((data[,var_treat][i]*(1/fitted_ps[i]))-1)*X_1[i,],nrow=1)         
    nu1_4<-matrix((-1)*as.vector((data[,var_treat][i]*(data[,var_y][i]- outcome_model_coef%*%X_1[i,]))/exp_beta_X[i])*X_P[i,],nrow=1)
    nu1<-cbind(1,0,nu1_3,nu1_4)
    colnames(nu1)<-NULL
    
    nu2_3<-matrix(((-(1+exp_beta_X[i]))*data[,var_treat][i]+exp_beta_X[i])*X_0[i,],nrow=1)
    nu2_4<- as.vector((-1)*exp_beta_X[i]*((1-data[,var_treat][i])*(data[,var_y][i]-(outcome_model_coef%*%X_0[i,]))))*X_P[i,]
    nu2<-cbind(0,1,matrix(nu2_3,nrow=1),matrix(nu2_4,nrow=1))
    colnames(nu2)<-NULL
    
    nu3_3<--X_out[i,]%*%t(X_out[i,])
    nu3<-cbind(0,0,nu3_3,
               matrix(0,ncol=length(ps_model_coef),nrow=dim(t(X_out)%*%X_out)[1]))
    colnames(nu3)<-NULL
    
    nu4_4<- -((fitted_ps[i])*(1-fitted_ps[i]))*(X_P[i,]%*%t(X_P[i,]))
    nu4<-cbind(0,0,matrix(0,ncol=length(outcome_model_coef),nrow=dim(nu4_4)[1]),
               nu4_4)
    colnames(nu4)<-NULL
    
    nu_theta<-rbind(nu1,nu2,nu3,nu4)
    nu_theta_array[,,i]<-nu_theta
    
  }
  
  j_theta<-apply(nu_theta_array,c(1,2),mean) 
  
  return(j_theta)
}

### J theta true calculate --------------------------------------------------------
theta_true<-matrix(c(log(1.5)*0.5 + log(2), log(1.5)*0.5, 0, log(2), log(1.5), log(2), -2, 0.01, 0.01),nrow=1)
j_true<-J_function(theta_true,data_L,var_treat,var_y,cov)


### Data generation --------------------------------------------------
### - # of obs = 10000 -----------------------------------------
N2<-10000
effect<-c(log(1.5),log(2),log(2))


### 1th replication data generating -----------------------------------------
delta0<--2
delta_b<-0.01
delta_c<-0.01
set.seed(123)

sample<-replicate(N2,expr={
  B<-rbinom(1,1,prob=0.5)
  C<-rnorm(1,0,1)
  p_z<-inv.logit(delta0+delta_b*B+delta_c*C)
  E<-rbinom(1,1,prob=p_z)
  c(B,C,E)
})

X_sample<-cbind(sample[1,],sample[2,],sample[3,])

# Y random sampling - continuous
Y_sample<-as.matrix(X_sample)%*%effect+rnorm(nrow(X_sample),0,1)


### check -----------------------------------------------------------------------
E<-X_sample[,3]
B<-X_sample[,1]
C<-X_sample[,2]
Y<-Y_sample[,1]
data<-data.frame("E"=E,"B"=B,"C"=C,"Y"=Y)
cov<-c("B","C")
cov_type<-c("binary","continuous")
var_treat<-"E"
var_y<-"Y"

X_1<-as.matrix(cbind(1,1,data[,cov]))
X_0<-as.matrix(cbind(1,0,data[,cov]))

DR_ATE<-DR_estimator("ATE",data,"E","Y",cov)
fit<-DR_ATE$fit


theta_hat<-matrix(c(DR_ATE$mu1_dr, DR_ATE$mu1_dr-DR_ATE$est, coef(DR_ATE$out_model), coef(DR_ATE$fit)), nrow=1)
j_hat<-J_function(theta_hat,data_L,var_treat,var_y,cov)

### Estimating equation : theta_true --------------------------------------------------------------
true_effect <- c(log(2),log(1.5), log(2))
true_ps<-inv.logit(-2 + 0.01*data$B + 0.01*data$C)

est_1_true<-(log(1.5)*0.5 + log(2))-((data$E*data$Y-(data$E-true_ps)*(as.vector(c(0, true_effect)) %*% t(X_1)))/true_ps)
est_2_true<-(log(1.5)*0.5)-(((1-data$E)*data$Y+(data$E-true_ps)*(as.vector(c(0, true_effect))%*%t(X_0)))/(1-true_ps))
est_3_true<-model.matrix(DR_ATE$out_model)*as.vector(data$Y - as.vector(true_effect)%*%t(as.matrix(data[,c("E",cov)])))
est_4_true<-(data$E-true_ps)*model.matrix(fit)

estimating_equation_true <- as.matrix(rbind(est_1_true,est_2_true,t(est_3_true),t(est_4_true)))

one<-matrix(apply(estimating_equation_true,1,sum)/sqrt(N2),ncol=1)

three<--sqrt(N2)*j_true%*%t(theta_hat-theta_true)
two<--sqrt(N2)*j_hat%*%t(theta_hat-theta_true) 

abs(one-two)
abs(one-three)
