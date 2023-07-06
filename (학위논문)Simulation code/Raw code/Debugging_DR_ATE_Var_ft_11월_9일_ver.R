### Debugging sandwich robust variance estimator of DR ATT Estimator --------
### 11 / 09 Version ---------------------------------------------------------

### <What to Do> -------------------
## coverage probability comparing ##
### --------------------------------


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


### Sandwich robust variance estimator of DR Estimator -----------------
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





### 1) D(theta_star) calculate ----------------------------------------------

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



### true Expectation of crossprod of estimating_equation calculate --------------
### Monte carlo approximation -----------------------------------------------
dim1<-9

X_1<-as.matrix(cbind(1,1,data_L[,cov]))
X_0<-as.matrix(cbind(1,0,data_L[,cov]))

true_effect <- c(log(2),log(1.5), log(2))
true_ps<-inv.logit(-2 + 0.01*data_L$B + 0.01*data_L$C)

DR_ATE<-DR_estimator("ATE",data_L,"E","Y",cov)
fit<-DR_ATE$fit

nu_true_array<-array(0,dim=c(dim1,dim1,nrow(data_L)))

for(j in 1:nrow(data_L)){
  
  est_1_true<-(log(1.5)*0.5 + log(2))-((data_L$E[j]*data_L$Y[j]-(data_L$E[j]-true_ps[j])*(as.vector(c(0, true_effect)) %*% X_1[j,]))/true_ps[j])
  est_2_true<-(log(1.5)*0.5)-(((1-data_L$E[j])*data_L$Y[j]+(data_L$E[j]-true_ps[j])*(as.vector(c(0, true_effect))%*%X_0[j,]))/(1-true_ps[j]))
  est_3_true<-model.matrix(DR_ATE$out_model)[j,]*as.vector(data_L$Y[j] - as.vector(true_effect)%*%t(as.matrix(data_L[j,c("E",cov)])))
  est_4_true<-(data_L$E[j]-true_ps[j])*model.matrix(fit)[j,]
  
  estimating_equation_true <- matrix(rbind(est_1_true,est_2_true,matrix(est_3_true,ncol=1),matrix(est_4_true,ncol=1)),ncol=1)
  nu_true_array[,,j]<-estimating_equation_true%*%t(estimating_equation_true)
  
}

nu_true_mean<-apply(nu_true_array,c(1,2),mean) 




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


cov_theta_star<-(solve(j_true)%*%nu_true_mean%*%t(solve(j_true)))
D_theta_star<-sqrt(cov_theta_star[1,1]+cov_theta_star[2,2]-2*cov_theta_star[1,2])
# 3.0783




### 1)-1. Coverage probability using D_theta_star ---------------------------
### Data generation --------------------------------------------------
### - # of obs = 100 -----------------------------------------
N = 100; REPL = 1000

B<-matrix(0,nrow=N,ncol=REPL)
C<-matrix(0,nrow=N,ncol=REPL)
E<-matrix(0,nrow=N,ncol=REPL)
Y<-matrix(0,nrow=N,ncol=REPL)

effect<-c(log(1.5),log(2),log(2))


### 1000th replication data generating -----------------------------------------
for(repl in 1:REPL){
  
  delta0<--2
  delta_b<-0.01
  delta_c<-0.01
  
  set.seed(123*repl)
  sample<-replicate(N,expr={
    B<-rnorm(1,0,1)
    C<-rnorm(1,0,1)
    p_z<-inv.logit(delta0+delta_b*B+delta_c*C)
    E<-rbinom(1,1,prob=p_z)
    c(B,C,E)
  })
  
  B[,repl]<-sample[1,]
  C[,repl]<-sample[2,]
  E[,repl]<-sample[3,]
  
  X_sample<-cbind(B[,repl],C[,repl],E[,repl])
  
  # Y random sampling - continuous
  Y[,repl]<-as.matrix(X_sample)%*%effect+rnorm(nrow(X_sample),0,1)
}


write.csv(B,file="B_100.csv",row.names = FALSE)
write.csv(C,file="C_100.csv",row.names = FALSE)
write.csv(E,file="E_100.csv",row.names = FALSE)
write.csv(Y,file="Y_100.csv",row.names = FALSE)

E_100<-read.csv("E_100.csv")
B_100<-read.csv("B_100.csv")
C_100<-read.csv("C_100.csv")
Y_100<-read.csv("Y_100.csv")


### Data generation --------------------------------------------------
### - # of obs = 1000 -----------------------------------------
N = 1000; REPL = 1000

B<-matrix(0,nrow=N,ncol=REPL)
C<-matrix(0,nrow=N,ncol=REPL)
E<-matrix(0,nrow=N,ncol=REPL)
Y<-matrix(0,nrow=N,ncol=REPL)

effect<-c(log(1.5),log(2),log(2))


### 1000th replication data generating -----------------------------------------
for(repl in 1:REPL){
  
  delta0<--2
  delta_b<-0.01
  delta_c<-0.01
  
  set.seed(123*repl)
  sample<-replicate(N,expr={
    B<-rnorm(1,0,1)
    C<-rnorm(1,0,1)
    p_z<-inv.logit(delta0+delta_b*B+delta_c*C)
    E<-rbinom(1,1,prob=p_z)
    c(B,C,E)
  })
  
  B[,repl]<-sample[1,]
  C[,repl]<-sample[2,]
  E[,repl]<-sample[3,]
  
  X_sample<-cbind(B[,repl],C[,repl],E[,repl])
  
  # Y random sampling - continuous
  Y[,repl]<-as.matrix(X_sample)%*%effect+rnorm(nrow(X_sample),0,1)
}


write.csv(B,file="B_1000.csv",row.names = FALSE)
write.csv(C,file="C_1000.csv",row.names = FALSE)
write.csv(E,file="E_1000.csv",row.names = FALSE)
write.csv(Y,file="Y_1000.csv",row.names = FALSE)

E_1000<-read.csv("E_1000.csv")
B_1000<-read.csv("B_1000.csv")
C_1000<-read.csv("C_1000.csv")
Y_1000<-read.csv("Y_1000.csv")


### Getting 1000th estimator ------------------------------------------------
est_DR_100<-rep(NA,length=REPL)

est_100_CV<-matrix(NA,nrow=REPL,ncol=3)
colnames(est_100_CV)<-c("CL","CU","contain")

cov<-c("B","C")
cov_type<-c("binary","continuous")

var_treat<-"E"
var_y<-"Y"

for(rep in 1:REPL){
  
  E<-E_100[,rep]
  B<-B_100[,rep]
  C<-C_100[,rep]
  Y<-Y_100[,rep]
  data<-data.frame("E"=E,"B"=B,"C"=C,"Y"=Y)
  
  DR_ATE<-DR_estimator("ATE",data,var_treat,var_y,cov)
  
  est_DR_100[rep]<-DR_ATE$est
  est_100_CV[rep,1]<-est_DR_100[rep]-1.96*(D_theta_star/sqrt(nrow(data)))
  est_100_CV[rep,2]<-est_DR_100[rep]+1.96*(D_theta_star/sqrt(nrow(data)))
  est_100_CV[rep,3]<-ifelse((est_100_CV[rep,1]<=log(2) & est_100_CV[rep,2]>=log(2)), 1, 0)

}

D_theta_star_coverage_100<-mean(est_100_CV[,3])
# 0.925




### Getting 1000th estimator ------------------------------------------------
est_DR_1000<-rep(NA,length=REPL)

est_1000_CV<-matrix(NA,nrow=REPL,ncol=3)
colnames(est_1000_CV)<-c("CL","CU","contain")

cov<-c("B","C")
cov_type<-c("binary","continuous")

var_treat<-"E"
var_y<-"Y"

for(rep in 1:REPL){
  
  E<-E_1000[,rep]
  B<-B_1000[,rep]
  C<-C_1000[,rep]
  Y<-Y_1000[,rep]
  data<-data.frame("E"=E,"B"=B,"C"=C,"Y"=Y)
  
  DR_ATE<-DR_estimator("ATE",data,var_treat,var_y,cov)
  
  est_DR_1000[rep]<-DR_ATE$est
  est_1000_CV[rep,1]<-est_DR_1000[rep]-1.96*(D_theta_star/sqrt(nrow(data)))
  est_1000_CV[rep,2]<-est_DR_1000[rep]+1.96*(D_theta_star/sqrt(nrow(data)))
  est_1000_CV[rep,3]<-ifelse((est_1000_CV[rep,1]<=log(2) & est_1000_CV[rep,2]>=log(2)), 1, 0)
  
}

D_theta_star_coverage_1000<-mean(est_1000_CV[,3])
# [1] 0.956






### -------------------------------------------------------------------------
### 1)-2. Coverage probability using D_theta_hat ---------------------------
hat_DR_100<-rep(NA,length=REPL)

hat_100_CV<-matrix(NA,nrow=REPL,ncol=3)
colnames(hat_100_CV)<-c("CL","CU","contain")

cov<-c("B","C")
cov_type<-c("binary","continuous")

var_treat<-"E"
var_y<-"Y"

for(rep in 1:REPL){
  
  E<-E_100[,rep]
  B<-B_100[,rep]
  C<-C_100[,rep]
  Y<-Y_100[,rep]
  data<-data.frame("E"=E,"B"=B,"C"=C,"Y"=Y)
  
  DR_ATE<-DR_estimator("ATE",data,var_treat,var_y,cov)
  D_theta_hat<-sandwich_var_DR("ATE",data,var_treat,var_y,cov)
  
  
  hat_DR_100[rep]<-DR_ATE$est
  hat_100_CV[rep,1]<-hat_DR_100[rep]-1.96*sqrt(D_theta_hat)
  hat_100_CV[rep,2]<-hat_DR_100[rep]+1.96*sqrt(D_theta_hat)
  hat_100_CV[rep,3]<-ifelse((hat_100_CV[rep,1]<=log(2) & hat_100_CV[rep,2]>=log(2)), 1, 0)
  
}

D_theta_hat_coverage_100<-mean(hat_100_CV[,3])
# [1] 0.276


### Getting 1000th estimator ------------------------------------------------
hat_DR_1000<-rep(NA,length=REPL)

hat_1000_CV<-matrix(NA,nrow=REPL,ncol=3)
colnames(hat_100_CV)<-c("CL","CU","contain")

cov<-c("B","C")
cov_type<-c("binary","continuous")

var_treat<-"E"
var_y<-"Y"

for(rep in 1:REPL){
  
  E<-E_1000[,rep]
  B<-B_1000[,rep]
  C<-C_1000[,rep]
  Y<-Y_1000[,rep]
  data<-data.frame("E"=E,"B"=B,"C"=C,"Y"=Y)
  
  DR_ATE<-DR_estimator("ATE",data,var_treat,var_y,cov)
  D_theta_hat<-sandwich_var_DR("ATE",data,var_treat,var_y,cov)
  
  result<-drgee(oformula=formula(Y~B+C),
                eformula=formula(E~B+C),
                olink = 'identity',elink='logit',
                data=data,estimation.method = 'dr')
  
  DR_ATE_package[i]<-result$vcov
  
  hat_DR_1000[rep]<-DR_ATE$est
  hat_1000_CV[rep,1]<-hat_DR_1000[rep]-1.96*sqrt(D_theta_hat)
  hat_1000_CV[rep,2]<-hat_DR_1000[rep]+1.96*sqrt(D_theta_hat)
  hat_1000_CV[rep,3]<-ifelse((hat_1000_CV[rep,1]<=log(2) & hat_1000_CV[rep,2]>=log(2)), 1, 0)
  
}

D_theta_hat_coverage_1000<-mean(hat_1000_CV[,3])
# [1] 0.116

### 1-3) package result -----------------------------------------------------
package_DR_1000<-rep(NA,length=REPL)

package_1000_CV<-matrix(NA,nrow=REPL,ncol=3)
colnames(package_1000_CV)<-c("CL","CU","contain")

cov<-c("B","C")
cov_type<-c("binary","continuous")

var_treat<-"E"
var_y<-"Y"

for(rep in 1:REPL){
  
  E<-E_1000[,rep]
  B<-B_1000[,rep]
  C<-C_1000[,rep]
  Y<-Y_1000[,rep]
  data<-data.frame("E"=E,"B"=B,"C"=C,"Y"=Y)
  
  DR_ATE<-DR_estimator("ATE",data,var_treat,var_y,cov)
  
  result<-drgee(oformula=formula(Y~B+C),
                eformula=formula(E~B+C),
                olink = 'identity',elink='logit',
                data=data,estimation.method = 'dr')
  
  var_ATE_package<-result$vcov
  
  package_DR_1000[rep]<-DR_ATE$est
  package_1000_CV[rep,1]<-package_DR_1000[rep]-1.96*sqrt(var_ATE_package)
  package_1000_CV[rep,2]<-package_DR_1000[rep]+1.96*sqrt(var_ATE_package)
  package_1000_CV[rep,3]<-ifelse((package_1000_CV[rep,1]<=log(2) & package_1000_CV[rep,2]>=log(2)), 1, 0)
  
}


package_coverage_1000<-mean(package_1000_CV[,3])