### Simulation Scenario result Debugging of Differenece between ATE ATT Estimator Version3------------------------------------

### <What TO DO> ------------------------------------------------------------
## estimator of Potential outcome denominator check when the difference between ATE, ATT is zero
##  -- N=REPL=1000
### -------------------------------------------------------------------------



### 1. import library & R source file ------------------------------------------------------
library(boot)
source("./Simulation Scenario code/Simulation_Hardcoding_function.R")


### 2. make small dataset ---------------------------------------------------
make_data<-function(A_RATIO, A_VAR1, A_VAR2, N, REPL){
  
  data<-list()
  EFFECT<-c(log(1.5),log(2),log(2))
  
  p<-matrix(NA, nrow=N, ncol=REPL)
  B1<-matrix(NA, nrow=N, ncol=REPL)
  C1<-matrix(NA, nrow=N, ncol=REPL)
  A1<-matrix(NA, nrow=N, ncol=REPL)
  Y1<-matrix(NA, nrow=N, ncol=REPL)
  
  for(repl in 1:REPL){
    delta0<-A_RATIO
    delta_b<-A_VAR1
    delta_c<-A_VAR2
    
    set.seed(123*repl)
    sample<-replicate(N,expr={
      B<-rbinom(1,1,0.5)
      C<-rnorm(1,0,1)
      p_z<-inv.logit(delta0+delta_b*B+delta_c*C)
      A<-rbinom(1,1,prob=p_z)
      c(B,C,A,p_z)
    })
    
    B1[,repl]<-sample[1,]
    C1[,repl]<-sample[2,]
    A1[,repl]<-sample[3,]
    p[,repl]<-sample[4,]
    
    X_sample<-cbind(B1[,repl],C1[,repl],A1[,repl])
    
    # Y random sampling - continuous
    Y1[,repl]<-as.matrix(X_sample)%*%EFFECT+rnorm(nrow(X_sample),0,1)
    
  }
  
  data$B<-B1
  data$C<-C1
  data$A<-A1
  data$Y<-Y1
  data$p<-p
  
  return(data)
}



### 3-1). Estimator check ------------------------------------------------------
### Exposure prevalence : 0.12
A_RATIO<--2
A_VAR1<-0.2
A_VAR2<-0.2
N<-1000
REPL<-1000

data<-make_data(A_RATIO,A_VAR1,A_VAR2,N,REPL)

IPW_estimator_store<-matrix(NA,nrow=2,ncol=REPL,dimnames = list(c("ATE","ATT"),c(1:REPL)))
row.names(IPW_estimator_store)<-c("ATE","ATT")

DR_estimator_store<-matrix(NA,nrow=2,ncol=REPL,dimnames = list(c("ATE","ATT"),c(1:REPL)))
row.names(DR_estimator_store)<-c("ATE","ATT")

cov<-c("B","C")
var_treat<-"A"
var_y<-"Y"


###-------------------------
for(i in 1:REPL){
  B<-data$B[,i]
  C<-data$C[,i]
  A<-data$A[,i]
  Y<-data$Y[,i]
  
  dt<-as.data.frame(cbind(B,C,A,Y))
  colnames(dt)<-c("B","C","A","Y")
  
  weight_IPW_ATE<-weight_make(var_treat,cov,"ATE",dt)
  result_obj_ATE<-lm(Y~A, weight=weight_IPW_ATE$untrimmed)
  IPW_estimator_store[1,i]<-coef(result_obj_ATE)[var_treat]
  
  weight_IPW_ATT<-weight_make(var_treat,cov,"ATT",dt)
  result_obj_ATT<-lm(Y~A, weight=weight_IPW_ATT$untrimmed)
  IPW_estimator_store[2,i]<-coef(result_obj_ATT)[var_treat]
  
  DR_estimator_store[1,i]<-DR_estimator("ATE",dt,var_treat,var_y,cov)$est
  
  DR_estimator_store[2,i]<-DR_estimator("ATT",dt,var_treat,var_y,cov)$est
  
  
}

table((IPW_estimator_store[1,]-IPW_estimator_store[2,])<=1e-06)
table((DR_estimator_store[1,]-DR_estimator_store[2,])<=1e-06)


### ---------------------
### ---------------------
ind3<-which((IPW_estimator_store[1,]-IPW_estimator_store[2,])<=1e-06)

IPW_deno_check<-matrix(NA,nrow=length(ind3),ncol=2)
colnames(IPW_deno_check)<-c("ATE_1","ATT_1")


for(i in 1:length(ind3)){
  
  B<-data$B[,ind3[i]]
  C<-data$C[,ind3[i]]
  A<-data$A[,ind3[i]]
  Y<-data$Y[,ind3[i]]
  
  fit<-glm(A~B+C,family='binomial')
  p<-fit$fitted.values
  
  IPW_deno_check[i,2]<-sum(A)
  IPW_deno_check[i,1]<-sum(A/p)
  
}

table((IPW_deno_check[,1]-IPW_deno_check[,2])<=1e-06)




### -------------------------------------------------------------------------
### ------------------------------------------------------------
ind4<-which((DR_estimator_store[1,]-DR_estimator_store[2,])<=1e-06)

DR_deno_check<-matrix(NA,nrow=length(ind4),ncol=2)
colnames(DR_deno_check)<-c("ATE_1","ATT_1")


for(i in 1:length(ind4)){
  
  B<-data$B[,ind4[i]]
  C<-data$C[,ind4[i]]
  A<-data$A[,ind4[i]]
  Y<-data$Y[,ind4[i]]
  
  fit<-glm(A~B+C,family='binomial')
  p<-fit$fitted.values
  
  DR_deno_check[i,2]<-sum(A)
  DR_deno_check[i,1]<-sum(p)
  
}


table((DR_deno_check[,1]-DR_deno_check[,2])<=1e-06)







### -------------------------------------------------------------------------
### 3-2). Estimator check ------------------------------------------------------
### Exposure prevalence : 0.5 ~ 0.6
A_RATIO<-0
A_VAR1<-0.6
A_VAR2<-0.6
N<-1000
REPL<-1000

data<-make_data(A_RATIO,A_VAR1,A_VAR2,N,REPL)

IPW_estimator_store<-matrix(NA,nrow=2,ncol=REPL,dimnames = list(c("ATE","ATT"),c(1:REPL)))
row.names(IPW_estimator_store)<-c("ATE","ATT")

DR_estimator_store<-matrix(NA,nrow=2,ncol=REPL,dimnames = list(c("ATE","ATT"),c(1:REPL)))
row.names(DR_estimator_store)<-c("ATE","ATT")

cov<-c("B","C")
var_treat<-"A"
var_y<-"Y"


###-------------------------
for(i in 1:REPL){
  B<-data$B[,i]
  C<-data$C[,i]
  A<-data$A[,i]
  Y<-data$Y[,i]
  
  dt<-as.data.frame(cbind(B,C,A,Y))
  colnames(dt)<-c("B","C","A","Y")
  
  weight_IPW_ATE<-weight_make(var_treat,cov,"ATE",dt)
  result_obj_ATE<-lm(Y~A, weight=weight_IPW_ATE$untrimmed)
  IPW_estimator_store[1,i]<-coef(result_obj_ATE)[var_treat]
  
  weight_IPW_ATT<-weight_make(var_treat,cov,"ATT",dt)
  result_obj_ATT<-lm(Y~A, weight=weight_IPW_ATT$untrimmed)
  IPW_estimator_store[2,i]<-coef(result_obj_ATT)[var_treat]
  
  DR_estimator_store[1,i]<-DR_estimator("ATE",dt,var_treat,var_y,cov)$est
  
  DR_estimator_store[2,i]<-DR_estimator("ATT",dt,var_treat,var_y,cov)$est
  
  
}

table((IPW_estimator_store[1,]-IPW_estimator_store[2,])<=1e-06)
table((DR_estimator_store[1,]-DR_estimator_store[2,])<=1e-06)


### ---------------------
### ---------------------
ind1<-which((IPW_estimator_store[1,]-IPW_estimator_store[2,])<=1e-06)

IPW_deno_check<-matrix(NA,nrow=length(ind1),ncol=2)
colnames(IPW_deno_check)<-c("ATE_1","ATT_1")


for(i in 1:length(ind1)){
  
  B<-data$B[,ind1[i]]
  C<-data$C[,ind1[i]]
  A<-data$A[,ind1[i]]
  Y<-data$Y[,ind1[i]]
  
  fit<-glm(A~B+C,family='binomial')
  p<-fit$fitted.values
  
  IPW_deno_check[i,2]<-sum(A)
  IPW_deno_check[i,1]<-sum((A/p))
  
}

table((IPW_deno_check[,1]-IPW_deno_check[,2])<=1e-06)





### -------------------------------------------------------------------------
### ------------------------------------------------------------
ind2<-which((DR_estimator_store[1,]-DR_estimator_store[2,])<=1e-06)

DR_deno_check<-matrix(NA,nrow=length(ind4),ncol=2)
colnames(DR_deno_check)<-c("ATE_1","ATT_1")


for(i in 1:length(ind2)){
  
  B<-data$B[,ind2[i]]
  C<-data$C[,ind2[i]]
  A<-data$A[,ind2[i]]
  Y<-data$Y[,ind2[i]]
  
  fit<-glm(A~B+C,family='binomial')
  p<-fit$fitted.values
  
  DR_deno_check[i,2]<-sum(A)
  DR_deno_check[i,1]<-sum(p)
  
}


table((DR_deno_check[,1]-DR_deno_check[,2])<=1e-06)

