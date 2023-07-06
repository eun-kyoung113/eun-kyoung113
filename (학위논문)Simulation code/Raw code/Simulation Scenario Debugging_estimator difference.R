### Simulation Scenario result Debugging ------------------------------------

### <What TO DO> ------------------------------------------------------------
## absolute difference between ATE, ATT Estimator check 
##  -- make very small dataset and check each estimator value
### -------------------------------------------------------------------------



### 1. import library & R source file ------------------------------------------------------
library(boot)
source("C:/Users/stat/OneDrive - 숙명여자대학교/석사연구/석사 논문/My paper Simulation code/Simulation_Hardcoding_function.R")


### 2. make small dataset ---------------------------------------------------
make_data<-function(A_RATIO, N, REPL){
  
  data<-list()
  EFFECT<-c(log(1.5),log(2),log(2))
  
  B1<-matrix(NA, nrow=N, ncol=REPL)
  C1<-matrix(NA, nrow=N, ncol=REPL)
  A1<-matrix(NA, nrow=N, ncol=REPL)
  Y1<-matrix(NA, nrow=N, ncol=REPL)
  
  for(repl in 1:REPL){
    delta0<-A_RATIO
    delta_b<-0.01
    delta_c<-0.01
    
    set.seed(123*repl)
    sample<-replicate(N,expr={
      B<-rbinom(1,1,0.5)
      C<-rnorm(1,0,1)
      p_z<-inv.logit(delta0+delta_b*B+delta_c*C)
      A<-rbinom(1,1,prob=p_z)
      c(B,C,A)
    })
    
    B1[,repl]<-sample[1,]
    C1[,repl]<-sample[2,]
    A1[,repl]<-sample[3,]
    
    X_sample<-cbind(B1[,repl],C1[,repl],A1[,repl])
    
    # Y random sampling - continuous
    Y1[,repl]<-as.matrix(X_sample)%*%EFFECT+rnorm(nrow(X_sample),0,1)

  }
  
  data$B<-B1
  data$C<-C1
  data$A<-A1
  data$Y<-Y1
  
  return(data)
}



### 3. Estimator check ------------------------------------------------------
A_RATIO<-0
N<-10
REPL<-20

data<-make_data(A_RATIO,N,REPL)

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

print(IPW_estimator_store)
print(DR_estimator_store)



B<-data$B[,6]
C<-data$C[,6]
A<-data$A[,6]
Y<-data$Y[,6]

dt7<-as.data.frame(cbind(B,C,A,Y))
colnames(dt7)<-c("B","C","A","Y") # A=1인 obs 1개
dt7

B<-data$B[,5]
C<-data$C[,5]
A<-data$A[,5]
Y<-data$Y[,5]

dt5<-as.data.frame(cbind(B,C,A,Y))
colnames(dt5)<-c("B","C","A","Y") # A=1인 obs 1개
dt5




B<-data$B[,9]
C<-data$C[,9]
A<-data$A[,9]
Y<-data$Y[,9]

dt1<-as.data.frame(cbind(B,C,A,Y))
colnames(dt1)<-c("B","C","A","Y")
dt1


B<-data$B[,3]
C<-data$C[,3]
A<-data$A[,3]
Y<-data$Y[,3]

dt3<-as.data.frame(cbind(B,C,A,Y))
colnames(dt3)<-c("B","C","A","Y")
dt3
