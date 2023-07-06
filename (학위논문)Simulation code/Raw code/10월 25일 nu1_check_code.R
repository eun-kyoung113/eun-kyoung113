library(boot)
library(tableone)
library(survey)
library(Hmisc)
library(geepack)
library(geex)
library(drgee)



### -------------------------------------------------------------------------
### monte carlo sample size 현재 100000임 ----------------------------------

### Data generation --------------------------------------------------
### - with # of obs = 100000 -----------------------------------------
B_sample<-matrix(0,nrow=1000000,ncol=1)
C_sample<-matrix(0,nrow=1000000,ncol=1)
E_sample<-matrix(0,nrow=1000000,ncol=1)
Y_sample<-matrix(0,nrow=1000000,ncol=1)

effect<-c(log(1.5),log(2),log(2))


### 1th replication data generating -----------------------------------------
delta0<--2
delta_b<-0.01
delta_c<-0.01
set.seed(123)
sample<-replicate(1000000,expr={
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

write.csv(B_sample[,1],file="B_1000000.csv",row.names = FALSE)
write.csv(C_sample[,1],file="C_1000000.csv",row.names = FALSE)
write.csv(E_sample[,1],file="E_1000000.csv",row.names = FALSE)
write.csv(Y_sample[,1],file="Y_1000000.csv",row.names = FALSE)


### -------------------------------------------------------------------------
### est_1_true 계산 -----------------------------------------------------------
### approximation of true score's variance ----------------------------------
E<-read.csv("E_1000000.csv")
B<-read.csv("B_1000000.csv")
C<-read.csv("C_1000000.csv")
Y<-read.csv("Y_1000000.csv")

data<-data.frame("E"=E,"B"=B,"C"=C,"Y"=Y)
colnames(data)<-c("E","B","C","Y")

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

### -------------------------------------------------------------------------




### Data generation --------------------------------------------------
### - # of obs = 1000 -----------------------------------------
N = 1000 ; REPL = 1000

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

### -------------------------------------------------------------------------
### est_1_true 계산 -----------------------------------------------------------
### approximation of true score's variance ----------------------------------
E_100<-read.csv("E_100.csv")
B_100<-read.csv("B_100.csv")
C_100<-read.csv("C_100.csv")
Y_100<-read.csv("Y_100.csv")


### -------------------------------------------------------------------------
### est_1_true 계산 -------------------------------------------------------
#### sample variance of averaged true score (# of samples = # of replications 1000)

z_true <- matrix(0, nrow=REPL, ncol=1)
true_effect <- c(log(2),log(1.5), log(2))

for(i in 1:REPL){
  E<-E_100[,i]
  B<-B_100[,i]
  C<-C_100[,i]
  Y<-Y_100[,i]
  data<-data.frame("E"=E,"B"=B,"C"=C,"Y"=Y)
  cov<-c("B","C")
  cov_type<-c("binary","continuous")
  
  X_1<-as.matrix(cbind(1,1,data[,cov]))
  X_0<-as.matrix(cbind(1,0,data[,cov]))
  
  true_ps<-inv.logit(-2 + 0.01*data$B + 0.01*data$C)
  
  # true score function의 nu_1 부분
  #est_1_true2<-(log(1.5)*0.5 + log(2))-((data$E*data$Y-(data$E-true_ps)*(as.vector(c(0, true_effect)) %*% t(X_1)))/true_ps)
  est_1_true<-(log(1.5)*0.5 + log(2))-((data$E*data$Y-(data$E-true_ps)*(X_1 %*% as.vector(c(0, true_effect))))/true_ps)
  #as.vector(est_1_true2) == as.vector(est_1_true)
  
  if (i==1){
    print(dim(data))
    print(dim(est_1_true))
  }
  
  z_true[i,1]<-mean(est_1_true)
  
  
}

nu_theta<-var(z_true)*N
abs(mean(nu_true_vec)-nu_theta)
