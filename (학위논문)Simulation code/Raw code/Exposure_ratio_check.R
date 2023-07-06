library(boot)

delta0<--2
delta_b<-0.01
delta_c<-0.01
#delta_u<-0.015

sample<-replicate(10000,expr={
  B<-rnorm(1,0,1)
  C<-rnorm(1,0,1)
  #U<-rbinom(1,1,0.5)
  p_z<-inv.logit(delta0+delta_b*B+delta_c*C)
  A<-rbinom(1,1,prob=p_z)
  c(B,C,A)
})
B<-sample[1,]
C<-sample[2,]
#U<-sample[3,]
A<-sample[3,]

mean(A)
# [1] 0.1168



### -------------------------------------------------------------------------
delta0<--2.5
delta_b<-0.01
delta_c<-0.01
#delta_u<-0.015

sample<-replicate(10000,expr={
  B<-rnorm(1,0,1)
  C<-rnorm(1,0,1)
  #<-rbinom(1,1,0.5)
  p_z<-inv.logit(delta0+delta_b*B+delta_c*C)
  A<-rbinom(1,1,prob=p_z)
  c(B,C,A)
})
B<-sample[1,]
C<-sample[2,]
#U<-sample[3,]
A<-sample[3,]

mean(A)
# [1] 0.0789

### -------------------------------------------------------------------------
delta0<--3
delta_b<-0.01
delta_c<-0.01
#delta_u<-0.015

sample<-replicate(10000,expr={
  B<-rnorm(1,0,1)
  C<-rnorm(1,0,1)
  U<-rbinom(1,1,0.5)
  p_z<-inv.logit(delta0+delta_b*B+delta_c*C)
  A<-rbinom(1,1,prob=p_z)
  c(B,C,A)
})
B<-sample[1,]
C<-sample[2,]
#U<-sample[3,]
A<-sample[3,]

mean(A)
# [1] 0.0495

### -------------------------------------------------------------------------
delta0<--4
delta_b<-0.01
delta_c<-0.01
#delta_u<-0.015

sample<-replicate(10000,expr={
  B<-rnorm(1,0,1)
  C<-rnorm(1,0,1)
  U<-rbinom(1,1,0.5)
  p_z<-inv.logit(delta0+delta_b*B+delta_c*C)
  A<-rbinom(1,1,prob=p_z)
  c(B,C,A)
})
B<-sample[1,]
C<-sample[2,]
#U<-sample[3,]
A<-sample[3,]

mean(A)
# [1] 0.0195

### -------------------------------------------------------------------------
delta0<--4.5
delta_b<-0.01
delta_c<-0.01
#delta_u<-0.015

sample<-replicate(10000,expr={
  B<-rnorm(1,0,1)
  C<-rnorm(1,0,1)
  U<-rbinom(1,1,0.5)
  p_z<-inv.logit(delta0+delta_b*B+delta_c*C)
  A<-rbinom(1,1,prob=p_z)
  c(B,C,A)
})
B<-sample[1,]
C<-sample[2,]
#U<-sample[3,]
A<-sample[3,]

mean(A)
# [1] 0.0124

### -------------------------------------------------------------------------
delta0<--5
delta_b<-0.01
delta_c<-0.01
#delta_u<-0.015

sample<-replicate(10000,expr={
  B<-rnorm(1,0,1)
  C<-rnorm(1,0,1)
  U<-rbinom(1,1,0.5)
  p_z<-inv.logit(delta0+delta_b*B+delta_c*C)
  A<-rbinom(1,1,prob=p_z)
  c(B,C,A)
})
B<-sample[1,]
C<-sample[2,]
#U<-sample[3,]
A<-sample[3,]

mean(A)
# [1] 0.0057