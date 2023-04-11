### -------------------------------------------------------------------------
### Code of getting result table  ------------------------------

### Change Scenario & Exposure ratio = 0.015 ---------------------------
### Change some Hardcoding function --------------------

### Version 0 for Simulation Scenario Version 4 ----------------------------------

### Add Confounder variable -------------------------------------------------



# -------------------------------------------------------------------------
# ---------------------- Import file & library ---------------------------
# -------------------------------------------------------------------------
library(boot)
source("./Simulation Scenario code/Debugging_function/For_ATT_Simulation_Hardcoding_function.R")

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
N<-1000
REPL<-1000

true_ATT<-1
z<-qnorm(0.975)

### Make matrix storing result -----------------------------------
ATT_1_result<-matrix(NA,nrow=REPL,ncol=13)
colnames(ATT_1_result)<-c("Outcome_reg_est","Outcome_reg_var","Outcome_reg_coverage",
                          "IPW_est","IPW_Naive_var","Naive_IPW_Coverage",
                          "IPW_Sandwich_var","Sandwich_IPW_Coverage",
                          "DR_est","DR_Naive_var","DR_Naive_coverage","DR_Sandwich_var","Sandwich_DR_coverage")


### Make matrix storing result -----------------------------------
ATT_2_result<-matrix(NA,nrow=REPL,ncol=13)
colnames(ATT_2_result)<-c("Outcome_reg_est","Outcome_reg_var","Outcome_reg_coverage",
                          "IPW_est","IPW_Naive_var","Naive_IPW_Coverage",
                          "IPW_Sandwich_var","Sandwich_IPW_Coverage",
                          "DR_est","DR_Naive_var","DR_Naive_coverage","DR_Sandwich_var","Sandwich_DR_coverage")


### Make matrix storing result -----------------------------------
ATT_3_result<-matrix(NA,nrow=REPL,ncol=13)
colnames(ATT_3_result)<-c("Outcome_reg_est","Outcome_reg_var","Outcome_reg_coverage",
                          "IPW_est","IPW_Naive_var","Naive_IPW_Coverage",
                          "IPW_Sandwich_var","Sandwich_IPW_Coverage",
                          "DR_est","DR_Naive_var","DR_Naive_coverage","DR_Sandwich_var","Sandwich_DR_coverage")



### Make matrix storing result -----------------------------------
ATT_4_result<-matrix(NA,nrow=REPL,ncol=13)
colnames(ATT_4_result)<-c("Outcome_reg_est","Outcome_reg_var","Outcome_reg_coverage",
                          "IPW_est","IPW_Naive_var","Naive_IPW_Coverage",
                          "IPW_Sandwich_var","Sandwich_IPW_Coverage",
                          "DR_est","DR_Naive_var","DR_Naive_coverage","DR_Sandwich_var","Sandwich_DR_coverage")



# ---------------------------------------------------------------
# ---------------------------------------------------------------
for(repl in 1:REPL){
  
  al<-c(-8, 0.7, 0.7, 0.5) # Version0
  #al<-c(-6.85, 0.6, 0.5, 0.4) # Version1
  #al<-c(-2.8, 0.5, 0.3, 0.1)
  
  theta<-c(1,2.5,2.5,1,1) # Version0
  #theta<-c(1,2.5,1.5,1,1) -- Version1
  #theta<-c(1,2,1.5,1,0.7)
  
  set.seed(123*repl)
  B<-rnorm(N,0,1)
  C<-rbinom(N,1,0.5)
  D<-runif(N,0,10)
  U<-runif(N,0,10)
  
  pi.vec<-1/(1+exp(-cbind(1,B,C,D)%*%al))
  range(pi.vec)
  A<-rbinom(N,1,pi.vec)
  mean(A)
  Y<-cbind(1,B,C,A,U)%*%theta+rnorm(N)
  
  
  ### replication start -------------------------------------------------------
  ### Scenario 1 - ATT --------------------------------------------------------
  data<-as.data.frame(cbind(B,C,A,Y))
  colnames(data)<-c("B","C","A","Y")
  
  cov_ps_1<-c("B","C")
  #cov_type<-c("continuous","continuous","binary")
  var_treat<-"A"
  var_y<-"Y"
  
  # ------------------------------------------------
  ### Outcome regression Estimator -----------------
  outcome_reg_cov_1<-c("B","C")
  outcome_data<-data[,c(var_treat,var_y,outcome_reg_cov_1)]
  outcome_reg_ATT_1<-lm(Y~., data=outcome_data)
  
  ATT_reg_est_1<-coef(outcome_reg_ATT_1)[var_treat]
  
  re_ATT<-summary(outcome_reg_ATT_1)
  re1_ATT<-as.data.frame(re_ATT$coefficients)
  outcome_reg_var_ATT_1<-re1_ATT$`Std. Error`[which(rownames(re1_ATT)==var_treat)]
  
  outcome_reg_ATT_lo<-ATT_reg_est_1-z*(outcome_reg_var_ATT_1)
  outcome_reg_ATT_up<-ATT_reg_est_1+z*(outcome_reg_var_ATT_1)
  coverage_outcome_reg_ATT_1<-ifelse((outcome_reg_ATT_lo<=true_ATT) & (outcome_reg_ATT_up>=true_ATT),1,0)
  
  ATT_1_result[repl,c(1:3)]<-c(ATT_reg_est_1,outcome_reg_var_ATT_1,coverage_outcome_reg_ATT_1)
  
  
  # ------------------------------
  ### IPW Estimator ---------------
  formula_1<-"A~B+C"
  weight_IPW_ATT_1<-weight_make(var_treat,cov_ps_1,"ATT",data,formula_1)
  result_obj_ATT_1<-lm(Y~A, weight=weight_IPW_ATT_1$untrimmed)
  IPW_ATT_1<-coef(result_obj_ATT_1)[var_treat]
  
  re_ATT<-summary(result_obj_ATT_1)
  re1_ATT<-as.data.frame(re_ATT$coefficients)
  
  Naive_IPW_var_ATT_1<-re1_ATT$`Std. Error`[which(rownames(re1_ATT)==var_treat)]
  IPW_Naive_lo_ATT<-IPW_ATT_1-z*(Naive_IPW_var_ATT_1)
  IPW_Naive_up_ATT<-IPW_ATT_1+z*(Naive_IPW_var_ATT_1)
  coverage_IPW_Naive_ATT_1<-ifelse((IPW_Naive_lo_ATT<=true_ATT) & (IPW_Naive_up_ATT>=true_ATT),1,0)
  
  sandwich_IPW_var_ATT_1<-sandwich_var_ATT(weight_IPW_ATT_1,result_obj_ATT_1,data,cov_ps_1,var_treat,var_y)
  IPW_sandwich_lo_ATT<-IPW_ATT_1-z*sqrt(sandwich_IPW_var_ATT_1)
  IPW_sandwich_up_ATT<-IPW_ATT_1+z*sqrt(sandwich_IPW_var_ATT_1)
  coverage_IPW_sandwich_ATT_1 <-ifelse((IPW_sandwich_lo_ATT<=true_ATT) & (IPW_sandwich_up_ATT>=true_ATT),1,0)
  
  ATT_1_result[repl,c(4:8)]<-c(IPW_ATT_1,Naive_IPW_var_ATT_1,coverage_IPW_Naive_ATT_1,
                               sandwich_IPW_var_ATT_1, coverage_IPW_sandwich_ATT_1)
  
  
  
  ### -------------------------------------------------------------------------
  ### DR ATT ---------------------------------------------------
  ps_formula_1<-"A~B+C"
  outcome_formula_1<-"Y~A+B+C"
  DR_outcome_cov_1<-c("B","C")
  DR_ATT_1<-DR_estimator("ATT",data,var_treat,var_y,DR_outcome_cov_1,cov_ps_1,ps_formula_1,outcome_formula_1)$est
  
  DR_Naive_var_ATT_1<-DR_Naive_var_estimator("ATT",data,var_treat,var_y,DR_outcome_cov_1,cov_ps_1,ps_formula_1,outcome_formula_1)
  DR_Naive_lo_ATT<-DR_ATT_1-z*sqrt(DR_Naive_var_ATT_1)
  DR_Naive_up_ATT<-DR_ATT_1+z*sqrt(DR_Naive_var_ATT_1)
  coverage_DR_Naive_ATT_1<-ifelse((DR_Naive_lo_ATT<=true_ATT) & (DR_Naive_up_ATT>=true_ATT),1,0)
  
  j_hat<-J_function(data,data,var_treat,var_y,DR_outcome_cov_1,cov_ps_1,ps_formula_1,outcome_formula_1)
  crossprod<-crossprod_est_equ(data,data,var_treat,var_y,DR_outcome_cov_1,cov_ps_1,ps_formula_1,outcome_formula_1)
  cov_hat<-solve(j_hat)%*%crossprod%*%t(solve(j_hat))
  
  DR_sandwich_var_ATT_1<-(cov_hat[1,1]+cov_hat[2,2]-2*cov_hat[1,2])/nrow(data)
  
  DR_sandwich_lo_ATT<-DR_ATT_1-z*sqrt(DR_sandwich_var_ATT_1)
  DR_sandwich_up_ATT<-DR_ATT_1+z*sqrt(DR_sandwich_var_ATT_1)
  
  coverage_DR_sandwich_ATT_1<-ifelse((DR_sandwich_lo_ATT<=true_ATT) & (DR_sandwich_up_ATT>=true_ATT),1,0)
  
  ATT_1_result[repl,c(9:ncol(ATT_1_result))]<-c(DR_ATT_1,DR_Naive_var_ATT_1, coverage_DR_Naive_ATT_1,
                                                DR_sandwich_var_ATT_1, coverage_DR_sandwich_ATT_1)
  
  
  
  # -------------------------------------
  # -------------------------------------
  ### Scenario 2 - ATT --------------------------------------------------------
  cov_ps_2<-c("B","C")
  #cov_type<-c("continuous","continuous","binary")
  var_treat<-"A"
  var_y<-"Y"
  
  # ------------------------------------------------
  ### Outcome regression Estimator -----------------
  outcome_reg_cov_2<-"C"
  outcome_data<-data[,c(var_treat,var_y,outcome_reg_cov_2)]
  outcome_reg_ATT_2<-lm(Y~., data=outcome_data)
  
  ATT_reg_est_2<-coef(outcome_reg_ATT_2)[var_treat]
  
  re_ATT<-summary(outcome_reg_ATT_2)
  re1_ATT<-as.data.frame(re_ATT$coefficients)
  outcome_reg_var_ATT_2<-re1_ATT$`Std. Error`[which(rownames(re1_ATT)==var_treat)]
  
  outcome_reg_ATT_lo<-ATT_reg_est_2-z*(outcome_reg_var_ATT_2)
  outcome_reg_ATT_up<-ATT_reg_est_2+z*(outcome_reg_var_ATT_2)
  coverage_outcome_reg_ATT_2<-ifelse((outcome_reg_ATT_lo<=true_ATT) & (outcome_reg_ATT_up>=true_ATT),1,0)
  
  ATT_2_result[repl,c(1:3)]<-c(ATT_reg_est_2,outcome_reg_var_ATT_2,coverage_outcome_reg_ATT_2)
  
  
  # ------------------------------
  ### IPW Estimator ---------------
  formula_2<-"A~B+C"
  weight_IPW_ATT_2<-weight_make(var_treat,cov_ps_2,"ATT",data,formula_2)
  result_obj_ATT_2<-lm(Y~A, weight=weight_IPW_ATT_2$untrimmed)
  IPW_ATT_2<-coef(result_obj_ATT_2)[var_treat]
  
  re_ATT<-summary(result_obj_ATT_2)
  re1_ATT<-as.data.frame(re_ATT$coefficients)
  
  Naive_IPW_var_ATT_2<-re1_ATT$`Std. Error`[which(rownames(re1_ATT)==var_treat)]
  IPW_Naive_lo_ATT<-IPW_ATT_2-z*(Naive_IPW_var_ATT_2)
  IPW_Naive_up_ATT<-IPW_ATT_2+z*(Naive_IPW_var_ATT_2)
  coverage_IPW_Naive_ATT_2<-ifelse((IPW_Naive_lo_ATT<=true_ATT) & (IPW_Naive_up_ATT>=true_ATT),1,0)
  
  sandwich_IPW_var_ATT_2<-sandwich_var_ATT(weight_IPW_ATT_2,result_obj_ATT_2,data,cov_ps_2,var_treat,var_y)
  IPW_sandwich_lo_ATT<-IPW_ATT_2-z*sqrt(sandwich_IPW_var_ATT_2)
  IPW_sandwich_up_ATT<-IPW_ATT_2+z*sqrt(sandwich_IPW_var_ATT_2)
  coverage_IPW_sandwich_ATT_2 <-ifelse((IPW_sandwich_lo_ATT<=true_ATT) & (IPW_sandwich_up_ATT>=true_ATT),1,0)
  
  ATT_2_result[repl,c(4:8)]<-c(IPW_ATT_2,Naive_IPW_var_ATT_2,coverage_IPW_Naive_ATT_2,
                               sandwich_IPW_var_ATT_2, coverage_IPW_sandwich_ATT_2)
  
  
  
  ### -------------------------------------------------------------------------
  ### DR ATT ---------------------------------------------------
  ps_formula_2<-"A~B+C"
  outcome_formula_2<-"Y~A+C"
  DR_outcome_cov_2<-"C"
  DR_ATT_2<-DR_estimator("ATT",data,var_treat,var_y,DR_outcome_cov_2,cov_ps_2,ps_formula_2,outcome_formula_2)$est
  
  DR_Naive_var_ATT_2<-DR_Naive_var_estimator("ATT",data,var_treat,var_y,DR_outcome_cov_2,cov_ps_2,ps_formula_2,outcome_formula_2)
  DR_Naive_lo_ATT<-DR_ATT_2-z*sqrt(DR_Naive_var_ATT_2)
  DR_Naive_up_ATT<-DR_ATT_2+z*sqrt(DR_Naive_var_ATT_2)
  coverage_DR_Naive_ATT_2<-ifelse((DR_Naive_lo_ATT<=true_ATT) & (DR_Naive_up_ATT>=true_ATT),1,0)
  
  j_hat<-J_function(data,data,var_treat,var_y,DR_outcome_cov_2,cov_ps_2,ps_formula_2,outcome_formula_2)
  crossprod<-crossprod_est_equ(data,data,var_treat,var_y,DR_outcome_cov_2,cov_ps_2,ps_formula_2,outcome_formula_2)
  cov_hat<-solve(j_hat)%*%crossprod%*%t(solve(j_hat))
  
  DR_sandwich_var_ATT_2<-(cov_hat[1,1]+cov_hat[2,2]-2*cov_hat[1,2])/nrow(data)
  
  DR_sandwich_lo_ATT<-DR_ATT_2-z*sqrt(DR_sandwich_var_ATT_2)
  DR_sandwich_up_ATT<-DR_ATT_2+z*sqrt(DR_sandwich_var_ATT_2)
  
  coverage_DR_sandwich_ATT_2<-ifelse((DR_sandwich_lo_ATT<=true_ATT) & (DR_sandwich_up_ATT>=true_ATT),1,0)
  
  ATT_2_result[repl,c(9:ncol(ATT_2_result))]<-c(DR_ATT_2,DR_Naive_var_ATT_2, coverage_DR_Naive_ATT_2,
                                                DR_sandwich_var_ATT_2, coverage_DR_sandwich_ATT_2)
  
  
  
  ### ---------------------------------------------------------
  ### ---------------------------------------------------------
  ### ---------------------------------------------------------
  ### Scenario 3 - ATT --------------------------------------------------------
  cov_ps_3<-"C"
  #cov_type<-c("continuous","continuous","binary")
  var_treat<-"A"
  var_y<-"Y"
  
  # ------------------------------------------------
  ### Outcome regression Estimator -----------------
  outcome_reg_cov_3<-c("B","C")
  outcome_data<-data[,c(var_treat,var_y,outcome_reg_cov_3)]
  outcome_reg_ATT_3<-lm(Y~., data=outcome_data)
  
  ATT_reg_est_3<-coef(outcome_reg_ATT_3)[var_treat]
  
  re_ATT<-summary(outcome_reg_ATT_3)
  re1_ATT<-as.data.frame(re_ATT$coefficients)
  outcome_reg_var_ATT_3<-re1_ATT$`Std. Error`[which(rownames(re1_ATT)==var_treat)]
  
  outcome_reg_ATT_lo<-ATT_reg_est_3-z*(outcome_reg_var_ATT_3)
  outcome_reg_ATT_up<-ATT_reg_est_3+z*(outcome_reg_var_ATT_3)
  coverage_outcome_reg_ATT_3<-ifelse((outcome_reg_ATT_lo<=true_ATT) & (outcome_reg_ATT_up>=true_ATT),1,0)
  
  ATT_3_result[repl,c(1:3)]<-c(ATT_reg_est_3,outcome_reg_var_ATT_3,coverage_outcome_reg_ATT_3)
  
  
  # ------------------------------
  ### IPW Estimator ---------------
  formula_3<-"A~C"
  weight_IPW_ATT_3<-weight_make(var_treat,cov_ps_3,"ATT",data,formula_3)
  result_obj_ATT_3<-lm(Y~A, weight=weight_IPW_ATT_3$untrimmed)
  IPW_ATT_3<-coef(result_obj_ATT_3)[var_treat]
  
  re_ATT<-summary(result_obj_ATT_3)
  re1_ATT<-as.data.frame(re_ATT$coefficients)
  
  Naive_IPW_var_ATT_3<-re1_ATT$`Std. Error`[which(rownames(re1_ATT)==var_treat)]
  IPW_Naive_lo_ATT<-IPW_ATT_3-z*(Naive_IPW_var_ATT_3)
  IPW_Naive_up_ATT<-IPW_ATT_3+z*(Naive_IPW_var_ATT_3)
  coverage_IPW_Naive_ATT_3<-ifelse((IPW_Naive_lo_ATT<=true_ATT) & (IPW_Naive_up_ATT>=true_ATT),1,0)
  
  sandwich_IPW_var_ATT_3<-sandwich_var_ATT(weight_IPW_ATT_3,result_obj_ATT_3,data,cov_ps_3,var_treat,var_y)
  IPW_sandwich_lo_ATT<-IPW_ATT_3-z*sqrt(sandwich_IPW_var_ATT_3)
  IPW_sandwich_up_ATT<-IPW_ATT_3+z*sqrt(sandwich_IPW_var_ATT_3)
  coverage_IPW_sandwich_ATT_3 <-ifelse((IPW_sandwich_lo_ATT<=true_ATT) & (IPW_sandwich_up_ATT>=true_ATT),1,0)
  
  ATT_3_result[repl,c(4:8)]<-c(IPW_ATT_3,Naive_IPW_var_ATT_3,coverage_IPW_Naive_ATT_3,
                               sandwich_IPW_var_ATT_3, coverage_IPW_sandwich_ATT_3)
  
  
  
  ### -------------------------------------------------------------------------
  ### DR ATT ---------------------------------------------------
  ps_formula_3<-"A~C"
  outcome_formula_3<-"Y~A+B+C"
  DR_outcome_cov_3<-c("B","C")
  DR_ATT_3<-DR_estimator("ATT",data,var_treat,var_y,DR_outcome_cov_3,cov_ps_3,ps_formula_3,outcome_formula_3)$est
  
  DR_Naive_var_ATT_3<-DR_Naive_var_estimator("ATT",data,var_treat,var_y,DR_outcome_cov_3,cov_ps_3,ps_formula_3,outcome_formula_3)
  DR_Naive_lo_ATT<-DR_ATT_3-z*sqrt(DR_Naive_var_ATT_3)
  DR_Naive_up_ATT<-DR_ATT_3+z*sqrt(DR_Naive_var_ATT_3)
  coverage_DR_Naive_ATT_3<-ifelse((DR_Naive_lo_ATT<=true_ATT) & (DR_Naive_up_ATT>=true_ATT),1,0)
  
  j_hat<-J_function(data,data,var_treat,var_y,DR_outcome_cov_3,cov_ps_3,ps_formula_3,outcome_formula_3)
  crossprod<-crossprod_est_equ(data,data,var_treat,var_y,DR_outcome_cov_3,cov_ps_3,ps_formula_3,outcome_formula_3)
  cov_hat<-solve(j_hat)%*%crossprod%*%t(solve(j_hat))
  
  DR_sandwich_var_ATT_3<-(cov_hat[1,1]+cov_hat[2,2]-2*cov_hat[1,2])/nrow(data)
  
  DR_sandwich_lo_ATT<-DR_ATT_3-z*sqrt(DR_sandwich_var_ATT_3)
  DR_sandwich_up_ATT<-DR_ATT_3+z*sqrt(DR_sandwich_var_ATT_3)
  
  coverage_DR_sandwich_ATT_3<-ifelse((DR_sandwich_lo_ATT<=true_ATT) & (DR_sandwich_up_ATT>=true_ATT),1,0)
  
  ATT_3_result[repl,c(9:ncol(ATT_3_result))]<-c(DR_ATT_3,DR_Naive_var_ATT_3, coverage_DR_Naive_ATT_3,
                                                DR_sandwich_var_ATT_3, coverage_DR_sandwich_ATT_3)
  
  
  
  ### ---------------------------------------------------------
  ### ---------------------------------------------------------
  ### ---------------------------------------------------------
  ### ---------------------------------------------------------
  ### Scenario 4 - ATT --------------------------------------------------------
  ### Scenario 3 - ATT --------------------------------------------------------
  cov_ps_4<-"C"
  #cov_type<-c("continuous","continuous","binary")
  var_treat<-"A"
  var_y<-"Y"
  
  # ------------------------------------------------
  ### Outcome regression Estimator -----------------
  outcome_reg_cov_4<-"C"
  outcome_data<-data[,c(var_treat,var_y,outcome_reg_cov_4)]
  outcome_reg_ATT_4<-lm(Y~., data=outcome_data)
  
  ATT_reg_est_4<-coef(outcome_reg_ATT_4)[var_treat]
  
  re_ATT<-summary(outcome_reg_ATT_4)
  re1_ATT<-as.data.frame(re_ATT$coefficients)
  outcome_reg_var_ATT_4<-re1_ATT$`Std. Error`[which(rownames(re1_ATT)==var_treat)]
  
  outcome_reg_ATT_lo<-ATT_reg_est_4-z*(outcome_reg_var_ATT_4)
  outcome_reg_ATT_up<-ATT_reg_est_4+z*(outcome_reg_var_ATT_4)
  coverage_outcome_reg_ATT_4<-ifelse((outcome_reg_ATT_lo<=true_ATT) & (outcome_reg_ATT_up>=true_ATT),1,0)
  
  ATT_4_result[repl,c(1:3)]<-c(ATT_reg_est_4,outcome_reg_var_ATT_4,coverage_outcome_reg_ATT_4)
  
  
  # ------------------------------
  ### IPW Estimator ---------------
  formula_4<-"A~C"
  weight_IPW_ATT_4<-weight_make(var_treat,cov_ps_4,"ATT",data,formula_4)
  result_obj_ATT_4<-lm(Y~A, weight=weight_IPW_ATT_4$untrimmed)
  IPW_ATT_4<-coef(result_obj_ATT_4)[var_treat]
  
  re_ATT<-summary(result_obj_ATT_4)
  re1_ATT<-as.data.frame(re_ATT$coefficients)
  
  Naive_IPW_var_ATT_4<-re1_ATT$`Std. Error`[which(rownames(re1_ATT)==var_treat)]
  IPW_Naive_lo_ATT<-IPW_ATT_4-z*(Naive_IPW_var_ATT_4)
  IPW_Naive_up_ATT<-IPW_ATT_4+z*(Naive_IPW_var_ATT_4)
  coverage_IPW_Naive_ATT_4<-ifelse((IPW_Naive_lo_ATT<=true_ATT) & (IPW_Naive_up_ATT>=true_ATT),1,0)
  
  sandwich_IPW_var_ATT_4<-sandwich_var_ATT(weight_IPW_ATT_4,result_obj_ATT_4,data,cov_ps_4,var_treat,var_y)
  IPW_sandwich_lo_ATT<-IPW_ATT_4-z*sqrt(sandwich_IPW_var_ATT_4)
  IPW_sandwich_up_ATT<-IPW_ATT_4+z*sqrt(sandwich_IPW_var_ATT_4)
  coverage_IPW_sandwich_ATT_4 <-ifelse((IPW_sandwich_lo_ATT<=true_ATT) & (IPW_sandwich_up_ATT>=true_ATT),1,0)
  
  ATT_4_result[repl,c(4:8)]<-c(IPW_ATT_4,Naive_IPW_var_ATT_4,coverage_IPW_Naive_ATT_4,
                               sandwich_IPW_var_ATT_4, coverage_IPW_sandwich_ATT_4)
  
  
  
  ### -------------------------------------------------------------------------
  ### DR ATT ---------------------------------------------------
  ps_formula_4<-"A~C"
  outcome_formula_4<-"Y~A+C"
  DR_outcome_cov_4<-"C"
  DR_ATT_4<-DR_estimator("ATT",data,var_treat,var_y,DR_outcome_cov_4,cov_ps_4,ps_formula_4,outcome_formula_4)$est
  
  DR_Naive_var_ATT_4<-DR_Naive_var_estimator("ATT",data,var_treat,var_y,DR_outcome_cov_4,cov_ps_4,ps_formula_4,outcome_formula_4)
  DR_Naive_lo_ATT<-DR_ATT_4-z*sqrt(DR_Naive_var_ATT_4)
  DR_Naive_up_ATT<-DR_ATT_4+z*sqrt(DR_Naive_var_ATT_4)
  coverage_DR_Naive_ATT_4<-ifelse((DR_Naive_lo_ATT<=true_ATT) & (DR_Naive_up_ATT>=true_ATT),1,0)
  
  j_hat<-J_function(data,data,var_treat,var_y,DR_outcome_cov_4,cov_ps_4,ps_formula_4,outcome_formula_4)
  crossprod<-crossprod_est_equ(data,data,var_treat,var_y,DR_outcome_cov_4,cov_ps_4,ps_formula_4,outcome_formula_4)
  cov_hat<-solve(j_hat)%*%crossprod%*%t(solve(j_hat))
  
  DR_sandwich_var_ATT_4<-(cov_hat[1,1]+cov_hat[2,2]-2*cov_hat[1,2])/nrow(data)
  
  DR_sandwich_lo_ATT<-DR_ATT_4-z*sqrt(DR_sandwich_var_ATT_4)
  DR_sandwich_up_ATT<-DR_ATT_4+z*sqrt(DR_sandwich_var_ATT_4)
  
  coverage_DR_sandwich_ATT_4<-ifelse((DR_sandwich_lo_ATT<=true_ATT) & (DR_sandwich_up_ATT>=true_ATT),1,0)
  
  ATT_4_result[repl,c(9:ncol(ATT_4_result))]<-c(DR_ATT_4,DR_Naive_var_ATT_4, coverage_DR_Naive_ATT_4,
                                                DR_sandwich_var_ATT_4, coverage_DR_sandwich_ATT_4)
}




# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
### Scenario1 - Performance record ------------------------------------------
A_RATIO<--8
A_VAR1<-0.7
A_VAR2<-0.7
A_VAR3<-0.5
true_ATT<-1

# Scenario1_result<-Scenario1_result(N, REPL)
# ATT_1_result<-as.data.frame(Scenario1_result$ATT)
source("./Simulation Scenario code/Scenario1_Simulation_Monte_Carlo_SD_Ratio_ver3.R")

Monte_Carlo_var<-Monte_result_1(A_RATIO, A_VAR1, A_VAR2, A_VAR3, 1000, 100000)
ATT_Monte_carlo<-as.data.frame(Monte_Carlo_var$ATT_var)
ATT_Monte_outcome_reg<-var(ATT_Monte_carlo$Outcome_reg_est) 
ATT_Monte_IPW<-var(ATT_Monte_carlo$IPW_est) 
ATT_Monte_DR<-var(ATT_Monte_carlo$DR_est) 

ATT_1_result<-as.data.frame(ATT_1_result)
#ATT_1_result<-read.csv("./Simulation Scenario code/ATT_Scenario_2_p_0.12_result.csv")

ATT_1_performance<-as.data.frame(matrix(NA,nrow=3,ncol=6))
colnames(ATT_1_performance)<-c("Bias","rMSE","Naive_var_coverage","Naive_var_SD_Ratio","Sandwich_robust_var_coverage",
                               "Sandwich_var_SD_Ratio")

row.names(ATT_1_performance)<-c("Outcome_reg","IPW","DR")

func_rMSE<-function(x){
  return(mean((x-true_ATT)^2))
}


### ---------------------------
ATT_1_performance[,"Bias"]<-apply(ATT_1_result[,grep("_est",colnames(ATT_1_result))],2,mean)-true_ATT

ATT_1_performance[,"rMSE"]<-apply(ATT_1_result[,grep("_est",colnames(ATT_1_result))],2,FUN=func_rMSE)

ATT_1_performance[,"Naive_var_coverage"]<-c(mean(ATT_1_result$Outcome_reg_coverage), mean(ATT_1_result$Naive_IPW_Coverage),
                                            mean(ATT_1_result$DR_Naive_coverage))

ATT_1_performance[,"Sandwich_robust_var_coverage"]<-c(NA, mean(ATT_1_result$Sandwich_IPW_Coverage, na.rm=TRUE),
                                                      mean(ATT_1_result$Sandwich_DR_coverage, na.rm=TRUE))

ATT_1_performance[,"Naive_var_SD_Ratio"]<-c(mean(ATT_1_result$Outcome_reg_var^2)/ATT_Monte_outcome_reg,
                                            mean(ATT_1_result$IPW_Naive_var^2)/ATT_Monte_IPW,
                                            mean(ATT_1_result$DR_Naive_var)/ATT_Monte_DR)

ATT_1_performance[,"Sandwich_var_SD_Ratio"]<-c(NA, mean(ATT_1_result$IPW_Sandwich_var)/ATT_Monte_IPW,
                                               mean(ATT_1_result$DR_Sandwich_var)/ATT_Monte_DR)







# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
### Sceanrio2 - Performance record ------------------------------------------
A_RATIO<--8
A_VAR1<-0.7
A_VAR2<-0.7
A_VAR3<-0.5
true_ATT<-1

# Sceanrio2_result<-Sceanrio2_result(N, REPL)
ATT_2_result<-as.data.frame(ATT_2_result)

source("./Simulation Scenario code/Scenario2_Simulation_Monte_Carlo_SD_Ratio_ver3.R")
Monte_Carlo_var<-Monte_result_2(A_RATIO, A_VAR1, A_VAR2, A_VAR3, 1000, 100000)
ATT_Monte_carlo<-as.data.frame(Monte_Carlo_var$ATT_var)
ATT_Monte_outcome_reg<-var(ATT_Monte_carlo$Outcome_reg_est) 
ATT_Monte_IPW<-var(ATT_Monte_carlo$IPW_est) 
ATT_Monte_DR<-var(ATT_Monte_carlo$DR_est) 

ATT_2_result<-as.data.frame(ATT_2_result)
#ATT_2_result<-read.csv("./Simulation Scenario code/ATT_Scenario_2_p_0.12_result.csv")

ATT_2_performance<-as.data.frame(matrix(NA,nrow=3,ncol=6))
colnames(ATT_2_performance)<-c("Bias","rMSE","Naive_var_coverage","Naive_var_SD_Ratio","Sandwich_robust_var_coverage",
                               "Sandwich_var_SD_Ratio")

row.names(ATT_2_performance)<-c("Outcome_reg","IPW","DR")

func_rMSE<-function(x){
  return(mean((x-true_ATT)^2))
}


### ---------------------------
ATT_2_performance[,"Bias"]<-apply(ATT_2_result[,grep("_est",colnames(ATT_2_result))],2,mean)-true_ATT

ATT_2_performance[,"rMSE"]<-apply(ATT_2_result[,grep("_est",colnames(ATT_2_result))],2,FUN=func_rMSE)

ATT_2_performance[,"Naive_var_coverage"]<-c(mean(ATT_2_result$Outcome_reg_coverage), mean(ATT_2_result$Naive_IPW_Coverage),
                                            mean(ATT_2_result$DR_Naive_coverage))

ATT_2_performance[,"Sandwich_robust_var_coverage"]<-c(NA, mean(ATT_2_result$Sandwich_IPW_Coverage, na.rm=TRUE),
                                                      mean(ATT_2_result$Sandwich_DR_coverage))

ATT_2_performance[,"Naive_var_SD_Ratio"]<-c(mean(ATT_2_result$Outcome_reg_var^2)/ATT_Monte_outcome_reg,
                                            mean(ATT_2_result$IPW_Naive_var^2)/ATT_Monte_IPW,
                                            mean(ATT_2_result$DR_Naive_var)/ATT_Monte_DR)

ATT_2_performance[,"Sandwich_var_SD_Ratio"]<-c(NA, mean(ATT_2_result$IPW_Sandwich_var)/ATT_Monte_IPW,
                                               mean(ATT_2_result$DR_Sandwich_var)/ATT_Monte_DR)


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
### Scenario 3 - Performance record ------------------------------------------
A_RATIO<--8
A_VAR1<-0.7
A_VAR2<-0.7
A_VAR3<-0.5
true_ATT<-1

ATT_3_result<-as.data.frame(ATT_3_result)
source("./Simulation Scenario code/Scenario3_Simulation_Monte_Carlo_SD_Ratio_ver3.R")

Monte_Carlo_var<-Monte_result_3(A_RATIO, A_VAR1, A_VAR2, A_VAR3, 1000, 100000)
ATT_Monte_carlo<-as.data.frame(Monte_Carlo_var$ATT_var)
ATT_Monte_outcome_reg<-var(ATT_Monte_carlo$Outcome_reg_est) 
ATT_Monte_IPW<-var(ATT_Monte_carlo$IPW_est) 
ATT_Monte_DR<-var(ATT_Monte_carlo$DR_est) 

ATT_3_result<-as.data.frame(ATT_3_result)
#ATT_3_result<-read.csv("./Simulation Scenario code/ATT_Scenario_2_p_0.12_result.csv")

ATT_3_performance<-as.data.frame(matrix(NA,nrow=3,ncol=6))
colnames(ATT_3_performance)<-c("Bias","rMSE","Naive_var_coverage","Naive_var_SD_Ratio","Sandwich_robust_var_coverage",
                               "Sandwich_var_SD_Ratio")

row.names(ATT_3_performance)<-c("Outcome_reg","IPW","DR")

func_rMSE<-function(x){
  return(mean((x-true_ATT)^2))
}


### ---------------------------
ATT_3_performance[,"Bias"]<-apply(ATT_3_result[,grep("_est",colnames(ATT_3_result))],2,mean)-true_ATT

ATT_3_performance[,"rMSE"]<-apply(ATT_3_result[,grep("_est",colnames(ATT_3_result))],2,FUN=func_rMSE)

ATT_3_performance[,"Naive_var_coverage"]<-c(mean(ATT_3_result$Outcome_reg_coverage), mean(ATT_3_result$Naive_IPW_Coverage),
                                            mean(ATT_3_result$DR_Naive_coverage))

ATT_3_performance[,"Sandwich_robust_var_coverage"]<-c(NA, mean(ATT_3_result$Sandwich_IPW_Coverage),
                                                      mean(ATT_3_result$Sandwich_DR_coverage))

ATT_3_performance[,"Naive_var_SD_Ratio"]<-c(mean(ATT_3_result$Outcome_reg_var^2)/ATT_Monte_outcome_reg,
                                            mean(ATT_3_result$IPW_Naive_var^2)/ATT_Monte_IPW,
                                            mean(ATT_3_result$DR_Naive_var)/ATT_Monte_DR)

ATT_3_performance[,"Sandwich_var_SD_Ratio"]<-c(NA, mean(ATT_3_result$IPW_Sandwich_var)/ATT_Monte_IPW,
                                               mean(ATT_3_result$DR_Sandwich_var)/ATT_Monte_DR)



# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
### Sceanrio4 - Performance record ------------------------------------------
A_RATIO<--8
A_VAR1<-0.7
A_VAR2<-0.7
A_VAR3<-0.5
true_ATT<-1

source("./Simulation Scenario code/Scenario4_Simulation_Monte_Carlo_SD_Ratio_ver3.R")

Monte_Carlo_var<-Monte_result_4(A_RATIO, A_VAR1, A_VAR2, A_VAR3, 1000, 100000)
ATT_Monte_carlo<-as.data.frame(Monte_Carlo_var$ATT_var)
ATT_Monte_outcome_reg<-var(ATT_Monte_carlo$Outcome_reg_est) 
ATT_Monte_IPW<-var(ATT_Monte_carlo$IPW_est) 
ATT_Monte_DR<-var(ATT_Monte_carlo$DR_est) 

ATT_4_result<-as.data.frame(ATT_4_result)
#ATT_4_result<-read.csv("./Simulation Scenario code/ATT_Scenario_2_p_0.12_result.csv")

ATT_4_performance<-as.data.frame(matrix(NA,nrow=3,ncol=6))
colnames(ATT_4_performance)<-c("Bias","rMSE","Naive_var_coverage","Naive_var_SD_Ratio","Sandwich_robust_var_coverage",
                               "Sandwich_var_SD_Ratio")

row.names(ATT_4_performance)<-c("Outcome_reg","IPW","DR")

func_rMSE<-function(x){
  return(mean((x-true_ATT)^2))
}


### ---------------------------
ATT_4_performance[,"Bias"]<-apply(ATT_4_result[,grep("_est",colnames(ATT_4_result))],2,mean)-true_ATT

ATT_4_performance[,"rMSE"]<-apply(ATT_4_result[,grep("_est",colnames(ATT_4_result))],2,FUN=func_rMSE)

ATT_4_performance[,"Naive_var_coverage"]<-c(mean(ATT_4_result$Outcome_reg_coverage), mean(ATT_4_result$Naive_IPW_Coverage),
                                            mean(ATT_4_result$DR_Naive_coverage))

ATT_4_performance[,"Sandwich_robust_var_coverage"]<-c(NA, mean(ATT_4_result$Sandwich_IPW_Coverage),
                                                      mean(ATT_4_result$Sandwich_DR_coverage))

ATT_4_performance[,"Naive_var_SD_Ratio"]<-c(mean(ATT_4_result$Outcome_reg_var^2)/ATT_Monte_outcome_reg,
                                            mean(ATT_4_result$IPW_Naive_var^2)/ATT_Monte_IPW,
                                            mean(ATT_4_result$DR_Naive_var)/ATT_Monte_DR)

ATT_4_performance[,"Sandwich_var_SD_Ratio"]<-c(NA, mean(ATT_4_result$IPW_Sandwich_var)/ATT_Monte_IPW,
                                               mean(ATT_4_result$DR_Sandwich_var)/ATT_Monte_DR)



### -------------------------------------------------------------------------
### 5. Store result table to excel file -------------------------------------
library(xlsx)
wb = createWorkbook()

sheet = createSheet(wb, "ATT_Performance")

addDataFrame(ATT_1_performance, sheet=sheet, startRow=1, row.names=TRUE) 
addDataFrame(ATT_2_performance, sheet=sheet, startRow=6, row.names=TRUE)
addDataFrame(ATT_3_performance, sheet=sheet, startRow=11, row.names=TRUE)
addDataFrame(ATT_4_performance, sheet=sheet, startRow=16, row.names=TRUE)

saveWorkbook(wb, "./Simulation Scenario code/Debugging_result/Ver0_Scenario_change_Ver4.p_0.015.xlsx")


