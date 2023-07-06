# Simulation Code ---------------------------------------------------
## Exposure ratio p = 0.12-----------------------------------------------------------------

## My plan -----------------------------------------------------------------
## 1. Each scenario, 1000th replication 

## 2. For each scenario code below process
##    --> data generating process 
##    --> Calculate three kinds of Estimator and store them
##    --> Calculate bias, rMSE and coverage prob

## 3. Make Result table
##    --> table name rule : "estimate name"_"scenario number"_"result or performance"
## -------------------------------------------------------------------------


### 1. import library & R source file ------------------------------------------------------
library(boot)

source("Simulation_Hardcoding_function.R")
source("Scenario1_make_result.R")

### 2. Get Simulation result -------------------------------------------------------------------------
N <- 100
REPL <- 1000
A_RATIO<--2 # p=0.53


Scenario1_result<-Scenario1_result(A_RATIO, N, REPL)


### -------------------------------------------------------------------------
### Store ATE result table -----------------------------------------------
ATE_1_result<-as.data.frame(Scenario1_result$ATE)
write.csv(ATE_1_result,file="ATE_Scenario_1_p_0.12_result.csv",row.names=FALSE)


### -------------------------------------------------------------------------
### Store ATT result table-----------------------------------------------
ATT_1_result<-as.data.frame(Scenario1_result$ATT)
write.csv(ATE_1_result,file="ATT_Scenario_1_p_0.12_result.csv",row.names=FALSE)
