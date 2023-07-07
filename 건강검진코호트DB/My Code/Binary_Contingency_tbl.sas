libname dir1 "/userdata12/room111/data_out/data_store";
run;

proc freq data=dir1.Mydata;
TABLE Asthma_result*COPD_result;
run;

proc freq data=dir1.Mydata;
TABLE Asthma_result*Atrial_fibrillation_result;
run;

proc freq data=dir1.Mydata;
TABLE Asthma_result*Thromboembolism_result;
run;

proc freq data=dir1.Mydata;
TABLE Asthma_result*CLD_result;
run;

proc freq data=dir1.Mydata;
TABLE Asthma_result*CKD_result;
run;

proc freq data=dir1.Mydata;
TABLE Asthma_result*CAD_result;
run;

proc freq data=dir1.Mydata;
TABLE Asthma_result*PVD_result;
run;

proc freq data=dir1.Mydata;
TABLE Asthma_result*Dementia_result;
run;

proc freq data=dir1.Mydata;
TABLE Asthma_result*GI_Disorders_result;
run;

proc freq data=dir1.Mydata;
TABLE Asthma_result*Hyperlipidemia_result;
run;

proc freq data=dir1.Mydata;
TABLE Asthma_result*Pneumonia_result;
run;

proc freq data=dir1.Mydata;
TABLE Asthma_result*Psychiatric_disorders_result;
run;

proc freq data=dir1.Mydata;
TABLE Asthma_result*Hypertension_result;
run;

proc freq data=dir1.Mydata;
TABLE Asthma_result*insulin_result;
run;

proc freq data=dir1.Mydata;
TABLE Asthma_result*oral_diabetes_result;
run;

proc freq data=dir1.Mydata;
TABLE Asthma_result*Non_statin_result;
run;

proc freq data=dir1.Mydata;
TABLE Asthma_result*statin_result;
run;

proc freq data=dir1.Mydata;
TABLE Asthma_result*Hypertension_renin_result;
run;

proc freq data=dir1.Mydata;
TABLE Asthma_result*Hypertension_except_RAS_result;
run;

proc freq data=dir1.Mydata;
TABLE Asthma_result*CNS_result;
run;

proc freq data=dir1.Mydata;
TABLE Asthma_result*Non_opioid_result;
run;

proc freq data=dir1.Mydata;
TABLE Asthma_result*Anticoagulants_result;
run;

proc freq data=dir1.Mydata;
TABLE Asthma_result*Antiplatelets_result;
run;

proc freq data=dir1.Mydata;
TABLE Asthma_result*immunosuppressant_result;
run;


/****************/
proc freq data=dir1.Mydata;
TABLE COPD_result*Atrial_fibrillation_result;
run;

proc freq data=dir1.Mydata;
TABLE COPD_result*Thromboembolism_result;
run;

proc freq data=dir1.Mydata;
TABLE COPD_result*CLD_result;
run;

proc freq data=dir1.Mydata;
TABLE COPD_result*CKD_result;
run;

proc freq data=dir1.Mydata;
TABLE COPD_result*CAD_result;
run;

proc freq data=dir1.Mydata;
TABLE COPD_result*PVD_result;
run;

proc freq data=dir1.Mydata;
TABLE COPD_result*Dementia_result;
run;

proc freq data=dir1.Mydata;
TABLE COPD_result*GI_Disorders_result;
run;

proc freq data=dir1.Mydata;
TABLE COPD_result*Hyperlipidemia_result;
run;

proc freq data=dir1.Mydata;
TABLE COPD_result*Pneumonia_result;
run;

proc freq data=dir1.Mydata;
TABLE COPD_result*Psychiatric_disorders_result;
run;

proc freq data=dir1.Mydata;
TABLE COPD_result*Hypertension_result;
run;

proc freq data=dir1.Mydata;
TABLE COPD_result*insulin_result;
run;

proc freq data=dir1.Mydata;
TABLE COPD_result*oral_diabetes_result;
run;

proc freq data=dir1.Mydata;
TABLE COPD_result*Non_statin_result;
run;

proc freq data=dir1.Mydata;
TABLE COPD_result*statin_result;
run;

proc freq data=dir1.Mydata;
TABLE COPD_result*Hypertension_renin_result;
run;

proc freq data=dir1.Mydata;
TABLE COPD_result*Hypertension_except_RAS_result;
run;

proc freq data=dir1.Mydata;
TABLE COPD_result*CNS_result;
run;

proc freq data=dir1.Mydata;
TABLE COPD_result*Non_opioid_result;
run;

proc freq data=dir1.Mydata;
TABLE COPD_result*Anticoagulants_result;
run;

proc freq data=dir1.Mydata;
TABLE COPD_result*Antiplatelets_result;
run;

proc freq data=dir1.Mydata;
TABLE COPD_result*immunosuppressant_result;
run;

/***********/
proc freq data=dir1.Mydata;
TABLE Atrial_fibrillation_result*Thromboembolism_result;
run;

proc freq data=dir1.Mydata;
TABLE Atrial_fibrillation_result*CLD_result;
run;

proc freq data=dir1.Mydata;
TABLE Atrial_fibrillation_result*CKD_result;
run;

proc freq data=dir1.Mydata;
TABLE Atrial_fibrillation_result*CAD_result;
run;

proc freq data=dir1.Mydata;
TABLE Atrial_fibrillation_result*PVD_result;
run;

proc freq data=dir1.Mydata;
TABLE Atrial_fibrillation_result*Dementia_result;
run;

proc freq data=dir1.Mydata;
TABLE Atrial_fibrillation_result*GI_Disorders_result;
run;

proc freq data=dir1.Mydata;
TABLE Atrial_fibrillation_result*Hyperlipidemia_result;
run;

proc freq data=dir1.Mydata;
TABLE Atrial_fibrillation_result*Pneumonia_result;
run;

proc freq data=dir1.Mydata;
TABLE Atrial_fibrillation_result*Psychiatric_disorders_result;
run;

proc freq data=dir1.Mydata;
TABLE Atrial_fibrillation_result*Hypertension_result;
run;

proc freq data=dir1.Mydata;
TABLE Atrial_fibrillation_result*insulin_result;
run;

proc freq data=dir1.Mydata;
TABLE Atrial_fibrillation_result*oral_diabetes_result;
run;

proc freq data=dir1.Mydata;
TABLE Atrial_fibrillation_result*Non_statin_result;
run;

proc freq data=dir1.Mydata;
TABLE Atrial_fibrillation_result*statin_result;
run;

proc freq data=dir1.Mydata;
TABLE Atrial_fibrillation_result*Hypertension_renin_result;
run;

proc freq data=dir1.Mydata;
TABLE Atrial_fibrillation_result*Hypertension_except_RAS_result;
run;

proc freq data=dir1.Mydata;
TABLE Atrial_fibrillation_result*CNS_result;
run;

proc freq data=dir1.Mydata;
TABLE Atrial_fibrillation_result*Non_opioid_result;
run;

proc freq data=dir1.Mydata;
TABLE Atrial_fibrillation_result*Anticoagulants_result;
run;

proc freq data=dir1.Mydata;
TABLE Atrial_fibrillation_result*Antiplatelets_result;
run;

proc freq data=dir1.Mydata;
TABLE Atrial_fibrillation_result*immunosuppressant_result;
run;


/****************/
proc freq data=dir1.Mydata;
TABLE Thromboembolism_result*CLD_result;
run;

proc freq data=dir1.Mydata;
TABLE Thromboembolism_result*CKD_result;
run;

proc freq data=dir1.Mydata;
TABLE Thromboembolism_result*CAD_result;
run;

proc freq data=dir1.Mydata;
TABLE Thromboembolism_result*PVD_result;
run;

proc freq data=dir1.Mydata;
TABLE Thromboembolism_result*Dementia_result;
run;

proc freq data=dir1.Mydata;
TABLE Thromboembolism_result*GI_Disorders_result;
run;

proc freq data=dir1.Mydata;
TABLE Thromboembolism_result*Hyperlipidemia_result;
run;

proc freq data=dir1.Mydata;
TABLE Thromboembolism_result*Pneumonia_result;
run;

proc freq data=dir1.Mydata;
TABLE Thromboembolism_result*Psychiatric_disorders_result;
run;

proc freq data=dir1.Mydata;
TABLE Thromboembolism_result*Hypertension_result;
run;

proc freq data=dir1.Mydata;
TABLE Thromboembolism_result*insulin_result;
run;

proc freq data=dir1.Mydata;
TABLE Thromboembolism_result*oral_diabetes_result;
run;

proc freq data=dir1.Mydata;
TABLE Thromboembolism_result*Non_statin_result;
run;

proc freq data=dir1.Mydata;
TABLE Thromboembolism_result*statin_result;
run;

proc freq data=dir1.Mydata;
TABLE Thromboembolism_result*Hypertension_renin_result;
run;

proc freq data=dir1.Mydata;
TABLE Thromboembolism_result*Hypertension_except_RAS_result;
run;

proc freq data=dir1.Mydata;
TABLE Thromboembolism_result*CNS_result;
run;

proc freq data=dir1.Mydata;
TABLE Thromboembolism_result*Non_opioid_result;
run;

proc freq data=dir1.Mydata;
TABLE Thromboembolism_result*Anticoagulants_result;
run;

proc freq data=dir1.Mydata;
TABLE Thromboembolism_result*Antiplatelets_result;
run;

proc freq data=dir1.Mydata;
TABLE Thromboembolism_result*immunosuppressant_result;
run;

/*************/

proc freq data=dir1.Mydata;
TABLE CLD_result*CKD_result;
run;

proc freq data=dir1.Mydata;
TABLE CLD_result*CAD_result;
run;

proc freq data=dir1.Mydata;
TABLE CLD_result*PVD_result;
run;

proc freq data=dir1.Mydata;
TABLE CLD_result*Dementia_result;
run;

proc freq data=dir1.Mydata;
TABLE CLD_result*GI_Disorders_result;
run;

proc freq data=dir1.Mydata;
TABLE CLD_result*Hyperlipidemia_result;
run;

proc freq data=dir1.Mydata;
TABLE CLD_result*Pneumonia_result;
run;

proc freq data=dir1.Mydata;
TABLE CLD_result*Psychiatric_disorders_result;
run;

proc freq data=dir1.Mydata;
TABLE CLD_result*Hypertension_result;
run;

proc freq data=dir1.Mydata;
TABLE CLD_result*insulin_result;
run;

proc freq data=dir1.Mydata;
TABLE CLD_result*oral_diabetes_result;
run;

proc freq data=dir1.Mydata;
TABLE CLD_result*Non_statin_result;
run;

proc freq data=dir1.Mydata;
TABLE CLD_result*statin_result;
run;

proc freq data=dir1.Mydata;
TABLE CLD_result*Hypertension_renin_result;
run;

proc freq data=dir1.Mydata;
TABLE CLD_result*Hypertension_except_RAS_result;
run;

proc freq data=dir1.Mydata;
TABLE CLD_result*CNS_result;
run;

proc freq data=dir1.Mydata;
TABLE CLD_result*Non_opioid_result;
run;

proc freq data=dir1.Mydata;
TABLE CLD_result*Anticoagulants_result;
run;

proc freq data=dir1.Mydata;
TABLE CLD_result*Antiplatelets_result;
run;

proc freq data=dir1.Mydata;
TABLE CLD_result*immunosuppressant_result;
run;


/*************/
proc freq data=dir1.Mydata;
TABLE CKD_result*CAD_result;
run;

proc freq data=dir1.Mydata;
TABLE CKD_result*PVD_result;
run;

proc freq data=dir1.Mydata;
TABLE CKD_result*Dementia_result;
run;

proc freq data=dir1.Mydata;
TABLE CKD_result*GI_Disorders_result;
run;

proc freq data=dir1.Mydata;
TABLE CKD_result*Hyperlipidemia_result;
run;

proc freq data=dir1.Mydata;
TABLE CKD_result*Pneumonia_result;
run;

proc freq data=dir1.Mydata;
TABLE CKD_result*Psychiatric_disorders_result;
run;

proc freq data=dir1.Mydata;
TABLE CKD_result*Hypertension_result;
run;

proc freq data=dir1.Mydata;
TABLE CKD_result*insulin_result;
run;

proc freq data=dir1.Mydata;
TABLE CKD_result*oral_diabetes_result;
run;

proc freq data=dir1.Mydata;
TABLE CKD_result*Non_statin_result;
run;

proc freq data=dir1.Mydata;
TABLE CKD_result*statin_result;
run;

proc freq data=dir1.Mydata;
TABLE CKD_result*Hypertension_renin_result;
run;

proc freq data=dir1.Mydata;
TABLE CKD_result*Hypertension_except_RAS_result;
run;

proc freq data=dir1.Mydata;
TABLE CKD_result*CNS_result;
run;

proc freq data=dir1.Mydata;
TABLE CKD_result*Non_opioid_result;
run;

proc freq data=dir1.Mydata;
TABLE CKD_result*Anticoagulants_result;
run;

proc freq data=dir1.Mydata;
TABLE CKD_result*Antiplatelets_result;
run;

proc freq data=dir1.Mydata;
TABLE CKD_result*immunosuppressant_result;
run;

/*******************/
proc freq data=dir1.Mydata;
TABLE CAD_result*PVD_result;
run;

proc freq data=dir1.Mydata;
TABLE CAD_result*Dementia_result;
run;

proc freq data=dir1.Mydata;
TABLE CAD_result*GI_Disorders_result;
run;

proc freq data=dir1.Mydata;
TABLE CAD_result*Hyperlipidemia_result;
run;

proc freq data=dir1.Mydata;
TABLE CAD_result*Pneumonia_result;
run;

proc freq data=dir1.Mydata;
TABLE CAD_result*Psychiatric_disorders_result;
run;

proc freq data=dir1.Mydata;
TABLE CAD_result*Hypertension_result;
run;

proc freq data=dir1.Mydata;
TABLE CAD_result*insulin_result;
run;

proc freq data=dir1.Mydata;
TABLE CAD_result*oral_diabetes_result;
run;

proc freq data=dir1.Mydata;
TABLE CAD_result*Non_statin_result;
run;

proc freq data=dir1.Mydata;
TABLE CAD_result*statin_result;
run;

proc freq data=dir1.Mydata;
TABLE CAD_result*Hypertension_renin_result;
run;

proc freq data=dir1.Mydata;
TABLE CAD_result*Hypertension_except_RAS_result;
run;

proc freq data=dir1.Mydata;
TABLE CAD_result*CNS_result;
run;

proc freq data=dir1.Mydata;
TABLE CAD_result*Non_opioid_result;
run;

proc freq data=dir1.Mydata;
TABLE CAD_result*Anticoagulants_result;
run;

proc freq data=dir1.Mydata;
TABLE CAD_result*Antiplatelets_result;
run;

proc freq data=dir1.Mydata;
TABLE CAD_result*immunosuppressant_result;
run;

/***************/
proc freq data=dir1.Mydata;
TABLE PVD_result*Dementia_result;
run;

proc freq data=dir1.Mydata;
TABLE PVD_result*GI_Disorders_result;
run;

proc freq data=dir1.Mydata;
TABLE PVD_result*Hyperlipidemia_result;
run;

proc freq data=dir1.Mydata;
TABLE PVD_result*Pneumonia_result;
run;

proc freq data=dir1.Mydata;
TABLE PVD_result*Psychiatric_disorders_result;
run;

proc freq data=dir1.Mydata;
TABLE PVD_result*Hypertension_result;
run;

proc freq data=dir1.Mydata;
TABLE PVD_result*insulin_result;
run;

proc freq data=dir1.Mydata;
TABLE PVD_result*oral_diabetes_result;
run;

proc freq data=dir1.Mydata;
TABLE PVD_result*Non_statin_result;
run;

proc freq data=dir1.Mydata;
TABLE PVD_result*statin_result;
run;

proc freq data=dir1.Mydata;
TABLE PVD_result*Hypertension_renin_result;
run;

proc freq data=dir1.Mydata;
TABLE PVD_result*Hypertension_except_RAS_result;
run;

proc freq data=dir1.Mydata;
TABLE PVD_result*CNS_result;
run;

proc freq data=dir1.Mydata;
TABLE PVD_result*Non_opioid_result;
run;

proc freq data=dir1.Mydata;
TABLE PVD_result*Anticoagulants_result;
run;

proc freq data=dir1.Mydata;
TABLE PVD_result*Antiplatelets_result;
run;

proc freq data=dir1.Mydata;
TABLE PVD_result*immunosuppressant_result;
run;

/*************/
proc freq data=dir1.Mydata;
TABLE Dementia_result*GI_Disorders_result;
run;

proc freq data=dir1.Mydata;
TABLE Dementia_result*Hyperlipidemia_result;
run;

proc freq data=dir1.Mydata;
TABLE Dementia_result*Pneumonia_result;
run;

proc freq data=dir1.Mydata;
TABLE Dementia_result*Psychiatric_disorders_result;
run;

proc freq data=dir1.Mydata;
TABLE Dementia_result*Hypertension_result;
run;

proc freq data=dir1.Mydata;
TABLE Dementia_result*insulin_result;
run;

proc freq data=dir1.Mydata;
TABLE Dementia_result*oral_diabetes_result;
run;

proc freq data=dir1.Mydata;
TABLE Dementia_result*Non_statin_result;
run;

proc freq data=dir1.Mydata;
TABLE Dementia_result*statin_result;
run;

proc freq data=dir1.Mydata;
TABLE Dementia_result*Hypertension_renin_result;
run;

proc freq data=dir1.Mydata;
TABLE Dementia_result*Hypertension_except_RAS_result;
run;

proc freq data=dir1.Mydata;
TABLE Dementia_result*CNS_result;
run;

proc freq data=dir1.Mydata;
TABLE Dementia_result*Non_opioid_result;
run;

proc freq data=dir1.Mydata;
TABLE Dementia_result*Anticoagulants_result;
run;

proc freq data=dir1.Mydata;
TABLE Dementia_result*Antiplatelets_result;
run;

proc freq data=dir1.Mydata;
TABLE Dementia_result*immunosuppressant_result;
run;

/**************/


proc freq data=dir1.Mydata;
TABLE GI_Disorders_result*Hyperlipidemia_result;
run;

proc freq data=dir1.Mydata;
TABLE GI_Disorders_result*Pneumonia_result;
run;

proc freq data=dir1.Mydata;
TABLE GI_Disorders_result*Psychiatric_disorders_result;
run;

proc freq data=dir1.Mydata;
TABLE GI_Disorders_result*Hypertension_result;
run;

proc freq data=dir1.Mydata;
TABLE GI_Disorders_result*insulin_result;
run;

proc freq data=dir1.Mydata;
TABLE GI_Disorders_result*oral_diabetes_result;
run;

proc freq data=dir1.Mydata;
TABLE GI_Disorders_result*Non_statin_result;
run;

proc freq data=dir1.Mydata;
TABLE GI_Disorders_result*statin_result;
run;

proc freq data=dir1.Mydata;
TABLE GI_Disorders_result*Hypertension_renin_result;
run;

proc freq data=dir1.Mydata;
TABLE GI_Disorders_result*Hypertension_except_RAS_result;
run;

proc freq data=dir1.Mydata;
TABLE GI_Disorders_result*CNS_result;
run;

proc freq data=dir1.Mydata;
TABLE GI_Disorders_result*Non_opioid_result;
run;

proc freq data=dir1.Mydata;
TABLE GI_Disorders_result*Anticoagulants_result;
run;

proc freq data=dir1.Mydata;
TABLE GI_Disorders_result*Antiplatelets_result;
run;

proc freq data=dir1.Mydata;
TABLE GI_Disorders_result*immunosuppressant_result;
run;

/********************/


proc freq data=dir1.Mydata;
TABLE Hyperlipidemia_result*Pneumonia_result;
run;

proc freq data=dir1.Mydata;
TABLE Hyperlipidemia_result*Psychiatric_disorders_result;
run;

proc freq data=dir1.Mydata;
TABLE Hyperlipidemia_result*Hypertension_result;
run;

proc freq data=dir1.Mydata;
TABLE Hyperlipidemia_result*insulin_result;
run;

proc freq data=dir1.Mydata;
TABLE Hyperlipidemia_result*oral_diabetes_result;
run;

proc freq data=dir1.Mydata;
TABLE Hyperlipidemia_result*Non_statin_result;
run;

proc freq data=dir1.Mydata;
TABLE Hyperlipidemia_result*statin_result;
run;

proc freq data=dir1.Mydata;
TABLE Hyperlipidemia_result*Hypertension_renin_result;
run;

proc freq data=dir1.Mydata;
TABLE Hyperlipidemia_result*Hypertension_except_RAS_result;
run;

proc freq data=dir1.Mydata;
TABLE Hyperlipidemia_result*CNS_result;
run;

proc freq data=dir1.Mydata;
TABLE Hyperlipidemia_result*Non_opioid_result;
run;

proc freq data=dir1.Mydata;
TABLE Hyperlipidemia_result*Anticoagulants_result;
run;

proc freq data=dir1.Mydata;
TABLE Hyperlipidemia_result*Antiplatelets_result;
run;

proc freq data=dir1.Mydata;
TABLE Hyperlipidemia_result*immunosuppressant_result;
run;

/**************/


proc freq data=dir1.Mydata;
TABLE Pneumonia_result*Psychiatric_disorders_result;
run;

proc freq data=dir1.Mydata;
TABLE Pneumonia_result*Hypertension_result;
run;

proc freq data=dir1.Mydata;
TABLE Pneumonia_result*insulin_result;
run;

proc freq data=dir1.Mydata;
TABLE Pneumonia_result*oral_diabetes_result;
run;

proc freq data=dir1.Mydata;
TABLE Pneumonia_result*Non_statin_result;
run;

proc freq data=dir1.Mydata;
TABLE Pneumonia_result*statin_result;
run;

proc freq data=dir1.Mydata;
TABLE Pneumonia_result*Hypertension_renin_result;
run;

proc freq data=dir1.Mydata;
TABLE Pneumonia_result*Hypertension_except_RAS_result;
run;

proc freq data=dir1.Mydata;
TABLE Pneumonia_result*CNS_result;
run;

proc freq data=dir1.Mydata;
TABLE Pneumonia_result*Non_opioid_result;
run;

proc freq data=dir1.Mydata;
TABLE Pneumonia_result*Anticoagulants_result;
run;

proc freq data=dir1.Mydata;
TABLE Pneumonia_result*Antiplatelets_result;
run;

proc freq data=dir1.Mydata;
TABLE Pneumonia_result*immunosuppressant_result;
run;

/******************/


proc freq data=dir1.Mydata;
TABLE Psychiatric_disorders_result*Hypertension_result;
run;

proc freq data=dir1.Mydata;
TABLE Psychiatric_disorders_result*insulin_result;
run;

proc freq data=dir1.Mydata;
TABLE Psychiatric_disorders_result*oral_diabetes_result;
run;

proc freq data=dir1.Mydata;
TABLE Psychiatric_disorders_result*Non_statin_result;
run;

proc freq data=dir1.Mydata;
TABLE Psychiatric_disorders_result*statin_result;
run;

proc freq data=dir1.Mydata;
TABLE Psychiatric_disorders_result*Hypertension_renin_result;
run;

proc freq data=dir1.Mydata;
TABLE Psychiatric_disorders_result*Hypertension_except_RAS_result;
run;

proc freq data=dir1.Mydata;
TABLE Psychiatric_disorders_result*CNS_result;
run;

proc freq data=dir1.Mydata;
TABLE Psychiatric_disorders_result*Non_opioid_result;
run;

proc freq data=dir1.Mydata;
TABLE Psychiatric_disorders_result*Anticoagulants_result;
run;

proc freq data=dir1.Mydata;
TABLE Psychiatric_disorders_result*Antiplatelets_result;
run;

proc freq data=dir1.Mydata;
TABLE Psychiatric_disorders_result*immunosuppressant_result;
run;

/******************/


proc freq data=dir1.Mydata;
TABLE Hypertension_result*insulin_result;
run;

proc freq data=dir1.Mydata;
TABLE Hypertension_result*oral_diabetes_result;
run;

proc freq data=dir1.Mydata;
TABLE Hypertension_result*Non_statin_result;
run;

proc freq data=dir1.Mydata;
TABLE Hypertension_result*statin_result;
run;

proc freq data=dir1.Mydata;
TABLE Hypertension_result*Hypertension_renin_result;
run;

proc freq data=dir1.Mydata;
TABLE Hypertension_result*Hypertension_except_RAS_result;
run;

proc freq data=dir1.Mydata;
TABLE Hypertension_result*CNS_result;
run;

proc freq data=dir1.Mydata;
TABLE Hypertension_result*Non_opioid_result;
run;

proc freq data=dir1.Mydata;
TABLE Hypertension_result*Anticoagulants_result;
run;

proc freq data=dir1.Mydata;
TABLE Hypertension_result*Antiplatelets_result;
run;

proc freq data=dir1.Mydata;
TABLE Hypertension_result*immunosuppressant_result;
run;


/*****************/
proc freq data=dir1.Mydata;
TABLE insulin_result*oral_diabetes_result;
run;

proc freq data=dir1.Mydata;
TABLE insulin_result*Non_statin_result;
run;

proc freq data=dir1.Mydata;
TABLE insulin_result*statin_result;
run;

proc freq data=dir1.Mydata;
TABLE insulin_result*Hypertension_renin_result;
run;

proc freq data=dir1.Mydata;
TABLE insulin_result*Hypertension_except_RAS_result;
run;

proc freq data=dir1.Mydata;
TABLE insulin_result*CNS_result;
run;

proc freq data=dir1.Mydata;
TABLE insulin_result*Non_opioid_result;
run;

proc freq data=dir1.Mydata;
TABLE insulin_result*Anticoagulants_result;
run;

proc freq data=dir1.Mydata;
TABLE insulin_result*Antiplatelets_result;
run;

proc freq data=dir1.Mydata;
TABLE insulin_result*immunosuppressant_result;
run;


/***************/
proc freq data=dir1.Mydata;
TABLE oral_diabetes_result*Non_statin_result;
run;

proc freq data=dir1.Mydata;
TABLE oral_diabetes_result*statin_result;
run;

proc freq data=dir1.Mydata;
TABLE oral_diabetes_result*Hypertension_renin_result;
run;

proc freq data=dir1.Mydata;
TABLE oral_diabetes_result*Hypertension_except_RAS_result;
run;

proc freq data=dir1.Mydata;
TABLE oral_diabetes_result*CNS_result;
run;

proc freq data=dir1.Mydata;
TABLE oral_diabetes_result*Non_opioid_result;
run;

proc freq data=dir1.Mydata;
TABLE oral_diabetes_result*Anticoagulants_result;
run;

proc freq data=dir1.Mydata;
TABLE oral_diabetes_result*Antiplatelets_result;
run;

proc freq data=dir1.Mydata;
TABLE oral_diabetes_result*immunosuppressant_result;
run;

/***************/

proc freq data=dir1.Mydata;
TABLE Non_statin_result*statin_result;
run;

proc freq data=dir1.Mydata;
TABLE Non_statin_result*Hypertension_renin_result;
run;

proc freq data=dir1.Mydata;
TABLE Non_statin_result*Hypertension_except_RAS_result;
run;

proc freq data=dir1.Mydata;
TABLE Non_statin_result*CNS_result;
run;

proc freq data=dir1.Mydata;
TABLE Non_statin_result*Non_opioid_result;
run;

proc freq data=dir1.Mydata;
TABLE Non_statin_result*Anticoagulants_result;
run;

proc freq data=dir1.Mydata;
TABLE Non_statin_result*Antiplatelets_result;
run;

proc freq data=dir1.Mydata;
TABLE Non_statin_result*immunosuppressant_result;
run;

/**************/


proc freq data=dir1.Mydata;
TABLE statin_result*Hypertension_renin_result;
run;

proc freq data=dir1.Mydata;
TABLE statin_result*Hypertension_except_RAS_result;
run;

proc freq data=dir1.Mydata;
TABLE statin_result*CNS_result;
run;

proc freq data=dir1.Mydata;
TABLE statin_result*Non_opioid_result;
run;

proc freq data=dir1.Mydata;
TABLE statin_result*Anticoagulants_result;
run;

proc freq data=dir1.Mydata;
TABLE statin_result*Antiplatelets_result;
run;

proc freq data=dir1.Mydata;
TABLE statin_result*immunosuppressant_result;
run;

/*************/
proc freq data=dir1.Mydata;
TABLE Hypertension_renin_result*Hypertension_except_RAS_result;
run;

proc freq data=dir1.Mydata;
TABLE Hypertension_renin_result*CNS_result;
run;

proc freq data=dir1.Mydata;
TABLE Hypertension_renin_result*Non_opioid_result;
run;

proc freq data=dir1.Mydata;
TABLE Hypertension_renin_result*Anticoagulants_result;
run;

proc freq data=dir1.Mydata;
TABLE Hypertension_renin_result*Antiplatelets_result;
run;

proc freq data=dir1.Mydata;
TABLE Hypertension_renin_result*immunosuppressant_result;
run;

/************/


proc freq data=dir1.Mydata;
TABLE Hypertension_except_RAS_result*CNS_result;
run;

proc freq data=dir1.Mydata;
TABLE Hypertension_except_RAS_result*Non_opioid_result;
run;

proc freq data=dir1.Mydata;
TABLE Hypertension_except_RAS_result*Anticoagulants_result;
run;

proc freq data=dir1.Mydata;
TABLE Hypertension_except_RAS_result*Antiplatelets_result;
run;

proc freq data=dir1.Mydata;
TABLE Hypertension_except_RAS_result*immunosuppressant_result;
run;

/*************/


proc freq data=dir1.Mydata;
TABLE CNS_result*Non_opioid_result;
run;

proc freq data=dir1.Mydata;
TABLE CNS_result*Anticoagulants_result;
run;

proc freq data=dir1.Mydata;
TABLE CNS_result*Antiplatelets_result;
run;

proc freq data=dir1.Mydata;
TABLE CNS_result*immunosuppressant_result;
run;

/**********/
proc freq data=dir1.Mydata;
TABLE Non_opioid_result*Anticoagulants_result;
run;

proc freq data=dir1.Mydata;
TABLE Non_opioid_result*Antiplatelets_result;
run;

proc freq data=dir1.Mydata;
TABLE Non_opioid_result*immunosuppressant_result;
run;

/**********/
proc freq data=dir1.Mydata;
TABLE Anticoagulants_result*Antiplatelets_result;
run;

proc freq data=dir1.Mydata;
TABLE Anticoagulants_result*immunosuppressant_result;
run;

proc freq data=dir1.Mydata;
TABLE Antiplatelets_result*immunosuppressant_result;
run;
