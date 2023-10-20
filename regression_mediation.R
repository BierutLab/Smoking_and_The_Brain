#global, subregion regression analyses + mediation analysis
#ever smoked, pack years, time since smoking cessation, and PRS (in different thresholds)

#then regression it is!

#time since smoking cessation is for former smokers only
#PRS is divided into (smokers,non-smokers,all sample->record n)

#for additional analysis: pack years for all samples (0 for never smokers)
prs_smoking_idp_all_confounds_imputed_pc_py0 <- prs_smoking_idp_all_confounds_imputed_pc %>% mutate(all_packyears = ifelse(ever_daily_smoked==0,0,packyears_imputed))
#all_packyears 

data <- final_data_0711 #or whatever final data you have 
attach(data)
#check if the total brain measures are correctly input in for loop *might not be 117:122 in some cases*

#daily smoking
write.table(paste("Name","Estimate","Std. Error", "t value", "P-value", sep="\t"), "ds_global_aseg_0711.txt", sep="\n", row.names=F, col.names=F, quote=F)
for (i in 117:122) {
  result1 <- lm(scale(data[,i]) ~ ever_daily_smoked + week_drinks + waist_hip + age_completed_imputed + Income + bmi+ dbp + sbp + site1_sex + site1_date + site1_age + site1_rfMRI_motion + site1_tfMRI_motion + site1_age_2 + site1_date_2 + site1_age_sex + site2_sex + site2_date + site2_age + site2_rfMRI_motion + site2_tfMRI_motion + site2_age_2 + site2_date_2 + site2_age_sex + site3_sex + site3_date + site3_age+ site3_rfMRI_motion + site3_tfMRI_motion + site3_age_2 + site3_date_2 + site3_age_sex + n_22009_0_1 + n_22009_0_10 + n_22009_0_2 + n_22009_0_3 + n_22009_0_4+n_22009_0_5+n_22009_0_6+n_22009_0_7+n_22009_0_8+n_22009_0_9 + stress + pa1 + pa2 + diabetes + cancer + other_diagnosis + vascular + site1_head_size + site2_head_size + site3_head_size)
  result2 <- summary.lm(result1)$coefficients[2,]
  write.table(paste(colnames(data)[i],result2[1],result2[2],result2[3],result2[4],sep="\t"), "ds_global_aseg_0711.txt", sep="\n", row.names=F, col.names=F, quote=F, append=T)
}

#pack years
write.table(paste("Name","Estimate","Std. Error", "t value", "P-value", sep="\t"), "py_global_aseg_0711.txt", sep="\n", row.names=F, col.names=F, quote=F)
for (i in 117:122) {
  result1 <- lm(scale(data[,i]) ~ packyears_imputed + week_drinks + waist_hip + age_completed_imputed + Income + bmi+ dbp + sbp + site1_sex + site1_date + site1_age + site1_rfMRI_motion + site1_tfMRI_motion + site1_age_2 + site1_date_2 + site1_age_sex + site2_sex + site2_date + site2_age + site2_rfMRI_motion + site2_tfMRI_motion + site2_age_2 + site2_date_2 + site2_age_sex + site3_sex + site3_date + site3_age+ site3_rfMRI_motion + site3_tfMRI_motion + site3_age_2 + site3_date_2 + site3_age_sex + n_22009_0_1 + n_22009_0_10 + n_22009_0_2 + n_22009_0_3 + n_22009_0_4+n_22009_0_5+n_22009_0_6+n_22009_0_7+n_22009_0_8+n_22009_0_9 + stress + pa1 + pa2 + diabetes + cancer + other_diagnosis + vascular + site1_head_size + site2_head_size + site3_head_size)
  result2 <- summary.lm(result1)$coefficients[2,]
  write.table(paste(colnames(data)[i],result2[1],result2[2],result2[3],result2[4],sep="\t"), "py_global_aseg_0711.txt", sep="\n", row.names=F, col.names=F, quote=F, append=T)
}

#time since smoking cessation
data_cessation <- data %>% filter(time_since_cessation != 0) %>% filter(!is.na(time_since_cessation))

attach(data_cessation)
write.table(paste("Name","Estimate","Std. Error", "t value", "P-value", sep="\t"), "ts_global_aseg_0712.txt", sep="\n", row.names=F, col.names=F, quote=F)
for (i in 117:122) {
  result1 <- lm(scale(data_cessation[,i]) ~ time_since_cessation + packyears_imputed + week_drinks + waist_hip + age_completed_imputed + Income + bmi+ dbp + sbp + site1_sex + site1_date + site1_age + site1_rfMRI_motion + site1_tfMRI_motion + site1_age_2 + site1_date_2 + site1_age_sex + site2_sex + site2_date + site2_age + site2_rfMRI_motion + site2_tfMRI_motion + site2_age_2 + site2_date_2 + site2_age_sex + site3_sex + site3_date + site3_age+ site3_rfMRI_motion + site3_tfMRI_motion + site3_age_2 + site3_date_2 + site3_age_sex + n_22009_0_1 + n_22009_0_10 + n_22009_0_2 + n_22009_0_3 + n_22009_0_4+n_22009_0_5+n_22009_0_6+n_22009_0_7+n_22009_0_8+n_22009_0_9 + stress + pa1 + pa2 + diabetes + cancer + other_diagnosis + vascular + site1_head_size + site2_head_size + site3_head_size)
  result2 <- summary.lm(result1)$coefficients[2,]
  write.table(paste(colnames(data)[i],result2[1],result2[2],result2[3],result2[4],sep="\t"), "ts_global_aseg_0712.txt", sep="\n", row.names=F, col.names=F, quote=F, append=T)
}


#prs
#repeat the below commands for all PRS thresholds!
data_prs_all <- data %>% filter(!is.na(Pt_0.5))
attach(data_prs_all)

write.table(paste("Name","Estimate","Std. Error", "t value", "P-value", sep="\t"), "prsall5e8_global_aseg_0711.txt", sep="\n", row.names=F, col.names=F, quote=F)
for (i in 117:122) {
  result1 <- lm(scale(data_prs_all[,i]) ~ scale(Pt_5e.08) + week_drinks + waist_hip + age_completed_imputed + Income + bmi+ dbp + sbp + site1_sex + site1_date + site1_age + site1_rfMRI_motion + site1_tfMRI_motion + site1_age_2 + site1_date_2 + site1_age_sex + site2_sex + site2_date + site2_age + site2_rfMRI_motion + site2_tfMRI_motion + site2_age_2 + site2_date_2 + site2_age_sex + site3_sex + site3_date + site3_age+ site3_rfMRI_motion + site3_tfMRI_motion + site3_age_2 + site3_date_2 + site3_age_sex + n_22009_0_1 + n_22009_0_10 + n_22009_0_2 + n_22009_0_3 + n_22009_0_4+n_22009_0_5+n_22009_0_6+n_22009_0_7+n_22009_0_8+n_22009_0_9 + stress + pa1 + pa2 + diabetes + cancer + other_diagnosis + vascular + site1_head_size + site2_head_size + site3_head_size)
  result2 <- summary.lm(result1)$coefficients[2,]
  write.table(paste(colnames(data)[i],result2[1],result2[2],result2[3],result2[4],sep="\t"), "prsall5e8_global_aseg_0711.txt", sep="\n", row.names=F, col.names=F, quote=F, append=T)
}

#never_smokers
data_prs_never_smokers <- data_prs_all%>% filter(ever_daily_smoked =="0")
attach(data_prs_never_smokers)

write.table(paste("Name","Estimate","Std. Error", "t value", "P-value", sep="\t"), "prsnever5e8_global_aseg_0712.txt", sep="\n", row.names=F, col.names=F, quote=F)
for (i in 117:122) {
  result1 <- lm(scale(data_prs_never_smokers[,i]) ~ scale(Pt_5e.08) + week_drinks + waist_hip + age_completed_imputed + Income + bmi+ dbp + sbp + site1_sex + site1_date + site1_age + site1_rfMRI_motion + site1_tfMRI_motion + site1_age_2 + site1_date_2 + site1_age_sex + site2_sex + site2_date + site2_age + site2_rfMRI_motion + site2_tfMRI_motion + site2_age_2 + site2_date_2 + site2_age_sex + site3_sex + site3_date + site3_age+ site3_rfMRI_motion + site3_tfMRI_motion + site3_age_2 + site3_date_2 + site3_age_sex + n_22009_0_1 + n_22009_0_10 + n_22009_0_2 + n_22009_0_3 + n_22009_0_4+n_22009_0_5+n_22009_0_6+n_22009_0_7+n_22009_0_8+n_22009_0_9 + stress + pa1 + pa2 + diabetes + cancer + other_diagnosis + vascular + site1_head_size + site2_head_size + site3_head_size)
  result2 <- summary.lm(result1)$coefficients[2,]
  write.table(paste(colnames(data)[i],result2[1],result2[2],result2[3],result2[4],sep="\t"), "prsnever5e8_global_aseg_0712.txt", sep="\n", row.names=F, col.names=F, quote=F, append=T)
}

#ever_smokers
data_prs_daily_smokers <- data_prs_all%>% filter(ever_daily_smoked =="1")
attach(data_prs_daily_smokers)

write.table(paste("Name","Estimate","Std. Error", "t value", "P-value", sep="\t"), "prsdaily5e8_global_aseg_0712.txt", sep="\n", row.names=F, col.names=F, quote=F)
for (i in 117:122) {
  result1 <- lm(scale(data_prs_daily_smokers[,i]) ~ scale(Pt_5e.08) + week_drinks + waist_hip + age_completed_imputed + Income + bmi+ dbp + sbp + site1_sex + site1_date + site1_age + site1_rfMRI_motion + site1_tfMRI_motion + site1_age_2 + site1_date_2 + site1_age_sex + site2_sex + site2_date + site2_age + site2_rfMRI_motion + site2_tfMRI_motion + site2_age_2 + site2_date_2 + site2_age_sex + site3_sex + site3_date + site3_age+ site3_rfMRI_motion + site3_tfMRI_motion + site3_age_2 + site3_date_2 + site3_age_sex + n_22009_0_1 + n_22009_0_10 + n_22009_0_2 + n_22009_0_3 + n_22009_0_4+n_22009_0_5+n_22009_0_6+n_22009_0_7+n_22009_0_8+n_22009_0_9 + stress + pa1 + pa2 + diabetes + cancer + other_diagnosis + vascular + site1_head_size + site2_head_size + site3_head_size)
  result2 <- summary.lm(result1)$coefficients[2,]
  write.table(paste(colnames(data)[i],result2[1],result2[2],result2[3],result2[4],sep="\t"), "prsdaily5e8_global_aseg_0712.txt", sep="\n", row.names=F, col.names=F, quote=F, append=T)
}


#----------main global analyses completed----------#

#subregions
#do everything first -> then correct for the total volume
#then separately correct for global thickness with thickness stuff

#this is removing total aseg measures (to avoid double adding them)
#keeping total white tho
final_data_0713 <- final_data_0711 %>% select(-c(117:121)) #or any other data file produced using sample_processing.R and merged with global volumes 

final_data_0713_aseg <- merge(final_data_0713,IDPs_45k_ASEG,by="eid",all.x = T)

final_data_0713_aseg_dkt <- merge(final_data_0713_aseg,IDPs_45k_DKT,by="eid",all.x = T)

#let's save it 
write.table(final_data_0713_aseg_dkt,file="final_data_0713_aseg_dkt.txt",row.names = F)

data <- final_data_0713_aseg_dkt
attach(data)

#without total adjustment 
write.table(paste("Name","Estimate","Std. Error", "t value", "P-value", sep="\t"), "subregion_raw_0713.txt", sep="\n", row.names=F, col.names=F, quote=F)
for (i in 118:402) {
  result1 <- lm(scale(data[,i]) ~ ever_daily_smoked + week_drinks + waist_hip + age_completed_imputed + Income + bmi+ dbp + sbp + site1_sex + site1_date + site1_age + site1_rfMRI_motion + site1_tfMRI_motion + site1_age_2 + site1_date_2 + site1_age_sex + site2_sex + site2_date + site2_age + site2_rfMRI_motion + site2_tfMRI_motion + site2_age_2 + site2_date_2 + site2_age_sex + site3_sex + site3_date + site3_age+ site3_rfMRI_motion + site3_tfMRI_motion + site3_age_2 + site3_date_2 + site3_age_sex + n_22009_0_1 + n_22009_0_10 + n_22009_0_2 + n_22009_0_3 + n_22009_0_4+n_22009_0_5+n_22009_0_6+n_22009_0_7+n_22009_0_8+n_22009_0_9 + stress + pa1 + pa2 + diabetes + cancer + other_diagnosis + vascular + site1_head_size + site2_head_size + site3_head_size)
  result2 <- summary.lm(result1)$coefficients[2,]
  write.table(paste(colnames(data)[i],result2[1],result2[2],result2[3],result2[4],sep="\t"), "subregion_raw_0713.txt", sep="\n", row.names=F, col.names=F, quote=F, append=T)
}

#with total adjustment
write.table(paste("Name","Estimate","Std. Error", "t value", "P-value", sep="\t"), "subregion_adjusted_0713_totalz.txt", sep="\n", row.names=F, col.names=F, quote=F)
for (i in 118:402) {
  result1 <- lm(scale(data[,i]) ~ ever_daily_smoked + scale(X26514.2.0) + week_drinks + waist_hip + age_completed_imputed + Income + bmi+ dbp + sbp + site1_sex + site1_date + site1_age + site1_rfMRI_motion + site1_tfMRI_motion + site1_age_2 + site1_date_2 + site1_age_sex + site2_sex + site2_date + site2_age + site2_rfMRI_motion + site2_tfMRI_motion + site2_age_2 + site2_date_2 + site2_age_sex + site3_sex + site3_date + site3_age+ site3_rfMRI_motion + site3_tfMRI_motion + site3_age_2 + site3_date_2 + site3_age_sex + n_22009_0_1 + n_22009_0_10 + n_22009_0_2 + n_22009_0_3 + n_22009_0_4+n_22009_0_5+n_22009_0_6+n_22009_0_7+n_22009_0_8+n_22009_0_9 + stress + pa1 + pa2 + diabetes + cancer + other_diagnosis + vascular + site1_head_size + site2_head_size + site3_head_size)
  result2 <- summary.lm(result1)$coefficients[2,]
  write.table(paste(colnames(data)[i],result2[1],result2[2],result2[3],result2[4],sep="\t"), "subregion_adjusted_0713_totalz.txt", sep="\n", row.names=F, col.names=F, quote=F, append=T)
}

#global thickness measures (left/right hemisphere different)
global_thickness <- IDPs_45k_swi_total_measures %>% select(eid,X26755.2.0,X26856.2.0)
final_data_0713_aseg_dkt <- merge(final_data_0713_aseg_dkt,global_thickness,by="eid",all.x = T)
View(final_data_0713_aseg_dkt)

write.table(final_data_0713_aseg_dkt,file="final_data_0713_aseg_dkt_thickness.txt",row.names = F)

data <- final_data_0713_aseg_dkt
attach(data)

#find where the mean thickness IDPs are 
grep("X27174.2.0",colnames(final_data_0713_aseg_dkt)) #mean thickness of caudalanteriorcingulate (left hemisphere) -> where the first group starts 
grep("X27204.2.0",colnames(final_data_0713_aseg_dkt)) #first group ends
grep("X27267.2.0",colnames(final_data_0713_aseg_dkt)) #Mean thickness of caudalanteriorcingulate (right hemisphere) -> where the second group starts
grep("X27297.2.0",colnames(final_data_0713_aseg_dkt)) #second group ends

#left hemisphere
write.table(paste("Name","Estimate","Std. Error", "t value", "P-value", sep="\t"), "left1_thickness_adjusted_0713.txt", sep="\n", row.names=F, col.names=F, quote=F)
for (i in 248:278) {
  result1 <- lm(scale(data[,i]) ~ ever_daily_smoked + X26755.2.0 + week_drinks + waist_hip + age_completed_imputed + Income + bmi+ dbp + sbp + site1_sex + site1_date + site1_age + site1_rfMRI_motion + site1_tfMRI_motion + site1_age_2 + site1_date_2 + site1_age_sex + site2_sex + site2_date + site2_age + site2_rfMRI_motion + site2_tfMRI_motion + site2_age_2 + site2_date_2 + site2_age_sex + site3_sex + site3_date + site3_age+ site3_rfMRI_motion + site3_tfMRI_motion + site3_age_2 + site3_date_2 + site3_age_sex + n_22009_0_1 + n_22009_0_10 + n_22009_0_2 + n_22009_0_3 + n_22009_0_4+n_22009_0_5+n_22009_0_6+n_22009_0_7+n_22009_0_8+n_22009_0_9 + stress + pa1 + pa2 + diabetes + cancer + other_diagnosis + vascular + site1_head_size + site2_head_size + site3_head_size)
  result2 <- summary.lm(result1)$coefficients[2,]
  write.table(paste(colnames(data)[i],result2[1],result2[2],result2[3],result2[4],sep="\t"), "left1_thickness_adjusted_0713.txt", sep="\n", row.names=F, col.names=F, quote=F, append=T)
}

#right hemisphere
write.table(paste("Name","Estimate","Std. Error", "t value", "P-value", sep="\t"), "right1_thickness_adjusted_0713.txt", sep="\n", row.names=F, col.names=F, quote=F)
for (i in 341:371) {
  result1 <- lm(scale(data[,i]) ~ ever_daily_smoked + X26856.2.0 + week_drinks + waist_hip + age_completed_imputed + Income + bmi+ dbp + sbp + site1_sex + site1_date + site1_age + site1_rfMRI_motion + site1_tfMRI_motion + site1_age_2 + site1_date_2 + site1_age_sex + site2_sex + site2_date + site2_age + site2_rfMRI_motion + site2_tfMRI_motion + site2_age_2 + site2_date_2 + site2_age_sex + site3_sex + site3_date + site3_age+ site3_rfMRI_motion + site3_tfMRI_motion + site3_age_2 + site3_date_2 + site3_age_sex + n_22009_0_1 + n_22009_0_10 + n_22009_0_2 + n_22009_0_3 + n_22009_0_4+n_22009_0_5+n_22009_0_6+n_22009_0_7+n_22009_0_8+n_22009_0_9 + stress + pa1 + pa2 + diabetes + cancer + other_diagnosis + vascular + site1_head_size + site2_head_size + site3_head_size)
  result2 <- summary.lm(result1)$coefficients[2,]
  write.table(paste(colnames(data)[i],result2[1],result2[2],result2[3],result2[4],sep="\t"), "right1_thickness_adjusted_0713.txt", sep="\n", row.names=F, col.names=F, quote=F, append=T)
}

#mediation analysis 
#Source: https://data.library.virginia.edu/introduction-to-mediation-analysis/
library(dplyr)
library(mediation)

aseg_final_0526_test <- merge(aseg_final_0526,prs_original,by.x="n_eid",by.y="IID",all.x=T)

#Select/omit the dataset
IDP_mediation <- aseg_final_0526_test %>% select(eid,n_eid,ever_daily_smoked,Pt_0.5.y,Pt_0.05.y,Pt_0.005,Pt_5e.08,Pt_5e.07,Pt_5e.06,Pt_5e.05,Pt_0.0005,Pt_0.005,week_drinks,waist_hip,age_completed_imputed,Income,bmi,dbp,sbp,site1_sex, site1_date , site1_age , site1_head_size , site1_rfMRI_motion , site1_tfMRI_motion , site1_age_2 , site1_date_2 , site1_age_sex , site2_sex , site2_date , site2_age , site2_head_size , site2_rfMRI_motion , site2_tfMRI_motion , site2_age_2 , site2_date_2 , site2_age_sex , site3_sex , site3_date , site3_age , site3_head_size , site3_rfMRI_motion , site3_tfMRI_motion , site3_age_2 , site3_date_2 , site3_age_sex, n_22009_0_1, n_22009_0_2, n_22009_0_3, n_22009_0_4,n_22009_0_5, n_22009_0_6, n_22009_0_7, n_22009_0_8, n_22009_0_9, n_22009_0_10, stress, vascular, diabetes, cancer, other_diagnosis,pa1,pa2,323:421,2:215)

IDP_mediation <- na.omit(IDP_mediation)
View(IDP_mediation)

IDP_mediation$ever_daily_smoked <- as.numeric(IDP_mediation$ever_daily_smoked)

data = IDP_mediation
attach(data)

#let's z-score the datasets (Pt_0.5.y and total grey matter volume 25005)
#separate the columns and use merge
data <- data %>% mutate(Pt_0.5=scale(SI_Pt_0.5),gmv=scale(n_25005_2_0))

data <- data %>% mutate(Pt_0.5=as.numeric(Pt_0.5),gmv2=as.numeric(gmv))

#convert mediator to numeric
data$ever_daily_smoked <- as.numeric(data$ever_daily_smoked)

library(mediation)
set.seed(2014)

model_0 <- lm(gmv2~ Pt0.5z + week_drinks + waist_hip + age_completed_imputed + Income + bmi+ dbp + sbp + site1_sex + site1_date + site1_age + site1_head_size + site1_rfMRI_motion + site1_tfMRI_motion + site1_age_2 + site1_date_2 + site1_age_sex + site2_sex + site2_date + site2_age + site2_head_size + site2_rfMRI_motion + site2_tfMRI_motion + site2_age_2 + site2_date_2 + site2_age_sex + site3_sex + site3_date + site3_age + site3_head_size + site3_rfMRI_motion + site3_tfMRI_motion + site3_age_2 + site3_date_2 + site3_age_sex+ n_22009_0_1 + n_22009_0_2 + n_22009_0_3 + n_22009_0_4 + n_22009_0_5 + n_22009_0_6 + n_22009_0_7 + n_22009_0_8 + n_22009_0_9 + n_22009_0_10 + stress+ diabetes+other_diagnosis+cancer+pa1+pa2+vascular,data=data)
summary(model_0)

model_1 <- lm(ever_daily_smoked ~ Pt0.5z + gmv2 + week_drinks + waist_hip + age_completed_imputed + Income + bmi+ dbp + sbp + site1_sex + site1_date + site1_age + site1_head_size + site1_rfMRI_motion + site1_tfMRI_motion + site1_age_2 + site1_date_2 + site1_age_sex + site2_sex + site2_date + site2_age + site2_head_size + site2_rfMRI_motion + site2_tfMRI_motion + site2_age_2 + site2_date_2 + site2_age_sex + site3_sex + site3_date + site3_age + site3_head_size + site3_rfMRI_motion + site3_tfMRI_motion + site3_age_2 + site3_date_2 + site3_age_sex+ n_22009_0_1 + n_22009_0_2 + n_22009_0_3 + n_22009_0_4 + n_22009_0_5 + n_22009_0_6 + n_22009_0_7 + n_22009_0_8 + n_22009_0_9 + n_22009_0_10+ stress+ diabetes+other_diagnosis+cancer+pa1+pa2+vascular,data=data)
summary(model_1)

#data$n_25005_2_0 (grey matter)

model_2 <- lm(gmv2 ~ Pt0.5z + ever_daily_smoked + week_drinks + waist_hip + age_completed_imputed + Income + bmi+ dbp + sbp + site1_sex + site1_date + site1_age + site1_head_size + site1_rfMRI_motion + site1_tfMRI_motion + site1_age_2 + site1_date_2 + site1_age_sex + site2_sex + site2_date + site2_age + site2_head_size + site2_rfMRI_motion + site2_tfMRI_motion + site2_age_2 + site2_date_2 + site2_age_sex + site3_sex + site3_date + site3_age + site3_head_size + site3_rfMRI_motion + site3_tfMRI_motion + site3_age_2 + site3_date_2 + site3_age_sex+ n_22009_0_1 + n_22009_0_2 + n_22009_0_3 + n_22009_0_4 + n_22009_0_5 + n_22009_0_6 + n_22009_0_7 + n_22009_0_8 + n_22009_0_9 + n_22009_0_10+ stress+ diabetes+other_diagnosis+cancer+pa1+pa2+vascular,data=data)
summary(model_2)

#mediation results 
#this is smoking (mediator) and brain (outcome)
#Good god save me 
results <- mediate(model_1,model_2,treat='wmv2',mediator='ever_daily_smoked',robustSE=TRUE,sims=5)
summary(results)

