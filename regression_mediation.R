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



#mediation analysis 
