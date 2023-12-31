#Smoking analysis for UKB_40K

#-------data files-------#
#1. Imaging confounds
#2. Smoking variables 
#3. Non-imaging confounds (+alcohol variable)
#4. IDP_40K

#set the input/output path

#Mac
output_path <- "~/Library/CloudStorage/Box-Box/UKB_Phenotypes/Association_output"

#Lab
output_path <- "C:/Users/yoonhoochang/Box/UKB_Phenotypes/Association_output"

#Download functions

library(dplyr) # Run install.packages("tidyverse") to get this
library(tidyr) # Run install.packages("tidyr") to get this
library(ggplot2)
library("reshape") # Run install.packages("reshape") to get this

#N tracker (tracking the sample size)
n_tracker <- data.frame(matrix(ncol=3,nrow=0))
colnames(n_tracker) <- c("Step","MRI","N")

#Import Imaging confounds (step 1)

library(readr)
#Mac
IDP_Confounds_45k_01_25_23 <- read_csv("~/Library/CloudStorage/Box-Box/UKB_IDPs_new/IDP_Confounds_45k_01.25.23.csv")

#Lab
IDP_Confounds_45k_01_25_23 <- read_csv("C:/Users/yoonhoochang/Box/UKB_IDPs_new/IDP_Confounds_45k_01.25.23.csv")

#record the sample size 
n_tracker <- rbind(n_tracker,data.frame("Step"="Imaging cohort","MRI"="all","N"=nrow(IDP_Confounds_45k_01_25_23)))

#Match ID between ukb48123 and ukb47627 (phenotype and imaging applications)
#if this doesn't work, download manually
ID_map_img_2023_04_14 <- read_csv("C:/Users/yoonhoochang/Box/2023 UKB Data/ID_map_img_2023-04-14.csv")

ID_key <- ID_map_img_2023_04_14

imaging_confounds <- IDP_Confounds_45k_01_25_23

imaging_confounds <- right_join(ID_key, imaging_confounds, by = "eid")
imaging_confounds <- imaging_confounds %>% filter(! is.na(pheno_eid))
imaging_confounds <- imaging_confounds %>% select(! eid) %>% dplyr::rename("n_eid" = "pheno_eid")

n_tracker <- rbind(n_tracker,data.frame("Step"="ID_matched","MRI"="all","N"=nrow(imaging_confounds)))

#Consent withdrawn IDs removed
#it's already removed but just to check

ukb48123_Consent_Withdrawn_IDs <- read.table("~/Library/CloudStorage/Box-Box/UKB_Phenotypes/ukb48123_Consent_Withdrawn_IDs.txt", quote="\"", comment.char="")

imaging_confounds <- imaging_confounds %>% filter(!n_eid %in% ukb48123_Consent_Withdrawn_IDs$V1)

n_tracker <- rbind(n_tracker,data.frame("Step"="Consent_withdrawn_removed","MRI"="all","N"=nrow(imaging_confounds)))

#Relatedness removed

ukb48123_kinship <- read.csv("C:/Users/yoonhoochang/Box/UKB_Phenotypes/ukb48123_kinship.txt", sep="")
eid_40k_kinship <- imaging_confounds %>% select(n_eid) 
relatedness <- ukb48123_kinship %>% filter(ukb48123_kinship$ID1 %in% eid_40k_kinship$n_eid) 
relatedness <- relatedness %>% filter(relatedness$ID2 %in% eid_40k_kinship$n_eid)
imaging_confounds <- imaging_confounds %>% filter(!n_eid %in% relatedness$ID1)

n_tracker <- rbind(n_tracker,data.frame("Step"="Relatedness_removed","MRI"="all","N"=nrow(imaging_confounds)))

#Neurological conditions removed
#Remove those with neurological conditions
#Credits to the amazing Vera Thornton#

library("reshape")
library(kableExtra)
library(viridis)

#neuro_code: the list and the code for all neurological diseases/conditions we exclude

neuro_codes <- data.frame((rbind(
  c("Dementia", 1263),
  c("Parkinsons", 1262),
  c("Chronic degenerative neurological", 1258),
  c("Guillan-Barre syndrome", 1256),
  c("Multiple sclerosis", 1261),
  c("Other demyelinating disease", 1397),
  c("Stroke or ischaemic stroke", 1081),
  c("Brain cancer", 1032),
  c("Brain hemorrhage", 1491),
  c("Brain / intracranial abcess", 1245),
  c("Cerebral aneurysm", 1425),
  c("Cerebral palsy", 1433),
  c("Encephalitis", 1246),
  c("Epilepsy", 1264),
  c("Head injury", 1266),
  c("Nervous system infection", 1244),
  c("Ischaemic stroke", 1583),
  c("Meningeal cancer", 1031),
  c("Meningioma", 1659),
  c("Meningitis", 1247),
  c("Motor neuron disease", 1259),
  c("Neurological disease / trauma", 1240),
  c("Spina bifida", 1524),
  c("Subdural hematoma", 1083),
  c("Subarachnoid hemorrhage", 1086),
  c("Transient ischemic attack", 1082))))

neuro_codes <- neuro_codes %>% dplyr::rename("condition" = X1, "code" = X2) %>% mutate(code = formatC(code, format = "d"))

View(neuro_codes)

#neuro codes -> neuro disease
neuro_disease_raw <- read_csv("C:/Users/yoonhoochang/Box/UKB_Phenotypes/neuro_disease.csv")
all_disease <- neuro_disease_raw

all_disease <- all_disease %>% filter(n_eid %in% imaging_confounds$n_eid)

neuro_disease <- all_disease %>% pivot_longer(cols = -n_eid, names_to = "field", values_to = "code") %>% dplyr::select(-field) %>% filter(!is.na(code)) %>% mutate(code = formatC(code, format = "d")) %>% filter(code %in% neuro_codes$code) %>% left_join(neuro_codes, by = "code")

#count the individuals and record type of disease
neuro_disease %>% group_by(condition) %>% tally() %>% mutate(percent = n / nrow(imaging_confounds)*100) %>% mutate(percent = formatC(percent, digits = 4, format = "f")) %>% arrange(desc(n)) %>% kable(caption = paste0("Prevalence of neurological conditions in Imaging cohort\nN=",nrow(imaging_confounds), ", N affected=", length(unique(neuro_disease$n_eid)))) %>% kable_classic(full_width = F, html_font = "Arial")

#remove these individuals 
imaging_confounds <- imaging_confounds %>% filter(!n_eid %in% neuro_disease$n_eid)

#record the number
n_tracker <- rbind(n_tracker,data.frame("Step"="Neuro_disease_removed","MRI"="all","N"=nrow(imaging_confounds)))

#Imaging confounds processing

imaging_confounds <- imaging_confounds %>%
  dplyr::rename("sex" = "31-0.0") %>%
  dplyr::rename("date" = "53-2.0") %>%
  dplyr::rename("site" = "54-2.0") %>%
  dplyr::rename("age" = "21003-2.0") %>%
  dplyr::rename("head_size" = "25000-2.0") %>%
  dplyr::rename("rfMRI_motion" = "25741-2.0") %>%
  dplyr::rename("tfMRI_motion" = "25742-2.0") %>%
  dplyr::rename("MHQ_date" = "20400-0.0")

# Select the confounds to include in the analysis
imaging_confounds <- imaging_confounds %>%
  select(n_eid, sex, date, site, age, head_size, rfMRI_motion, tfMRI_motion)

#Convert dates to a numeric. In r this is the number of days since January 1 1970
#https://statistics.berkeley.edu/computing/faqs/dates-and-times-r

imaging_confounds <- imaging_confounds %>%
  mutate(date = as.Date(date)) %>%
  mutate(date = as.numeric(date))

#Scale the confounds using the median and median absolute deviation * 1.48. Do this for all columns except sex and site. If this is done to sex it turns into NaN and Inf
imaging_confounds <- imaging_confounds %>%
  mutate_at(c("date", "age", "head_size", "rfMRI_motion", "tfMRI_motion"), ~scale(.x, center = median(.x, na.rm = TRUE), scale = (mad(.x, , na.rm = TRUE) * 1.48)))

#Remove outliers greater than 8
imaging_confounds <- imaging_confounds %>%
  mutate(date = ifelse(abs(date) > 8, NA, date)) %>%
  mutate(age = ifelse(abs(age) > 8, NA, age)) %>%
  mutate(head_size = ifelse(abs(head_size) > 8, NA, head_size)) %>%
  mutate(rfMRI_motion = ifelse(abs(rfMRI_motion) > 8, NA, rfMRI_motion)) %>%
  mutate(tfMRI_motion = ifelse(abs(tfMRI_motion) > 8, NA, tfMRI_motion))

#split in sites
unique(imaging_confounds$site)

site1_conf <- imaging_confounds %>%
  filter(site == "11025") %>%
  mutate(site = "site1")

site2_conf <- imaging_confounds %>%
  filter(site == "11026") %>%
  mutate(site = "site2")

site3_conf <- imaging_confounds %>%
  filter(site == "11027") %>%
  mutate(site = "site3")

#Replace all NA and missing with the median for the site
site1_conf <- site1_conf %>%
  mutate(date = ifelse(is.na(date), median(site1_conf$date, na.rm = TRUE), date)) %>%
  mutate(age = ifelse(is.na(age), median(site1_conf$age, na.rm = TRUE), age)) %>%
  mutate(head_size = ifelse(is.na(head_size), median(site1_conf$head_size, na.rm = TRUE), head_size)) %>%
  mutate(rfMRI_motion = ifelse(is.na(rfMRI_motion), median(site1_conf$rfMRI_motion, na.rm = TRUE), rfMRI_motion)) %>%
  mutate(tfMRI_motion = ifelse(is.na(tfMRI_motion), median(site1_conf$tfMRI_motion, na.rm = TRUE), tfMRI_motion))


site2_conf <- site2_conf %>%
  mutate(date = ifelse(is.na(date), median(site2_conf$date, na.rm = TRUE), date)) %>%
  mutate(age = ifelse(is.na(age), median(site2_conf$age, na.rm = TRUE), age)) %>%
  mutate(head_size = ifelse(is.na(head_size), median(site2_conf$head_size, na.rm = TRUE), head_size)) %>%
  mutate(rfMRI_motion = ifelse(is.na(rfMRI_motion), median(site2_conf$rfMRI_motion, na.rm = TRUE), rfMRI_motion)) %>%
  mutate(tfMRI_motion = ifelse(is.na(tfMRI_motion), median(site2_conf$tfMRI_motion, na.rm = TRUE), tfMRI_motion))


site3_conf <- site3_conf %>%
  mutate(date = ifelse(is.na(date), median(site3_conf$date, na.rm = TRUE), date)) %>%
  mutate(age = ifelse(is.na(age), median(site3_conf$age, na.rm = TRUE), age)) %>%
  mutate(head_size = ifelse(is.na(head_size), median(site3_conf$head_size, na.rm = TRUE), head_size)) %>%
  mutate(rfMRI_motion = ifelse(is.na(rfMRI_motion), median(site3_conf$rfMRI_motion, na.rm = TRUE), rfMRI_motion)) %>%
  mutate(tfMRI_motion = ifelse(is.na(tfMRI_motion), median(site3_conf$tfMRI_motion, na.rm = TRUE), tfMRI_motion))

#Calculate the z-scores by site so that mean is zero and sd is 1
site1_conf <- site1_conf %>%
  mutate_at(c("date", "age", "head_size", "rfMRI_motion", "tfMRI_motion"), ~scale(.x, center = TRUE, scale = TRUE))

site2_conf <- site2_conf %>%
  mutate_at(c("date", "age", "head_size", "rfMRI_motion", "tfMRI_motion"), ~scale(.x, center = TRUE, scale = TRUE))

site3_conf <- site3_conf %>%
  mutate_at(c("date", "age", "head_size", "rfMRI_motion", "tfMRI_motion"), ~scale(.x, center = TRUE, scale = TRUE))

#check mean/sd
mean(site1_conf$head_size) #close to 0
sd(site1_conf$head_size)

#age^2 and date^2
site1_conf <- site1_conf %>%
  mutate(age_2 = age^2) %>%
  mutate(date_2 = date^2)

site2_conf <- site2_conf %>%
  mutate(age_2 = age^2) %>%
  mutate(date_2 = date^2)

site3_conf <- site3_conf %>%
  mutate(age_2 = age^2) %>%
  mutate(date_2 = date^2)

#age*sex
site1_conf <- site1_conf %>%
  mutate(sex = scale(sex, center = TRUE, scale = TRUE)) %>%
  mutate(age_sex = age * sex)

site2_conf <- site2_conf %>%
  mutate(sex = scale(sex, center = TRUE, scale = TRUE)) %>%
  mutate(age_sex = age * sex)

site3_conf <- site3_conf %>%
  mutate(sex = scale(sex, center = TRUE, scale = TRUE)) %>%
  mutate(age_sex = age * sex)

#stitch tables together
site1_conf <- site1_conf %>%
  dplyr::rename("site1_sex" = "sex") %>%
  dplyr::rename("site1_date" = "date") %>%
  dplyr::rename("site1_date_2" = "date_2") %>%
  dplyr::rename("site1_age" = "age") %>%
  dplyr::rename("site1_age_2" = "age_2") %>%
  dplyr::rename("site1_age_sex" = "age_sex") %>%
  dplyr::rename("site1_head_size" = "head_size") %>%
  dplyr::rename("site1_rfMRI_motion" = "rfMRI_motion") %>%
  dplyr::rename("site1_tfMRI_motion" = "tfMRI_motion")

site2_conf <- site2_conf %>%
  dplyr::rename("site2_sex" = "sex") %>%
  dplyr::rename("site2_date" = "date") %>%
  dplyr::rename("site2_date_2" = "date_2") %>%
  dplyr::rename("site2_age" = "age") %>%
  dplyr::rename("site2_age_2" = "age_2") %>%
  dplyr::rename("site2_age_sex" = "age_sex") %>%
  dplyr::rename("site2_head_size" = "head_size") %>%
  dplyr::rename("site2_rfMRI_motion" = "rfMRI_motion") %>%
  dplyr::rename("site2_tfMRI_motion" = "tfMRI_motion")


site3_conf <- site3_conf %>%
  dplyr::rename("site3_sex" = "sex") %>%
  dplyr::rename("site3_date" = "date") %>%
  dplyr::rename("site3_date_2" = "date_2") %>%
  dplyr::rename("site3_age" = "age") %>%
  dplyr::rename("site3_age_2" = "age_2") %>%
  dplyr::rename("site3_age_sex" = "age_sex") %>%
  dplyr::rename("site3_head_size" = "head_size") %>%
  dplyr::rename("site3_rfMRI_motion" = "rfMRI_motion") %>%
  dplyr::rename("site3_tfMRI_motion" = "tfMRI_motion")

#mutate to add 0s
site1_conf <- site1_conf %>%
  mutate("site2_sex" = 0) %>%
  mutate("site2_date" = 0) %>%
  mutate("site2_age" = 0) %>%
  mutate("site2_date_2" = 0) %>%
  mutate("site2_age_2" = 0) %>%
  mutate("site2_age_sex" = 0) %>%
  mutate("site2_head_size" = 0) %>%
  mutate("site2_rfMRI_motion" = 0) %>%
  mutate("site2_tfMRI_motion" = 0) %>%
  mutate("site2_age_2" = 0) %>%
  mutate("site2_date_2" = 0) %>%
  mutate("site2_age_sex" = 0) %>%
  mutate("site3_sex" = 0) %>%
  mutate("site3_date" = 0) %>%
  mutate("site3_age" = 0) %>%
  mutate("site3_date_2" = 0) %>%
  mutate("site3_age_2" = 0) %>%
  mutate("site3_age_sex" = 0) %>%
  mutate("site3_head_size" = 0) %>%
  mutate("site3_rfMRI_motion" = 0) %>%
  mutate("site3_tfMRI_motion" = 0) %>%
  mutate("site3_age_2" = 0) %>%
  mutate("site3_date_2" = 0) %>%
  mutate("site3_age_sex" = 0)

site2_conf <- site2_conf %>%
  mutate("site1_sex" = 0) %>%
  mutate("site1_date" = 0) %>%
  mutate("site1_age" = 0) %>%
  mutate("site1_date_2" = 0) %>%
  mutate("site1_age_2" = 0) %>%
  mutate("site1_age_sex" = 0) %>%
  mutate("site1_head_size" = 0) %>%
  mutate("site1_rfMRI_motion" = 0) %>%
  mutate("site1_tfMRI_motion" = 0) %>%
  mutate("site1_age_2" = 0) %>%
  mutate("site1_date_2" = 0) %>%
  mutate("site1_age_sex" = 0) %>%
  mutate("site3_sex" = 0) %>%
  mutate("site3_date" = 0) %>%
  mutate("site3_age" = 0) %>%
  mutate("site3_date_2" = 0) %>%
  mutate("site3_age_2" = 0) %>%
  mutate("site3_age_sex" = 0) %>%
  mutate("site3_head_size" = 0) %>%
  mutate("site3_rfMRI_motion" = 0) %>%
  mutate("site3_tfMRI_motion" = 0) %>%
  mutate("site3_age_2" = 0) %>%
  mutate("site3_date_2" = 0) %>%
  mutate("site3_age_sex" = 0)


site3_conf <- site3_conf %>%
  mutate("site1_sex" = 0) %>%
  mutate("site1_date" = 0) %>%
  mutate("site1_age" = 0) %>%
  mutate("site1_date_2" = 0) %>%
  mutate("site1_age_2" = 0) %>%
  mutate("site1_age_sex" = 0) %>%
  mutate("site1_head_size" = 0) %>%
  mutate("site1_rfMRI_motion" = 0) %>%
  mutate("site1_tfMRI_motion" = 0) %>%
  mutate("site1_age_2" = 0) %>%
  mutate("site1_date_2" = 0) %>%
  mutate("site1_age_sex" = 0) %>%
  mutate("site2_sex" = 0) %>%
  mutate("site2_date" = 0) %>%
  mutate("site2_age" = 0) %>%
  mutate("site2_date_2" = 0) %>%
  mutate("site2_age_2" = 0) %>%
  mutate("site2_age_sex" = 0) %>%
  mutate("site2_head_size" = 0) %>%
  mutate("site2_rfMRI_motion" = 0) %>%
  mutate("site2_tfMRI_motion" = 0) %>%
  mutate("site2_age_2" = 0) %>%
  mutate("site2_date_2" = 0) %>%
  mutate("site2_age_sex" = 0)

#merge everything
processed_confounds <- rbind(site1_conf, site2_conf, site3_conf)

#----------Step 1. Imaging confounds done----------#

#Now step 2. Smoking variable processing
#Download the data #Lab version
baseline_smoking_4_5_23 <- read.csv("C:/Users/yoonhoochang/Box/2023 UKB Data/baseline_smoking_4_5_23.csv")
imaging_smoking_4_5_23 <- read_csv("C:/Users/yoonhoochang/Box/2023 UKB Data/imaging_smoking_4_5_23.csv")
ukbio_smoking <- merge(baseline_smoking_4_5_23,imaging_smoking_4_5_23,by="n_eid")

# Ever daily smoked: 1) current daily smoking (1 for n_1239_0_0 and n_1239_2_0), 2) former daily smoking (1 for n_1249_0_0 and n_1249_2_0), 3) current -> former (1 for n_1239_0_0 and 1 for n_1249_2_0), 4) former -> current (1 for n_1249_0_0 and 1 for n_1239_2_0)

current_daily <- ukbio_smoking %>% 
  filter(n_1239_0_0 == "1" & n_1239_2_0 == "1") %>% mutate(ever_daily_smoked = "1") 

former_daily <- ukbio_smoking %>% 
  filter(n_1249_0_0=="1" & n_1249_2_0 == "1") %>% mutate(ever_daily_smoked = "1")

current_former_daily <- ukbio_smoking %>% 
  filter(n_1239_0_0 == "1" & n_1249_2_0 == "1") %>% mutate(ever_daily_smoked = "1")

former_current_daily <- ukbio_smoking %>% 
  filter(n_1249_0_0=="1" & n_1239_2_0 == "1") %>% mutate(ever_daily_smoked = "1")

ever_daily_smoker <- rbind(current_daily,former_daily,current_former_daily,former_current_daily)

# Never smoked: 1) Never smoked (4 for n_1249_0_0 and n_1249_2_0), 2) Less than 100 (0 for n_2644_0_0 and n_2644_2_0), 3) less than 100 -> never (0 for n_2644_0_0 and 4 for n_1249_2_0), 4) never -> less than 100 (4 for n_1249_0_0 and 0 for n_2644_2_0)

never_smoked <- ukbio_smoking %>% 
  filter(n_1249_0_0=="4" & n_1249_2_0 == "4") %>% mutate(ever_daily_smoked = "0")

less_than100 <- ukbio_smoking %>% 
  filter(n_2644_0_0=="0" & n_2644_2_0 == "0") %>% mutate(ever_daily_smoked = "0")

less_than100_never <- ukbio_smoking %>% 
  filter(n_2644_0_0=="0" & n_1249_2_0 == "4") %>% mutate(ever_daily_smoked = "0")

never_lessthan100 <- ukbio_smoking %>% 
  filter(n_1249_0_0=="4" & n_2644_2_0 == "0") %>% mutate(ever_daily_smoked = "0")

never_daily_smoked <- rbind(never_smoked,less_than100,less_than100_never,never_lessthan100)

smoking_status <- rbind(ever_daily_smoker,never_daily_smoked) %>% select(n_eid,ever_daily_smoked)
#N=45163

#Merge data + ever daily smoked status
ukbio_smoking_subset <- merge(ukbio_smoking,smoking_status,by="n_eid")
View(ukbio_smoking_subset)

#Pack years
#back-fill pack years, make time since smoking cessation variable
#pack years
packyears_backfilled <- ukbio_smoking_subset %>% mutate(packyears_imputed=coalesce(n_20161_2_0,n_20161_0_0)) %>% select(n_eid, packyears_imputed)

ukbio_smoking_subset <- merge(ukbio_smoking_subset,packyears_backfilled,by="n_eid",all=T)
View(ukbio_smoking_subset)

#Time since smoking cessation
#un-do the imaging confound
imaging_confounds <- IDP_Confounds_45k_01_25_23

imaging_confounds <- right_join(ID_key, imaging_confounds, by = "eid")
imaging_confounds <- imaging_confounds %>% filter(! is.na(pheno_eid))
imaging_confounds <- imaging_confounds %>% select(! eid) %>% dplyr::rename("n_eid" = "pheno_eid")

#keep age
age_imaging <- imaging_confounds %>% select(n_eid,`21003-2.0`)
ukbio_smoking_subset <- merge(ukbio_smoking_subset,age_imaging,by="n_eid")

#time since smoking cessation
#removing character values (do not know, prefer not to answer) and change the column into numeric, allowing coalesce to work 
ukbio_smoking_subset$n_2897_0_0[ukbio_smoking_subset$n_2897_0_0=="-3"] <- ""
ukbio_smoking_subset$n_2897_0_0[ukbio_smoking_subset$n_2897_0_0=="-1"] <- ""
ukbio_smoking_subset$n_2897_0_0 <- as.numeric(as.character(ukbio_smoking_subset$n_2897_0_0))

agestop_backfilled <- ukbio_smoking_subset %>% mutate(agestop_imputed=coalesce(n_2897_2_0,n_2897_0_0)) %>% mutate(time_since_cessation = `21003-2.0`-agestop_imputed) 

#if they said they are current daily smokers in imaging study (n_1239_2_0=1), then their time since cessation value is 0 
agestop_backfilled <- agestop_backfilled %>% mutate(time_since_cessation = ifelse(n_1239_2_0=="1",0,time_since_cessation))

#drop NAs, check the sample size
time_since_smoking_cessation <- agestop_backfilled %>% drop_na(time_since_cessation) %>% select(n_eid,time_since_cessation)

ukbio_smoking_subset <- merge(ukbio_smoking_subset,time_since_smoking_cessation,by="n_eid",all=T)

#----------Step 2. Smoking variables done----------#

#Step 3. Non-imaging confounds (+alcohol)
#Download the data #Lab version 
#non-imaging confounds 
non_imaging_confounds_4_5_23 <- read.csv("C:/Users/yoonhoochang/Box/2023 UKB Data/non_imaging_confounds_4_5_23.csv")
age_edu_complete_4_25_23 <- read.csv("C:/Users/yoonhoochang/Box/2023 UKB Data/age_edu_complete_4_25_23.csv")
add_cov_0518 <- read.csv("C:/Users/yoonhoochang/Box/UKB_Phenotypes/add_cov_0518.csv")

ukb_total_covariates <- merge(non_imaging_confounds_4_5_23,age_edu_complete_4_25_23,by="n_eid")
ukb_total_covariates <- merge(ukb_total_covariates,add_cov_0518,by="n_eid")

non_imaging_confounds <- ukb_total_covariates
non_imaging_confounds$n_845_0_0[non_imaging_confounds$n_845_0_0=="-1"] <- NA
non_imaging_confounds$n_845_0_0[non_imaging_confounds$n_845_0_0=="-3"] <- NA
non_imaging_confounds$n_845_0_0[non_imaging_confounds$n_845_0_0=="-2"] <- NA
non_imaging_confounds$n_845_0_0 <- as.numeric(as.character(non_imaging_confounds$n_845_0_0))

#backfilling
non_imaging_confounds <- non_imaging_confounds %>% mutate(qualification=coalesce(n_6138_2_0,n_6138_0_0),bmi=n_21001_0_0,dbp=coalesce(n_4079_2_0,n_4079_0_0),sbp=coalesce(n_4080_2_0,n_4080_0_0),Income=coalesce(n_738_2_0,n_738_0_0),waist=coalesce(n_48_2_0,n_48_0_0),hip=coalesce(n_49_2_0,n_49_0_0),age_completd_edu = coalesce(n_845_2_0,n_845_0_0),stress=coalesce(n_6145_0_0,n_6145_2_0),diabetes=coalesce(n_2443_0_0,n_2443_2_0),cancer=coalesce(n_2453_0_0,n_2453_2_0),other_diagnosis=coalesce(n_2473_0_0,n_2473_2_0),pa1=coalesce(n_884_0_0,n_884_2_0),pa2=coalesce(n_904_0_0,n_904_2_0),vascular=coalesce(n_6150_0_0,n_6150_2_0)) %>% mutate(waist_hip = waist/hip)  #make waist/hip ratio from waist circumference, hip circumference data fields

#remove do not know and prefer not to answer
non_imaging_confounds$Income[non_imaging_confounds$Income=="-1"] <- NA
non_imaging_confounds$Income[non_imaging_confounds$Income=="-3"] <- NA

#convert to categorical variables with 5 levels 
non_imaging_confounds$Income <- as.factor(as.character(non_imaging_confounds$Income)) 

#Change qualifications to categorical "numbers" 

#remove none of the above and prefer not to answer
non_imaging_confounds$qualification[non_imaging_confounds$qualification=="-7"] <- NA
non_imaging_confounds$qualification[non_imaging_confounds$qualification=="-3"] <- NA

#Average age of completing following education = A levels: 17, college/university: 23, gcse/cse: 15, professional qualifications (nursing/teaching): 22, nvq/hnd/hnc: unknown)
non_imaging_confounds <- non_imaging_confounds %>% mutate(qualification = replace(qualification,qualification=="2","17")) %>% mutate(qualification = replace(qualification,qualification=="4","15")) %>% mutate(qualification = replace(qualification,qualification=="6","22")) %>% mutate(qualification = replace(qualification,qualification=="3","15")) %>% mutate(qualification = replace(qualification,qualification=="1","23")) %>% mutate(qualification = replace(qualification,qualification=="5",NA)) 

#change qualification column to numeric, this is to impute for age completed full time education
non_imaging_confounds$qualification<- as.numeric(as.character(non_imaging_confounds$qualification))

#impute age_completed full time education with the qualification average age 
non_imaging_confounds <- non_imaging_confounds %>% mutate(age_completed_imputed=coalesce(age_completd_edu,qualification))

non_imaging_confounds <- merge(ukbio_smoking_subset,non_imaging_confounds,by="n_eid",all.x=T)

#let's do additional covariates (stress, diabetes, cancer, other diagnosis, non-vigorous and vigorous physical activities)

#1. stress
#remove prefer not to answer
non_imaging_confounds$stress[non_imaging_confounds$stress=="-3"] <- NA

#group those who answered together 
non_imaging_confounds <- non_imaging_confounds %>% mutate(stress = replace(stress,stress=="1","1")) %>% mutate(stress = replace(stress,stress=="2","1")) %>% mutate(stress = replace(stress,stress=="3","1")) %>% mutate(stress = replace(stress,stress=="4","1")) %>% mutate(stress = replace(stress,stress=="5","1")) %>% mutate(stress = replace(stress,stress=="6","1")) %>% mutate(stress = replace(stress,stress=="-7","0")) 

#2. diabetes
#remove do not know and prefer not to answer
non_imaging_confounds$diabetes[non_imaging_confounds$diabetes=="-1"] <- NA
non_imaging_confounds$diabetes[non_imaging_confounds$diabetes=="-3"] <- NA

#3. cancer
#remove do not know and prefer not to answer
non_imaging_confounds$cancer[non_imaging_confounds$cancer=="-1"] <- NA
non_imaging_confounds$cancer[non_imaging_confounds$cancer=="-3"] <- NA

#4. other diagnosis
#remove do not know and prefer not to answer
non_imaging_confounds$other_diagnosis[non_imaging_confounds$other_diagnosis=="-1"] <- NA
non_imaging_confounds$other_diagnosis[non_imaging_confounds$other_diagnosis=="-3"] <- NA

#5. non-vigorous physical activites (pa1)
#remove do not know and prefer not to answer
non_imaging_confounds$pa1[non_imaging_confounds$pa1=="-1"] <- NA
non_imaging_confounds$pa1[non_imaging_confounds$pa1=="-3"] <- NA

#6. vigorous physical activites (pa2)
#remove do not know and prefer not to answer
non_imaging_confounds$pa2[non_imaging_confounds$pa2=="-1"] <- NA
non_imaging_confounds$pa2[non_imaging_confounds$pa2=="-3"] <- NA

#7. vascular/heart problems
#remove prefer not to answer
non_imaging_confounds$vascular[non_imaging_confounds$vascular=="-3"] <- NA

#group those who answered together 
non_imaging_confounds <- non_imaging_confounds %>% mutate(vascular = replace(vascular,vascular=="1","1")) %>% mutate(vascular = replace(vascular,vascular=="2","1")) %>% mutate(vascular = replace(vascular,vascular=="3","1")) %>% mutate(vascular = replace(vascular,vascular=="4","1")) %>% mutate(vascular = replace(vascular,vascular=="-7","0")) 

#Let's do Alcohol (With Vera's script, calculate_drinks.R -> included in this github folder)
#load calculate_drinks function 
#load alcohol dataset
baseline_alcohol_4_5_23 <- read_csv("C:/Users/yoonhoochang/Box/2023 UKB Data/baseline_alcohol_4_5_23.csv")

alcohol_40k <- calculate_drinks(baseline_alcohol_4_5_23)
View(alcohol_40k)
alcohol_40k <- alcohol_40k %>% select(n_eid,week_drinks)

non_imaging_confounds <- merge(non_imaging_confounds,alcohol_40k,by="n_eid")
View(non_imaging_confounds)

non_imaging_confounds_for_impute <- non_imaging_confounds %>% select(n_eid,bmi,dbp,sbp,Income,waist_hip,age_completed_imputed,week_drinks,stress,diabetes,cancer,other_diagnosis,pa1,pa2,vascular)

#Let's do imputation with MICE
#get sex from imaging confounds
sex_imaging <- imaging_confounds %>% select(n_eid,sex=`31-0.0`)

non_imaging_confounds_for_impute <- merge(non_imaging_confounds_for_impute,sex_imaging,by="n_eid")

#Report missingness
processed_confounds_id <- processed_confounds %>% select(n_eid)

non_imaging_confounds_for_impute <- merge(processed_confounds_id,non_imaging_confounds_for_impute,by="n_eid")

pMiss <- function(x){sum(is.na(x))/length(x)*100}
missing_report <- apply(non_imaging_confounds_for_impute,2,pMiss)

pMiss <- function(x){sum(is.na(x))}
missing_report <- apply(non_imaging_confounds_for_impute,2,pMiss)

#record missingness
missing_report %>% kable(caption = paste0("Missing percentage in non-imaging confounds\nN=",nrow(non_imaging_confounds_for_impute))) %>% kable_classic(full_width=F,html_font = "Arial")

#change stress and vascular to binomial variable
#diabetes, cancer, other diagnosis to binomial variable
#convert to factor variables
non_imaging_confounds_for_impute$stress <- as.factor(as.character(non_imaging_confounds_for_impute$stress)) 
non_imaging_confounds_for_impute$vascular <- as.factor(as.character(non_imaging_confounds_for_impute$vascular)) 
non_imaging_confounds_for_impute$diabetes <- as.factor(as.character(non_imaging_confounds_for_impute$diabetes))
non_imaging_confounds_for_impute$cancer <- as.factor(as.character(non_imaging_confounds_for_impute$cancer))
non_imaging_confounds_for_impute$other_diagnosis <- as.factor(as.character(non_imaging_confounds_for_impute$other_diagnosis))

#imputation based on this post (https://datascienceplus.com/handling-missing-data-with-mice-package-a-simple-approach/)
library(mice)
init = mice(non_imaging_confounds_for_impute,maxit=0)
meth = init$method
predM = init$predictorMatrix

#eid will not be included as a predictor, so I set it to predM = 0
predM[, c("n_eid")]=0

#sex will be skipped, but will be used in the prediction 
meth[c("sex")]=""

#different variables with different method: continuous variables were mainly using norm method, categorical income was using polyreg, and binomial variables were using logreg (common usages)
meth[c("age_completed_imputed")]="norm"
meth[c("Income")]="polyreg"
meth[c("bmi")]="norm"
meth[c("waist_hip")]="norm"
meth[c("dbp")]="norm"
meth[c("sbp")]="norm"
meth[c("week_drinks")]="norm"
meth[c("pa1")]="norm"
meth[c("pa2")]="norm"
meth[c("vascular")]="logreg"
meth[c("stress")]="logreg"
meth[c("other_diagnosis")]="logreg"
meth[c("cancer")]="logreg"
meth[c("diabetes")]="logreg"

set.seed(103)
imputed = mice(non_imaging_confounds_for_impute,method=meth,predictorMatrix = predM,m=5)
imputed <- complete(imputed)
sapply(imputed,function(x) sum(is.na(x)))

non_imaging_confounds_imputed <- imputed
#okay we have no missing values now

#Merge, merge, merge
#PCs
library(readr)
ukb_pc20 <- read_csv("C:/Users/yoonhoochang/Box/UKB_Phenotypes/ukb_pc20.csv")

#PRS Smoking Initiation
ukb45966.PRS.GSCAN.NoUKB.SI.white.all <- read.csv("C:/Users/yoonhoochang/Box/UKB_Phenotypes/PRS_raw/PRS_raw_new/ukb45966.PRS.GSCAN.NoUKB.SI.white.all.score", sep="")
SI_prs <- ukb45966.PRS.GSCAN.NoUKB.SI.white.all
#IMPORTANT NOTE: We expanded the PRS thresholds upon the reviewer's suggestion. The first few thresholds (0.5, 0.4, 0.3, 0.2, 0.1, 0.05) is from the previous PRS version, and all others are from the later version 
#Previous version of PRS: Files_YoonhooChang/PRS_raw
#Later version of PRS: Files_YoonhooChang/PRS_raw/PRS_raw_new/

#imaging, non-imaging confounds
all_confounds_imputed <- merge(processed_confounds,non_imaging_confounds_imputed,by="n_eid")

#update the final sample size
#update N-tracker
n_tracker <- rbind(n_tracker,data.frame("Step"="Merged with smoking data","MRI"="all","N"=nrow(all_confounds_imputed)))

#pcs
all_confounds_imputed_pc <- merge(all_confounds_imputed,ukb_pc20,by="n_eid")

#smoking
smoking_all_confounds_imputed_pc <- merge(all_confounds_imputed_pc,ukbio_smoking_subset,by="n_eid")

#prs
prs_smoking_all_confounds_imputed_pc <- merge(smoking_all_confounds_imputed_pc,SI_prs,by.x="n_eid",by.y="FID",all.x=T)

#save this  n=32,094 dataset for the peace of mind
write.table(prs_smoking_all_confounds_imputed_pc, paste0(output_path, "association_prs_smoking_all_confounds_imputed_pc_", Sys.Date(), ".txt"), row.names = F)
