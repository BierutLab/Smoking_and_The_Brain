#Global and subregion IDP processing
#But also diffusion, not for this publication, but for future references

#Global
#IDP_45K_ASEG -> is in Files_YoonhooChang/IDP_processed/

#first make the total results file (with only the total measures from aseg)
#brainseg, gmv, wmv (merge two together), csf

grep("X26514.2.0",colnames(IDPs_45k_ASEG))
grep("X26518.2.0",colnames(IDPs_45k_ASEG))
grep("X26553.2.0",colnames(IDPs_45k_ASEG))
grep("X26584.2.0",colnames(IDPs_45k_ASEG))
grep("X26527.2.0",colnames(IDPs_45k_ASEG))

IDPs_45k_new_total <- IDPs_45k_ASEG %>% select(1,15,19,54,85,28)
IDPs_45k_new_total <- IDPs_45k_new_total %>% mutate(totalwhite=rowSums(across(c("X26553.2.0","X26584.2.0"))))
#why rowsums? -> white matter is divided into left and right hemisphere -> we're adding them up to one IDP 

write.table(IDPs_45k_new_total,file="IDPs_45k_new_total_0711.txt",row.names = F)

#Subregion (Freesurfer ASEG, Freesurfer DKT)
#IDP_45K_ASEG -> is in Files_YoonhooChang/IDP_processed/
#IDP_45K_DKT -> is in Files_YoonhooChang/IDP_processed/


#Diffusion
#creating global diffusion measures 

IDPs_map_45k_diffusion <- merge(IDPs_45k_diffusion,ID_map_img_2023_04_14,by="eid")
IDP_diffusion_full_40k <- merge(IDPs_map_45k_diffusion,association_prs_smoking_all_confounds_imputed_pc_2023.04.25,by.x="pheno_eid",by.y="n_eid") #second file can be any file created from Sample_processing.R

# let's create global diffusion MRI
# (Skeleton)
# FA: 25056-25103 (3-50 -> the column number defined with: grep("X25730.2.0",colnames(IDP_diffusion_full_40k)))
# MD: 25104-25151 (51-98)
# MO: 25152-25199 (99-146)
# ICVF: 25344-25391 (291-338)
# OD: 25392-25439 (339-386)
# ISOVF: 25440-25487 (387-434)

# (Tract)
# FA: 25488-25514 (435-461)
# MD: 25515-25541 (462-488)
# MO: 25542-25568 (489-515)
# ICVF: 25650-25676 (597-623)
# OD: 25677-25703 (624-650)
# ISOVF: 25704-25730 (651-677)

#2. Global FA

data <- IDP_diffusion_full_40k
attach(data)

global_diffusion_fa <- data %>% select(eid,3:50,435:461) %>% transmute(eid,mean_fa=rowMeans(select(.,-eid)))
View(global_diffusion_fa)
  
#3. Global MD
#51:98, 462:488
global_diffusion_md <- data %>% select(eid,51:98,462:488) %>% transmute(eid,mean_md=rowMeans(select(.,-eid)))
View(global_diffusion_md)

#4. Global MO
#MO: 99-146, 489-515

global_diffusion_mo <- data %>% select(eid,99:146,489:515) %>% transmute(eid,mean_mo=rowMeans(select(.,-eid)))
View(global_diffusion_mo)

#5. Global ICVF
#ICVF: 291-338, 597-623

global_diffusion_icvf <- data %>% select(eid,291:338,597:623) %>% transmute(eid,mean_icvf=rowMeans(select(.,-eid)))
View(global_diffusion_icvf)

#6. Global OD
#OD: 339-386, 624-650

global_diffusion_od <- data %>% select(eid,339:386,624:650) %>% transmute(eid,mean_od=rowMeans(select(.,-eid)))
View(global_diffusion_od)

#7. Global ISOVF
#ISOVF: 387-434, 651-677

global_diffusion_isovf <- data %>% select(eid,387:434,651:677) %>% transmute(eid,mean_isovf=rowMeans(select(.,-eid)))
View(global_diffusion_isovf)

global_diffusion_all <- list(global_diffusion_fa,global_diffusion_md,global_diffusion_mo,global_diffusion_icvf,global_diffusion_od,global_diffusion_isovf)

global_diffusion_all <- as.data.frame(global_diffusion_all)

global_diffusion_all <- global_diffusion_all %>% select(eid,mean_fa,mean_md,mean_mo,mean_icvf,mean_od,mean_isovf)
write.table(global_diffusion_all,file="global_diffusion_all.txt",row.names = FALSE)
