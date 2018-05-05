library(stringr)
library(metafor)
options(stringsAsFactors = FALSE)

enigma_path = '##' # The path

sys_name = Sys.info()[['sysname']]
if (sys_name == 'Windows'){
  data_dir = paste0(enigma_path, '/Data/analysis_run_meta')
  dataset_info_file = paste0(enigma_path, '/stats/dataset_info.csv')
  out_dir = paste0(enigma_path, '/stats/stats_asy')
  sexout_dir = paste0(enigma_path, '/stats/sexstats_asy')
  ageout_dir = paste0(enigma_path, '/stats/agestats_asy')
  age2out_dir = paste0(enigma_path, '/stats/age2stats_asy')
  icvout_dir = paste0(enigma_path, '/stats/icvstats_asy')
  sumout_dir = paste0(enigma_path, '/stats/sum_stats')
}else if (sys_name == 'Mac'){
  data_dir = '/Volumes/NEO00/analysis_run_meta'
  dataset_info_file = '/Users/kongxiangzhen/ownCloud/Codes/asym/scripts/ENIGMA/stats/dataset_info.csv'
}else{
  print('Error! Paths may need to be edit.')
  stop()
}

area_list = c("bankssts","caudalanteriorcingulate","caudalmiddlefrontal","_cuneus", "entorhinal", 
              "fusiform","inferiorparietal", "inferiortemporal","isthmuscingulate", "lateraloccipital",
              "lateralorbitofrontal","lingual", "medialorbitofrontal","middletemporal", "parahippocampal",
              "paracentral","parsopercularis","parsorbitalis", "parstriangularis", "pericalcarine",
              "postcentral", "posteriorcingulate", "precentral", "precuneus", "rostralanteriorcingulate",
              "rostralmiddlefrontal", "superiorfrontal", "superiorparietal", "superiortemporal", 
              "supramarginal", "frontalpole", "temporalpole", "transversetemporal", "insula")

csv_file_list = c('Asy_L_SurfArea_asy.csv', 'Asy_L_Thickness_asy.csv',
                  paste(area_list, '_asy_thick.csv', sep=''),
                  paste(area_list, '_asy_area.csv', sep=''))

out_dir = icvout_dir # remember to change!

# For Age (Age Median) effects in the moderator analysis
for (i in 1:length(csv_file_list)){
  csv_file = paste(out_dir, csv_file_list[i], sep='/')
  print(csv_file)
  csv_dat = read.csv(csv_file)
  
  res = rma(yi=yi, vi=vi, data = csv_dat, mods = cbind(AgeMed)) # run the meta analysis with random effect modelling. 
  res_stats = c(csv_file_list[i], res$k, res$tau2, res$se.tau2, res$I2, res$H2, res$R2, res$QE, res$QEp, res$QM, res$QMp, res$b[2], res$se[2], res$zval[2], res$pval[2], res$ci.lb[2], res$ci.ub[2])
  if (i==1){
    all_res_stats = res_stats
  }else{
    all_res_stats = rbind(all_res_stats, res_stats)
  }
}
all_res_stats = as.data.frame(all_res_stats)
colnames(all_res_stats) = c('Region', 'DOF', 'Tau2', 'Tau2SE', 'I2', 'H2', 'R2', 'Q', 'Qp','QM', 'QMp','estimate','se','zval','pval','ci.lb','ci.ub')
csv_file = paste(sumout_dir, 'icv_asy_meta_mods-age.csv', sep='/')# remember to change!
write.csv(all_res_stats, file = csv_file, row.names = FALSE)
print(paste('csv file saved for',csv_file))


# For Sex (Male Proportion) effects in the moderator analysis
for (i in 1:length(csv_file_list)){
  csv_file = paste(out_dir, csv_file_list[i], sep='/')
  print(csv_file)
  csv_dat = read.csv(csv_file)
  
  if (csv_file_list[i] == 'paracentral_asy_area.csv'){
    res = rma(yi=yi, vi=vi, data = csv_dat, mods = cbind(MaleProp), control=list(stepadj=0.5)) # run the meta analysis with random effect modelling. 
  }else{
    res = rma(yi=yi, vi=vi, data = csv_dat, mods = cbind(MaleProp)) # run the meta analysis with random effect modelling. 
  }
  res_stats = c(csv_file_list[i], res$k, res$tau2, res$se.tau2, res$I2, res$H2, res$R2, res$QE, res$QEp, res$QM, res$QMp, res$b[2], res$se[2], res$zval[2], res$pval[2], res$ci.lb[2], res$ci.ub[2])
  if (i==1){
    all_res_stats = res_stats
  }else{
    all_res_stats = rbind(all_res_stats, res_stats)
  }
}
all_res_stats = as.data.frame(all_res_stats)
colnames(all_res_stats) = c('Region', 'DOF', 'Tau2', 'Tau2SE', 'I2', 'H2', 'R2', 'Q', 'Qp','QM', 'QMp','estimate','se','zval','pval','ci.lb','ci.ub')
csv_file = paste(sumout_dir, 'icv_asy_meta_mods-sex.csv', sep='/')# remember to change!
write.csv(all_res_stats, file = csv_file, row.names = FALSE)
print(paste('csv file saved for',csv_file))


# For Handedness (Right Proportion) effects in the moderator analysis
for (i in 1:length(csv_file_list)){
  csv_file = paste(out_dir, csv_file_list[i], sep='/')
  print(csv_file)
  csv_dat = read.csv(csv_file)
  
  res = rma(yi=yi, vi=vi, data = csv_dat, mods = cbind(RightProp)) # run the meta analysis with random effect modelling. 
  res_stats = c(csv_file_list[i], res$k, res$tau2, res$se.tau2, res$I2, res$H2, res$R2, res$QE, res$QEp, res$QM, res$QMp, res$b[2], res$se[2], res$zval[2], res$pval[2], res$ci.lb[2], res$ci.ub[2])
  if (i==1){
    all_res_stats = res_stats
  }else{
    all_res_stats = rbind(all_res_stats, res_stats)
  }
}
all_res_stats = as.data.frame(all_res_stats)
colnames(all_res_stats) = c('Region', 'DOF', 'Tau2', 'Tau2SE', 'I2', 'H2', 'R2', 'Q', 'Qp','QM', 'QMp','estimate','se','zval','pval','ci.lb','ci.ub')
csv_file = paste(sumout_dir, 'icv_asy_meta_mods-hand.csv', sep='/')# remember to change!
write.csv(all_res_stats, file = csv_file, row.names = FALSE)
print(paste('csv file saved for',csv_file))


# For ICV (ICV Median) effects in the moderator analysis
for (i in 1:length(csv_file_list)){
  csv_file = paste(out_dir, csv_file_list[i], sep='/')
  print(csv_file)
  csv_dat = read.csv(csv_file)
  
  if (csv_file_list[i] == 'paracentral_asy_area.csv'){
    res = rma(yi=yi, vi=vi, data = csv_dat, mods = cbind(ICVMed), control=list(stepadj=0.5)) # run the meta analysis with random effect modelling. 
  }else{
    res = rma(yi=yi, vi=vi, data = csv_dat, mods = cbind(ICVMed)) # run the meta analysis with random effect modelling. 
  }
  res_stats = c(csv_file_list[i], res$k, res$tau2, res$se.tau2, res$I2, res$H2, res$R2, res$QE, res$QEp, res$QM, res$QMp, res$b[2], res$se[2], res$zval[2], res$pval[2], res$ci.lb[2], res$ci.ub[2])
  if (i==1){
    all_res_stats = res_stats
  }else{
    all_res_stats = rbind(all_res_stats, res_stats)
  }
}
all_res_stats = as.data.frame(all_res_stats)
colnames(all_res_stats) = c('Region', 'DOF', 'Tau2', 'Tau2SE', 'I2', 'H2', 'R2', 'Q', 'Qp','QM', 'QMp','estimate','se','zval','pval','ci.lb','ci.ub')
csv_file = paste(sumout_dir, 'icv_asy_meta_mods-icv.csv', sep='/')  # remember to change!
write.csv(all_res_stats, file = csv_file, row.names = FALSE)
print(paste('csv file saved for',csv_file))
