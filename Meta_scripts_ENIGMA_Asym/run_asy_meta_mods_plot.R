library(stringr)
library(metafor)
options(stringsAsFactors = FALSE)

enigma_path = '##' # The path

sys_name = Sys.info()[['sysname']]
if (sys_name == 'Windows'){
  data_dir = paste0(enigma_path, '/Data/analysis_run_meta')
  dataset_info_file = paste0(enigma_path, '/stats/dataset_info.csv')
  out_dir0 = paste0(enigma_path, '/stats/reuslts_adults/stats_asy')
  out_dir = paste0(enigma_path, '/stats/stats_asy_mods')
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


par(mar=c(5,5,1,2))
# For Age (Age Median) effects in the moderator analysis
age_list = c(5:90)
for (i in 1:length(csv_file_list)){
  csv_file = paste(out_dir0, csv_file_list[i], sep='/')
  print(csv_file)
  csv_dat = read.csv(csv_file)
  
  res = rma(yi=yi, vi=vi, data = csv_dat, mods = cbind(AgeMed)) # run the meta analysis with random effect modelling. 
  
  preds = predict(res, newmods=age_list)
  wi = 1/sqrt(csv_dat$vi)
  size = 0.5 +3.0*(wi-min(wi, na.rm = TRUE))/(max(wi, na.rm = TRUE)-min(wi, na.rm = TRUE))
  
  svg_file = str_replace(paste(out_dir, csv_file_list[i], sep='/'), '.csv', '_AgeMed.svg')
  svg(svg_file, width = 6, height = 7)
  
  plot(csv_dat$AgeMed[!is.na(csv_dat$yi)], csv_dat$yi[!is.na(csv_dat$yi)], 
       pch=19, cex=size, xlab="Meidan Age", main = str_replace(csv_file_list[i], '.csv', ''),
       ylab="Asymmetry Effect Size (L>R)", las=1, bty="l")
  lines(age_list, preds$pred)
  lines(age_list, preds$ci.lb, lty="dashed")
  lines(age_list, preds$ci.ub, lty="dashed")
  abline(h=0, lty="dotted")

  dev.off()
}


# For Sex (Male Proportion) effects in the moderator analysis
prop_list = seq(0.10, 1, by=0.05)
for (i in 1:length(csv_file_list)){
  csv_file = paste(out_dir0, csv_file_list[i], sep='/')
  print(csv_file)
  csv_dat = read.csv(csv_file)
  
  res = rma(yi=yi, vi=vi, data = csv_dat, mods = cbind(MaleProp)) # run the meta analysis with random effect modelling. 
  
  preds = predict(res, newmods=prop_list)
  wi = 1/sqrt(csv_dat$vi)
  size = 0.5 +3.0*(wi-min(wi, na.rm = TRUE))/(max(wi, na.rm = TRUE)-min(wi, na.rm = TRUE))
  
  svg_file = str_replace(paste(out_dir, csv_file_list[i], sep='/'), '.csv', '_MaleProp.svg')
  svg(svg_file, width = 6, height = 7)
  
  plot(csv_dat$MaleProp[!is.na(csv_dat$yi)], csv_dat$yi[!is.na(csv_dat$yi)], 
       pch=19, cex=size, xlab="Male Proportion", main = str_replace(csv_file_list[i], '.csv', ''),
       ylab="Asymmetry Effect Size (L>R)", las=1, bty="l")
  lines(prop_list, preds$pred)
  lines(prop_list, preds$ci.lb, lty="dashed")
  lines(prop_list, preds$ci.ub, lty="dashed")
  abline(h=0, lty="dotted")
  
  dev.off()
  
}



# For Handedness (Right Proportion) effects in the moderator analysis
prop_list = seq(0.7, 1, by=0.02)
for (i in 1:length(csv_file_list)){
  csv_file = paste(out_dir0, csv_file_list[i], sep='/')
  print(csv_file)
  csv_dat = read.csv(csv_file)
  
  res = rma(yi=yi, vi=vi, data = csv_dat, mods = cbind(RightProp)) # run the meta analysis with random effect modelling. 
  
  preds = predict(res, newmods=prop_list)
  wi = 1/sqrt(csv_dat$vi)
  size = 0.5 +3.0*(wi-min(wi, na.rm = TRUE))/(max(wi, na.rm = TRUE)-min(wi, na.rm = TRUE))
  
  svg_file = str_replace(paste(out_dir, csv_file_list[i], sep='/'), '.csv', '_RightProp.svg')
  svg(svg_file, width = 6, height = 7)
  
  plot(csv_dat$RightProp[!is.na(csv_dat$yi)], csv_dat$yi[!is.na(csv_dat$yi)], 
       pch=19, cex=size, xlab="Right Handedness Proportion", main = str_replace(csv_file_list[i], '.csv', ''),
       ylab="Asymmetry Effect Size (L>R)", las=1, bty="l")
  lines(prop_list, preds$pred)
  lines(prop_list, preds$ci.lb, lty="dashed")
  lines(prop_list, preds$ci.ub, lty="dashed")
  abline(h=0, lty="dotted")
  
  dev.off()
}


# For ICV (ICV Median) effects in the moderator analysis
icv_list = seq(1050000, 1674265, by=50000)
for (i in 1:length(csv_file_list)){
  csv_file = paste(out_dir0, csv_file_list[i], sep='/')
  print(csv_file)
  csv_dat = read.csv(csv_file)
  
  res = rma(yi=yi, vi=vi, data = csv_dat, mods = cbind(ICVMed)) # run the meta analysis with random effect modelling. 
  
  preds = predict(res, newmods=icv_list)
  wi = 1/sqrt(csv_dat$vi)
  size = 0.5 +3.0*(wi-min(wi, na.rm = TRUE))/(max(wi, na.rm = TRUE)-min(wi, na.rm = TRUE))
  
  svg_file = str_replace(paste(out_dir, csv_file_list[i], sep='/'), '.csv', '_ICVMed.svg')
  svg(svg_file, width = 6, height = 7)
  
  plot(csv_dat$ICVMed[!is.na(csv_dat$yi)], csv_dat$yi[!is.na(csv_dat$yi)], 
       pch=19, cex=size, xlab="Median ICV", main = str_replace(csv_file_list[i], '.csv', ''),
       ylab="Asymmetry Effect Size (L>R)", las=1, bty="l")
  lines(icv_list, preds$pred)
  lines(icv_list, preds$ci.lb, lty="dashed")
  lines(icv_list, preds$ci.ub, lty="dashed")
  abline(h=0, lty="dotted")
  
  dev.off()
}