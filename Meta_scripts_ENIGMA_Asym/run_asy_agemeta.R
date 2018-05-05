library(stringr)
library(metafor)
options(stringsAsFactors = FALSE)

enigma_path = '##' # The path

sys_name = Sys.info()[['sysname']]
if (sys_name == 'Windows'){
  data_dir = paste0(enigma_path, '/Data/analysis_run_meta')
  dataset_info_file = paste0(enigma_path, '/stats/dataset_info_updated.csv')
  out_dir = paste0(enigma_path, '/stats/stats_asy')
  sexout_dir = paste0(enigma_path, '/stats/sexstats_asy')
  ageout_dir = paste0(enigma_path, '/stats/agestats_asy')
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

out_dir = ageout_dir # remember to change!

for (i in 1:length(csv_file_list)){
  csv_file = paste(out_dir, csv_file_list[i], sep='/')
  print(csv_file)
  csv_dat = read.csv(csv_file)
  
  res = rma(yi=yi, vi=vi, data = csv_dat) # run the meta analysis with random effect modelling. 
  res_stats = c(csv_file_list[i], res$k, res$tau2, res$se.tau2, res$I2, res$H2, res$k.eff, res$QE, res$QEp, res$b[1], res$se, res$zval, res$pval, res$ci.lb, res$ci.ub)
  if (i==1){
    all_res_stats = res_stats
  }else{
    all_res_stats = rbind(all_res_stats, res_stats)
  }
  
  svg_file = str_replace(csv_file, '.csv', '_forest.svg')
  svg(svg_file, width = 8, height = 12)
  position_y = sum(!is.na(dataset_info$yi))+2
  
  forest(res, xlim=c(-2.5, 2.5), slab = 1:dim(dataset_info)[1])
  text(-2.5, position_y, "Dataset", pos=4, cex=0.6)
  text( 2.5, position_y, "Observed SMD [95% CI]", pos=2, cex=0.6)
  dev.off()
}
all_res_stats = as.data.frame(all_res_stats)
colnames(all_res_stats) = c('Region', 'DOF', 'Tau2', 'Tau2SE', 'I2', 'H2', 'Qdf', 'Q', 'Qp','estimate','se','zval','pval','ci.lb','ci.ub')
csv_file = paste(sumout_dir, 'age_asy_meta.csv', sep='/') # remember to change!
write.csv(all_res_stats, file = csv_file, row.names = FALSE)
print(paste('csv file saved for',csv_file))

