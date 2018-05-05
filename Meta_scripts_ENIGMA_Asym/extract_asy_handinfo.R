library(stringr)
options(stringsAsFactors = FALSE)

enigma_path = '##' # The path

sys_name = Sys.info()[['sysname']]
if (sys_name == 'Windows'){
  data_dir = paste0(enigma_path, '/Data/analysis_run_meta')
  dataset_info_file = paste0(enigma_path, '/stats/dataset_info.csv')
  out_dir = paste0(enigma_path,'/Stats/stats/stats_asy')
  sexout_dir = paste0(enigma_path, '/Stats/stats/sexstats_asy')
  #handout_dir = paste0(enigma_path, 'Stats/stats/handstats_asy')
  handout_dir = paste0(enigma_path, '/Stats/stats/results_all/handstats_asy')
  sumout_dir = paste0(enigma_path, '/Stats/stats/results_all/sum_stats')
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

for (i in 1:length(csv_file_list)){
  csv_file = paste(handout_dir, csv_file_list[i], sep='/') # remember to change the folder name!
  print(csv_file)
  csv_dat = read.csv(csv_file)
  
  N_left = sum(csv_dat$Left[!is.na(csv_dat$vi)])
  N_right = sum(csv_dat$Right[!is.na(csv_dat$vi)])
  
  N_sites = sum(!is.na(csv_dat$vi))
  
  handinfo_stats = c(csv_file_list[i], N_left, N_right, N_sites)
  if (i==1){
    all_handinfo_stats = handinfo_stats
  }else{
    all_handinfo_stats = rbind(all_handinfo_stats, handinfo_stats)
  }
  
}

all_handinfo_stats = as.data.frame(all_handinfo_stats)
colnames(all_handinfo_stats) = c('Region', 'Left', 'Right', 'Site')

csv_file = paste(sumout_dir, 'handinfo_asy_info4meta.csv', sep='/')  # remember to change the file name!

write.csv(all_handinfo_stats, file = csv_file, row.names = FALSE)
print(paste('csv file saved for',csv_file))

print(paste('Sample size range of left handers: [', min(all_handinfo_stats$Left), ',', max(all_handinfo_stats$Left),']'))
print(paste('Sample size range of right handers: [', min(all_handinfo_stats$Right), ',', max(all_handinfo_stats$Right),']'))
