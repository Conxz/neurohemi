#=========================
# Notes: this code includes several parts, please run each part accordingly. 
# ========================
library(stringr)
options(stringsAsFactors = FALSE)

enigma_path = '##' # The path

sys_name = Sys.info()[['sysname']]
if (sys_name == 'Windows'){
  data_dir = paste0(enigma_path, '/Data/analysis_run_meta')
  dataset_info_file = paste0(enigma_path, '/stats/dataset_info_updated.csv')
  out_dir = paste0(enigma_path, '/stats/stats_asy')
  sexout_dir = paste0(enigma_path, '/stats/sexstats_asy')
  ageout_dir = paste0(enigma_path, '/stats/agestats_asy')
  #age2out_dir = paste0(enigma_path, '/stats/age2stats_asy')
  handout_dir = paste0(enigma_path, '/stats/handstats_asy')
  icvout_dir = paste0(enigma_path, '/stats/icvstats_asy')
}else if (sys_name == 'Mac'){
  data_dir = '/Volumes/NEO00/analysis_run_meta'
  dataset_info_file = '/Users/kongxiangzhen/ownCloud/Codes/asym/scripts/ENIGMA/stats/dataset_info_updated.csv'
}else{
  print('Error! Paths may need to be edit.')
  stop()
}

dataset_info = read.csv(dataset_info_file)
dataset_list = dataset_info$Dataset
info_file_list = dataset_info$File

col_names_list = list()
col_names_list[[1]] = c('Asy_L_SurfArea', 'Asy_LSurfArea', 'Asy_lh_WhiteSurfArea_area', 'Asy_L_WhiteSurfArea_area')
col_names_list[[2]] = c('Asy_L_Thickness','Asy_LThickness', 'Asy_lh_MeanThickness_thickness', 'Asy_L_MeanThickness_thickness', 'Asy_lh_total','Asy_L_MeanThickness')

age_range_threshod = 10

#
# For Age effects
#
for (col_i in 1:length(col_names_list)){
  dataset_info$yi = NA
  dataset_info$vi = NA
  dataset_info$AgeRange = NA

  col_names = col_names_list[[col_i]]
  
  for (i in 1:length(dataset_list)){
    age_range = dataset_info$AgeMax[i] - dataset_info$AgeMin[i]
    dataset = dataset_list[i]
    info_file = info_file_list[i]
    info_file_path = dirname(info_file)
    info_file_name = basename(info_file)
    
    if (!str_detect(info_file_name, 'nosex')){
      eff_file_name = 'ASY_Effects_WithoutHand.Rdata'
    }else{
      eff_file_name = 'ASY_Effects_WithoutHand_nosex.Rdata'
    }
    
    if (info_file_path == 'GloriaBPSydney'){ # for particular situation with data missing. 
      eff_file_name = 'ASY_Effects_WithHand.Rdata'
    }
    
    eff_file = file.path(data_dir, info_file_path, eff_file_name)
    
    if (file.exists(eff_file)){
      load(eff_file)
      tmp = as.data.frame(r.partcor)
      
      tmp.se = as.data.frame(se.partcor)
      colnames(tmp.se) = colnames(tmp)
      
      col_name = NA
      for (i_avg in 1:length(col_names)){
        if (sum(str_detect(colnames(tmp), col_names[i_avg]))!=0){
          col_name = col_names[i_avg]
          break()
          }
      }
      
      if (!is.na(col_name) && age_range>age_range_threshod){ # to exclude stats without data or age range less than age_range_threshod years
        eff_r = tmp[[col_name]][2]
        eff_rav = tmp.se[[col_name]][2]^2
      }else{
        eff_r = NA
        eff_rav = NA
      }
      
    }else{
      eff_r = NA
      eff_rav = NA
    }
    
    dataset_info$yi[i] = eff_r
    dataset_info$vi[i] = eff_rav
    dataset_info$AgeRange[i] = age_range
  }
  
  csv_file_name = paste(col_names[1], '_asy.csv', sep='')
  write.csv(dataset_info, file = paste(ageout_dir, csv_file_name, sep='/'), row.names = FALSE)
  print(paste('csv file saved for',col_names[1]))
}


area_list = c("bankssts","caudalanteriorcingulate","caudalmiddlefrontal","_cuneus", "entorhinal", 
              "fusiform","inferiorparietal", "inferiortemporal","isthmuscingulate", "lateraloccipital",
              "lateralorbitofrontal","lingual", "medialorbitofrontal","middletemporal", "parahippocampal",
              "paracentral","parsopercularis","parsorbitalis", "parstriangularis", "pericalcarine",
              "postcentral", "posteriorcingulate", "precentral", "precuneus", "rostralanteriorcingulate",
              "rostralmiddlefrontal", "superiorfrontal", "superiorparietal", "superiortemporal", 
              "supramarginal", "frontalpole", "temporalpole", "transversetemporal", "insula")


#==========================================================================
# For checking data, NO need to run again after ensuring that there's no problem. 

for (j in 1:length(area_list)){
  area_str = area_list[j]
  
  for (i in 1:length(dataset_list)){
    dataset = dataset_list[i]
    info_file = info_file_list[i]
    info_file_path = dirname(info_file)
    info_file_name = basename(info_file)
    if (!str_detect(info_file_name, 'nosex')){
      eff_file_name = 'ASY_Effects_WithoutHand.Rdata'
    }else{
      eff_file_name = 'ASY_Effects_WithoutHand_nosex.Rdata'
    }
    eff_file = file.path(data_dir, info_file_path, eff_file_name)
    load(eff_file)
    
    tmp = as.data.frame(d.main)
    tmp_colnames = colnames(tmp)
    
    tmp_col_name = NA
    counter = 0
    for (i_avg in 1:length(tmp_colnames)){
      if (str_detect(tmp_colnames[i_avg], area_str)){
        tmp_col_name = tmp_colnames[i_avg]
        counter = counter +1
      }
    }
    if (is.na(tmp_col_name)||(counter <6)){
      if (area_str == 'entorhinal'){
        area_str2 = 'entorhil'
        for (i_avg in 1:length(tmp_colnames)){
          if (str_detect(tmp_colnames[i_avg], area_str2)){
            tmp_col_name = tmp_colnames[i_avg]
            counter = counter +1
          }
        }
      }else if (area_str =='supramarginal'){
        area_str2 = 'supramargil'
        for (i_avg in 1:length(tmp_colnames)){
          if (str_detect(tmp_colnames[i_avg], area_str2)){
            tmp_col_name = tmp_colnames[i_avg]
            counter = counter +1
          }
        }
      }else{
        print('Please check and add special expamples here.') 
      }
    }
    if (!counter == 6){
      print(paste('Please check', dataset, area_str, counter)) 
    }
  }
}



#==========================================================================
# For running the analysis, for thickness which appear in the first half. 

print('thickness')

for (j in 1:length(area_list)){
  dataset_info$yi = NA
  dataset_info$vi = NA
  dataset_info$AgeRange = NA
  area_str = area_list[j]
  
  
  for (i in 1:length(dataset_list)){
    age_range = dataset_info$AgeMax[i] - dataset_info$AgeMin[i]
    dataset = dataset_list[i]
    info_file = info_file_list[i]
    info_file_path = dirname(info_file)
    info_file_name = basename(info_file)
    
    if (!str_detect(info_file_name, 'nosex')){
      eff_file_name = 'ASY_Effects_WithoutHand.Rdata'
    }else{
      eff_file_name = 'ASY_Effects_WithoutHand_nosex.Rdata'
    }
    
    if (info_file_path == 'GloriaBPSydney'){ # for particular situation with data missing. 
      eff_file_name = 'ASY_Effects_WithHand.Rdata'
    }
    
    eff_file = file.path(data_dir, info_file_path, eff_file_name)
    
    if (file.exists(eff_file)){
      load(eff_file)
      tmp = as.data.frame(r.partcor)
      tmp_colnames = colnames(tmp)
      
      tmp.se = as.data.frame(se.partcor)
      colnames(tmp.se) = tmp_colnames
      
      tmp_col_name = NA
      tmp_colnames = tmp_colnames[141:length(tmp_colnames)]
      for (i_avg in 1:length(tmp_colnames)){
        if (str_detect(tmp_colnames[i_avg], area_str)){
          tmp_col_name = tmp_colnames[i_avg]
          break
        }
      }
      if (is.na(tmp_col_name)){
        if (area_str == 'entorhinal'){
          area_str2 = 'entorhil'
          for (i_avg in 1:length(tmp_colnames)){
            if (str_detect(tmp_colnames[i_avg], area_str2)){
              tmp_col_name = tmp_colnames[i_avg]
              break
            }
          }
        }else if (area_str =='supramarginal'){
          area_str2 = 'supramargil'
          for (i_avg in 1:length(tmp_colnames)){
            if (str_detect(tmp_colnames[i_avg], area_str2)){
              tmp_col_name = tmp_colnames[i_avg]
              break
            }
          }
        }else{
          print('Please check and add special expamples here.') 
        }
      }
      
      if (!is.na(tmp_col_name) && (age_range>age_range_threshod)){ # to exclude stats without data or age range less than age_range_threshod years
        eff_r = tmp[[tmp_col_name]][2]
        eff_rav = tmp.se[[tmp_col_name]][2]^2
      }else{
        eff_r = NA
        eff_rav = NA
      }
      
    }else{
      eff_r = NA
      eff_rav = NA
    }
    
    dataset_info$yi[i] = eff_r
    dataset_info$vi[i] = eff_rav
    dataset_info$AgeRange[i] = age_range
  }
  csv_file_name = paste(area_str, '_asy_thick.csv', sep='') # remember to change the file name!
  write.csv(dataset_info, file = paste(ageout_dir, csv_file_name, sep='/'), row.names = FALSE)
  print(paste('csv file saved for',csv_file_name))
}


#==========================================================================
# For running the analysis, for area which appear in the second half. 
# Similar code with that for thickness above, but search for the second column with specific region string.
# That is, search from back. 

print('area')

for (j in 1:length(area_list)){
  dataset_info$yi = NA
  dataset_info$vi = NA
  dataset_info$AgeRange = NA
  area_str = area_list[j]
  
  
  for (i in 1:length(dataset_list)){
    age_range = dataset_info$AgeMax[i] - dataset_info$AgeMin[i]
    dataset = dataset_list[i]
    info_file = info_file_list[i]
    info_file_path = dirname(info_file)
    info_file_name = basename(info_file)
    
    if (!str_detect(info_file_name, 'nosex')){
      eff_file_name = 'ASY_Effects_WithoutHand.Rdata'
    }else{
      eff_file_name = 'ASY_Effects_WithoutHand_nosex.Rdata'
    }
    
    if (info_file_path == 'GloriaBPSydney'){ # for particular situation with data missing. 
      eff_file_name = 'ASY_Effects_WithHand.Rdata'
    }
    
    eff_file = file.path(data_dir, info_file_path, eff_file_name)
    
    if (file.exists(eff_file)){
      load(eff_file)
      tmp = as.data.frame(r.partcor)
      tmp_colnames = colnames(tmp)
      
      tmp.se = as.data.frame(se.partcor)
      colnames(tmp.se) = tmp_colnames
      
      tmp_col_name = NA
      tmp_colnames = tmp_colnames[141:length(tmp_colnames)]
      tmp_colnames = rev(tmp_colnames) # This is the main difference with code above (for thickness). 
      
      for (i_avg in 1:length(tmp_colnames)){
        if (str_detect(tmp_colnames[i_avg], area_str)){
          tmp_col_name = tmp_colnames[i_avg]
          break
        }
      }
      if (is.na(tmp_col_name)){
        if (area_str == 'entorhinal'){
          area_str2 = 'entorhil'
          for (i_avg in 1:length(tmp_colnames)){
            if (str_detect(tmp_colnames[i_avg], area_str2)){
              tmp_col_name = tmp_colnames[i_avg]
              break
            }
          }
        }else if (area_str =='supramarginal'){
          area_str2 = 'supramargil'
          for (i_avg in 1:length(tmp_colnames)){
            if (str_detect(tmp_colnames[i_avg], area_str2)){
              tmp_col_name = tmp_colnames[i_avg]
              break
            }
          }
        }else{
          print('Please check and add special expamples here.') 
        }
      }
      
      if (!is.na(tmp_col_name) && (age_range>age_range_threshod)){ # to exclude stats without data or age range less than age_range_threshod years
        eff_r = tmp[[tmp_col_name]][2]
        eff_rav = tmp.se[[tmp_col_name]][2]^2
      }else{
        eff_r = NA
        eff_rav = NA
      }
      
    }else{
      eff_r = NA
      eff_rav = NA
    }
    
    dataset_info$yi[i] = eff_r
    dataset_info$vi[i] = eff_rav
    dataset_info$AgeRange[i] = age_range
  }
  csv_file_name = paste(area_str, '_asy_area.csv', sep='') # remember to change the file name!
  write.csv(dataset_info, file = paste(ageout_dir, csv_file_name, sep='/'), row.names = FALSE)
  print(paste('csv file saved for',csv_file_name))
}