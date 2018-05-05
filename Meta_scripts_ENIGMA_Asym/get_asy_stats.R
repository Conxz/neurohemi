#=========================
# Notes: this code includes several parts, please run each part accordingly. 
# ========================
library(stringr)
options(stringsAsFactors = FALSE)

enigma_path = '##' # The path

sys_name = Sys.info()[['sysname']]
if (sys_name == 'Windows'){
  data_dir = paste0(enigma_path, '/Data/analysis_run_meta')
  dataset_info_file = paste0(enigma_path, '/Stats/stats/dataset_info_updated_ageOrdered_3T.csv')
  out_dir = paste0(enigma_path, '/Stats/stats/stats_asy_ageOrdered_3T')
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

#https://stat.ethz.ch/pipermail/r-help/2012-April/308946.html
#Then yi = (m1i - m2i) / sdi with sampling variance vi = 1/ni + yi^2 / (2*ni).
sv.d<-function(yi, ni){
  sv = 1/ni + yi^2/(2*ni)
  return(sv)
}

col_names_list = list()
col_names_list[[1]] = c('Asy_L_SurfArea', 'Asy_LSurfArea', 'Asy_lh_WhiteSurfArea_area', 'Asy_L_WhiteSurfArea_area')
col_names_list[[2]] = c('Asy_L_Thickness','Asy_LThickness', 'Asy_lh_MeanThickness_thickness', 'Asy_L_MeanThickness_thickness', 'Asy_lh_total')
#col_names_list[[3]]

for (col_i in 1:length(col_names_list)){
  dataset_info$yi = NA
  dataset_info$vi = NA
  dataset_info$df = NA
  col_names = col_names_list[[col_i]]
  
  for (i in 1:length(dataset_list)){
    dataset = dataset_list[i]
    info_file = info_file_list[i]
    info_file_path = dirname(info_file)
    info_file_name = basename(info_file)
    if (!str_detect(info_file_name, 'nosex')){
      avg_file_name = 'ASY_PairtedTAsys.Rdata'
    }else{
      avg_file_name = 'ASY_PairtedTAsys_nosex.Rdata'
    }
    avg_file = file.path(data_dir, info_file_path, avg_file_name)
    load(avg_file)
    
    tmp = as.data.frame(out.pairedt)

    col_name = NA
    for (i_avg in 1:length(col_names)){
      if (sum(str_detect(colnames(tmp), col_names[i_avg]))!=0){
        col_name = col_names[i_avg]
        break()
      }
    }
    
    if (!is.na(col_name)){
      avg_d = tmp[[col_name]][1]
      avg_df =  tmp[[col_name]][3]
      avg_dav = sv.d(avg_d, avg_df+1)
    }else{
      avg_d = NA
      avg_df = NA
      avg_dav = NA
    }
    dataset_info$yi[i] = avg_d
    dataset_info$vi[i] = avg_dav
    dataset_info$df[i] = avg_df
  }
  
  csv_file_name = paste(col_names[1], '_asy.csv', sep='')
  write.csv(dataset_info, file = paste(out_dir, csv_file_name, sep='/'), row.names = FALSE)
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
      avg_file_name = 'ASY_PairtedTAsys.Rdata'
    }else{
      avg_file_name = 'ASY_PairtedTAsys_nosex.Rdata'
    }
    avg_file = file.path(data_dir, info_file_path, avg_file_name)
    load(avg_file)
    
    tmp = as.data.frame(out.pairedt)
    tmp_colnames = colnames(tmp)
    
    tmp_col_name = NA
    counter = 0
    for (i_avg in 1:length(tmp_colnames)){
      if (str_detect(tmp_colnames[i_avg], area_str)){
        tmp_col_name = tmp_colnames[i_avg]
        counter = counter +1
      }
    }
    if (is.na(tmp_col_name)||(counter <2)){
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
    if (!counter == 2){
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
  dataset_info$df = NA
  area_str = area_list[j]
  
  for (i in 1:length(dataset_list)){
    dataset = dataset_list[i]
    info_file = info_file_list[i]
    info_file_path = dirname(info_file)
    info_file_name = basename(info_file)
    if (!str_detect(info_file_name, 'nosex')){
      avg_file_name = 'ASY_PairtedTAsys.Rdata'
    }else{
      avg_file_name = 'ASY_PairtedTAsys_nosex.Rdata'
    }
    avg_file = file.path(data_dir, info_file_path, avg_file_name)
    load(avg_file)
    
    tmp = as.data.frame(out.pairedt)
    tmp_colnames = colnames(tmp)
    
    tmp_col_name = NA
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

    if (!is.na(tmp_col_name)){
      avg_d = tmp[[tmp_col_name]][1]
      avg_df =  tmp[[tmp_col_name]][3]
      avg_dav = sv.d(avg_d, avg_df+1)
    }else{
      avg_d = NA
      avg_df = NA
      avg_dav = NA
    }
    dataset_info$yi[i] = avg_d
    dataset_info$vi[i] = avg_dav
    dataset_info$df[i] = avg_df
  }
  csv_file_name = paste(area_str, '_asy_thick.csv', sep='')
  write.csv(dataset_info, file = paste(out_dir, csv_file_name, sep='/'), row.names = FALSE)
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
  dataset_info$df = NA
  area_str = area_list[j]

  for (i in 1:length(dataset_list)){
    dataset = dataset_list[i]
    info_file = info_file_list[i]
    info_file_path = dirname(info_file)
    info_file_name = basename(info_file)
    if (!str_detect(info_file_name, 'nosex')){
      avg_file_name = 'ASY_PairtedTAsys.Rdata'
    }else{
      avg_file_name = 'ASY_PairtedTAsys_nosex.Rdata'
    }
    avg_file = file.path(data_dir, info_file_path, avg_file_name)
    load(avg_file)
    
    tmp = as.data.frame(out.pairedt)
    tmp_colnames = colnames(tmp)
    tmp_colnames = rev(tmp_colnames) # This is the main difference with code above (for thickness). 
    
    tmp_col_name = NA
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
    
    if (!is.na(tmp_col_name)){
      avg_d = tmp[[tmp_col_name]][1]
      avg_df =  tmp[[tmp_col_name]][3]
      avg_dav = sv.d(avg_d, avg_df+1)
    }else{
      avg_d = NA
      avg_df = NA
      avg_dav = NA
    }
    dataset_info$yi[i] = avg_d
    dataset_info$vi[i] = avg_dav
    dataset_info$df[i] = avg_df
  }
  csv_file_name = paste(area_str, '_asy_area.csv', sep='')
  write.csv(dataset_info, file = paste(out_dir, csv_file_name, sep='/'), row.names = FALSE)
  print(paste('csv file saved for',csv_file_name))
}
