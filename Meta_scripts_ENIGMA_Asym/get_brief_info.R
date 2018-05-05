library(stringr)

enigma_path = '##' # The path

sys_name = Sys.info()[['sysname']]
if (sys_name == 'Windows'){
  workingdir = paste0(enigma_path, '/Data/analysis_run_meta')
  out_dir = paste0(enigma_path, '/stats/')
}else if (sys_name == 'Mac'){
  workingdir = '/Volumes/NEO00/analysis_run_meta'
  out_dir = '/Users/kongxiangzhen/ownCloud/Codes/asym/scripts/ENIGMA/stats'
}else{
  print('Error! Paths may need to be edit.')
  stop()
}

setwd(workingdir)
options(stringsAsFactors = FALSE)

excluded_list_file = 'Excluded.txt'
excluded_list = read.table(excluded_list_file,col.names = 'Excluded')
n_excluded_list = length(excluded_list$Excluded)

dataset_list = list.files('./', recursive = FALSE)
dataset_list = dataset_list[dataset_list!=excluded_list_file]

for (i in 1:n_excluded_list){
  dataset_list = dataset_list[dataset_list!=excluded_list$Excluded[i]]
}
n_dataset = length(dataset_list)

info_file_name = 'ASY_DescriptiveInfo.Rdata'
info_file_name2 = 'ASY_DescriptiveInfo_nosex.Rdata'

counter = 0

get_age_info <- function(){
  tmp = as.data.frame(Covars_stats)
  colnames(tmp)=c('Index','Label','Info')
  age_list = c('age', 'Age', 'AGE')
  age = NA
  age_info = NA
  for (i_age in 1:length(age_list)){
    if (sum(str_detect(tmp$Label, age_list[i_age]))!=0){
      age = age_list[i_age]
    }
  }
  if (!is.na(age)){
    index_list = which(str_detect(tmp$Label, age))
    age_min_index = index_list[1]
    age_min_str = str_split(str_replace_all(tmp$Info[age_min_index],' ', ''), ':')[[1]][1]
    if (age_min_str == 'Min.'){
      age_min = str_split(str_replace_all(tmp$Info[age_min_index],' ', ''), ':')[[1]][2]
      age_med_index = age_min_index+2
      age_max_index = age_min_index+5
      age_med = str_split(str_replace_all(tmp$Info[age_med_index],' ', ''), ':')[[1]][2]
      age_max = str_split(str_replace_all(tmp$Info[age_max_index],' ', ''), ':')[[1]][2]
    }else{
      index_list_new = index_list[!is.na(tmp$Info[index_list])]
      age_min_index = index_list_new[1]
      age_med_index = index_list_new[ceiling(length(index_list_new)/2)]
      age_max_index = index_list_new[length(index_list_new)]
      age_min = str_split(str_replace_all(tmp$Info[age_min_index],' ', ''), ':')[[1]][1]
      age_med = str_split(str_replace_all(tmp$Info[age_med_index],' ', ''), ':')[[1]][1]
      age_max = str_split(str_replace_all(tmp$Info[age_max_index],' ', ''), ':')[[1]][1]
    }
    age_info = c(age_min, age_max, age_med)
  }
  return(age_info)
}

get_icv_info <- function(){
  tmp = as.data.frame(Covars_stats)
  colnames(tmp)=c('Index','Label','Info')
  icv_list = c('icv', 'Icv', 'ICV')
  icv = NA
  icv_info = NA
  for (i_icv in 1:length(icv_list)){
    if (sum(str_detect(tmp$Label, icv_list[i_icv]))!=0){
      icv = icv_list[i_icv]
    }
  }
  if (!is.na(icv)){
    index_list = which(str_detect(tmp$Label, icv))
    icv_min_index = index_list[1]
    icv_med_index = icv_min_index+2
    icv_max_index = icv_min_index+5
    icv_min = str_split(str_replace_all(tmp$Info[icv_min_index],' ', ''), ':')[[1]][2]
    icv_med = str_split(str_replace_all(tmp$Info[icv_med_index],' ', ''), ':')[[1]][2]
    icv_max = str_split(str_replace_all(tmp$Info[icv_max_index],' ', ''), ':')[[1]][2]
    icv_info = c(icv_min, icv_max, icv_med)
  }
  return(icv_info)
}

get_sex_info <- function(){
  tmp = as.data.frame(Covars_stats)
  colnames(tmp)=c('Index','Label','Info')
  sex_list = c('sex', 'Sex', 'SEX')
  sex = NA
  sex_info = NA
  for (i_sex in 1:length(sex_list)){
    if (sum(str_detect(tmp$Label, sex_list[i_sex]))!=0){
      sex = sex_list[i_sex]
    }
  }
  if (!is.na(sex)){
    index_list = which(str_detect(tmp$Label, sex))
    male_index = index_list[2] # '1'
    female_index = index_list[1] # '-1'
    male_n = str_split(str_replace_all(tmp$Info[male_index],' ', ''), ':')[[1]][2]
    female_n = str_split(str_replace_all(tmp$Info[female_index],' ', ''), ':')[[1]][2]
    sex_info = c(male_n, female_n)
  }else{
    male_n = max(mean_sds[,4], na.rm = TRUE)
    female_n = 0
    sex_info = c(male_n, female_n)
  }
  return(sex_info)
}

get_hand_info <- function(){
  tmp = as.data.frame(Covars_stats)
  colnames(tmp)=c('Index','Label','Info')
  hand_list = c('hand', 'Hand', 'HAND')
  hand = NA
  hand_info = NA
  for (i_hand in 1:length(hand_list)){
    if (sum(str_detect(tmp$Label, hand_list[i_hand]))!=0){
      hand = hand_list[i_hand]
    }
  }
  if (!is.na(hand)){
    index_list = which(str_detect(tmp$Label, hand))
    right_index = index_list[2] # '1'
    left_index = index_list[1] # '-1'
    right_n = str_split(str_replace_all(tmp$Info[right_index],' ', ''), ':')[[1]][2]
    left_n = str_split(str_replace_all(tmp$Info[left_index],' ', ''), ':')[[1]][2]
    hand_info = c(right_n, left_n)
  }
  return(hand_info)
}

get_brief_info <- function(info_file) {
  dataset_name = rev(strsplit(info_file, '/')[[1]])[2]
  all_n = max(mean_sds[,4], na.rm = TRUE)
  
  outliter_n = all_n - max(final_mean_sds[,,6],na.rm = TRUE)
  
  sex_info = get_sex_info()
  male_n = sex_info[1]
  female_n = sex_info[2]
  
  hand_info = get_hand_info()
  right_n = hand_info[1]
  left_n = hand_info[2]
  
  age_info = get_age_info()
  age_min = age_info[1]
  age_max = age_info[2]
  age_med = age_info[3]
  
  icv_info = get_icv_info()
  icv_min = icv_info[1]
  icv_max = icv_info[2]
  icv_med = icv_info[3]
  
  return(list(dataset_name, all_n, male_n, female_n, right_n, left_n, age_min, age_max, age_med, icv_min, icv_max, icv_med, info_file, outliter_n))
}

update_data <- function(new_line, counter){
  new_line = t(new_line)
  if (counter == 1){
    all_info = new_line
  }else{
    all_info = rbind(all_info, new_line)
  }
  return(all_info)
}

all_info = NA

for (i in 1:n_dataset){
  #print(dataset_list[i])
  file_list = list.files(dataset_list[i])
  if (info_file_name %in% file_list){
    info_file = paste(dataset_list[i], info_file_name, sep = '/')
    load(info_file)
    brief_info = get_brief_info(info_file)
    print(paste(brief_info[1], brief_info[2],brief_info[3], brief_info[4],brief_info[5], brief_info[6], brief_info[7], brief_info[8],brief_info[9], brief_info[10], brief_info[11], sep = ';'))
    counter = counter + 1
    #all_list[[counter]] = brief_info
    all_info = update_data(brief_info, counter)
  }else if(info_file_name2 %in% file_list){
    info_file = paste(dataset_list[i], info_file_name2, sep = '/')
    load(info_file)
    brief_info = get_brief_info(info_file)
    print(paste(brief_info[1], brief_info[2],brief_info[3], brief_info[4],brief_info[5], brief_info[6], brief_info[7], brief_info[8],brief_info[9], brief_info[10], brief_info[11], sep = ';'))
    counter = counter + 1
    #all_list[[counter]] = brief_info
    all_info = update_data(brief_info, counter)
  }else{
    dir_list = list.dirs(dataset_list[i], recursive=FALSE)
    for (j in 1:n_excluded_list){
      excluded_file = paste(dataset_list[i], excluded_list$Excluded[j], sep = '/')
      dir_list = dir_list[dir_list!=excluded_file]
    }
    n_dir = length(dir_list)
    
    for (k in 1:n_dir){
      file_list = list.files(dir_list[k])
      if (info_file_name %in% file_list){
        info_file = paste(dir_list[k], info_file_name, sep = '/')
        load(info_file)
        brief_info = get_brief_info(info_file)
        print(paste(brief_info[1], brief_info[2],brief_info[3], brief_info[4],brief_info[5], brief_info[6], brief_info[7], brief_info[8],brief_info[9], brief_info[10], brief_info[11], sep = ';'))
        counter = counter + 1
        #all_list[[counter]] = brief_info
        all_info = update_data(brief_info, counter)
      }else if(info_file_name2 %in% file_list){
        info_file = paste(dir_list[k], info_file_name2, sep = '/')
        load(info_file)
        brief_info = get_brief_info(info_file)
        print(paste(brief_info[1], brief_info[2],brief_info[3], brief_info[4],brief_info[5], brief_info[6], brief_info[7], brief_info[8],brief_info[9], brief_info[10], brief_info[11], sep = ';'))
        counter = counter + 1
        #all_list[[counter]] = brief_info
        all_info = update_data(brief_info, counter)
      }else{
        print(paste('Error with ',dir_list[k], sep = ''))
      }
    }
  }
}

print(paste('Number of datasets:', counter, sep = ' '))

all_info = as.data.frame(all_info)
colnames(all_info) = c('Dataset', 'N', 'Male', 'Female', 'Right', 'Left', 'AgeMin', 'AgeMax', 'AgeMed','ICVMin', 'ICVMax', 'ICVMed','File', 'N_outliter')

df <- data.frame(lapply(all_info, as.character), stringsAsFactors=FALSE)
write.csv(df, paste(out_dir, 'dataset_info.csv', sep='/'), row.names=FALSE)
