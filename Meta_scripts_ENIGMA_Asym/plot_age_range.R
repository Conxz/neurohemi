library(stringr)

options(stringsAsFactors = FALSE)

enigma_path = '##' # The path

dat_file = paste0(enigma_path, '/Stats/stats/plot_age_range.csv')
dat = read.csv(dat_file)

my_colors = colorRampPalette(c("light green", "yellow", "orange", "red"))
N_cols = 5

N2color <- function(n_list, n_col){
  color_list = c()
  i = 1
  cols = my_colors(n_col)
  
  for (n in n_list){
    if (n<50){
      color_list[i] = cols[1]
    }else if(n<100){
      color_list[i] = cols[2]
    }else if(n<200){
      color_list[i] = cols[3]
    }else if(n<400){
      color_list[i] = cols[4]
    }else{
      color_list[i] = cols[5]
    }
    i = i+1
  }
  return(color_list)
}

my_dotcolors = colorRampPalette(c("gray", "black"))
dotcolor <- function(n_list, n_col){
  color_list = c()
  i = 1
  cols = my_dotcolors(n_col)
  for (n in n_list){
    if (is.na(n)){
      color_list[i] = cols[1]
    }else{
      color_list[i] = cols[2]
    }
    i = i+1
  }
  return(color_list)
}

fig_file = str_replace(dat_file, '.csv', '.pdf')
#svg(fig_file, width = 7, height = 8)
pdf(fig_file, width = 3.42, height = 4.2, pointsize = 8)

#postscript(fig_file, width = 3.42, height = 4.2, 
#           horizontal = FALSE, onefile = FALSE, paper = "special", 
#           colormodel = "rgb", family = "Arial")
#tiff(fig_file, units="in", width=5, height=4, res=300, compression = 'lzw')
yIndex = 1:dim(dat)[1]
#par(mar=c(5,3,2,2)+0.1)

plot(dat$AgeMin[order(dat$AgeMin)], yIndex, col='red', type='n', xlim =c(min(dat$AgeMin), max(dat$AgeMax)),
     ylim =c(1, dim(dat)[1]), 
     yaxt="n", bty='l',
     #main = "Age ranges for each sample", xlab ='Age (years)', ylab='Datasets')
     xlab ='Age (years)',
     ylab="Datasets")
axis(2, at=c(1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 99))
#points(dat$AgeMax[order(dat$AgeMin)], yIndex, col='gray', pch='.',cex=4)
color_list = N2color(dat$N[order(dat$AgeMin)], N_cols)
segments(dat$AgeMin[order(dat$AgeMin)], yIndex, dat$AgeMax[order(dat$AgeMin)], yIndex, col=color_list, lwd=1.5)

handcolor_list = dotcolor(dat$Hand[order(dat$AgeMin)],2)
#points(dat$AgeMed[order(dat$AgeMin)], yIndex, col='gray', pch=20)
points(dat$AgeMed[order(dat$AgeMin)], yIndex, col=handcolor_list, pch=20, cex=0.6)

legend(x="bottomright", 
       #legend=rev(c('N < 50', '50 \u2264 N < 100', '100 \u2264 N < 200', '200 \u2264 N < 400', 'N \u2265 400')), 
       legend=rev(c('N < 50', expression('50'<='N < 100'), expression('100'<='N < 200'), expression('200'<='N < 400'), expression('N '>='400'))), 
       col=rev(my_colors(N_cols)), lwd=1, lty=c(NA,NA,NA,NA), 
       pch=c(15,15,15,15), merge=FALSE, bty='n')

dev.off()