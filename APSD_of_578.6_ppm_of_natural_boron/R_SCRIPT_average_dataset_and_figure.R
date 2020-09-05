# This script is in the R language. The purpose is to make 
# the final dataset with the frequencies, average APSD, 
# and its uncertainty. Also, the figure of the average APSD
# can be created as the last step of the script.

# setwd("TYPE_THE_PATH")

# Needed packages
library(dplyr)
library(ggplot2)
library(scales)

# Grouping the files
file_numbers <- c(1:163)

for (i in file_numbers){
  file_name <- paste('Raw_data/spectra', i, sep = '')
  col <- read.csv(file = file_name, 
             header = FALSE,
             sep = '\t')
  if (i == 1) {
    APSDs_data <- col %>% select(V1)
    names(APSDs_data) <- 'n'
    col <- col %>% select(V2)
  } else {
    col <- col %>% select(V2)
  }
  
  names(col) <- paste('spectra', i, sep = '')

  APSDs_data <- cbind(APSDs_data, col)
}

head(APSDs_data)

# Frequency column
Dwell_Time = 5E-6 # seconds
number_of_channels = 62500
average_APSD <- data.frame(APSDs_data[1]/(Dwell_Time * number_of_channels))
names(average_APSD) <- 'Frequency'

# Mean APSD column
average_APSD$APSD <- signif(apply(APSDs_data[-1], 1, mean), 6)

# Uncertainties of the mean APSD
func <- function(x){
  sum(x**2)
}

sum_sqrt <- apply(APSDs_data[-1], 1, func)
number_of_averages = 3000

# APSD uncertainty column
average_APSD$sd <- signif(sqrt(sum_sqrt)/
                            ((ncol(APSDs_data)-1) * sqrt(number_of_averages)), 6)

# Final dataset
# The first two columns have to be eliminated
average_APSD <- average_APSD[-c(1:2),]
head(average_APSD)

# Salving the final dataset
# write.csv(x = average_APSD, 
#           file = 'R_average_APSD_578ppm_nat_boron.csv', 
#           row.names = FALSE, 
#           quote = FALSE)

# Figure
scientific_10 <- function(x) {
  parse(text = gsub("e", "%.%10^", scientific_format()(x)))
}

# Making and saving the figure of the average APSD
png(filename = 'R_average_APSD_578ppm_nat_boron.png',
    width = 560, height = 360, units = "px")

ggplot(data = average_APSD) +
  geom_point(aes(x = Frequency, y = APSD), size = 0.5) + 
  geom_errorbar(aes(x = Frequency, y = APSD, 
                    ymin = APSD - sd, ymax = APSD + sd),
                width = 0.03) +
  scale_x_log10(labels = trans_format('log10', math_format(10^.x))) +
  scale_y_continuous(label = function(x) {
    parse(text = gsub("e", "%.%10^", scientific_format()(x))) }) +
  xlab('Frequency ('~Hz~')') +
  ylab('APSD ('~counts^2/Hz~')') + 
  theme_bw(base_size = 16)

dev.off()
