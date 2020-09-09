#exer 1
#falta explicar
library(readr)
library(ggplot2)
data <- read_csv("data/co2_mm_mlo (1).csv",na = '-99.99', comment = "#")

data
p <- ggplot(data, aes(x=`decimal date`, y=average)) +
  geom_line() + 
  xlab("")
p


#exer 2