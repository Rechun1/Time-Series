#exer 1
#falta explicar
library(readr)
library(ggplot2)
data <- read_csv("data/co2_mm_mlo (1).csv",na = '-99.99', comment = "#") #substituo por NA Todos os dados com -99.99 

data
#série temporal sendo o eixo x a data em decimal e y a média sem -99.99
p <- ggplot(data, aes(x=`decimal date`, y=average)) +
  geom_line() + 
  ylab("Média") +
  ggtitle("Série temporal da média de CO2")
p
month <- c(12,3,6,2,3,7)
meses <- c('Janeiro','Fevereiro','Março','Abril','Maio','Junho','Julho','Agosto','Setembro','Outubro','Novembro','Dezembro')
meses[month]
meses
# verificando a sazonalidade por ano e podemos vericar que no meio da ano.
p <- ggplot(data, aes(x=month, y=average, color=as.factor(year))) +
  geom_line() + 
  xlab("")+
  ggtitle("Série temporal da média de  por ano")

p


