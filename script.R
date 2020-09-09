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


meses <- c('Janeiro','Fevereiro','Março','Abril','Maio','Junho','Julho','Agosto','Setembro','Outubro','Novembro','Dezembro')
# verificando a sazonalidade por ano e podemos vericar que no meio da ano.
p <- ggplot(data, aes(x=month, y=average, color=as.factor(year))) +
  geom_line() + 
  xlab("")+
  ggtitle("Série temporal da média de  por ano") +
  scale_x_continuous(labels = meses[month])

p
#Primeira diferença remove não estacionariedade

p <- ggplot(data, aes(x=`decimal date`, y=c(0,diff(average,differences = 1)))) +
  geom_line() + 
  ylab("Média") +
  ggtitle("Série temporal da média de CO2")
p
