#data de temperaturas medias no brasil
#carregar dados do site Berkeley Earth
#http://berkeleyearth.org/data


install.packages("readr")
install.packages("data.table")
library(readr)
library(dplyr)
library(ggplot2)
library(readr)
library(scales)
library(data.table)
library(dtplyr)

#carregar datos usando time

#usando o read.csv2() obs system.time: calcular o tempo de execucao
system.time(df_teste1 <- read.csv2("TemperaturasGlobais.csv"))

#usando o read table
system.time(df_teste2 <- read.table("TemperaturasGlobais.csv"))

#usando o fread
?fread
system.time(df<-fread("TemperaturasGlobais.csv"))
df<- read.csv("TemperaturasGlobais.csv")

#crio um novo dataframe so com os dados do brasil
cidadesBrasil<- subset(df,Country=="Brazil")
grep("Teresina",cidadesBrasil,value=T)
#remove na usando omit
cidadesBrasil<- na.omit(cidadesBrasil)
head(cidadesBrasil)
nrow(df)
nrow(cidadesBrasil)
dim(cidadesBrasil)

#preparando a organização

#convertendo as datas
cidadesBrasil$dt<- as.POSIXct(cidadesBrasil$dt,format='%Y-%m-%d')
cidadesBrasil$Month<-month(cidadesBrasil$dt)
cidadesBrasil$Year<-year(cidadesBrasil$dt)

#carregando os subsets
#palmas
plm<- subset(cidadesBrasil,City=='Palmas')
plm<- subset(plm, Year %in% c(1796,1846,1896,1946,2012))

#carregar teresina 
the<- subset(cidadesBrasil,City=='Teresina')
the<- subset(the,Year %in% c(2005,2006,2007,2008,2009,2010,2011,2012,2013,2014))

Bom<- subset(cidadesBrasil,City=='Bom Jesus do Piaui')
Bom<- subset(the,Year %in% c(2005,2006,2007,2008,2009,2010,2011,2012,2013,2014))
#curitiba
crt<- subset(cidadesBrasil,City=='Curitiba')
crt <-subset(crt,Year %in% c(1796,1846,1896,1946,2012))

#recife
recf<-  subset(cidadesBrasil,City=='Recife')
recf <- subset(recf,Year %in% c(1796,1846,1896,1946,1996,2012))

# Construindo os Plots
p_plm <- ggplot(plm, aes(x = (Month), y = AverageTemperature, color = as.factor(Year))) +
  geom_smooth(se = FALSE,fill = NA, size = 2) +
  theme_light(base_size = 20) +
  xlab("Mês")+
  ylab("Temperatura Media") +
  scale_color_discrete("") +
  ggtitle("Temperatura Média ao longo dos anos em Palmas") +
  theme(plot.title = element_text(size = 18))

p_crt <- ggplot(crt, aes(x = (Month), y = AverageTemperature, color = as.factor(Year))) +
  geom_smooth(se = FALSE,fill = NA, size = 2) +
  theme_light(base_size = 20) +
  xlab("Mês")+
  ylab("Temperatura") +
  scale_color_discrete("") +
  ggtitle("Temperatura Média ao longo dos anos em Curitiba") +
  theme(plot.title = element_text(size = 18))

p_recf <- ggplot(recf, aes(x = (Month), y = AverageTemperature, color = as.factor(Year))) +
  geom_smooth(se = FALSE,fill = NA, size = 2) +
  theme_light(base_size = 20) +
  xlab("Mês")+
  ylab("Temperatura Média") +
  scale_color_discrete("") +
  ggtitle("Temperatura Média ao longo dos anos em Recife") +
  theme(plot.title = element_text(size = 18))

p_the <- ggplot(the, aes(x = (Month), y = AverageTemperature, color = as.factor(Year))) +
  geom_smooth(se = FALSE,fill = NA, size = 2) +
  theme_light(base_size = 15) +
  xlab("Mês")+
  ylab("Temperatura Media") +
  scale_color_discrete("") +
  ggtitle("Temperatura Média ao longo dos anos em teresina") +
  theme(plot.title = element_text(size = 15))

p_Bom <- ggplot(Bom, aes(x = (Month), y = AverageTemperature, color = as.factor(Year))) +
  geom_smooth(se = FALSE,fill = NA, size = 2) +
  theme_light(base_size = 15) +
  xlab("Mês")+
  ylab("Temperatura Media") +
  scale_color_discrete("") +
  ggtitle("Temperatura Média ao longo dos anos em Bom Jesus do Piaui") +
  theme(plot.title = element_text(size = 15))

# Plotando
p_plm
p_crt
p_recf
p_the
p_Bom


