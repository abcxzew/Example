
library(ggplot2)
CO2
單變數:類別型
ggplot(data = CO2, aes(x = Type))+
  geom_bar(fill = "lightblue", colour = "black")

單變數:連續型
ggplot(data = CO2, aes(x = uptake))+
  geom_histogram()
##stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

雙變數:連續vs連續
ggplot(data = CO2, aes(x = conc, y = uptake))+
  geom_point()

雙變數:離散vs連續
ggplot(CO2, aes(x = Type, y = uptake))+
  geom_boxplot()

多變量:攝取的二氧化碳總體關係
library(ggplot2)
library(GGally)
library(scales)
library(memisc)
set.seed(84)
CO2_samp<-CO2[sample(1:length(CO2$conc), 10),]
ggpairs(CO2_samp, lower = list(continuous = wrap("points", shape = I('.'))),
        upper = list(combo = wrap("box", outlier.shape = I('.'))))