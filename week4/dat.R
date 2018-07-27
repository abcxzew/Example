#Data From https://www.kaggle.com/therabiulawal/art-of-happiness/notebook
library(data.table)
library(ggplot2)
library(sqldf)
library(rworldmap)
library(ggmap)
library(mapproj)
library(maptools)
library(maps)
library(wordcloud)
library(tm)
library(stringr)
library(SnowballC)   
library(Tmisc)
library(scales)
#記得確認檔案類型
Happiness <- read.csv(file = "./DATA/2017.csv")
str(Happiness)

colnames (Happiness) <- c("Country", "Happiness.Rank", "Happiness.Score",
                          "Whisker.High", "Whisker.Low", "Economy", "Family", "Life.Expectancy", 
                          "Freedom", "Generosity", "Trust", "Dystopia.Residual")
# Creating a new column for continents

Happiness$Continent <- NA

Happiness$Continent[which(Happiness$Country %in% c("Israel", "United Arab Emirates", "Singapore", "Thailand", "Taiwan Province of China",
                                                   "Qatar", "Saudi Arabia", "Kuwait", "Bahrain", "Malaysia", "Uzbekistan", "Japan",
                                                   "South Korea", "Turkmenistan", "Kazakhstan", "Turkey", "Hong Kong S.A.R., China", "Philippines",
                                                   "Jordan", "China", "Pakistan", "Indonesia", "Azerbaijan", "Lebanon", "Vietnam",
                                                   "Tajikistan", "Bhutan", "Kyrgyzstan", "Nepal", "Mongolia", "Palestinian Territories",
                                                   "Iran", "Bangladesh", "Myanmar", "Iraq", "Sri Lanka", "Armenia", "India", "Georgia",
                                                   "Cambodia", "Afghanistan", "Yemen", "Syria"))] <- "Asia"
Happiness$Continent[which(Happiness$Country %in% c("Norway", "Denmark", "Iceland", "Switzerland", "Finland",
                                                   "Netherlands", "Sweden", "Austria", "Ireland", "Germany",
                                                   "Belgium", "Luxembourg", "United Kingdom", "Czech Republic",
                                                   "Malta", "France", "Spain", "Slovakia", "Poland", "Italy",
                                                   "Russia", "Lithuania", "Latvia", "Moldova", "Romania",
                                                   "Slovenia", "North Cyprus", "Cyprus", "Estonia", "Belarus",
                                                   "Serbia", "Hungary", "Croatia", "Kosovo", "Montenegro",
                                                   "Greece", "Portugal", "Bosnia and Herzegovina", "Macedonia",
                                                   "Bulgaria", "Albania", "Ukraine"))] <- "Europe"
Happiness$Continent[which(Happiness$Country %in% c("Canada", "Costa Rica", "United States", "Mexico",  
                                                   "Panama","Trinidad and Tobago", "El Salvador", "Belize", "Guatemala",
                                                   "Jamaica", "Nicaragua", "Dominican Republic", "Honduras",
                                                   "Haiti"))] <- "North America"
Happiness$Continent[which(Happiness$Country %in% c("Chile", "Brazil", "Argentina", "Uruguay",
                                                   "Colombia", "Ecuador", "Bolivia", "Peru",
                                                   "Paraguay", "Venezuela"))] <- "South America"
Happiness$Continent[which(Happiness$Country %in% c("New Zealand", "Australia"))] <- "Australia"
Happiness$Continent[which(is.na(Happiness$Continent))] <- "Africa"


# Changing Continent column to factor

Happiness$Continent <- as.factor(Happiness$Continent)

head(Happiness)
summary(Happiness)


require(ggplot2)
ggplot(data=Happiness, aes(x=Happiness$Continent, y=Happiness.Score))+
  geom_boxplot() + coord_flip()+
  labs(y='Happiness.Score',x='Continent',
       title='Continents Happiness Score Box')

#區域跟快樂指數有沒有關係
library(Hmisc)
tapply(Happiness.Score, Happiness$Continent, mean)
ggplot(data=Happiness, aes(x=Happiness$Continent, y=Happiness.Score))+
  stat_summary(fun.data = 'mean_cl_boot', size=1)+
  scale_y_continuous(breaks = seq(1, 10, by = 1)) +
  geom_hline(yintercept = mean(Happiness$Continent) , 
             linetype = 'dotted') +
  labs(x = '區域分布', y = '快樂指數') +
  coord_flip()

#快樂指數可能是跟區域內國家的GDP有關
anova(m1<-lm(Happiness.Score~Happiness$Continent, data=Happiness))
ggplot(data=Happiness, aes(group=Happiness$Continent,
                           y=Happiness.Score, x=Economy))+
  geom_point()+
  stat_smooth(method='lm',se=F)+
  stat_smooth(aes(group=Happiness$Continent,
                  y=Happiness.Score, x=Economy),
              method='lm',se=F)+
  facet_grid(.~Happiness$Continent)+
  labs(x='GDP',y='快樂指數')
  
#運用ANOVA檢驗
anova(m2<-update(m1, . ~ . +
                   Economy, data=Happiness))
anova(m3<-update(m2, . ~ . -
                   Happiness$Continent, data=Happiness))

res_lm<-lapply(list(m1, m2, m3), summary)
(res_lm[[2]]$r.sq - res_lm[[3]]$r.sq)/res_lm[[2]]$r.sq
anova(m3, m2)

(res_lm[[2]]$r.sq - res_lm[[1]]$r.sq)/res_lm[[1]]$r.sq
anova(m1,m2)

#畫圖
require(coefplot)
m2<-lm(Happiness.Score~Happiness$Continent+Economy- 1,
       data=Happiness)
coefplot(m2, xlab='估計值', ylab='迴歸變項', title='反應變項=快樂指數')

#把資料與迴歸分析的預測值/殘差/影響度放入資料
fit_m2<-data.frame(Happiness[, c(3, 6, 13)], fitted=fitted(m2), resid=resid(m2),
                   infl=influence(m2)$hat)

#觀測值合併預測值
ggplot(data = fit_m2, aes(x = Happiness.Score, group = Continent )) +
  stat_density(geom = 'path', position = 'identity') +
  stat_density(geom = 'path', position = 'identity', aes(x = fitted)) +
  geom_vline(xintercept = c(with(Happiness, tapply(Happiness.Score,Continent, mean))), linetype = 'dotted')+
  facet_grid(Continent ~ .) +
  scale_x_continuous(breaks = seq(1, 10, by = 0.5))+
  labs(x = '快樂指數', y = '機率密度')

#根據殘差分配，依地區檢視常態與變異數同質假設
ggplot(data=fit_m2, aes(x=scale(resid)), group=Happiness$Continent)+
  stat_density(geom='path', position='identity', aes(linetype=Continent))+
  scale_linetype_manual(values=6:1)+
  guides(linetype=guide_legend(reverse=TRUE))+
  labs(x='標準化殘差',y='機率密度')+
  theme(legend.position = c(.15, .8))

#根據殘差Q-Q圖，依地區檢視常態假設
require(lattice)
qqmath(~scale(resid)|Continent, data=fit_m2, type=c('p', 'g', 'r'),
       xlab='常態位數', ylab='標準化殘差', layout=c(2, 3),
       pch='.', cex=2)

#畫預測值與殘差散佈圖，檢查線性與等分散假設
require(MASS)
ggplot(data = fit_m2, aes(x = fitted, y = scale(resid), group = Continent)) +
  geom_point(pch = 20, size = 1) +
  stat_smooth(method = 'rlm', se = F) +
  facet_grid(Continent ~ .) +
  labs(x = '快樂指數預測值', y = '標準化殘差')

#呈現影響值與標準化殘差
ggplot(data=fit_m2, aes(x=infl, y=scale(resid), group = Continent)) +
  geom_text(aes(label = rownames(fit_m2)), cex = 2) +
  geom_hline(yintercept = 0, linetype = 'dotted') +
  facet_grid(Continent ~ .) +
  labs(x = '影響值', y = '標準化殘差')
#影響值
summary(influence(m2)$hat)

