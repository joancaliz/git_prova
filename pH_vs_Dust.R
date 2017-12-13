library(ggplot2)

#pH vs Season
environ <- read.table("METADATA_ALLSAMPLES_CAMARERO.txt", sep="\t", header=T)
environ$Season_WD <- factor(environ$Season_WD, levels=c("Spring", "Summer", "Autumn", "Winter"))
cols<-c("Winter"="#C77CFF", "Spring"="#7CAE00", "Summer"="#00BFC4", "Autumn"="#F8766D")

ggplot(data=environ, aes(factor(Season_WD), pH, fill=Season_WD))+
  geom_boxplot()+
  geom_jitter(size=2, alpha=.4)+
  scale_fill_manual(values=cols)
labs(x="", y="pH")+
  theme_bw()

ggplot(data=environ, aes(factor(Season_WD), Ca, fill=Season_WD))+
  geom_boxplot()+
  scale_y_continuous(limits=c(0,50))+
  geom_jitter(size=2, alpha=.4)+
  scale_fill_manual(values=cols)+
  labs(x="", y="pH")+
  theme_bw()
  



###Time vs Chemical
#tot químics correlacionen positivament (veure corgram.pdf) a excepció de Total_rain

environ <- read.table("METADATA_ALLSAMPLES_CAMARERO.txt", sep="\t", header=T)
date<-as.Date(as.character(environ$Data_mod_WD), format="%d/%m/%y")

qplot(date, pH, data=environ) +
  geom_smooth(method="loess", span=0.19, size=2, se=T)+
  theme_bw()

qplot(date, Total_rain, data=environ) +
  geom_smooth(method="lm", span=0.19, size=2, se=F)+
  theme_bw()+
  scale_y_sqrt()

qplot(date, Num_Rains, data=environ) +
  geom_smooth(method="loess", span=0.2, size=2, se=F)+
  theme_bw()

qplot(date, X3+X7+X2+X5, data=environ) +
  geom_smooth(method="loess", span=0.15, size=2, se=T)+
  theme_bw()

qplot(date, X1+X4+X6, data=environ) +
  geom_smooth(method="loess", span=0.12, size=2, se=T)+
  theme_bw()

qplot(date, Cl, data=environ) +
  geom_smooth(method="loess", span=0.19, size=2, se=T)+
  theme_bw()+
  scale_y_continuous(limits=c(0,200))

qplot(date, Na, data=environ) +
  geom_smooth(method="loess", span=.15, size=2, se=T)+
  theme_bw()+
  scale_y_continuous(limits=c(0,100))




###Plot Pols i pH in the same plot

environ <- read.table("METADATA_ALLSAMPLES_CAMARERO.txt", sep="\t", header=T)
value.st <- scale(environ$pH)
date<-as.Date(as.character(environ$Data_mod_WD), format="%d/%m/%y")
pH <- cbind(environ,value.st, date)
pH$category <- "pH"

intrusion<-read.table("intrusion2007_2014.txt", header=T)
value.st <- scale(intrusion$Days.month)
date<-as.Date(as.character(intrusion$Date), format="%d/%m/%y")
intrusion <- cbind(intrusion, value.st, date)
intrusion$category <- "Dust"

alldata <- rbind(intrusion[,c(11,12, 13)], pH[,c(43,44,45)])

qplot(date, value.st, data=alldata, colour=factor(category)) +
  geom_smooth(data=alldata, method="loess", span=0.15, size=2, se=F)+
  theme_bw()+
  labs(x="", y="")

cols<-c("Dust"="tan2", "pH"="skyblue1")
qplot(date, value.st, data=alldata, colour=category, size=I(2), alpha=I(.5))+
  scale_colour_manual(values=cols)+
  geom_smooth(data=alldata, method="loess", span=0.12, size=1.5, se=F)+
  theme_bw()+
  labs(x="", y="")+
  scale_y_continuous(limits=c(-4,4))

  theme(legend.position = c(.9, .15))




###pH vs Dust

dust.ph <- read.table("Dust_pH_mean.txt", sep="\t", header=T)
attach(dust.ph)

cor.test(pH_mean, Dust, method="spearman")
cor.test(pH_mean, Dust, method="pearson")

qplot(pH_mean, Dust, data=dust.ph, size=I(2))+
  geom_smooth(method="lm", size=1.5, se=T, color="blue")+
  theme_bw()+
  labs(y="Number of days with Saharan dust intrusion per month", x="mean pH per month")

date<-as.Date(as.character(dust.ph$Date), format="%d/%m/%y")

qplot(date, Dust, data=dust.ph, size=I(2))+
  geom_smooth(method="loess", size=1.5, se=T, color="skyblue1")+
  theme_bw()


#MANTEL
library("vegan")

table <-read.table("Dust_pH_mean.txt",sep="\t", header=TRUE,row.names=1)
coral.dist.pH<-vegdist(table[,"promig"], method="euc")#generar la matriu de distàncies a partir de l'index de diversitat compartida
coral.dist.dust<-vegdist(table[,"Dust"], method="euc")#generar la matriu de distàncies a partir de l'index de diversitat compartida

mantel(coral.dist.dust, coral.dist.pH, method="spearman")
#el resultat del Mantel dóna relació baixa en comparació amb Correlació. No tinc clar si està ben utilitzat
#el mantel en aquest cas




