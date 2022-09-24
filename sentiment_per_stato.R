library(igraph)
library(tm)
library(SnowballC)
library(sqldf)
library(foreach)
library(syuzhet)
library(ggplot2)
library(dplyr)

dir = "/Users/MeijiTenno/Desktop/Social Network Analisys/GunControl/"
setwd(dir)
load("tabelle/finalDF.RData")
load("tabelle/geoTag.RData")
load("tabelle/coord_polarity.RData")
coord_polarity2<-read.csv("tabelle/coord_polarity2.csv", header=T, as.is=T, sep = ",", stringsAsFactors = FALSE)

ts <- data.frame(finalDF , stringsAsFactors = FALSE)

# è servito per creare coord_polarity

proGunColumn <- vector()
negGunColumn <- vector()


for(i in 1:nrow(geoTag)) {
  temp <- ts[ts$stato == geoTag$stato[i],]
  texts <- temp$text

  # prendo il testo e attraverso la funzione get_nrc_sentiment analizzo le parole in esso 
  sentiment = get_nrc_sentiment(texts)
  # per poter analizzare melgio i dati metto tutto all' interno di un data.frame
  # facendo la trasposta della tabella sentiment
  td = data.frame(t(sentiment))
  #td[,1:5] #per visualizzare
  td = data.frame(rowSums(td[-1]))
  names(td)[1] <- "count"
  tdw <- cbind("sentiment" = rownames(td), td)
  rownames(tdw) <- NULL
  
  #prendo dalla riga 1 a 8 ( della tabella tdw) e lo metto in td_em, idem per td_pol ma da 9 e 10
  td_em  = tdw[1:8, ] # emotions
  td_pol = tdw[9:10, ] # polarity (commenti positivi o negativi)
  proGunColumn[i] <-td_pol[2,2]
  negGunColumn[i] <-td_pol[1,2]
  
}

coord_polarity <- cbind(geoTag, proGunColumn)
coord_polarity <- cbind(coord_polarity, negGunColumn)

save(coord_polarity, file ="coord_polarity.RData")
# fine creazione coord_polarity

# istogrmma delle polarità   

#tempTab <- coord_polarity[1:50,]
tempTab <- coord_polarity[1:10,] #limito a 10 per capirci qualcosa 

library(reshape2)
df1 <- data.frame(tempTab$proGunColumn, tempTab$negGunColumn, tempTab$stato)
df2 <- melt(df1, id.vars='tempTab.stato')
head(df2)

ggplot(df2, aes(x=tempTab.stato, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge')


library(usmap)
library(ggplot2)
library(maps)
#control <- c(2,1,1,3,5,3,3,4,2,2,5,1,4,2,3,1,2,3,1,5,4,3,4,1,1,2,3,3,1,5,2,5,2,2,2,2,3,2,3,3,2,2,2,2,1,2,3,1,2,1)
#coord_polarity <- cbind(coord_polarity2, control)
coord_polarity <- coord_polarity2
coord_polarity <- coord_polarity[- c(2, 11),]
new_cp <- data.frame(coord_polarity$stato, coord_polarity$control)
names(new_cp) <- c("state", "control")

us_states <- map_data("state")



#### sentiment vs restrizione leggi
new_cp$region <- tolower(new_cp$state)
states <- left_join(us_states, new_cp)
##head(states)
coord_polarity$negGunColumn <- coord_polarity$negGunColumn * 10
coord_polarity$proGunColumn <- coord_polarity$proGunColumn * 10
ggplot(data = states,aes(x = long, y = lat)) +
  geom_polygon(color = "gray90", size = 0.1, aes(group = group, fill = control)) +
  #scale_fill_continuous(low="white", high="red", name = "Gun control laws", label = scales::comma) +
  geom_point(aes(lon, latC+0.6, size = negGunColumn), coord_polarity, color = "red")+
  geom_point(aes(lon, latP-0.6, size = proGunColumn), coord_polarity, color="green")


### sentiment vs shooting avvenuti 
shootings <- read.csv("tabelle/ShootingTab.csv", header=T, as.is=T, sep = ";", stringsAsFactors = FALSE)
shootings$region <- tolower(shootings$state)
states <- left_join(us_states, shootings)
head(states)

ggplot(data = states,aes(x = long, y = lat)) +
  geom_polygon(color = "black", size = 0.2, aes(group = group, fill = shoot)) +
  scale_fill_continuous(low="white", high="navyblue", name = "# Shootings", label = scales::comma) +
  geom_point(aes(lon, latC+0.7, size = negGunColumn*1000), coord_polarity, color = "red")+
  geom_point(aes(lon, latP-0.7, size = proGunColumn*1000), coord_polarity, color="green")

#####non bello e solo un punto
pdf("testingplotblabla.pdf")
plot_usmap(data = new_cp, values = "control") + 
  scale_fill_continuous(low="white", high="blue", name = "Gun control laws", label = scales::comma) + 
  theme(legend.position = "right")  +
  geom_point(aes(lon, lat, color= "black", size = negGunColumn), coord_polarity)
dev.off()



