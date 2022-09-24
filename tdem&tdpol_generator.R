library(igraph)
library(tm)
library(SnowballC)
library(sqldf)
library(syuzhet)
library(ggplot2)
library(dplyr)

dir = "/Users/MeijiTenno/Desktop/Social Network Analisys/GunControl/"
setwd(dir)
load("tabelle/finalDF.RData")
load("tabelle/geoTag.RData")
#load("tabelle/coord_polarity.RData")
coord_polarity2<-read.csv("tabelle/coord_polarity2.csv", header=T, as.is=T, sep = ",", stringsAsFactors = FALSE)

ts <- data.frame(finalDF , stringsAsFactors = FALSE)


###polarity generica
texts <- ts$text[1:200]
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

td_em[1,2] <- 0
td_em[2,2] <- 0
td_em[3,2] <- 0
td_em[4,2] <- 0
td_em[5,2] <- 0
td_em[6,2] <- 0
td_em[7,2] <- 0
td_em[8,2] <- 0
td_pol[1,2] <- 0 
td_pol[2,2] <- 0 

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
  td_em[1,2] <- td_em[1,2] + td[1,1]
  td_em[2,2] <- td_em[2,2] + td[2,1]
  td_em[3,2] <- td_em[3,2] + td[3,1]
  td_em[4,2] <- td_em[4,2] + td[4,1]
  td_em[5,2] <- td_em[5,2] + td[5,1]
  td_em[6,2] <- td_em[6,2] + td[6,1]
  td_em[7,2] <- td_em[7,2] + td[7,1]
  td_em[8,2] <- td_em[8,2] + td[8,1]
  td_pol[1,2] <- td_pol[1,2] + td[9,1]
  td_pol[2,2] <- td_pol[2,2] + td[10,1]
  
}
save(td_pol, file ="td_pol.RData")
save(td_em, file ="td_em.RData")

load("td_pol.RData")
load("td_em.RData")

###plotto la polarità
ggplot(td_pol, aes(x = sentiment, y = count, fill = sentiment)) +
  geom_bar(stat = "identity") +
  labs(x = "polarity") +
  theme(axis.text.x=element_text(angle=45, hjust=1), legend.title = element_blank())
### plotto le emozioni
ggplot(td_em, aes(x = sentiment, y = count, fill = sentiment)) +
  geom_bar(stat = "identity") +
  labs(x = "emotion") +
  theme(axis.text.x=element_text(angle=45, hjust=1), legend.title = element_blank())
