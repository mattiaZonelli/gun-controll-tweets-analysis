library(igraph)
library(tm)
library(SnowballC)
library(sqldf)
library(syuzhet)
library(ggplot2)
library(dplyr)

dir = "/Users/MeijiTenno/Documents/Scuola/3^anno/Social Network Analisys/GunControl"
setwd(dir)

load("tabelle/finalDF.RData")
#load("tabelle/geoTag.RData")
load("tabelle/coord_polarity.RData")

#finalDF <- read.csv("tabelle/finalTab1.csv", header=T, as.is=T, sep = ";", stringsAsFactors = FALSE)
ts <- data.frame(finalDF , stringsAsFactors = FALSE)
####bipartita ####

edges = data.frame("from" = ts$status_id[1:5000], "to" = ts$reply_to_user_id[1:5000], stringsAsFactors = F )
g <- graph.data.frame(edges, directed = F)
V(g)$type <- bipartite_mapping(g)$type
# Faccio un po di cose per disegnare la bipartita
V(g)$color <- ifelse(V(g)$type, "lightblue", "salmon")
#V(g)$shape <- ifelse(V(g)$type, "circle", "square")
E(g)$color <- "grey"
l <- layout_as_bipartite(g)

plot(g, vertex.label = NA, 
     vertex.size=6,
     edge.width=0.2,
     edges.size = 0.1,
     layout = l)
#### fine bipartita

#### Proiezione su tweet e comm detection ####

sample_status_id = sample(ts$status_id, 400, replace = F)
sample_ts= ts[ts$status_id %in% sample_status_id,]
edges = data.frame("from" = sample_ts$status_id,
                   "to" = sample_ts$reply_to_user_id, stringsAsFactors = F )

g <- graph.data.frame(edges, directed = F)
V(g)$type <- V(g)$name %in% edges[,2]
g_comm <- g
g = g_comm

bipartite_matrix <- as_incidence_matrix(g_comm)
tweets_matrix <- tcrossprod(bipartite_matrix)
diag(tweets_matrix) <- 0
g_tweets_r = graph_from_adjacency_matrix(tweets_matrix, 
                                        mode = "undirected", 
                                        weighted = TRUE)
g_tweets_r = simplify(g_tweets_r)

# plot proiezione sui video
V(g_tweets_r)$size <- degree(g_tweets_r)
V(g_tweets_r)$label.cex <- degree(g_tweets_r) * 0.2
E(g_tweets_r)$color <- "blue"

pdf("proj_40nodes.pdf")
plot(g_tweets_r, vertex.label.cex = 0.2, 
     #vertex.label.color = "blue",
     vertex.label = NA,
     edge.width=0.5,
     edges.size = 0.2,
     edge.lty = 2,
     vertex.size = 1.2,
     edge.curved = TRUE,
     layout=layout.circle(g_tweets_r)
)
dev.off()


cl_fg = cluster_fast_greedy(g_tweets_r)

pdf("comm_det.pdf")
#metodo originale
plot(g_tweets_r, 
     #vertex.color=membership(cl_fg),
     vertex.label = NA,
     vertex.size = 3,
     edge.color=membership(cl_fg),
     edge.curved = TRUE,
     edge.width = 1,
     layout=layout.circle(g_tweets_r)
)
dev.off()











###fine proj e com det

#sentiment e altro ####
#SENTIMENT generico
#operazioni necessarei per utilizzare i dati
proGunColumn <- vector()
negGunColumn <- vector()
foreach(i=1:nrow(geoTag)) %do% {
        temp <- ts[ts$stato == geoTag$stato[i],]  #geoTag$stato[i]
        texts <- temp$text
        
        
        
        texts <- iconv(texts, 'UTF-8', 'ASCII') # encoding for emojis
        corpus <- Corpus(VectorSource(texts))
        getTransformations()
        #
        toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})
        # Convert the text to lower case
        corpus <- tm_map(corpus, content_transformer(tolower))
        # Remove numbers
        corpus <- tm_map(corpus, removeNumbers)
        # Remove English common stopwords
        corpus <- tm_map(corpus, removeWords, stopwords("english"))
        # Remove punctuations
        corpus <- tm_map(corpus, removePunctuation)
        # Eliminate extra white spaces
        corpus <- tm_map(corpus, stripWhitespace)
        #
        #Creo una DTM  a matrix that lists all occurrences of words in the corpus, by document
        #In the DTM, documents are represented by rows and terms by columns
        dtm = DocumentTermMatrix(corpus)
        # Ottengo la frequenza delle occorrenze di ogni parola
        freq <- colSums(as.matrix(dtm))
        #controllo che la trasformazione sia andata a buon fine in modo corretto, deve restituire TRUE
        length(freq) == dim(dtm)[2]
        #ordino in modo decrescente
        ord <- order(freq, decreasing = TRUE)
        
        #prima si crea un dataframe
        wf = data.frame(word = names(freq), # names() allows to get or set the names of an object
                        freq = freq)
        #versione più carina 
        library(wordcloud)
        wordcloud(wf$word, wf$freq, 
                  scale=c(5,0.5), min.freq = 5, 
                  random.order = F, 
                  rot.per = 0.15, 
                  colors = brewer.pal(8, "Spectral")
        )
        
        ############
        #Sentiment Analysis
        
        library('syuzhet')
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
        proGunColumn[i] <-td_pol[1,2]
        negGunColumn[i] <-td_pol[2,2]
        
}

### plotto le emozioni
ggplot(td_em, aes(x = sentiment, y = count, fill = sentiment)) +
        geom_bar(stat = "identity") +
        labs(x = "emotion") +
        theme(axis.text.x=element_text(angle=45, hjust=1), legend.title = element_blank())


###plotto la polarità
ggplot(td_pol, aes(x = sentiment, y = count, fill = sentiment)) +
        geom_bar(stat = "identity") +
        labs(x = "polarity") +
        theme(axis.text.x=element_text(angle=45, hjust=1), legend.title = element_blank())



# istogrmma delle polarità   

#tempTab <- coord_polarity[1:50,]
tempTab <- coord_polarity[1:10,] #limito a 10 per capirci qualcosa 

library(reshape2)
df1 <- data.frame(tempTab$proGunColumn, tempTab$negGunColumn, tempTab$stato)
df2 <- melt(df1, id.vars='tempTab.stato')
head(df2)

ggplot(df2, aes(x=tempTab.stato, y=value, fill=variable)) +
        geom_bar(stat='identity', position='dodge')



#varie cose necessarie per preparmi a plottare le mappe
library(usmap)
library(ggplot2)
library(maps)
control <- c(2,1,1,3,5,3,3,4,2,2,5,1,4,2,3,1,2,3,1,5,4,3,4,1,1,2,3,3,1,5,2,5,2,2,2,2,3,2,3,3,2,2,2,2,1,2,3,1,2,1)
coord_polarity <- cbind(coord_polarity, control)

new_cp <- data.frame(coord_polarity$stato, coord_polarity$control)
names(new_cp) <- c("state", "control")

us_states <- map_data("state")



#### sentiment vs restrizione leggi
new_cp$region <- tolower(new_cp$state)
states <- left_join(us_states, new_cp)
head(states)

ggplot(data = states,aes(x = long, y = lat)) +
        geom_polygon(color = "gray90", size = 0.1, aes(group = group, fill = control)) +
        #scale_fill_continuous(low="white", high="red", name = "Gun control laws", label = scales::comma) +
        geom_point(aes(lon, lat, color= "black"), coord_polarity)

### sentiment vs shooting avvenuti 
shootings <- read.csv("tabelle/ShootingTab.csv", header=T, as.is=T, sep = ";", stringsAsFactors = FALSE)
shootings$region <- tolower(shootings$state)
states <- left_join(us_states, shootings)
head(states)

ggplot(data = states,aes(x = long, y = lat)) +
        geom_polygon(color = "gray90", size = 0.1, aes(group = group, fill = shoot)) +
        #scale_fill_continuous(low="white", high="red", name = "Gun control laws", label = scales::comma) +
        geom_point(aes(lon, lat, color= "black"), coord_polarity)
