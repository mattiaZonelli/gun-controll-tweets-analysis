
dir = "/Users/MeijiTenno/Desktop/Social Network Analisys/GunControl/"
setwd(dir)


tweet1 <- read.csv("06_to_15-12/gunviolence.csv", header=T, as.is=T)
tweet2 <- read.csv("15_to_22-12/gunviolence.csv", header=T, as.is=T)
tweet3 <- read.csv("22_to_29-12/gunviolence.csv", header=T, as.is=T)
tweet4 <- read.csv("29-12_to_05-01/gunviolence.csv", header=T, as.is=T)
tweet5 <- read.csv("05_to_12-01/gunviolence.csv", header=T, as.is=T)

ts = rbind(tweet1, tweet2)
ts = rbind(ts, tweet3)
ts = rbind(ts, tweet4)
ts = rbind(ts, tweet5)


save(ts, file ="finalTab/gunviolence.RData")
write_as_csv(ts, "finalTab/gunviolence.csv", prepend_ids = T, na ="", fileEncoding = "UTF-8")

############################################################################################################################

dir = "/Users/MeijiTenno/Desktop/Social Network Analisys/GunControl/tabelle"
setwd(dir)

load("tweets.rdata")

tweet1 <- read.csv("pre 6-12/guncontrol.csv", header=T, as.is=T, sep = ";")
tweet2 <- read.csv("pre 6-12/guncontrolnow.csv", header=T, as.is=T, sep = ";")
tweet3 <- read.csv("pre 6-12/gunsense.csv", header=T, as.is=T, sep = ",")
tweet4 <- read.csv("pre 6-12/gunrights.csv", header=T, as.is=T, sep = ";")
tweet5 <- read.csv("pre 6-12/gunviolence.csv", header=T, as.is=T, sep = ",")


nrow(ts) + nrow(tweet1) + nrow(tweet2) + nrow(tweet3) + nrow(tweet4) + nrow(tweet5) 

tsfinal = ts
tsfinal = rbind(tsfinal, tweet1)
tsfinal = rbind(tsfinal, tweet2)
tsfinal = rbind(tsfinal, tweet3)
tsfinal = rbind(tsfinal, tweet4)
tsfinal = rbind(tsfinal, tweet5)

nrow(tsfinal) # 57686

save(tsfinal, file ="all_in.RData")
library(rtweet)
write_as_csv(tsfinal, "all_in.csv", prepend_ids = T, na ="", fileEncoding = "UTF-8")




