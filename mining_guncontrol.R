library(rtweet)

####  geocode = lookup_coords("usa")
dir = "/Users/MeijiTenno/Desktop/Social Network Analisys/GunControl/15_to_22-12/USA"
setwd(dir)

#guncontrol
guncontrol <- search_tweets( "#guncontrol","lang:en",geocode = lookup_coords("usa"), n = 10000, include_rts = FALSE, type = "recent"
)
save_as_csv(dfs, "guncontrol", prepend_ids = TRUE, na = "NA",
            fileEncoding = "UTF-8")
#guncontrol now
guncontrolnow <- search_tweets( "#guncontrolnow","lang:en",geocode = lookup_coords("usa"), n = 10000, include_rts = FALSE, type = "recent"
)
save_as_csv(dfs, "guncontrolnow", prepend_ids = TRUE, na = "NA",
            fileEncoding = "UTF-8")
#gunrights
gunrights <- search_tweets( "#gunrights","lang:en",geocode = lookup_coords("usa"), n = 10000, include_rts = FALSE, type = "recent"
)
save_as_csv(dfs, "gunrights", prepend_ids = TRUE, na = "NA",
            fileEncoding = "UTF-8")
#gunsense
gunsense <- search_tweets( "#gunsense","lang:en",geocode = lookup_coords("usa"), n = 10000, include_rts = FALSE, type = "recent"
)
save_as_csv(dfs, "gunsense", prepend_ids = TRUE, na = "NA",
            fileEncoding = "UTF-8")
#gunviolence
gunviolence <- search_tweets( "#gunviolence","lang:en",geocode = lookup_coords("usa"), n = 10000, include_rts = FALSE, type = "recent"
)
save_as_csv(dfs, "gunviolence", prepend_ids = TRUE, na = "NA",
            fileEncoding = "UTF-8")

### NO geocode ####
dir = "/Users/MeijiTenno/Desktop/Social Network Analisys/GunControl/05-01_to_12-01/"
setwd(dir)
#guncontrol
guncontrol <- search_tweets( "#guncontrol", n = 10000, include_rts = FALSE, type = "recent"
)
save_as_csv(guncontrol, "guncontrol", prepend_ids = TRUE, na = "NA",
            fileEncoding = "UTF-8")
#guncontrol now
guncontrolnow <- search_tweets( "#guncontrolnow", n = 10000, include_rts = FALSE, type = "recent"
)
save_as_csv(guncontrolnow, "guncontrolnow", prepend_ids = TRUE, na = "NA",
            fileEncoding = "UTF-8")
#gunrights
gunrights <- search_tweets( "#gunrights", n = 10000, include_rts = FALSE, type = "recent"
)
save_as_csv(gunrights, "gunrights", prepend_ids = TRUE, na = "NA",
            fileEncoding = "UTF-8")
#gunsense
gunsense <- search_tweets( "#gunsense", n = 10000, include_rts = FALSE, type = "recent"
)
save_as_csv(gunsense, "gunsense", prepend_ids = TRUE, na = "NA",
            fileEncoding = "UTF-8")
#gunviolence
gunviolence <- search_tweets( "#gunviolence", n = 10000, include_rts = FALSE, type = "recent"
)
save_as_csv(gunviolence, "gunviolence", prepend_ids = TRUE, na = "NA",
            fileEncoding = "UTF-8")




library(data.table)

setDT(dfs)
colnames(dfs)
head(dfs)

rt <- dfs
rt <- lat_lng(rt)

library(maps)
## plot state boundaries
par(mar = c(0, 0, 0, 0))
maps::map("state", lwd = .25)

## plot lat and lng points onto state map
with(rt, points(lng, lat, pch = 20, cex = .75, col = rgb(0, .3, .7, .75)))

