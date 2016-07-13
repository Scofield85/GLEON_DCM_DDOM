library(rLakeAnalyzer)
library(dplyr)
library(plyr)
library(ggplot2)
#
chl = load.ts("Crystal/Crystal_2014_midnight.chla")
do = load.ts("Crystal/Crystal_2014_midnight.doobs")
dosat = load.ts("Crystal/Crystal_2014_midnight.dosat")
temp = load.ts("Crystal/Crystal_2014_midnight.wtr")

#
crystal = cbind(chl[1],gather(temp[,-1]),gather(chl[,-1]),gather(do[,-1]),gather(dosat[,-1]))
head(crystal)
names(crystal)[c(3,5,7,9)] = c("temp", "chl", "do_mg_L","dosat")
crystal = crystal
#
crystal.depth = data.frame(crystala_depth = unique(crystal$key), depth = seq(1:18))
crystal = crystal[,-c(4,6,8)]
head(crystal)
#
crystal$depth = crystal$key
crystal$depth = mapvalues(crystal$depth, from = unique(crystal$key), to = seq(1:18))
crystal = crystal[,-2]
#
str(crystal)
crystal$month = month(crystal$datetime)
crystal$day = day(crystal$datetime)
crystal$depth = as.numeric(crystal$depth)
#
crystal$chl_scaled = crystal$chl / 10
crystal$temp_scaled = crystal$temp / 25
crystal$do_scaled = crystal$do_mg_L / 15
crystal$dosat_scaled = crystal$dosat / 100
#
data.plot = crystal[crystal$month == 6 & crystal$day <11,]
data.plot = crystal[crystal$month == 6 & crystal$day >10 & crystal$day <21,]
data.plot = crystal[crystal$month == 6 & crystal$day > 20,]
#windows()
t <- ggplot(data.plot, aes(chl_scaled, depth)) + 
  #geom_point(color = "dark green") + 
  geom_path(color = "dark green") +
  geom_path(color = "blue", aes(do_scaled, depth)) +
  geom_path(color = "red", aes(temp_scaled, depth)) +
  facet_grid(. ~ day) +
  #labs(x = "crystala Fluorescence", y = "Depth (meters)") +
  theme(axis.text=element_text(angle = 90,size=12, colour = "black"),
        panel.background = element_rect(colour = "gray",fill = "white"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.text = element_text(size = 20, colour = "black"),
        axis.title = element_text(size = 20, colour = "black")) +
  scale_y_reverse(limits = c(20,0), breaks = c(0,5,15,20)) + 
  scale_x_continuous(breaks = c(0,.5,1))

setwd("C:/Users/annie/Dropbox/Shared_Folders/GLEON Profiling Buoys Project/Crystal/profilePlots")
png("June_early_do.png", width = 8, height = 3, units = "in", res = 300)
png("June_mid.png", width = 8, height = 3, units = "in", res = 300)
png("June_late.png", width = 8, height = 3, units = "in", res = 300)

t
dev.off()
?scale_y_discrete

