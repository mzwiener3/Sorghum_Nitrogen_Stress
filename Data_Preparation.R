library(readxl)
library(tidyverse)
library(stringr)

###

### Map preparation
map <- read.csv("data/work/Sorghum_Field_Map.csv", header = F)
plot9 <- read.csv("data/work/Plot9xxx.csv")

mapXY <- reshape2::melt(map)

mapXY <- mapXY %>%
         mutate(Col=which(map>1, arr.ind = T)[,1], 
                Row=which(map>1, arr.ind = T)[,2])

colnames(mapXY)[2] <- "Plot"

mapXY <- mapXY %>%
  select(Plot, Row, Col)

###

dat <- read_xlsx("data/raw/total_LA_data1.xlsx", col_types = c("text", "text", rep("numeric", 6), "date", "text"))

####################
la <- dat %>%
  select(Plot, Asseccion, LA_1, LA_2, LA_3, LA_4, LA_5, LA_6)

la <- merge(mapXY, la,by="Plot")
la$Block <- as.character(str_split_fixed(la$Plot, "", 2)[,1])
la$Trt <- ifelse(la$Block %in% c("5", "7"), "SN", "NN")

la$Block[which(la$Plot %in% plot9$Block_4)] <- "4"
la$Block[which(la$Plot %in% plot9$Block_5)] <- "5"
la$Block[which(la$Plot %in% plot9$Block_6)] <- "6"
la$Block[which(la$Plot %in% plot9$Block_7)] <- "7"
la$Block[which(la$Plot %in% plot9$Block_8)] <- "8"

la$Trt[which(la$Plot %in% plot9$Block_4)] <- "NN"
la$Trt[which(la$Plot %in% plot9$Block_5)] <- "SN"
la$Trt[which(la$Plot %in% plot9$Block_6)] <- "NN"
la$Trt[which(la$Plot %in% plot9$Block_7)] <- "SN"
la$Trt[which(la$Plot %in% plot9$Block_8)] <- "NN"

la <- pivot_longer(la, cols = 5:10)
la <- na.omit(la)

la[la$Asseccion=="Btx623",]$Asseccion <- "BTx623"

la <- la %>%
  filter(!Asseccion %in% names(which(table(la$Asseccion)<10)))
table(la$Block)
##################
dtf <- dat %>%
  select(Plot, Asseccion, `Bloom date`)

planting <- "2020-06-08 UTC"
planting <- as.Date("2020-06-08 UTC")
dtf$dtf <- as.numeric(difftime(dtf$`Bloom date`, planting))

dtf <- dtf %>%
  select(Plot, Asseccion, dtf)

dtf <- na.omit(dtf)

dtf <- merge(mapXY, dtf, by="Plot")
dtf$Block <- as.character(str_split_fixed(dtf$Plot, "", 2)[,1])
dtf$Trt <- ifelse(dtf$Block %in% c("5", "7"), "SN", "NN")

dtf$Block[which(dtf$Plot %in% plot9$Block_4)] <- "4"
dtf$Block[which(dtf$Plot %in% plot9$Block_5)] <- "5"
dtf$Block[which(dtf$Plot %in% plot9$Block_6)] <- "6"
dtf$Block[which(dtf$Plot %in% plot9$Block_7)] <- "7"
dtf$Block[which(dtf$Plot %in% plot9$Block_8)] <- "8"

dtf$Trt[which(dtf$Plot %in% plot9$Block_4)] <- "NN"
dtf$Trt[which(dtf$Plot %in% plot9$Block_5)] <- "SN"
dtf$Trt[which(dtf$Plot %in% plot9$Block_6)] <- "NN"
dtf$Trt[which(dtf$Plot %in% plot9$Block_7)] <- "SN"
dtf$Trt[which(dtf$Plot %in% plot9$Block_8)] <- "NN"

dtf[dtf$Asseccion=="Btx623",]$Asseccion <- "BTx623"

##################

write.csv(la, "data/work/LA2.csv", row.names = F)
write.csv(dtf, "data/work/DTF2.csv", row.names = F)


##################

