if (!require(remotes)) {
    install.packages("remotes")
}
remotes::install_github('jorvlan/raincloudplots')


library(raincloudplots)

a <- read.csv("merged_dtf.csv")

df_1x1 <- data_1x1(
  array_1 = a$nndtf,
  array_2 =  a$sndtf,
  jit_distance = .15,
  jit_seed = 321)

raincloud_d <- raincloud_1x1_repmes(
    data = df_1x1,
    colors = (c('dodgerblue', 'darkorange')),
    fills = (c('dodgerblue', 'darkorange')),
    line_color = 'gray',
    line_alpha = .3,
    size = 1,
    alpha = .6,
    align_clouds = FALSE) +

scale_x_continuous(breaks=c(1,2),  labels=c("0 lbs/acre", "80 lbs/acre"), limits=c(0, 3)) +
  xlab("Nitrogen Application") +
  ylab("Days To Anthesis") +
    theme_classic()

b <- read.csv("merged_yield.csv")

df_1x1 <- data_1x1(
  array_1 = b$nnyield,
  array_2 =  b$snyield,
  jit_distance = .15,
  jit_seed = 321)

raincloud_y <- raincloud_1x1_repmes(
    data = df_1x1,
    colors = (c('dodgerblue', 'darkorange')),
    fills = (c('dodgerblue', 'darkorange')),
    size = 1,
    alpha = .6,
    align_clouds = FALSE) +

scale_x_continuous(breaks=c(1,2),  labels=c("0 lbs/acre", "80 lbs/acre"), limits=c(0, 3)) +
  xlab("Nitrogen Application") +
  ylab("Plot Yield") +
    theme_classic()


c <- read.csv("merge_la2.csv")

la_1x1 <- data_1x1(
  array_1 = c$nnla,
  array_2 =  c$snla,
  jit_distance = .15,
  jit_seed = 321)

raincloud_l <- raincloud_1x1_repmes(
  data = la_1x1,
  colors = (c('dodgerblue', 'darkorange')),
  fills = (c('dodgerblue', 'darkorange')),
  line_color = 'gray',
  line_alpha = .3,
  size = 1,
  alpha = .6,
  align_clouds = FALSE) +
  
  scale_x_continuous(breaks=c(1,2),  labels=c("0 lbs/acre", "80 lbs/acre"), limits=c(0, 3)) +
  xlab("Nitrogen Application") +
  ylab("Leaf Angle") +
  theme_classic()





pdf("yieldplot.pdf")

raincloud_y

dev.off()

pdf("dtfplot.pdf")

raincloud_d

dev.off() 

pdf("leafangle_rainplt.pdf")

raincloud_l

dev.off()
