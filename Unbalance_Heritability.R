library(tidyverse)
library(sommer)
library(psych)
library(bigmemory)

gtg <- read.table("data/rMVP/SAP.geno.ind")
Kin <-  attach.big.matrix("data/rMVP/SAP.kin.desc")[]
colnames(Kin) <- gtg$V1
rownames(Kin) <- gtg$V1
colnames(gtg) <- "Plot"

gtg2 <- read.csv("data/work/gtg.csv")


la <- read.csv("data/work/LA2.csv")
la$Plot <- as.character(la$Plot)
la$Block <- as.character(la$Block)

trt <- c("HN", "LN")

H2 <- c()

for (i in trt){
  
  A <- la %>%
    filter(Trt==i) # select data from one treatemnt 
  
  # Fint model with random effect
  mod <- mmer(fixed  = value ~1 + Block, 
              random = ~ Asseccion,
              data   = A)
  
  ### H2 Cullis
  vc_g     <- mod %>% pluck("sigma") %>% pluck("Asseccion") %>% as.numeric      # genetic variance component
  n_g      <- mod %>% pluck("U") %>% pluck("Asseccion") %>% pluck("value") %>% length # number of genotypes
  C22_g    <- mod %>% pluck("PevU") %>% pluck("Asseccion") %>% pluck("value")         # Prediction error variance matrix for genotypic BLUPs
  trC22_g  <- tr(as.matrix(C22_g))                                         # trace
  vdBLUP_g <- 2/n_g * (trC22_g - (sum(C22_g)-trC22_g) / (n_g-1))
  
  H2Cullis <- 1-(vdBLUP_g / 2 / vc_g)     # H2 Cullis
  
  B <- data.frame(PI = gsub("Asseccion", "", rownames(as.data.frame(mod$U$Asseccion))), 
                  pheno = as.numeric(unlist(mod$U$Asseccion)))
  
  B <- plyr::join(gtg2, B, by="PI")
  B <- plyr::join(gtg, B, by="Plot")
  B$Plot <- as.character(B$Plot)  
  
  b <-mmer(pheno ~ 1,
           random= ~vs(Plot, Gu=Kin),
           rcov= ~units,
           data=B)
  
  a <- vpredict(b, h2~(V1)/( V1+V2) )
  
  
  z<- data.frame(trait=i, H2=H2Cullis, h2=a$Estimate)
  H2 <- rbind(H2, z)
  print(paste(i, "Done!"))
}

