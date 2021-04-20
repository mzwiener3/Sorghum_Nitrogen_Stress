pheno <- read.csv("data/work/All_final.csv")

fam <- read.table("data/plink/SAP.fam")
fam <- data.frame(Plot=fam$V2)

#GEMMA doesn't chacke phenoype order so:
ph <- plyr::join(fam, pheno, by="Plot") # join your phenotype with Asseccion list from plink file to make sure that they are in the same order. This crucial like in rMVP. 

# Save your phenotype as matrix. You can't have any column and row names, there must be pure numbers. 
write.table(ph[,2:7], row.names = F, col.names = F, quote = F, sep = "\t", file = "gemma/phenotype4.txt")

# Phenotype names for naming output files from loop (if you want to is it, it is not necessery.)
write.table(colnames(ph)[2:7], row.names = F, col.names = F, quote = F, sep = "\t", file = "gemma/phenotypeNames4.txt")

library(bigmemory)

#same story with PC. We know they are in correct order but we have to convert it to regular text file. 
pc <- bigmemory::as.matrix(attach.big.matrix("data/rMVP/SAP.pc.desc"))
write.table(pc, row.names = F, col.names = F, quote = F, sep = "\t", file = "gemma/covariate4.txt")


#install.packages("qqman") #Do it once
#visualize gemma results
library(qqman)
dat <- read.table("gemma/output/DTF_GxE.assoc.txt", header = T) # can use any output from gemma
par(mfrow=c(1,2))
manhattan(dat, "chr", "ps", "p_lrt", suggestiveline = F, genomewideline = -log10(0.05/82142), ylim=c(0,10), main = "a") # manhattan plot
qq(dat$p_lrt, main = "b") #qq plot
dat[head(order(dat$p_lrt), n=15),] # look at 10 top associated SNPs



  ##terminal
./gemma -bfile ../data/plink/SAP -p phenotype3.txt -gk 1 -o SAPKin
mv output/SAPKin.cXX.txt ./
  
  
  #!/bin/sh
  


for i in $(seq 1 6);

do



echo ${i}



a=$(sed -n ${i}p phenotypeNames4.txt)

echo ${a}



./gemma -bfile ../data/plink/SAP -p phenotype4.txt -k SAPKin.cXX.txt -c covariate4.txt  -lmm 2 -n ${i} -o ${a}



done

