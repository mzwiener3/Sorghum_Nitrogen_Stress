library(rMVP)

#MVP.Data(fileBed = "data/plink/SAP", out = "data/rMVP/SAP", 
#        fileKin = T,
#         filePC = T, 
#         pcs.keep = 3, 
#        priority = "speed")

geno <-  attach.big.matrix("SAP.geno.desc")
Kin <-  attach.big.matrix("SAP.kin.desc")
map <- data.table::fread("SAP.geno.map", header = T, data.table = F)
pc <- bigmemory::as.matrix(attach.big.matrix("SAP.pc.desc"))
gtg <- read.table("SAP.geno.ind")
colnames(gtg) <- "Plot"

pheno <- read.csv("All_final.csv")

pheno <- plyr::join(gtg, pheno, by="Plot")

#Run GWAS for each trait: LA and DTF

for(i in 2:ncol(pheno)){
  imMVP <- MVP(
    phe=pheno[, c(1, i)],
    geno=geno,
    map=map,
    K=Kin,
    CV.GLM=pc,
    CV.MLM=pc,
    CV.FarmCPU=pc,
    priority="speed",
    ncpus=5,
    vc.method="EMMA",
    maxLoop=10,
    method.bin="FaST-LMM",
    threshold=0.05,
    method=c("GLM", "MLM", "FarmCPU"), 
    p.threshold = 0.05/nrow(map), file.output = F
  )
  gc()
  
  MVP.Report(imMVP, plot.type = c("q", "m"), memo = colnames(pheno)[i], multracks = T, 
             outpath = "results/rMVP_final/", threshold = 0.05/nrow(map))
  
  res <- cbind(imMVP$map, imMVP$glm.results, imMVP$mlm.results, imMVP$farmcpu.results)
  data.table::fwrite(res, file = paste("results/rMVP_final/",colnames(pheno)[i], ".csv", sep = ""))
}

    # Run GWAS for LA in HN with DTF as covariance  

pcDTFHN <- cbind(pc, pheno$LA_SN)

imMVP <- MVP(
  phe=pheno[, c(1, 2)],
  geno=geno,
  map=map,
  K=Kin,
  CV.GLM=pcDTFHN,
  CV.MLM=pcDTFHN,
  CV.FarmCPU=pcDTFHN,
  priority="speed",
  ncpus=5,
  vc.method="EMMA",
  maxLoop=10,
  method.bin="FaST-LMM",
  threshold=0.05,
  method=c("GLM", "MLM", "FarmCPU"), 
  p.threshold = 0.05/nrow(map), file.output = F
)

res <- cbind(imMVP$map, imMVP$glm.results, imMVP$mlm.results, imMVP$farmcpu.results)
data.table::fwrite(res, file = "results/rMVP_final/LA_SN_DTFcov.csv")

# Run GWAS for LA in LN with DTF as covariance  

pcDTFLN <- cbind(pc, pheno$LA_NN)

imMVP <- MVP(
  phe=pheno[, c(1, 3)],
  geno=geno,
  map=map,
  K=Kin,
  CV.GLM=pcDTFLN,
  CV.MLM=pcDTFLN,
  CV.FarmCPU=pcDTFLN,
  priority="speed",
  ncpus=5,
  vc.method="EMMA",
  maxLoop=10,
  method.bin="FaST-LMM",
  threshold=0.05,
  method=c("GLM", "MLM", "FarmCPU"), 
  p.threshold = 0.05/nrow(map), file.output = F
)

res <- cbind(imMVP$map, imMVP$glm.results, imMVP$mlm.results, imMVP$farmcpu.results)
data.table::write(res, file = "results/rMVP_final/LA_NN_DTFcov.csv")
    








