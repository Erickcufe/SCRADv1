## code to prepare `DATASET` dataset goes here


Data_dementia<-read.csv("Riesgos_totales_Class_Cell.csv")
Data_dementia<-Data_dementia[,c(2,38:49)]
# usethis::use_data(Data_dementia, overwrite = TRUE)

Riesgo<-read.csv("Riesgos_totales_Class_Cell_SNPs.csv")
Riesgo<-Riesgo[,c(2,38:49)]
# usethis::use_data(Riesgo)
