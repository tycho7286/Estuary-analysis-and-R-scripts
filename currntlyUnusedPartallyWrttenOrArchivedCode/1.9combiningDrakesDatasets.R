### install and load packages
# install.packages("dplyr")
library(dplyr)

### get file names
# # Windows
# strInPath <- "D:/Google/School/2026Summer-BML-UCDGAP/Data/cleanData/EMPA"
# strOutPath <- "D:/Google/School/2026Summer-BML-UCDGAP/Data/dataCombined"

# Linux
strInPath <- "/mnt/internalShared/Google/School/2026Summer-BML-UCDGAP/Data/cleanData/EMPA"
strOutPath <- "/mnt/internalShared/Google/School/2026Summer-BML-UCDGAP/Data/dataCombined"

fileList <- list.files(strInPath)
#fileList <- fileList [1:20]
strWriteFilename <- "combinedDataset.csv"
estuaryList <- list()
estuaryCombined <- data.frame()
index = 0

### import all the files into R
for (i in fileList){
  index <- index+1
  strFullName <- paste0(strInPath,"/",i)
  dfImport <- read.csv(strFullName) %>% mutate(sensorid = as.character(sensorid))
  estuaryName = paste0("estuary-",substr(i,start = 1,stop=7))
  estuaryList[[i]] <- dfImport
  print(cat(index," ",i))
}

### Clear out unneeded dfImport
rm(dfImport)
gc()

### Combine into 1 data set
for (i in 1:length(estuaryList)){
  estuaryCombined <- bind_rows(estuaryCombined, estuaryList[[i]])
  print(i)
  print(names(estuaryList)[i])
}

### Export combined dataset
fullWriteName <- paste0(strOutPath,"/",strWriteFilename)
write.csv(estuaryCombined, fullWriteName, row.names = FALSE)

print(object.size(estuaryList)/1024^2)
print(object.size(estuaryCombined)/1042^2)

### Run garbage collector:
gc()
