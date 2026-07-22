### install and load packages
# install.packages("dplyr")
library(dplyr)

### get file names
# # Windows
# strInPath <- "D:/Google/School/2026Summer-BML-UCDGAP/Data/rawData/EMPA"
# strOutPath <- "D:/Google/School/2026Summer-BML-UCDGAP/cleanData/EMPA"

# Linux
strInPath <- "/mnt/internalShared/Google/School/2026Summer-BML-UCDGAP/Data/rawData/EMPA"
strOutPath <- "/mnt/internalShared/Google/School/2026Summer-BML-UCDGAP/cleanData/EMPA"

fileList <- list.files(strInPath)
estuaryList <- list()
estuaryCombined <- data.frame()
index = 0

### import all the files, move  into R, remove unwanted columns
for (i in fileList){
  index <- index+1
  strFullName <- paste0(strInPath,"/",i)
  dfImport <- read.csv(strFullName) %>% mutate(sensorid = as.character(sensorid))
  estuaryName = paste0("estuary-",substr(i,start = 1,stop=7))
  fullWriteName = paste0(strOutPath,"/",estuaryName,"CleaningStep01.csv")
  ### Remove columns
  dfImport <- dfImport %>%
    select (
      -raw_ph,
      -raw_ph_qcflag,
      -raw_turbidity,
      -raw_turbidity_qcflag,
      -raw_turbidity_unit,
      -raw_chlorophyll,
      -raw_chlorophyll_unit,
      -raw_chlorophyll_qcflag,
      -raw_orp,
      -raw_orp_unit,
      -raw_orp_qcflag,
      -qaqc_comment
      )
  ### Remove rows with no sensor or unknown sensor
  #Various cleaning tasks
  dfImport <- dfImport[dfImport$sensortype != "", ]
  dfImport <- dfImport[dfImport$sensortype != "unknown", ]
  
  
  write.csv(dfImport, fullWriteName, row.names = FALSE)
  print(cat(index," ",estuaryName))
}


