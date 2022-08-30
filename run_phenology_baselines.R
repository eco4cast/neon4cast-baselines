#renv::restore()

print(paste0("Running Creating phenology Targets at ", Sys.time()))


print(paste0("Running daily climatology at ", Sys.time()))
source("models/phenology_climatology.R")
print(paste0("Completed daily climatology ", Sys.time()))

print(paste0("Running daily persistence at ", Sys.time()))
source("models/phenology_persistenceRW.R")
print(paste0("Completed daily persistenceRW ", Sys.time()))

RCurl::url.exists("https://hc-ping.com/a5a12c66-6e38-4415-aa40-4eb24881a949")
