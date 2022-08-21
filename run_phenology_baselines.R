print(paste0("Running Creating phenology Targets at ", Sys.time()))


print(paste0("Running daily climatology at ", Sys.time()))
source("models/phenology_climatology.R")
print(paste0("Completed daily climatology ", Sys.time()))

print(paste0("Running daily persistence at ", Sys.time()))
source("models/phenology_persistance.R")
print(paste0("Completed daily persistance ", Sys.time()))