#renv::restore()

print(paste0("Running Creating Aquatics baselines at ", Sys.time()))


print(paste0("Running daily climatology at ", Sys.time()))
source("models/aquatics_climatology.R")
print(paste0("Completed daily climatology ", Sys.time()))
print(paste0("Running daily persistence at ", Sys.time()))
source("models/aquatics_persistenceRW.R")
print(paste0("Completed daily persistence ", Sys.time()))