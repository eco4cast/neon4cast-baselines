print(paste0("Running Creating Aquatics baselines at ", Sys.time()))


print(paste0("Running daily climatology at ", Sys.time()))
source("aquatics_climatology.R")
print(paste0("Completed daily climatology ", Sys.time()))