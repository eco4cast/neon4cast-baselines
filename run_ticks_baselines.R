print(paste0("Running Creating ticks baselines at ", Sys.time()))


print(paste0("Running daily climatology at ", Sys.time()))
source("models/ticks_climatology.R")
print(paste0("Completed daily climatology ", Sys.time()))