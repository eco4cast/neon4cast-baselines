renv::restore()

print(paste0("Running Creating ticks baselines at ", Sys.time()))


print(paste0("Running climatology at ", Sys.time()))
source("models/ticks_climatology.R")
print(paste0("Completed climatology ", Sys.time()))
print(paste0("Running mean at ", Sys.time()))
source("models/ticks_mean.R")
print(paste0("Completed mean ", Sys.time()))