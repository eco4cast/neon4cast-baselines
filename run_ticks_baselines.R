#renv::restore()

print(paste0("Running Creating ticks baselines at ", Sys.time()))


print(paste0("Running climatology at ", Sys.time()))
source("models/ticks_climatology.R")
print(paste0("Completed climatology ", Sys.time()))
print(paste0("Running mean at ", Sys.time()))
source("models/ticks_mean.R")
print(paste0("Completed mean ", Sys.time()))

RCurl::url.exists("https://hc-ping.com/9fd0a2f4-13b3-4d11-880b-1487a1d801ca")
