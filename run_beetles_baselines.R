#renv::restore()

print(paste0("Running Creating Beetles baselines at ", Sys.time()))


print(paste0("Running mean model at ", Sys.time()))
source("models/beetles_mean.R")
print(paste0("Completed mean model ", Sys.time()))

RCurl::url.exists("https://hc-ping.com/28df2e97-6036-4fa1-9d8a-6aa9fbaa0726")
