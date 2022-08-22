library(cronR)

home_dir <-  path.expand("~")
log_dir <- path.expand("~/log/cron")

repo <- "neon4cast-baselines"

#Aquatics
health_checks_url <- "https://hc-ping.com/a848914e-9abf-45e4-bcf3-27f570cc3623"
cmd <- cronR::cron_rscript(rscript = file.path(home_dir, repo,"run_aquatics_baselines.R"),
                           rscript_log = file.path(log_dir, "aquatics-climatology.log"),
                           log_append = FALSE,
                           workdir = file.path(home_dir, repo),
                           trailing_arg = paste0("curl -fsS -m 10 --retry 5 -o /dev/null ", health_checks_url))
cronR::cron_add(command = cmd, frequency = 'daily', at = "11AM", id = 'aquatics-null-models')

#Terrestrial
health_checks_url <- "https://hc-ping.com/bbe3ddc7-4020-4c53-bb13-08580d765e32"
cmd <- cronR::cron_rscript(rscript = file.path(home_dir, repo,"run_terrestrial_baselines.R"),
                           rscript_log = file.path(log_dir, "terrestrial-null.log"),
                           log_append = FALSE,
                           workdir = file.path(home_dir, repo),
                           trailing_arg = paste0("curl -fsS -m 10 --retry 5 -o /dev/null ", health_checks_url))
cronR::cron_add(command = cmd, frequency = 'daily', at = "11AM", id = 'terrestrial-null')

#Phenology
health_checks_url <- "https://hc-ping.com/a5a12c66-6e38-4415-aa40-4eb24881a949"
cmd <- cronR::cron_rscript(rscript = file.path(home_dir, repo,"run_phenology_baselines.R"),
                           rscript_log = file.path(log_dir, "phenology-null.log"),
                           log_append = FALSE,
                           workdir = file.path(home_dir, repo),
                           trailing_arg = paste0("curl -fsS -m 10 --retry 5 -o /dev/null ", health_checks_url))
cronR::cron_add(command = cmd, frequency = 'daily', at = "10PM", id = 'phenology-nullmodel')


# Beetles
health_checks_url <- "https://hc-ping.com/28df2e97-6036-4fa1-9d8a-6aa9fbaa0726"
cmd <- cronR::cron_rscript(rscript = file.path(home_dir, repo, "run_beetles_baselines.R"),
                           rscript_log = file.path(log_dir, "beetles-null.log"),
                           log_append = FALSE,
                           workdir = file.path(home_dir, repo),
                           trailing_arg = paste0("curl -fsS -m 10 --retry 5 -o /dev/null ", health_checks_url))
cronR::cron_add(command = cmd, frequency = "0 12 * * SUN", id = 'beetles-null')

# Ticks
health_checks_url <- "https://hc-ping.com/9fd0a2f4-13b3-4d11-880b-1487a1d801ca"
cmd <- cronR::cron_rscript(rscript = file.path(home_dir, repo,"run_ticks_baselines.R"),
                           rscript_log = file.path(log_dir, "ticks-null.log"),
                           log_append = FALSE,
                           workdir = file.path(home_dir, repo),
                           trailing_arg = paste0("curl -fsS -m 10 --retry 5 -o /dev/null ", health_checks_url))
cronR::cron_add(command = cmd, frequency = "0 1 * * SUN", id = 'ticks-null')
