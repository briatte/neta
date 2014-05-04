
# scripts
scripts = paste0("functions/", dir("functions", pattern = ".r"))

# load all functions
scripts = lapply(scripts, source)

# required packages
packages = c("animation", "coda", "downloader", "ergm", "ggplot2", "ggmap", "grid",
             "latentnet", "lubridate", "network", "plyr", "RColorBrewer", "reshape",
             "rjson", "RPostgreSQL", "stringr", "XML", "dplyr", "tnet", "rgexf",
             "sna", "scales", "xtable")

# install-and-load
packages = lapply(packages, function(x) {
  if(!try_load(x)) {
    install.packages(x, quiet = TRUE)
    try_load(x)
  }
})

# folders
folders = c("amendements", "dossiers", "indexes")
folders = c("app", "data", "plots", "raw", "models",
            paste0("models/", c("degree", "modularity", "ergm", "ergmm")),
            paste0("raw/",
                   c(
                     "an", paste0("an/", c(folders, "deputes")),
                     "se", paste0("se/", c(folders, "senateur"))
                     )
                   )
            )

# create if absent
folders = lapply(folders, function(x) { if(!file.exists(x)) dir.create(x) })

# dev ggnet code
if(!exists("ggnet")) {
  download("https://raw.githubusercontent.com/briatte/ggnet/master/ggnet.R",
           "functions/ggnet.r", mode = "wb", quiet = TRUE)
  source("functions/ggnet.r")
}

# go
