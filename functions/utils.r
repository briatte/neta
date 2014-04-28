#' Silent package loader
#' 
try_load <- function(x) {
  return(suppressPackageStartupMessages(require(x, character.only = TRUE, quietly = TRUE)))
}

#' Download function
#'
try_dl <- function(u, f, stop = TRUE, sleep = 0) {

  y = try(download(u, f, mode = "wb", quiet = TRUE), silent = TRUE)

  if("try-error" %in% class(y) & stop)
    return(stop(paste("Scraper error:", u)))

  if(sleep > 0)
      Sys.sleep(sleep)
}

#' Messaging
#'
msg <- function(...) {
  return(message(paste(...)))
}

#' File size
#'
file_size <- function(x) {
  return(paste(round(file.info(x)$size / 10^6, 1), "MB"))
}

#' Zip raw data
#'
zip_files <- function(x = list.dirs("raw", full.names = TRUE, recursive = FALSE), verbose = TRUE) {
  for(i in x) {
    file = paste0(i, ".zip")
    suppressWarnings(file.remove(file))
    zip(file, files = i, flags = "-r9Xq", extras = "-x __MACOSX* *.DS_Store")
    if(verbose)
      msg("Zipped:", i, file_size(file))
  }
}

#' Update National Assembly data
#
set_update <- function(sessions) {

  for(i in sessions) {

    # refresh all bill indexes
    files = c("depots", "projets", "propositions", "resolutions", "resol-art34-1", "amendements")
    files = c(
      paste0("raw/an/", files, ".rda"),                  # master datasets
      paste0("raw/an/indexes/", files, ".", i, ".html"), # legislation indexes
      paste0("raw/an/indexes/sponsors", i, ".txt"),      # sponsorship logs
      paste0("raw/an/indexes/amendements", i, ".csv")    # amendments
    )

    # refresh network objects
    files = c(files,
              paste0("data/am_an", i, ".rda"),
              paste0("data/am_se", i, ".rda")
              )

    suppressWarnings(file.remove(files))

  }

  msg("Legislatures to be updated:", sessions)

}

#' Combine amendments and bills data series
#' 
get_data <- function(...) {
  
  datasets = c(...)
  stopifnot(all(datasets %in% c("an", "se")))
  
  cols = c("uid", "legislature", "date", "t", "texte", "sort")
  for(i in datasets) {
        
    load(paste0("data/am_", i, ".rda"))
    
    amendments = amendments[ !amendments$government & amendments$sample, cols ]

    sponsorships = unique(sponsorships[ sponsorships$sample, 1:4 ]) # deduplicate rows
    
    sample = table(sponsorships$uid) > 1
    sample = names(sample)[ sample ]
    sponsorships = sponsorships[ sponsorships$uid %in% sample, c("uid", "name", "status") ]

    sp = sponsorships
    au = sponsors
    
    load(paste0("data/bi_", i, ".rda"))
    
    bills = bills[ with(bills, !government & sample), cols ]
    bills$sort = gsub("ée$", "é", bills$sort) # harmonized outcome
    
    sponsorships = unique(sponsorships[ sponsorships$sample, 1:4 ]) # deduplicate rows
    
    sample = table(sponsorships$uid) > 1
    sample = names(sample)[ sample ]
    sponsorships = sponsorships[ sponsorships$uid %in% sample, c("uid", "name", "status") ]
    
    # merge shorter amendment series
    sessions = na.omit(as.numeric(amendments$legislature))
    sessions = min(sessions):max(sessions)

    data = lapply(sessions, function(x) {
      a = subset(amendments, legislature == x)
      b = subset(bills, legislature == x)
      msg("Legislature", x, nrow(a), "amendments", nrow(b), "bills")
      rbind(
        cbind( a, type = "amendment" ), 
        cbind( b, type = "bill" )
      )
    })
    data = rbind.fill(data)
    
    b = bills[ bills$legislature < min(sessions), ]
    b = b[ !is.na(b$legislature), ]

    msg("Appending legislatures", paste0(range(unique(b$legislature)), collapse = "-"), nrow(b), "bills")

    data = rbind(
      cbind( b, type = "bill" ),
      data
    )
        
    print(table(data$type, data$sort, exclude = NULL))

    sponsors = read.csv(paste0("raw/", i, "/",
                        ifelse(i == "an", "deputes", "senateurs"), ".txt"), 
                        stringsAsFactors = FALSE)
    
    legislation = sort_df(unique(data), c("legislature", "uid"))
    
    legislation$government = 0
    legislation$sample = TRUE
        
    sponsorships = lapply(sessions, function(x) {
      rbind(
        subset(sp, uid %in% data$uid),
        subset(sponsorships, uid %in% data$uid)
      )
    })
    sponsorships = rbind.fill(sponsorships)

    sponsorships = sort_df(unique(sponsorships), c("uid", "status"))
    
    file = paste0("data/", i, ".rda")
    save(legislation, sponsorships, sponsors, file = file)
    
    msg("Saved:", nrow(data), "items to", file, file_size(file))
    
  }
  
}

# kthxbye
