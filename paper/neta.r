library(ggplot2)
library(lubridate)
library(network)
library(igraph)
library(tnet)
library(plyr)
library(RColorBrewer)
library(reshape2)
library(sna)
library(xtable)
library(dplyr)

#' Draft plots

# plot_attributes(sessions = 8:14)
# plot_attributes(sessions = 8:14  , type = "bi")
# plot_attributes(sessions = 12:14 , type = "am")
# plot_egos("data/an14.rda")
# plot_egos("data/se14.rda")
# plot_sponsorships("data/an.rda", sessions = 13:14)
# plot_sponsorships("data/se.rda", sessions = 13:14)
# plot_groups(sessions = 8:14)

#' Test alpha parameter

#' Get table
#'
#'#' @examples
#' # type + chamber specific data
#' tbl = get_tbl('data/bi_an.rda')
#' 
#' # counts and mean cosponsors (incl. Fowler table columns 3-4 and 8)
#' print(xtable(tbl[, c(1:4, 9)], digits = 2), include.rownames = FALSE)
#' 
#' # MP-specific means (incl. Fowler table columns 5-7)
#' print(xtable(tbl[, c(1, 10, 7:8)], digits = 2), include.rownames = FALSE)
#' 
#' # network properties
#' print(xtable(tbl[, c(1, 15, 11:14)], digits = 2), include.rownames = FALSE)
#' 
#' # network modularity
#' print(xtable(tbl[, c(1, 16:20)], digits = 2), include.rownames = FALSE)
#' 
#' # chamber-specific data
#' tbl = get_tbl('data/an.rda')
#' 
#' # just counts (incl. Fowler table columns 3-4)
#' print(xtable(tbl[, c(1:6)], digits = 2), include.rownames = FALSE)
#' 
#' # means (incl. Fowler table columns 5-7)
#' print(xtable(tbl[, c(1, 11, 9:10, 12)], digits = 2), include.rownames = FALSE)
#' 
#' # network properties (incl. Fowler table columns 9-10)
#' print(xtable(tbl[, c(1, 17, 13:16)], digits = 2), include.rownames = FALSE)
#' 
#' # network modularity (as in Waugh et al.)
#' tbl$"$M / \max{M}$" = tbl$Modularity / apply(tbl[, c(19, 21)], 1, max)
#' print(xtable(tbl[, c(1, 18, 20, 22:23)], digits = 2), include.rownames = FALSE)
get_tbl <- function(key, sessions = 8:14) {
  
  # load datasets
  load(key)
  data = get(ifelse(exists("bills"), "bills",
                    ifelse(exists("amendments"), "amendments", "legislation")))
  
  # extract properties
  tbl = data.frame()
  for(x in sessions) {
    
    years = c("1986-1988", "1988-1993", "1993-1997", "1997-2002", "2002-2007", "2007-2012", "2012--")[x - 7]
    years = paste0(years, " (", x, ")")
    
    # network
    net = get(paste0("net", x))
    
    s = filter(data, legislature == x)$uid
    s = filter(sponsorships, uid %in% unique(s))
    
    # counts
    n_items = n_distinct(s$uid)     # items
    if("type" %in% names(data)) {
      n_bills = n_distinct(s$uid[ s$uid %in% filter(data, legislature == x, type == "bill")$uid ])
      if(n_distinct(data$type[ data$legislature == x ]) > 1)
        n_amdts = n_distinct(s$uid[ s$uid %in% filter(data, legislature == x, type == "amendment")$uid ])
      else
        n_amdts = 0
    } else {
      n_bills = n_amdts = NA
    }
    
    n_sponsors = n_distinct(s$name) # sponsors
    n_authors = n_distinct(s$name[ s$status == "author" ]) # first authors
    n_cosponsors = n_distinct(s$name[ s$status == "cosponsor" ])
    
    ties = network.edgecount(net) /
      (n_authors * n_cosponsors - n_cosponsors * sum(!unique(s$name[ s$status == "author" ]) %in% 
                                                       unique(s$name[ s$status == "author" ])))
    
    # number of parliamentary groups
    n_groups = na.omit(net %v% "party")
    n_groups = n_groups[ n_groups != "SE" ]
    n_groups = n_distinct(names(table(n_groups))[ table(n_groups) > 9 ])
    
    # mean bills per legislator
    m_sponsorships = mean(table(s$name[ s$status == "author" ]), na.rm = TRUE)
    
    # mean cosponsorships per legislator
    m_cosponsorships = mean(table(s$name[s$status == "cosponsor"]), na.rm = TRUE)
    
    # mean cosponsors per bill
    m_cosponsors = mean(table(s$name) - 1, na.rm = TRUE)
    
    # mean cosponsors per legislator (from the directed edge list)
    edges = get(paste0("edges", x))
    cosponsors = mean(summarise(group_by(edges, i), y = length(j))$y, na.rm = TRUE)
    
    load(paste0(gsub("data/(.*).rda", "measures/modularity/\\1", key), x, ".rda"))
    
    # table
    tbl = rbind(tbl,
                data.frame(years, n_items, n_bills, n_amdts,
                           n_groups, n_sponsors, n_authors, n_cosponsors,
                           m_sponsorships, m_cosponsorships,
                           m_cosponsors, cosponsors, 
                           density = net %n% "density",
                           degree = net %n% "degree",
                           distance = net %n% "distance",
                           clustering = net %n% "clustering",
                           ties = 100 * ties,
                           modularity = modularity,
                           modularity_max = max(c(modularity(louvain), modularity(walktrap))),
                           modularity_ratio = modularity / max(c(modularity(louvain), modularity(walktrap))),
                           louvain_dg = n_distinct(louvain$membership) - n_groups,
                           walktrap_dg = n_distinct(walktrap$membership) - n_groups,
                           stringsAsFactors = FALSE))
    
  }
  
  # table column titles
  colnames(tbl) = c("Legislature",
                    "$\\Sigma$~items", "$\\Sigma$~bills", "$\\Sigma$~amdts", # items
                    "$\\Sigma$~groups", "$\\Sigma$~sponsors", # groups and sponsors
                    "$\\Sigma$~authors", "$\\Sigma$~cosponsors", # unique counts
                    "$\\mu$~items/MP", "$\\mu$~cosponsorships/MP", 
                    "$\\mu$~cosponsors/item", "$\\mu$~cosponsors/MP", #means
                    "Density", "Degree", "Distance", "Clustering", "\\% ties",
                    "Modularity", "$\\max{M}$", "$M / \\max{M}$",
                    "$\\Delta$~Louvain", "$\\Delta$~Walktrap")
  
  if(exists("amendments")) {
    colnames(tbl)[2] = "$\\Sigma$~amdts"
    colnames(tbl)[9] = "$\\mu$~amdts/MP"
    tbl = tbl[, -3:-4]
  }
  
  if(exists("bills")) {
    colnames(tbl)[2] = "$\\Sigma$~bills"
    colnames(tbl)[9] = "$\\mu$~bills/MP"
    tbl = tbl[, -3:-4]
  }
  
  return(tbl)
  
}

t = "tables/an.tex"
if(!file.exists(t)) {
  tbl = get_tbl('../data/an.rda')
  print(xtable(tbl[, c(1:6)], digits = 2, 
               display = c("s", "d", "d", "d", "d", "d", "d")),
        sanitize.colnames.function = as.character,
        floating = FALSE, include.rownames = FALSE, file = t)
}
t = "tables/se.tex"
if(!file.exists(t)) {
  tbl = get_tbl('../data/se.rda')
  print(xtable(tbl[, c(1:6)], digits = 2,
               display = c("s", "d", "d", "d", "d", "d", "d")),
        sanitize.colnames.function = as.character,
        floating = FALSE, include.rownames = FALSE, file = t)
}
t = "tables/an_modularity.tex"
if(!file.exists(t)) {
  tbl = get_tbl('../data/an.rda')
  print(xtable(tbl[, c(1, 18:20)], digits = 2,
               display = c("s", "s", "f", "f", "f")),
        sanitize.colnames.function = as.character,
        floating = FALSE, include.rownames = FALSE, file = t)
}
t = "tables/se_modularity.tex"
if(!file.exists(t)) {
  tbl = get_tbl('../data/se.rda')
  print(xtable(tbl[, c(1, 18:20)], digits = 2,
               display = c("s", "s", "f", "f", "f")),
        sanitize.colnames.function = as.character,
        floating = FALSE, include.rownames = FALSE, file = t)
}
f = "tables/an_ergm.tex"
if(!file.exists(f)) {
  m = matrix(ncol = 7, nrow = 28)
  t = read.csv("../models/ergm/ergm_0.025_0.975.csv", stringsAsFactors = FALSE)
  n = t[, 2]
  t = t[, 17:30 ]
  names(t)[ !grepl("^AN\\d+", names(t)) ] = "SE"
  for(i in seq(1, 14, by = 2)) {
    m[ c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27),  1 + (i %/% 2) ] = round(t[, i], 2)
    m[ c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28), 1 + (i %/% 2) ] = paste0("(", round(t[, i + 1], 2), ")")
    m[ , 1 + (i %/% 2) ] = gsub("\\(NA\\)", "", m[ , 1 + (i %/% 2) ])
    m[ , 1 + (i %/% 2) ] = gsub("-", "–", m[ , 1 + (i %/% 2) ])
  }
  colnames(m) = c(1986, 1988, 1993, 1997, 2002, 2007, 2012) # 8:14
  m[25:28, ] = round(as.numeric(m[25:28, ]), 0) # round BICs
  Legislature = unlist(lapply(n, c, ""))
  print(xtable(cbind(Legislature, m)[ -c(26, 28), ], align = "llccccccc",
               caption = "ERGM coefficients for all National Assembly legislatures and party groups, with standard errors in brackets below each estimate.", label = "tbl:ergm_an"),
        include.rownames = FALSE, hline.after = c(-1, 0, 24, 26), file = f)
}
f = "tables/se_ergm.tex"
if(!file.exists(f)) {
  m = matrix(ncol = 7, nrow = 26)
  t = read.csv("../models/ergm/ergm_0.025_0.975.csv", stringsAsFactors = FALSE)
  n = t[ !grepl("FN", t[, 2]), 2]
  t = t[ !grepl("FN", t[, 2]), 3:16 ]
  names(t)[ !grepl("^SE\\d+", names(t)) ] = "SE"
  for(i in seq(1, 14, by = 2)) {
    m[ c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25),  1 + (i %/% 2) ] = round(t[, i], 2)
    m[ c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26), 1 + (i %/% 2) ] = paste0("(", round(t[, i + 1], 2), ")")
    m[ , 1 + (i %/% 2) ] = gsub("\\(NA\\)", "", m[ , 1 + (i %/% 2) ])
    m[ , 1 + (i %/% 2) ] = gsub("-", "–", m[ , 1 + (i %/% 2) ])
  }
  colnames(m) = c(1986, 1988, 1993, 1997, 2002, 2007, 2012) # 8:14
  m[23:26, ] = round(as.numeric(m[23:26, ]), 0) # round BICs
  Legislature = unlist(lapply(n, c, ""))
  print(xtable(cbind(Legislature, m)[ -c(24, 26), ], align = "llccccccc",
               caption = "ERGM coefficients for all Senate legislatures and party groups, with standard errors in brackets below each estimate.", label = "tbl:ergm_se"),
        include.rownames = FALSE, , hline.after = c(-1, 0, 22, 24), file = f)
}
# # type + chamber specific data
# t = "tables/bi_an.tex"
# if(!file.exists(t)) {
#   tbl = get_tbl('../data/bi_an.rda')
#   print(xtable(tbl[, c(1:4, 9)], digits = 2), include.rownames = FALSE, file = t)
# }
# t = "tables/am_an.tex"
# if(!file.exists(t)) {
#   tbl = get_tbl('../data/am_an.rda', 12:14)
#   print(xtable(tbl[, c(1:4, 9)], digits = 2), include.rownames = FALSE, file = t)
# }
# t = "tables/bi_se.tex"
# if(!file.exists(t)) {
#   tbl = get_tbl('../data/bi_se.rda')
#   print(xtable(tbl[, c(1:4, 9)], digits = 2), include.rownames = FALSE, file = t)
# }
# t = "tables/am_se.tex"
# if(!file.exists(t)) {
#   tbl = get_tbl('../data/am_se.rda', 11:14)
#   print(xtable(tbl[, c(1:4, 9)], digits = 2), include.rownames = FALSE, file = t)
# }