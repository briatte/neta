#' Parse sponsors
#'
#' Reports missing legislature/sponsor name dyads in the sponsorships.
#' @return the postprocessed sponsors
parse_sponsors <- function(data, sponsorships, sponsors, 
                           by.x = "name", by.y = "nom", verbose = FALSE) {

  # validate data and sponsors
  stopifnot(c("uid", "legislature", "url") %in% names(data))
  stopifnot(c("uid", by.x) %in% names(sponsorships))
  stopifnot(c(by.y, "legislature") %in% names(sponsors))
  
  sponsors$sample = FALSE
  sponsors = sponsors[ !is.na(sponsors$legislature), ]
  
  for(i in unique(data$legislature)) {

    s = unique(subset(sponsors, legislature == i)[, by.y ])
    a = unique(subset(data, legislature == i & !government)$uid)
    a = subset(sponsorships, uid %in% a)[, by.x ]
    
    # sponsorships with no sponsor name
    n = sum(is.na(a))
    if(n)
      warning(paste("Missing:", n, "sponsor names in legislature", i,
                    round(n / length(a), 2), "% of total"))
    
    # sponsorships with no sponsor name in the same legislature
    m = length(a[ !a %in% c(s, NA) ])
    if(m)
      warning(paste("Missing:", m, "sponsor details in legislature", i,
                    round(m / length(a), 2), "% of total"))
    
    # sample sponsors
    sponsors[ sponsors$legislature == i, "sample" ] = 
      sponsors[ sponsors$legislature == i, by.y ] %in% s

  }

  return(sponsors)
  
}

#' Parse sponsorships
#' 
#' Reports unused sponsorships and missing or incomplete sponsors.
#' @return the postprocessed sponsorships
parse_sponsorships <- function(data, sponsorships, sponsors,
                               by.x = "name", by.y = "nom", verbose = FALSE) {
  
  # validate data and sponsors
  stopifnot(c("uid", "texte", "government") %in% names(data))
  data = unique(data[, c("uid", "texte", "government")])
  
  uid = sponsorships$uid %in% unique(data$uid) & 
    !sponsorships$uid %in% unique(data$uid[ is.na(data$government) ])

  if(any(!uid) | verbose)
    warning(paste("Dropping:", sum(!uid), "unidentified sponsorships"))
  
  gov = sponsorships$uid %in% unique(data$uid[ data$government == 1])

  if(any(gov) | verbose)
    warning(paste("Dropping:", sum(gov), "government sponsorships"))  
  
  # final sample check: sponsorship appears in master data and is not government
  sponsorships$sample = uid & !gov

  # check for sponsor details
  stopifnot(c(by.y, "party", "url") %in% names(sponsors))
  
  # subset to unique name/url
  sponsors = na.omit(unique(sponsors[, c(by.y, "party", "url")]))
  
  # process sponsorships data
  msg("Parsing:", nrow(sponsorships), "sponsorships")
  
  # check for type of author
  names(sponsorships)[ which(names(sponsorships) == "url") ] = "url_an"
  sponsorships = merge(sponsorships,
                       sponsors,
                       by.x = by.x, by.y = by.y,
                       all.x = TRUE)
  
  # recode unidentified authors
  sponsorships[ !sponsorships$url %in% unique(sponsors$url), by.x ] = NA
  
  # add party groups if still missing
  if(!"party" %in% names(sponsorships))
    sponsorships = merge(sponsorships,
                         sponsors[, -which(names(sponsors) == "url") ], 
                         by.x = by.x, by.y = by.y,
                         all.x = TRUE)

  cols = c("uid", "sample", by.x, "status", "party", "url")
  return(arrange(sponsorships[, cols ], uid, status))

}

#' Parse raw data
#' 
#' Reports missing and sampled items in the raw data.
#' @return the postprocessed data
parse_data <- function(data, sponsorships, sponsors,
                       by.x = "name", by.y = "nom", verbose = FALSE) {
  
  stopifnot(c("uid", "texte", "government") %in% names(data))
  stopifnot(c(by.y, "sample") %in% names(sponsors))
  stopifnot(c(by.x, "uid", "status") %in% names(sponsorships))
  stopifnot(all(unique(sponsorships$status) %in% c("author", "cosponsor")))

  # count missing data
  msg("Missing:", sum(is.na(data$government)), "sponsorships",
      round(100 * sum(is.na(data$government)) / nrow(data), 2), 
      "% of total")

  # count missing sponsors
  msg("Missing:", sum(is.na(sponsorships[, by.x ])), "sponsors",
      round(100 * sum(is.na(sponsorships[, by.x ])) / nrow(sponsorships), 2), 
      "% of total")

  # paste sponsor lists (does not yet take by.x and by.y into account)
  auteurs = unique(sponsors$nom[ sponsors$sample ])
  auteurs = summarise(group_by(subset(sponsorships, status %in% c("author", "cosponsor")), uid),
      sample = ifelse(all(is.na(name)), FALSE, sum(name %in% auteurs, na.rm = TRUE) > 1))

  # merge sponsor lists
  data = merge(data[, c("legislature", "uid", "date", "t", "url", "texte", "titre", "sort", "government") ],
               auteurs, by = "uid", all = TRUE)

  # check each legislature contains all amendment sponsors
  for(i in unique(data$legislature)) {

    m = sum(data$sample[ data$legislature == i], na.rm = TRUE)

    msg("Sampled: legislature", i, m, "items",
        round(100 * m / sum(data$legislature == i & 
        data$government == 0, na.rm = TRUE), 2), "% of total")

  }

  # sampling frame
  msg("Sampled:", sum(data$sample, na.rm = TRUE), "cosponsorships",
      round(100 * sum(data$sample, na.rm = TRUE) / nrow(data), 2), 
      "% of all data")

  return(data)

}

#' Get French constituency geocodes
#'
#' Discards ex-colonies, overseas and parser bugs.
parse_geo <- function(x) {

  # drop locations outside of metropolitan France
  x = x[ !grepl("hors de France|Somalis|Président|Orléansville-Médéa|Oran-Tlemcen|Oasis|Tizi-Ouzou|Alger|Constantine|Saoura|Guyane|Guadeloupe|La Réunion|Saint-Pierre-et-Miquelon|Martinique|Nouvelle-Calédonie|Polynésie|Wallis|Futuna|Saint-Barthélemy|Saint-Martin|Gabon|Tchad|Algérie|Oubangui|Affars", x) ]
  x = na.omit(unique(x[ nchar(x) > 2 ]))

  # geocode constituency/canton
  msg("Geocoding:", length(x), "constituencies")
  msg("Google Maps API Terms of Service: http://developers.google.com/maps/terms")

  x = suppressMessages(cbind(nom_circo = x, 
                             geocode(paste(x, "France"), output = "latlona")))
  
  # exclude imprecise results
  return(subset(x, address != "france")[, -4])
  
}

#' Get National Assembly legislature number
#'
#' Harmonizes network construction between both chambers. Discards legislation 
#' examined by different sponsors during different legislatures.
parse_legislature <- function(m, subset = 1:14) {
  l = rep(NA, length(m))
  l [ m >= ymd("1958-11-23") & m < ymd("1962-11-18") ] = 1  # UNR
  l [ m >= ymd("1962-11-18") & m < ymd("1967-03-05") ] = 2  # UNR
  l [ m >= ymd("1967-03-05") & m < ymd("1968-06-23") ] = 3  # UD-Ve R
  l [ m >= ymd("1968-06-23") & m < ymd("1973-03-04") ] = 4  # UDR
  l [ m >= ymd("1973-03-04") & m < ymd("1978-03-12") ] = 5  # UDR
  l [ m >= ymd("1978-03-12") & m < ymd("1981-06-14") ] = 6  # RPR
  l [ m >= ymd("1981-06-14") & m < ymd("1986-03-16") ] = 7  # SOC
  l [ m >= ymd("1986-03-16") & m < ymd("1988-06-05") ] = 8  # SOC/RPR split, PR, cohab.
  l [ m >= ymd("1988-06-05") & m < ymd("1993-03-21") ] = 9  # SOC
  l [ m >= ymd("1993-03-21") & m < ymd("1997-05-25") ] = 10 # RPR, cohab.
  l [ m >= ymd("1997-05-25") & m < ymd("2002-06-09") ] = 11 # SOC, cohab.
  l [ m >= ymd("2002-06-09") & m < ymd("2007-06-10") ] = 12 # UMP
  l [ m >= ymd("2007-06-10") & m < ymd("2012-06-10") ] = 13 # UMP
  l [ m >= ymd("2012-06-10") & m < ymd("2017-06-10") ] = 14 # SOC, ongoing
  l[ !l %in% subset ] = NA
  return(l)
}

#' Get French ymd dates and time counter
#'
#' The \code{t} counter proxies for time since legislature start.
parse_time <- function(data) {

  stopifnot("date" %in% names(data))

  # year, month, weekday, and days since first registered item
  x = with(data, data.frame(
    annee = year(date),
    mois  = month(date),
    jour  = weekdays(date, abbreviate = TRUE),
    t     = julian(date, origin = min(date, na.rm = TRUE))
  ))
  
  # French weekdays (reordered from English equivalents)
  d = c("lun", "mar", "mer", "jeu", "ven", "sam", "dim")
  levels(x$jour) = d[ c(5, 1, 6, 7, 4, 2, 3) ]
  x$jour = factor(x$jour,
                  levels = ,
                  ordered = TRUE)

  x = cbind(data, x)
  x = subset(x, !is.na(date))

  missing = nrow(data) - nrow(x)

  # rewrite tse if the data contain more than one legislature
  if("legislature" %in% names(x)) {
  
    x = subset(x,
               (legislature == 8 &
                 (date >= as.Date(ymd("1986-03-16"))  & 
                  date < as.Date(ymd("1988-06-05")))) |
               (legislature == 9 &
                 (date >= as.Date(ymd("1988-06-05"))  & 
                  date < as.Date(ymd("1993-03-21")))) |
               (legislature == 10 &
                 (date >= as.Date(ymd("1993-03-21"))  & 
                  date < as.Date(ymd("1997-05-25")))) |
               (legislature == 11 &
                 (date >= as.Date(ymd("1997-05-25"))  & 
                  date < as.Date(ymd("2002-06-09")))) |
               (legislature == 12 &
                 (date >= as.Date(ymd("2002-06-09"))  & 
                  date < as.Date(ymd("2007-06-10")))) |
               (legislature == 13 & 
                 (date >= as.Date(ymd("2007-06-10"))  &
                  date < as.Date(ymd("2012-06-10")))) |
               (legislature == 14 &
                 (date >= as.Date(ymd("2012-06-10"))  &
                  date < as.Date(ymd("2017-06-10")))) # expected
              )

    x = subset(x, !is.na(legislature))
    x = group_by(x, legislature)
    x = mutate(x, t = as.numeric(julian(date, origin = min(date))))

    } else {
      warning("No legislature to parse in dataset.")
    }
  
  d = nrow(data) - nrow(x)
  if(d) {    
    warning(paste("Dropped", d, "timestamps:",
                  missing, "missing", d - missing, "problematic",
                  round(100 * d / nrow(data), 2), "% of all data"))
  }
  
  return(as.data.frame(x))

}

#' Word cleaner (preserves case and text)
#'
#' @return a just-letters, no-extra-whitespace character string
clean <- function(x, preserve = NULL) {
  
  # strip to plain ASCII
  x = iconv(x, to = "ASCII//TRANSLIT")
  
  # drop accents
  x = gsub("`|'|\\^|\"", "", x)
  
  # drop numbers and punctuation
  x = gsub(paste0("[^", preserve,"\\sa-zA-Z]"), " ", x)
  
  # strip multiple spaces
  x = gsub("\\s+", " ", x)
  
  return(x)
}

#' Convert party colors to party abbreviations
#'
#' Returns basic codes for each major party group, based on colors defined
#' in the sponsors data. Codes are ordered left-right with unaffiliateds at the
#' centre of the scale. The same codes are used for both chambers.
#' @return an ordered vector of color codes for Communists (COM, red), 
#' ECO (Greens, green), Socialists (SOC, pink), Radicals (RAD, yellow),
#' SE (unaffiliateds, grey), Centrists (CEN, orange), rightwing (DRO, blue)
#' and Front National (FN, brown) group members.
parse_color <- function(x, colors,
                        order = c("COM", "ECO", "RAD", "SOC", "SE", "CEN", "DRO", "FN"),
                        party = c(
                          "#4DAF4A" = "ECO",
                          "#E41A1C" = "COM",
                          "#999999" = "SE",
                          "#FFFF33" = "RAD",
                          "#F781BF" = "SOC",
                          "#FF7F00" = "CEN",
                          "#377EB8" = "DRO",
                          "#A65628" = "FN"
                        )) {

  return(factor(party[ colors[ x ] ], order, ordered = TRUE))

}

#' Clean French names
#'
#' Removes particles and apostrophes of the following forms:
#    X d'Y
#    X l'Y
#    X à Y
#    X de Y
#    X du Y
#    X le Y
# @return an uppercase shortname
clean_names <- function(x) {
  
  # remove particles as in "M. d'X, Mme l'Y"
  x = clean(gsub("d'|l'", "", tolower(x)))
  
  # remove particles as in "M. X à Y, Mme X de/du Y"
  x = gsub(" a | (d|l)(e|u) ", " ", x) 
  
  # return uppercase
  return(toupper(x))
  
}

#' Clean French dates
#' 
#' Note: the \code{gsub} line is solved in R > 3.0.3 and can be commented out.
clean_date <- function(x) {
  
  # French locale bug
  if(getRversion() < "3.0.3") x = gsub("juillet", "07", x)

  # National Assembly data mistakes
  x[ grepl("1005|1970", x) ] = NA

  # parse to Date format
  x = suppressWarnings(parse_date_time(x, "%d %m* %Y", locale = "fr_FR.UTF-8"))
  x = as.Date(x)
  
  return(x)
}

#' Clean amendment outcomes
#'
#' Uses generic categories for both National Assembly and Senate. Bills are only
#' coded 'adopted' or 'rejected': the other categories (which all code for some
#' some form of rejection) are available for amendments only.
clean_sort <- function(x) {
  
  # collapse 'irrecevables', 'satisfaits' and unclassifiables
  x[ grepl("Re$|4\\d+|^Satisfait", x) | nchar(x) < 2 ] = NA

  # in case of strange accent issues
  x[grepl("^Adopt" , x)] = "Adopté"
  x[grepl("^Rejet" , x)] = "Rejeté"
  x[grepl("^Retir" , x)] = "Retiré"
  x[grepl("^Tomb"  , x)] = "Tombé"

  # factorize
  x = factor(x,
             levels = c("Adopté", "Rejeté", "Retiré", "Non soutenu", "Tombé"))
  
  return(x)
}

# have a nice day
