#' Scrape National Assembly bills
#'
#' @param update force create all sponsor logs (off by default)
get_bills <- function(sessions = 8:14, update = FALSE) {
  
  sessions = 8:14
  load("raw/an/propositions.rda")
  load("raw/an/resolutions.rda")
  load("raw/an/resol-art34-1.rda")
  
  # paste raw data
  data = rbind(propositions, resolutions, get("resol-art34-1"))
  
  msg("Parsing:",
      nrow(propositions), "bills",
      nrow(data) - nrow(propositions), "resolutions")
  
  # fix legislature
  data$legislature = as.numeric(as.character(data$legislature))
  data = subset(data, !is.na(legislature))

  for(i in sessions) {
    file = paste0("raw/an/indexes/sponsors", i, ".txt")
    
    if(!file.exists(file) | update) {
      
      bills = unique(data$url[grepl(paste0("/", i, "/"), data$url)])
      msg("Parsing: legislature", i, length(bills), "bills")
      
      sink(gsub(".txt", ".log", file))
      bills = rbind.fill(lapply(bills, get_authors, verbose = TRUE))
      sink()
      
      write.table(bills, file, sep = ",", col.names = TRUE, row.names = FALSE)
      
    } else {
      
      bills = read.table(file, sep = ",", header = TRUE, stringsAsFactors = FALSE)

      msg("Parsing: legislature", i,
          n_distinct(bills$url), "bills",
          nrow(bills), "sponsors")
      
      # check log
      text = readLines(gsub(".txt", ".log", file))
      text = text [ grepl("^Error: scraper failed to fetch authors", text) ]
      
      if(length(text) > 0) {
        
        # target bills with missing data
        text = gsub("Error: scraper failed to fetch authors at |\\s+", "", text)
        msg("Attempting to fix", length(text), "scraper error(s)")
        
        # scrape again
        pass = lapply(text, get_authors, verbose = FALSE)
        pass = rbind.fill(pass)
        
        # replace rows
        bills = subset(bills, !url %in% text)
        bills = rbind(bills, pass)
        
      }
      
    }
  }
  
  ## final sponsorships dataset
  sponsorships = rbind.fill(lapply(sessions, function(x) {
    
    # load bills
    bills = read.table(paste0("raw/an/indexes/sponsors", x, ".txt"), sep = ",", header = TRUE, stringsAsFactors = FALSE)
    
    # fix homonyms
    if(x == 8)
      bills$name[ bills$name == "BERNARD SCHREINER" ] = "BERNARD JEAN JOSEPH SCHREINER"
    if(x == 9)
      bills$name[ bills$name == "JACQUES HOUSSIN" ] = "JACQUES HOUSSIN NORD PREMIER"
    if(x < 10)
      bills$name[ bills$name == "PIERRE BERNARD" ] = "PIERRE BERNARD TARN"
    
    return(bills)
    
  }))
  
  # fix a few mistakes
  sponsorships$name[ sponsorships$name == "PHILIPPE MARTIN GERS" ] = "PHILIPPE MARTIN"
  sponsorships$name[ sponsorships$name == "PHILIPPE ARMAND MARTIN MARNE" ] = "PHILIPPE ARMAND MARTIN"
  sponsorships$name[ sponsorships$name == "J C R AME BIGNON" ] = "JEROME BIGNON"
  sponsorships$name[ sponsorships$name == "OLIVIER JARD C" ] = "OLIVIER JARDE"
  sponsorships$name[ sponsorships$name == "BERNARD DEBR C" ] = "BERNARD DEBRE"
  sponsorships$name[ sponsorships$name == "C TIENNE BLANC" ] = "ETIENNE BLANC"
  sponsorships$name[ sponsorships$name == "FRAN ASSOIS XAVIER VILLAIN" ] = "FRANCOIS XAVIER VILLAIN"
  sponsorships$name[ sponsorships$name == "FRAN ASSOIS RUGY" ] = "FRANCOIS RUGY"
  sponsorships$name[ sponsorships$name == "JEAN FRAN ASSOIS MANCEL" ] = "JEAN FRANCOIS MANCEL"
  sponsorships$name[ sponsorships$name == "NO L MAM C RE" ] = "NOEL MAMERE"
  sponsorships$name[ 
    sponsorships$name == "LES MEMBRES GROUPE SOCIALISTE RADICAL CITOYEN ET DIVERS GAUCHE ET APPARENT C S" 
    ] = "GROUPE SOCIALISTE RADICAL CITOYEN ET DIVERS GAUCHE ET APPARENTES"
  
  sponsorships = unique(sponsorships)
    
  # get MP details
  deputes = subset(read.csv("raw/an/deputes.txt", stringsAsFactors = FALSE), legislature %in% sessions)

  # URL used as UID in sponsorships
  data = data[, c("url", "legislature", "texte", "date",
                  "amendements", "titre", "origine", "seance",
                  "debats", "loi", "loi_adoptee", "loi_date") ]
  names(data)[1] = "uid"
  
  # full URL separately
  data$url = paste0("http://www.assemblee-nationale.fr", data$uid)
  
  # adopted/rejected
  data$sort = ifelse(data$loi_adoptee, "Adoptée", "Rejetée")
  
  # dummy for government bills (for parsers)
  data$government = 0
  
  # time variables
  data = parse_time(data)
  
  # uid
  u = data$uid
  data = group_by(data, legislature)
  data = mutate(data, uid = paste0("AB",
                                   sprintf("%02d", legislature),
                                   sprintf("%08d", 1:length(uid))
                                   ))
  uids = data$uid
  names(uids) = u
  sponsorships$uid = as.vector(uids[ sponsorships$uid ])
  
  ## parse for network use

  sponsorships = parse_sponsorships(data, sponsorships, deputes, verbose = TRUE)
  sponsors = parse_sponsors(data, sponsorships, deputes, verbose = TRUE)
  bills = parse_data(data, sponsorships, sponsors, verbose = TRUE)
  
  file = "data/bi_an.rda"
  save(bills, sponsorships, sponsors, file = file)
  
  msg("Saved:", file, nrow(bills), "bills", nrow(sponsors), "MPs", file_size(file))
    
}

#' Scrape National Assembly sponsors
#'
#' @param expand add group cosponsors to sponsorship list (off by default)
get_authors <- function(x, verbose = TRUE, root = "http://www.assemblee-nationale.fr", encoding = "latin1", expand = FALSE) {

  file = paste0("raw/an/dossiers/", paste0("depot", gsub("/", ".", x)))
  file = gsub("dossiers\\.", "", gsub(".asp", ".html", file))

  # fix a few UTF-8 files (disguised as latin1)
  if(x %in% c("/13/dossiers/collectivites_charges_rmi.asp", "/14/dossiers/reduction_activite_moniteurs_ski.asp"))
    encoding = "UTF-8"
    
  # "/13/dossiers/rapport_developpement_durable.asp", 
  #  "/13/dossiers/republique_decente.asp",
  #  "/13/dossiers/transparence_vie_publique.asp",

  if(verbose) cat(paste("Scraping:", file, "\n"))
  
  # build XPath query
  path = c("tribun", "senfic", "senateur", "groupe")
  path = paste0("contains(@href, '", path, "')")
  path = paste0("[", paste0(path, collapse = " or "), "]")
  
  # fetch dossier page
  
  # you could also parse the online version with paste0(root, x)
  html = try(htmlParse(file, encoding = "UTF-8"), silent = TRUE)
  if("try-error" %in% class(html)) {
    cat(paste("Error: scraper failed to fetch authors at", x, "\n"))
    
    authors = cbind(name = NA, url = NA)
    sponsors = NULL
    
  }
  else {
    
    # should always return a positive length vector
    text = readLines(file, encoding = encoding, warn = FALSE)
    text = text[ grepl("Proposition de", text) & grepl("/tribun/fiches_id|/senateur|/senfic", text) ]

    # if not, let it be and continue to cosponsors
    if(!length(text)) {

      cat(paste("Warning: no authors found at", x, "(no link)", "\n"))
      authors = cbind(name = NA, url = NA)

    }
    else {

      # encoding overriden if provided in page
      text = htmlParse(text, asText = TRUE, encoding = encoding)
      authors = sapply(xpathApply(text, paste0("//div/a", path)), xmlValue)
      
      # required for later ASCII conversion
      authors = as.character(authors)
      Encoding(authors) = "UTF-8"
      
      links = unlist(xpathApply(text, paste0("//div/a", path, "/@href")))
      authors = cbind(name = authors, url = paste0(root, links))
      
    }
    
    # catch group cosponsors from the popup link
    sponsors = unlist(xpathApply(html, "//div/a[contains(@href, 'cosignataires')]/@href"))

    if(is.null(sponsors)) {
      
      # declare no cosponsors
      sponsors = NULL

    } else {

      sponsors = gsub("^javascript:ouvre_popup\\('|'\\)", "", sponsors)
      sponsors = paste0(gsub("(.*)(/dossiers/)(.*)", "\\1\\2", x), sponsors)
      
      # parse one or more cosponsorship links
      sponsors = rbind.fill.matrix(lapply(sponsors, get_cosignataires, root = root, path = path))

    }
    
  }

  authors = data.frame(status = "author", authors, stringsAsFactors = FALSE)
  
  if(!is.null(sponsors))
    authors = rbind(authors, data.frame(status = "cosponsor", sponsors, stringsAsFactors = FALSE))

  # final sponsors listing
  authors = data.frame(uid = x, authors)
  
  # convert group sponsors
  groups = which(grepl("groupe(App)?", authors$url))
  if(expand & length(groups) > 0) {
    
    # get cosponsor lists
    group_cosponsors = lapply(authors$url[ groups ], get_cosignataires, root = NULL, path = path)    
    cosponsors = NULL
    
    # collate the data
    for(i in length(group_cosponsors)) {

      # get the cosponsors
      group = as.data.frame(group_cosponsors[[ i ]])
      
      # add to list if non-null
      if(nrow(group) > 0)
        cosponsors = rbind(cosponsors, data.frame(uid = x, status = authors$status[ groups ][i], group))

    }

    if(!is.null(cosponsors)) {
      
      # subset cosponsors to non-authors
      cosponsors$url = paste0(root, cosponsors$url)
      cosponsors = cosponsors[ !cosponsors$url%in% authors$url, ]
    
    }

    # subset authors to non-groups and merge
    authors = authors[ -groups, ]
    authors = rbind(authors, cosponsors)

  }
  else if(length(groups) > 0) {
    
    msg("Dropping:", length(groups), "group sponsorships")
    authors = authors[ -groups, ]
    
  }

  # clean up sponsor names (uses ASCII conversion)
  authors$name = str_trim(gsub("^M |^MME |^LES MEMBRES |DEPUTE E S ", "", clean_names(authors$name)))

  # clean up links to senators
  authors$url = gsub("^(.*)/(senfic|senateur)", "http://www.senat.fr/senateur", authors$url)

  return(authors)

}

#' Scrape National Assembly cosponsors
#'
get_cosignataires <- function(x, root, path) {
  
  # get links
  html = try(htmlParse(paste0(root, x), encoding = "latin1"), silent = TRUE)
  if("try-error" %in% class(html)) {
    
    cat(paste("Error: scraper failed to fetch cosponsors at", x, "\n"))
    sponsors = cbind(name = NA, url = NA)
    
  }
  else {
    
    links = xpathSApply(html, paste0("//a", path, "/@href"))
    
    # get names
    html = xpathSApply(html, paste0("//a", path))
    names = sapply(html, xmlValue)
    
    # required for later ASCII conversion
    names = as.character(names)
    Encoding(names) = "UTF-8"
    
    sponsors = cbind(name = names, url = paste0(root, links))
    
    # flag empty popups
    if(ncol(sponsors) < 2) {
      
      cat(paste("Warning: no cosponsors found at", x, "(empty list)", "\n"))
      sponsors = cbind(name = NA, url = NA)
      
    }
    
  }
  
  return(sponsors)
  
}

# have a nice day
