#' Download and process National Assembly amendments
#'
#' Updated 2014-04-23 to new National Assembly amendments search engine:
#' ongoing legislature 14 scraped from paginated JSON results,
#' past legislatures 12:13 scraped from paginated HTML results.
#' @param sleep sleeping time for download loop, passed to \code{try_dl}
#' @param verbose adds tables of unmatched sponsorships to screen messages
get_amendments <- function(sessions = 12:14, verbose = TRUE, sleep = 0) {

  stopifnot(all(sessions %in% 12:14))

  for(session in sessions) {

    if(file.exists(paste("raw/an/indexes/amendements", session, "csv", sep = "."))) {
      
      msg("Downloading: legislature", session)

      if(session < 14) {
      
        ## scrape from Bootstrap HTML tables

        url = paste0("http://www2.assemblee-nationale.fr/recherche/archives_amendements?LEGISLATURE=", session)
        all = paste("raw/an/indexes/amendements", session, "html", sep = ".")
        if(!file.exists(all)) try_dl(url, all)

        all = htmlParse(all)
        all = sapply(xpathSApply(all, "//select[@id='field_numInit']/option"), xmlValue)
        all = na.omit(as.numeric(gsub("(^\\d+) (.*)", "\\1", all)))
      
        # fill data table on the fly
        data = data.frame()

        for(i in rev(all)) {
      
            # download landing page
            html = paste("raw/an/amendements/texte", session, i, 0, "html", sep = ".")
            if(!file.exists(html))
              try_dl(paste0("http://www2.assemblee-nationale.fr/recherche/archives_resultats_amendements?NUM_INIT=", i, "&LEGISLATURE=", session), html)
      
          # download other pages
          html = htmlParse(html)
          page = xpathSApply(html, "//div[@class='pagination-bootstrap pagination-right pagination-small']/ul/li/a/@href")
      
          if(length(page) > 1) {
      
            link = gsub("(.*)/\\(query\\)/(.*)", "\\2", page[1])
            page = gsub("(.*)/\\(offset\\)/(\\d+)/(.*)", "\\2", page)
            page = as.numeric(page)
      
            page = range(na.omit(page))
            page = seq(page[1], page[2], by = 10)
      
            file = paste("raw/an/amendements/texte", session, i, page, "html", sep = ".")
            file = page[ !file.exists(file) ]

            if(length(file))
              msg("Downloading: bill", i, max(page) / 10, "page(s)")

            for(j in file)
              try_dl(paste0("http://www2.assemblee-nationale.fr/recherche/archives_resultats_amendements/(offset)/", j, "/(query)/", link),
                     paste("raw/an/amendements/texte", session, i, j, "html", sep = "."))
                
          }
        
          # parse landing page and other pages if any
          tbls = dir("raw/an/amendements", paste("texte", session, i, "\\d+", "html", sep = "."))
          tbls = lapply(paste0("raw/an/amendements/", tbls), htmlParse)
          tbls = lapply(unlist(tbls), readHTMLTable, which = 1, stringsAsFactors = FALSE)
          data = rbind(data, data.frame(rbind.fill.matrix(tbls), stringsAsFactors = FALSE))
      
          # readHTMLTable expected to return this:
          # "//div[@class='clearfix']/div[@class='interieur-contenu-principal clearfix']/table"
      
        }
      
        names(data) = c("numInit", "numAmend", "designationArticle", "dateDepot","signataires","sort")

      
      } else {
      
        ## scrape from JSON queries
      
        all = readLines('http://www2.assemblee-nationale.fr/recherche/query_amendements?id=&typeRes=liste&rows=1', warn = FALSE)
        all = 1 + as.numeric(gsub("\\D", "", all[ grepl("nb_resultats", all)]))
      
        # fill data table on the fly
        data = matrix(nrow = 0, ncol = 12)
        colnames(data) = c("id", "numInit", "titreDossierLegislatif", "urlDossierLegislatif", "instance", "numAmend", "urlAmend", "designationArticle", "designationAlinea", "dateDepot", "signataires", "sort")
      
        i = 1
        while(i < all) {
      
          # hypothetically works if session > 14
          f = paste("raw/an/amendements/amendements", session, i, "json", sep = ".")
          u = paste0("http://www2.assemblee-nationale.fr/recherche/query_amendements?id=&typeRes=liste&rows=1000&start=", i)
      
          # download segments (last one systematically included)
          if(!file.exists(f) | i + 1000 > all) {
      
            msg("Downloading: page", i, all - i, "item(s) left")
            try_dl(u, f)
                    
          }
        
          f = readLines(f, warn = FALSE)
      
          # variable names
          f = fromJSON(f[ 1 + which(grepl("data_table", f)) ])
          f = lapply(strsplit(f, "\\|"), matrix, ncol = 12)
          f = rbind.fill.matrix(f)
        
          data = rbind(data, f)
          i = i + 1000
      
        }
      
      }

      
      msg("Saved:", nrow(data), "amendments")
      write.csv(cbind(legislature = session, data),
                paste0("raw/an/indexes/amendements", session, ".csv"), row.names = FALSE)
      
    }
        
  }

  # data
  data = paste0("raw/an/indexes/amendements", sessions, ".csv")
  data = lapply(data, read.csv, stringsAsFactors = FALSE)
  data = rbind.fill(data)

  # uid
  data = group_by(data, legislature)
  data = mutate(data, uid = paste0("AA",
                                   sprintf("%02d", legislature),
                                   sprintf("%08d", 1:length(id))
                                   )
                )

  # texte
  data$texte = gsub("\\D", "", data$numInit)

  # titre
  names(data)[ which(names(data) == "titreDossierLegislatif") ] = "titre"

  # url
  names(data)[ which(names(data) == "urlDossierLegislatif") ] = "url"

  # organe
  names(data)[ which(names(data) == "instance") ] = "organe"

  # no
  names(data)[ which(names(data) == "numAmend") ] = "no"

  # place
  names(data)[ which(names(data) == "designationArticle") ] = "place"

  # sort
  data$sort  = clean_sort(data$sort)   # amendment outomes

  # date
  data$date = clean_date(data$dateDepot)

  # ymd
  data = parse_time(data)

  # government amendments
  g = grepl("^(le )?gouvernement", data$signataires, ignore.case = TRUE)
  data$signataires[ g ] = "GOUVERNEMENT"

  # column mismatches
  p = grepl("alinea|article|chapitre", data$signataires, ignore.case = TRUE)
  data$signataires[ p ] = NA

  m = is.na(data$signataires)
  if(sum(m)) {
    warning(paste("Dropped", sum(m), "amendments (parsing issues)",
            round(100 * sum(m) / nrow(data), 2), "% of all data"))
    data = data[ !m, ]
  }

  a = read.table("raw/an/deputes.txt", sep = ",", 
                 header = TRUE, stringsAsFactors = FALSE)

  for(session in sessions) {
  
    amendments = subset(data, legislature == session)
    sponsors = subset(a, legislature == session)

    # commas
    s = gsub("(^|,|\\s)+(et|[M]{1,2}\\.?|Mme[s]?)\\s+", ",", amendments$signataires, ignore.case = TRUE)
    s = gsub("(^,)|(,$)", "", s) # trim start/end commas
    s = gsub("\\s+", " ", s)     # trim multiple whitespace
    s = gsub("(,\\s?)+", ",", s) # trim multiple commas
    s = gsub(" au nom de (.*)", "", s) # trim commission titles

    s = stringr::str_split(s, ",")
    s = lapply(s, matrix)
    s = mapply(cbind, amendments$uid, s)
    s = rbind.fill.matrix(s)

    s = s[ nchar(s[, 2]) > 2 & !grepl("\\d|rapporteur|commissaire|d(é|e)put(é|e)|membre|r(é|e)publicain|ind(é|e)pendant|pour avis|accidents|assurance|famille|familiales|pr(é|e)sident|sociales|recettes|(é|e)quilibre|co(m){1,}ission|apparent(é|e)|au no|environnement|territoire|adoption|accord du|radical|citoyen|m(é|e)dico|divers gauche|(é|e)ducation|progressiste", s[, 2], ignore.case = TRUE), ]

    # HTML (legislature 14)
    s[, 2] = gsub("(\\s+)?&apos;(\\s+)?", "", s[, 2])
    s[, 2] = gsub("&#231;", "c", s[, 2])
    s[, 2] = gsub("&#244;", "o", s[, 2])
    s[, 2] = gsub("&#(232|233);", "e", s[, 2])

    s[, 2] = str_trim(clean_names(s[, 2]))
    s[, 2] = gsub("^(M{1,2}|MME|ET)\\s+", "", s[, 2])
    s = s[ !s[, 2] %in% c("", "ET"), ]

    # particles
    s[, 2] = gsub("^(BORGN|BOUILLONNEC|FUR|GUEN|ROUX)$", "LE \\1", s[, 2])
    s[, 2] = gsub("^(RUGY|PANAFIEU|LA RAUDIERE)$", "DE \\1", s[, 2])
    s[, 2] = gsub("LE MOAL", "MOAL", s[, 2]) # in legislature 13 (?)

    # 12 typos
    s[, 2] = gsub("BEAUDOIN", "BEAUDOUIN", s[, 2])
    s[, 2] = gsub("DELAGNOLA", "DELL AGNOLA", s[, 2])
    s[, 2] = gsub("HOFFMANN RISPAL", "HOFFMAN RISPAL", s[, 2])
    s[, 2] = gsub("LE PETIT", "LEPETIT", s[, 2])
    s[, 2] = gsub("CLAYES", "CLAEYS", s[, 2])
    s[, 2] = gsub("J M GUEN", "JEAN MARIE GUEN", s[, 2])
    s[, 2] = gsub("GUIGUOU", "GUIGOU", s[, 2])

    # 12 homonyms
    s[, 2] = gsub("GUI(N)?CHARD\\s?KUNSTLER", "PAULETTE GUINCHARD", s[, 2])
    s[, 2] = gsub("PERRIN GAILLARD", "GENEVIEVE GAILLARD", s[, 2])
    s[, 2] = gsub("(DE\\s?)?COURSON", "DE COURSON", s[, 2])

    # 13 homonyms
    s[, 2] = gsub("MARTIN GERS|MARTIN PHILIPPE", "PHILIPPE MARTIN", s[, 2])
    s[, 2] = gsub("PHILIPE ARMAND MARTIN|MARTIN MARNE", "PHILIPPE ARMAND MARTIN", s[, 2])

    # 14 typos
    s[, 2] = gsub("MOREL LHUISSIER", "MOREL HUISSIER", s[, 2])
    s = s[ s[, 2] != "MEYER HABIB", ] # missing Sycomore profile for Meyer Habib

    # replace family names by full names
    u = duplicated(sponsors$nom_de_famille)
    u = subset(sponsors, !nom_de_famille %in% u)
    n = u$nom
    names(n) = u$nom_de_famille
    s[ s[, 2] %in% names(n), 2] = n[ s[ s[, 2] %in% names(n), 2] ]

    # subset amendments
    m = amendments$uid %in% unique(s[, 1])
    if(sum(!m)) {
      warning(paste("Dropped", sum(!m), "amendments (parsing issues)",
              round(100 * sum(!m) / nrow(data), 2), "% of all data"))
      amendments = amendments[ m, ]
    }

    # subset sponsorships
    m = s[, 2] %in% c("GOUVERNEMENT", unique(sponsors$nom))
    if(sum(!m)) {
      if(verbose)
        table(s[which(!m), 2])[ table(s[which(!m), 2]) > 2 ]
      
      warning(paste("Dropped", sum(!m), "sponsors (no matching author)",
              round(100 * sum(!m) / nrow(s), 2), "% of all data"))
      s = s[ m, ]

    }

    # finalize sponsorships
    s = data.frame(s, stringsAsFactors = FALSE)
    names(s) = c("uid", "name")
    s = mutate(group_by(s, uid),
               status = c("author", rep("cosponsor", length(uid) - 1)))

    msg("Parsed: legislature", session,
        nrow(amendments), "amendments", nrow(sponsors), "sponsors",
        nrow(ungroup(s)), "sponsorships")

    assign(paste0("amendments", session), amendments)
    assign(paste0("sponsors", session), sponsors)
    assign(paste0("sponsorships", session), s)

  }

  amendments = rbind.fill(mget(ls(pattern = "amendments[0-9]+")))
  amendments$government = as.numeric(amendments$signataires == "GOUVERNEMENT")

  sponsorships = rbind.fill(mget(ls(pattern = "sponsorships[0-9]+")))
  sponsors = rbind.fill(mget(ls(pattern = "sponsors[0-9]+")))

  # parse sponsorships
  sponsorships = parse_sponsorships(amendments, sponsorships, sponsors, verbose = TRUE)

  # parse sponsors
  sponsors = parse_sponsors(amendments, sponsorships, sponsors, verbose = TRUE)

  # parse amendments
  amendments = parse_data(amendments, sponsorships, sponsors, verbose = TRUE)

  # save final dataset
  data = "data/am_an.rda"
  save(amendments, sponsorships, sponsors, file = data)

  msg("Saved:", data, nrow(amendments), "amendments", nrow(sponsors), "sponsors", file_size(data))
  
}

# have a nice day
