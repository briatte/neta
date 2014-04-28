#' Query Ameli database for Senate amendments through PostgreSQL
#' 
get_ameli <- function(file = "data/am_se.rda", sql, 
                      sessions = 11:14, verbose = TRUE) {
  
  if(!file.exists(file)) {
  
    # get senators
    sponsors = get_sql("select entid, nomuse, prenomuse from sen_ameli", sql) # grpid, qua
    names(sponsors)[1:3] = c("uid", "nom_de_famille", "prenom") # "groupe_sigle", "sexe"
    # sponsors$qua = ifelse(sponsors$qua == "M.", "H", "F")
  
    # senator name
    sponsors$nom = clean_names(paste(sponsors$prenom, sponsors$nom_de_famille))
  
    # check fullname-uid matches are unique
    stopifnot(n_distinct(sponsors$nom) == nrow(sponsors))
  
    # get sponsorships list
  
    sponsorships = get_sql("select amdid, senid, rng from amdsen", sql)
    sponsorships$rng = ifelse(sponsorships$rng == 0, "author", "cosponsor")
  
    # rename columns
    names(sponsorships) = c("uid", "name", "status")
    sponsorships = arrange(sponsorships, uid, status)
  
    # match full names
    rownames(sponsors) = sponsors$uid
    sponsorships$name = clean_names(sponsors[ as.character(sponsorships$name), "nom" ])
  
    # add demographics to senator data
    senateurs = subset(read.csv("raw/se/senateurs.txt", stringsAsFactors = FALSE), legislature %in% sessions)

    # merge
    sponsors = merge(sponsors[, c("nom", "uid") ], senateurs, by = "nom", all = TRUE)
    
    if(verbose)
      msg("Missing:", nrow(subset(sponsors, is.na(legislature))), "senator details", 
          round(100 * nrow(subset(sponsors, is.na(legislature))) / n_distinct(sponsors$nom), 2), 
          "% of total")
  
    # get amendments
    amendments = get_sql("select id, nomentid, txtid, num, ord, typ, datdep, sorid from amd", sql)
  
    # add outcome
  
    q = get_sql("select * from sor", sql)
    sort = q$lib
    names(sort) = as.character(q$id)
  
    amendments$sorid = clean_sort(sort[ as.character(amendments$sorid) ])
  
    # get dossiers (bills)
    dossiers = get_sql("select id, sesinsid, num, int, doslegsignet from txt_ameli", sql) # sesdepid
  
    # add session
  
    q = get_sql("select id, lil from ses", sql)
    session = q$lil
    names(session) = as.character(q$id)
  
    dossiers$sesinsid = session[ as.character(dossiers$sesinsid) ]
    # dossiers$sesdepid = session[ as.character(dossiers$sesdepid) ]
    names(dossiers)[ names(dossiers) == "id" ] = "txtid"
    names(dossiers)[ names(dossiers) == "num" ] = "texte"
  
    ## merge bills and amendments
  
    amendments = merge(dossiers, amendments, by = "txtid")
  
    names(amendments)[ names(amendments) == "typ" ] = "type"
    type = c("A" = "amendement", "S" = "sous-amendement", "M" = "motion")
    amendments$type = type[ amendments$type ]
  
    names(amendments)[ names(amendments) == "ord" ] = "place"
    names(amendments)[ names(amendments) == "datdep" ] = "date"
    names(amendments)[ names(amendments) == "sorid" ] = "sort"
    # names(amendments)[ names(amendments) == "sesdepid" ] = "annee_depot"
    names(amendments)[ names(amendments) == "sesinsid" ] = "annee_inscr"
    names(amendments)[ names(amendments) == "doslegsignet" ] = "url"
    names(amendments)[ names(amendments) == "int" ] = "titre"
    names(amendments)[ names(amendments) == "num" ] = "no"
    names(amendments)[ names(amendments) == "id" ] = "uid"
    names(amendments)[ names(amendments) == "nomentid" ] = "government"
    names(amendments)[ names(amendments) == "txtid" ] = "dossier"
  
    # legislatures (endpoints at end of legislative election year)
    amendments$date = ymd(substr(amendments$date, 1, 10))
    amendments$legislature = parse_legislature(amendments$date, subset = sessions)
    amendments = filter(amendments, legislature %in% sessions)

    # timestamps
    amendments$date = as.Date(amendments$date)
  
    amendments = parse_time(amendments)
    amendments$organe = ifelse(grepl("^COM-", amendments$no), "Toutes commissions", "Séance publique")
  
    # code fallen amendments (no sponsorship)
    amendments$government[ amendments$government == 0 ] = NA   # retirés avant séance
    amendments$sort[ is.na(amendments$government) ] = "Retiré" # impute outcome
  
    # code government sponsorship
    gov = get_sql("select entid from gvt", sql)$entid
    amendments$government[ !is.na(amendments$government) ] = amendments$government[ 
      !is.na(amendments$government) ] %in% c(5, gov)
  
    # code party sponsorship (unused)
  
    # q = get_sql("select entid, cod from grppol_ameli", sql)
    # party = q$cod
    # names(party) = as.character(q$entid)
    # 
    # groups = amendments$auteurs %in% names(party)
    # amendments$auteurs[ groups ] = party[ amendments$auteurs[ groups ] ]
  
    # code committee sponsorship (unused)
  
    # q = get_sql("select entid, cod from com_ameli", sql)
    # committee = q$cod
    # names(committee) = as.character(q$entid)
  
    # groups = amendments$auteurs %in% names(committee)
    # amendments$auteurs[ groups ] = committee[ amendments$auteurs[ groups ] ]
  
    # uid
    u = amendments$uid
    amendments = group_by(amendments, legislature)
    amendments = mutate(amendments,
                        uid = paste0("SA",
                                     sprintf("%02d", legislature),
                                     sprintf("%08d", 1:length(uid))
                                     ))
    uids = amendments$uid
    names(uids) = u
    sponsorships$uid = as.vector(uids[ sponsorships$uid ])
  
    # parse for network use
  
    sponsorships = parse_sponsorships(amendments, sponsorships, sponsors, verbose = TRUE)
    sponsors = parse_sponsors(amendments, sponsorships, sponsors, verbose = TRUE)
    sponsors = cbind(uid = sponsors$uid_senat, sponsors[, !grepl("uid", names(sponsors)) ])
    
    amendments = parse_data(amendments, sponsorships, sponsors, verbose = TRUE)
  
    save(amendments, sponsorships, sponsors, file = file)
    
    msg(nrow(amendments), "amendments and",
        nrow(sponsors), "senators saved to", file, file_size(file))
    
  }
  
}


#' Draft benchmark code for Ameli
#'
#' Tries to rebuild Ameli by scraping its online table extracts.
#' @return a table that contains roughly 80% of Ameli data by a few estimates
get_ameli2 <- function(senat = "http://www.senat.fr/", years = 2000:2013) {

  # get links to dossiers
  links = lapply(years, function(year, verbose = TRUE) {
    
    # depots available for years 1977-2013
    # amendments available for years 2000-2013
    stopifnot(year %in% 2000:2013)
    if(verbose) msg("Scraping: year", year)
    
    # pause briefly in case the vectorization proceeds too quickly
    # Sys.sleep(1)
    file = paste0("raw/se/indexes/depots.", year, ".html")
    if(!file.exists(file)) download(paste0(senat, "dossiers-legislatifs/depots/depots-", year, ".html"), 
                                    file, quiet = TRUE, mode = "wb")
    html = try(htmlParse(file, encoding = "UTF-8"))
    
    if(!"try-error" %in% class(html)) {
      
      urls = xpathSApply(html, "//div[@id='wysiwyg']/li/span/a/@href")
      urls = urls[ grepl("/(pjl|ppl)", urls) ]
      urls = gsub("/dossierleg/", "", urls)
      
      files = sum(!file.exists(paste0("raw/se/dossiers/depot.", unique(urls))))
      if(files > 0 | verbose) msg("Downloading:", files, "dossiers out of", n_distinct(urls))
      
      # links to amendments
      urls = lapply(unique(urls), function(x, verbose = FALSE) {
        
        link = paste0("http://www.senat.fr/dossier-legislatif/", x)
        file = paste0("raw/se/dossiers/depot.", x)
        if(!file.exists(file)) download(link, file, quiet = TRUE, mode = "wb")
        
        html = try(htmlParse(file, encoding = "UTF-8"))
        if(!"try-error" %in% class(html)) {
          
          urls = xpathSApply(html, "//a[contains(@href, 'amendements')]/@href")
          urls = sapply(urls, gsub, pattern = "^/amendements", replacement = "http://www.senat.fr/amendements")
          urls = urls [ grepl("^http://", urls) ]
          
          # links to CSV files
          csvs = try(lapply(urls, htmlParse, encoding = "UTF-8"))
          if(!"try-error" %in% class(csvs)) {
            csvs = lapply(csvs, function(x) xpathSApply(x, "//a[contains(@href, '.csv')]/@href"))
          }
          else {
            warning(paste("Failed to scrape files of dossier", x))
            urls = NULL
          }
          
        }
        else {
          warning(paste("Failed to scrape index of dossier", x))
          urls = NULL
        }
        if(length(urls) > 0) {
          if(verbose) msg(x, length(csvs), "files")
          return(data.frame(texte = x, link = paste0(gsub("accueil.html", "", urls), csvs)))
        }
        else
          return(data.frame())
      })
      urls = rbind.fill(urls)
      
      links = unique(urls$link)
      names(links) = paste0("raw/se/amendements/", gsub("(.*)jeu_complet_", "", links))
      
      files = names(links)
      files = files [ !file.exists(files) & grepl(".csv$", links) ]
      
      if(length(files) > 0 | verbose)
        msg("Downloading:", length(files), "files out of", length(links))
      
      for(i in files) {
        
        # pause briefly in case the loop proceeds too quickly
        # Sys.sleep(1)
        file = try(download(links[i], i, quiet = TRUE, mode = "wb"))
        if("try-error" %in% class(file)) warning(file)
        
      }
      
      return(data.frame(year, urls))
    }
    else {
      warning(paste("Failed to scrape year", year))
      return(NULL)
    }
    
  })
  links = rbind.fill(links)
  
  # special committees
  commission = paste0("raw/se/amendements/", dir("raw/se/amendements", pattern = "commission(.*).csv"))
  commission = lapply(commission, read.csv, skip = 1, sep = "\t", encoding = "latin1", stringsAsFactors = FALSE)
  commission = rbind.fill(commission)
  commission = commission[, 1:12]
  
  # open sessions
  seance = paste0("raw/se/amendements/", dir("raw/se/amendements", pattern = "^[0-9](.*).csv"))
  seance = lapply(seance, function(x) {
    file = try(which(grepl("^Nature", readLines(x, warn = FALSE, encoding = "UTF-8"))) - 1, silent = TRUE)
    if("try-error" %in% class(file)) {
      return(data.frame())
    }
    else {
      file = try(read.csv(x, encoding = "latin1", skip = file, sep = "\t", na.strings = "", stringsAsFactors = FALSE), silent = TRUE)
      if("try-error" %in% class(file)) file = data.frame()
      return(file)
    }
  })
  seance = rbind.fill(seance)
  seance = seance[, 1:12]
  
  # benchmark against official data
  # alternately: nrow(dplyr::tbl(src_postgres("ameli"), "amd"))
  
  size = nrow(fetchQuery("select id from amd", verbose = FALSE))
  
  msg("Scraped:", nrow(seance), "plenary debate amendments")
  msg("Scraped:", nrow(commission), "committee amendments")
  msg("Size:", round(100 * (nrow(seance) + nrow(commission)) / size, 2),
      "% of current Ameli row count")
  
  save(commission, seance, file = "data/ameli.rda")
  
}

#' Query Dosleg database for Senate bills through PostgreSQL
#' 
get_dosleg <- function(file = "data/bi_se.rda", sql,
                       sessions = 8:14, verbose = TRUE) {
  
  if(!file.exists(file)) {
    
    senateurs = subset(read.csv("raw/se/senateurs.txt", stringsAsFactors = FALSE), legislature %in% sessions)

    # URL match variable
    senateurs$autmat = toupper(gsub("(\\D)+(.*).html", "\\2", senateurs$url))
    
    # sponsor UID variable
    sen = get_sql("select autcod, autfct, nomuse, prenom, autmat from auteur", sqlb)
    sen$nom = clean_names(paste(sen$prenom, sen$nomuse))
    
    # process problematic names
    sen$nom[sen$nom == "LOUIS FERDINAND ROCCA SERRA" ] = "FERDINAND ROCCA SERRA LOUIS"
    sen$nom[sen$nom == "BERNARD MICHEL HUGO" ] = "MICHEL HUGO BERNARD"
    sen$nom[sen$nom == "SOSEFO MAKAPE PAPILIO" ] = "MAKAPE PAPILIO SOSEFO"
    
    sen$autmat[sen$nom == "JACKY DARNE"] = "nnnn" # not a senator
    sen$autmat = toupper(sen$autmat)
    
    match = with(sen, nom %in% unique(senateurs$nom))
    group = with(sen, is.na(prenom) | nchar(prenom) < 3 | prenom == "groupe")
    
    missing = sen$nom[ !match & sen$autmat %in% unique(senateurs$autmat) ]
    if(length(missing))
      warning(paste("Missing:", head(missing)))
    
    sen = sen[ !group & match, ]
    
    sponsors = merge(senateurs, sen[, c("autcod", "autmat", "autfct") ],
                     by = "autmat", all.x = TRUE)
    
    nul = is.na(sponsors$autcod)
    if(sum(nul)) {

      # unrecognized authors
      dnk = grepl("s(é|e)nateur", sponsors$autfct[ nul ], ignore.case = TRUE)

      # warning if author status is senator
      if(any(dnk))
        msg("Dropping:", n_distinct(sponsors$autfct[ nul ][ dnk ]),
            "senators (no sponsorships)")

      msg("Dropping:", n_distinct(sponsors$nom[ nul ][ !dnk ]),
          "sponsors (not senators)")

      sponsors = sponsors[ !is.na(sponsors$autcod), ]

    }

    sponsorships = get_sql("select texcod, autcod, ecrnumtri from ecr where typedoc = 'T'", sqlb)
    
    sponsorships = merge(sponsorships[, c("texcod", "autcod", "ecrnumtri") ],
                         unique(sponsors[, c("autcod", "autfct", "nom", "party", "url") ]),
                         by = "autcod", all.x = TRUE)
    sponsors$autcod = NULL        

    nul = is.na(sponsorships$nom) | is.na(sponsorships$autcod)
    if(sum(nul)) {

      # unrecognized authors
      dnk = grepl("s(é|e)nateur", sponsorships$autfct[ nul ], ignore.case = TRUE)

      # warning if author status is senator
      if(any(dnk))
        msg("Dropping:", n_distinct(sponsorships$autcod[ nul ][ dnk ]),
            "unmatched senator(s)")

      msg("Dropping:", n_distinct(sponsorships$autcod[ nul ][ !dnk ]),
          "umatched sponsor(s)")

      warning(paste("Dropping:", nrow(sponsorships[ nul, ]),
                    "umatched sponsorship(s)",
                    round(100 * nrow(sponsorships[ nul, ]) / 
                      nrow(sponsorships), 2),
                    "% of total"))

      sponsorships = sponsorships[ !nul, ]

    }
    
    # sample only bills where a senator is first author
    sample = unique(with(sponsorships, texcod[ ecrnumtri < 2 ]))
    sample = sponsorships$texcod %in% sample
    sample = sample & !any(with(sponsorships, is.na(nom), is.na(ecrnumtri)))

    # out-of-sample
    if(any(!sample)) {

      msg("Dropping:", nrow(sponsorships[ !sample, ]),
          "bills (not by a senator)")

      sponsorships = sponsorships[ sample, ]

    }

    sponsorships = arrange(sponsorships, texcod, ecrnumtri)
    sponsorships$status = ifelse(sponsorships$ecrnumtri == 1, "author", "cosponsor")
    names(sponsorships)[ names(sponsorships) == "texcod" ] = "uid"
    names(sponsorships)[ names(sponsorships) == "nom" ] = "name"
    
    # select Senator bills
    bills = get_sql("select texcod, sesann, txtoritxtdat, texnum, texurl from texte where sesann > 1985 and typtxtcod = '1'", sqlb)

    bills = filter(bills, texcod %in% unique(sponsorships$uid))

    msg("Parsing:", n_distinct(bills$texcod), "bills")

    names(bills)[ names(bills) == "texnum" ] = "texte"
    names(bills)[ names(bills) == "texcod" ] = "uid"
    names(bills)[ names(bills) == "texurl" ] = "url"

    # identifiers
    bills$url = str_trim(bills$url)
    bills$government = 0
    
    # missing variables (in other dosleg databases)
    bills$titre = NA
    bills$sort = NA
    
    # time of introduction
    bills$date = as.Date(bills$txtoritxtdat)

    # legislature number (based on examination date)
    bills$legislature = parse_legislature(ymd(bills$txtoritxtdat), subset = sessions)

    # parse timestamps
    bills = parse_time(bills)
    
    # titles and outcomes
    # laws = get_sql("select loicod, typloicod, etaloicod, signet, date_loi, loititjo, loitit from loi", sqlb)
    
    # uid
    bills = group_by(bills, legislature)
    bills = mutate(bills, old = uid,
                   uid = paste0("SB",
                                sprintf("%02d", as.numeric(legislature)),
                                sprintf("%08d", uid)
                                ))
    uids = bills$uid
    names(uids) = as.character(bills$old)
    sponsorships$uid = as.vector(uids[ as.character(sponsorships$uid) ])
    
    ## parse for network use
    
    sponsorships = parse_sponsorships(bills, sponsorships[, c("uid", "name", "status") ], sponsors, verbose = TRUE) # sponsorships

    names(sponsors)[ names(sponsors) == "autmat" ] = "uid_senat"
    sponsors = unique(sponsors[, c("legislature", "uid_senat", "nom", "nom_de_famille", "mandat", "sexe", "annee_naissance", "party", "nom_circo", "lon", "lat", "nb_mandats", "reelected", "url") ])
    
    sponsors = parse_sponsors(bills, sponsorships, sponsors, verbose = TRUE)
    sponsors = cbind(uid = sponsors$uid_senat, sponsors[, !grepl("uid", names(sponsors)) ])
    
    bills = parse_data(bills, sponsorships, sponsors, verbose = TRUE)
    
    save(bills, sponsorships, sponsors, file = file)
    
    msg("Saved:", file, nrow(bills), "bills", nrow(sponsors), "MPs", file_size(file))
    
  }
}

# have a nice day
