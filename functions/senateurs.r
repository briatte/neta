#' Scrape senator details
#' 
#' @seealso NosSenateurs.fr: \url{http://cpc.regardscitoyens.org/trac/wiki/API}
get_senat <- function(x = "senateurs", verbose = FALSE,
                      root = "http://www.senat.fr") {
  
  file = paste0("raw/se/indexes/", x, ".html")
  
  if(!file.exists(file))
    try_dl(paste0(root,
                  ifelse(grepl("anciens", x),
                         paste0(x, "-5eme-republique"),
                         x),
                  "/senatl.html"), file)
  
  html = htmlParse(file, encoding = "UTF-8")
  urls = unique(as.vector(xpathSApply(html, "//a[contains(@href,'/senateur/')]/@href")))
  
  if(verbose)
    msg("Parsing:", length(urls), x)

  ancien = grepl("anciens", x)
  senateurs = lapply(urls, function(x) {
    
    link = paste0(root, x)
    file = paste0("raw/se", x)
    
    if(!file.exists(file))
      download(link, file, mode = "wb", quiet = TRUE)
    
    html = try(htmlParse(file, encoding = "UTF-8"))
    
    if("try-error" %in% class(html)) {
      
      warning(paste("Could not find senator details at", x))
      return(NULL)
      
    }
    
    etat_civil = sapply(xpathSApply(html, "//dd"), xmlValue)
    etat_civil = etat_civil[ grepl("Né(e)? le", etat_civil) ]
    
    annee_naissance = gsub("(\\d{1,})(\\D+)(\\d{4})(.*)", "\\1\\2\\3", etat_civil)
    annee_naissance = suppressWarnings(year(clean_date(annee_naissance)))
    
    sexe = ifelse(grepl("Née le", etat_civil), "F", "H")
    
    nom = gsub("(Anciens sénateurs Vème République : )|( - Sénat)", "", 
               sapply(xpathSApply(html, "//title"), xmlValue))
    
    # fix missing dash
    nom = gsub("Jean Louis", "Jean-Louis", nom)
    
    nom_de_famille = unlist(strsplit(nom, " "))
    nom_de_famille = paste0(nom_de_famille[ -length(nom_de_famille) ], collapse = " ")
    prenom = rev(unlist(strsplit(nom, " ")))[1]
    
    # reorder name
    nom = paste(prenom, nom_de_famille)
    
    text = sapply(xpathSApply(html, "//ul[@class='list-type-03']/li"), xmlValue)
    
    mandat_debut = gsub("(\\D+) le (.*)", "\\2", text [ grepl("Elu[e]? le|Sénat(eur|rice) le", text) ])
    if(length(mandat_debut) < 1) mandat_debut = NA
    mandat_debut = mandat_debut[1] # first is earliest start of mandate
    mandat_debut = clean_date(gsub("(\\d{1,})(\\D+)(\\d{4})(.*)", "\\1\\2\\3", mandat_debut))
    
    mandat_fin = gsub("(\\D+) le (.*)", "\\2", text [ grepl("(Démission|Fin de mandat) le", text) ])
    
    if(length(mandat_fin) < 1)
      mandat_fin = NA
    
    mandat_fin = mandat_fin[1] # should always be unique
    
    if(ancien)
      mandat_fin = clean_date(gsub("(\\d{1,})(\\D+)(\\d{4})(.*)", "\\1\\2\\3", mandat_fin))
    else
      mandat_fin = Sys.Date()
    
    mandats = text[ grep("Réélu(e)?\\s+le", text) ]
    
    if(length(na.omit(mandats)) > 0)
      mandats = c(mandat_debut, clean_date(gsub("(\\D+)(\\d{1,})(\\D+)(\\d{4})(.*)",
                                                "\\2\\3\\4", mandats)))
    else
      mandats = mandat_debut
    
    nb_mandats = length(mandats)
    
    l = c(1958, 1962, 1967, 1968, 1973, 1978, 1981, 1986, 1988, 1993, 1997, 2002, 2007, 2012, 2017)
    l = as.Date(parse_date_time(paste0("01 12 ", l), "%d %m %y"))
    
    if(!is.na(mandat_debut) & !is.na(mandat_fin)) {
      
      m = max(which(mandat_debut > l)):max(which(mandat_fin > l))
      legislature = l[m]
      mandat = l[m]
      mandat = rbind.fill(lapply(mandat, function(x) {
        
        y = which(mandats < x)
        if(length(y) > 0)
          y = max(mandats[ y ])
        else
          y = min(mandats)
        z = which(mandats == y)
        return(data.frame(mandat = as.character(y), nb_mandats = z, stringsAsFactors = FALSE))
        
      }))
      
      nb_mandats = mandat[, 2]
      mandat = mandat[, 1]
      legislature = sapply(legislature, function(x) { which(l == x) })
      
    } else {
      
      legislature = mandat = NA
      
    }
    
    nom_circo = sapply(xpathSApply(html, "//h2[@class='subtitle-02']"), xmlValue)
    nom_circo = nom_circo[ grepl(ifelse(ancien, "Ancien sénat", "Sénat"), nom_circo) ]
    nom_circo = gsub("(Ancien )?(S|s)énat(eur|rice)( de | de l'| d'| des | de la | du | représentant les )", 
                     "", nom_circo)
    nom_circo = str_trim(gsub("\\s+", " ", nom_circo))
    
    if(ancien) {
      
      text = gsub("\\n", " ", text)
      groupe = text [ grepl("(d|a)u\\s+(G|g)roupe", text) ]
      
    } else {
      
      groupe = xpathSApply(html, "//ul[@class='list-type-01']/li")
      groupe = sapply(groupe, xmlValue)
      groupe = gsub("\\n", " ", groupe)
      groupe = groupe[ grepl("(d|a)u\\s+(G|g)roupe", groupe) ]
      
    }
    if(length(groupe) < 1) groupe = NA
    
    senateur = cbind(url = paste0(root, x),
                     nom, 
                     nom_de_famille, 
                     prenom,
                     legislature = as.character(legislature),
                     mandat, 
                     nb_mandats, 
                     groupe, 
                     nom_circo, 
                     annee_naissance = as.character(annee_naissance), 
                     sexe, 
                     mandat_debut = as.character(mandat_debut), 
                     mandat_fin = as.character(mandat_fin))
    senateur = str_trim(gsub("\\\n", "", senateur))
    
    return(senateur)
    
  })
  senateurs = rbind.fill.matrix(senateurs[ !sapply(senateurs, is.null) ])
  
  return(as.data.frame(senateurs, stringsAsFactors = FALSE))
  
}

get_senateurs <- function(sessions = 1:14, verbose = TRUE) {
  
  data = "raw/se/senateurs.txt"
  if(!file.exists(data)) {
    
    # get senators
    senateurs = rbind(get_senat("anciens-senateurs"),
                      get_senat("senateurs"))
    senateurs = filter(senateurs, legislature %in% sessions)
    
    if(verbose)
      msg("Parsing:", n_distinct(senateurs$nom), "senators")

    
    senateurs$legislature = as.numeric(senateurs$legislature)
    # clean names
    senateurs$nom = clean_names(senateurs$nom)
    senateurs$nom_de_famille = clean_names(senateurs$nom_de_famille)
    
    # fix homonym
    senateurs[ with(senateurs, grepl("JEAN BOYER", nom) & nom_circo == "Isère"), "nom"] = "JEAN BOYER ISERE"

    # fix constituency
    senateurs[ with(senateurs, grepl("JEAN PIERRE BEL", nom) & nom_circo != "Ariège Midi-Pyrénées"), "nom_circo"] = "Ariège Midi-Pyrénées"

    d = summarise(group_by(senateurs, nom), n = n_distinct(url))
    u = d$n > 1
    if(sum(u)) {
      warning(paste(sum(u), "homonyms"))
      if(sum(u)) print(senateurs[ senateurs$nom %in% d$nom[ u ], ])
    }
    
    # assign cross-legislature parliamentary groups
    # source: groups from http://senat.fr/anciens-senateurs-5eme-republique/
    y = rep(NA, nrow(senateurs))
    y[ grepl("Rassemblement Démocratique et Européen", senateurs$groupe) ] = "RDE"
    y[ grepl("Rassemblement Démocratique et Social Européen", senateurs$groupe) ] = "RDSE"
    y[ grepl("Républicains et( des)? Indépendants", senateurs$groupe) ] = "RI"
    y[ grepl("Républicains Indépendants d'Action Sociale", senateurs$groupe) ] = "RIAS"
    y[ grepl("Union Centriste des Démocrates de Progrès", senateurs$groupe) ] = "UCDP"
    y[ grepl("Union Centriste et Républicaine", senateurs$groupe) ] = "UCR"
    y[ grepl("Union (C|c)entriste", senateurs$groupe) ] = "UC-UDF"
    y[ grepl("Union des Démocrates pour la République", senateurs$groupe) ] = "UDR"
    y[ grepl("Union des( )?Démocrates et Indépendants", senateurs$groupe) ] = "UDI"
    y[ grepl("Rassemblement pour la République", senateurs$groupe) ] = "RPR"
    y[ grepl("Centre Républicain d'Action Rurale et Sociale", senateurs$groupe) ] = "CRARS"
    y[ grepl("Union pour la Nouvelle République", senateurs$groupe) ] = "UNR"
    y[ grepl("Union pour un( )?Mouvement Populaire", senateurs$groupe) ] = "UMP"
    y[ grepl("Communiste", senateurs$groupe) ] = "C"
    y[ grepl("[C|c]ommuniste [R|r]épublicain et [C|c]itoyen", senateurs$groupe) ] = "CRC"
    y[ grepl("Centre National des Indépendants et Paysans", senateurs$groupe) ] = "CNIP"
    y[ grepl("Centre Démocratique", senateurs$groupe) ] = "CD"
    y[ grepl("Gauche Démocratique", senateurs$groupe) ] = "GD"
    y[ grepl("Républicain[s]? Populaire[s]?", senateurs$groupe) ] = "MRP"
    y[ grepl("écologiste", senateurs$groupe) ] = "ECOLO"
    y[ grepl("[S|s]ocialiste", senateurs$groupe) ] = "SOC" # + apparentés
    y[ grepl("Partide Gauche", senateurs$groupe) ] = "PG" # hors liste
    y[ is.na(y) ] = "SE" # residuals
    
    stopifnot(all(!is.na(y)))
    
    # party group
    colors = c(
      "ECOLO" = "#4DAF4A", # green
      "C"     = "#E41A1C", # red
      "PG"    = "#E41A1C", # red
      "CRC"   = "#E41A1C", # red
      "NI"    = "#999999", # grey
      "GD"    = "#FFFF33", # yellow (radicaux)
      "RDE"   = "#FFFF33", # yellow
      "RDSE"  = "#FFFF33", # yellow
      "SOC"   = "#F781BF", # pink
      "UC-UDF"= "#FF7F00", # orange (centristes)
      "UDI"   = "#FF7F00", # orange
      "CD"    = "#FF7F00", # orange
      "UCR"   = "#FF7F00", # orange
      "UCDP"  = "#FF7F00", # orange
      "CNIP"  = "#FF7F00", # orange (CNIP)
      "CRARS" = "#FF7F00", # orange (CNIP)
      "RI"    = "#FF7F00", # orange
      "RIAS"  = "#FF7F00", # orange
      "UNR"   = "#377EB8", # blue (Gaullists)
      "UDR"   = "#377EB8", # blue (Gaullists)
      "MRP"   = "#377EB8", # blue (Gaullists)
      "UMP"   = "#377EB8", # blue
      "RPR"   = "#377EB8", # blue
      "SE"    = "#999999") # grey
    
    # add as sponsor variable
    senateurs$party = parse_color(y, colors)
    
    # clean up extra content in constituencies
    senateurs$nom_circo = gsub("\\(|\\)|&nbsp|puis| du | de la | des |Ancien sénateur|indéterminé", 
                               "", senateurs$nom_circo)
    
    # geocode constituencies
    geo = "raw/se/geocodes.txt"
    
    if(!file.exists(geo))
      write.csv(parse_geo(senateurs$nom_circo), file = geo, row.names = FALSE)
    
    senateurs = merge(senateurs, read.csv(geo, stringsAsFactors = FALSE),
                      by = "nom_circo", all.x = TRUE)
    
    # reelection dummy
    senateurs = mutate(group_by(senateurs, nom),
                       reelected = as.numeric(nb_mandats < max(nb_mandats)))
    
    # last legislature
    senateurs$reelected[ senateurs$legislature == max(senateurs$legislature) ] = NA
    
    # UID
    j = unique(arrange(senateurs, mandat_debut, annee_naissance)$nom)
    senateurs$uid_senat = as.numeric(factor(senateurs$nom, levels = j))
    
    # mandate
    senateurs$mandat_debut = format(as.Date(senateurs$mandat_debut), "%d/%m/%Y")
    senateurs$mandat_fin = format(as.Date(senateurs$mandat_fin), "%d/%m/%Y")
    senateurs$mandat_fin[ senateurs$mandat_fin == format(Sys.Date(), "%d/%m/%Y") ] = "?"
    senateurs$mandat = paste(senateurs$mandat_debut, senateurs$mandat_fin, sep = " - ")
    
    j = c("legislature", "uid_senat", "nom", "nom_de_famille", "mandat", "sexe", "annee_naissance",
          "party", "nom_circo", "lon", "lat", "nb_mandats", "reelected", "url")  
    
    # save master sponsor dataset
    write.csv(arrange(senateurs[, j], nom, legislature),
              file = data, row.names = FALSE)
    
    if(verbose)
      return(msg("Saved:", data, n_distinct(senateurs$uid_senat), "sponsors",
                 nrow(senateurs), "mandates"))
    
  }
  else {
    
    if(verbose)
      return(msg("Saved:", data,
             nrow(read.csv(data)), "mandates", file_size(data)))
    
  }
  
}

# have a nice day
