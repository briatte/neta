#' Scrape MP details
#' 
#' @seealso NosDéputes.fr: \url{http://cpc.regardscitoyens.org/trac/wiki/API}
get_sycomore <- function(x, sample = FALSE, verbose = FALSE, 
                         root = "http://www.assemblee-nationale.fr/sycomore/") {
  
  # validate historical session (1789-today)
  stopifnot(x %in% 2:61)
  
  file = paste("raw/an/indexes/deputes", x - 47, "html", sep = ".")
  if(!file.exists(file))
    try_dl(paste0(root, "result.asp?radio_dept=tous_departements&regle_nom=contient&Nom=&departement=&choixdate=intervalle&D%C3%A9butMin=&FinMin=&Dateau=&legislature=", x, "&choixordre=chrono&Rechercher=Lancer+la+recherche"), file)
  
  sycomore = htmlParse(file, encoding = "UTF-8")
  sycomore = unlist(xpathSApply(sycomore, "//a[contains(@href, 'num_dept')]/@href"))
  sycomore = paste0(root, unique(sycomore))
  
  if(verbose)
    msg("Parsing: legislature", x - 47, length(sycomore), "MPs")
  
  # MP-level details
  sycomore = lapply(sycomore, function(i, session = x - 47) {
        
    file = paste0("raw/an/deputes/", gsub("\\D", "", i), ".html")

    if(!file.exists(file))
      download(i, file, mode = "wb", quiet = TRUE)
    
    html = try(htmlParse(file, encoding = "UTF-8"), silent = TRUE)
    if("try-error" %in% class(html)) {
      warning(paste("Parser error:", i))
      return(data.frame())
    }
    else {
      
      # full name and family names
      nom = xmlValue(xpathApply(html, "//h1[@class='deputy-headline-title']")[[1]])
      nom_de_famille = paste0(unlist(str_extract_all(nom, "[A-ZÉÀÙÊÎÔÈÙ]{2,}")), collapse = " ")
      
      # birth year and birth place
      naissance = xmlValue(xpathApply(html, "//dl[@class='deputy-attribute-list']/dd/ul/li")[[1]])
      annee_naissance = gsub("(.*) le (.*) à (.*)", "\\2", naissance)
      annee_naissance = year(parse_date_time(annee_naissance, "%d/%m/%y", quiet = TRUE))
      # lieu_naissance = str_trim(gsub("(.*) à (.*)", "\\2", naissance))
      sexe = NA
      sexe[ grepl("(N|n)é le", naissance) ] = "H"
      sexe[ grepl("(N|n)ée le", naissance) ] = "F"

      # constituency and party affil.
      mandat = xpathSApply(html, "//div[@id='assemblee']/*/ul/li/div[@class='article-content']/p[count(b) > 0]")
      mandat = gsub("(.*):\\s+(.*)", "\\2", sapply(mandat, xmlValue))
      
      if(!length(mandat)) {
        nom_circo = groupe_sigle = NA
      } else {
        nom_circo = str_trim(gsub("(.*) - (.*)", "\\1", mandat))
        groupe_sigle = str_trim(gsub("(.*) - (.*)", "\\2", mandat))
      }

      # vector of individual mandates
      mandat = xpathSApply(html, "//div[@id='assemblee']/*/ul/li/div[@class='article-content']/p/b")
      if(!length(mandat)) {
        mandat = NA
      } else {
        mandat = str_trim(sapply(mandat, xmlValue))
      }
      
      if(identical(c(mandat, groupe_sigle), c(NA, NA)))
        warning(paste("Could not find MP details at", i))
      
      # match url_an from other datasets
      url_an = xpathSApply(html, "//a[contains(@onclick, 'tribun')]/@onclick")
      url_an = as.vector(gsub("window.location=|'", "", url_an))
      url_an = ifelse(!length(url_an), NA, paste0("http://www.assemblee-nationale.fr", url_an))
      
      return(
        data.frame(
          legislature = session,
          num_dept = gsub("(.*)num_dept=(.*)", "\\2", i),
          nom,
          nom_de_famille,
          sexe,
          annee_naissance,
          nb_mandats = length(mandat),
          mandat,
          nom_circo,
          groupe_sigle,
          url_an,
          stringsAsFactors = FALSE)
      )
    }
  })
  
  return(rbind.fill(sycomore))
  
}

#' Parse MPs to final dataset
#'
#' Family names are shortened to their shortest form, without opening particles 
#' and maiden names. Homonyms and the like are fixed manually.
get_deputes <- function(sessions = 1:14, verbose = TRUE) {
  
  data = "raw/an/deputes.txt"
  if(!file.exists(data)) {
    
    # download Fifth Republic MPs
    deputes = unique(rbind.fill(lapply( x + 47, get_sycomore))) # 1:14 = 55:61
    
    # clean names
    deputes$nom = str_trim(clean_names(deputes$nom))
    deputes$nom_de_famille = str_trim(clean_names(deputes$nom_de_famille))
    
    # fix cedilla encoding issues
    deputes$nom_de_famille[ deputes$nom == "MICHEL FRANCAIX" ] = "FRANCAIX" 
    deputes$nom_de_famille[ deputes$nom == "PHILIPPE BOENNEC" ] = "BOENNEC" 
    
    # fix family names
    deputes$nom[ deputes$nom == "ODETTE EUGENIE GERMAINE DURIEZ NEE GAMELIN" ] = "ODETTE DURIEZ"
    deputes$nom_de_famille[ deputes$nom == "ODETTE DURIEZ" ] = "DURIEZ"
    deputes$nom[ deputes$nom == "CHRISTIANE TAUBIRA DELANNON" ] = "CHRISTIANE TAUBIRA"
    deputes$nom_de_famille[ deputes$nom == "CHRISTIANE TAUBIRA" ] = "TAUBIRA"
    deputes$nom[ deputes$nom == "MARTINE AURILLAC NEE ADRIAN" ] = "MARTINE AURILLAC"
    deputes$nom_de_famille[ deputes$nom == "MARTINE AURILLAC" ] = "AURILLAC"
    deputes$nom[ deputes$nom == "FRANCOISE BRANGET NEE MINELLO" ] = "FRANCOISE BRANGET"
    deputes$nom_de_famille[ deputes$nom == "FRANCOISE BRANGET" ] = "BRANGET"
    deputes$nom[ deputes$nom == "FRANCOISE VALLET NEE JOUANNE" ] = "FRANCOISE VALLET"
    deputes$nom_de_famille[ deputes$nom == "FRANCOISE VALLET" ] = "VALLET"
    deputes$nom[ deputes$nom == "BERNADETTE PAIX NEE ABRIBAT" ] = "BERNADETTE PAIX"
    deputes$nom_de_famille[ deputes$nom == "BERNADETTE PAIX" ] = "PAIX"
    deputes$nom[ deputes$nom == "MARCELLE MAURICETTE RAMONET NEE CHITRE" ] = "MARCELLE RAMONET"
    deputes$nom_de_famille[ deputes$nom == "MARCELLE RAMONET" ] = "RAMONET"
    deputes$nom[ deputes$nom == "COLETTE MARIE FRANCOISE MOAL NEE AMICEL" ] = "COLETTE MOAL"
    deputes$nom_de_famille[ deputes$nom == "COLETTE MOAL" ] = "MOAL"
    deputes$nom[ deputes$nom == "PAULETTE GUINCHARD KUNTZLER" ] = "PAULETTE GUINCHARD"
    deputes$nom_de_famille[ deputes$nom == "PAULETTE GUINCHARD" ] = "GUINCHARD"
    deputes$nom[ deputes$nom == "JOSIANE BOYCE NEE PONTIEUX" ] = "JOSIANE BOYCE"
    deputes$nom_de_famille[ deputes$nom == "JOSIANE BOYCE" ] = "BOYCE"

    # fix first names
    deputes$nom[ deputes$nom == "JACQUES MICHEL PIERRE CHABAN DELMAS" ] = "JACQUES CHABAN DELMAS"
    deputes$nom[ deputes$nom == "CHARLES PINETON CHAMBRUN" ] = "CHARLES CHAMBRUN"
    deputes$nom[ deputes$nom == "MICHEL PAUL PIERRE GHYSEL" ] = "MICHEL GHYSEL"
    deputes$nom[ deputes$nom == "JEAN NOEL LIPKOWSKI" ] = "JEAN LIPKOWSKI"
    deputes$nom[ deputes$nom == "JEAN PIERRE FERNAND SCHENARDI" ] = "JEAN PIERRE SCHENARDI"
    deputes$nom[ deputes$nom == "ROLAND LEON LOUIS DUMAS" ] = "ROLAND DUMAS"
    deputes$nom[ deputes$nom == "ANDRE MARIE ANTOINE DROITCOURT" ] = "ANDRE DROITCOURT"
    deputes$nom[ deputes$nom == "ALBERT LIKUVALU" ] = "APELETO ALBERT LIKUVALU" # longer
    deputes$nom[ deputes$nom == "MICHEL JEAN ELI ROSSI" ] = "MICHEL ROSSI"
    deputes$nom[ deputes$nom == "GUY GEORGES CAMILLE LEFRAND" ] = "GUY LEFRAND"
    deputes$nom[ deputes$nom == "PHILIPPE LUCIEN MARIE MORENVILLIER" ] = "PHILIPPE MORENVILLIER"
    deputes$nom[ deputes$nom == "RAYMOND JOANNES DURAND" ] = "RAYMOND DURAND"
    deputes$nom[ deputes$nom == "GERARD ANDRE MILLET" ] = "GERARD MILLET"
    deputes$nom[ deputes$nom == "PATRICE OLIVIER PAUL DEBRAY" ] = "PATRICE DEBRAY"
    deputes$nom[ deputes$nom == "ROBERT JEAN PIERRE MARIE DIAT" ] = "ROBERT DIAT"
    deputes$nom[ deputes$nom == "CLAUDE ANNE DARCIAUX" ] = "CLAUDE DARCIAUX"
    deputes$nom[ deputes$nom == "PHILIPPE CALIXTE GERARD EDMOND MARIETTE" ] = "PHILIPPE EDMOND MARIETTE"
    deputes$nom[ deputes$nom == "LAURENT EMILE MICHEL HENART" ] = "LAURENT HENART"
    deputes$nom[ deputes$nom == "PIERRE ADRIEN MENJUCQ" ] = "PIERRE MENJUCQ"
    deputes$nom[ deputes$nom == "JEAN PIERRE ANDRE MARCHE" ] = "JEAN PIERRE MARCHE"
    deputes$nom[ deputes$nom == "PIERRE CLAUDE LANFRANCA" ] = "CLAUDE LANFRANCA"
    deputes$nom[ deputes$nom == "AUGUSTE DAMASE LEGROS" ] = "AUGUSTE LEGROS"
    deputes$nom[ deputes$nom == "GEORGES ROBERT DELATRE" ] = "GEORGES DELATRE"
    deputes$nom[ deputes$nom == "GEORGES PAUL MARIE WAGNER" ] = "GEORGES PAUL WAGNER"
    deputes$nom[ deputes$nom == "HENRI CHARLES MICHEL" ] = "HENRI MICHEL"
    deputes$nom[ deputes$nom == "JACQUES ROBERT BAUMEL" ] = "JACQUES BAUMEL"
    deputes$nom[ deputes$nom == "ANDRE LOUIS RENE DELEHEDDE" ] = "ANDRE DELEHEDDE"
    deputes$nom[ deputes$nom == "ADRIEN JEAN LOUIS DURAND" ] = "ADRIEN DURAND"
    deputes$nom[ deputes$nom == "ELIE ARISTIDE MARTY" ] = "ELIE MARTY"
    deputes$nom[ deputes$nom == "PIERRE EMILE GUILLAIN BENOUVILLE" ] = "PIERRE BENOUVILLE"
    deputes$nom[ deputes$nom == "BERNARD CLAUDE SAVY" ] = "BERNARD SAVY"
    deputes$nom[ deputes$nom == "ALBERT ADOLPHE MARCEL BROCHARD" ] = "ALBERT BROCHARD"
    
    # fix homonyms
    deputes$nom[ with(deputes, nom == "JEAN MICHEL BOUCHERON" & nom_circo == "Charente") ] = "JEAN MICHEL BOUCHERON CHARENTE"
    deputes$nom[ with(deputes, nom == "JACQUES HOUSSIN" & annee_naissance == 1928) ] = "JACQUES HOUSSIN NORD PREMIER"
    deputes$nom[ with(deputes, nom == "PIERRE BERNARD" & nom_circo == "Tarn") ] = "PIERRE BERNARD TARN"
    deputes$nom[ with(deputes, nom == "JEAN BERNARD" & nom_circo == "Isère") ] = "JEAN BERNARD ISERE"
    
    # remove extra rows created from presidency dates
    deputes = subset(deputes, !grepl("Présidence", mandat))
    
    if(verbose)
      msg("Parsing:", n_distinct(deputes$nom), "MPs")
    
    d = summarise(group_by(deputes, nom), n = n_distinct(num_dept))
    u = d$n > 1
    if(sum(u)) {
      print(deputes[ deputes$nom %in% d$nom[ u ], ])
      stop(paste(sum(u), "homonyms detected"))
    }
    
    # subset to unique legislature mandates
    deputes = summarise(group_by(deputes, num_dept, mandat),
                        nom = unique(nom),
                        nom_de_famille = unique(nom_de_famille),
                        sexe = unique(sexe),
                        annee_naissance = unique(annee_naissance),
                        groupe_sigle = unique(groupe_sigle),
                        nb_mandats = unique(nb_mandats),
                        nom_circo = unique(nom_circo))
    
    # legislature number (based on first round election dates)
    m = parse_date_time(substr(deputes$mandat, 1, 10), "%d/%m/%y")
    deputes$legislature = parse_legislature(m, subset = sessions)

    deputes = filter(deputes, legislature %in% sessions)
    
    # fix extra content in party groups
    deputes$groupe_sigle = gsub("(\\s|-)+([R|r]éélu|Invalidé|\\d+)(.*)", "", deputes$groupe_sigle)
    
    # fix party group of Bernard Gérard
    # http://www.assemblee-nationale.fr/sycomore/fiche.asp?num_dept=17212
    deputes$groupe_sigle[ deputes$nom == "BERNARD GERARD" ] = "Union pour un mouvement populaire"
    
    # fix legislature of Marc Francina (elected three months before election)
    # http://www.assemblee-nationale.fr/sycomore/fiche.asp?num_dept=12108
    deputes$legislature[ deputes$nom == "MARC FRANCINA" & deputes$legislature == 11 ] = 12
    
    # assign cross-legislature parliamentary groups
    # note: two groups also look at the constituency column due to a few inversions in the parsed data
    y = rep(NA, nrow(deputes))
    y[ grepl("Anciens départements d'Algérie|Unité de la République|aucun groupe|non inscrits|Non( |-)inscrit|République et liberté", deputes$groupe_sigle) ] = "SE"
    y[ grepl("Réformateurs démocrates sociaux|Progrès et démocratie moderne|Républicains indépendants|Indépendants et paysans d'action sociale|Union centriste|[C|c]entre démocratique|Union pour la démocratie française|Nouveau Centre|Union des démocrates et indépendants|Union du centre|Union pour la démocratie française et du centre", deputes$groupe_sigle) | grepl("Républicains indépendants", deputes$nom_circo) ] = "CEN"
    y[ grepl("Union pour la nouvelle République|Union des démocrates pour la République|Union démocratique pour la V° République|Démocratie libérale|Union pour la majorité présidentielle|Union pour un [M|m]ouvement [P|p]opulaire|Rassemblement pour la République", deputes$groupe_sigle) | grepl("Démocratie libérale|Union pour la majorité présidentielle|Union pour un [M|m]ouvement [P|p]opulaire|Rassemblement pour la République", deputes$nom_circo) ] = "DRO"
    y[ grepl("Entente démocratique|Rassemblement démocratique|Radicaux de gauche|[S|s]ocialiste", deputes$groupe_sigle) | grepl("[S|s]ocialiste", deputes$nom_circo) ] = "SOC"
    y[ grepl("Écologiste|Radical, citoyen et verts", deputes$groupe_sigle) ] = "ECOLO"
    y[ grepl("Radical, républicain, démocrate et progressiste", deputes$groupe_sigle) ] = "RRDP"
    y[ grepl("Front national", deputes$groupe_sigle) ] = "FN"
    y[ grepl("[C|c]ommuniste|Gauche démocrate et républicaine", deputes$groupe_sigle) ] = "COM"
    
    # manual fixes
    y[ deputes$nom == "DANIELE HOFFMAN RISPAL" ] = "SOC"
    y[ deputes$nom == "YVON ABIVEN" ] = "SE"
    y[ deputes$nom == "JEAN YVES AUTEXIER" ] = "SOC"
    y[ deputes$nom == "ALEXANDRE LEONTIEFF" ] = "SE" # ex-RPR
    y[ deputes$nom == "JEAN CLAUDE CHERMANN" ] = "SE"
    y[ deputes$nom == "PIERRE RINGENBACH" ] = "DRO"
    
    # "Suppléant de Patrick Devedjian de 1988 à 2002 il équilibre la 
    #  circonscription par ses conseils, ses convictions et sa fibre
    #  sociale." (from his Wikipedia page, oldid 100562264, 2014)
    
    y[ deputes$nom == "VICTOR RINGEISEN" ] = "SE" # radical valoisien
    y[ deputes$nom == "BENOIT ROY" ] = "SE" 
    y[ deputes$nom == "FRANCIS GALIZI" ] = "CEN"
    y[ deputes$nom == "GUY DESESSART" ] = "SE" # election cancelled
    
    # manual fixes, 2007-2012, mostly leftwing
    y[ deputes$nom == "JACQUELINE FARREYROL" ] = "DRO"
    y[ deputes$nom == "ARNAUD ROBINET" ] = "DRO"
    y[ deputes$nom == "CECILE DUMOULIN" ] = "DRO"
    y[ deputes$nom == "ANNY POURSINOFF" ] = "SOC" # Green
    y[ deputes$nom == "JEAN PIERRE FOUGERAT" ] = "SOC"
    y[ deputes$nom == "JEAN LUC DRAPEAU" ] = "SOC"
    y[ deputes$nom == "JEAN CLAUDE GOUGET" ] = "SOC" # PRG
    y[ deputes$nom == "ANDRE ROUXEL" ] = "SOC"
    y[ deputes$nom == "THERESE GUILBERT" ] = "SOC"
    y[ deputes$nom == "EMMANUELLE AJON" ] = "SOC"
    y[ deputes$nom == "JOELLE BOROWSKI" ] = "SOC"
    y[ deputes$nom == "PATRICE FRANCOIS" ] = "SE" # Isère
    y[ deputes$nom == "YVON ROBERT" ] = "SOC"
    y[ deputes$nom == "JEROME GUEDJ" ] = "SOC"
    y[ deputes$nom == "REMI CHAINTRON" ] = "SOC"
    y[ deputes$nom == "JACQUES MOIGNARD" ] = "SOC" # in RDSE seat (wtf)
    y[ deputes$nom == "AUDREY MARIE" ] = "SE" # Guyane
    y[ deputes$nom == "JEAN MARIE BEFFARA" ] = "SOC"
    y[ deputes$nom == "CARLOS DA SILVA" ] = "SOC"
    y[ deputes$nom == "FLORENCE DELAUNAY" ] = "SOC"
    
    # solve remaining MPs from colonial territories
    y[ is.na(y) & with(deputes, groupe_sigle == nom_circo) ] = "SE"
    y[ deputes$nom == "TETUAAPUA POUVANAA OOPA" ] = "SE"
    
    stopifnot(all(!is.na(y)))
    
    # MP colors
    colors = c(
      "ECOLO" = "#4DAF4A", # green
      "COM"   = "#E41A1C", # red
      "RRDP"  = "#FFFF33", # yellow
      "SOC"   = "#F781BF", # pink
      "CEN"   = "#FF7F00", # orange
      "DRO"   = "#377EB8", # blue
      "FN"    = "#A65628", # brown
      "SE"    = "#999999") # grey
    
    # add as sponsor variable
    deputes$party = parse_color(y, colors)
    
    # clean up extra content in constituencies
    deputes$nom_circo = gsub("(.*) - (Fédération|Union|Rassemblement|Républicains|Socialiste)(.*)", "\\1", deputes$nom_circo)
    
    # geocode constituencies
    geo = "raw/an/geocodes.txt"
    
    if(!file.exists(geo))
      write.csv(parse_geo(deputes$nom_circo), file = geo, row.names = FALSE)
    
    deputes = merge(deputes, read.csv(geo, stringsAsFactors = FALSE),
                    by = "nom_circo", all.x = TRUE)
    
    # convert total number of mandates to legislature-specific cumulative count
    deputes = deputes %.%
      group_by(num_dept) %.%
      arrange(num_dept, legislature) %.%
      mutate(nb_mandats = seq(1 + max(nb_mandats) - length(nb_mandats), 
                              max(nb_mandats)))
    
    # reelection dummy
    deputes = mutate(group_by(deputes, num_dept),
                     reelected = as.numeric(nb_mandats < max(nb_mandats)))
    
    # last legislature
    deputes$reelected[ deputes$legislature == max(deputes$legislature) ] = NA
    
    # URL and UID
    deputes$url = paste0("http://www.assemblee-nationale.fr/sycomore/fiche.asp?num_dept=", deputes$num_dept)
    deputes$uid = deputes$num_dept
    
    j = c("legislature", "uid", "nom", "nom_de_famille", "mandat", "sexe", "annee_naissance",
          "party", "nom_circo", "lon", "lat", "nb_mandats", "reelected", "url")  
    
    # save master sponsor dataset
    write.csv(arrange(deputes[, j], nom, legislature),
              file = data, row.names = FALSE)
    
    if(verbose)
      return(msg("Saved:", data, 
                 n_distinct(deputes$uid), "unique sponsors",
                 nrow(deputes), "mandates"))
    
  }
  else {
    
    if(verbose)
      return(msg("Saved:", data,
             nrow(read.csv(data)), "mandates", file_size(data)))
    
  }
  
}

# have a nice day
