#' Assemblée nationale: scrape depots, projets or propositions de loi
#'
#' Available sessions: 8:14 (1986-today), except art. 34-1 resolutions (13:14).
get_dossiers <- function(key, sessions = 8:14, verbose = FALSE,
                         base = "http://www.assemblee-nationale.fr") {

  stopifnot(key %in% c("projets", "propositions", "depots", "resolutions", "resol-art34-1"))

  raw = paste0("raw/an/", key, ".rda")

  if(key == "resol-art34-1")
    sessions = sessions[ sessions > 12 ]

  data = lapply(sessions, function(x, quiet = !verbose) {

    # download
    link = ifelse(key == "propositions", "proposition", key)
    link = paste0("documents/index-", link, ".asp")
    link = paste0(base, "/", x, "/", link)
    file = paste0("raw/an/indexes/", key, ".", x, ".html")

    if(!file.exists(file))
      download(link, file, mode = "wb", quiet = TRUE)

    if(!quiet)
      msg("Saved:", file)

    # parse
    html = htmlParse(file, encoding = "UTF-8")

    path = c("Proposition", "Projet", "Rapport", "Allocution", "Avis", "Lettre", "Déclaration", "Message", "Annexe")
    if(key == "projets") path = c("Projet de loi")
    if(key == "propositions") path = c("Proposition de loi")
    if(key %in% c("resolutions", "resol-art34-1")) path = c("Proposition de résolution")

    path = c(path, paste0(" ", path))
    path = paste0("starts-with(., '", path, "')")
    path = paste0("//td[", paste0(path, collapse = " or "), "]")

    depo = xpathSApply(html, paste0(path, "/preceding-sibling::td"))
    depo = lapply(depo, xmlValue)

    desc = xpathSApply(html, path)
    desc = lapply(desc, xmlValue)

    link = ifelse(grepl("Voir le dossier", desc), TRUE, FALSE)

    if(key == "resol-art34-1")
      path = xpathSApply(html, paste0(path, "//a/@href"))
    else
      path = xpathSApply(html, ifelse(x < 13 | key %in% c("projets", "resolutions"), 
                                      paste0(path, "//b/font/a/@href"), 
                                      paste0(path, "//b/a/@href")))
    link[link] = path
    link[link == "FALSE"] = NA

    if(!length(depo))
      data = data.frame()
    else
      data = data.frame(legislature = x,
                        texte = gsub("\\D", "", depo),
                        url = gsub("(.asp)(.*)", "\\1", link),
                        titre = str_trim(gsub("(\\n )?Voir le dossier", "", desc)),
                        stringsAsFactors = FALSE)

    return(data)

  })

  # aggregate indexes
  data = rbind.fill(data)
  data = unique(data[ !is.na(data$legislature) & !is.na(data$texte), ])

  # unique id
  data$uid = paste0(data$legislature, "_", data$texte)
  
  # check uids are unique
  # stopifnot(nrow(data) == n_distinct(data$uid))

  # scrape details
  file = data$url[ !is.na(data$url) ]
  file = cbind(paste0(base, file),
               paste("raw/an/dossiers/depot",
                     gsub("/(\\d+)/(.*)", "\\1", file), # session
                     gsub(".asp", ".html", str_extract(file, "(\\w|-)+.asp")) # file
                     , sep = "."))

  # remove duplicates
  if(any(duplicated(file[, 2])))
    file = file[ -which(duplicated(file[, 2])), ]

  if(verbose)
    msg("Parsing:", nrow(file), "files",
        sum(!file.exists(file[, 2])), "to download")

  # quick enough
  for(i in 1:nrow(file))
        if(!file.exists(file[i, 2]))
          download(file[i, 1], file[i, 2], mode = "wb", quiet = TRUE)

  file = file[ sapply(file[, 2], function(x) file.info(x)$size) > 0, ]
  info = lapply(na.omit(file[, 2]), function(x, quiet = !verbose) {  

    if(!quiet)
      msg("Parsing: ", x)

    html = htmlParse(x, encoding = "UTF-8")
    amendements = paste0(gsub("\\D", "", unique(unlist(lapply(xpathSApply(html, "//a[contains(@href,'NUM_INIT') and contains(@href,'RESULTATS')]"), xmlValue)))), collapse = ",")
    amendements = ifelse(amendements == "", NA, amendements)
    url  = xpathSApply(html, "//meta[@name='URL_DOSSIER']/@content")
    url  = gsub("(.asp)(.*)", "\\1", url)
    origine  = xpathSApply(html, "//meta[@name='ORIGINE_DOCUMENT']/@content")
    seance = xpathSApply(html, "//meta[@name='DISCUTE_SEANCE']/@content")
    debats = length(xpathSApply(html, "//a[contains(@href,'/cri/')]"))
    loi  = xpathSApply(html, "//meta[@name='LOI_PROMULGUEE']/@content")
    # projet = length(xpathSApply(html, "//a[contains(@href,'/projets/')]"))
    # proposition = length(xpathSApply(html, "//a[contains(@href,'/propositions/')]"))
    # declaration = length(xpathSApply(html, "//a[contains(@href,'/dg/')]"))
    # rapport = length(xpathSApply(html, "//a[contains(@href,'/rapports/')]"))
    # cr_loi = length(xpathSApply(html, "//a[contains(@href,'/cr-cloi/')]"))
    # cr_ext = length(xpathSApply(html, "//a[contains(@href,'/cr-cafe/')]"))
    # cr_def = length(xpathSApply(html, "//a[contains(@href,'/cr-cdef/')]"))
    # cr_eco = length(xpathSApply(html, "//a[contains(@href,'/cr-cpro/')]"))
    # cr_fin = length(xpathSApply(html, "//a[contains(@href,'/cr-cfiab/')]"))
    # cr_soc = length(xpathSApply(html, "//a[contains(@href,'/cr-soc/')]"))
    # rap_info = length(xpathSApply(html, "//a[contains(@href,'/rap-info/')]"))
    # rap_enq = length(xpathSApply(html, "//a[contains(@href,'/rap-enq/')]"))
    resume = xpathSApply(html, "//div[contains(@align, 'left')]")
    resume = ifelse(length(resume) > 1, xmlValue(resume[[2]]), NA)
    # full_title = xpathSApply(html, "//meta[@name='TITRES_DOC']/@content")
    # url_docs   = xpathSApply(html, "//meta[@name='URLS_DOC']/@content")
    # url_law  = xpathSApply(html, "//meta[@name='LIEN_LOI_PROMULGUEE']/@content")
    cbind(url, amendements, origine, seance, debats, loi, resume)
  })

  nulls = sapply(info, is.null)
  info = rbind.fill.matrix(info[ !nulls ])
  info = data.frame(info, stringsAsFactors = FALSE)
  info = unique(info[ !is.na(info$url), ])

  if(verbose)
    msg("Parsing:", nrow(info), "index rows out of", nrow(data))

  data = unique(merge(data, info, by = "url"))
  data = arrange(data, legislature, texte)

  # législature
  data$legislature = factor(data$legislature, levels = sessions)

  if(verbose)
    print(table(data$legislature, exclude = NULL))

  # date
  data$date = str_extract(data$resume, ", déposé[e]{0,1}\\s+le\\s+(\\d+)\\s+(\\w+)\\s+(\\d+)")
  data$date = gsub(", déposé[e]{0,1}\\s+le\\s+", "", data$date)
  data$date = clean_date(data$date)

  # type
  if(key == "depots") {
    data$type_etendu = str_extract(data$titre, "Proposition de (loi|résolution)|Projet de loi|Rapport|Message|Avis|Déclaration|Allocution|Lettre") # discarding Annexes from reports (filed only up to 2002)
    data$type = data$type_etendu
    data$type[grepl("Avis|Déclaration|Message|Allocution|Lettre", data$type)] = NA
  }

  # debates
  data$seance = ifelse(data$seance == "N", 0, 1)
  data$debats = as.numeric(data$debats)
  debats = table(data$legislature, data$debats > 0)
  debats = data$legislature %in% rownames(debats)[debats[, 2] == 0]
  data$debats[debats] = NA

  # law
  if(grepl("^resol", key)) {

    data$loi = NA
    data$loi_adoptee = NA
    data$loi_date = NA

  } else {
  
    data$loi_adoptee = ifelse(data$loi == "", 0, 1)
    data$loi_date = str_extract(data$loi, "du\\s+\\d{1,2}\\s+\\w+\\s+\\d{4}")
    data$loi_date = gsub("^du ", "", data$loi_date)
    data$loi_date = clean_date(data$loi_date)
  
  }

  data$resume = NULL
  assign(key, data)

  # final dataset
  save(list = key, file = raw)

  return(msg("Saved:", raw, nrow(data), key))
  
}

# have a nice day
