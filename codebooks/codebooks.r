
#
# SETTINGS
#

# knitr defaults
library(knitr)
opts_chunk$set(
  echo=FALSE,
  cache=FALSE,
  include=FALSE,
  results='asis'
)

# refresh data?
update = FALSE

#
# FUNCTIONS
#

#' generic readers
#
get_db <- function(db, url = NULL, n = NULL) {
   
  # presets

  file = paste0(db, ".zip")
  data = paste0(db, ".sql")

  if(db == "ameli") {
    url = "http://data.senat.fr/data/ameli/ameli.zip"
    n = 2293
  }
  if(db == "dosleg") {
    url = "http://data.senat.fr/data/ameli/dosleg.zip"
    n = 8500
  }  
  stopifnot(!any(is.null(url), is.null(n)))
  
  # data
  
  if(!file.exists(file)) {
    download.file(url, file, mode = "wb")
    update = TRUE
  }
  if(!file.exists(data) | update) unzip(file, junkpaths = TRUE)
  
  db = readLines(data, encoding = "UTF-8", n = 8500)
  file.remove(data)
  
  # comments
  
  db = db[grepl("^COMMENT", db)]
  return(db)

}

parse_db <- function(db, x) {
  tbl = db[grepl(paste("^COMMENT ON", toupper(x)), db)]
  tbl = gsub(paste0("^COMMENT ON ", toupper(x)," |\\\"|(')?;$|;"), "", tbl)
  tbl = gsub("( IS ')", ";", tbl)
  tbl = gsub("''", "'", tbl)
  tbl = t(matrix(rbind.fill.matrix(strsplit(tbl, ";")), nrow = 2))
  colnames(tbl) = c(x, "comment")
  rownames(tbl) = NULL
  return(tbl)
}

parse_tables <- function(db) {
  return(parse_db(db, x = "table"))
}

parse_columns <- function(db, tbl) {
  var = parse_db(db, x = "column")
  var = rbind.fill(lapply(tbl[, "table"], function(x) {
    y = subset(data.frame(var), grepl(paste0("^", x, "\\."), column))
    data.frame(table = x,
      column = gsub( paste0(x, "\\."), "", y$column ),
      comment = y$comment
    )
  }))
}

#' from the \code{xtable} package
#
sanitize <- function(x) {
  y = gsub("\\\\", "SANITIZE.BACKSLASH", x)
  y = gsub("$", "\\$", y, fixed = TRUE)
  y = gsub(">", "$>$", y, fixed = TRUE)
  y = gsub("<", "$<$", y, fixed = TRUE)
  y = gsub("|", "$|$", y, fixed = TRUE)
  y = gsub("{", "\\{", y, fixed = TRUE)
  y = gsub("}", "\\}", y, fixed = TRUE)
  y = gsub("%", "\\%", y, fixed = TRUE)
  y = gsub("&", "\\&", y, fixed = TRUE)
  y = gsub("_", "\\_", y, fixed = TRUE)
  y = gsub("#", "\\#", y, fixed = TRUE)
  y = gsub("^", "\\verb|^|", y, fixed = TRUE)
  y = gsub("~", "\\~{}", y, fixed = TRUE)
  y = gsub("SANITIZE.BACKSLASH", "$\\backslash$", y, fixed = TRUE)
  return(y)
}

#' generic print
#
print_table <- function(x, data = var, tables = tbl) {
  y = subset(data, table == x)
  y$column = paste0("\\texttt{", sanitize(y$column), "}")
  y$comment = sanitize(y$comment)
  z = paste(paste0("\\texttt{", sanitize(x), "}"), ":", tables[ tables[, 1] == x, "comment" ])
  print(xtable(y[, -1], caption = z, align="lp{3cm}p{10cm}"), 
        floating = FALSE,
        tabular.environment = "longtable",
        include.colnames = FALSE,
        include.rownames = FALSE,
        sanitize.text.function = as.character,
        caption.placement = "top")
}
