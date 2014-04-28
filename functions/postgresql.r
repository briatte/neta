#' Make a connection to a database
#' 
#' This function abstracts the idea of a database connection, allowing variable parameters 
#' depending on the type of database you're connecting to
#'@param config a named list of the configuration options for the database connection
#'@return a connection to the database defined in the config
#'@author Erik Gregory
makeCxn <- function(config) {
  if (class(config[['drv']]) == "character") {
    config[['drv']] <- dbDriver(config[['drv']])
  }
  do.call(dbConnect, config)
}

#' This function runs a query on a database, fetching the result if desired
#' 
#' The purpose of this function is to remove connection management from the querying process
#' @param query the query you want to make to the SQL connection you've specified
#' @param config a named list of the configuration options for the connection
#' @param n the number of rows to return, or -1 for all rows
#' @param verbose Should the queries be printed as they're made?
#' @param split Should the queries be split on semicolons, or run as a block?
#' @return A list of results if multiple queries, or a single result if one query.
#' @author Erik Gregory
fetchQuery <- function(query, config = sql, split = FALSE, verbose = TRUE, n = -1) {
  res <- list()
  cxn <- makeCxn(config)
  t1 <- Sys.time()
  queries <- query
  if (split == TRUE) {
    queries <- strsplit(query, ";", fixed = TRUE)[[1]] # Split the query into components
  }
  for (item in queries) {
    if(verbose) {
      cat(paste(item, '\n'))
    }
    tmp <- try(dbSendQuery(cxn, item)) # send the query
    if ('try-error' %in% class(tmp)) {
      res[[item]] <- dbGetException(cxn)
      next
    }
    type <- tolower(substring(gsub(" ", "", item), 0, 6)) # identify if select, insert, delete
    if (type == "select" | grepl("with..", type) | grepl('EXPLAI|explai', type) | !split) {
      res[[item]] <- try(fetch(tmp, n))
    }
    else {
      res[[item]] <- dbGetRowsAffected(tmp)
      cat(res[[item]])
    }
    if (verbose) {
      print(Sys.time() - t1)
      if (!is.null(dim(res))) {
        print(dim(res))
      }
    }
    dbClearResult(tmp)
  }
  dbDisconnect(cxn)
  if (length(res) == 1) {
    res <- res[[1]]
  }
  res
}

#' PotsgreSQL shortcut
#' 
#' A wrapper around driver and query functions.
get_sql <- function(x, sql, verbose = FALSE) {
  fetchQuery(query = x, config = sql, verbose = verbose)
}

# have a nice day
