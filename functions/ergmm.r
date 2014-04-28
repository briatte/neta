#' Model a two-dimensional latent space in each legislature
#'
#' Caution: the estimator does not scale well, so you might want to parallelize
#' the run with foreach, or use a more parsimonious method like the one covered
#' in Vu et al., Ann. Appl. Stat. 7: 1010-1039, 2013 (doi:10.1214/12-AOAS617).
get_ERGMM = function(ch, sessions = 8:14) {

  stopifnot(ch %in% c("an", "se"))  
  load(paste0("data/", ch, ".rda"))

  R = paste0("ergmm_", ch)
  N = ls(pattern = "^net[0-9]+")
  N = sort(as.numeric(gsub("\\D", "", N)))
  N = N [ N %in% sessions ]

  message(paste(ifelse(ch == "an", "Assemblée nationale", "Sénat"),
                length(N), "legislatures"))

  for(i in N) {

    file = paste0("models/", R, i, ".rda")
    name = paste0(R, i)

    if(!file.exists(file)) {

      net = get(paste0("net", i))
      t = table(net %v% "party")
      
      # ignore small-n and 'SE'
      g = length(t[ t > 5 & nchar(t) > 2 ])

      message(paste("Modeling: legislature", i, g, "groups"))

      # be patient for a few days
      ERGMM = ergmm(net ~ euclidean(d = 2, G = g) + rreceiver,
                    response = "count", family = "Poisson.log",
                    control = ergmm.control(burnin = 10^3,
                                            sample.size = 2000,
                                            interval = 5),
                    verbose = TRUE)

      assign(name, ERGMM)
      save(list = name, file = file)
      
    }

  }

}

# have a nice day
