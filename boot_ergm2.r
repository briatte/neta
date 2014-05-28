rm(list = ls())
require(ergm)
setwd('/Users/fr/Documents/Code/R/neta')

#' @source Jennifer Hayes Clarke and Veronica Caro, "Multimember Districts and 
#' the Substantive Representation of Women: An Analysis of Legislative 
#' Cosponsorship Networks", Politics & Gender 9: 1-30, 2013.
bootMPLE = function(Y, X, nboot){
  
  coefs = NULL
  ut = unique(X$i)
  x = X[, 1:(ncol(X) - 1)]
  
  for(i in 1:nboot) {
    
    cat(nboot - i, "... ")
    b = sample(ut, length(ut), replace = TRUE)
    
    indic = NULL
    for(j in 1:length(b)) {
      indic = c(indic, which(X$i == b[j]))
    }
    
    xi = as.matrix(x[indic, ])
    yi = Y[indic]
    esti = glm(yi ~ -1 + xi, family = binomial)
    coefs = rbind(coefs, coef(esti))
  }
  cat("done.\n")
  return(coefs)
}

get_tergm = function(ch, sessions = 8:14, nboot = 1000) {
  
  stopifnot(ch %in% c("an", "se"))

  if(is.character(sessions)) {
    stopifnot(length(sessions) == 1 & sessions %in% c("lw", "rw"))
    suffix = paste0("_", sessions)
    if(sessions == "rw") sessions = c(8, 10, 12:13) # rightwing government
    if(sessions == "lw") sessions = c(9, 11, 14)    # leftwing government
  } else {
    suffix = ""
  }
  
  data = paste0("models/ergm/boot_", ch, nboot, suffix, ".rda")
  message(paste("Model:", data), " legislatures ", paste0(sessions, collapse = ", "))
  
  if(!file.exists(data)) {
    
    raw = gsub(".rda", "_raw.rda", data)
    if(!file.exists(raw)) {
      load(paste0("data/", ch, ".rda"))
    
      N = mget(paste0("net", sessions))
    
      n = length(N)
      Y = NULL
      X = NULL
    
      for(i in 1:n) {
      
        message(paste("Networks to estimate:", n - i + 1))
      
        net = N[[i]]
        net %v% "female" = as.numeric(net %v% "sexe" == "F")
        net %v% "seniority" = net %v% "nb_mandats"
        net %v% "rightwing" = as.numeric(net %v% "party" %in% c("DRO", "CEN", "FN"))
      
        # merge appended sponsors to coalition
        s = net %v% "party"
        if(sum(s == "ECO", na.rm = TRUE) < 10 & any(s == "ECO"))
          s[ s == "ECO" ] = "SOC"
        if(sum(s == "COM", na.rm = TRUE) < 10 & any(s == "COM"))
          s[ s == "COM" ] = "SOC"
      
        # mark unaffiliated sponsors as missing
        net %v% "party" = ifelse(s == "SE", NA, s)
      
        m = ergmMPLE(net ~ edges + # size
                       mutual +    # reciprocality
                       gwesp + # GWESP
                       gwdsp(alpha = 1, fixed = TRUE) + # GWDSP
                       gwidegree(decay = 1, fixed = TRUE) + # GWD
                       gwodegree(decay = 1, fixed = TRUE) + # GWD
                       nodefactor("female") +               # NF
                       nodematch("female") +                # Uniform homoph.
                       nodecov("seniority") +               # Nodal cov.
                       absdiff("seniority") +               # absolute diff.
                       nodefactor("rightwing") +            # NF
                       absdiffcat("rightwing") +            # categ. diff.
                       nodefactor("party") +                # NF
                       nodematch("party", diff = TRUE))
      
        # empty nodal factor sets
        if(!"nodefactor.party.COM" %in% colnames(m$predictor))
          m$predictor = cbind(nodefactor.party.COM = 0, m$predictor)
        if(!"nodefactor.party.ECO" %in% colnames(m$predictor))
          m$predictor = cbind(nodefactor.party.ECO = 0, m$predictor)
        if(!"nodefactor.party.SOC" %in% colnames(m$predictor))
          m$predictor = cbind(nodefactor.party.SOC = 0, m$predictor)
        if(!"nodefactor.party.RAD" %in% colnames(m$predictor))
          m$predictor = cbind(nodefactor.party.RAD = 0, m$predictor)
        if(!"nodefactor.party.CEN" %in% colnames(m$predictor))
          m$predictor = cbind(nodefactor.party.CEN = 0, m$predictor)
        if(!"nodefactor.party.DRO" %in% colnames(m$predictor))
          m$predictor = cbind(nodefactor.party.DRO = 0, m$predictor)
        if(!"nodefactor.party.FN" %in% colnames(m$predictor))
          m$predictor = cbind(nodefactor.party.FN = 0, m$predictor)
        if(!"nodefactor.party.SE" %in% colnames(m$predictor))
          m$predictor = cbind(nodefactor.party.SE = 0, m$predictor)
      
        # empty differential homophily sets
        if(!"nodematch.party.COM" %in% colnames(m$predictor))
          m$predictor = cbind(nodematch.party.COM = 0, m$predictor)
        if(!"nodematch.party.ECO" %in% colnames(m$predictor))
          m$predictor = cbind(nodematch.party.ECO = 0, m$predictor)
        if(!"nodematch.party.SOC" %in% colnames(m$predictor))
          m$predictor = cbind(nodematch.party.SOC = 0, m$predictor)
        if(!"nodematch.party.RAD" %in% colnames(m$predictor))
          m$predictor = cbind(nodematch.party.RAD = 0, m$predictor)
        if(!"nodematch.party.CEN" %in% colnames(m$predictor))
          m$predictor = cbind(nodematch.party.CEN = 0, m$predictor)
        if(!"nodematch.party.DRO" %in% colnames(m$predictor))
          m$predictor = cbind(nodematch.party.DRO = 0, m$predictor)
        if(!"nodematch.party.FN" %in% colnames(m$predictor))
          m$predictor = cbind(nodematch.party.FN = 0, m$predictor)
        if(!"nodematch.party.SE" %in% colnames(m$predictor))
          m$predictor = cbind(nodematch.party.SE = 0, m$predictor)
      
        m$predictor = m$predictor[, c("edges", "mutual", "gwesp", "gwdsp.fixed.1", "gwidegree", "gwodegree", "nodefactor.female.1", "nodematch.female", "nodecov.seniority", "absdiff.seniority", "nodefactor.rightwing.1", "absdiff.rightwing.1", "nodefactor.party.COM", "nodefactor.party.ECO", "nodefactor.party.SOC", "nodefactor.party.RAD", "nodefactor.party.CEN", "nodefactor.party.DRO", "nodefactor.party.FN", "nodefactor.party.SE", "nodematch.party.COM", "nodematch.party.ECO", "nodematch.party.SOC", "nodematch.party.RAD", "nodematch.party.CEN", "nodematch.party.DRO", "nodematch.party.FN", "nodematch.party.SE") ]
      
        Y = c(Y, m$response)
        X = rbind(X, cbind(m$predictor, i))
      
      }
    
      save(X, Y, file = raw)
    
    } else {
    
      load(raw)
    
    }

    bsample = bootMPLE(Y, data.frame(X), nboot)
    
    message("Fitting bootstrapped TERGM...")
    B = as.matrix(X[, 1:(ncol(X) - 1)])
    m = glm(Y ~ B - 1, family = binomial)
    summary(m)
  
    save(B, Y, m, bsample, file = data)

  }
  
}

get_tergm(ch = "se", sessions = "rw", nboot = 1000)
get_tergm(ch = "se", sessions = "lw", nboot = 1000)

get_tergm(ch = "an", sessions = "rw", nboot = 1000)
get_tergm(ch = "an", sessions = "lw", nboot = 1000)

# get_tergm(ch = "an", sessions = 8:14, nboot = 20)
# get_tergm(ch = "se", sessions = 8:14, nboot = 20)
