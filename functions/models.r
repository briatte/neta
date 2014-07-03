#' Makefile call
#'
get_models <- function(key, sessions = 8:14) {
  
  if(key == "ergmm") {

    # latent space cluster models
    get_ERGMM("an", sessions)
    get_ERGMM("se", sessions)

  } else if(key == "ergm") {
    
    get_TERGM(ch = "an", sessions, nboot = 1000)
    get_TERGM(ch = "se", sessions, nboot = 1000)

    if(sessions == 8:14) {
      
      # leftwing/rightwing, divided/unified (for full series only)
      for(j in c("lw", "rw", "di", "un")) {

        get_TERGM(ch = "se", sessions = j, nboot = 1000)
        get_TERGM(ch = "an", sessions = j, nboot = 1000)

      }
      
      plot_TERGM("models/ergm.csv", key = "rw")
      plot_TERGM("models/ergm.csv", key = "di")

    }

  }

}

#' Model a two-dimensional latent space in each legislature
#'
#' Caution: the estimator does not scale well, so you might want to parallelize
#' the run with foreach, or use a more parsimonious method like the one covered
#' in Vu et al., Ann. Appl. Stat. 7: 1010-1039, 2013 (doi:10.1214/12-AOAS617).
get_ERGMM <- function(sessions = 8:14) {
  
  for(ch in c("an", "se")) {
    
    chamber = ifelse(ch == "an", "Assemblée nationale", "Sénat")
    
    for(file in paste0(ch, sessions)) {
      
      legid = gsub("\\D", "", file)
      title = paste(chamber, "legislature", legid)
      
      m = paste0("models/ergmm/ergmm_", file, ".rda")
      if(!file.exists(m)) {
        
        data = paste0("data/", file, ".rda")
        if(!file.exists(data))
          load(paste0("data/bi_", file, ".rda"))
        else
          load(paste0("data/", file, ".rda"))
        
        net %v% "female" = net %v% "sexe" == "F"
        net %v% "seniority" = net %v% "nb_mandats"
        
        # identify number of relevant latent space clusters
        g = table(na.omit(net %v% "party"))
        g = length(g[ g > 9 & names(g) != "SE" ])
        
        msg("Modeling:", title, g, "clusters (n > 10)")

        # dependent variable and unobserved group memberships
        print(table(net %e% "count"))
        print(table(net %v% "party", exclude = NULL))
        
        ctrl = ergmm.control(burnin = 10^5,      # 100 * default
                             sample.size = 5000, # default + 1000
                             interval = 10)      # default
                                                      
        # two-dimensional latent clustering random effects model; Krivitsky et
        # al. 2012 equation 4, using counts of ties and a Poisson distribution 
        # to account for overdispersion (NOTE: takes several hours to compute)
        ERGMM = ergmm(net ~ euclidean(d = 2, G = g) + rreceiver + rsender,
                      response = "count", family = "Poisson.log",
                      control = ctrl, seed = 2357, verbose = TRUE)

        # save model results
        save(ERGMM, file = m)
        
        # densities and traceplots
        ggmcmc(ggs(as.mcmc.list(ERGMM)),
               file = gsub(".rda", "_trace.pdf", m),
               plot = "density traceplot")
        
      }
      
    }
    
  }
  
}

#' Draw a circle in ggplot2
#'
#' @source \url{http://stackoverflow.com/a/6863490/635806}
circle <- function(center = c(0, 0), diameter = 1, npoints = 100) {
  r = diameter / 2
  t = seq(0, 2 * pi, length.out = npoints)
  x = center[1] + r * cos(t)
  y = center[2] + r * sin(t)
  return(data.frame(x = x, y = y))
}

#' Plot latent space conditional posterior means, traceplots and iterations
#'
#' @source hacked from \code{plot.ergmm} in the \code{latentnet} package
#' (version 2.5.1, 2014-02-17) by Pavel N. Krivitsky, Mark S. Handcock, 
#' Susan M. Shortreed, Jeremy Tantrum and Peter D. Hoff
plot_ERGMM <- function(file = "(an|se)[0-9]+", update = FALSE,
                       palette = brewer.pal(8, "Set3")) {

  stopifnot(all(grepl("^(\\(|an|se)", file)))

  objects = dir("models/ergmm/", pattern = paste0(file, ".rda"))
  objects = paste0("models/ergmm/", objects)

  for(i in objects) {

    chamber = ifelse(grepl("/an", i), "Assemblée nationale", "Sénat")
    title = paste(chamber, "legislature", gsub("\\D", "", i))
    msg("Plotting:", title)

    load(i)
    plots = gsub("^models/ergmm", "plots", i)
  
    # conditional posterior means (to plots folder)

    p = gsub(".rda", "_ergmm.pdf", plots)
    if(!file.exists(p) | update) {

      G  = ERGMM[["model"]][["G"]]
      Yg = ERGMM[["model"]][["Yg"]]
      n = network.size(Yg)
      
      cat("Plotting:", n, "nodes", G, "groups...")
      s = summary(ERGMM, point.est = "pmean")
      
      # placement
      Z = s[["pmean"]][["Z"]]
      Yg %v% "lon" = Z[, 1]
      Yg %v% "lat" = Z[, 2]
      
      # conditional means
      s      = s[["pmean"]]
      Z.mean = s[["Z.mean"]]
      Z.var  = s[["Z.var"]]
      Z.K    = s[["Z.K"]]
      
      # size nodes by quartiles of random effects
      if(ERGMM[["model"]][ "sender" ][[1]]) {
        q = as.vector(s['sender'][[1]])
        q = cut(q, quantile(q), include.lowest = TRUE)
        Yg %v% "size" = as.numeric(q) - 1
      } else {
        Yg %v% "size" = 0
      }
      
      # plot conditional posterior means
      mu = lapply(1:nrow(Z.mean), function(x) { 
        return(data.frame(K = x, circle(Z.mean[x, ], diameter = Z.var[x])))
      })
      mu = rbind.fill(mu)
  
      g = ggnet(Yg, mode = "geo", size = 0, geo.outliers = FALSE, arrow.size = 1/2) +
        geom_point(aes(size = 9 + Yg %v% "size", color = Yg %v% "party"), alpha = .5) +
        geom_point(aes(size = 9, color = Yg %v% "party"), alpha = 1) +
        geom_text(aes(size = 3, label = Z.K), color = "white", alpha = .5, fontface = "bold") +
        geom_path(data = mu, aes(group = factor(K), x = x, y = y), alpha = 2/3) +
        scale_color_manual("", values = Yg %n% "party_colors", breaks = Yg %n% "party_order") +
        scale_size_area(max_size = 12) + 
        guides(size = FALSE, 
               color  = guide_legend(override.aes = list(alpha = .5, size = 6))) + 
        theme(text = element_text(size = 28),
              legend.key = element_rect(colour = "white", fill = NA),
              legend.key.size = unit(28, "pt"),
              plot.margin = rep(unit(0, "mm"), 4)) +
        coord_equal()
      # labs(y = expression(Z[2]), x = expression(Z[1]))
      
      ggsave(p, g, width = 11, height = 9)
      cat("done.\n")

      # table of node memberships
      # togetherness = round(100 * apply(t, 1, max) / rowSums(t), 1)
      t = table(Yg %v% "party", Z.K)

      write.csv(t, row.names = TRUE, file = gsub(".rda", ".csv", i))

    }

    # traceplot (to models folder)

    p = gsub(".rda", "_trace.pdf", i)
    if(!file.exists(p) | update) {
  
      d = as.matrix(as.mcmc.list(ERGMM))
      d = data.frame(d)

      d$iterations = as.vector(time(as.mcmc.list(ERGMM)))
      cat("Plotting: trace of", max(d$iterations), "iterations...")

      g = qplot(data = d, x = iterations, y = receiver.1, geom = "line", alpha = I(.75)) + 
        labs(y = "Trace of receiver effect\n", x = "MCMC draws",
             title = paste(title, "\n")) +
        scale_x_continuous(breaks = c(10^3, 10^4)) +
        theme_grey(16)
      cat("done.\n")

      ggsave(p, g, width = 9, height = 9)

    }

    # animation (to plots folder)

    p = gsub(".rda", "_ergmm.gif", plots)
    if(!file.exists(p) | update) {

      j = seq(1, ERGMM$control$sample.size, by = 50)
      j = c( j[1]:j[2], j[ -1:-2 ] )

      message(paste("Plotting:", length(j), "MCMC draws...",
                    "\nFilename:", gsub("plots/", "", p),
                    "\nSaving to", paste0(getwd(), "/plots")))

      saveGIF({
        for(k in j) {
          plot(ERGMM, cluster.col = palette, what = k,
               suppress.axes = TRUE, print.formula = FALSE)
        }
      }, interval = .1,
         movie.name = gsub("plots/", "", p),
         outdir = paste0(getwd(), "/plots"))
      cat("done.\n")

    }
 
  }

}

#' @source Jennifer Hayes Clarke and Veronica Caro, "Multimember Districts and 
#' the Substantive Representation of Women: An Analysis of Legislative 
#' Cosponsorship Networks", Politics & Gender 9: 1-30, 2013.
bootMPLE <- function(Y, X, nboot){
  
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

#' Get differential homophily ERGMs
#'
get_TERGM <- function(ch, sessions = 8:14, nboot = 1000) {
  
  stopifnot(ch %in% c("an", "se"))

  if(sessions == "yr") {
    
    suffix = "_yr"
    sessions = dir("data", pattern = "(an|se)[0-9]{1,2}.[0-9]{4}.rda")
    sessions = table(str_extract(sessions, "\\d+\\.\\d+"))
    sessions = names(sessions)[ sessions > 1 & names(sessions) != "8.1988" ]
    sessions = sessions[ order(as.numeric(sessions)) ]
    
  }
  else if(is.character(sessions)) {

    stopifnot(length(sessions) == 1 & sessions %in% c("lw", "rw", "di", "un"))
    suffix = paste0("_", sessions)
    
    if(sessions == "rw") sessions = c(8, 10.2, 12:13)  # rightwing government
    if(sessions == "lw") sessions = c(9, 11, 14)       # leftwing government
    if(sessions == "di") sessions = c(8, 11)           # divided government (missing 10.1)
    if(sessions == "un") sessions = c(9, 10.2, 12:14)  # unified government

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

        msg("Networks to estimate:", n - i + 1, net %n% "legislature")
      
        net = N[[i]]

        net %v% "female" = as.numeric(net %v% "sexe" == "F")
        net %v% "seniority" = net %v% "nb_mandats"
        net %v% "rightwing" = ifelse(net %v% "party" == "SE", NA,
                                     as.numeric(net %v% "party" %in% c("DRO", "CEN", "FN")))
      
        # merge appended sponsors to coalition
        s = net %v% "party"
        if(sum(s == "ECO", na.rm = TRUE) < 10 & any(s == "ECO"))
          s[ s == "ECO" ] = "SOC"
        if(sum(s == "COM", na.rm = TRUE) < 10 & any(s == "COM"))
          s[ s == "COM" ] = "SOC"
        if(sum(s == "RAD", na.rm = TRUE) < 10 & any(s == "RAD"))
          s[ s == "RAD" ] = "SOC"
        if(sum(s == "CEN", na.rm = TRUE) < 10 & any(s == "CEN"))
          s[ s == "CEN" ] = "DRO"
        if(sum(s == "FN", na.rm = TRUE) < 10 & any(s == "FN"))
          s[ s == "FN" ] = "SE"
        
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

plot_TERGM <- function(models = "models/ergm.csv", key = "rw") {
  
  if(!file.exists(models)) {
    
    if(key == "rw")
      d = dir("models/ergm", pattern = "(\\d|_lw|_rw).rda")
    else if(key == "di")
      d = dir("models/ergm", pattern = "(\\d|_un|_di).rda")

    p = data.frame()
    for(i in d) {

      load(paste0("models/ergm/", i))
      
      coefs = data.frame(summary(m)[['coefficients']])
      coefs = cbind(rownames(coefs), coefs)
      
      names(coefs) = c("term", "b", "se", "z", "p")
      coefs$model = gsub("boot_|.rda", "", i)
      
      p = rbind(p, coefs)

    }

    write.csv(p, file = paste0("models/ergm_", key, ".csv"), row.names = FALSE)

  }

  coefs = read.csv(models)
  p = subset(coefs, !grepl("factor|nodecov", term))
  p$term = gsub("(.*).party.|^B|.1$", "", p$term)
  p$term = factor(p$term, levels = c("edges", "gwesp", "gwdsp.fixed", "gwidegree", "gwodegree",
                                     "mutual", "nodematch.female", "absdiff.seniority", "absdiff.rightwing",
                                     "COM", "ECO", "SOC", "RAD", "CEN", "DRO", "FN", "SE"))
  levels(p$term) = c("Edges", "GWESP", "GWDSP", "GWD cosponsors", "GWD authors",
                     "Reciprocity", "Same gender", "Diff. seniority", "Diff. left-right",
                     "Both Communist", "Both Green", "Both Socialist", "Both Radical", "Both Centrist", "Both Conservative", "Both FN", "Both unaffiliated")
  p$type = ifelse(grepl("Both", p$term), as.character(p$term), "Network controls")
  p$ch = ifelse(grepl("^an", p$model), "Assemblée nationale", "Sénat")
  
  if(key == "rw") {

    p$model = ifelse(grepl("_lw", p$model), "Leftwing\ngovernments",
                     ifelse(grepl("_rw", p$model), "Rightwing\ngovernments", "All governments\n(1986-2014)"))
    cols = c("", "\\textbf{All legislatures}", "SE", "\\textbf{Leftwing governments}", "SE", "\\textbf{Rightwing governments}", "SE")
    
    m = subset(p, ch == "Assemblée nationale" & grepl("All", model))[, 1:3]
    m = merge(m, subset(p, ch == "Assemblée nationale" & grepl("Left", model))[, 1:3], by = "term", all = TRUE)
    m = merge(m, subset(p, ch == "Assemblée nationale" & grepl("Right", model))[, 1:3], by = "term", all = TRUE)
    
  }
  else if(key == "di") {

    p$model = ifelse(grepl("_di", p$model), "Divided\ngovernments",
                     ifelse(grepl("_un", p$model), "Unified\ngovernments", "All governments\n(1986-2014)"))
    cols = c("", "\\textbf{All legislatures}", "SE", "\\textbf{Divided governments}", "SE", "\\textbf{Unified governments}", "SE")
    
    m = subset(p, ch == "Assemblée nationale" & grepl("All", model))[, 1:3]
    m = merge(m, subset(p, ch == "Assemblée nationale" & grepl("Divided", model))[, 1:3], by = "term", all = TRUE)
    m = merge(m, subset(p, ch == "Assemblée nationale" & grepl("Unified", model))[, 1:3], by = "term", all = TRUE)
    
  }
  
  m = m[ order(as.numeric(m$term)), ]
  m$term = as.character(m$term)
  m[, -1] = round(m[, -1], 2)
  m[, 3] = paste0("(", m[, 3], ")")
  m[, 5] = paste0("(", m[, 5], ")")
  m[, 7] = paste0("(", m[, 7], ")")

  m = rbind(c("\\textit{Network structure}", rep("", ncol(m) - 1)),
            m[1:6, ],
            c("\\textit{Balancing effects}", rep("", ncol(m) - 1)),
            m[7:9, ],
            c("\\textit{Party homophily}", rep("", ncol(m) - 1)),
            m[10:16, ]
  )

  names(m) = cols

  print(xtable(m, align = "lrrrrrrr"),
        sanitize.colnames.function = as.character,
        sanitize.text.function = as.character,
        include.rownames = FALSE, file = paste0("paper/tables/ergm_", key, "an.tex"))

  if(key == "rw") {

    m = subset(p, ch != "Assemblée nationale" & grepl("All", model))[, 1:3]
    m = merge(m, subset(p, ch != "Assemblée nationale" & grepl("Left", model))[, 1:3], by = "term", all = TRUE)
    m = merge(m, subset(p, ch != "Assemblée nationale" & grepl("Right", model))[, 1:3], by = "term", all = TRUE)

  } else if(key == "di") {
    
    m = subset(p, ch != "Assemblée nationale" & grepl("All", model))[, 1:3]
    m = merge(m, subset(p, ch != "Assemblée nationale" & grepl("Divided", model))[, 1:3], by = "term", all = TRUE)
    m = merge(m, subset(p, ch != "Assemblée nationale" & grepl("Unified", model))[, 1:3], by = "term", all = TRUE)
    
  }
  
  m = m[ order(as.numeric(m$term)), ]
  m$term = as.character(m$term)
  m[, -1] = round(m[, -1], 2)
  m[, 3] = paste0("(", m[, 3], ")")
  m[, 5] = paste0("(", m[, 5], ")")
  m[, 7] = paste0("(", m[, 7], ")")

  m = rbind(c("\\textit{Network structure}", rep("", ncol(m) - 1)),
            m[1:6, ],
            c("\\textit{Balancing effects}", rep("", ncol(m) - 1)),
            m[7:9, ],
            c("\\textit{Party homophily}", rep("", ncol(m) - 1)),
            m[10:15, ]
  )

  names(m) = cols

  print(xtable(m, align = "lrrrrrrr"),
        sanitize.colnames.function = as.character,
        sanitize.text.function = as.character,
        include.rownames = FALSE, file = paste0("paper/tables/ergm_", key, "se.tex"))

  qplot(data = subset(p, grepl("Both", term)), color = type,
        y = b, x = model, ymin = b - 3 * se, ymax = b + 3 * se,
        geom = "pointrange") +
    geom_pointrange(color = "black", alpha = .5) +
    scale_y_continuous(breaks = 0:4) +
    scale_color_manual("", values = c(
      "Network controls" = "grey25",
      "Both Communist" = "#E41A1C",    # (L; red)
      "Both Green" = "#4DAF4A", # (L; green)
      "Both Radical" = "#FFFF33",      # (L; yellow)
      "Both Socialist" = "#F781BF",    # (or large L coalition; pink)
      "Both Centrist" = "#FF7F00",     # (R; orange)
      "Both Conservative" = "#377EB8", # (R; blue)
      "Both FN" = "#A65628" # (R; brown)
    )) +
    facet_grid(term ~ ch) +
    geom_hline(yintercept = 0, color = "grey25", linetype = "dashed") +
    labs(x = NULL, y = NULL) +
    theme_grey(16) +
    theme(legend.position = "none",
          axis.text = element_text(color = "black"),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank())

  ggsave(paste0("models/ergm_", key, "diff.pdf"), height = 12, width = 9)

  qplot(data = subset(p, !grepl("Both|GW|Edges", term)),
        y = b, x = model, ymin = b - 3 * se, ymax = b + 3 * se,
        geom = "pointrange") +
    geom_pointrange(color = "black", alpha = .25) +
    scale_y_continuous(minor_breaks = NULL) +
    facet_grid(term ~ ch, scales = "free") +
    geom_hline(yintercept = 0, color = "grey25", linetype = "dashed") +
    labs(x = NULL, y = NULL) +
    theme_grey(16) +
    theme(legend.position = "none",
          axis.text = element_text(color = "black"),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank())
  ggsave(paste0("models/ergm_", key, "cov.pdf"), height = 12, width = 9)

  qplot(data = subset(p, grepl("GW(D|E)|Edges", term)),
        y = b, x = model, ymin = b - 3 * se, ymax = b + 3 * se,
        geom = "pointrange") +
    geom_pointrange(color = "black", alpha = .25) +
    scale_y_continuous(minor_breaks = NULL) +
    facet_grid(term ~ ch, scales = "free") +
    geom_hline(yintercept = 0, color = "grey25", linetype = "dashed") +
    labs(x = NULL, y = NULL) +
    theme_grey(16) +
    theme(legend.position = "none",
          axis.text = element_text(color = "black"),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank())
  ggsave(paste0("models/ergm_", key, "net.pdf"), height = 12, width = 9)

  print(unique(p$model))
  
}

# have a nice day
