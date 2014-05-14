#' Makefile call
#'
get_models <- function(key, sessions = 8:14) {
  
  if(key == "ergmm") {

    # latent space cluster models
    get_ERGMM("an", sessions)
    get_ERGMM("se", sessions)

  } else if(key == "ergm") {
    
    # exponential random graph model at threshold (.025, .975) ~ 5% tie loss
    get_ERGM(sessions, base = TRUE, plot = TRUE)
    
    # sensitivity tests at different thresholds
    get_ERGM(sessions, cutoff = c(0.025, 1)) # no upper threshold
    get_ERGM(sessions, cutoff = c(0, 0.975)) # no lower threshold
    get_ERGM(sessions, cutoff = c(.05, .95)) # ~ 10% tie loss
    get_ERGM(sessions, cutoff = c(0, 1))     # no subsampling
    
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
                       palette = RColorBrewer::brewer.pal(8, "Set3")) {

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

#' Get differential homophily ERGMs
#'
#' @param cutoff threshold parameters used to thin the network, as a vector of
#' two values between 0 and 1 used as quantile cutoff points against the edge
#' weights log-distribution, which is roughly normal in several instances.
get_ERGM <- function(sessions = 8:14, cutoff = c(.025, .975), base = FALSE, plot = FALSE, verbose = TRUE) {
  
  coefs = data.frame()       # plots
  betas = data.frame(b = NA) # table
  bics = c()  # BICs
  bbics = c() # baseline model BICs

  for(ch in c("se", "an")) {

    chamber = ifelse(ch == "an", "Assemblée nationale", "Sénat")
  
    for(file in paste0(ch, sessions)) {
    
      legid = gsub("\\D", "", file)
      title = paste(chamber, "legislature", legid)
    
      m = paste0("models/ergm/ergm_", cutoff[1], "_", cutoff[2], "_", file, ".rda")
      if(!file.exists(m)) {
      
        sink(gsub(".rda", ".log", m))
        msg("Modeling:", title, "cutoffs at", cutoff[1], "and", cutoff[2])
      
        data = paste0("data/", file, ".rda")
        if(!file.exists(data))
          load(paste0("data/bi_", file, ".rda"))
        else
          load(paste0("data/", file, ".rda"))
      
        net %v% "female" = net %v% "sexe" == "F"
        net %v% "seniority" = net %v% "nb_mandats"
      
        w = log(net %e% "wpc")
        q = c(-Inf, Inf)
        if(cutoff[1] > 0) q[1] = as.vector(quantile(w, probs = cutoff[1]))
        if(cutoff[2] < 1) q[2] = as.vector(quantile(w, probs = cutoff[2]))

        cutoffs = qplot(w, geom = "density") + 
          labs(title = paste0(title, "\n"))

        if(!all(is.infinite(q)))
          cutoffs = cutoffs +
            geom_vline(xintercept = q[ !is.infinite(q) ],
                       color = "grey25", linetype = "dashed")

        net %n% "data" = paste(net %n% "data", "within log-weight quantiles", cutoff[1], cutoff[2])
        net %n% "edge_bounds" = q
                
        q = which(w < q[1] | w > q[2])

        if(verbose)
          cat("Dropping:", length(q), "edges out of", length(w),
              round(100 * length(q) / length(w), 1), "% of sample\n")

        network::delete.edges(net, q)
                
        net %n% "edge_sample" = network.edgecount(net) / length(w)

        # merge appended sponsors to main groups
        s = net %v% "party"
        if(sum(s == "ECO", na.rm = TRUE) < 10 & any(s == "ECO")) {
          cat("Marking", sum(s == "ECO", na.rm = TRUE),
              "Green MPs as Socialists (n < 10)\n")
          s[ s == "ECO" ] = "SOC"
        }
        if(sum(s == "COM", na.rm = TRUE) < 10 & any(s == "COM")) {
          cat("Marking", sum(s == "COM", na.rm = TRUE),
              "Communist MPs as Socialists (n < 10)\n")
          s[ s == "COM" ] = "SOC"
        }
        network::set.vertex.attribute(net, "party", s)

        q = which(s == "SE")
        if(verbose)
          cat("Marking", length(q), "unaffiliated MPs as NA\n")

        net %v% "party" = ifelse(s == "SE", NA, s)
        print(table(net %v% "party", exclude = NULL))

        net %v% "rightwing" = as.numeric(net %v% "party" %in% c("DRO", "CEN", "FN"))

        cat("\n")
        print(net)

        ctrl = control.ergm(MCMLE.trustregion = 10^3,
                            MCMLE.maxit = 100, seed = 3258) # tryin 200, 2357

        # Poisson-reference ERGM
        ERGM = ergm(net ~ edges + mutual + 
                      absdiff("seniority") + 
                      nodematch("female") + 
                      nodematch("party", diff = TRUE) + 
                      absdiffcat("rightwing"),
                    control = ctrl)
                
        print(summary(ERGM))
        sink()
        
        save(ERGM, net, cutoffs, file = m)
        
        # baseline coefficients (saved but not logged)
        if(base) {
          message("Adding baseline model...")
          ERGM_base = ergm(net ~ edges + mutual + 
                             absdiff("seniority") + 
                             nodematch("female") + 
                             absdiffcat("rightwing"),
                           control = ctrl)

          save(ERGM_base, ERGM, net, cutoffs, file = m)
      
        }
    
      }
    
      load(m)
      
      f = summary(ERGM)$coefs[, 1:2]
      b = c("edges" = "Edges",
            "mutual" = "Reciprocity",
            "nodematch.female" = "same: Gender",
            "absdiff.seniority" = "diff: Seniority",
            "absdiff.rightwing.1" = "diff: Left-Right",
            "nodematch.party.FN"  = "Both FN",
            "nodematch.party.DRO" = "Both Conservatives",
            "nodematch.party.CEN" = "Both Centrists",
            "nodematch.party.RAD" = "Both Radicals",
            "nodematch.party.SOC" = "Both Socialists",
            "nodematch.party.COM" = "Both Communists",
            "nodematch.party.ECO" = "Both Greens")
      f$b = b[ rownames(f) ]
  
      # update BICs
      bics = cbind(bics, summary(ERGM)[['bic']])

      if(base)
        bbics = cbind(bbics, summary(ERGM_base)[['bic']])
      
      # update coefficients plot
      s = cbind(Chamber = chamber, Legislature = legid, Model = "Full", f)
      coefs = rbind(coefs, s)
  
      # update coefficients table
      names(f)[1] = paste0(ch, legid)
      names(f)[2] = paste0(ch, legid, "_se")
      betas = merge(betas, f, by = "b", all = TRUE)
  
      # add baseline coefficients
      if(base) {
    
        # go through summary
        f = summary(ERGM_base)$coefs[, 1:2]
        f$b = b[ rownames(f) ]
    
        # update coefficients plot
        s = cbind(Chamber = chamber, Legislature = legid, Model = "Baseline", f)
        coefs = rbind(coefs, s)
          
      }
      
      # diagnostics
      g = gsub(".rda", ".pdf", m)
      if(!file.exists(g) & plot)
        ggmcmc(ggs(as.mcmc.list(ERGM$sample)), file = g, plot = "density traceplot")
    
    }
  
  }

  # order table rows
  betas$b = factor(betas$b, levels = b[ !grepl("not reported", b)], ordered = TRUE)
  betas = betas[ !is.na(betas$b) , ]
  betas = betas[ order(as.numeric(betas$b)), ]

  # save coefficients
  names(betas) = toupper(names(betas))
  names(betas) = gsub("(.*)_SE", "SE", names(betas))
  bics = cbind(c("BIC (full model)", "BIC (baseline)"),
               rbind(unlist(lapply(bics, c, "")),
                     unlist(lapply(bbics, c, ""))
                     )
               )
  betas = as.data.frame(rbind(as.matrix(betas), bics), row.names = NA, stringsAsFactors = FALSE)
  betas[, -1] = apply(betas[, -1], 2, as.numeric)
  write.csv(betas, file = paste0("models/ergm/ergm_", cutoff[1], "_", cutoff[2], ".csv"))

  # reordering
  l = c("Both Conservatives", "Both Centrists", "Both Radicals", "Both Socialists", "Both Communists")
  l = c("Edges", "Reciprocity", "same: Gender", "diff: Seniority", "diff: Left-Right", l)
  coefs$b = factor(coefs$b, levels = l)

  # t-values
  names(coefs)[5] = "SE"
  coefs$t = coefs$SE / coefs$Estimate
  coefs = subset(coefs, !is.na(b) & abs(t) < 2/3) # drop Greens and FN (too few points) and v. large SEs

  # sort models
  coefs$Model = factor(coefs$Model, levels = c("Baseline", "Full"))

  # sort chambers
  coefs$Chamber = factor(coefs$Chamber, levels = c("Assemblée nationale", "Sénat"))

  # sort legislatures
  coefs$id = paste0(as.character(coefs$Legislature), substr(coefs$Model, 1, 1))
  coefs$id = factor(coefs$id, levels = unlist(lapply(8:14, paste0, c("B", "F"))))

  g = qplot(data = subset(coefs, !grepl("Both", b)),
            x = id, y = Estimate, 
            ymin = Estimate - 3 * SE, ymax = Estimate + 3 * SE, group = b, geom = "pointrange") + 
    scale_y_continuous(breaks = c(-6, -4, -2, -1, 0, 1, 2, 4, 6)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey75") +
    facet_grid(b ~ Chamber, scales = "free_y") +
    labs(y = "ERGM coefficient estimate ± 3 standard errors\n", x = "\nLegislature (1986-2014)") +
    coord_equal() +
    theme_grey(16) +
    theme(legend.position = "bottom",
          panel.grid = element_line(color = "white"),
          panel.background = element_rect(fill = "grey95"),
          strip.background = element_rect(fill = "grey85"),
          panel.margin = unit(.5, "cm"))

  if(base)
    g = g + aes(color = Model) + 
      scale_x_discrete(breaks = gsub("(.*)F", "", levels(coefs$id)),
                       labels = gsub("\\D", "", levels(coefs$id))) # +
      # scale_color_manual("Model", values = c("Full" = "black", "Baseline" = "grey50"))
  else
    g = g + scale_x_discrete(labels = sessions)
    
  ggsave(paste0("models/ergm/ergm_beta_", cutoff[1], "_", cutoff[2], ".pdf"),
         g, width = 9, height = 11)
    
  # differential homophily
  g = qplot(data = subset(coefs, Model == "Full" & grepl("Both", b)),
            x = Legislature, y = Estimate, color = b, group = b, 
            ymin = Estimate - 3 * SE, ymax = Estimate + 3 * SE,
            geom = "pointrange") + 
    scale_y_continuous(breaks = c(0, 2, 4, 6)) +
    geom_pointrange(alpha = .5, color = "grey25") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey75") +
    scale_color_manual("", values = c(
      "Both Communists" = "#E41A1C",   # (L; red)
      "Both Radicals" = "#FFFF33",     # (L; yellow)
      "Both Socialists" = "#F781BF",   # (or large L coalition; pink)
      "Both Centrists" = "#FF7F00",    # (R; orange)
      "Both Conservatives" = "#377EB8" # (R; blue)
      )) +
    facet_grid(b ~ Chamber) +
    labs(y = "ERGM coefficient estimate ± 3 standard errors\n", x = "\nLegislature (1986-2014)") +
    theme_grey(16) +
    theme(legend.position = "none",
          panel.grid = element_line(color = "white"),
          panel.background = element_rect(fill = "grey95"),
          strip.background = element_rect(fill = "grey85"),
          panel.margin = unit(.5, "cm"))
  
  ggsave(paste0("models/ergm/ergm_diff_", cutoff[1], "_", cutoff[2], ".pdf"),
         g, width = 9, height = 11)
  
}

# have a nice day
