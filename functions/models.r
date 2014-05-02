#' Makefile call
#'
get_models <- function(key, sessions = 8:14) {
  
  if(key == "ergmm") {

    # latent space cluster models
    get_ERGMM("an", sessions)
    get_ERGMM("se", sessions)

  } else if(key == "ergm") {
    
    # exponential random graph model at threshold (.025, .975) ~ 5% tie loss
    get_ERGM(sessions)
    
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
get_ERGMM <- function(ch, sessions = 8:14) {

  stopifnot(ch %in% c("an", "se"))  
  load(paste0("data/", ch, ".rda"))

  R = paste0("ergmm/", ch)
  N = ls(pattern = "^net[0-9]+")
  N = sort(as.numeric(gsub("\\D", "", N)))
  N = N [ N %in% sessions ]

  chamber = ifelse(ch == "an", "Assemblée nationale", "Sénat")
  message(paste("Parsing:", chamber, length(N), "legislatures"))

  for(i in N) {

    file = paste0("models/", R, i, ".rda")
    name = paste0(R, i)
    title = paste(chamber, "legislature", gsub("\\D", "", i))

    if(!file.exists(file)) {

      net = get(paste0("net", i))
      t = table(net %v% "party")
      
      # ignore small-n and 'SE'
      g = length(t[ t > 5 & names(t) != "SE" ])

      message(paste("Modeling:", title, g, "groups"))

      # be patient for a few days
      ERGMM = ergmm(net ~ euclidean(d = 2, G = g) + rreceiver,
                    response = "count", family = "Poisson.log",
                    control = ergmm.control(burnin = 10^3,
                                            sample.size = 2000,
                                            interval = 5),
                    verbose = TRUE)

      save(ERGMM, file = file)
      
    } else{
      
      plot_ERGMM(paste0(ch, i))

    }

  }

}

#' Draw a circle in ggplot2
#'
#' @source \url{http://stackoverflow.com/a/6863490/635806}
circle <- function(center = c(0, 0),diameter = 1, npoints = 100) {
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

#' Plot latent space conditional posterior means, traceplots and iterations
#'
#' @source hacked from \code{plot.ergmm} in the \code{latentnet} package
#' (version 2.5.1, 2014-02-17) by Pavel N. Krivitsky, Mark S. Handcock, 
#' Susan M. Shortreed, Jeremy Tantrum and Peter D. Hoff
plot_ERGMM <- function(file = "(an|se)[0-9]+", update = FALSE,
                       palette = brewer.pal(8, "Set3")) {

  stopifnot(all(grepl("^(an|se)", file)))

  objects = dir("models/ergmm/", pattern = paste0(file, ".rda"))
  objects = paste0("models/ergmm/", objects)

  for(i in objects) {

    chamber = ifelse(grepl("/an", i), "Assemblée nationale", "Sénat")
    title = paste(chamber, "legislature", gsub("\\D", "", i))
    message(paste("Plotting:", title))

    load(i)
    plots = gsub("^models/ergmm", "plots", i)
  
    # conditional posterior means (to plots folder)

    p = gsub(".rda", "_ergmm.pdf", plots)
    if(!file.exists(p) | update) {

      Yg = ERGMM[["model"]][["Yg"]]
      n = network.size(Yg)
      G  = ERGMM[["model"]][["G"]]

      cat("Plotting:", n, "nodes", G, "groups...")

      summ <- summary(ERGMM, point.est = "pmean")
      Z.pos  <- summ[["pmean"]][["Z"]]
      summ   <- summ[["pmean"]]
      Z.mean <- summ[["Z.mean"]]
      Z.var  <- summ[["Z.var"]]
      Z.K    <- summ[["Z.K"]]

      # color by cluster membership
    
      if(is.latent.cluster(ERGMM)) {
        vertex.col <- palette[ Z.K ]
      } else {
        vertex.col <- palette[1]
      }
  
      vertex.col <- rep(vertex.col, length.out = n)

      # size by receiver random effects

      vertex.cex = 1
      if(ERGMM[["model"]][ "receiver" ][[1]]) {
        rand.eff.mul =  exp( summ[ "receiver" ][[1]] )
        rand.eff.mul = sqrt( rand.eff.mul )
        vertex.cex = vertex.cex * rand.eff.mul
      }
  
      vertex.cex = rep(vertex.cex, length.out = n)
  
      # zoom region
      Yg %v% "lon" = Z.pos[, 1]
      Yg %v% "lat" = Z.pos[, 2]

      # table of node memberships
    
      k = as.numeric(factor(vertex.col))
      t = table(Yg %v% "party", k)
      t = cbind(t, togetherness = round(100 * apply(t, 1, max) / rowSums(t), 1))

      write.csv(t, file = gsub("ergmm/", "", gsub(".pdf", ".csv", p)), row.names = TRUE)
    
      # plot conditional posterior means
    
      mu = lapply(1:nrow(Z.mean), function(x) { 
        return(data.frame(k = x, circle(Z.mean[x, ], diameter = Z.var[x])))
      })
      mu = rbind.fill(mu)
    
      g = ggnet(Yg, mode = "geo", size = 0, geo.outliers = FALSE, arrow.size = 1/3) +
        geom_point(aes(size = 3 + vertex.cex, color = Yg %v% "party"), alpha = .75) +
        geom_text(aes(size = vertex.cex * .5, label = factor(k)), color = "white", alpha = .75) +
        geom_path(data = mu, aes(group = k, x = x, y = y), alpha = 1) +
        scale_fill_brewer(expression(Z[K]), palette = "Set3") +
        scale_color_manual("", values = Yg %n% "party_colors") +
        scale_size_area(max_size = 9) + 
        guides(size = FALSE, fill = FALSE,
               color  = guide_legend(override.aes = list(size = 6))) + 
        labs(y = expression(Z[2]), x = expression(Z[1])) +
        coord_equal()
  
      ggsave(p, g, width = 9, height = 9)
      cat("done.\n")

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
get_ERGM <- function(sessions = 8:14, cutoff = c(.025, .975), verbose = TRUE, update = FALSE) {
  
  coefs = dir( "models/ergm", pattern = paste0(cutoff[1], "_", cutoff[2]) )
  if(length(coefs) > 0 & !update) {
    
    if(verbose)
      message("Set update to FALSE to refresh an existing model.")
    
  } else {
    
    coefs = data.frame()
    for(ch in c("an", "se")) {

      chamber = ifelse(ch == "an", "Assemblée nationale", "Sénat")
    
      for(file in paste0(ch, sessions)) {
      
        title = paste(chamber, "legislature", gsub("\\D", "", file))
      
        m = paste0("models/ergm/ergm_", cutoff[1], "_", cutoff[2], "_", file, ".rda")
        if(!file.exists(m)) {
        
          sink(gsub(".rda", ".log", m))
          message(paste("Modeling:", title, "cutoffs at", cutoff[1], "and", cutoff[2]))
        
          data = paste0("data/", file, ".rda")
          if(!file.exists(data))
            load(paste0("data/bi_", file, ".rda"))
          else
            load(paste0("data/", file, ".rda"))
        
          net %v% "party" = ifelse(net %v% "party" == "SE", NA, net %v% "party")
          net %v% "rightwing" = as.numeric(net %v% "party" %in% c("DRO", "CEN", "FN"))
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

          if(verbose)
            print(cutoffs)
        
          net %n% "data" = paste(net %n% "data", "(sample edges)")
          net %n% "edge_cutoff" = q
        
          q = which(w < q[1] | w > q[2])
          cat("Dropping:", length(q), "edges out of", length(w),
              round(100 * length(q) / length(w), 1), "% of sample\n")
          delete.edges(net, q)
        
          net %n% "edge_sample" = network.edgecount(net) / length(w)
                
          # remove small parties
          s = which(net %v% "party" %in% c("ECO", "COM"))
          cat("Dropping: small parties (COM, ECO, n =", length(s), "MPs)\n\n")
          delete.vertices(net, s)
        
          print(net)
        
          # Poisson-reference ERGM
          ERGM = ergm(net ~ edges + 
                        mutual + 
                        absdiff("seniority") + 
                        nodematch("female") + 
                        nodematch("party", diff = TRUE) + 
                        absdiffcat("rightwing"),
                      control = control.ergm(MCMLE.trustregion = 1000)
          )
        
        
          print(summary(ERGM))
          sink()
        
          save(ERGM, net, cutoffs, file = m)
        
        }
      
        load(m)
        s = summary(ERGM)[['coefs']][-3]
        s = cbind(Chamber = chamber, Legislature = gsub("\\D", "", file), b = rownames(s), s)
        coefs = rbind(coefs, s)
      
        # mcmc.diagnostics(fit)
      
      }
    
    }
  
    # renaming
    coefs$b = as.character(coefs$b)
    coefs$b [ coefs$b == "edges" ] = "Edges"
    coefs$b [ coefs$b == "mutual" ] = "Reciprocity"
    coefs$b [ coefs$b == "absdiff.seniority" ] = "diff: Seniority"
    coefs$b [ coefs$b == "nodematch.female" ] = "same: Gender"
    coefs$b [ coefs$b == "nodematch.party.CEN" ] = "Both Centrists"
    coefs$b [ coefs$b == "nodematch.party.DRO" ] = "Both Rightwing"
    coefs$b [ coefs$b == "nodematch.party.SOC" ] = "Both Socialists"
    coefs$b [ coefs$b == "nodematch.party.RAD" ] = "Both Radicals"
    coefs$b [ coefs$b == "nodematch.party.FN" ] = "single estimate, not shown"
    coefs$b [ coefs$b == "absdiff.rightwing.1" ] = "diff: Left-Right"
  
    # reordering
    l = unique(coefs$b)[ grepl("Both", unique(coefs$b)) ]
    l = c("Edges", "Reciprocity", "same: Gender", "diff: Seniority", "diff: Left-Right", l)
    coefs$b = factor(coefs$b, levels = l)
    names(coefs)[5] = "SE"
    names(coefs)[6] = "p"
    coefs$t = coefs$SE / coefs$Estimate
  
    # dropping large SEs
    coefs = subset(coefs, !is.na(b) & t < .5 & t > -.5)
  
    # coefficients
    g = qplot(data = subset(coefs, !grepl("Both|Edges", b)),
              x = Legislature, y = Estimate,
              ymin = Estimate - 3 * SE, ymax = Estimate + 3 * SE,
              group = b, color = I("grey25"), geom = "pointrange") + 
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey75") +
      facet_grid(b ~ Chamber, scales = "free_y") +
      labs(y = "Estimate\n", x = "\nLegislature") +
      coord_equal() +
      theme_grey(16) +
      theme(legend.position = "none",
            panel.grid = element_blank(),
            panel.background = element_rect(fill = "grey95"),
            strip.background = element_rect(fill = "grey85"),
            panel.margin = unit(.5, "cm"))
  
    if(verbose)
      print(g)

    ggsave(paste0("models/ergm_beta_", cutoff[1], "_", cutoff[2], ".pdf"),
           g, width = 9, height = 9)
  
    # differential homophily
    g = qplot(data = subset(coefs, grepl("Both", b) & Estimate + 3 * SE < 5),
              x = Legislature, y = Estimate,
              ymin = Estimate - 3 * SE, ymax = Estimate + 3 * SE,
              group = b, color = I("grey25"), geom = "pointrange") + 
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey75") +
      facet_grid(b ~ Chamber) +
      labs(y = "Estimate\n", x = "\nLegislature") +
      theme_grey(16) +
      theme(legend.position = "none",
            panel.grid = element_blank(),
            panel.background = element_rect(fill = "grey95"),
            strip.background = element_rect(fill = "grey85"),
            panel.margin = unit(.5, "cm"))
  
    if(verbose)
      print(g)

    ggsave(paste0("models/ergm_diff_", cutoff[1], "_", cutoff[2], ".pdf"),
           g, width = 9, height = 9)
    
  }
  
}

# have a nice day