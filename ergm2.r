setwd('/Users/fr/Documents/Code/R/neta')
library(network)
library(ggplot2)
library(grid)
library(ggmcmc)
library(ergm)
msg <- function(...) message(paste(...))

get_ERGM2 <- function(sessions = 8:14, cutoff = c(.025, .975), base = FALSE, plot = FALSE, verbose = TRUE) {
  
  coefs = data.frame()       # plots
  betas = data.frame(b = NA) # table

  bics = c()  # full model BICs
  nbics = c() # baseline model BICs
  cbics = c() # covariates model BICs

  for(ch in c("se", "an")) {

    chamber = ifelse(ch == "an", "Assemblée nationale", "Sénat")
  
    for(file in paste0(ch, sessions)) {
    
      legid = gsub("\\D", "", file)
      title = paste(chamber, "legislature", legid)
    
      m = paste0("models/ergm2/ergm_", cutoff[1], "_", cutoff[2], "_", file, ".rda")
      if(!file.exists(m)) {
      
        sink(gsub(".rda", ".log", m))
        msg("Modeling:", title, "cutoffs at", cutoff[1], "and", cutoff[2])
      
        data = paste0("data/", file, ".rda")
        if(!file.exists(data))
          load(paste0("data/bi_", file, ".rda"))
        else
          load(paste0("data/", file, ".rda"))
      
        net %v% "female" = as.numeric(net %v% "sexe" == "F")
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

        ctrl = control.ergm(MCMLE.trustregion = 10^3, seed = 3258) # MCMLE.maxit = 100, 

        # full model
        ERGM = ergm(net ~ edges + # size
                      mutual +    # reciprocality
                      gwesp + # GWESP
                      gwdsp(alpha = 1, fixed = TRUE) + # GWDSP
                      gwidegree(decay = 1, fixed = TRUE) +     # GWD
                      gwodegree(decay = 1, fixed = TRUE) +     # GWD
                      nodefactor("female") +                   # NF
                      nodematch("female") +                    # Uniform homoph.
                      nodecov("seniority") +                   # Nodal cov.
                      absdiff("seniority") +                   # absolute diff.
                      nodefactor("rightwing") +                # NF
                      absdiffcat("rightwing") +                # categ. diff.
                      nodefactor("party") +                    # NF
                      nodematch("party", diff = TRUE),         # Differential h.
                    control = ctrl, estimate = "MPLE")

        print(summary(ERGM))
        sink()

        save(ERGM, net, cutoffs, file = m)

        # baseline coefficients (saved but not logged)
        if(base) {
          message("Adding baseline model...")
          ERGM_net = ergm(net ~ edges + # size
                            mutual +    # reciprocality
                            gwesp + gwdsp(alpha = 1, fixed = TRUE) + # GWD
                            gwidegree(decay = 1, fixed = TRUE) +     # GWD
                            gwodegree(decay = 1, fixed = TRUE),      # GWD
                          control = ctrl)

          message("Adding covariates model...")
          ERGM_cov = ergm(net ~ edges + # size
                            mutual +    # reciprocality
                            gwesp + gwdsp(alpha = 1, fixed = TRUE) + # GWD
                            gwidegree(decay = 1, fixed = TRUE) +     # GWD
                            gwodegree(decay = 1, fixed = TRUE) +     # GWD
                            nodefactor("female") +                   # NF
                            nodecov("seniority") +                   # N. cov.
                            nodefactor("rightwing") +                # NF
                            nodefactor("party"),                     # NF
                          control = ctrl)

          save(ERGM_net, ERGM_cov, ERGM, net, cutoffs, file = m)

        }

      } else {

        load(m) ## debug
        msg("Loaded:", net %n% "chamber", "legislature", net %n% "legislature")

      }

      e = summary(ERGM)
      f = e$coefs[, 1:2]
      b = c("edges" = "Edges",
            "mutual" = "Reciprocity",
            "gwesp" = "GWESP",
            "gwdsp.fixed.1" = "GWDSP",
            "gwodegree" = "GWD(authors)",
            "gwidegree" = "GWD(cosponsors)",
            "nodefactor.female.1" = "NF female",
            "nodecov.seniority" = "NC seniority",
            "nodefactor.rightwing.1" = "NF rightwing",
            "nodefactor.party.COM" = "NF COM",
            "nodefactor.party.DRO" = "NF DRO",
            "nodefactor.party.RAD" = "NF RAD",
            "nodefactor.party.SOC" = "NF SOC",
            "nodefactor.party.CEN" = "NF CEN",
            "nodefactor.party.ECO" = "NF ECO",
            "nodefactor.party.FN" = "NF FN",
            "nodematch.female" = "Same gender",
            "absdiff.seniority" = "Diff. seniority",
            "absdiff.rightwing.1" = "Diff. left-right",
            "nodematch.party.FN"  = "Both FN",
            "nodematch.party.DRO" = "Both Conservatives",
            "nodematch.party.CEN" = "Both Centrists",
            "nodematch.party.RAD" = "Both Radicals",
            "nodematch.party.SOC" = "Both Socialists",
            "nodematch.party.COM" = "Both Communists",
            "nodematch.party.ECO" = "Both Greens")
      f$b = b[ rownames(f) ]
        
      # update BICs
      bics = cbind(bics, e[['bic']])
      
      if(base) {
        nbics = cbind(nbics, summary(ERGM_net)[['bic']])
        cbics = cbind(cbics, summary(ERGM_cov)[['bic']])
      }
      
      # update coefficients plot
      s = cbind(Chamber = chamber, Legislature = legid, Model = "Full", f)
      coefs = rbind(coefs, s)
        
      # update coefficients table
      names(f)[1] = paste0(ch, legid)
      names(f)[2] = paste0(ch, legid, "_se")
      betas = merge(betas, f, by = "b", all = TRUE)
        
      # # add baseline coefficients
      # if(base) {
      #     
      #   # go through summary
      #   f = summary(ERGM_base)$coefs[, 1:2]
      #   f$b = b[ rownames(f) ]
      #     
      #   # update coefficients plot
      #   s = cbind(Chamber = chamber, Legislature = legid, Model = "Baseline", f)
      #   coefs = rbind(coefs, s)
      #     
      # }
      
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
                     unlist(lapply(nbics, c, "")),
                     unlist(lapply(cbics, c, ""))
                     )
               )
  betas = as.data.frame(rbind(as.matrix(betas), bics),
                        row.names = NA, stringsAsFactors = FALSE)
  betas[, -1] = apply(betas[, -1], 2, as.numeric)

  write.csv(betas, file = paste0("models/ergm2/ergm_", cutoff[1], "_", cutoff[2], ".csv"))

  # network structure controls
  l = c("Edges", "Reciprocity", "GWD(authors)", "GWD(cosponsors)", "GWDSP", "GWESP")
  # homophily and differences
  l = c(l, "Same gender", "Diff. seniority", "Diff. left-right", "Both FN", "Both Conservatives", "Both Centrists", "Both Radicals", "Both Socialists", "Both Greens", "Both Communists")
  # nodal factor main effects
  l = c(l, "NF female", "NC seniority", "NF rightwing", "NF FN", "NF DRO", "NF CEN", "NF RAD", "NF SOC", "NF ECO", "NF COM")
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
  
  #
  #
  #
  g = qplot(data = subset(coefs, grepl("^(NF|NC)", b)),
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
  g = g + scale_x_discrete(labels = sessions)
  ggsave(paste0("models/ergm2/ergm_node_", cutoff[1], "_", cutoff[2], ".pdf"),
         g, width = 9, height = 11)
         #
         #
         #
  g = qplot(data = subset(coefs, grepl("^(Edges|GW)", b)),
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
  g = g + scale_x_discrete(labels = sessions)
  ggsave(paste0("models/ergm2/ergm_ctrl_", cutoff[1], "_", cutoff[2], ".pdf"),
         g, width = 9, height = 11)
  #
  #
  #
  g = qplot(data = subset(coefs, !grepl("^(Edges|Both|GW|NF|NC)", b)),
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
    
  ggsave(paste0("models/ergm2/ergm_beta_", cutoff[1], "_", cutoff[2], ".pdf"),
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
      "Both Communists" = "#E41A1C",    # (L; red)
      "Both Greens" = "#4DAF4A", # (L; green)
      "Both Radicals" = "#FFFF33",      # (L; yellow)
      "Both Socialists" = "#F781BF",    # (or large L coalition; pink)
      "Both Centrists" = "#FF7F00",     # (R; orange)
      "Both Conservatives" = "#377EB8", # (R; blue)
      "Both FN" = "#A65628" # (R; brown)
      )) +
    facet_grid(b ~ Chamber) +
    labs(y = "ERGM coefficient estimate ± 3 standard errors\n", x = "\nLegislature (1986-2014)") +
    theme_grey(16) +
    theme(legend.position = "none",
          panel.grid = element_line(color = "white"),
          panel.background = element_rect(fill = "grey95"),
          strip.background = element_rect(fill = "grey85"),
          panel.margin = unit(.5, "cm"))
  
  ggsave(paste0("models/ergm2/ergm_diff_", cutoff[1], "_", cutoff[2], ".pdf"),
         g, width = 9, height = 11)
  
}

get_ERGM2(sessions = 8:11)
# get_ERGM2(sessions = 8:14, cutoff = c(0, 1))
