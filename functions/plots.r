#' Grid alignment for plots
#' 
#' @source \url{http://stackoverflow.com/a/9491019/635806}
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

#' Plot raw counts of amendments and bills
#' 
#' Colored from blue to red.
#' @param type subset to a specific type of legislation (defaults to all items)
plot_data <- function(x, type = "all", verbose = TRUE) {

  stopifnot(file.exists(paste0("data/", x, ".rda")))
  load(paste0("data/", x, ".rda"))

  data = get(ifelse(nchar(x) > 2, ifelse(grepl("am", x), "amendments", "bills"),
             "legislation"))

  if(type != "all")
    data = data[ data$type == type, ]

  data = na.omit(data[, c("uid", "legislature", "date", "t", "government", "sample") ])
  data = data[ !data$government, ] # keep MP legislation, incl. single-authored

  # legislature floor dates
  years = c("1986-03-16", "1988-06-05", "1993-03-21", "1997-05-25", "2002-06-09", "2007-06-10", "2012-06-10")
  data$tmin = as.Date( years[ data$legislature - 7 ] )
  data$date = as.Date( data$date )
  data$date[ with(data, date < tmin) ] = data$tmin[ with(data, date < tmin) ]
  data$t = as.numeric( with(data, date - tmin) )
  data$legislature = factor(data$legislature, levels = 8:14, ordered = TRUE)

  print(summarise(group_by(data, legislature),
          n = length(uid),
          min = year(min(date, na.rm = TRUE)),
          mid = year(mean(date, na.rm = TRUE)),
          max = year(max(date, na.rm = TRUE)),
          t = max(t, na.rm = TRUE)
          )
        )
  
  # number of authors per item
  t = summarise(group_by(sponsorships, uid), n_authors = length(name))
  data = left_join(data, t, by = "uid")

  # single-authored legislation
  data$n_authors[ !data$sample | is.na(data$n_authors) ] = 1

  # manual breaks
  b = c(0, 1, 2, 5, 10, 25, 50, 100, 250, 500, Inf)
  data$q_authors = cut(data$n_authors, breaks = b, include.lowest = TRUE)

  print(summarise(group_by(data, q_authors),
          n = length(n_authors),
          min = min(n_authors, na.rm = TRUE),
          med = median(n_authors, na.rm = TRUE),
          max = max(n_authors, na.rm = TRUE)
          )
        )
  
  # raw counts
  counts = summarise(group_by(data, legislature, t = t %/% 30, q_authors),
                     n = length(t))
                     
  v = rev(brewer.pal(length(levels(counts$q_authors)), "RdYlBu"))
  l = c("1", "2", "3-5", "6-10", "11-25", "26-50", "51-100", "101-250", "251-500", "> 500")
  g = qplot(data = counts, x = t, y = n, fill = factor(q_authors), 
            position = "stack", stat = "identity", geom = "bar") + 
      scale_fill_manual("Sponsors ", values = v, na.value = "grey75",
                        labels = paste0(" ", l, " ")) +
      scale_x_continuous(breaks = 12 * 0:5, labels = 0:5, limits = c(0, 60)) +
      facet_grid(legislature ~ ., scales = "free_y") +
      labs(y = "Number of items", x = "Year since start of legislature") +
      theme_grey(16) + 
      theme(
        panel.border = element_rect(fill = NA, color = "white"),
        panel.margin = unit(2, "lines"),
        panel.grid.major.x = element_line(color = "white"),
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        plot.title = element_text(face = "bold", vjust = 1),
        axis.title.y = element_text(angle = 90, vjust = -.25),
        axis.title.x = element_text(vjust = -1),
        legend.key = element_rect(color = "white"),
        legend.key.size = unit(16, "pt"), 
        legend.position = "bottom", 
        legend.margin = unit(32, "pt"),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 16, face = "plain"),
        strip.background = element_rect(fill = NA, color = "white"),
        strip.text = element_text(size = 16, face = "plain")
    )
  
  ggsave(paste0("plots/counts/", ifelse(nchar(x) < 3, "full_", ""),
                ifelse(type != "all", paste0(substr(type, 1, 2), "_"), ""),
         x, ".pdf"), g, width = 12, height = 11)

}

#' Plot a weighted cosponsorship network
#' 
#' @keywords paper draft
plot_network <- function(net, mode = "kamadakawai", file = NULL,
                           size = 1, max_size = 6, alpha = .5,
                           width = 11, height = 9) {
    
  v = net %n% "party_colors"
  b = net %n% "party_order"
  
  sizer = scale_size_area(name = expression(log(C[B])), max_size = max_size, 
                          breaks = seq(2.5, 10, by = 2.5),
                          labels = gsub("\\.0", "", as.character(seq(2.5, 10, by = 2.5), 1)))
  guide = guides(color = guide_legend(override.aes = list(size = max_size)))
  color = scale_color_manual("", values = v, breaks = b)
  clean = theme(text = element_text(size = 24),
                legend.key = element_rect(colour = "white", fill = NA),
                legend.key.size = unit(24, "pt"))
  
  g = suppressMessages(ggnet(net, mode = mode,
                             node.group = net %v% "party",
                             node.color = net %n% "party_colors",
                             segment.color = factor(colors[ nodes[ net %e% "source", "party" ] ]),
                             segment.alpha = .5, size = 1) +
                         geom_point(aes(size = log(net %v% "betweenness")),
                                    alpha = .75) +
                         geom_rug(sides = "lrtb", size = .5) +
                         coord_equal() +
                         guide +
                         sizer + 
                         color +
                         clean)
  
  if(!is.null(file)) ggsave(file, g, width = width, height = height)
  return(g)
  
}

#' Plot legislation outcomes
#'
#' @keywords paper draft
plot_outcomes <- function(x, type = "all", verbose = TRUE) {

  stopifnot(file.exists(paste0("data/", x, ".rda")))
  load(paste0("data/", x, ".rda"))

  data = get(ifelse(nchar(x) > 2, ifelse(grepl("am", x), "amendments", "bills"),
             "legislation"))

  if(type != "all")
    data = data[ data$type == type, ]

  data = na.omit(data[, c("uid", "legislature", "date", "t", "sort", "sample") ])
  
  # legislature floor dates
  years = c("1986-03-16", "1988-06-05", "1993-03-21", "1997-05-25", "2002-06-09", "2007-06-10", "2012-06-10")
  data$tmin = as.Date( years[ data$legislature - 7 ] )
  data$date = as.Date( data$date )
  data$date[ with(data, date < tmin) ] = data$tmin[ with(data, date < tmin) ]
  data$t = as.numeric( with(data, date - tmin) )
  data$legislature = factor(data$legislature, levels = 8:14, ordered = TRUE)

  if(verbose)
    print(table(year(data$date), data$legislature))
  
  # raw counts of outcomes
  counts = summarise(group_by(data, legislature, t = t %/% 30, sort),
                     n = length(sort))

  counts$sort[ grepl("^Adopt", counts$sort) ] = "Adopted"
  counts$sort[ grepl("^Rejet", counts$sort) ] = "Rejected"
  counts$sort[ !grepl("ted$", counts$sort) ] = "Withdrawn" # residual category
  counts$sort = factor(counts$sort, levels = c("Adopted", "Rejected", "Withdrawn"))

  # pad levels for bottom plot legend
  levels(counts$sort) = paste0(levels(counts$sort), " ")
  
  v = brewer.pal(5, "Set3")[ c(1, 4, 5) ]
  g = qplot(data = counts, x = t, y = n, fill = factor(sort),
            position = "stack", stat = "identity", geom = "bar") + 
            # scale_fill_brewer(palette = "Set2") +
      scale_fill_manual("Outcome ", values = v, na.value = "grey75") +
      scale_x_continuous(breaks = 12 * 0:5, labels = 0:5, limits = c(0, 60)) +
      facet_grid(legislature ~ ., scales = "free_y") +
      labs(y = "Number of items", x = "Year since start of legislature") +
      theme_grey(16) + 
      theme(
        panel.border = element_rect(fill = NA, color = "white"),
        panel.margin = unit(2, "lines"),
        panel.grid.major.x = element_line(color = "white"),
        panel.grid.minor.x = element_blank(),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        plot.title = element_text(face = "bold", vjust = 1),
        axis.title.y = element_text(angle = 90, vjust = -.25),
        axis.title.x = element_text(vjust = -1),
        legend.key = element_rect(color = "white"),
        legend.key.size = unit(16, "pt"), 
        legend.position = "bottom", 
        legend.margin = unit(32, "pt"),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 16, face = "plain"),
        strip.background = element_rect(fill = NA, color = "white"),
        strip.text = element_text(size = 16, face = "plain")
    )
  
  ggsave(paste0("plots/",
                ifelse(type != "all", paste0(substr(type, 1, 2), "_"), ""),
         x, "_outcomes.pdf"), g, width = 12, height = 11)
  
}

#' Plot mandates
#'
#' @keywords paper draft
plot_mandates <- function(data, colors) {
  
  data$mandat_debut = parse_date_time(substr(data$mandat, 1, 10), "%d/%m/%Y")
  data$mandat_fin = parse_date_time(substr(data$mandat, 14, 23), "%d/%m/%Y")
print(head(data))

  d = data %.%
    filter(!is.na(mandat_debut), !is.na(mandat_fin)) %.%
    arrange(mandat_debut, mandat_fin)
  
  g = qplot(data = d, 
            y = reorder(nom_de_famille, mandat_debut), yend = reorder(nom_de_famille, mandat_debut),
            x = mandat_debut, xend = mandat_fin, color = party, 
            geom = "segment") + 
    theme(axis.ticks.y = element_blank()) +
    scale_color_manual("", values = colors) + 
    labs(y = NULL, x = NULL)
  
  return(g)
  
}

#' Plot example egocentric networks by selecting random sponsors in three groups
#'
#' @keywords paper
plot_egos <- function(x, update = FALSE, which = NULL, root = "paper/figures/") {

  file = paste0(root, "egos_", gsub("data/|.rda", "", x), ".pdf")
  if(!file.exists(file) | update) {

    load(x)
    u = ego.extract(net)

    com = subset(nodes, party == "COM")$nom
    com = sample(com, 2)
    udf = subset(nodes, party == "CEN")$nom
    udf = sample(udf, 2)
    dro = subset(nodes, party == "DRO")$nom
    dro = sample(dro, 2)
    soc = subset(nodes, party == "SOC")$nom
    soc = sample(soc, 2)
    els = subset(nodes, party %in% c("RAD", "ECO", "FN", "SE"))$nom
    els = sample(els, 1)
    
    if(!is.null(which)) {

      com = which[[ "com" ]][ 1:2 ]
      udf = which[[ "udf" ]][ 1:2 ]
      dro = which[[ "dro" ]][ 1:2 ]
      soc = which[[ "soc" ]][ 1:2 ]
      els = which[[ "els" ]][ 1 ]

    }

    plot_ego <- function(x, sponsors = nodes, ...) {
      n = network(u[[ which(names(u) == x) ]])
      n %v% "id"  = network.vertex.names(n)
      n %v% "ego" = (n %v% "id" == x)
      n %v% "party" = sponsors[ n %v% "id", "party" ]
      k = net %n% "party_colors"
      k = k [ names(k) %in% unique(n %v% "party") ]
      n %n% "colors" = k

      msg(x, network.size(n), "nodes")

      g = ggnet(n, mode = "kamadakawai", node.group = n %v% "party", node.color = n %n% "colors",
                size = 6, segment.color = "grey75", segment.alpha = .5, ...) +
        geom_point(size = 3) +
        geom_point(color = k [ sponsors[ x, "party" ] ], size = 12 * as.numeric(n %v% "ego")) +
        geom_point(color = "black", alpha = .5, size = 12 * as.numeric(n %v% "ego")) +
        geom_point(color = "white", size = 10 * as.numeric(n %v% "ego")) +
        geom_point(color = k [ sponsors[ x, "party" ] ], size = 8 * as.numeric(n %v% "ego")) +
        ggtitle(x) +
        guides(size = FALSE)
      return(g)
      
    }
    
    pdf(file, width = 20, height = 20)
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(3, 3)))
    print(plot_ego(com[1]), vp = vplayout(1, 1))
    print(plot_ego(com[2]), vp = vplayout(1, 2))
    print(plot_ego(udf[1]), vp = vplayout(1, 3))
    print(plot_ego(udf[2]), vp = vplayout(2, 1))
    print(plot_ego(dro[1]), vp = vplayout(2, 2))
    print(plot_ego(dro[2]), vp = vplayout(2, 3))
    print(plot_ego(soc[1]), vp = vplayout(3, 1))
    print(plot_ego(soc[2]), vp = vplayout(3, 2))
    print(plot_ego(els[1]), vp = vplayout(3, 3))
    dev.off()
    
  }
  
}

#' Plot top fifty legislation sponsors in the last two legislatures
#'
#' @keywords paper draft
plot_sponsorships <- function(file, sessions = 13:14) {

  fig = gsub("data/", "paper/figures/sponsorships_", file)
  fig = gsub(".rda", ".pdf", fig)

  if(!file.exists(fig)) {

    load(file)
    
    g = lapply(sessions, function(i) {
      
      x = legislation[ legislation$legislature == i, "uid" ]
      y = sponsorships[ sponsorships$uid %in% x, ]
      z = get(paste0("nodes", i))
      z = z[ sponsors$nom %in% unique(y$name), ]
      
      z$sponsorships = as.vector(table(subset(y, status == "author")$name)[ z$nom ])
      z$sponsorships[ is.na(z$sponsorships) ] = 0
      
      z$cosponsorships = as.vector(table(subset(y, status == "cosponsor")$name)[ z$nom ])
      z$cosponsorships[ is.na(z$cosponsorships) ] = 0
      
      z$total = z$sponsorships + z$cosponsorships
      
      top = z[ order(-z$total)[1:50], c("nom_de_famille", "sponsorships", "cosponsorships", "total", "party"), ]
      #top$nom = with(top, factor(top$nom, levels = reorder(nom, total)))
      
      g = qplot(data = top, x = reorder(nom_de_famille, total), y = total, fill = party, width = 1,
                stat = "identity", alpha = "total", geom = "bar") + 
        geom_bar(aes(y = cosponsorships, alpha = "cosponsored"), stat = "identity") + 
        scale_alpha_manual("Amendements", 
                           values = c("total" = 1/4, "cosponsored" = 3/4), 
                           labels = c("co-signés", "tous")) +
        scale_fill_manual("", values = get(paste0("colors", i))) +
        coord_flip() + 
        guides(alpha = FALSE) +
        theme_minimal(20) + 
        theme(legend.position = "none") +
        labs(x = NULL, y = NULL)
      
      return(g)
      
    })
    
    pdf(fig, width = 15, height = 20)
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(1, 2)))
    print(g[[1]], vp = vplayout(1, 1))
    print(g[[2]], vp = vplayout(1, 2))
    dev.off()
    
  }
  
}

#' Plot party-level measures
#'
#' @keywords paper
plot_groups <- function(sessions = 8:14) {

  if(length(dir("paper/figures", "^party(.*).pdf")) < 2) {
    
    colors = c(
      "COM" = "#E41A1C", # Communists (leftwing, red)
      "ECO" = "#4DAF4A", # Greens (leftwing, green)
      "RAD" = "#FFFF33", # Radicals (leftwing, yellow)
      "SOC" = "#F781BF", # Socialists (or Leftwing coalition, pink)
      "SE" = "#999999",  # Unaffiliated/Unknown (grey)
      "CEN" = "#FF7F00", # Centrists (RI, UDF, UDI, NC...; rightwing, orange)
      "DRO" = "#377EB8", # Rightwing (Gaullists, RPR + DL, UMP; blue)
      "FN" = "#A65628"   # Front National (extreme-right, brown)
    )

    party_measures <- function(x, ch, update = TRUE, weights = "wpc") {
  
      data = paste0("data/", ch, x, ".rda")
      if(file.exists(data))
        load(data)
      else
        load(paste0("data/bi_", ch, x, ".rda"))
  
      t = table(nodes$party)
      nodes$n_au = as.vector(nodes$n_au)
      nodes$n_co = as.vector(nodes$n_co)
      nodes$total = with(nodes, log10(n_au + n_co))
      nodes$n = t[ nodes$party ]
      nodes$hf = as.numeric(nodes$sexe == "F")
      nodes$age = c(1986, 1988, 1993, 1997, 2002, 2007, 2012)[ x - 7 ]
      nodes$age = with(nodes, age - annee_naissance)
      nodes$senior = nodes$nb_mandats > 1
      nodes$noob = nodes$nb_mandats == 1
      if(x == 14) nodes$reelected = NA

      nodes = nodes[, c("party", "n", "senior", "noob", "age", "hf", "total", "degree", "distance", "clustering", "betweenness", "closeness", "constraint") ]
      names(nodes) = c("party", "N", "Senior MPs", "New MPs", "Age", "Female", "log10(Sponsorships)", "Centralization", "Distance", "Clustering", "Betweenness", "Closeness", "Constraint")
  
      return(data.frame(chamber = ifelse(ch == "an", "Assemblée nationale", "Sénat"),
                        Legislature = x, nodes, stringsAsFactors = FALSE))
  
    }

    se = rbind.fill(lapply(sessions, party_measures, ch = "se"))
    an = rbind.fill(lapply(sessions, party_measures, ch = "an"))
    da = melt(rbind(an, se), c("chamber", "Legislature", "party"))

    da = subset(da, !is.na(value) & !is.nan(value) & !is.infinite(value))
    g = qplot(data = subset(da, !(party %in% c("SE", "FN", "ECO"))),
          x = Legislature, y = value, group = party, fill = party, color = party,
          stat = "summary", fun.y = "mean", geom = c("line", "point")) +
      scale_x_continuous(breaks = sessions) +
      scale_colour_manual("", values = colors) +
      scale_fill_manual("", values = colors) +
      facet_wrap(~ variable, ncol = 3, scales = "free_y") +
      labs(y = NULL, x = "\nLegislature (1986-2014)") +
      theme_grey(16) +
      coord_equal() +
      theme(legend.position = "bottom", panel.grid = element_blank())

    ggsave("paper/figures/party_measures.pdf", g, width = 10, height = 9)

    da$data = ifelse(da$Legislature < 11, "1986-1997 (sparse networks)", "1997-2014 (dense networks)")
    g = qplot(data = subset(da, variable == "Clustering" & !(party %in% c("SE"))), x = Legislature, y = value,
              fill = party, color = party, group = party, 
              stat = "summary", fun.y = "mean",
              geom = c("point", "line")) +
      scale_x_continuous(breaks = sessions) +
      scale_y_continuous(lim = c(0, 1)) +
      scale_colour_manual("", values = colors) +
      scale_fill_manual("", values = colors) +
      facet_grid(chamber ~ data, scales = "free_x") +
      labs(y = "Clustering (weighted transitivity)\n", x = "\nLegislature (1986-2014)") +
      theme_grey(16) +
      coord_equal() +
      theme(legend.position = "bottom", panel.grid = element_blank())
  
    ggsave("paper/figures/party_clustering.pdf", g, width = 9, height = 9)
    
  }
  
}

#' Plot network statistics
#'
#' @keywords paper
plot_attributes <- function(type = "all", sessions = 8:14) {
  
  if(sessions[1] == "yr") {

    sessions = dir("data", pattern = "(an|se)[0-9]{1,2}.[0-9]{4}.rda")
    sessions = table(str_extract(sessions, "\\d+\\.\\d+"))
    sessions = names(sessions)[ sessions > 1 ]

  }
  
  type = ifelse(type == "all", "", paste0(type, "_"))
  se = rbind.fill(lapply(sessions, get_measures, ch = paste0(type, "se")))
  an = rbind.fill(lapply(sessions, get_measures, ch = paste0(type, "an")))
  da = melt(rbind(an, se), c("chamber", "Legislature"))

  # mark amendments series
  dots_an = subset(da, chamber == "Assemblée nationale" & as.integer(Legislature) > 11)
  dots_se = subset(da, chamber == "Sénat" & as.integer(Legislature) > 10)

  # mark A10 as faulty
  da$faulty = with(da, chamber == "Assemblée nationale" & Legislature == 10 & variable %in% c("Centralization", "Distance", "Global.Clustering", "Modularity", "Modularity.Max", "Modularity.Ratio", "Betweenness", "Local.Clustering", "Constraint"))

  labels = sessions
  if(!all(is.numeric(sessions))) {
    sessions = c(1986, 1988, 1993, 1995, 1997, 2002, 2007, 2012, 2014)
    labels = c(86, 88, 93, 95, 97, "02", "07", 12, 14)
    da$Legislature = as.numeric(gsub("(\\d+)\\.(\\d{4})", "\\2", da$Legislature))
  }
    
  g = qplot(data = subset(da, !faulty), x = Legislature, y = value,
            color = chamber, geom = "line") +
    geom_point(data = subset(da, faulty), color = "grey50") +
    scale_x_continuous(breaks = sessions, labels = labels) +
    scale_colour_brewer("", palette = "Set2") +
    facet_wrap(~ variable, ncol = 3, scales = "free_y") +
    labs(y = NULL, x = "\nLegislature (1986-2014)") +
    theme_grey(16) +
    theme(legend.position = "bottom", panel.grid = element_blank())
    
  if(type != "bi")
    g = g +
      geom_point(data = dots_an) +
      geom_point(data = dots_se)

  type = ifelse(type == "", "full", gsub("_", "", type))
  ggsave(paste0("plots/measures/", type, ".pdf"),
         g, width = 11, height = 9)

}

# have a nice day
