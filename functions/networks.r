#' Build cosponsorship networks
#'
#' @param plot export plot of counts (on by default)
#' @param export export GEXF file (on by default; will ignore non-merged series)
get_networks <- function(sessions, file, verbose = TRUE, plot = FALSE, export = FALSE) {
  
  file = gsub(".rda", "", file)
  if(nchar(file) < 3)
    get_data(file)
  
  if(plot)
    plot_data(file, verbose = verbose)
  
  load(paste0("data/", file, ".rda"))
  
  master = get(ls(pattern = "amendments|bills|legislation"))
  master$year = year(master$date)
  stopifnot(exists("master"))
  
  if(sessions[1] == "yr")
    sessions = apply(expand.grid(8:14, 1986:2014), 1, paste0, collapse = ".")
  
  # get divided and unified governments government periods

  if(10 %in% sessions & file %in% c("an", "se"))
    sessions = c(sessions, 10.2) # no data at 10.1

  if(10 %in% sessions & file == "se")
    sessions = c(sessions, 10.1)

  if(13 %in% sessions & file %in% c("an", "se"))
    sessions = c(sessions, 13.1, 13.2)
  
  for(i in sessions) {
    
    if(i == as.integer(i))
      edges = subset(master, legislature == i & !government & sample)$uid
    else if(i == 10.1) # 1993-1995, leftwing weakly divided
      edges = subset(master, legislature == 10 & date < as.Date("1995-04-23") & !government & sample)$uid
    else if(i == 10.2) # 1995-1997, rightwing strongly unified
      edges = subset(master, legislature == 10 & date >= as.Date("1995-04-23") & !government & sample)$uid
    else if(i == 13.1) # 2007-2011, rightwing strongly unified
      edges = subset(master, legislature == 13 & date < as.Date("2011-09-25") & !government & sample)$uid
    else if(i == 13.2) # 2011-2012, rightwing weakly unified (leftwing Senate)
      edges = subset(master, legislature == 13 & date >= as.Date("2011-09-25") & !government & sample)$uid
    else {
      y = as.numeric(str_pad(gsub("\\d+\\.(\\d+)", "\\1", i), width = 4, side = "right", pad = "0"))
      edges = subset(master, legislature == as.integer(i) & year == y & !government & sample)$uid
    }
    
    #     if(exists("legislation") & length(edges))
    #       print(with(master[ master$uid %in% edges, ], table(type, legislature)))
    
    edges = subset(sponsorships, uid %in% edges)
    #     message(paste(i, y, nrow(edges)))
    
    data = file
    
    # append bills-only datasets for National Assembly
    if((file == "an") & (as.integer(i) < 12))
      data = paste0("bi_", data)
    
    # append bills-only datasets for Senate
    if((file == "se") & (as.integer(i) < 11))
      data = paste0("bi_", data)
    
    data = paste0("data/", data, i, ".rda")
    
    if(!file.exists(data) & nrow(edges) > 0) {
      
      au = table(edges$name[ edges$status == "author" ])
      co = table(edges$name[ edges$status == "cosponsor" ])
      
      # weighted edges
      net = get_edges(edges, verbose = verbose)
      edges = net[["edges"]]
      net = net[["network"]]
      
      nodes = sponsors[ sponsors$legislature == as.integer(i) & sponsors$nom %in% network.vertex.names(net), ]
      
      # find missing sponsors
      unknown = !network.vertex.names(net) %in% nodes$nom
      if(sum(unknown) > 0) {
        
        # find sponsors from past legislatures
        prev = network.vertex.names(net)[ unknown ]
        prev = subset(sponsors, legislature < i & nom %in% prev)
        
        if(nrow(prev) > 0) {
          
          # find latest sponsor-legislature dyads
          last = aggregate(legislature ~ nom, max, data = prev)
          last = gsub("\\s+", " ", apply(last, 1, paste0, collapse = " "))
          
          # select and conform to master data
          prev$uid = paste(prev$nom, prev$legislature, sep = " ")
          prev = subset(prev, uid %in% last)
          
          if(verbose)
            msg("Adding:", nrow(prev), "identified sponsor(s)")
          
          # merge to master data
          if(!"uid" %in% names(nodes)) nodes = cbind(nodes, uid = 1:nrow(nodes))
          nodes = rbind(prev, nodes)
          
        }
        
        # identify missing nodes
        missing = !network.vertex.names(net) %in% nodes$nom
        
        if(sum(missing > 0)) {
          
          warning(
            paste("Dropping:", sum(missing), "unidentified sponsor(s)",
                  ifelse(verbose, 
                         paste(head(network.vertex.names(net)[ missing ]),
                               collapse = ", "),
                         "")
            )
          )
          
        }
        
        # subset network
        network::delete.vertices(net, which(missing))
        
      }
      
      if(verbose)
        msg("Network:", network.size(net),
            "nodes", network.edgecount(net), "edges")
      
      # rewrite UID and rownames
      nodes$uid = 1:nrow(nodes)
      rownames(nodes) = nodes$nom
      
      # authorship count
      nodes$n_au = au[ rownames(nodes) ]
      nodes$n_au[ is.na(nodes$n_au) ] = 0
      
      # cosponsorship count
      nodes$n_co = co[ rownames(nodes) ]
      nodes$n_co[ is.na(nodes$n_co) ] = 0
      
      # weighted propensity to cosponsor (Gross, Kirkland and Shalizi 2012)
      wpc = net %e% "weight" / nodes$n_au[ net %e% "source" ]
      network::set.edge.attribute(net, "wpc", as.vector(wpc))
      
      # party affiliation
      net %v% "party" = as.character(nodes$party)
      
      # add colors to Left-Right party groups
      colors = c(
        "COM" = "#E41A1C", # Communists and Far Left (L; red)
        "ECO" = "#4DAF4A", # Greens (L; green)
        "RAD" = "#FFFF33", # Radicals (L; yellow)
        "SOC" = "#F781BF", # Socialists (or large L coalition; pink)
        "SE" = "#999999",  # Unaffiliated/Unknown (mixed; grey)
        "CEN" = "#FF7F00", # Centrists: RI, UDF, UDI, NC, Modem (R; orange)
        "DRO" = "#377EB8", # Conservatives: Gaullists, RPR, DL, UMP (R; blue)
        "FN" = "#A65628"   # Front National (extreme-right; brown)
      )
      
      net %n% "party_colors" = colors[ unique(net %v% "party") ]
      
      # add left/right ordering (unaffiliateds at centre)
      net %n% "party_order" = names(colors) [ colors %in% network::get.network.attribute(net, "party_colors") ]
      
      # sponsor details
      net = get_nodes(net, weights = "wpc", verbose = verbose)
      attrs = c("uid", "nom", "nom_de_famille", "sexe", "annee_naissance",
                "party", "nom_circo", "lon", "lat",
                "mandat", "nb_mandats", "reelected",
                "n_au", "n_co", "url")
      nodes = merge(nodes[, attrs], net[["measures"]], by.x = "nom", by.y = "name")
      rownames(nodes) = nodes$nom
      
      # network object
      net = net[["network"]]
      
      # identifiers
      net %n% "legislature" = i
      net %n% "data" = ifelse(grepl("bi", file), "Bills", 
                              ifelse(grepl("am", file), "Amendments", 
                                     "All legislation"))
      net %n% "chamber" = ifelse(grepl("an", file),
                                 "Assemblée nationale", "Sénat")
      
      # paste node-level attributes
      for(j in names(nodes))
        network::set.vertex.attribute(net, j, as.vector(nodes[ network.vertex.names(net), j ]))
      
      # send to session dataset
      save(net, edges, nodes, colors, file = data)
      
      if(verbose)
        msg("Saved:", data, file_size(data))
      
    } else if(file.exists(data) & nrow(edges) > 0) {
      
      load(data)
      
      if(verbose)
        msg("Loaded:", data)
      
    }
    
    if(nrow(edges) > 0) {
      
      assign(paste0("net", i), net)
      assign(paste0("edges", i), edges)
      assign(paste0("nodes", i), nodes)
      get_measures(i, ch = file, update = FALSE)
      
    }
    
    plot = paste0("plots/", gsub("data/(.*).rda", "\\1", data), ".pdf")
    if(!file.exists(plot) & nrow(edges) > 0) {
      
      if(verbose)
        msg("Plotting:", gsub("data/(.*).rda", "\\1", data), nrow(edges), "edges")
      
      # prepare plot
      v = net %n% "party_colors"
      b = net %n% "party_order"
      
      net %v% "size" = 3 * as.numeric(factor(quantile(net %v% "degree", c(0, .5, 1))))

      del = which(is.na(net %v% "closeness"))
      # msg("Dropping:", length(del), "nodes from graph")
      network::delete.vertices(net, del)
      
      # color edges by source node
      g = suppressMessages(ggnet(net,
                                 segment.color = factor(colors[ nodes[ net %e% "source", "party" ] ]),
                                 segment.alpha = .5,
                                 node.group = net %v% "party", node.color = net %n% "party_colors",
                                 size = 0) +
#                              scale_color_manual("", values = v, breaks = b,
#                                                 guide  = guide_legend(override.aes = list(size = 6))) +
#                              geom_point(aes(size = net %v% "size"), alpha = 1/3) +
#                              geom_point(size = 3, alpha = 1/2) +
                                 scale_color_manual("", values = v, breaks = b,
                                                    guide  = guide_legend(override.aes = list(size = 6))) +
                                 geom_point(size = 9, alpha = 1/3) +
                                 geom_point(size = 6, alpha = 1/2) +
                                 scale_size_area(max_size = 9) +
                                 guides(size = FALSE) + 
                                 scale_size_area(max_size = 9) +
                                 guides(size = FALSE) + 
                             theme(text = element_text(size = 28),
                                   legend.key = element_rect(colour = "white", fill = NA),
                                   legend.key.size = unit(28, "pt")))
      
      # save plot
      ggsave(plot, g, width = 11, height = 9)

    }
    
  }
  
  data = paste0("data/", file, ".rda")
  
  save(list = ls(pattern = "^(amendments|bills|colors|legislation|sponsors)|(net|edges|nodes)[0-9]+"), 
       file = data)
  
  if(verbose)
    msg("Saved:", data, file_size(data))
  
  # export to GEXF
  if(nchar(file) < 3 & export)
    get_gexf(file)     
    
}

#' Create a weighted directed edge list from first authors to cosponsors
#' 
#' The resulting network is weighted by inverse quantity of cosponsors per bill
#' (used by Newman for scientific coauthorships and by Fowler for Congressional
#' cosponsorships).
get_edges <- function(edges,
                      uid = "uid", name = "name",
                      verbose = TRUE) {
  
  q = unique(edges[, uid])
  
  if(verbose)
    msg("Parsing:", length(q), "authors", nrow(edges) - length(q), "cosponsors")
  
  # tie first author to all other sponsors
  edges = lapply(q, function(x) {
    cbind(as.character(x), 
          edges[edges[, uid] == x, name][1], # first author
          edges[edges[, uid] == x, name][-1] # everyone else
    )
  })
  edges = rbind.fill.matrix(edges)
  
  # inverse number of sponsors per bill (Newman 2001)
  w = edges[, 1]
  w = table(w) [ w ]
  edges = data.frame(edges[, 2:3], 1 / w, stringsAsFactors = FALSE)
  
  # remove self-loops (and drop authors with no cosponsors)
  names(edges) = c("X1", "X2", "W")
  edges = subset(edges, X1 != X2)
  
  # identify unique ties
  edges$UID = paste( edges$X1, edges$X2, sep = "_")
  
  # raw count of unique ties
  count = table( edges$UID )
  
  # weighted quantity of bills cosponsored (Fowler 2006)
  edges = aggregate(W ~ UID, sum, data = edges)  
  
  # paste to save later
  count = count[ edges$UID ]
  
  # reexpand to (weighted) edgelist
  edges = cbind(ldply(strsplit(edges$UID, "_")), edges$W)
  names(edges) = c("i", "j", "w")
  
  # convert to network object
  edges = data.frame(na.omit(edges))
  net = network(edges[, 1:2], directed = TRUE)
  
  # add edge weights to one-mode network
  network::set.edge.attribute(net, "source", edges[, 1])
  network::set.edge.attribute(net, "target", edges[, 2])
  network::set.edge.attribute(net, "weight", edges[, 3])
  network::set.edge.attribute(net, "count", as.vector(count))
  
  return(list(network = net, edges = edges))
  
}

#' Compute weighted propensity to cosponsor and weighted network measures
#' 
#' Divides Newman-Fowler weights by the number of bills per author (Gross, 
#' Shalizi and Kirkland 2012). The resulting weight distributions are roughly
#' log-normal in most networks.
get_nodes <- function(net, weights = "weight", verbose = TRUE) {
  
  # weighted adjacency matrix to tnet
  tnet = as.tnet(as.sociomatrix(net, attrname = weights), type = "weighted one-mode tnet")
  
  # symmetrise for undirected algorithms 
  tnet = symmetrise_w(tnet, method = "AMEAN")
  
  # measures dataset
  data = data.frame(name = network.vertex.names(net),
                    node = 1:network.size(net), stringsAsFactors = FALSE)
  
  # generalized measures
  data = merge(data, degree_w(tnet)[, -3], alpha = 1, by = "node") # [1]
  dist = distance_w(tnet) # [2]
  
  # shortest paths
  dist = data.frame(distance = rowMeans(dist, na.rm = TRUE), node = attr(dist, "nodes"))
  data = merge(data, dist, by = "node", all.x = TRUE)
  
  # centrality
  data = merge(data, closeness_w(tnet)[, -2], alpha = 1, by = "node", all.x = TRUE) # [3]
  data = merge(data, clustering_local_w(tnet), by = "node") # [4]
  data = merge(data, betweenness_w(tnet), by = "node") # [5]
  
  # [1] http://toreopsahl.com/2008/11/28/network-weighted-network/
  # [2] http://toreopsahl.com/2009/01/09/average-shortest-distance-in-weighted-networks/
  # [3] http://toreopsahl.com/tnet/weighted-networks/node-centrality/
  # [4] http://toreopsahl.com/2009/01/23/weighted-local-clustering-coefficient/
  # [5] http://toreopsahl.com/2009/02/20/betweenness-in-weighted-networks/
  
  # rename columns
  names(data)[ names(data) == "n.closeness" ] = "closeness"
  names(data)[ names(data) == "am" ] = "clustering"
  
  # graph-level properties
  net %n% "degree" = mean(data$degree, na.rm = TRUE) # mean centralization (weighted)
  net %n% "distance" = mean(data$distance, na.rm = TRUE) # mean distance (weighted)
  net %n% "density" = network.density(net) # density (unweighted); idem graph.density(inet)
  net %n% "clustering" = clustering_w(tnet) # generalized clustering coefficient (weighted)
  
  # rename vertices
  tnet = data.frame(
    i = network.vertex.names(net)[ tnet[, 1] ],
    j = network.vertex.names(net)[ tnet[, 2] ],
    w = tnet[, 3]
  )
  
  # convert to igraph
  inet = graph.edgelist(as.matrix(tnet[, 1:2]), directed = FALSE)
  E(inet)$weight = tnet[, 3]
  
  # Burt's constraint  
  data$constraint = constraint(inet, weights = E(inet)$weight)[ data$name ]
  
  return(list("network" = net, "measures" = data))
  
}

#' Export GEXF network objects
#'
#' Presets to export a GEXF object.
get_gexf <- function(file = c("an", "se"), sessions = 8:14,
                     mode = "kamadakawai", nodes = "degree",
                     entropize = function(x, n) {
                       by = diff(range(x, na.rm = TRUE)) / 20
                       return(round(x + runif(n, min = by * -1, max = by), 2))
                     }) {
  
  file = gsub("data/", "", file)
  stopifnot(all(file %in% c("an", "se")))
  
  stopifnot(all(sessions %in% 8:14))
  stopifnot(nodes %in% c("degree", "distance"))
  
  file = gsub(".rda", "", file)
  for(j in file) {
    
    load(paste0("data/", j, ".rda"))
    
    for(i in sessions) {
      
      net = get(paste0("net", i))
      colors = t(col2rgb(net %n% "party_colors"))
      
      meta = as.character(net %n% "chamber")
      meta = list(creator = "rgexf",
                  description = paste0(meta, ", legislature ", i, ", ", mode,
                                       " placement; edges weighted by propensity to cosponsor, nodes sized by ", nodes),
                  keywords = "Parlement, Parliament, France")
      
      people = get(paste0("nodes", i))
      
      node.att = c("nom", "url", "nom_circo", "nb_mandats", "party", "lon", "lat", "degree", "distance")
      node.att = people[, node.att ]
      
      names(node.att)[1] = "label"
      
      # compress float
      node.att$distance = round(node.att$distance, 2)
      
      # plot nodes with no coordinates at means
      node.att$lon[ is.na(node.att$lon) ] = mean(node.att$lon, na.rm = TRUE)
      node.att$lat[ is.na(node.att$lat) ] = mean(node.att$lat, na.rm = TRUE)
      
      # add 5% random noise to avoid overplotting
      if(is.function(entropize)) {
        
        node.att$lon = entropize(node.att$lon, network.size(net))
        node.att$lat = entropize(node.att$lat, network.size(net))
        
      }
      
      people = data.frame(id = as.numeric(factor(people$nom)),
                          label = people$nom,
                          stringsAsFactors = FALSE)      
      
      relations = data.frame(
        source = as.numeric(factor(net %e% "source", levels = levels(factor(people$label)))),
        target = as.numeric(factor(net %e% "target", levels = levels(factor(people$label)))),
        weight = round(net %e% "wpc", 4)
      )
      relations = na.omit(relations)
      
      nodecolors = lapply(node.att$party, function(x)
        data.frame(r = colors[x, 1], g = colors[x, 2], b = colors[x, 3], a = .3 ))
      nodecolors = as.matrix(rbind.fill(nodecolors))
      
      net = as.matrix.network.adjacency(net)
      
      # placement method (Kamada-Kawai best at separating at reasonable distances)
      position = paste0("gplot.layout.", mode)
      if(!exists(position)) stop("Unsupported placement method '", position, "'")
      
      position = do.call(position, list(net, NULL))
      position = as.matrix(cbind(position, 1))
      colnames(position) = c("x", "y", "z")
      
      # compress floats
      position[, "x"] = round(position[, "x"], 2)
      position[, "y"] = round(position[, "y"], 2)
      
      # strong ties (upper quartile)
      q = (relations[, 3] >= quantile(relations[, 3], .75))
      
      file = paste0("app/", j, i, ".gexf")
      write.gexf(nodes = people,
                 edges = relations[, -3],
                 edgesWeight = 1 + (relations[, 3] >= quantile(relations[, 3], .75)),
                 nodesAtt = node.att,
                 nodesVizAtt = list(position = position, color = nodecolors,
                                    size = node.att[, nodes]),
                 edgesVizAtt = list(size = relations[, 3]),
                 defaultedgetype = "directed", meta = meta,
                 output = file)
      
      msg("Exported legislature", i, file_size(file))
      
    }
    
  }
  
}

#' Sensitivity tests
#'
#' Saves the table from Opsahl et al. (2010) that shows top nodes by weighted 
#' degree at different values of the alpha parameter. Run on all available data
#' by default.
#' @author Tore Opsahl
#' @source \url{http://toreopsahl.com/2010/04/21/article-node-centrality-in-weighted-networks-generalizing-degree-and-shortest-paths/}
get_measures <- function(x, ch, update = FALSE, weights = "wpc") {
  
  file = paste0("measures/modularity/", ch, x, ".rda")
  chamber = ifelse(grepl("an", ch), "Assemblée nationale", "Sénat")
  
  data = paste0("data/", ch, x, ".rda")

  if(file.exists(data))
    load(data)
  else
    load(paste0("data/bi_", ch, x, ".rda"))
  
  msg("Network:", chamber, "legislature", x)
  
  # weighted adjacency matrix to tnet
  tnet = as.tnet(as.sociomatrix(net, attrname = weights), type = "weighted one-mode tnet")
  
  # get degree at several values of alpha
  degree = data.frame(
    network.vertex.names(net),
    degree_w(tnet, measure = c("degree", "output", "alpha"), alpha = 0.5),
    degree_w(tnet, measure = "alpha", alpha = 1.5)[, "alpha"],
    stringsAsFactors = FALSE
  )
  names(degree) = c("name", "node", "a00", "a10", "a05", "a15")  
  
  # merge to names and degree into columns
  degree = data.frame(
    1:nrow(degree),
    degree[order( -degree[, "a00"], -degree[, "a10"]), c("name", "a00" )],
    degree[order( -degree[, "a05"], -degree[, "a10"]), c("name", "a05" )],
    degree[order( -degree[, "a10"], -degree[, "a10"]), c("name", "a10" )],
    degree[order( -degree[, "a15"], -degree[, "a10"]), c("name", "a15" )]
  )
  names(degree) = c("rank", "a00.name", "a00", "a05.name", "a05", 
                    "a10.name", "a10", "a15.name", "a15")
  
  write.csv(degree, file = paste0("measures/degree/", ch, x, ".csv"),
            row.names = FALSE)
  
  if(!file.exists(file) | update) {
    
    # symmetrise for undirected algorithms 
    tnet = symmetrise_w(tnet, method = "AMEAN")
    
    # rename vertices
    tnet = data.frame(
      i = network.vertex.names(net)[ tnet[, 1] ],
      j = network.vertex.names(net)[ tnet[, 2] ],
      w = tnet[, 3]
    )
    
    # convert to igraph
    inet = graph.edgelist(as.matrix(tnet[, 1:2]), directed = FALSE)
    E(inet)$weight = tnet[, 3]
    
    # merge appended sponsors to main groups
    s = net %v% "party"
    if(sum(s == "ECO", na.rm = TRUE) < 10 & any(s == "ECO"))
      s[ s == "ECO" ] = "SOC"
    if(sum(s == "COM", na.rm = TRUE) < 10 & any(s == "COM"))
      s[ s == "COM" ] = "SOC"
    if(sum(s == "RAD", na.rm = TRUE) < 10 & any(s == "RAD"))
      s[ s == "RAD" ] = "SOC"
    if(sum(s == "CEN", na.rm = TRUE) < 10 & any(s == "CEN"))
      s[ s == "CEN" ] = "DRO"
    
    # drop unaffiliated sponsors
    s = ifelse(s == "SE", NA, s)
    
    # get MP parliamentary groups
    names(s) = network.vertex.names(net)
    
    # subset to nonmissing groups
    V(inet)$party = factor(s[ V(inet)$name ])
    print(table(V(inet)$party, exclude = NULL))
    
    inet = inet - which(is.na(V(inet)$party))
    
    # modularity
    modularity = modularity(inet, membership = V(inet)$party, weights = E(inet)$weight)
    
    msg("Modularity:", round(modularity, 2),
        "over", n_distinct(V(inet)$party), "groups")
    
    # maximized Walktrap (Waugh et al. 2009, arXiv:0907.3509, Section 2.3)
    walktrap = lapply(1:50, function(x) walktrap.community(inet, steps = x))
    
    # max. partition
    maxwalks = order(sapply(walktrap, modularity), decreasing = TRUE)[1]
    walktrap = walktrap[[ maxwalks ]]
    
    msg("Maximized to", n_distinct(walktrap[[ "membership" ]]), "groups (Walktrap,", maxwalks, "steps out of 50)")
    
    # multilevel Louvain (Blondel et al. 2008, arXiv:0803.0476)
    louvain = multilevel.community(inet)
    
    msg("Maximized to", n_distinct(louvain[[ "membership" ]]), "groups (Louvain)")
    
  } else {
    
    load(file)
    
  }
  
  save(degree, modularity, walktrap, louvain, file = file)
  
  modularity_max = max(c( modularity(walktrap), modularity(louvain) ))
  return(data.frame(chamber,
                    Legislature = x,
                    # unweighted graph-level
                    Vertices = network.size(net),
                    Edges = network.edgecount(net),
                    Density = net %n% "density",
                    # weighted graph-level
                    Centralization = net %n% "degree",
                    Distance = net %n% "distance",
                    Global.Clustering = net %n% "clustering",
                    # maximized modularity
                    Modularity = modularity,
                    Modularity.Max = modularity_max,
                    Modularity.Ratio = modularity / modularity_max,
                    stringsAsFactors = FALSE)
  )
  
}

# have a nice day
