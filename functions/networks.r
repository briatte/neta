#' Build cosponsorship networks
#'
#' @param plot export plot of counts (on by default)
#' @param export export GEXF file (on by default; will ignore non-merged series)
get_networks <- function(sessions, file, verbose = TRUE, plot = TRUE, export = FALSE) {
  
  file = gsub(".rda", "", file)

  if(plot)
    plot_data(file, verbose = verbose)

  load(paste0("data/", file, ".rda"))

  master = get(ls(pattern = "amendments|bills|legislation"))
  stopifnot(exists("master"))
  
  for(i in sessions) {
    
    data = file

    # append bills-only datasets for National Assembly
    if((file == "an") & (i < 12))
      data = paste0("bi_", data)

    # append bills-only datasets for Senate
    if((file == "se") & (i < 11))
      data = paste0("bi_", data)
    
    data = paste0("data/", data, i, ".rda")

    if(!file.exists(data)) {
      
      edges = filter(master, legislature == i, !government, sample)$uid

      if(exists("legislation"))
        print(with(master[ master$uid %in% edges, ], table(type, legislature)))

      edges = subset(sponsorships, uid %in% edges)

      au = table(edges$name[ edges$status == "author" ])
      co = table(edges$name[ edges$status == "cosponsor" ])
      
      # weighted edges
      net = weighted_edges(edges, verbose = verbose)
      edges = net[["edges"]]
      net = net[["network"]]
      
      nodes = sponsors[ sponsors$legislature == i & sponsors$nom %in% network.vertex.names(net), ]
            
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
      
      # weighted propensity to cosponsor (Gross and Shalizi)
      wpc = net %e% "weight" / nodes$n_au[ net %e% "source" ]
      network::set.edge.attribute(net, "wpc", as.vector(wpc))

      # party affiliation
      net %v% "party" = as.character(nodes$party)
            
      # add colors
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
      
      net %n% "party_colors" = colors[ unique(net %v% "party") ]
      
      # add left/right ordering (unaffiliateds at centre)
      net %n% "party_order" = names(colors) [ colors %in% network::get.network.attribute(net, "party_colors") ]
      
      # sponsor details
      net = weighted_nodes(net, weights = "wpc", verbose = verbose)
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
      
    } else {
      
      load(data)
      
      if(verbose)
        msg("Loaded:", data)
      
    }
    
    # prepare plot
    v = net %n% "party_colors"
    b = net %n% "party_order"
    
    # color edges by source node
    g = suppressMessages(ggnet(net, 
                               node.group = net %v% "party", node.color = net %n% "party_colors",
                               segment.color = factor(colors[ nodes[ net %e% "source", "party" ] ]),
                               node.alpha = .75, segment.alpha = .5, size = 6) +
                           scale_color_manual("", values = v, breaks = b) +
                           theme(text = element_text(size = 28),
                                 legend.key = element_rect(colour = "white", fill = NA),
                                 legend.key.size = unit(28, "pt")))
    
    # save plot
    ggsave(paste0("plots/", file, i, ".pdf"), width = 11, height = 9)
    
    # rename objects
    assign(paste0("ggnet", i), g)
    assign(paste0("net", i), net)
    assign(paste0("colors", i), v)
    assign(paste0("edges", i), edges)
    assign(paste0("nodes", i), nodes)
    
  }
  
  data = paste0("data/", file, ".rda")
  
  save(list = ls(pattern = "^(amendments|bills|legislation|sponsors)|(net|edges|nodes|colors|ggnet)[0-9]+"), 
       file = data)
  
  if(verbose)
    msg("Saved:", data, file_size(data))
  
  if(export)
    weighted_gexf(file)
  
}

#' Create a weighted directed edge list from first authors to all other sponsors
#' 
#' The resulting network is weighted by inverse quantity of cosponsors per bill
#' (used by Newman for scientific coauthorships and by Fowler for Congressional
#' cosponsorships). To normalize Newman-Fowler weigts as in Gross, Shalizi and 
#' Kirkland (weighted propensity to cosponsor), divide them by the number of 
#' bills per author and rebuild the networks.
weighted_edges <- function(edges,
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

#' Compute weighted network measures
#' 
#' - Node centrality computed with \code{\link{tnet}}
#' - Party modularity computed with \code{\link{igraph}}
weighted_nodes <- function(net, weights = "weight", verbose = TRUE) {
  
  tnet = as.tnet(as.sociomatrix(net), type = "weighted one-mode tnet")
  
  # Gross, Kirkland and Shalizi 2012
  if(weights == "wpc") {
    wpc = table(tnet[, 1])[ as.character(tnet[, 1]) ]
    tnet = cbind(tnet[, 1:2], w = tnet[, 3] / wpc)
  }
  
  # symmetrise for undirected algorithms 
  tnet = symmetrise_w(tnet, method = "AMEAN") # answers warning above
  
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
  
  # get MP parliamentary groups
  party = network::get.vertex.attribute(net, "party")
  names(party) = network.vertex.names(net)
  
  # subset to nonmissing groups
  V(inet)$party = party[ V(inet)$name ]
  inet = inet - V(inet) [ is.na(V(inet)$party) ]
  
  # modularity
  net %n% "modularity" = modularity(inet, membership = factor(V(inet)$party))

  # Burt's constraint  
  data$constraint = constraint(inet, weights = E(inet)$weight)[ data$name ]

  return(list("network" = net, "measures" = data))

}

#' Sensitivity tests
#'
#' Saves the table from Opsahl et al. (2010) that shows top nodes by weighted 
#' degree at different values of the alpha parameter. Run on all available data
#' by default.
#' @author Tore Opsahl
#' @source \url{http://toreopsahl.com/2010/04/21/article-node-centrality-in-weighted-networks-generalizing-degree-and-shortest-paths/}
weighted_degree <- function(x = dir("data", pattern = "(am|bi)_(an|se)[0-9]+"),
                            folder = "rankings") {
  
  for(i in x) {
    
    load(paste0("data/", i))
    
    # all weighted edges should be unique
    tnet = edges
    stopifnot(nrow(tnet) == nrow(unique(tnet)))
    
    # convert nodes to numbers for tnet
    lvl = unique(unlist(tnet[, 1:2]))
    tnet$i = as.numeric(factor(tnet$i, levels = lvl))
    tnet$j = as.numeric(factor(tnet$j, levels = lvl))
    
    # symmetrise edge list
    tnet = suppressWarnings(as.tnet(tnet, type = "weighted one-mode tnet"))
    tnet = symmetrise_w(tnet, method = "AMEAN") # answers warning above
    
    # get degree at several values of alpha
    rank = data.frame(
      lvl,
      degree_w(tnet, measure = c("degree", "output", "alpha"), alpha = 0.5),
      degree_w(tnet, measure = "alpha", alpha = 1.5)[, "alpha"],
      stringsAsFactors = FALSE
    )
    names(rank) = c("name", "node", "a00", "a10", "a05", "a15")  

    # merge to names and rank into columns
    rank = data.frame(
      1:nrow(rank),
      rank[order( -rank[, "a00"], -rank[, "a10"]), c("name", "a00" )],
      rank[order( -rank[, "a05"], -rank[, "a10"]), c("name", "a05" )],
      rank[order( -rank[, "a10"], -rank[, "a10"]), c("name", "a10" )],
      rank[order( -rank[, "a15"], -rank[, "a10"]), c("name", "a15" )]
    )
    names(rank) = c("rank", "a00.name", "a00", "a05.name", "a05", 
                    "a10.name", "a10", "a15.name", "a15")
    
    # save full table
    write.csv(rank, file = paste0(folder, "/", gsub(".rda", ".csv", i)),
              row.names = FALSE)

  }
  
}

#' Export GEXF network objects
#'
#' Presets to export a GEXF object.
weighted_gexf <- function(file = c("an", "se"), sessions = 8:14,
                          mode = "kamadakawai", nodes = "degree", ties = "weight",
                          entropize = function(x, n) {
                            by = diff(range(x, na.rm = TRUE)) / 20
                            return(round(x + runif(n, min = by * -1, max = by), 2))
                          }) {
  
  stopifnot(all(sessions %in% 8:14))
  stopifnot(all(file %in% c("an", "se")))
  
  stopifnot(ties %in% c("weight", "wpc"))
  stopifnot(nodes %in% c("degree", "distance"))

  file = gsub(".rda", "", file)
  for(j in file) {
    
    load(paste0("data/", j, ".rda"))
  
    for(i in sessions) {
    
      net = get(paste0("net", i))
      people = get(paste0("nodes", i))
      people = subset(people, nom %in% network.vertex.names(net))
      colors = t(col2rgb(net %n% "party_colors"))
    
      node.att = c("nom", "url", "nom_circo", "nb_mandats", "party", "lon", "lat", "degree", "distance")
      node.att = people[, node.att ]
      node.att$party[ is.na(node.att$party) ] = "SE"
    
      names(node.att)[1] = "label"
    
      # compress floats and add 5% jitter to lon/lat to avoid overplotting
      node.att[, "distance"] = round(node.att[, "distance"], 2)
    
      node.att[ is.na(node.att[, "lon"]), "lon"] = mean(node.att[, "lon"], na.rm = TRUE)
      node.att[ is.na(node.att[, "lat"]), "lat"] = mean(node.att[, "lat"], na.rm = TRUE)
      # node.att[ grepl("Guyane|Polynésie|hors de France", node.att[, "nom_circo"]), "lon"] = mean(node.att[, "lon"], na.rm = TRUE)
      # node.att[ grepl("Guyane|Polynésie|hors de France", node.att[, "nom_circo"]), "lat"] = mean(node.att[, "lat"], na.rm = TRUE)

      # add 5% random noise to avoid overplotting
      if(is.function(entropize)) {
       
        node.att[, "lon"] = entropize(node.att[, "lon"], network.size(net))
        node.att[, "lat"] = entropize(node.att[, "lat"], network.size(net))
        
      }
    
      people = data.frame(id = as.numeric(factor(people$nom)), label = people$nom, stringsAsFactors = FALSE)
    
      relations = get(paste0("edges", i))
    
      relations = data.frame(
        source = as.numeric(factor(relations$i, levels = levels(factor(people$label)))),
        target = as.numeric(factor(relations$j, levels = levels(factor(people$label)))),
        weight = relations$w
      )
      relations = na.omit(relations)
    
      edge.att = data.frame(type = "arrow", weight = relations[, 3], stringsAsFactors = FALSE)
      relations = relations[, -3]

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
    
      file = paste0("app/", j, i, ".gexf")
      write.gexf(nodes = people, edges = relations,
                 nodesAtt = node.att, edgesWeight = edge.att$weight, # edgesAtt = edge.att,
                 # edgesVizAtt = list(color = edgecolors, size = edge.att[, ties]),
                 nodesVizAtt = list(position = position, color = nodecolors, size = node.att[, nodes]),
                 defaultedgetype = "arrow",
                 meta = list(creator = "rgexf",
                             description = paste("chamber:", j, "legislature:", i, "- placement:", mode,
                                                 "- tie size:", ties, "- node size:", nodes),
                             keywords = "Parlement, Parliament, France"),
                 output = file)
    
      msg("Exported legislature", i, file_size(file))
    
    }

  }
  
}

# have a nice day
