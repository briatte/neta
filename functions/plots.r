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

  #data = data[ with(data, !is.na(legislature) & !is.na(date)), ]
  data = na.omit(data[, c("uid", "legislature", "date", "t", "government", "sample") ])
  
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

  # manual breaks
  b = c(0, 1, 2, 5, 10, 25, 50, 100, 250, 500, Inf)
  data$q_authors = cut(data$n_authors, breaks = b, include.lowest = TRUE)
  data$q_authors[ data$government ] = NA

  if(verbose)
    print(table(year(data$date), data$legislature))
  
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
  
  ggsave(paste0("plots/",
                ifelse(type != "all", paste0(substr(type, 1, 2), "_"), ""),
         x, ".pdf"), g, width = 12, height = 11)
  
}

#' Plot a weighted cosponsorship network
#' 
#' Unused (default settings hardcoded into network routine).
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
#' Unused (too much missing data in the complete series).
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

# have a nice day
