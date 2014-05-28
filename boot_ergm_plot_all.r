setwd('/Users/fr/Documents/Code/R/neta')
library(ggplot2)
library(xtable)

models = "models/ergm.csv"
if(!file.exists(models)) {
  d = dir("models/ergm", pattern = ".rda")
  p = data.frame()
  for(i in d[ !grepl("_raw", d) ]) {
    load(paste0("models/ergm/", i))
    coefs = data.frame(summary(m)[['coefficients']])
    coefs = cbind(rownames(coefs), coefs)
    names(coefs) = c("term", "b", "se", "z", "p")
    coefs$model = gsub("boot_|.rda", "", i)
    p = rbind(p, coefs)
  }
  p
  write.csv(p, file = "models/ergm.csv", row.names = FALSE)
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
p$model = ifelse(grepl("_lw", p$model), "Leftwing\ngovernments",
                 ifelse(grepl("_rw", p$model), "Rightwing\ngovernments", "All governments\n(1986-2014)"))
                 
cols = c("", "\\textbf{All legislatures}", "SE", "\\textbf{Leftwing governments}", "SE", "\\textbf{Rightwing governments}", "SE")

m = subset(p, ch == "Assemblée nationale" & grepl("All", model))[, 1:3]
m = merge(m, subset(p, ch == "Assemblée nationale" & grepl("Left", model))[, 1:3], by = "term", all = TRUE)
m = merge(m, subset(p, ch == "Assemblée nationale" & grepl("Right", model))[, 1:3], by = "term", all = TRUE)
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
      include.rownames = FALSE, file = "paper/tables/ergm_an.tex")

m = subset(p, ch != "Assemblée nationale" & grepl("All", model))[, 1:3]
m = merge(m, subset(p, ch != "Assemblée nationale" & grepl("Left", model))[, 1:3], by = "term", all = TRUE)
m = merge(m, subset(p, ch != "Assemblée nationale" & grepl("Right", model))[, 1:3], by = "term", all = TRUE)
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
      include.rownames = FALSE, file = "paper/tables/ergm_se.tex")

qplot(data = subset(p, grepl("Both", term)), color = type,
      y = b, x = model, ymin = b - 3 * se, ymax = b + 3 * se,
      geom = "pointrange") +
  geom_pointrange(color = "black", alpha = .5) +
  #   scale_color_manual(values = c("Control term" = "grey50", "Differential homophily" = "black")) +
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
ggsave("models/ergm_diff.pdf", height = 12, width = 9)

qplot(data = subset(p, !grepl("Both|GW|Edges", term)),
      y = b, x = model, ymin = b - 3 * se, ymax = b + 3 * se,
      geom = "pointrange") +
  geom_pointrange(color = "black", alpha = .25) +
  #   scale_color_manual(values = c("Control term" = "grey50", "Differential homophily" = "black")) +
  scale_y_continuous(minor_breaks = NULL) +
  facet_grid(term ~ ch, scales = "free") +
  geom_hline(yintercept = 0, color = "grey25", linetype = "dashed") +
  labs(x = NULL, y = NULL) +
  theme_grey(16) +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())
ggsave("models/ergm_cov.pdf", height = 12, width = 9)

qplot(data = subset(p, grepl("GW(D|E)|Edges", term)),
      y = b, x = model, ymin = b - 3 * se, ymax = b + 3 * se,
      geom = "pointrange") +
  geom_pointrange(color = "black", alpha = .25) +
  #   scale_color_manual(values = c("Control term" = "grey50", "Differential homophily" = "black")) +
  scale_y_continuous(minor_breaks = NULL) +
  facet_grid(term ~ ch, scales = "free") +
  geom_hline(yintercept = 0, color = "grey25", linetype = "dashed") +
  labs(x = NULL, y = NULL) +
  theme_grey(16) +
  theme(legend.position = "none",
        axis.text = element_text(color = "black"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())
ggsave("models/ergm_net.pdf", height = 12, width = 9)

print(unique(p$model))
