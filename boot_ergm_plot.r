library(ggplot2)

# coefs = data.frame(summary(m)[['coefficients']])
# coefs = cbind(rownames(coefs), coefs)
# names(coefs) = c("term", "b", "se", "z", "p")

p = subset(coefs, !grepl("factor|nodecov", term))
p$term = gsub("(.*).party.|^B|.1$", "", p$term)
p$term = factor(p$term, levels = c("edges", "gwesp", "gwdsp.fixed", "gwidegree", "gwodegree",
                                   "mutual", "nodematch.female", "absdiff.seniority", "absdiff.rightwing",
                                   "COM", "ECO", "SOC", "RAD", "CEN", "DRO", "FN", "SE"))
levels(p$term) = c("Edges", "GWESP", "GWDSP", "GWD cosponsors", "GWD authors",
                   "Reciprocity", "Same gender", "Diff. seniority", "Diff. left-right",
                   "Both Communist", "Both Green", "Both Socialist", "Both Radical", "Both Centrist", "Both Conservative", "Both FN", "Both unaffiliated")
p$type = ifelse(grepl("Both", p$term), as.character(p$term), "Network controls")
qplot(data = subset(p, !grepl("GW", term)), color = type,
      y = b, x = factor(term, levels = rev(levels(term))), ymin = b - 3 * se, ymax = b + 3 * se,
      geom = "pointrange") +
  geom_pointrange(color = "black", alpha = .25) +
#   scale_color_manual(values = c("Control term" = "grey50", "Differential homophily" = "black")) +
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
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  coord_flip() +
  labs(x = NULL, y = NULL) +
  theme_grey(16) +
  theme(legend.position = "none", axis.text = element_text(color = "black"))

#str(coefs)
