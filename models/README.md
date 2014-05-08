## `ergmm`

Two-dimensional latent space models, using counts of ties in the full networks as the dependent variable. The model is set to identify as many groups `G` as there are party groups in the legislature under examination, includes a random receiver effect, and is referenced against a Poisson distribution to account for overdispersion.

The `latentnet` ([Krivitsky and Handcock 2008](http://www.jstatsoft.org/v24/i05)) specification is

```{S}
ergmm(net ~ euclidean(d = 2, G = g) + rreceiver,
    response = "count", family = "Poisson.log",
    control = ergmm.control(burnin = 10^3,
                            sample.size = 2000,
                            interval = 5))
```

The model takes a few days to converge on all series, and does so unequally well on each legislature (see e.g. legislature 10 of the National Assembly).

For another use of similar models, see [Kirkland 2014](http://jhkirkla.wordpress.com/2013/08/16/chamber-size-effects-on-the-collaborative-structure-of-legislatures/), just published in _Legislative Studies Quarterly_. His network model specification is fairly different from the one I have used so far (all predictors are binary adjacency matrices):

```{S}
ergmm(ARnet ~ latentcov(mat)  +
        latentcov(same.party) + # (Committee)
        latentcov(RelRankmat) + # (Rank)
        latentcov(matCR) +      # (Committee) x (Rank)
        latentcov(matCP) +      # (Committee) x (Party)
        latentcov(matPR) +      # (Rank) x (Party)
        latentcov(matCPR) +     # (Committee) x (Party) x (Rank)
      euclidean(d = 2),
  family = "Poisson",
  response = "values",
  verbose = TRUE,
  control = ergmm.control(sample = 30000, burnin = 50000))
```

## `ergm`

Exponential random graph models that look at fixed differential homophily effects in the cosponsorship networks of each legislature. Differential homophily estimates are provided for parliamentary groups with at least ten members present in the network sample (Senate rules require only ten members to form a group; National Assembly rules used to require 20 before 2009, and now require only 15).

The `ergm` ([Morris, Handock and Hunter 2008](http://www.jstatsoft.org/v24/i04)) specification is

```{S}
ergm(net ~ edges + 
     mutual + 
     absdiff("seniority") + 
     nodematch("female") + 
     nodematch("party", diff = TRUE) + 
     absdiffcat("rightwing"),
   control = control.ergm(MCMLE.trustregion = 10^3,
                          MCMLE.maxit = 100, seed = 3258)
```

Prior to passing the network to the model, we thin it ([Cranmer and Desmarais 2011](http://people.umass.edu/bruced/pubs/Cranmer_Desmarais_PA2011.pdf), p. 78) by taking two quantiles of the log-distribution of edge weights, and subsampling the network to ties contained within this range. Because the log-distribution of edges weights is approximately normal in many cases, the default values (0.025, 0.975) thin the networks to an approximate maximum of 5% of all edges.

The paper appendix contain sensitivity tests of the original models at different values of the thinning parameter:

* unbounded version (-∞, +∞) with no subsampling (maximum density)
* semi-bounded versions (0, 0.975) and (0.025, 1) of the original model
* highly bounded version (0.05, 0.95) that drops around 10% of all ties
