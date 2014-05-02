# SPECS

Memo to self on network model specifications.

## ERGMM

The `ergmm` files contain the results of a two-dimensional latent space model, using counts of ties in the full networks. The model identifies as many groups `G` as there are party groups in the legislature under examination, includes a random receiver effect, and is referenced against a Poisson distribution to account for overdispersion in the dependent variable.

The `latentnet` ([Krivitsky and Handcock 2008](http://www.jstatsoft.org/v24/i05)) specification is

```{S}
ergmm(net ~ euclidean(d = 2, G = g) + rreceiver,
    response = "count", family = "Poisson.log",
    control = ergmm.control(burnin = 10^3,
                            sample.size = 2000,
                            interval = 5))
```

The model takes a few days to converge on all series, and does so unequally well on each legislature (see e.g. legislature 10 of the National Assembly).

## ERGM

The `ergm` files contain the results of exponential random graph models that look for fixed differential homophily effects in the cosponsorship networks of each legislature. Estimates are provided only for major parliamentary groups where the sample size is sufficient to correctly estimate standard errors and avoid further model degeneracy due to infinite coefficients.

The `ergm` ([Morris, Handock and Hunter 2008](http://www.jstatsoft.org/v24/i04)) specification is

```{S}
ergm(net ~ edges + 
     mutual + 
     absdiff("seniority") + 
     nodematch("female") + 
     nodematch("party", diff = TRUE) + 
     absdiffcat("rightwing"),
   control = control.ergm(MCMLE.trustregion = 1000)
```

Prior to passing the network to the model, we thin it ([Cranmer and Desmarais 2011](http://people.umass.edu/bruced/pubs/Cranmer_Desmarais_PA2011.pdf), p. 78) by taking two quantiles of the log-distribution of edge weights, and subsampling the network to ties contained within this range. Because the log-distribution of edges weights is approximately normal, the default values `(0.025, 0.975)` thin the networks to an approximate maximum of 5% of all edges.

The paper appendix contain sensitivity tests of the original models at different values of the thinning parameter:

* unbounded version `(-Inf, Inf)` with no subsampling (maximum density)
* semi-bounded versions `(0, 0.975)` and `(0.025, 1)` of the original model
* excessively bounded version `(0.05, 0.95)` that drops around 10% of all ties

