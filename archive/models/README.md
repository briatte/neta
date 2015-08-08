# SPECS

The `latentnet` ([Krivitsky and Handcock 2008](http://www.jstatsoft.org/v24/i05)) specification is

```{S}
ergmm(net ~ euclidean(d = 2, G = g) + rreceiver + rsender,
    response = "count", family = "Poisson.log",
    control = ergmm.control(burnin = 10^5,
                            sample.size = 5000,
                            interval = 10))
```

For another use of similar models, see [Kirkland 2014](http://jhkirkla.wordpress.com/2013/08/16/chamber-size-effects-on-the-collaborative-structure-of-legislatures/), just published in _Legislative Studies Quarterly_:

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

* * *

The `ergm` ([Morris, Handock and Hunter 2008](http://www.jstatsoft.org/v24/i04)) specification is

```{S}
net ~ edges +                          # size intercept
  mutual +                             # reciprocality
  gwesp +                              # GWESP
  gwdsp(alpha = 1, fixed = TRUE) +     # GWDSP
  gwidegree(decay = 1, fixed = TRUE) + # GWD cosponsors
  gwodegree(decay = 1, fixed = TRUE) + # GWD authors
  nodefactor("female") +               # main effect
  nodematch("female") +                # uniform homophily
  nodecov("seniority") +               # nodal covariance
  absdiff("seniority") +               # absolute difference
  nodefactor("rightwing") +            # main effect
  absdiffcat("rightwing") +            # categorical difference
  nodefactor("party") +                # main effect
  nodematch("party", diff = TRUE))     # differential homophily
```

The TERGM estimation method follows [Desmarais and Cranmer 2012](http://people.umass.edu/bruced/pubs/Desmarais_Cranmer_PhysA2012.pdf), using the code at the end of [Hayes Clark and Caro 2013](https://www.academia.edu/4765164/Multimember_Districts_and_the_Substantive_Representation_of_Women_An_Analysis_of_Legislative_Cosponsorship_Networks).

