# HOWTO

This repo will

* scrape details on Members of Parliament since 1958,
* download all available legislation from both chambers since 1986,
* and build their cosponsorships into networks.

The main entry point is [`make.r`][makefile], which runs in R and requires a PostgreSQL setup as detailed below. The other dependencies are the R packages installed by [`load.r`](https://github.com/briatte/neta/blob/master/load.r), all of which will correctly self-install from CRAN with R 3.1.0, at the exception of `RPostgreSQL`, which you might have to install and load manually:

```{S}
install.packages('RPostgreSQL', type = 'source')
library(RPostgreSQL)
```

Running [`make.r`][makefile] will then try to rebuild the full network series after downloading all available data. The ~ 1.3 GB raw data (260 MB zipped) collected so far are not included in the repo but are available per request if you want to significantly speed up execution time and avoid network errors.

The processed network data objects are saved as objects of class `network` to the `data` folder and exported to [GEXF](http://gexf.net/format/) (Gephi) format to the `app` folder. The `rda` data files also contain the complete, unsampled legislation and sponsorships, including government and single-authored bills.

## PostgreSQL

The PostgreSQL setup expects the following [Data Sénat][ds] databases and user:

```{SQL}
CREATE USER opendata;
CREATE DATABASE ameli;
CREATE DATABASE dosleg;
```

[ds]: http://data.senat.fr/

Other PostgreSQL server settings can be adjusted in the first lines of [`make.r`][makefile].

One way to get the databases if you have a TeX distribution installed is to knit the `ameli.Rnw` and `dosleg.Rnw` files from the [`codebooks`](https://github.com/briatte/neta/tree/master/codebooks) folder, which will

1. download up-to-date versions of `Ameli` and `Dosleg` if none are present in the `codebooks` folder, and
2. print a list of all tables and variables for both databases, as a PDF of table column names, to that folder.

## Other functions

The few functions that are not run in [`make.r`][makefile] can be run separately, such as `weighted_degree` to export centrality rankings at different values of the node weighting parameter, using code from [Tore Opsahl][to]:

```{S}
weighted_degree('rankings')
```

[jf]: http://jhfowler.ucsd.edu/cosponsorship.htm
[to]: http://toreopsahl.com/tnet/weighted-networks/

The `get_ameli2` scraper function can also try to reconstitute the `Ameli` data by downloading all database extracts available online from the online dossier pages. The result contained ~ 1,171 bills as of January 2014, which represented roughly 80% of the `Ameli` database at the same data.

* * *

See the `README` files of the [`app`](https://github.com/briatte/neta/blob/master/app/README.md) and [`data`](https://github.com/briatte/neta/blob/master/data/README.md) folders for additional details. The code also contains a few additional documentation lines.

[makefile]: https://github.com/briatte/neta/blob/master/make.r
