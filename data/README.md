
| file    |                              | N(all)  | N(sampled)   |
|:--------|:-----------------------------|:--------|:-------------|
| `bi_an` | National Assembly bills      | 8,909   | 4,761 (53%)  |
| `am_an` | National Assembly amendments | 84,091  | 40,011 (47%) |
| `bi_se` | Senate bills                 | 3,759   | 2,181 (58%)  |
| `am_se` | Senate amendments            | 102,551 | 55,378 (54%) |

> Last updated 2014-04-06 (scraped and SQL data queried up to that date).

The data objects carry a `sample` column to identify:

* in `amendments`, `bills` and `legislation` data frames: the legislation included in the cosponsorship networks
* in `sponsors` data frames: the sponsors who sat through the entire legislature and chamber in which the legislative item was examined

The internal structures of the most relevant objects are listed below.

# SPONSORS

All datasets contain a `nodes` object holding:

* `legislature`: Fifth Republic legislature (1--14)
* `uid`: unique identifier
* `nom`: full name (uppercased and trimmed)
* `nom_de_famille`: family name (uppercased and trimmed)
* `sexe`: sex (`H`/`F`)
* `annee_naissance`: date of birth (yyyy)
* `party`: party family (`COM`, `ECO`, `RAD`, `SOC`, `CEN`, `DRO`, `FN`, `SE`)
	* `COM`: Communists
	* `ECO`: Greens
	* `RAD`: Radical-Socialists
	* `CEN`: Centrists
	* `DRO`: Gaullists and rightwing groups
	* `FN`: _Front national_ (1986)
	* `SE`: unaffiliated MPs
* `mandat`: mandate period (dd/mm/yyyy-dd/mm/yyyy)
* `nb_mandats`: number of terms served, including current
* `nom_circo`: constituency
* `lon`: longitude (excluding overseas)
* `lat`: latitude (excluding overseas)
* `reelected`: reelected at end of term (0/1)
* `url`: link to National Assembly or Senate profile

Sponsors include all MPs listed in the National Assembly and Senate biographical databases, minus a few unsolved homonyms (the major ones have been solved).

# LEGISLATION

> As of 2014-04-25, data are available for legislatures 8--14 (bills in both chambers), 11--14 (Senate amendments) and 12--14 (National Assembly amendments).

* `legislature`: Fifth Republic legislature (1--14)
* `uid`: unique amendment or bill identifier
* `date`: date of introduction
* `t`: time since legislature started, in days
* `url`: URL to online _dossier_
* `texte`: bill number (legislature-specific)
* `titre`: bill title (if available)
* `sort`: amendment or bill outcome (_adopté_, _rejeté_, _tombé_, etc.)
* `government`: introduced by a member of government (0/1)
 
The sponsorship data include all nominally cited sponsors from the chamber in which the legislation was introduced, at the time at which it was introduced.

The little missing data are due to bills such as [this one](http://www.assemblee-nationale.fr/9/dossiers/880111.asp), which was introduced in 1988 but examined only 17 years later by additional sponsors.
