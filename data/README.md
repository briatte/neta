
# DATA

The `an` and `se` files hold cosponsored legislation and their corresponding network objects for both chambers of Parliament from legislature 8 (1986) to today. Data were last collected on April 6, 2014 from [assemblee-nationale.fr](http://www.assemblee-nationale.fr/), [senat.fr](http://www.senat.fr/) and [data.senat.fr](http://data.senat.fr/) for the Ameli and Dosleg databases. The overall sample contains roughly 100,000 legislation items:

| chamber           | file | bills | amendments |
|:------------------|:-----|:------|:-----------|
| National Assembly | `an` | 4,670 | 60,048     |
| Senate            | `se` | 2,194 | 36,160     |

Bills (and resolutions) were available in both chambers for the entire observation period. Senate amendments are available only from 2001 (legislature 11) to today, and National Assembly amendments from late 2004 (legislature 12) to today (legislature 14).

The networks are built from roughly one third of all legislation, by sampling all legislative items authored by more than one Member of Parliament who sat in the chamber during the observation period. There are a few missing data here and there due to minor parser errors.

The internal structure of the most relevant objects are listed below.

# SPONSORS

All datasets contain a `nodes` object holding:

* `legislature`: Fifth Republic legislature (1--14)
* `uid`: unique identifier
* `nom`: full name (uppercased and trimmed)
* `nom_de_famille`: family name (uppercased and trimmed)
* `sexe`: sex (`H`/`F`)
* `annee_naissance`: date of birth (yyyy)
* `party`: party family, ordered from left to right:
	* `COM`: Communists
	* `ECO`: Greens
	* `RAD`: Radical-Socialists
	* `SE`: unaffiliated MPs
	* `CEN`: Centrists
	* `DRO`: Gaullists and rightwing groups
	* `FN`: Front national (1986)
* `mandat`: mandate period (dd/mm/yyyy-dd/mm/yyyy)
* `nb_mandats`: number of terms served, including current
* `nom_circo`: constituency
* `lon`: longitude
* `lat`: latitude
* `reelected`: reelected at end of term (dummy)
* `url`: link to National Assembly or Senate profile
* `sample`: network sample marker

Geocodes are from [Google](https://developers.google.com/maps/terms) and exclude the overseas. The geocodes exported to the GEXF networks are first processed through 5% random noise to avoid overplotting.

# LEGISLATION

All datasets contain an `amendments`, `bills` or merged `legislation` object holding:

* `legislature`: Fifth Republic legislature (1--14)
* `uid`: unique amendment or bill identifier
	* `AA`: National Assembly amendments
	* `AB`: National Assembly bill
	* `SA`: Senate amendment
	* `SB`: Senate bill
* `date`: date of introduction
* `t`: time since legislature started, in days
* `texte`: bill number (legislature-specific)
* `sort`: amendment or bill outcome (_adopté_, _rejeté_, _tombé_, etc.)
* `government`: introduced by a member of government (dummy)
* `sample`: network sample marker

Unique identifiers are 12-character strings (padded with zeros where relevant) that start with the codes indicated above, followed by the legislature number, followed by a unique identification number that is inherited from the original database or created by the scraper if absent.

# SPONSORSHIPS

All datasets contain a `sponsorship` object holding:

* `uid`: the legislation unique identifier
* `name`: the sponsor full name (uppercased and trimmed)
* `status`: the author status (`author` or `cosponsor`)
* `sample`: network sample marker

The data parser functions basically match the first column to the legislation data, the second one to the sponsors data, and then create directed edge lists from the sampled sponsors in the third column, from first authors to all other MP cosponsors from the same chamber.
