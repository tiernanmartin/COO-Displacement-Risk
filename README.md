Project Description
================
Tiernan Martin, [Futurewise](http://www.futurewisewa.org/)
17 September, 2016

### Usage

Several of the datasets used in this project were collected using an `R` package named [`acs`](https://cran.r-project.org/web/packages/acs/acs.pdf). This package accesses data collected by the [American Community Survey](https://www.census.gov/programs-surveys/acs/) using the [Census Data API](http://www.census.gov/data/developers/data-sets.html), which requires its users to obtain a key before using. Any users interested in reproducing these datasets **must** obtain their own personal API key (<http://api.census.gov/data/key_signup.html>) and install it using the `acs::api.key.install` command. Several of the `acs` functions will not work until this key is installed.
