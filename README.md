Project Description
================
Tiernan Martin, [Futurewise](http://www.futurewisewa.org/)
| Last updated: 01 February, 2017

### Usage Notes

#### Rendering a Gitbook

This repository contains the data, scripts, and outputs for the Displacement Risk Assessment project. The directory is structured in the following folder hierarchy:

    .
    ├── 1-data
    │   ├── 1-notebooks
    │   │   └── html_support_files
    │   ├── 2-raw
    │   ├── 3-external
    │   │   └── manual
    │   ├── 4-interim
    │   │   └── archive
    │   │   └── tmp
    │   └── 5-tidy
    ├── 2-analysis
    ├── 3-communication
    │   ├── 1-bookdown          <- contains the bookdown document render script
    │   │   ├── _book 
    │   │   ├── css
    │   │   ├── images
    │   │   └── latex
    │   ├── 2-shinyapps
    │   └── others
    │       ├── html
    │       ├── images
    │       ├── msword
    │       ├── pdf
    │       └── sp
    ├── javascripts
    ├── packrat
    ├── proj-setup-scripts
    ├── stylesheets
    └── vault

Because the [bookdown](https://bookdown.org/) folders are located *below* the repo's root directory, it is necessary to use a custom R script to render the User Guide document. To run this script, perform the following commands in a [shell](https://en.wikipedia.org/wiki/Shell_(computing)):

1.  Move into the repo's local directory (e.g., `cd ~/Downloads/COO-Displacement-Risk`)
2.  Move into the `_book` directory: `cd ./3-communication/1-bookdown/`
3.  Run the rendering script: `Rscript _render.R 'bookdown::gitbook'`
    -   Note: if this script fails and returns an error saying `C stack usage is too close to the limit`, then the limit must be manually adjusted by running the following shell command: `$ ulimit -s 16384`. For more details see [here](http://stackoverflow.com/questions/14719349/error-c-stack-usage-is-too-close-to-the-limit).

4.  To view the result, open the following HTML file in a browser: `./3-communication/1-bookdown/_book/index.html`

#### Working with a clone of the repo

In an effort to make this project as easy to reproduce as possible, the project uses only free and open-source software, is thoroughly documented, and is published on Github. However, there are several things to consider when working with a clone of this repo:

##### Downloading Datasets Too Large for Github

Due to Github's [file size limitations](https://help.github.com/categories/managing-large-files/), some of the raw datasets are not included in the repo. Running the package for the first time on a local clone will trigger the download of these large files, which will be a time-consuming operation (perhaps prohibitively slow, depending on the user's internet connection speed). It is recommended that users step through each of the rmarkdown files (`*.Rmd`) in `1-data/1-notebooks/`, running each code chunk and allowing the downloads to take place as necessary.

While this process is acceptably quick for most of the notebooks, there are a few that contain operations that may take anywhere from 30 minutes to an hour to run. These processes involve external geospatial datasets that are very large (e.g. 2GB) and are not available online (this may change eventually, but for now they must be obtained by contacting [King County's GIS Department](http://www.kingcounty.gov/services/gis.aspx)). If a user needs to reproduce this assessment "from scratch", they should contact the repo maintainer: `tiernan[at]futurewise[dot]org`

##### Using the `acs` R Package

Several of the datasets used in this project were collected using an `R` package named [`acs`](https://cran.r-project.org/web/packages/acs/acs.pdf). This package accesses data collected by the [American Community Survey](https://www.census.gov/programs-surveys/acs/) using the [Census Data API](http://www.census.gov/data/developers/data-sets.html), which requires its users to obtain a key before using. Any users interested in reproducing these datasets **must** obtain their own personal API key (<http://api.census.gov/data/key_signup.html>) and install it using the `acs::api.key.install` command. Several of the `acs` functions will not work until this key is installed.
