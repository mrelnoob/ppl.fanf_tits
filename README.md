
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ppl.fanf_tits

<!-- badges: start -->
<!-- badges: end -->

The `ppl.fanf_tits` project (and associated Github repository) is meant
to contain all the R code I used to clean, explore, and analyse the data
from the urban tits reproduction study launched in 2021 in the context
of the PubPrivLands project. The aim was to investigate the importance
of foraging habitat connectivity and patch size to explain the breeding
success of urban Great tits (*Parus major*) and Blue tits (*Cyanistes
caeruleus*) in Dijon, France. In accordance to the FAIR (Findable,
Accessible, Interoperable, Reusable) principles of open-science, you
will find in the `ppl.fanf_tits` repository all the **data and codes** I
used to produce the results presented in the manuscript we submitted to
*Journal of Biogeography* and that is currently called “Foraging habitat
connectivity is more important than local habitat quantity to explain
urban tits breeding success”. Upon acceptance, I will update this file
to link it to the DOI of the article (if you intend to reuse this
dataset, please cite the associated article in your own communications).

If you have any question, feel free to contact me through my Github
profile or using the e-mail address given in the manuscript.

## Content

In the `ppl.fanf_tits` repository, you will find several folders (and
sub-folders):

- **data** – contains the raw datasets used to produce the cleaned
  dataset (cf. ~/output/tables/ndata_final.csv) used for our formal
  statistical analyses.
- **output** – contains the elements produced by the R scripts,
  including the final dataset, some numeric results, tables and plots,
  as well as an **exploratory modelling report** (in .Rmd, .pdf or .html
  formats). The final dataset PDF metadata file will also be placed here
  upon acceptance of the article.
- **R** – contains all R scripts. These are numbered. Files that start
  with 01\_ contains the custom functions I made to wrangle, clean and
  prepare the data. Files that start with 02\_ contains the code I used
  to analyses the data and produce the results reported and discussed in
  the manuscript.
- **renv** – contains the local library of the project. That is all the
  packages in the same version I used to produce the results (and as
  listed in the **renv.lock** file).

## Download and reproductibility

To download the `ppl.fanf_tits` repository, click on the green **Code**
button on the top-right of the Github page
(<https://github.com/mrelnoob/ppl.fanf_tits>) and select “Download ZIP”.
Once the repository is downloaded, you need to install the same global R
version as the one I used (R 4.4.1.), set your working directory inside
the repository, and run:

``` r
install.packages('renv')
renv::init()
renv::restore()
```

The `{renv}` package will read the **renv.lock** file and automatically
install all the packages I used in the correct version (see also:
<https://rstudio.github.io/renv/>). You may then run my R scripts. Note
that many functions created in the R files are only used in the EDA
report (RMarkdown file located within the “ouput/” folder).
