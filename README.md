# Epidemiological Model applied to Santa Catarina state (epiCata)

Theoretical details can be seen in our preprint:

> Avelar, P. H. da C., Lamb, L. C., Tsoka, S., & Cardoso-Silva, J. (2021). Weekly Bayesian modelling strategy to predict deaths by COVID-19: a model and case study for the state of Santa Catarina, Brazil. (preprint) http://arxiv.org/abs/2104.01133

If you reuse this code, please cite the reference above.

In case you encounter any problems replicating this code or have any suggestions, you can [open a new issue](https://github.com/Data-Science-Brigade/modelo-epidemiologico-sc/issues) here on Github.

Ideas, questions and other general comments can be made at the [Discussions](https://github.com/Data-Science-Brigade/modelo-epidemiologico-sc/discussions) page.

## Current focus

You can see our project board [here](https://github.com/Data-Science-Brigade/modelo-epidemiologico-sc/projects/1).

## Useful Links

Tutorials are available in Brazilian Portuguese (PT-BR):

- [00 - Setup do Ambiente](https://data-science-brigade.github.io/modelo-epidemiologico-sc/tutoriais/PT-BR/Tutorial00.html)
- [01 - Preprocessamento](https://data-science-brigade.github.io/modelo-epidemiologico-sc/tutoriais/PT-BR/Tutorial01.html)
- [02 - Modelo base e uma única localização](https://data-science-brigade.github.io/modelo-epidemiologico-sc/tutoriais/PT-BR/Tutorial02.html)

Our earlier reports (2020) with weekly experiments and testes are available online every at [this dedicated Social Good Brasil website](https://socialgoodbrasil.org.br/modelo-epidemiologico/).

If you want to understand how R packages work, [this online book](http://r-pkgs.had.co.nz/) is the best source for it.

## Dependencies of epiCata

If you are using Debian-based Linux distributions, the following packages have to be installed prior to installing the R dependencies:

```
sudo apt install libcurl4-openssl-dev libssl-dev libxml2-dev libv8-dev
```

### R Dependencies

First, install the following packages to be able to build the package locally in your machine and run the scripts:

* optparse==1.6.6
* devtools==2.4.0
* usethis==2.0.1 (devtools dependency)

Now, you need to install the R packages below to get it to work for you. The easiest way to do this is by opening your terminal and running the script:

```
R -f install_dependencies.R
```

This will guarantee that you have the same setup as the developers of the project:
 
* rstan==2.19.3
* zoo==1.8.0
* bayesplot==1.7.2
* tidyverse==1.3.0
* dplyr==1.0.0
* tidyr==1.1.0
* EnvStats==2.3.1
* cowplot==1.0.0
* ggrepel==0.8.2
* scales==1.1.1
* ggplot2==3.3.2
* optparse==1.6.6
* slider==0.2.1


### Compiling

If you are [using RStudio](https://support.rstudio.com/hc/en-us/articles/200486508-Building-Testing-and-Distributing-Packages), create a new project by using the parent directory. Otherwise, if you want to run it from the command line, just go to the project directory: `cd modelo-epidemiologico-sc`.

Within an R terminal, run the following commands to build both `epiCata` and `epiCataPlot` packages:

```R
setwd("epiCata")
devtools::build()
devtools::install(upgrade="never")
setwd("../")

setwd("epiCataPlot")
devtools::build()
devtools::install(upgrade="never")
setwd("../")
```

After this, make sure that both libraries are accessible and installed with:

```R
library(epiCataPlot)
library(epiCata)
```

## How to run the model

The current standard way of running the model is by following the steps below:

1. Place required data in the parent directory: `deaths.csv` and `Global_Mobility_Report.csv`. For information on how to obtain death data for Santa Catarina, refer to [Tutorial 01](https://data-science-brigade.github.io/modelo-epidemiologico-sc/tutoriais/PT-BR/Tutorial01.html) (currently available only in Brazilian Portuguese) and see sections below to understand how to download these data.

2. Run scripts A and B simultaneously to make a better use of time. For example, to use `base-reported.stan` model with 4000 iterations (2000 of which are warmup), 4 chains and a max tree depth of 9, you can run the commands: 

```R
nohup Rscript run_health_regions_A.R -u "base-reported" -m DEVELOP -k 7 -i 4000 -w 2000 -c 4 -t 9 -r "2021-07-19" > ../logs/health-regions-B-base-reported-2021-07-19-DEVELOP-4000-2000-4-9.out &
```

and

```R
nohup Rscript run_health_regions_B.R -u "base-reported" -m DEVELOP -k 7 -i 4000 -w 2000 -c 4 -t 9 -r "2021-07-19" > ../logs/health-regions-B-base-reported-2021-07-19-DEVELOP-4000-2000-4-9.out &
```

3. Monitor progress with `tail` or, prettier, with `multitail` command. Example:

```console
multitail -c ../logs/health-regions-A-base-reported-2021-07-19-DEVELOP-4000-2000-4-9.out

multitail -c ../logs/health-regions-B-base-reported-2021-07-19-DEVELOP-4000-2000-4-9.out
```

4. Once both scripts have completed, combine the results:

```R
 Rscript combine_independent_results.R -u "base-reported" -m DEVELOP -k 7 -i 4000 -w 2000 -c 4 -t 9 -r "2021-07-19"
```

Use the same parameters just to make sure names and dates are properly referenced.

5. Have a look at the generated figures, which will usually be saved by default to the path `../figures/YYYY_MM_DD` and a .Rdata file will be saved to `../results/YYYY_MM_DD`

5. To generate plots with the same format as in the reports, use :

```r
cd epiCataDashboard/dashboard_results/
vim plot_report.R #edit the reference date
R -f plot_report.R
```
