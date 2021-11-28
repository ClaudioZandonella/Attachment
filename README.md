
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Evaluating Informative Hypotheses with Equality and Inequality Constraints: A Tutorial Using the Bayes Factor via the Encompassing Prior Approach

In this repository we collect all the materials of the study
“*Evaluating informative hypotheses with equality and inequality
constraints: A tutorial using the Bayes factor via the encompassing
prior approach*”. The paper aims to provide a clear and detailed
description of the Bayes factor with the encompassing prior approach
considering an applied example regarding attachment theory.

The pre-print version of the article is available at \[TODO: add link\].
The Supplemental Material with all the analyses is available at
<https://claudiozandonella.github.io/Attachment/>.

## Project Structure

The project structure is based on the traditional R-package project
structure, but the repository is not meant to be installed as a package
(see Section [Reproducibility](#reproducibility) on how to run the
analyses). R-package project structure, however, allows us to get
advantage of some functionalities:

-   `DESCRIPTION` file is used to track project metadata and manage
    project dependencies (note that we also used `renv` for package
    dependencies, see Section below)
-   `devtools::load_all()` allows to easily load all functions defined
    to run the analysis (note that we followed the *functional
    programming paradigm*, see Section below)

Moreover, the analysis workflow is managed using `drake` (see Section
below) and include other (*non-standard*) folders to organize the
scripts for the analysis and for the produced outputs (i.e., paper and
supplemental material).

The actual repository folder structure is:

-   `Analysis/` - includes the file `Analysis_Drake.R` used to run the
    analysis workflow using `drake`
-   `Data/` - includes the data used in the analyses
-   `Documents/` - includes all the scripts and utility files for the
    creation of the outputs. In particular,
    -   `Bookdown/` - the Supplemental Material available online
    -   `Paper/` - the pre-print version of the article
-   `R/` - includes all the `.R` scripts with the functions used in the
    analyses
-   `docs/` - includes `bookdown` outputs used to build the GitHub Pages
    site
-   `renv/` - utility folder used by `renv`

## Reproducibility

To guarantee the reproducibility of the results, the R-package`drake` is
used to manage the analysis workflow and to enhance the readability and
transparency of the analysis. To know more about `drake` consider the
[official Git-hub page](https://github.com/ropensci/drake) or the [user
manual](https://books.ropensci.org/drake/). Summarizing, using `drake`
the user defines the plan of the analysis where each step in the
analysis is defined through functions. Functions can be appropriately
defined to obtain desired targets (i.e., R-output with results of
interests) and they are declared in another script. Subsequently,
`drake` manages the whole analysis recognizing the dependency structure
of the different targets. When any change is made to the code `drake`
evaluates the analysis and updates the results. Following the
*functional programming paradigm* (i.e., defining functions for each
step of the analysis) allows us to avoid “*coping and paste*” in the
code, it makes debugging easier, and it facilitates the reading of the
code.

Moreover, the R-package `renv` is used to manage the dependencies of the
R-packages used in the analysis. The `renv` package allows the creation
of an isolated, portable, and reproducible environment where the
analyses are run. To know more about `renv` consider the [official
documentation](https://rstudio.github.io/renv/articles/renv.html).
However, `renv` is limited as it can not handle different R versions.

To overcome this issue, we also provide a `Dockerfile` to build a Docker
image with R version 4.1.0. To know more about Docker see [official
documentation](https://www.docker.com/) or [rOpenSci Labs
tutorial](https://jsta.github.io/r-docker-tutorial/).

### Run the Analysis

To reproduce the analysis:

1.  Download the repository on your local machine

    ``` bash
    git clone https://github.com/ClaudioZandonella/Attachment
    ```

2.  Open the R project by double-clicking the file `Attachment.Rproj`
    you can find in the project main directory. A new R-studio session
    will open.

3.  Run `renv::restore()` to install the project’s dependencies (have a
    coffee, it takes some time).

4.  Now the required package will be loaded. Restart the R session
    (`command`/`ctrl` + `shift` + `f10`) so `devtools::load_all()` is
    automatically run (according to `.Rprofile`) to load all functions
    used in the analysis defined in the `R/` folder.

    > Note that also `source("Documents/Utils_report.R")` is
    > automatically run to load report utility functions (functions to
    > load analysis results and to compile the bookdown).

5.  Open `Analysis/Analysis_Drake.R` and run each line (one by one) to
    run the analysis using drake. First, the analysis environment is set
    together with the analysis plan (available at `R/Plan.R`). Next, the
    analysis targets are computed (have a coffee, it takes a long time).
    Finally, the analysis results are loaded and briefly presented.

    > Note that you can access the analysis targets using the functions
    > `drake::loadd(<name_target>)`, or load all the results with
    > `drake_load_all()`.

    > It could happen that sometimes the session stays idle when fitting
    > `brms` models without throwing errors. In this case, it is
    > necessary to force restarting the R session and rerun the
    > analysis. An error warning message will appear:
    > `Error: drake's cache is locked`. Just follow the instructions
    > provided by drake to force unlock of the cache
    > `drake::drake_cache("...")$unlock()`.

6.  To compile the pre-print version of the article (possible after the
    analysis), open `Documents/Paper/Paper.Rnw` and compile the file
    using `knitr` and `pdfLaTeX` (see [project
    options](https://support.rstudio.com/hc/en-us/articles/200532247-Weaving-Rnw-Files-in-the-RStudio-IDE)).
    Note that, as usual for LaTeX files, the first run will fail. Run
    again to obtain the actual output.

7.  To compile the Supplemental Material, run `make_my_book()`. The
    resulting output is saved in the `docs/` folder.

#### Docker

To run the analysis in a Docker container, first, download the
repository on your local machine. Next, build the image according to the
`Dockerfile` present in the project main directory, using

``` bash
docker build -t attachment .
```

> It is important to build the image before running the analysis

Next, you can run the container using

``` bash
docker run --rm -p 8787:8787 -e PASSWORD="<your-PW>" attachment
```

Now, open the folder `Attachment/` and run the analysis following the
instructions above from step 2.
