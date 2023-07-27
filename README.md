# A2SIT

This is the repository for the Admin2 Severity Index Tool (A2SIT). The A2SIT is a Shiny web app built in R which allows users to upload data and build a severity index!

:warning: *The A2SIT is currently under development...* :construction:

## Installation

Although the final aim is to host the A2SIT online, the A2SIT app is encapsulated in an R package, so it can be installed and run locally (as long as you have R installed). To do this, run (in R):

```
remotes::install_github("UNHCR-Guatemala/A2SIT")
```

Note this requires the 'remotes' package.

The app is currently in a beta testing phase, which means it is fully-functional but may still contain some bugs and may be updated.

## Running the app

The prototype is accessible @ https://rstudio.unhcr.org/SeverityIndex/ 

To run the app locally, after installing the A2SIT package, run:

```
library(A2SIT)

run_app()
```

Note that an example data set can be found at the directory specified by `system.file("data_module-input.xlsx", package = "A2SIT")`, or you can download the file [here](https://github.com/UNHCR-Guatemala/A2SIT/raw/main/inst/data_module-input.xlsx).

## Documentation

The A2SIT app is documented in an online book:

:blue_book: [A2SIT Online Book](https://unhcr-guatemala.github.io/A2SIT/book/index.html)

The functions in the package are also available online:

:question: [Function documentation](https://unhcr-guatemala.github.io/A2SIT/)
