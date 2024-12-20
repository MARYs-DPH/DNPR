# Working with data from the Danish National Patient Registry (DNPR)

We provide functions for categorizing patient types in DNPR3 and computing contact duration. 

## Installation

This is an *R* package. [*R*](https://www.r-project.org/) is required,
[*RStudio*](https://www.rstudio.com/) is recommended.


The package is available on GitHub. To install the package, start by installing the `devtools` package. The best way to do this is from CRAN, by typing:

```{}
install.packages("devtools")
```

Install the `DNPR` package from GitHub using the following code.

```{}
devtools::install_github("MARYs-DPH/DNPR", build_vignettes = TRUE)
```

After installing the package has to be attached as usual:

```{}
library(DNPR)
```

And you are ready to go!
