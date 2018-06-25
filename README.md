
<!-- README.md is generated from README.Rmd. Please edit that file -->
dyntoy
======

dyntoy simulates single-cell expression data in which a single-cell trajectory is present. Even though the model to generate the data is very simplistic (and far from realistic), it can simulate very complex trajectory models, such as large trees, convergences and loops.

As the data is relatively easy, it can be used to quickly test and prototype a [TI method](https://github.com/dynverse/dynmethods). However, for more realistic synthetic data, check out our [dyngen package](https://github.com/dynverse/dyngen).

Installation
------------

Install using devtools:

``` r
# install.packages("devtools")
devtools::install_github("dynverse/dyntoy")
```

Example
-------

dyntoy contains some pre-generated toy data within the `toy_tasks` data object:

``` r
data("toy_tasks", package = "dyntoy")
```

Data can be generated using `generate_dataset`:

``` r
library(dyntoy)
task <- generate_dataset(
  model = model_bifurcating(num_branchpoints = 2),
  num_cells = 1000,
  num_genes = 1000
)
#> Note that the names of some metrics have changed, see 'Renamed metrics' in ?calculateQCMetrics.
#> Old names are currently maintained for back-compatibility, but may be removed in future releases.

task$milestone_network
#> # A tibble: 5 x 4
#>   from  to    length directed
#>   <chr> <chr>  <dbl> <lgl>   
#> 1 M3    M4     0.235 TRUE    
#> 2 M1    M3     0.477 TRUE    
#> 3 M5    M6     0.732 TRUE    
#> 4 M3    M5     0.669 TRUE    
#> 5 M5    M2     0.754 TRUE
```

Related tools
-------------

-   PROSSTT: <https://github.com/soedinglab/prosstt>
-   Splatter: <https://github.com/Oshlack/splatter>
