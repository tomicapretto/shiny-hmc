# ShinyHMC

An interactive Shiny app to learn about Hamiltonian Monte Carlo sampling.

This is still a work in progress. I do have a couple of ideas to try and I would like to document everything a bit more. Suggestions are more than welcomed :)

## Installation

Install this package from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("tomicapretto/shiny-hmc")
```

## Run application

The function `run_app()` fires up the application. Just do

``` r
shinyhmc::run_app()
```

and have fun!


## Ideas

### Features

* Add content about each target distribution in the main container
* Change color of segments as more segments are added?
  * To make clear when a trajectory returns over the same point
  * Gradient instead of transparency?
* Allow users to specify a momentum

### Usage

* Show what happens when momentum is null (i.e. c(0, 0))
* Show what happens when momentum is huge (e.g. c(4, 3.5))
* Show what happens when momentum is exactly the same in both directions (c(2, 2))