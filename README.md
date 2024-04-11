# README

This project uses `{renv}`. If you clone the repository and open the project, `{renv}` should set everything up for you.

If it doesn't work, this is a list of dependencies you need to consider

* `{shiny}`
* `{shinyjs}`
* `{shinyWidgets}`
* `{rgl}`
* `{katex}`
* `{here}`
* `{Deriv}`
* `{R6}`

To run the app, simply execute the `app.R` script.

This is still a work in progress. I do have a couple of ideas to try and I would like to document everything a bit more. Suggestions are more than welcomed :)

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