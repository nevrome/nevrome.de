---
title: "Integrating Futhark code into an R package"
author: "Clemens Schmid"
---

## Introduction

It's [#DigiArchMaintainathon](https://sslarch.github.io/maintainathon) week and computational archaeologists around the world tackle outstanding housekeeping in their software projects. Part of that is improving documentation, and I wanted to use the opportunity to write about a peculiar setup I've implemented some time last year already: Using the Futhark programming language in an R package. I've [posted about it](https://archaeo.social/@ClemensSchmid/115157603838749836), but a proper write-up has been outstanding for a while.

## Motivation

[R](https://www.r-project.org/) is an excellent language for data analysis and vizualisation, not least thanks to the vast number of well-crafted open-source "packages" available to extend it. Simultanously, as a slightly dated interpreted language with an [extreme focus on dynamism](http://adv-r.had.co.nz/Performance.html), R is not particular fast on its own. It does make it comparatively easy, though, to integrate C, C++, and Fortran code, that does not suffer from the same performance constraints. Many R package authors make use of this for performance-critical functions, e.g. by implementing them in C++ and calling them through the convenient mechanisms of the [Rcpp package](https://cran.r-project.org/web/packages/Rcpp/vignettes/Rcpp-package.pdf).

I personally don't enjoy writing C++ very much -- mostly because of its verbosity and manual memory management -- so I often wondered if I could instead integrate other languages with R. As i) there already is a very convenient path to C++ and thereby to C in R, and ii) C has become the de-facto lingua franca among high level languages through foreign function interfaces and transpilation, this is indeed possible.

A specific application for my [ggpointgrid R package](https://github.com/nevrome/ggpointgrid) finally gave me a good enough justification to dive into this. ggpointgrid is an extension for the [ggplot](https://ggplot2.tidyverse.org) data visualization package. My extension adds functions for regular grid-arrangement of points in scatterplots, fundamentally to avoid overplotting.

![Demonstration of ggpointgrid's regular grid arrangement with `geom_pointgrid()`, compared to a normal scatterplot (`geom_point()`) and randomized point locations (`geom_jitter()`). `geom_pointgrid()` strictly avoids overplotting.](/images/2026-01-12-futhark-in-ggpointgrid/Rplot01.png){width=100%}

Its core algorithm, accessible through the R function `arrange_points_on_grid()`, is a greedy search loop, that assigns input positions to their closest grid positions. It is computationally expensive for large input datasets and arrangement grids, yet still relatively concise with simple in- and outputs.

```r
arrange_points_on_grid(
    # Numeric matrix. Grid coordinates to which the points should be mapped.
    # 2-column matrix with x-axis coordinates in the first, and y-axis coordinates
    # in the second column.
    grid_xy,
    # Numeric matrix. Point (observation) coordinates that should be mapped to the grid.
    # 2-column matrix with x-axis coordinates in the first, and y-axis coordinates in
    # the second column.
    pts_xy
)
```

![Demonstration of the grid arrangement with scattered input points in black, and arranged output points in red.](/images/2026-01-12-futhark-in-ggpointgrid/Rplot.png){width=50% align="center"}

I wanted to use ggpointgrid for a large plot and realized that the performance of my R implementation felt insufficient. A classic situation to bring in compiled language. This time, instead of going for C++, I opted to do this in [Futhark](https://futhark-lang.org), a *statically typed, data-parallel, and purely functional array language* with Haskell-like syntax and solid transpilation to single- and multi-threaded C (and GPU code via CUDA and OpenCL, but that is less relevant here).

## Tutorial

When building this, I worked my way from the inside to the outside: Futhark -> C -> C++ -> R. I tried to write as little manual boilerplate code as possible. And I did use the help of ChatGPT, especially for bridging between the languages and for the optimization of my Furthark code.

I will try to present what I did in the following sections in the form of a brief tutorial. For readers, who would like to follow along, or for myself, when I want to do it again.

### Futhark

To develop in Futhark I first needed to [install the compiler](https://futhark.readthedocs.io/en/stable/installation.html). 
