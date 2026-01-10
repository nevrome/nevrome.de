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

I will try to present what I did in the following sections in the form of a brief tutorial. For readers, who would like to follow along, or for myself, when I want to do it again. I assume you have an R package where you're already actively developing and now want to outsource specific functionality to Futhark.

### Futhark

1. [Install the Futhark compiler](https://futhark.readthedocs.io/en/stable/installation.html) to develop code in the language. I'm on an an Arch Linux derivative, so I could install a binary directly from the [AUR](https://aur.archlinux.org/packages/futhark-bin).

2. Write the Futhark code that covers the functionality you would like to cover in a `.fut` file in the `src` directory of your R package. In my case this was a function of the following type in the [`arrange.fut`](https://github.com/nevrome/ggpointgrid/blob/1.4.0/src/arrange.fut) source file:

```txt
entry arrange_from_coordinates (grid_xs: []f64) (grid_ys: []f64) (pts_x: []f64)  (pts_y: []f64):
                               ([]f64, []f64)
```

> I will not dive into the details of the implementation here, but instead highlight two observations: i) Futhark is a pure functional language, so a language without side effects or I/O. The only way to communicate with the outside world is either through the main (`def main`), or another `entry` point function. We will see below how this function can be called. ii) Futhark itself has an ecosystem of software libraries that can be installed and imported directly from GitHub with the compiler executable. I made use of such a dependency to get access to a faster sorting algorithm (`import "lib/github.com/diku-dk/sorts/radix_sort"`). For that it needs to be registered in a [`futhark.pkg`](https://github.com/nevrome/ggpointgrid/blob/1.4.0/src/futhark.pkg) file in the `src` directory:

```txt
require {
  github.com/diku-dk/sorts 0.6.0 #f448cd990c86c566715915a8fc4182c8c2ff5400
}
```

> If such a file is present, then the `futhark` command line software can install the required depdencies to a `src/lib` dir with `futhark pkg sync`. The compiler software conveniently comes with the package manager build in.

3. Use the compiler to translate the futhark code to C. The command line tool supports two relevant interfaces for this usecase: `futhark c` and `furthark multicore` (besides `futhark cuda`, `opencl`, `pyopencl`, `python`, `wasm` and `wasm-multicore`, which are all available in version 0.25.29.). The generated multicore code is currently only compatible with Unix operating systems. To not exclude Windows users later on, we here choose the the simple sequential `c` output. We can then transpile our code file like this:

```bash
futhark c --library arrange.fut
```

> The `--library` flag causes futhark to create a C library, instead of an exectutable. For testing purposes we can create an executable, and test the behaviour of our entry point function on the command line. 

```bash
futhark c arrange.fut
echo [1,2,3,4,5] [1,2,3,4,5] [1,1,1,1] [1,1,1,1] | ./arrange -e arrange_from_coordinates
```
```txt
[1.000000000000000f64, 2.000000000000000f64, 3.000000000000000f64, 4.000000000000000f64]
[1.000000000000000f64, 2.000000000000000f64, 3.000000000000000f64, 4.000000000000000f64]
```

> This is very useful for debugging throughout the development process. Futhark also comes with powerful profiling and debugging features that are accessible on like this.

### C

4. Inspect the resulting C code. `futhark c --library arrange.fut` generates a very large .c file [`arrange.c`](https://github.com/nevrome/ggpointgrid/blob/1.4.0/src/arrange.c), and a short header file [`arrange.h`](https://github.com/nevrome/ggpointgrid/blob/1.4.0/src/arrange.h). This header file contains the essential interface between C and Futhark, so functions and data types necessary to correctly call the entry function we defined above and now want to thread through to R. It would now be possible to do this directly and call C code from R without another intermediate C++ layer. It seemed easier to me, though, to follow well-trodden paths here with Rcpp.

### C++

5. Setup Rcpp. Using Rcpp requires some changes to an R package, as documented in a vignette [here](https://cloud.r-project.org/web/packages/Rcpp/vignettes/Rcpp-package.pdf).


