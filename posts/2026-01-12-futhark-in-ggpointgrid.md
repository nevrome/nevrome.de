---
title: "Integrating Futhark code into an R package"
author: "Clemens Schmid"
---

It's [#DigiArchMaintainathon](https://sslarch.github.io/maintainathon) week where computational archaeologists (and others!) are invited to do tackle outstanding housekeeping in their software projects. Part of that is improving documentation, and I decided to use the opportunity to write about a peculiar setup I've implemented some time last year already: Using the Futhark programming language in an R package. I've [posted about it](https://archaeo.social/@ClemensSchmid/115157603838749836), but a proper write-up has been outstanding for a while.

## Motivation

[R](https://www.r-project.org/) is an excellent language for data analysis and vizualisation, not least thanks to the vast number of well-crafted open-source "packages" available to extend it. Simultanously, as a slightly dated interpreted language with an [extreme focus on dynamism](http://adv-r.had.co.nz/Performance.html), R is not particular fast on its own. It does make it exceptionally easy, though, to integrate C, C++, and Fortran code, that does not suffer from the same performance constraints. Many R packages make use of this for performance-critical functions, e.g. by writing them in C++ and calling them through the convenient mechanisms of the [Rcpp package](https://cran.r-project.org/web/packages/Rcpp/vignettes/Rcpp-package.pdf).

I personally don't enjoy writing C++ very much -- mostly because of its verbosity and manual memory management -- so I often wondered if I could instead integrate other languages with R. As i) there already is a very convenient path to C++ and thereby to C in R, and ii) C has become the de-facto lingua franca among high level languages through foreign function interfaces, this is indeed possible. I just had to figure out how.

My [ggpointgrid R package](https://github.com/nevrome/ggpointgrid) finally gave me a reason to dive into this. This package covers a very specific plotting application: ...