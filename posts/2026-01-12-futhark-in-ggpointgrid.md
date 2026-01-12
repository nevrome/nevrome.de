---
title: "Integrating Futhark code into an R package"
author: "Clemens Schmid"
---

## Introduction

It's [#DigiArchMaintainathon](https://sslarch.github.io/maintainathon) week and computational archaeologists around the world tackle outstanding housekeeping in their software projects. I wanted to use the opportunity to document a peculiar setup I've implemented last year: Using the Futhark programming language in an R package. I've [posted about it](https://archaeo.social/@ClemensSchmid/115157603838749836) back then, but a proper write-up has been outstanding.

## Motivation

[R](https://www.r-project.org/) is an excellent language for data analysis and visualisation, not least thanks to the vast number of open-source *packages* available to extend it. Simultaneously, as a slightly dated interpreted language with [lazyness and an extreme focus on dynamism](http://adv-r.had.co.nz/Performance.html), R is not particularly fast on its own. It does make it comparatively easy, though, to integrate C, C++, and Fortran code, that does not suffer from the same performance constraints. Many R package authors make use of this for performance-critical functions, e.g. by implementing them in C++ and calling them through the convenient mechanisms of the [Rcpp package](https://cran.r-project.org/web/packages/Rcpp/vignettes/Rcpp-package.pdf).

I personally don't enjoy writing C++, mostly because of its verbosity and resource management semantics. So I often wondered if I could instead integrate other languages with R. As there already is a very convenient path to C, and C has become the de-facto lingua franca among high level languages through foreign function interfaces and transpilation, this is indeed possible.

A computationally intensive application for my [ggpointgrid R package](https://github.com/nevrome/ggpointgrid) finally gave me a good enough reason to dive into this. ggpointgrid is an extension for the [ggplot](https://ggplot2.tidyverse.org) package. It adds functions for regular grid-arrangement of points in scatterplots, fundamentally to avoid overplotting.

![Demonstration of ggpointgrid's regular grid arrangement with `geom_pointgrid()`, compared to a normal scatterplot with `geom_point()` and randomized point locations with `geom_jitter()`. `geom_pointgrid()` reliably shows all observations, and positions them close to where they should be in x-y-space.](/images/2026-01-12-futhark-in-ggpointgrid/Rplot01.png){width=100%}

Its core algorithm is accessible through the R function `arrange_points_on_grid()` and implements a greedy search loop, that assigns input positions to the closest free grid positions. It is computationally expensive for large input datasets and grids, yet still relatively concise with simple in- and outputs.

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

I wanted to use ggpointgrid for a large plot and perceived the performance of my old R implementation of this algorithm insufficient. A classic situation to bring in a compiled language! This time, instead of going for C++, I opted to extend my package with [Futhark](https://futhark-lang.org). That is a *statically typed, data-parallel, and purely functional array language* with Haskell-like syntax and solid transpilation to single- and multi-threaded C code (and other targets, like GPU code via CUDA and OpenCL). 

ggpointgrid v1.4.0 features a complete, working product of this experiment. I will try to present what I did in the following sections in the form of a brief tutorial. For readers, who would like to follow along, or for myself, when I want to do it again. *Disclaimer:* I did use the help of ChatGPT, especially for bridging between the languages and for the optimization of my Furthark code.

## Tutorial: Using Futhark in R

I assume you have an R package where you're already actively developing and now want to outsource specific functionality to Futhark. We are going to work our way from the inside to the outside: Futhark -> C -> C++ -> R. 

### Futhark

1. [Install the Futhark compiler](https://futhark.readthedocs.io/en/stable/installation.html) `futhark` to develop code in the language. I'm on an Arch Linux derivative, so I could install a binary of version v0.25.29 directly from the [AUR](https://aur.archlinux.org/packages/futhark-bin).

2. Write the Futhark code that covers the functionality you would like to include in a `.fut` file in the `src/` directory of your R package. A good starting point on how to use the Futhark language is the open online book [Parallel Programming in Futhark](https://futhark-book.readthedocs.io/en/latest/). For my use case I wrote a function `arrange_from_coordinates` with the following type signature in a source file [`src/arrange.fut`](https://github.com/nevrome/ggpointgrid/blob/1.4.0/src/arrange.fut):

```txt
entry arrange_from_coordinates (grid_xs: []f64) (grid_ys: []f64) (pts_x: []f64)  (pts_y: []f64):
                               ([]f64, []f64)
```

> I will not dive into the details of the implementation here, but instead highlight two observations:

> i) Futhark is a pure functional language, so a language without side effects or I/O. The only way to communicate with the outside world is either through the `main`, or another `entry` point function. So while my source file includes a number of additional helper functions, there is only one entry point. We will see below how this entry function can be called from C, C++ and eventually R.
> ii) Futhark itself has an ecosystem of software libraries that can be installed and imported directly from GitHub with the compiler executable `futhark`. So the compiler software conveniently comes with a package manager build in! I made use of a dependency for a faster sorting algorithm (`import "lib/github.com/diku-dk/sorts/radix_sort"`). For that it needs to be registered in a [`futhark.pkg`](https://github.com/nevrome/ggpointgrid/blob/1.4.0/src/futhark.pkg) file in the `src/` directory, which can be automatically created by calling `futhark pkg add github.com/diku-dk/sorts`.

```txt
require {
  github.com/diku-dk/sorts 0.6.0 #f448cd990c86c566715915a8fc4182c8c2ff5400
}
```

> > If such a file is present, then the `futhark` command line software can install the required dependencies to a `src/lib/` directory with `futhark pkg sync`. 

3. Use the compiler to translate the Futhark code to C. The command line tool supports two relevant interfaces for this use case: `futhark c` and `furthark multicore` (besides `futhark cuda`, `opencl`, `pyopencl`, `python`, `wasm` and `wasm-multicore`, which are all available in version 0.25.29.). The generated multicore code is currently only compatible with Unix operating systems ([because of a dependency on pthreads](https://futhark.readthedocs.io/en/latest/man/futhark-multicore.html)). To not exclude Windows users later on, I chose the the simple sequential `c` output. We can then transpile our code file like this:

```bash
futhark c --library arrange.fut
```

> The `--library` flag causes `futhark` to create a C library, instead of an executable. For testing purposes we can create the executable, and test the behaviour of our entry point function on the command line.

```bash
futhark c arrange.fut
echo [1,2,3,4,5] [1,2,3,4,5] [1,1,1,1] [1,1,1,1] | ./arrange -e arrange_from_coordinates
```
```txt
[1.000000000000000f64, 2.000000000000000f64, 3.000000000000000f64, 4.000000000000000f64]
[1.000000000000000f64, 2.000000000000000f64, 3.000000000000000f64, 4.000000000000000f64]
```

> This is very useful for debugging throughout the development process. Futhark also comes with powerful profiling features that are easily accessible like this.

> To avoid cluttering your Git repository with unnecessary files, the following addition to your .gitignore file may be sensible at this point:

```txt
# files generated by Futhark and gcc
*.o
*.so
src/lib/
src/arrange
```

### C

4. Inspect the resulting C code. `futhark c --library arrange.fut` generates a very large .c file [`arrange.c`](https://github.com/nevrome/ggpointgrid/blob/1.4.0/src/arrange.c), and a short header file [`arrange.h`](https://github.com/nevrome/ggpointgrid/blob/1.4.0/src/arrange.h). This header file contains the essential interface between C and Futhark, so functions and data types necessary to correctly call the entry function we defined above, and now want to thread through to R. It would now be possible to do this directly and call C code from R without another intermediate C++ layer. It seemed easier to me, though, to follow well-trodden paths and introduce Rcpp.

### C++

5. Setup Rcpp. Using Rcpp requires some changes to an R package, as documented in the vignette [here](https://cloud.r-project.org/web/packages/Rcpp/vignettes/Rcpp-package.pdf). The essential steps are i) adding `Imports: Rcpp (>= 0.11.0)` and `LinkingTo: Rcpp` to the package's DESCRIPTION file, and ii) adding `useDynLib(mypackage)` and `importFrom(Rcpp, evalCpp)` to the package's NAMESPACE file. If you're using [roxygen2](https://roxygen2.r-lib.org/) to generate the documentation for the package, which I strongly recommend, then you should not manually edit the NAMESPACE file, but instead add a [`zzz.R`](https://github.com/nevrome/ggpointgrid/blob/1.4.0/R/zzz.R) file in the `R/` directory with the following content:

```r
#' @useDynLib mypackage
#' @importFrom Rcpp evalCpp
NULL

#' @export
.onUnload <- function(libpath) {
  library.dynam.unload("mypackage", libpath)
}
```

> With that being setup and the Rcpp package itself installed, it should now be possible to use Rcpp in the package.

6. Write the C++ -> C -> Futhark bridge. This was the most challenging part for me in this process, and I required the help of an LLM to get it right. We need to define a C++ function that i) takes as input the relevant data types Rcpp introduces to represent R data types in C++ (e.g. `Rcpp::NumericVector`), ii) transforms them to the data types by which Futhark types are represented in C (e.g. `futhark_f64_1d`), iii) calls the C version of the Futhark entry point function (here `futhark_entry_arrange_from_coordinates`), and iv) finally transforms its output back to a meaningful Rcpp type. We also have to be careful about memory management. The Futhark runtime requires explicit creation and freeing of contexts and array objects. If we fail to do so correctly, then we can introduce memory leaks.

> My [`futhark_bridge_rcpp.cpp`](https://github.com/nevrome/ggpointgrid/blob/1.4.0/src/futhark_bridge_rcpp.cpp) may serve as an example, but the details depend on the input and output data types your application requires.

```C++
// [[Rcpp::depends(Rcpp)]]
#include <Rcpp.h>
extern "C" {
#include "arrange.h"  // generated by Futhark
}
using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::List futhark_entry_arrange_from_coordinates_cpp(
    Rcpp::NumericVector grid_xs,
    Rcpp::NumericVector grid_ys,
    Rcpp::NumericVector pts_x,
    Rcpp::NumericVector pts_y
) {
    ... // see the code file for the implementation details
}
```

7. Generate an [`RcppExports.cpp`](https://github.com/nevrome/ggpointgrid/blob/1.4.0/src/RcppExports.cpp) file and an [`RcppExports.R`](https://github.com/nevrome/ggpointgrid/blob/1.4.0/R/RcppExports.R) file with the `Rcpp::compileAttributes()` R function. To call `futhark_entry_arrange_from_coordinates_cpp` from R we need yet another two wrapper functions, one in C++ and one in R. Fortunately Rcpp can generate these automatically for us. Of course `compileAttributes()` must be rerun any time the type signature of `futhark_entry_arrange_from_coordinates_cpp` changes.

8. Compile the C and C++ code in the package. All components should be there now for compilation. To conveniently trigger this from R we can call `devtools::build()` or `devtools::document()`. The latter handles more additional details, so I would generally recommend to run this. Any coding mistakes in the previous steps will now lead to failure and must be addressed.

### R

9. Use the Futhark function in your R code. It can now already be called just like any other R function:

```r
futhark_entry_arrange_from_coordinates_cpp(
    c(1,2,3,4,5), c(1,2,3,4,5), c(1,1,1,1), c(1,1,1,1)
)
```
```txt
[[1]]
[1] 1 2 3 4

[[2]]
[1] 1 2 3 4
```

> For my use case I wrote a user-friendly wrapper around it in [`R/arrange_on_grid.R`](https://github.com/nevrome/ggpointgrid/blob/1.4.0/R/arrange_on_grid.R). Just to document it properly, validate inputs and slightly restructure the output for more convenience.

This concludes the essential steps I applied to use Futhark code in an R package. Please let me know if you find anything missing to get things to work!

## Cleaning up

In this process we added a number of files to the R package that are unusual and should be ignored in the package's automatic build process. We have to add them to the [`.Rbuildignore`](https://github.com/nevrome/ggpointgrid/blob/1.4.0/.Rbuildignore) file:

```txt
^src/arrange$
^src/arrange\.fut$
^src/arrange\.json$
^src/futhark\.pkg$
```

If you share this project on GitHub and want GitHub's [linguist](https://github.com/github-linguist/linguist) to generate a meaningful language breakdown graph, then you can add a [`.gitattributes`](https://github.com/nevrome/ggpointgrid/blob/1.4.0/.gitattributes) file with instructions to ignore the automatically generated C and C++ code:

```txt
*.c linguist-vendored
*.h linguist-vendored
src/RcppExports.cpp linguist-vendored
```

Otherwise the thousands of lines of generated C will dominate the graph.

![GitHub language graph of ggpointgrid v1.4.0 with the .gitattributes set as above.](/images/2026-01-12-futhark-in-ggpointgrid/language_graph.png){width=60%}

## R package checks

I did not submit `ggpointgrid` to the central R package archive [CRAN](https://cran.r-project.org/), but I still made sure to address ERRORs, WARNINGS, and NOTES revealed by `devtools::check()`. There are three, though, that emerge from the integration of Futhark, which are hard to avoid:

```txt
❯ checking pragmas in C/C++ headers and code ... WARNING
  File which contains non-portable pragma(s)
    ‘src/arrange.c’
  File which contains pragma(s) suppressing diagnostics:
    ‘src/arrange.c’

❯ checking compilation flags used ... NOTE
  Compilation used the following non-portable flag(s):
    ‘-Werror=format-security’ ‘-Wformat’ ‘-Wp,-D_FORTIFY_SOURCE=3’
    ‘-Wp,-D_GLIBCXX_ASSERTIONS’ ‘-march=x86-64’
    ‘-mno-omit-leaf-frame-pointer’

❯ checking compiled code ... NOTE
  File ‘ggpointgrid/libs/ggpointgrid.so’:
    Found ‘stderr’, possibly from ‘stderr’ (C)
      Object: ‘arrange.o’
  
  Compiled code should not call entry points which might terminate R nor
  write to stdout/stderr instead of to the console, nor use Fortran I/O
  nor system RNGs nor [v]sprintf.
  
  See ‘Writing portable packages’ in the ‘Writing R Extensions’ manual.
```

When I saw these, I was quick to ask [Troels Henriksen](https://sigkill.dk/), the mastermind behind Futhark, if the output of his transpiler could be tweaked slightly for this particular context. While he was rightfully hesitant to make such changes for a single user, he also gave me some helpful background information on what they mean and why he considers them less severe than they sound. See our exchange here: <https://archaeo.social/@ClemensSchmid/115157603838749836>. My understanding is now as follows:

1. **checking pragmas in C/C++ headers and code ... WARNING**: These pragmas in the C code are for disabling warnings in the compilation process. The R WARNING can thus be avoided simply by deleting a section of the generated C code in `arrange.c` (`#ifdef __clang__` to `#endif`). This renders the C compilation output more messy, but doesn't change any functionality either way.

2. **checking compilation flags used  ... NOTE**: These flags were set when R was built for my personal system, not the package. This NOTE should only pop up on my computer.

3. **checking compiled code ... NOTE**: Writing to `stderr` only happens in code paths that a user of the entry point function would not reach. I'm not sure if they could be removed easily, but they should not have a detrimental effect on R's functionality.

So 1. can be avoided relatively easily and 2. should be only relevant on certain systems. Only 3. may be an issue. Whether a CRAN submission would be allowed with this NOTE is unclear to me, but I think it would be worth a try.

## Final words

In the end I did manage to come up with a relatively convenient workflow to integrate Futhark in an R package. It was great fun to figure out the process step by step, and the result is production-ready: I have installed and used the ggpointgrid package a number of times since I made the change and never encountered any issues. I will most likely do this again, when I discover a clearly constrained bottleneck during R package development.

Regarding performance, the Futhark solution is, in this particular case, a bit, but not extremely much faster than my original R implementation. Major cost centers of the core algorithm are sorting and pairwise distance calculation. And for these I could already use very fast functions available through R package dependencies. Using the Furthark multicore C output would speed things up a bit more, but I decided not to use it for the moment. Troels signalled interest in making the multicore output Windows-compatible eventually, so I have something to look forward to!
