# BenchmarksWM

R data package for benchmark data in working memory research. 

## Purpose of project

The purpose of this forked repository is to generate an R data package from oberauer/BenchmarksWM. The package is intended to be useful for those interested in formal models of working memory, but also as a proof of principle for the distribution of benchmark datasets in formal modelling in other areas (e.g. category learning). 

## Reasons for project

Publicly-avaialble benchmark datasets are essential for the relative adequacy comparison of formal models (Wills & Pothos, 2012; Wills et al., 2017). Oberauer et al. (2018) proposed a substantial set of benchmark phenomena in the area of working memory research, perhaps the most comprehensive set yet proposed in any area of psychology. Oberauer (2019) collected data relating to these benchmarks in a github repository. 

This is an excellent development, but another step is needed to make the data provided by Oberauer (2019) suitable for efficient use by modellers -- the data needs to be in a standard format, with each data set well-documented (a 'codebook'), and with code that reproduces the benchmark (which is typically a summary) from the data provided.

R data packages provide a good medium for this additional step, because they are designed to associate data (.rda files) with relevant documentation. They also permit the syntactic checking of this documentation, which reduces the chance of errors, and they permit the inclusion of example code, which permits the inclusion of code that reproduces the benchmark from the data.

## Progress

2019-05-01: Oberauer et al.'s (2018) Benchmark WM1.1 drafted as a proof of principle, and sent out for consultation as package version 0.1.1 

## How to install

Download the latest release from the 'releases' directory to your local machine and use `R CMD INSTALL BenchmarksWM_0.1.1.tar.gz` from the command line. Or from within R, navigate to the directory containing the downloaded file and use `install.packages("BenchmarksWM_0.1.1.tar.gz", repos=NULL)`

## How to use
Once installed, type `library(BenchmarksWM)`. For an overview help file including a list of datasets, type `?BenchmarksWM`. For documentation on a particular dataset, type e.g. `?adam15`. To view a dataset, type e.g. `View(adam15)`. To reproduce a benchmark, use the example code provided in the documentation for that dataset.

## Repository structure

BenchmarksWM.Data - Original data from oberauer/BenchmarksWM

build - R code to convert the contents of BenchmarksWM.Data into .rda files in pkg/data

pkg - Directory containing the R package source. Use `R CMD build pkg` to generate the package.

releases - tar.gz files for each release

## References

Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM

Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A., Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma, W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward, G. (2018). Benchmarks for models of short-term and working memory. _Psychological Bulletin, 144_, 885-958.

Wills, A.J., O'Connell, G., Edmunds, C.E.R., & Inkster, A.B. (2017). Progress in modeling through distributed collaboration: Concepts, tools, and category-learning examples. _Psychology of Learning and Motivation, 66_, 79-115.

Wills, A.J., & Pothos, E.M. (2012). On the adequacy of current empirical evaluations of formal models of categorization. _Psychological Bulletin, 138_, 102-125
