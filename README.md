
<!-- README.md is generated from README.Rmd. Please edit that file -->

**NOTE: This is an incomplete package, more functions are to come.**

# lorR

<!-- badges: start -->
<!-- badges: end -->

The package can be used as handler for Legends of Runeterra API methods.

A series of utility functions to transform the outputs from the API are
also included.

## Installation

You can install the development version of lorR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("MaouLegna/lorR")
#> Downloading GitHub repo MaouLegna/lorR@HEAD
#> glue (1.4.2 -> 1.5.0) [CRAN]
#> Installing 1 packages: glue
#> Installazione pacchetto in 'C:/Users/Valentino Vazzoler/AppData/Local/Temp/RtmpcP2FfD/temp_libpath474053f1117'
#> (perché 'lib' non è specificato)
#> 
#>   C'è una versione binaria disponibile, ma la versione con le sorgenti
#>   è successiva:
#>      binary source needs_compilation
#> glue  1.4.2  1.5.0              TRUE
#> installazione pacchetto sorgenti 'glue'
#>          checking for file 'C:\Users\Valentino Vazzoler\AppData\Local\Temp\Rtmp8SjC7b\remotes4785bd42fd0\MaouLegna-lorR-79bdd21/DESCRIPTION' ...  v  checking for file 'C:\Users\Valentino Vazzoler\AppData\Local\Temp\Rtmp8SjC7b\remotes4785bd42fd0\MaouLegna-lorR-79bdd21/DESCRIPTION' (623ms)
#>       -  preparing 'lorR':
#>    checking DESCRIPTION meta-information ...     checking DESCRIPTION meta-information ...   v  checking DESCRIPTION meta-information
#>       -  checking for LF line-endings in source and make files and shell scripts
#>       -  checking for empty or unneeded directories
#>       -  building 'lorR_0.0.0.9000.tar.gz'
#>      
#> 
#> Installazione pacchetto in 'C:/Users/Valentino Vazzoler/AppData/Local/Temp/RtmpcP2FfD/temp_libpath474053f1117'
#> (perché 'lib' non è specificato)
```

## Setup

As an API key is necessary to work with the API methods, use the
following to set up a Sys.setenv variable containing the key.

``` r
lorR::setAPIkey("RGAPI-01234567-abcd-8ef9-abcd-0123456789AB")
```

You can also put this in your .Rprofile for your project so you don’t
have to type it every time.

## Usage

The library provides a series of functions wraps the methods of the LoR
API.

The outputs are returned either clean or json text format.

## Note

The package is still incomplete missing several fundamental functions.

Options beings considered:

-   Working with setting that assume a Developer Key

-   Working with multiple Developer Key
