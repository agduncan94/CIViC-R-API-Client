# CIViC DB API Client for R
[![Build Status](https://travis-ci.org/agduncan94/CIViC-R-API-Client.svg?branch=develop)](https://travis-ci.org/agduncan94/CIViC-R-API-Client)

A simple (unofficial) R package for interacting with the [CIViC DB](https://civicdb.org/home) API.

See the official [CIViC DB API Documentation](https://griffithlab.github.io/civic-api-docs/) for more detailed information on the API.

Benefits of using this package
* Don't have to write your own functions to talk to the CIViC API
* Response objects stored as custom S3 objects
* HTTP errors are converted to R errors

## Contributing
Any contribution is much appreciated, whether it be in the form of bug reports or pull requests.

## Bug Reports/Feature Requests
If you find any bugs or have any feature requests then please create an issue in this repository.

## Building package
1. Open a terminal and go to `./CIViC-R-API-Client/`
2. Run `R CMD build civicApiClient`
3. Run `R CMD INSTALL civicApiClient_<version>.tar.gz`
4. Check that it passes CRAN checks `R CMD check --as-cran civicApiClient_<version>.tar.gz`

## Updating documentation
1. Go to RStudio and open the project
2. Load devtools `library(devtools)`
3. Setwd to `./CIViC-R-API-Client/civicApiClient`
4. Run document()
