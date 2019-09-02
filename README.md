# CIViC DB API Client for R
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

## Updating documentation
1. Go to RStudio and open the project
2. Load devtools `library(devtools)`
3. Setwd to `./CIViC-R-API-Client/civicApiClient`
4. Run document()
