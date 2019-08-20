# CIViC API Client for the R programming language

# Load required libraries
library(httr)
library(jsonlite)

# Set some constants
baseAPIUrl <- "https://civicdb.org/"
userAgent <- user_agent("https://github.com/agduncan94/CIViC-R-API-Client")

#' Get a list of genes
#'
#' Retrieve all genes from the CIViC DB
#' @param page the page number to retrieve
#' @param count the number of genes to retrieve
#' @return An S3 Object of type civic_api containing the content, url, and response
#' @export
#' @keywords genes
#' @examples
#' getAllGenes(count = 10)
getAllGenes <- function(page = 1, count = 25) {
  url <- modify_url(baseAPIUrl, path = "api/genes")
  response <- GET(url, accept_json(), userAgent, query = list("page" = page, "count" = count))
  verifyJsonResponse(response)
  handleFailure(response)
  genes <- content(response, "parsed")

  return(structure(
    list(
      content = genes,
      url = url,
      response = response
    ),
    class = "civic_api"
  ))

}

handleFailure <- function(response) {
  if (http_error(response)) {
    errorResponse <- content(response, "parsed")
    stop(
      sprintf(
        "CIViC API request failed [%s]\n%s",
        status_code(response),
        errorResponse$error
      ),
      call. = FALSE
    )
  }
}

verifyJsonResponse <- function(response) {
  if (http_type(response) != "application/json") {
    stop("CIViC API did not return a JSON", call. = FALSE)
  }
}

