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
  .verifyJsonResponse(response)
  .handleFailure(response)
  genes <- content(response, "parsed")
  return(.createReturnStructure(genes, url, response))
}

#' Get a specific gene
#'
#' Retrieve a specific gene from the CIViC DB
#' @param id ID of the gene of interest
#' @param identifier_type Type of gene identifier (entrez_id, entrez_symbol, civic_id)
#' @return An S3 Object of type civic_api containing the content, url, and response
#' @export
#' @keywords gene
#' @examples
#' getGene(id = 1)
#' getGene(id = "ALK", identifier_type = "entrez_symbol")
#' getGene(id = 238, identifier_type = "entrez_id")
getGene <- function(id, identifier_type = "civic_id") {
  url <- modify_url(baseAPIUrl, path = paste("api/genes", id, sep = "/"))
  response <- GET(url, accept_json(), userAgent, query = list("identifier_type" = identifier_type))
  .verifyJsonResponse(response)
  .handleFailure(response)
  gene <- content(response, "parsed")
  return(.createReturnStructure(gene, url, response))
}

#' Get a list of variants
#'
#' Retrieve all variants from the CIViC DB
#' @param page the page number to retrieve
#' @param count the number of variants to retrieve
#' @return An S3 Object of type civic_api containing the content, url, and response
#' @export
#' @keywords variants
#' @examples
#' getAllVariants(count = 10)
getAllVariants <- function(page = 1, count = 25) {
  url <- modify_url(baseAPIUrl, path = "api/variants")
  response <- GET(url, accept_json(), userAgent, query = list("page" = page, "count" = count))
  .verifyJsonResponse(response)
  .handleFailure(response)
  variants <- content(response, "parsed")
  return(.createReturnStructure(variants, url, response))
}

#' Get a specific variant
#'
#' Retrieve a specific variant from the CIViC DB
#' @param id Internal CIViC ID of the variant of interest
#' @return An S3 Object of type civic_api containing the content, url, and response
#' @export
#' @keywords variant
#' @examples
#' getVariant(id = 1)
getVariant <- function(id) {
  url <- modify_url(baseAPIUrl, path = paste("api/variants", id, sep = "/"))
  response <- GET(url, accept_json(), userAgent)
  .verifyJsonResponse(response)
  .handleFailure(response)
  variant <- content(response, "parsed")
  return(.createReturnStructure(variant, url, response))
}

#' Get a list of evidence items
#'
#' Retrieve all evidence items from the CIViC DB
#' @param page the page number to retrieve
#' @param count the number of evidence items to retrieve
#' @return An S3 Object of type civic_api containing the content, url, and response
#' @export
#' @keywords evidence items
#' @examples
#' getAllEvidenceItems(count = 10)
getAllEvidenceItems <- function(page = 1, count = 25) {
  url <- modify_url(baseAPIUrl, path = "api/evidence_items")
  response <- GET(url, accept_json(), userAgent, query = list("page" = page, "count" = count))
  .verifyJsonResponse(response)
  .handleFailure(response)
  evidenceItems <- content(response, "parsed")
  return(.createReturnStructure(evidenceItems, url, response))
}

#' Get a specific evidence item
#'
#' Retrieve a specific evidence item from the CIViC DB
#' @param id Internal CIViC ID of the evidence item of interest
#' @return An S3 Object of type civic_api containing the content, url, and response
#' @export
#' @keywords evidence item
#' @examples
#' getEvidenceItem(id = 1)
getEvidenceItem <- function(id) {
  url <- modify_url(baseAPIUrl, path = paste("api/evidence_items", id, sep = "/"))
  response <- GET(url, accept_json(), userAgent)
  .verifyJsonResponse(response)
  .handleFailure(response)
  evidenceItem <- content(response, "parsed")
  return(.createReturnStructure(evidenceItem, url, response))
}

#' Get a list of variant groups
#'
#' Retrieve all variant groups from the CIViC DB
#' @param page the page number to retrieve
#' @param count the number of variant groups to retrieve
#' @return An S3 Object of type civic_api containing the content, url, and response
#' @export
#' @keywords variant groups
#' @examples
#' getAllVariantGroups(count = 10)
getAllVariantGroups <- function(page = 1, count = 25) {
  url <- modify_url(baseAPIUrl, path = "api/variant_groups")
  response <- GET(url, accept_json(), userAgent, query = list("page" = page, "count" = count))
  .verifyJsonResponse(response)
  .handleFailure(response)
  variantGroups <- content(response, "parsed")
  return(.createReturnStructure(variantGroups, url, response))
}

#' Handle failure case for httr
#' 
#' @param response httr error response
.handleFailure <- function(response) {
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

#' Verify that the httr response is of type "application/json"
#' 
#' @param response httr response
.verifyJsonResponse <- function(response) {
  if (http_type(response) != "application/json") {
    stop("CIViC API did not return a JSON", call. = FALSE)
  }
}

#' Create an s3 return structure for exported functions
#' 
#' @param content Content of the response
#' @param url URL of the request
#' @param response httr response
#' @return An S3 Object of type civic_api containing the content, url, and response
.createReturnStructure <- function(content, url, response) {
  return(structure(
    list(
      content = content,
      url = url,
      response = response
    ),
    class = "civic_api"
  ))
}

