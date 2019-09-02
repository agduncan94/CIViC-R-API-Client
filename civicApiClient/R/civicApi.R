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
  return(.commonIndexEndpoint("genes", page, count))
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

#' Get a specific gene metadata
#'
#' Retrieve metadata for a specific gene from the CIViC DB
#' @param id ID of the gene of interest
#' @param identifier_type Type of gene identifier (entrez_id, entrez_symbol, civic_id)
#' @param type Type of metadata (comments, suggested_changes, revisions, variants)
#' @return An S3 Object of type civic_api containing the content, url, and response
#' @export
#' @keywords gene, metadata
#' @examples
#' getGeneMetadata(id = 1, type = "comments")
#' getGeneMetadata(id = "ALK", type = "comments", identifier_type = "entrez_symbol")
#' getGeneMetadata(id = 238, type = "comments", identifier_type = "entrez_id")
getGeneMetadata <- function(id, type, identifier_type = "civic_id") {
  url <- modify_url(baseAPIUrl, path = paste("api/genes", id, type, sep = "/"))
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
  return(.commonIndexEndpoint("variants", page, count))
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
  return(.commonDetailEndpoint("variants", id))
}

#' Get a specific variant metadata information
#'
#' Retrieve metadata information for a specific variant from the CIViC DB
#' @param id Internal CIViC ID of the variant of interest
#' @param type Type of metadata (comments, suggested_changes, revisions, evidence_items)
#' @return An S3 Object of type civic_api containing the content, url, and response
#' @export
#' @keywords variant, metadata
#' @examples
#' getVariantMetadata(id = 1, type = "comments")
getVariantMetadata <- function(id, type) {
  return(.commonDetailEndpoint("variants", id, type))
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
  return(.commonIndexEndpoint("evidence_items", page, count))
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
  return(.commonDetailEndpoint("evidence_items", id))
}

#' Get a specific evidence item metadata information
#'
#' Retrieve metadata information for a specific evidence item from the CIViC DB
#' @param id Internal CIViC ID of the evidence item of interest
#' @param type Type of metadata (comments, suggested_changes, revisions)
#' @return An S3 Object of type civic_api containing the content, url, and response
#' @export
#' @keywords evidence item, metadata
#' @examples
#' getEvidenceItemMetadata(id = 1, type = "comments")
getEvidenceItemMetadata <- function(id, type) {
  return(.commonDetailEndpoint("evidence_items", id, type))
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
  return(.commonIndexEndpoint("variant_groups", page, count))
}

#' Get a specific variant group
#'
#' Retrieve a specific variant group from the CIViC DB
#' @param id Internal CIViC ID of the variant group of interest
#' @return An S3 Object of type civic_api containing the content, url, and response
#' @export
#' @keywords variant group
#' @examples
#' getVariantGroup(id = 1)
getVariantGroup <- function(id) {
  return(.commonDetailEndpoint("variant_groups", id))
}

#' Get a specific variant group metadata information
#'
#' Retrieve metadata information for a specific variant group from the CIViC DB
#' @param id Internal CIViC ID of the variant group of interest
#' @param type Type of metadata (comments, suggested_changes, revisions)
#' @return An S3 Object of type civic_api containing the content, url, and response
#' @export
#' @keywords variant group, metadata
#' @examples
#' getVariantGroupMetadata(id = 1, type = "comments")
getVariantGroupMetadata <- function(id, type) {
  return(.commonDetailEndpoint("variant_groups", id, type))
}

#' Get a list of assertions
#'
#' Retrieve all assertions from the CIViC DB
#' @param page the page number to retrieve
#' @param count the number of assertions to retrieve
#' @return An S3 Object of type civic_api containing the content, url, and response
#' @export
#' @keywords assertions
#' @examples
#' getAllAssertions(count = 10)
getAllAssertions <- function(page = 1, count = 25) {
  return(.commonIndexEndpoint("assertions", page, count))
}

#' Get a specific assertion
#'
#' Retrieve a specific assertion from the CIViC DB
#' @param id Internal CIViC ID of the assertion of interest
#' @return An S3 Object of type civic_api containing the content, url, and response
#' @export
#' @keywords assertion
#' @examples
#' getAssertion(id = 1)
getAssertion <- function(id) {
  return(.commonDetailEndpoint("assertions", id))
}

#' Get a specific assertion metadata information
#'
#' Retrieve metadata information for a specific variant group from the CIViC DB
#' @param id Internal CIViC ID of the assertion of interest
#' @param type Type of metadata (comments, suggested_changes, revisions)
#' @return An S3 Object of type civic_api containing the content, url, and response
#' @export
#' @keywords assertion, metadata
#' @examples
#' getAssertionMetadata(id = 1, type = "comments")
getAssertionMetadata <- function(id, type) {
  return(.commonDetailEndpoint("assertions", id, type))
}

#' Handle common index endpoints
#' 
#' @param type Type of index endpoint
#' @param page the page number to retrieve
#' @param count the number of assertions to retrieve
.commonIndexEndpoint <- function(type, page, count) {
  url <- modify_url(baseAPIUrl, path = paste("api", type, sep = "/"))
  response <- GET(url, accept_json(), userAgent, query = list("page" = page, "count" = count))
  .verifyJsonResponse(response)
  .handleFailure(response)
  indexResponse <- content(response, "parsed")
  return(.createReturnStructure(indexResponse, url, response))
}

#' Handle common detail endpoints
#' 
#' @param type Type of detail endpoint
#' @param id The internal CIViC ID
#' @param metadataType Optional type for base detail endpoints
.commonDetailEndpoint <- function(type, id, metadataType=NULL) {
  appendedPath <- paste("api", type, id, sep = "/")
  if (!is.null(metadataType)) {
    appendedPath <- paste(appendedPath, metadataType, sep = "/")
  }
  url <- modify_url(baseAPIUrl, path = appendedPath)
  print(url)
  response <- GET(url, accept_json(), userAgent)
  .verifyJsonResponse(response)
  .handleFailure(response)
  detailResponse <- content(response, "parsed")
  return(.createReturnStructure(detailResponse, url, response))
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

