#' @importFrom BiocFileCache bfcadd bfcquery bfcremove bfcrid bfcrpath
#' @importFrom BiocFileCache BiocFileCache
#' @importFrom httr2 req_body_json req_perform req_url_path_append
#' @importFrom httr2 request resp_body_json resp_body_raw
#' @importFrom IRanges CharacterList IntegerList
#' @importFrom jsonlite unbox
#' @importFrom purrr list_transpose
#' @importFrom S4Vectors DataFrame
#' @importFrom yaml read_yaml
NULL

url_toppgene <- function() {
    getOption("TOPPGENE_API_URL", "https://toppgene.cchmc.org/API")
}

## The cache is only used to check the API verison once a week; do not cache
## other requests to ToppGene because ToppGene's databases are regularly
## updated and caching those would yield inconsistent results.
cache_toppgene <- function() {
    getOption("TOPPGENE_CACHE", tools::R_user_dir("toppgene", which = "cache"))
}

cache_expiration_toppgene <- function() {
    getOption("TOPPGENE_CACHE_API_EXPIRATION_DAYS", 7L)
}

file_openapi_yaml <- function(bfc = NULL) {
    if (is.null(bfc)) {
        bfc <- BiocFileCache(cache_toppgene(), ask = FALSE)
    }
    ## Download if necessary.
    url <- file.path(url_toppgene(), "openapi.yaml")
    for (iter in c("create", "update")) {
        df <- bfcquery(bfc, basename(url), field = "rname", exact = TRUE)
        if (! nrow(df)) {
            file_yaml <- bfcadd(
                bfc,
                rname = basename(url),
                fpath = url,
                rtype = "web",
                progress = FALSE,
                fname = "exact")
            df <- bfcquery(bfc, basename(url), field = "rname", exact = TRUE)
            break
        }
        ## BiocFileCache currently lacks forcing the expiration time of a
        ## remote resource (Bioconductor/BiocFileCache#36), therefore manually
        ## add and remove database resources.
        stopifnot(nrow(df) == 1L)
        if (difftime(as.POSIXlt(df$create_time), Sys.time(), units = "days") >=
                cache_expiration_toppgene()) {
            bfcremove(bfc, rids = bfcrid(df))
        }
    }
    file_yaml <- bfcrpath(bfc, rids = bfcrid(df))
    file_yaml
}

api_version_toppgene <- function(bfc = NULL) {
    yaml <- read_yaml(file_openapi_yaml(bfc))
    version <- yaml[["info"]][["version"]]
    version
}

.checkAPIVersionToppGene <- function(bfc = NULL) {
    version_expected <- "1.0.0"
    version_actual <- api_version_toppgene(bfc)
    if (version_actual != version_expected) {
        warning(
            "ToppGene API has changed! Found ", version_actual,
            ", expected ", version_expected)
    }
}

EmptyLookupDFEntrez <- function() {
    DataFrame(
        OfficialSymbol = character(),
        Entrez = integer(),
        Submitted = character(),
        Description = character())
}

#' Return integer Entrez IDs from gene symbols, ensembl references, etc.
#'
#' The ToppGene API returns many lookup differs from Bioconductor's identifier
#' lookup, therefore we have to use the web API instead of the typical
#' Bioconductor functions of [GSEABase::mapIdentfiers()],
#' [AnnotationDbi::mapIds()], etc.
#'
#' @param symbols Character vector of genes.
#' @param max_tries Number of attempts passed on to httr2::req_retry.
#' @return DataFrame with 4 columns: "Submitted" symbol character vector in the
#'     same order as the symbols input parameter, corresponding "Entrez"
#'     integer IDs, "OfficialSymbol" character vector, and "Description" of
#'     gene.
#' @export
#' @examples
#' # Sample lookup call of the ToppGene API specification:
#' # - FLDB is an obsolete symbol for APOB.
#' # - APOE is the current symbol for APOE.
#' # - ENSG00000113196 is an ensembl gene symbol for HAND1.
#' # - ENSMUSG00000020287 is a mouse gene MPG.
#' lookup(c("FLDB", "APOE", "ENSG00000113196", "ENSMUSG00000020287"))
lookup <- function(symbols, max_tries = 3L) {
    stopifnot(is.character(symbols))
    .checkAPIVersionToppGene()
    if (length(symbols) == 1L) {
        ## Ensure the JSON input is always a list.
        symbols <- list(symbols)
    }
    response <-
        url_toppgene() |>
        request() |>
        req_url_path_append("lookup") |>
        req_body_json(list(Symbols = symbols)) |>
        req_retry(max_tries = max_tries) |>
        req_perform()
    content <-
        response |>
        resp_body_json() |>
        (function(x) x[["Genes"]])()
    if (is.null(content)) {
        warning("No matching gene symbols found")
        return(EmptyLookupDFEntrez())
    }
    lists <- list_transpose(content, simplify = FALSE)
    df <- DataFrame(lapply(lists, unlist))
    if (nrow(df) != length(symbols)) {
        warning("Some gene symbols were not found")
    }
    df
}

EmptyEnrichDF <- function() {
    DataFrame(
        Category = character(),
        ID = character(),
        Name = character(),
        PValue = double(),
        QValueFDRBH = double(),
        QValueFDRBY = double(),
        QValueBonferroni = double(),
        TotalGenes = integer(),
        GenesInTerm = integer(),
        GenesInQuery = integer(),
        GenesInTermInQuery = integer(),
        Source = character(),
        URL = character(),
        GenesEntrez = IntegerList(),
        GenesSymbol = CharacterList())
}

#' Return functional enrichment of gene Entrez IDs.
#'
#' The ToppGene API returns many [CATEGORIES] of gene list erichment.
#'
#' @param entrez_ids Integer vector of genes.
#' @param categories If no categories are provided, return all categories.
#' @param max_tries Number of attempts passed on to httr2::req_retry.
#' @return DataFrame with 15 columns containing the enrichment Category, ID,
#'     and associated data.
#' @export
#' @examples
#' # Sample functional enrichment calls of the ToppGene API specification:
#' enrich(2L)
#' enrich(as.integer(c(1482, 4205, 2626, 9421, 9464, 6910, 6722)))
enrich <- function(entrez_ids, categories = CategoriesDataFrame(),
    max_tries = 3L) {
    stopifnot(is.integer(entrez_ids))
    .checkAPIVersionToppGene()
    if (length(entrez_ids) == 1L) {
        ## Ensure the JSON input is always a list.
        entrez_ids <- list(entrez_ids)
    }
    req_data <- list(Genes = entrez_ids)
    if (! is.null(categories)) {
        stopifnot(is(categories, "CategoriesDataFrame"))
        ## NB: There is a bug with the API requiring all categories to be
        ## specified otherwise the web request silently fails and returns NULL.
        req_data$Categories <-
            cbind(
                Type = rownames(categories),
                as.data.frame(categories)) |>
            as.list() |>
            list_transpose(simplify = FALSE) |>
            ## Prevent inner-most item conversion to a JSON list using unbox(),
            ## otherwise the web request silently fails and returns NULL.
            (function(x) {
                for (type_ in seq_along(x)) {
                    for (item in seq_along(x[[type_]])) {
                        x[[type_]][[item]] <- unbox(x[[type_]][[item]])
                    }
                }
                x
            })()
    }
    response <-
        url_toppgene() |>
        request() |>
        req_url_path_append("enrich") |>
        req_body_json(req_data) |>
        req_retry(max_tries = max_tries) |>
        req_perform()
    lists <-
        response |>
        resp_body_json() |>
        (function(x) x[["Annotations"]])()
    ## The server sometimes returns NULL if the request failed.
    if (is.null(lists)) {
        warning("No results for this query; try relaxing categories PValue")
        return(EmptyEnrichDF())
    }
    lists <- list_transpose(lists)
    ## Extract the nested lists from Genes.
    lists$Genes <- lapply(lists$Genes, list_transpose, simplify = FALSE)
    lists$GenesEntrez <- IntegerList(
        lapply(lists$Genes, function(x) x$Entrez))
    lists$GenesSymbol <- CharacterList(
        lapply(lists$Genes, function(x) x$Symbol))
    lists$Genes <- NULL
    DataFrame(lists)
}
