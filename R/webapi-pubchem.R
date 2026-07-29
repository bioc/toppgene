#' @importFrom httr2 req_body_raw req_headers req_perform req_perform_parallel
#' @importFrom httr2 req_retry req_throttle req_url_path_append request
#' @importFrom httr2 resp_body_xml
#' @importFrom purrr list_transpose
#' @importFrom readr read_tsv
#' @importFrom S4Vectors DataFrame endoapply List merge
#' @importFrom stats setNames
#' @importFrom xml2 as_xml_document xml_add_child xml_attr xml_dtd
#' @importFrom xml2 xml_find_first xml_new_root xml_text
NULL

url_pubchem <- function() {
    getOption(
        "PUBCHEM_API_URL",
        "https://pubchem.ncbi.nlm.nih.gov/pug/pug.cgi")
}

req_pubchem <- function(xml_document) {
    xml <-
        xml_dtd(
            "PCT-Data",
            "-//NCBI//NCBI PCTools/EN",
            "NCBI_PCTools.dtd") |>
        xml_new_root() |>
        xml_add_child(xml_document)
    request(url_pubchem()) |>
        req_body_raw(as.character(xml)) |>
        req_headers("content-type" = "application/xml")
}

xml_pubchem_query <- function(ids, registry = NULL) {
    if (! is.null(registry) && registry == "SYNONYM") {
        registry <- NULL
    }
    if (! is.null(registry)) {
        stopifnot(is.character(registry))
        stopifnot(length(registry) == 1L)
    }
    stopifnot(is.character(ids))
    stopifnot(length(ids) >= 1L)
    if (is.null(registry)) {
        input <- list(
            "PCT-QueryUids" = list(
                "PCT-QueryUids_synonyms" =
                    setNames(
                        lapply(ids, function(x) list(x)),
                        rep("PCT-QueryUids_synonyms_E",
                            length(ids)))))
    } else {
        input <- list(
            "PCT-QueryUids" = list(
                "PCT-QueryUids_source-ids" = list(
                    "PCT-RegistryIDs" = list(
                        "PCT-RegistryIDs_source-name" = list(
                            registry),
                        "PCT-RegistryIDs_source-ids" =
                            setNames(
                                lapply(ids, function(x) list(x)),
                                rep("PCT-RegistryIDs_source-ids_E",
                                    length(ids)))))))
    }
    id_exchange <- list(
        "PCT-QueryIDExchange" = list(
            "PCT-QueryIDExchange_input" = input,
            "PCT-QueryIDExchange_operation-type" =
                structure(list(), value = "same"),
            "PCT-QueryIDExchange_output-type" =
                structure(list(), value = "cid"),
            "PCT-QueryIDExchange_output-method" =
                structure(list(), value = "file-pair"),
            "PCT-QueryIDExchange_compression" =
                structure(list(), value = "gzip")
        ))
    as_xml_document(list(
        "PCT-Data" = list(
            "PCT-Data_input" = list(
                "PCT-InputData" = list(
                    "PCT-InputData_query" = list(
                        "PCT-Query" = list(
                            "PCT-Query_type" = list(
                                "PCT-QueryType" = list(
                                    "PCT-QueryType_id-exchange" =
                                        id_exchange)))))))))
}

xml_pubchem_status <- function(reqid) {
    stopifnot(is.character(reqid))
    stopifnot(length(reqid) == 1L)
    as_xml_document(list(
        "PCT-Data" = list(
            "PCT-Data_input" = list(
                "PCT-InputData" = list(
                    "PCT-InputData_request" = list(
                        "PCT-Request" = list(
                            "PCT-Request_reqid" = list(
                                reqid),
                            "PCT-Request_type" = structure(
                                list(), value = "status"))))))))
}

SOURCES_DRUG <- c(
    "Broad Institute CMAP Down",
    "Broad Institute CMAP Up",
    "CTD",
    "Drug Bank",
    "Stitch")

## PubChem RegistryIDs corresponding to the above ToppGene Drug Source IDs.
SOURCES_REGISTRY <- function(source, synonym = NULL) {
    switch(
        source,
        "Broad Institute CMAP Down" = synonym, # synonym instead of registry.
        "Broad Institute CMAP Up" = synonym,   # synonym instead of registry.
        "CTD" = "Comparative Toxicogenomics Database",
        "Drug Bank" = "DrugBank",
        "Stitch" = NA, # Stitch IDs already are CIDs.
        "NOT-FOUND")   # Default.
}

.validSources <- function(df) {
    is_known_source <- df$Source %in% SOURCES_DRUG
    if (! all(is_known_source)) {
        unknown_names <- unique(df$Source[! is_known_source])
        stop(
            "Unknown drug sources ", paste(unknown_names, collapse = ", "),
            " at row(s) ", paste(which(! is_known_source), collapse = ", "))
    }
}

EmptyLookupDFPubChem <- function() {
    DataFrame(
        Source = character(),
        ID = character(),
        CID = character())
}

req_pubchem_query <- function(ids, registry = NULL) {
    ## Submit query to the queue.
    req_pubchem(xml_pubchem_query(ids, registry))
}

parse_pubchem_query <- function(resp) {
    ## Parse queued response for request ID.
    reqid <-
        resp |>
        resp_body_xml() |>
        xml_find_first("//PCT-Waiting_reqid") |>
        xml_text()
    stopifnot(reqid != "")
    reqid
}

check_pubchem_status <- function(resp) {
    status <-
        resp_body_xml(resp) |>
        xml_find_first("//PCT-Status") |>
        xml_attr("value")
    status != "success"
}

req_pubchem_status <- function(reqid) {
    ## Repeat status requests until success.
    req_pubchem(xml_pubchem_status(reqid)) |>
        req_retry(
            is_transient = check_pubchem_status,
            max_tries = 100L,
            backoff = \(x) ifelse(x < 20, 0.5, 5))
}

req_pubchem_status_no_retry <- function(reqid) {
    req_pubchem(xml_pubchem_status(reqid))
}

parse_pubchem_status <- function(resp) {
    ## Download the result of the registry ID(s) conversion.
    resp |>
        resp_body_xml() |>
        xml_find_first("//PCT-Download-URL") |>
        xml_text() |>
        ## The download URL that PubChem returns is an unencrypted ftp that
        ## will always timeout because to comply with federal law PubChem is
        ## currently transitioning to encypted protocols.  Therefore, download
        ## using the https protocol.
        sub(pattern = "^[^:]+:", replacement = "https:") |>
        read_tsv(col_names = c("ID_", "CID"), col_types = "cc") |>
        ## Remove the tibble "problems" pointer and "col_spec" class list.
        as.data.frame() |>
        as("DataFrame")
}

#' Return table of drug identifiers to PubChem CIDs.
#'
#' Many downstream drug analyses in Bioconductor make use of PubChem CIDs but
#' ToppGened drug identifiers require changes prior to conversion, and the
#' conversion itself is involved when scaling to large lists of identifiers.
#'
#' Therefore this function submits queries to the PubChem Power User Gateway
#' (PUG) in parallel for each query.
#'
#' @param df DataFrame with 15 columns containing the enrichment Category, ID,
#'     and associated data.
#' @return DataFrame with 2 columns subset to Category == Drug containing input
#'     Source, ID, and output CID.
#' @export
#' @examples
#' library(DFplyr)
#' cats <- CategoriesDataFrame()
#' cats <-
#'     cats |>
#'     mutate(
#'         PValue = case_when(
#'             grepl("Drug", rownames(cats)) ~ PValue,
#'             .default = 1e-100),
#'         MaxResults = case_when(
#'             grepl("Drug", rownames(cats)) ~ 1000L,
#'             .default = 1L))
#' # EGFR gene that has hits in all drug databases.
#' df_enrich <- toppgene::enrich(1956L, cats)
#' df_cid <- lookup_pubchem(df_enrich)
#' df_cid
lookup_pubchem <- function(df) {
    if (nrow(df) == 0L) {
        return(EmptyLookupDFPubChem())
    }
    if (all(df$Category == "Drug")) {
        ## Edge case for subset() with only Drug Category entries.
        df_drug <- df
    } else {
        df_drug <- subset(df, df$Category == "Drug")
    }
    if (nrow(df_drug) == 0L) {
        return(EmptyLookupDFPubChem())
    }
    .validSources(df_drug)
    ## The Stitch database already has CIDs and only requires reformatting the
    ## string.
    df_res <-
        df_drug |>
        subset(df_drug$Source == "Stitch", select = c("Source", "ID")) |>
        (function(x) {
            x[["CID"]] <- sub("^CID[0]*", "", x$ID)
            x
        })()
    ## Split DataFrame to prepare parallel network requests for each PubChem
    ## registry.
    dfs_reg <-
        df_drug |>
        subset(df_drug$Source != "Stitch") |>
        (function(x) {
            x[["Registry"]] <-
                vapply(
                    x$Source, SOURCES_REGISTRY, "", synonym = "SYNONYM",
                    USE.NAMES = FALSE)
            x$Registry <- unlist(x$Registry, recursive = FALSE)
            x
        })() |>
        (function(x) {
            ## There is a bug in spliting an S4 DataFrame where it splits on
            ## columns instead of rows!  Therefore manually split
            regs <- unique(x$Registry)
            names(regs) <- regs
            lapply(
                regs,
                function(reg) {
                    subset(x, x$Registry == reg, select = -ncol(x))
                })
        })()
    if (length(dfs_reg) == 0L) {
        return(df_res)
    }
    lists_req <-
        dfs_reg |>
        endoapply(subset, select = c("ID", "Name"))
    if ("SYNONYM" %in% names(lists_req)) {
        lists_req$SYNONYM$ID <-
            lists_req$SYNONYM$Name |>
            sub(pattern = " [[].*", replacement = "") |>
            sub(pattern = ";.*", replacement = "")
    }
    if ("Comparative Toxicogenomics Database" %in% names(lists_req)) {
        lists_req$`Comparative Toxicogenomics Database`$ID <-
            lists_req$`Comparative Toxicogenomics Database`$ID |>
            sub(pattern = "^ctd:", replacement = "")
    }
    lists_req <-
        lists_req |>
        (function(x) {
            list(
                ids = lapply(unname(x), function(x) x$ID),
                registry =
                    names(x) |>
                    lapply(\(x) x))
        })() |>
        list_transpose(simplify = FALSE)
    ## Queue parallel network queries to minimize time waiting for responses.
    resps <-
        lapply(
            lists_req,
            function(x) {
                req_pubchem_query(x$ids, x$registry) |>
                    req_throttle(capacity = 30)
            }) |>
        req_perform_parallel(progress = FALSE)
    ## Retrieve request IDs.
    reqids <- lapply(resps, parse_pubchem_query)
    ## Queue parallel network statuses to minimize time waiting for responses.
    ##
    ## However, note that there is a bug with req_perform_parallel() not
    ## calling the is_transient function of req_retry() even though this
    ## capability is intended to be supported per the "can retry a transient
    ## error" unit test in httr2 test suite for req_perform_parallel().
    retry <- rep(TRUE, length(reqids))
    i <- 1L
    while (i <= 100L && any(retry)) {
        ## Workaround for req_retry(is_transient = ...) not working with
        ## req_perform_parallel().
        i <- i + 1L
        ## Only requery failed queries.
        resps[retry] <-
            lapply(
                reqids[retry],
                \(x) req_pubchem_status_no_retry(x)) |>
            req_perform_parallel(progress = FALSE)
        Sys.sleep(0.5)
        retry[retry] <-
            vapply(resps[retry], check_pubchem_status, logical(1))
    }
    ## Retrieve DataFrames.
    dfs_res <- lapply(resps, parse_pubchem_status)
    unlist(List(dfs_reg), use.names = FALSE) |>
        subset(select = c("Source", "ID")) |>
        (function(x) {
            x[["ID_"]] <-
                lists_req |>
                list_transpose(simplify = FALSE) |>
                (function(x) unlist(x$ids))()
            x
        })() |>
        merge(unlist(List(dfs_res), use.names = FALSE), by = "ID_") |>
        subset(select = c("Source", "ID", "CID")) |>
        rbind(df_res)
}

#' Convert identifiers of a single ToppGene drug database to PubChem CIDs.
#'
#' Map external Registry IDs to PubChem CIDs using the PubChem Power User
#' Gateway (PUG) (https://pubchem.ncbi.nlm.nih.gov/docs/power-user-gateway),
#' also specified by the NCBI identifer exchange service:
#' https://pubchem.ncbi.nlm.nih.gov/idexchange/
#'
#' @param ids character vector of one or more Registry identifiers.
#' @param registry optional character vector of length one with Registry name.
#'     Not specifying this argument falls back to a PubChem synonym lookup.
#' @return DataFrame with PubChem CIDs or NAs guaranteed to be the length as
#'     the input when using a registry.  The DataFrame may have more rows than
#'     the input when using a non-registry synonym lookup.
lookup_pubchem_ <- function(ids, registry = NULL) {
    resp <-
        req_pubchem_query(ids, registry) |>
        req_perform()
    reqid <- parse_pubchem_query(resp)
    resp <-
        req_pubchem_status(reqid) |>
        req_perform()
    parse_pubchem_status(resp)
}
