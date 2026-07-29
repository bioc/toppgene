#' @import methods
#' @importFrom S4Vectors DataFrame
NULL

## Note that although by convention classes and generics are placed in
## AllClasses.R and AllGenerics.R to be lexicographically processed before
## other classes, this package has only two files (the second starting with a
## "w") and so this file with its classes will be executed before the others
## and is more legible here.

.CategoriesDataFrame <-
    setClass(
        "CategoriesDataFrame",
        contains = "DFrame")

## Specified by https://toppgene.cchmc.org/API/openapi.yaml
CATEGORIES <- c(
    "Coexpression",
    "CoexpressionAtlas",
    "Computational",
    "Cytoband",
    "Disease",
    "Domain",
    "Drug",
    "GeneFamily",
    "GeneOntologyBiologicalProcess",
    "GeneOntologyCellularComponent",
    "GeneOntologyMolecularFunction",
    "HumanPheno",
    "Interaction",
    "MicroRNA",
    "MousePheno",
    "Pathway",
    "Pubmed",
    "TFBS",
    "ToppCell")
PROPS_DEFAULT <- list(
    PValue = 0.05,
    MinGenes = 1L,
    MaxGenes = 1500L,
    MaxResults = 100L,
    Correction = "FDR")
PROPS_ALLOWED <- list(
    PValue = c(min = 0, max = 1),
    MinGenes = c(min = 1L, max = 5000L),
    MaxGenes = c(min = 2L, max = 5000L),
    MaxResults = c(min = 1L, max = 5000L),
    Correction = c("None", "FDR", "Bonferroni"))

#' CategoriesDataFrame objects
#'
#' @description
#'
#' Specialized [DataFrame] class with the following additional constraints to
#' represent ToppGene parameters to run a ToppGene [enrich()] query:
#'
#' \itemize{
#' \item{Fixed number of rows with fixed order for each category.}
#' \item{Columns can only be set to the allowed numerical and set values that
#' are described when showing the object.}
#' }
#'
#' The DataFrame semantics allow quickly setting multiple parameters at a time.
#' Unlike typical DataFrame behavior, [show()] displays all 19 rows instead of
#' using ellipses.
#'
#' @param x `CategoriesDataFrame` object.
#' @param i With `j` (x[i, j]]), the row slice(s) / row name(s) , otherwise
#'   (x[[i]]) the column slice / name.
#' @param j The column slice / name (x[i, j]).
#' @param value Atomic or vector assigned to `x`.
#' @param object `CategoriesDataFrame` object used in [validObject()] and
#'   [show()] function calls.
#' @param ... Arguments passed on to inherited methods.
#' @returns `CategoriesDataFrame` with 19 rows (categories: CoExpression, ...,
#'   ToppCell) and 5 columns (PValue, MinGenes, MaxGenes, MaxResults,
#'   Correction).
#' @seealso [DataFrame::DataFrame()]
#' @keywords classes methods
#' @aliases CategoriesDataFrame CategoriesDataFrame-class
#' @rdname CategoriesDataFrame-class
#' @docType class
#' @export
#' @examples
#' library(DFplyr)
#' cats <- CategoriesDataFrame()
#' cats <-
#'     cats |>
#'     mutate(
#'         PValue = 0.001,
#'         MaxResults = case_when(
#'             grepl("Onto", rownames(cats)) ~ 25L,
#'             .default = MaxResults))
#' cats
CategoriesDataFrame <- function(...) {
    df <- DataFrame(
        PValue = rep(PROPS_DEFAULT$PValue, length(CATEGORIES)),
        MinGenes = PROPS_DEFAULT$MinGenes,
        MaxGenes = PROPS_DEFAULT$MaxGenes,
        MaxResults = PROPS_DEFAULT$MaxResults,
        Correction = PROPS_DEFAULT$Correction,
        ...)
    rownames(df) <- CATEGORIES
    .CategoriesDataFrame(df)
}

## The rownames match API Types.
.validRows <- function(msg, object) {
    if (nrow(object) != length(CATEGORIES)) {
        msg <- c(msg, "number of rows must remain the same")
    } else if (! all(rownames(object) == CATEGORIES)) {
        msg <- c(msg, "rows names and order must not change")
    }
    msg
}

## Allow the user to add their own columns, but check that the necessary
## columns are present.
.validColsPresent <- function(msg, object) {
    cols <- names(default(object))
    if (! all(cols %in% colnames(object))) {
        msg <- c(
            msg,
            paste0(
                "all columns (",
                paste(cols, sep = ", "),
                ") must be present"))
    }
    msg
}

## Check column contents.
.validColsContents <- function(msg, object) {
    for (i in seq_along(PROPS_ALLOWED)) {
        col <- names(PROPS_ALLOWED)[i]
        if (col %in% colnames(object)) {
            if (! is.null(names(PROPS_ALLOWED[[i]])) &&
                    all(names(PROPS_ALLOWED[[i]]) == c("min", "max"))) {
                ## Numeric column limited by min and max.
                if (! all(object[, col, drop = TRUE] >=
                            PROPS_ALLOWED[[i]]["min"])) {
                    msg <- c(
                        msg,
                        paste(
                            "column", col,
                            "must contain values >=",
                            PROPS_ALLOWED[[i]]["min"]))
                }
                if (! all(object[, col, drop = TRUE] <=
                            PROPS_ALLOWED[[i]]["max"])) {
                    msg <- c(
                        msg,
                        paste(
                            "column", col,
                            "must contain values <=",
                            PROPS_ALLOWED[[i]]["max"]))
                }
            } else {
                ## Non-numeric column limited by set.
                if (! all(object[, col, drop = TRUE] %in% PROPS_ALLOWED[[i]])) {
                    msg <- c(
                        msg,
                        paste0(
                            "column ", col,
                            " must contain values in {",
                            paste(PROPS_ALLOWED[[i]], collapse = ", "),
                            "}"))
                }
            }
        }
    }
    msg
}

.validCategoriesDataFrame <- # nolint: cyclocomp_linter
    setValidity(
        "CategoriesDataFrame",
        function(object) {
            ## Accumulate all the errors.
            msg <- NULL
            msg <-
                msg |>
                .validRows(object) |>
                .validColsPresent(object) |>
                .validColsContents(object)
            if (! is.null(msg)) {
                return(msg)
            }
            TRUE
        })

## Show all 19 rows instead of taking the head and tail.  Also show allowed
## values.
#' @rdname CategoriesDataFrame-class
#' @export
setMethod(
    "show",
    "CategoriesDataFrame",
    function(object) {
        cat(
            "ToppGene CategoriesDataFrame with ",
            nrow(object),
            " categories\n",
            sep = "")
        print(as.data.frame(object))
        cat(
            "------------------------------\n",
            "Values allowed by ToppGene are:\n",
            sep = "")
        for (i in seq_along(PROPS_ALLOWED)) {
            cat("  ", names(PROPS_ALLOWED)[i], ": ", sep = "")
            if (! is.null(names(PROPS_ALLOWED[[i]])) &&
                    all(names(PROPS_ALLOWED[[i]]) == c("min", "max"))) {
                delim <- c("[", "]")
            } else {
                delim <- c("{", "}")
            }
            cat(delim[1], paste(PROPS_ALLOWED[[i]], collapse = ", "), delim[2],
                " <", class(PROPS_ALLOWED[[i]]), ">\n",
                sep = "")
        }
        invisible(NULL)
    })

#' @rdname CategoriesDataFrame-class
#' @export
setGeneric("default", function(x) standardGeneric("default"))
#' @rdname CategoriesDataFrame-class
#' @export
setMethod("default", "CategoriesDataFrame", function(x) PROPS_DEFAULT)

#' @aliases [<-,CategoriesDataFrame-method
#' @rdname CategoriesDataFrame-class
#' @export
setMethod(
    "[<-",
    "CategoriesDataFrame",
    function(x, i, j, ..., value) {
        y <- x
        y <- callNextMethod(y, i, j, ..., value = value)
        validObject(y)
        x <- y
    })

#' @aliases [[<-,CategoriesDataFrame-method
#' @rdname CategoriesDataFrame-class
#' @export
setMethod(
    "[[<-",
    "CategoriesDataFrame",
    function(x, i, j, ..., value) {
        y <- x
        y@listData[[i]] <- value
        validObject(y)
        x <- y
    })

#' @aliases rownames<-,CategoriesDataFrame-method
#' @rdname CategoriesDataFrame-class
#' @export
setMethod(
    "rownames<-",
    "CategoriesDataFrame",
    function(x, value) {
        y <- x
        y <- callNextMethod(y, value)
        validObject(y)
        x
    })

#' @aliases subset,CategoriesDataFrame-method
#' @rdname CategoriesDataFrame-class
#' @export
setMethod(
    "subset",
    "CategoriesDataFrame",
    function(x, ...) {
        y <- x
        ## callNextMethod no longer calls our intended subset(), so explicitly
        ## choose the superclass method to call.
        f <- getMethod("subset", "RectangularData")
        y <- f(x, ...)
        validObject(y)
        x <- y
    })
