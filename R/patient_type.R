#' Create In/Out Patient Type Variable in DNPR3
#'
#' Learns patient type (inpatient/acute outpatient/elective outpatient)
#' from contact details.
#'
#' @param dataset Data set including the following variables
#' \itemize{
#' \item Either one variable containing contact duration in hours named \code{duration_h} or all of the following: \code{dato_start}, \code{tidspunkt_start}, \code{dato_slut}, and \code{tidspunkt_slut}
#' \item Either \code{elective} containing a logical value indicating whether contact is elective or acute *or* \code{prioritet} as defined in DNPR3.
#' \item Either \code{overnight} containing a logical value indicating whether contact is overnight or \code{dato_start} and \code{dato_end} containing start and end date of contact.
#' \item If \code{method = "hybrid_dep"}: p_overnight: Name of variable for percentage of overnight contacts.
#' }
#' @param method Name of patient type algorithm to use for classification.
#'   Possible values are \code{method = "cluster"} (default) or \code{method = "hybrid"}.
#'
#' @return Data set with added patient type variable
#'
#' @examples
#' # example code
#' # Using example data from package
#' files <- list.files(system.file("extdata", package = "DNPR"),
#'     pattern = "\\.csv$", ignore.case = TRUE, full.names = TRUE)
#' data <- files[1]
#' newdata <- read.csv(data)
#' head(newdata)
#' newdata <- patient.type(newdata)
#' head(newdata)
#'
#' @import polars
#' @importFrom data.table fcase
#'
#' @export
patient.type <- function(dataset, method = c("cluster", "hybrid")) {


  method <- match.arg(method)
  datavars <- colnames(dataset)

  # Variable check -------------------------------------------------------------
  # Check that necessary variables are included
  # Duration
  var_check_duration <- c("tidspunkt_start", "tidspunkt_slut", "dato_start", "dato_slut")
  if(!(all(var_check_duration %in% datavars) |
       ("duration_h" %in% datavars))) {
    stop(paste0("Input data must contain all the following variables: ",
                paste(var_check_duration, collapse = ", "), " or the variable duration_h"))
  }
  # Overnight
  if(!("overnight" %in% datavars | "dato_start" %in% datavars | "dato_slut" %in% datavars)) {
    stop(paste0("Input data must contain all of the following variables: dato_start, dato_slut or the variable overnight"))
  }
  # Elective
  if(!("elective" %in% datavars | "prioritet" %in% datavars)) {
    stop(paste0("Input data must contain one of the following variables: elective, prioritet"))
  }

  is_polars_available <- TRUE#require(polars)

  if (!("duration_h" %in% datavars)) {
    dataset <- duration(dataset, unit = "hours")
  }
  if (!("elective" %in% datavars)) {
    if (!is_polars_available) {
      dataset$elective <- as.numeric(grepl("^ATA3", dataset$prioritet))
    } else {
      dataset <- as_polars_df(dataset)$
        with_columns(elective = pl$
                       when(pl$col("prioritet")$str$starts_with("ATA3"))$
                       then(1)$
                       otherwise(0))$
        to_data_frame()
    }
  }
  if (!("overnight" %in% datavars)) {
    if (!is_polars_available) {
      dataset$overnight <- as.numeric(dataset$dato_start != dataset$dato_slut)
    } else {
      dataset <- as_polars_df(dataset)$
        with_columns(overnight = pl$
                       when(pl$col("dato_start") != pl$col("dato_slut"))$
                       then(1)$
                       otherwise(0))$
        to_data_frame()
    }
  }
  if (!("over24h" %in% datavars)) {
    if (!is_polars_available) {
      dataset$over24h <- as.numeric(dataset$duration_h >= 24)
    } else {
      dataset <- as_polars_df(dataset)$
        with_columns(over24h = pl$
                       when(pl$col("duration_h") >= 24)$
                       then(1)$
                       otherwise(0))$
        to_data_frame()
    }
  }

  dataset$`Patient Type` = switch(EXPR = method,
                                   cluster = proxy_cluster(dataset),
                                   hybrid = proxy_hybrid(dataset)
    )

  dataset
}
#' Cluster-based patient type algorithm
proxy_cluster <- function(dataset) {
  with(dataset,
    fcase(
      (elective != 1 & over24h != 1 & duration_h < 9), "Acute Outpatient",
      (elective != 1 & over24h == 1) |
        (elective != 1 & over24h != 1 & duration_h >= 9) |
        (elective == 1 & overnight == 1) |
        (elective == 1 & overnight != 1 & duration_h >= 3.5), "Inpatient",
      elective == 1 & overnight != 1 & duration_h < 3.5, "Elective Outpatient"
    )
  )
}
#' Hybrid-based patient type algorithm
proxy_hybrid <- function(dataset) {
  with(dataset,
       fcase(
    elective == 1 & overnight != 1 & duration_h < 2.6, "Elective Outpatient",
    (elective == 1 & overnight == 1) |
      (elective == 1 & overnight != 1 & duration_h >= 2.6) |
      (elective != 1 & over24h == 1) |
      (elective != 1 & over24h != 1 & duration_h >= 4), "Inpatient",
    (elective != 1 & over24h != 1 & duration_h < 4), "Acute Outpatient")
  )
}
#' Hybrid department-based patient type algorithm
hybrid_dep_fct <- function(dataset) {
  with(dataset,
       fcase((elective != 1 & p_overnight >= 0.23) |
               (elective == 1 & p_overnight >= 0.3), "Inpatient",
             elective != 1 & p_overnight < 0.23, "Acute Outpatient",
             elective == 1 & p_overnight < 0.3, "Elective Outpatient")
  )
}
