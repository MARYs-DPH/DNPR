#' Create Duration Variable in DNPR3
#'
#' Compute contact duration from contact details.
#'
#' Make sure that data covers the desired time period, e.g., dato_start in specific time interval
#' Make sure that data contains the required variables
#'
#' @param data Data set
#' @param unit Duration unit. Possible values are seconds, minutes, hours (default), or days.
#' @param output Character string for desired output format.
#'
#' @import polars
#'
#' @return Data set with added duration variable
#'
#' @examples
#' # Input example data from package and return object of class \code{data.frame}
#' files <- list.files(system.file("extdata", package = "DNPR"), pattern = "\\.csv$", ignore.case = TRUE, full.names = TRUE)
#' data <- read.csv(files[1])
#' head(data)
#' newdata <- duration(data)
#' head(newdata)
#' # Return duration in minutes
#' newdata <- duration(data, unit = "minutes")
#' head(newdata)
#'
#' # Input example data from package (.csv file) and return object of class \code{data.frame}
#' files <- list.files(system.file("extdata", package = "DNPR"), pattern = "\\.csv$", ignore.case = TRUE, full.names = TRUE)
#' newdata <- duration(files)
#' # Return duration in minutes
#' newdata <- duration(files, unit = "minutes")
#'
#' # Input example data from package (.parquet file) and return object of class \code{RPolarsDataFrame}
#' files <- list.files(system.file("extdata", package = "DNPR"), pattern = "\\.parquet$", ignore.case = TRUE, full.names = TRUE)
#' newdata <- duration(files, output = "RPolarsDataFrame")
#'
#'
#' @export
duration <- function(data, unit = c("hours", "minutes", "days", "seconds"),
                     output = c("data.frame", "RPolarsDataFrame", "RPolarsLazyFrame", "csv", "parquet")) {

  unit <- match.arg(unit)
  output <- match.arg(output)

  method = ifelse(class(data) == "character", ifelse(substr(data, nchar(data)-2, nchar(data)) == "csv", "csv",
                                                     ifelse(substr(data, nchar(data)-6, nchar(data)) == "parquet",
                                                            "parquet", "")),
                  ifelse(class(data) == "RPolarsDataFrame", "RPolarsDataFrame",
                         ifelse(class(data) == "data.frame", "data.frame",
                                ifelse(class(data) == "RPolarsLazyFrame", "RPolarsLazyFrame", "")
                                )
                  )
  )

  # LazyFrame of "kontakter" table =============================================
  lf <- switch(EXPR = method,
               parquet = pl$
                 scan_parquet(data),
               csv = pl$
                 scan_csv(
                   path = data,
                   dtypes = list("tidspunkt_start" = pl$String,
                                 "tidspunkt_slut" = pl$String,
                                 "dato_start" = pl$String,
                                 "dato_slut" = pl$String,
                                 "DW_EK_KONTAKT" = pl$String,
                                 "PNR" = pl$String)),
               RPolarsDataFrame = data$lazy(),
               data.frame = as_polars_lf(data),
                RPolarsLazyFrame = data
  )

  # Variable check -------------------------------------------------------------
  # Check that necessary variables are included
  var_check <- c("tidspunkt_start",
                 "tidspunkt_slut",
                 "dato_start",
                 "dato_slut")

  if(!all(var_check %in% lf$columns)) {
    stop(paste0("Input data must contain the following variables: ",
                paste(var_check, collapse = ", ")))
  }

  # Rename/select/drop/transform variables -------------------------------------
  lf_out <- lf$
    with_columns(
      "tid_start" = pl$col("tidspunkt_start")$str$to_time(strict = FALSE),
      "tid_slut" = pl$col("tidspunkt_slut")$str$to_time(strict = FALSE),
      "date_start" = pl$col("dato_start")$str$to_date(format = "%m/%d/%Y", strict = FALSE),
      "date_slut" = pl$col("dato_slut")$str$to_date(format = "%m/%d/%Y", strict = FALSE)
    )$
    with_columns(pl$col("date_start")$dt$combine(pl$col("tid_start"))$alias("d1"),
                 pl$col("date_slut")$dt$combine(pl$col("tid_slut"))$alias("d2"))

  if ("minutes" %in% unit) {
    lf_out <- lf_out$
      with_columns((pl$col("d2") - pl$col("d1"))$dt$total_minutes()$alias("duration_m"))
  }
  if ("hours" %in% unit) {
    lf_out <- lf_out$
      with_columns((pl$col("d2") - pl$col("d1"))$dt$total_hours()$alias("duration_h"))
  }
  if ("seconds" %in% unit) {
    lf_out <- lf_out$
      with_columns((pl$col("d2") - pl$col("d1"))$dt$total_seconds()$alias("duration_s"))
  }
  if ("days" %in% unit) {
    lf_out <- lf_out$
      with_columns((pl$col("d2") - pl$col("d1"))$dt$total_days()$alias("duration_d"))
  }

  lf_out <- lf_out$
    drop(c("d1", "d2", "tid_start", "tid_slut", "date_start", "date_slut"))$
    unique()

  # Output =====================================================================
  out <- switch(EXPR = output,
               csv = lf_out$
                 collect()$
                 write_csv(file = paste0(substr(data, 1, nchar(data)- 4), "_out.csv")),
               parquet = lf_out$collect()$
                 write_parquet(file = paste0(substr(data, 1, nchar(data)- 8), "_out.parquet")),
               RPolarsDataFrame = lf_out$collect(),
               data.frame = lf_out$collect()$
                 to_data_frame(),
               RPolarsLazyFrame = lf_out
  )

  out

}







