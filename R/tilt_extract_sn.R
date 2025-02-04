#' Extract sensor serial number from the file name(s)
#'
#' @param files Vector of the tilt meter current and/or temperature file names.
#'   Files must start with the 7-digit sensor serial number. Function will give
#'   a warning if the files have different serial numbers.
#'
#' @param return_numeric Logical argument indicating whether to convert the
#'   serial number from a character to numeric.
#'
#' @return Returns the first 7 characters of the file name, which should be the
#'   sensor serial number.
#'
#' @export


tilt_extract_sn <- function(files, return_numeric = TRUE) {

  # extract SN from file name
  sn <- unique(unlist(lapply(files, FUN = "substr", start = 1, stop = 7)))

  if(length(files) > 1 & length(sn) > 1)  {
    stop(
      paste0(
        "current and temperature files have different serial numbers: ",
        paste(sn, collapse = " "))
    )
  }

  if(isTRUE(numeric)) sn <- as.numeric(sn)

  sn
}
