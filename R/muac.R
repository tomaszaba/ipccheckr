#'
#'Identify Flags in the Dataset using MUAC-for-age MFAZ z-score and Crude MUAC
#'
#'This function is designed to flag MFAZ and crude MUAC-based values if the fall
#'  outside outside ±3 from the observed survey sample mean and a fixed range of
#'  <100mm and >200mm for MFAZ and crude MUAC respectively. This is based on the
#'  research findings by [Bilukha, O., & Kianian, B. (2023)
#'  .](https://doi.org/10.1111/mcn.13478)
#'
#'@param x A numeric value from the variable storing MFAZ or crude MUAC's
#'  observations in the dataset, as applicable.
#'
#'@param method The method you wish `add_flags()` to identify flags on. A choice
#'  between "mfaz" and "muac". If you wish to get flags for MFAZ set
#'  `method = "mfaz"`. If your wish to get flags for crude MUAC, set
#'  `method = "muac"`. The default is "mfaz". If by mistake a different option
#'  is supplied, an error will be thrown with a message guiding you what to do.
#'
#'@return A vector of two values: 1 and 0, where 1 signifies flagged value and
#'  0 not flagged.
#'
#'@examples
#'  # Sample data of crude MUAC
#'  x <- c(90, 110, 140, 200, 119, 235)
#'  # Apply `add_flags()` with method set to "muac"
#'  add_flags(x, method = "muac")
#'
#'  # Sample data of MFAZ
#'  x <- c(-2.265, -5.275, -0.72, -2.261, -2.264, -4.451, -2.261, -1.828)
#'  # Apply `add_flags()` with method set to "mfaz"
#'  add_flags(x, method = "mfaz")
#'
#'@export
#'
add_flags <- function(x, method = c("mfaz", "muac")) {
  if (method == "mfaz") {
    flags <- ifelse((x < - 3 | x > 3), 1, 0)
    flags <- ifelse(is.na(x), NA, flags)
    flags
  } else if (method == "muac") {
    flags <- ifelse(x < 100 | x > 200, 1, 0)
    flags <- ifelse(is.na(x), NA, flags)
    flags
  } else {
    message("This method is not applicable. Please choose between 'mfaz' and
    'muac'")
  }
}



