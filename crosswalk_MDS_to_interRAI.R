# Required libraries:
#
library(dplyr)     # data manipulation
library(stringr)   # text processing
library(rlang)     # evaluate expressions

# Function icode_transform()
#
# Given a list of desired i-codes, MDS data, and a conversion table (crosswalk),
# produces a table containing the MDS data transformed into the given i-code variables.
# Checks first that necessary conversion rules and MDS data are available.
#
# Arguments:
#
#   icodes - the i-code names of the desired variables to transform data into. A character vector.
#   data - the MDS data needed to be transformed. A data frame.
#   crosswalk - a table with conversion rules written in a specific syntax (see below).
#   crosswalk_cols (optional) - a named vector giving the column indices of certain entities in the crosswalk (see below).
#   stop_if_missing (optional) - if TRUE (default), the function will stop and throw an error in case either
#                                the crosswalk algorithm or required data are missing for any desired i-code.
#                                If FALSE, the function will skip the faulty i-codes with a warning and transform others.
#   include_nontransformable (optional) - if TRUE, empty columns will be included in the returned table for i-codes
#                                         that cannot be transformed. If FALSE (default), such columns will not appear.
# Value:
#
#   A data frame containing the transformed data for the desired i-code variables.
#
# Crosswalk table:
#
#   Every row in the crosswalk (conversion) table corresponds to a specific i-code and contains the rules for 
#   obtaining the values for this variable from the MDS data. The table should contain the following columns.
#   They can be named in any way, since they are accessed by their position. If some columns are not in their
#   default position (given in parentheses below), all positions must be specified in the argument "crosswalk_cols".
#
#     icode (1) - the i-code name. Rows that do not contain a name starting with "i" in this column are skipped.
#     cw_type (3) - the crosswalk type, one of CW0-5 or Der. Refer to the crosswalk documentation for definitions.
#     mds_var (4) - the names of MDS variables needed to reconstruct this i-code variable.
#     recode (7) - a simple recoding rule with comma-separated equations of the form "mds_value=icode_value".
#                  Used with crosswalk types CW2 and CW3.
#     other_tf (8) - a more complicated transformation rule written directly as an R expression. Used with type CW4.
#
icode_transform <- function(icodes,
                            data,
                            crosswalk,
                            crosswalk_cols = c(
                              "icode" = 1,
                              "cw_type" = 3,
                              "mds_var" = 4,
                              "recode" = 7,
                              "other_tf" = 8
                            ),
                            stop_if_missing = TRUE,
                            include_nontransformable = FALSE
                            ) {

  # General logic:
  # 1. Check that required variables exist (call verify_transformability)
  # 2. For each i-code, use a suitable tranformation rule (call transform_row):
  #   a. Either do a recode with case_match (call perform_recode inside transform_row)
  #   b. Or use a custom R expression (call perform_transformation inside transform_row)
  
  library(dplyr)
  
  # Standardise column names in the crosswalk table
  colnames(crosswalk) <- paste("col", seq_along(crosswalk), sep = ".")
  colnames(crosswalk)[crosswalk_cols] <- names(crosswalk_cols)

  # Ensure common case for MDS variable names
  colnames(data) <- tolower(colnames(data))
  crosswalk$mds_var <- tolower(crosswalk$mds_var)
  crosswalk$other_tf <- tolower(crosswalk$other_tf)

  # Verify input and report
  verification <- verify_transformability(icodes, crosswalk, colnames(data))
  
  if (length(verification) > 0) {
    error_message <- paste(paste(names(verification), verification, sep = ": "), collapse = "\n")
    if (stop_if_missing) {
      stop(error_message)
    } else {  # remove i-codes with errors
      icodes <- icodes[!(tolower(icodes) %in% names(verification))]
      warning(paste0(error_message, "\nSkipping problematic i-codes"))
    }
  }

  # Restrict to i-codes requested in the call
  cw <- crosswalk %>% 
    filter(!is.na(icode) & tolower(icode) %in% tolower(icodes))
  if (!include_nontransformable)
    cw <- cw %>% filter(cw_type != "CW5")

  # Create an empty data frame to receive transformed data
  transformed = data.frame(matrix(nrow = nrow(data), ncol = nrow(cw)))
  colnames(transformed) = cw$icode

  # Apply transformation function to each row in the cw table, with the data
  for (i in seq_len(nrow(cw))) {
    row <- unlist(cw[i,])
    transformed[i] <- transform_row(row, data)
  }

  return (transformed)
}

# Function verify_transformability()
#
# Given a list of desired i-codes, a conversion table, and names of available MDS variables
# checks that
#   a) the conversion table contains a row for the desired i-code
#   b) the names of all MDS variables required for the conversion are found in the list.
#
# Arguments:
#
#   icodes - the i-code names of the desired variables to transform data into. A character vector.
#   crosswalk - a table with conversion rules (see icode_transform).
#   variables - the MDS variable names that are available in the data. A character vector.
#
# Value:
#
#   A named vector of error messages, named after the i-codes that produced the error.
#   Empty (length 0) if no errors.
#
verify_transformability <- function(icodes, crosswalk, variables) {
  
  # Make sure all variable names are in the same case
  icodes <- tolower(icodes)
  crosswalk$icode <- tolower(crosswalk$icode)
  crosswalk$mds_var <- tolower(crosswalk$mds_var)
  variables <- tolower(variables)
  
  # Check that
  # 1. Every desired i-code is covered in the crosswalk
  # 2. For each conversion, all needed MDS names are found in the variables 
  error_messages <- sapply(icodes, function(icode) {
    if (!(icode %in% crosswalk$icode)) {
      "crosswalk not found"
    } else {
      required_vars <- stringr::str_split(
        crosswalk$mds_var[crosswalk$icode == icode],
        pattern = "\\s*,\\s*", 
        simplify = TRUE)
      not_found <- required_vars[!(required_vars %in% variables)]
      if (!all(is.na(required_vars)) & any(length(not_found) > 0)) {
        paste("required variables",
               paste(not_found, sep = ", ", collapse = ", "),
               "not found in the data set")
      } else {
        NA    # everything in order
      } 
    }
  })
  error_messages[!is.na(error_messages)]
}
 
# Function transform_row()
#
# Given a row in the crosswalk table (with column names) and MDS data,
# produces the values for the i-code specified by the row by calling one of
#
#   perform_recode (for CW2 and CW3)
#   perform_transformation (for CW4)
#
# Arguments:
#
#   row - the crosswalk row containing the i-code and its conversion rule. A named vector.
#   data - MDS variable data that need to be transformed. A data frame.
#
# Value:
#
#   A vector containing the transformed values for the desired i-code.
# 
transform_row <- function(row, data) {
  
  if (row["cw_type"] %in% c("CW0", "CW1"))
    return(data[[row["mds_var"]]])
  
  if (row["cw_type"] %in% c("CW2", "CW3"))
    return(perform_recode(row["mds_var"], row["recode"], data))
  
  if (row["cw_type"] == "CW4")
    return(perform_transformation(row["other_tf"], data))
}

# Function perform_recode()
#
# Given an MDS variable name, a recoding rule, and MDS data, produces the corresponding values
# by recoding the MDS variable.
#
# Arguments:
#
#   var - MDS variable name.
#   recoding - the recoding rule as a string of comma-separated equations
#              of the type "mds_value=icode_value".
#   data - MDS data containing the required MDS variable.
#
# Value:
#
#   A vector containing the recoded values.
#
perform_recode <- function(var, recoding, data) {
  recoding <- stringr::str_replace_all(recoding, "=", "~")
  # evaluate to a list of formulae
  recoding <- eval(rlang::parse_expr(paste0("list(",recoding,")")))
  # perform recoding by inserting the formula list as parameters into case_match()
  dplyr::case_match(data[[var]], !!!recoding)
}

# Function perform_transformation()
#
# Given a custom transformation rule and MDS data, produces the transformed values.
#
# Arguments:
#
#   tranformation - an R expression containing the transformation as a character string.
#   data - MDS data containing the variables needed for the transformation.
#
# Value:
#
#   A vector containing the transformed values.
#
perform_transformation <- function(transformation, data) {
  dplyr::transmute(data, eval(rlang::parse_expr(transformation)))[[1]]
}
