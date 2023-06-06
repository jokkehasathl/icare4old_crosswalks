# icare4old_crosswalks
MDS-interRAI crosswalk developed in the I-CARE4OLD project

The R function icode_transform() converts MDS 2.0 data into interRAI i-code (Suite) standard,
using crosswalk tables produced in the I-CARE4OLD project.

In addition to this code, you will need:
 - the crosswalk tables
 - MDS 2.0 data with standard MDS variable names

## Examples

crosswalk <- readxl::read_excel("MDS_HC_2.0_crosswalk_to_icodes.xlsx")

transformed_data_1 <- icode_transform(icodes, mds_data, crosswalk)

- Some optional parameters:
transformed_data_2 <- icode_transform(icodes, mds_data, crosswalk, stop_if_missing = FALSE, include_nontranformable = TRUE)

- Different column ordering in crosswalk table:
transformed_data_2 <- icode_transform(icodes, mds_data, crosswalk,
                                      crosswalk_cols = c("icode" = 1, "cw_type" = 3, "mds_var" = 4, "recode" = 5, "other_tf" = 6))
