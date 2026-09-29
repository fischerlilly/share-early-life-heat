#!/usr/bin/env Rscript

# Confirm that every provider file required by the raw-to-results pipeline is
# present before any processed output is created.

source(file.path("02_code", "00_config", "config.R"))

required_raw_files <- c(
  file.path(DIR$raw_share, "easyshare", "easySHARE_rel9_0_0.rda"),
  file.path(DIR$raw_share, "wave3", c(
    "sharew3_rel9-0-0_ac.dta", "sharew3_rel9-0-0_st.dta",
    "sharew3_rel9-0-0_cv_r.dta"
  )),
  file.path(DIR$raw_share, "wave7", c(
    "sharew7_rel9-0-0_ra.dta", "sharew7_rel9-0-0_dn.dta",
    "sharew7_rel9-0-0_cv_r.dta", "sharew7_rel9-0-0_gv_isced.dta"
  )),
  file.path(DIR$raw_eobs, c(
    "tn_ens_mean_0.1deg_reg_1950-1964_v33.0e.nc",
    "tn_ens_mean_0.1deg_reg_1965-1979_v33.0e.nc",
    "tx_ens_mean_0.1deg_reg_1950-1964_v33.0e.nc",
    "tx_ens_mean_0.1deg_reg_1965-1979_v33.0e.nc"
  )),
  INPUT$nuts_2024
)

assert_inputs_exist(required_raw_files, "Raw provider input")
message("All raw provider inputs are present.")