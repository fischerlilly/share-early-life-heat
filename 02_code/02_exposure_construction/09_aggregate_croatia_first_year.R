#!/usr/bin/env Rscript
source(file.path("02_code", "00_config", "config.R"))

suppressPackageStartupMessages(library(data.table))
input_file <- file.path(DIR$processed_croatia,
  "eobs_monthly_threshold_days_1950_1965_croatia_share_regions.csv")
output_file <- file.path(DIR$processed_croatia,
  "eobs_first12months_threshold_days_birthmonths_1950_1964_croatia_share_regions.csv")
monthly <- fread(input_file,na.strings=c("","NA"))
monthly[,month_date:=as.IDate(paste0(year_month,"-01"))]
birth_months <- seq(as.IDate("1950-01-01"),as.IDate("1964-12-01"),by="month")
windows <- rbindlist(lapply(birth_months,function(b) data.table(
  birth_month_date=b,month_date=seq(b,by="month",length.out=12L))))
expanded <- monthly[windows,on="month_date",nomatch=0L,allow.cartesian=TRUE]
threshold_cols <- "days_ge_20"
strict_sum <- function(x) if(anyNA(x)) NA_integer_ else as.integer(sum(x))
first12 <- expanded[,c(list(months_expected=12L,
  months_with_data=sum(days_available==days_in_month),
  expected_days=sum(days_in_month),days_available=sum(days_available)),
  lapply(.SD,strict_sum)),
  by=.(weather_region_id,weather_region_name,birth_month_date,temperature_variable),
  .SDcols=threshold_cols]
first12[,birth_year:=year(birth_month_date)]
first12[,birth_month:=month(birth_month_date)]
first12[,birth_year_month:=format(birth_month_date,"%Y-%m")]
first12[,coverage_complete:=days_available==expected_days]
setcolorder(first12,c("weather_region_id","weather_region_name","birth_year",
  "birth_month","birth_year_month","temperature_variable","months_expected",
  "months_with_data","expected_days","days_available","coverage_complete",threshold_cols))
setorder(first12,weather_region_id,birth_year_month,temperature_variable)
if(nrow(first12)!=20L*180L*1L) stop("Unexpected first-12-month row count.")
fwrite(first12,output_file,na="")
cat("Saved:",output_file,"\nRows:",nrow(first12),"\n")
cat("Incomplete exposure windows:",sum(!first12$coverage_complete),"\n")
