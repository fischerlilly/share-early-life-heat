#!/usr/bin/env Rscript
source(file.path("02_code", "00_config", "config.R"))

suppressPackageStartupMessages(library(data.table))
input_file <- file.path(DIR$processed_croatia,
  "eobs_daily_1950_1965_croatia_share_region_area_weighted.csv.gz")
output_file <- file.path(DIR$processed_croatia,
  "eobs_monthly_threshold_days_1950_1965_croatia_share_regions.csv")
input_connection <- gzfile(input_file, open = "rt")
d <- as.data.table(read.csv(
  input_connection,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = c("", "NA")
))
close(input_connection)
d[,date:=as.IDate(date)]
d[,year:=year(date)]; d[,month:=month(date)]; d[,year_month:=format(date,"%Y-%m")]
long <- melt(d,id.vars=c("date","year","month","year_month",
  "weather_region_id","weather_region_name"),
  measure.vars=c("tn_mean"),
  variable.name="temperature_variable",value.name="temperature")
thresholds <- c(20)
monthly <- long[,c(list(days_in_month=.N,days_available=sum(!is.na(temperature))),
  setNames(lapply(thresholds,function(z) sum(temperature>=z,na.rm=TRUE)),
  paste0("days_ge_",thresholds))),
  by=.(weather_region_id,weather_region_name,year,month,year_month,temperature_variable)]
setorder(monthly,weather_region_id,year,month,temperature_variable)
if(anyDuplicated(monthly[,.(weather_region_id,year_month,temperature_variable)])) stop("Duplicate keys.")
fwrite(monthly,output_file,na="")
cat("Saved:",output_file,"\nRows:",nrow(monthly),"\n")