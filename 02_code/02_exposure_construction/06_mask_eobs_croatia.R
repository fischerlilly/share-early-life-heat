#!/usr/bin/env Rscript
source(file.path("02_code", "00_config", "config.R"))

suppressPackageStartupMessages({library(sf); library(data.table)})
progress <- function(s, x) {cat(sprintf("[%s] %s %s\n", format(Sys.time(), "%H:%M:%S"), s, x)); flush.console()}

input_file <- file.path(DIR$processed_italy,"eobs_daily_tx_tn_1950_1965_bbox_35_48N_5_20E.csv.gz")
boundary_file <- INPUT$nuts_2024
output_dir <- DIR$processed_croatia
output_file <- file.path(output_dir,"eobs_daily_1950_1965_croatia.csv.gz")
mask_file <- file.path(output_dir,"eobs_gridcells_overlapping_croatia.csv")
temporary_output <- paste0(output_file,".part")
dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
grid_cells_per_day <- 19500L
number_of_days <- 5844L
lines_per_chunk <- 500000L

progress("1/4","Read coordinates from the first day.")
con <- gzfile(input_file,"rt")
header <- readLines(con,n=1L,warn=FALSE)
lines <- readLines(con,n=grid_cells_per_day,warn=FALSE)
close(con)
first <- fread(text=paste(c(header,lines),collapse="\n"))
if(nrow(first)!=grid_cells_per_day || uniqueN(first$date)!=1L) stop("Unexpected first-day grid.")
first[,cell_id:=.I]
first[,longitude_key:=round(longitude,6)]
first[,latitude_key:=round(latitude,6)]

half <- 0.05
cell_polygon <- function(lon,lat) st_polygon(list(matrix(c(
  lon-half,lat-half,lon+half,lat-half,lon+half,lat+half,
  lon-half,lat+half,lon-half,lat-half),ncol=2,byrow=TRUE)))
grid_sf <- st_sf(
  cell_id=first$cell_id,longitude=first$longitude,latitude=first$latitude,
  longitude_key=first$longitude_key,latitude_key=first$latitude_key,
  geometry=st_sfc(Map(cell_polygon,first$longitude,first$latitude),crs=4326))
boundary <- st_read(boundary_file,quiet=TRUE)
boundary <- boundary[boundary$CNTR_CODE=="HR" & boundary$LEVL_CODE==0,]
if(nrow(boundary)!=1L) stop("Expected one Croatian NUTS-0 geometry.")

progress("2/4","Identify grid squares intersecting Croatia.")
keep <- lengths(st_intersects(st_transform(grid_sf,3035),st_transform(boundary,3035)))>0L
cells <- first[keep,.(cell_id,longitude,latitude,longitude_key,latitude_key)]
if(!nrow(cells)) stop("No Croatian grid cells found.")
fwrite(cells,mask_file)
setkey(cells,longitude_key,latitude_key)
cat("Croatian grid cells:",nrow(cells),"\n")
if(identical(Sys.getenv("TEMP_MASK_VALIDATE_ONLY"),"1")) quit(save="no",status=0)

progress("3/4","Filter large daily file in blocks.")
cin <- gzfile(input_file,"rt")
cout <- gzfile(temporary_output,"wt",compression=6)
stopifnot(identical(readLines(cin,n=1L,warn=FALSE),header))
read_rows <- 0
written_rows <- 0
chunk_id <- 0L
header_written <- FALSE
repeat {
  lines <- readLines(cin,n=lines_per_chunk,warn=FALSE)
  if(!length(lines)) break
  chunk_id <- chunk_id+1L
  read_rows <- read_rows+length(lines)
  d <- fread(text=paste(c(header,lines),collapse="\n"),na.strings=c("","NA"))
  d[,longitude_key:=round(longitude,6)]
  d[,latitude_key:=round(latitude,6)]
  d <- d[cells,on=.(longitude_key,latitude_key),nomatch=0L]
  d <- d[,.(date,longitude,latitude,tx,tn)]
  write.table(d,cout,sep=",",row.names=FALSE,col.names=!header_written,quote=FALSE,na="")
  header_written <- TRUE
  written_rows <- written_rows+nrow(d)
  progress("3/4",paste("Chunk",chunk_id,"-",written_rows,"rows written."))
}
close(cin); close(cout)
if(read_rows != as.double(grid_cells_per_day)*number_of_days) stop("Incomplete bounding-box input.")
if(written_rows != as.double(nrow(cells))*number_of_days) stop("Unexpected Croatia row count.")
if(file.exists(output_file)) unlink(output_file)
if(!file.rename(temporary_output,output_file)) stop("Could not finalize Croatia file.")
progress("4/4",paste("Saved:",output_file))

