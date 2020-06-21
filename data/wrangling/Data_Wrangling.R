library( dplyr )
library( knitr )
library( pander )

# convert variables to numeric
# and remove missing values placeholders;
# inpute missing values with mean
clean_x <- function( x )
{
  x <- as.numeric( x )
  x[ x == -999 ] <- NA
  mean.x <- mean( x, na.rm=T )
  x[ is.na(x) ] <- mean.x
  return(x)
}

# apply the clean var x function to all columns 
clean_d <- function( d, start.column )
{
  # d <- fix_names( d )
  
  these <- start.column:ncol(d)
  d[ these ] <- lapply( d[ these ], clean_x )
  
  return( d )
}

# FIX VARIABLE NAMES
# input dataframe
# standardize variable names 
# output data frame with fixed names
fix_names <- function( d )
{
  nm <- names( d )
  nm <- tolower( nm )
  
  nm[ nm == "statea"  ] <- "state"
  nm[ nm == "countya" ] <- "county"
  nm[ nm == "tracta"  ] <- "tract"
  nm[ nm == "trtid10" ] <- "tractid"
  nm[ nm == "mar-70"  ] <- "mar70"
  nm[ nm == "mar-80"  ] <- "mar80"
  nm[ nm == "mar-90"  ] <- "mar90"
  nm[ nm == "mar.00"  ] <- "mar00"
  nm[ nm == "x12.mar" ] <- "mar12"  
  nm <- gsub( "sp1$", "", nm )
  nm <- gsub( "sp2$", "", nm )
  nm <- gsub( "sf3$", "", nm )
  nm <- gsub( "sf4$", "", nm )
  
  # nm <- gsub( "[0-9]{2}$", "", nm )
  
  names( d ) <- nm
  return( d )
}

# FIX TRACT IDS
# put into format: SS-CCC-TTTTTT
fix_ids <- function( x )
{
  x <- stringr::str_pad( x, 11, pad = "0" )
  state <- substr( x, 1, 2 )
  county <- substr( x, 3, 5 )
  tract <- substr( x, 6, 11 )
  x <- paste( "fips", state, county, tract, sep="-" )
  return(x)
}

tidy_up_data <- function( file.name )
{
  path <- paste0( "../raw/", file.name )
  d <- read.csv( path, colClasses="character" ) 
  type <- ifelse( grepl( "sample", file.name ), "sample", "full" )
  year <- substr( file.name, 10, 13 )
  
  # fix names 
  d <- fix_names( d )
  
  # fix leading zero problem in tract ids
  d$tractid <- fix_ids( d$tractid )
  
  # drop meta-vars
  drop.these <- c("state", "county", "tract", "placefp10",
                  "cbsa10", "metdiv10", "ccflag10", 
                  "globd10", "globg10","globd00", "globg00",
                  "globd90", "globg90","globd80", "globg80")
  d <- d[ ! names(d) %in% drop.these ]
  
  # column position where variables start after IDs
  d <- clean_d( d, start.column=2 )
  
  # add year and type (sample/full)
  d <- data.frame( year, type, d, stringsAsFactors=F )
  
  return( d )
}

build_year <- function( fn1, fn2, year )
{ 
  
  d1 <- tidy_up_data( fn1 )
  d1 <- select( d1, - type )
  
  d2 <- tidy_up_data( fn2 )
  d2 <- select( d2, - type )
  
  d3 <- merge( d1, d2, by=c("year","tractid"), all=T )
  
  file.name <- paste0( "../rodeo/LTDB-", year, ".rds" )
  saveRDS( d3, file.name )
  
} 

year <- 1970
f1 <- "LTDB_Std_1970_fullcount.csv"
f2 <- "ltdb_std_1970_sample.csv"
build_year( fn1=f1, fn2=f2, year=year )

year <- 1980
f1 <- "LTDB_Std_1980_fullcount.csv"
f2 <- "ltdb_std_1980_sample.csv"
build_year( fn1=f1, fn2=f2, year=year )

year <- 1990
f1 <- "LTDB_Std_1990_fullcount.csv"
f2 <- "ltdb_std_1990_sample.csv"
build_year( fn1=f1, fn2=f2, year=year )

year <- 2000
f1 <- "LTDB_Std_2000_fullcount.csv"
f2 <- "ltdb_std_2000_sample.csv"
build_year( fn1=f1, fn2=f2, year=year )

year <- 2010
f1 <- "LTDB_Std_2010_fullcount.csv"
f2 <- "ltdb_std_2010_sample.csv"
build_year( fn1=f1, fn2=f2, year=year )

# Metro Data Metadata
URL <- "https://data.nber.org/cbsa-msa-fips-ssa-county-crosswalk/cbsatocountycrosswalk.csv"
cw <- read.csv( URL, colClasses="character" )

# all metro areas in the country
sort( unique( cw$cbsaname ) ) %>% head() %>% pander()

cw$urban <- ifelse( cw$cbsaname == "", "rural", "urban" )

keep.these <- c( "countyname","state","fipscounty", 
                 "msa","msaname", 
                 "cbsa","cbsaname",
                 "urban" )

cw <- dplyr::select( cw, keep.these )

saveRDS( cw, "../raw/cbsa-crosswalk.rds" )

# Create Meta-Data Table
extract_metadata <- function( file.name )
{
  path <- paste0( "../raw/", file.name )
  d <- read.csv( path, colClasses="character" ) 
  type <- ifelse( grepl( "sample", file.name ), "sample", "full" )
  year <- substr( file.name, 10, 13 )
  
  # fix names 
  d <- fix_names( d )
  
  # fix leading zero problem in tract ids
  d$tractid <- fix_ids( d$tractid )
  
  # drop meta-vars
  keep.these <- c("tractid","state", "county", "tract", "placefp10",
                  "cbsa10", "metdiv10", "ccflag10", 
                  "globd10", "globg10","globd00", "globg00",
                  "globd90", "globg90","globd80", "globg80")
  d <- d[ names(d) %in% keep.these ]
  return( d )
}


f.1970 <- "LTDB_Std_1970_fullcount.csv"
f.1980 <- "LTDB_Std_1980_fullcount.csv"
f.1990 <- "LTDB_Std_1990_fullcount.csv"
f.2000 <- "LTDB_Std_2000_fullcount.csv"

meta.d.2000 <- extract_metadata( file.name=f.2000 )

meta.d.1990 <- extract_metadata( file.name=f.1990 )
meta.d.1990 <- select( meta.d.1990, tractid, globd90, globg90 )

meta.d.1980 <- extract_metadata( file.name=f.1980 )
meta.d.1980 <- select( meta.d.1980, tractid, globd80, globg80 )

meta.d <- merge( meta.d.2000, meta.d.1990, all=T )
meta.d <- merge( meta.d, meta.d.1980, all=T )

meta.d$fipscounty <- paste0( substr( meta.d$tractid, 6, 7 ), 
                             substr( meta.d$tractid, 9, 11 ) )

cw <- select( cw, -countyname, -state )

cw <- cw[ ! duplicated(cw$fipscounty) , ]

meta.d <- merge( meta.d, cw, by="fipscounty", all.x=T )

saveRDS( meta.d, "../rodeo/LTDB-META-DATA.rds" )