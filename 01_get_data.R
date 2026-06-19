# //////////////////////////////////////////////////////////////////////////////
# //////////////////////////////////////////////////////////////////////////////

# This script does WHAAAAAAAAAAAAT

#**Inputs**



#**Outputs**



# //////////////////////////////////////////////////////////////////////////////
# //////////////////////////////////////////////////////////////////////////////

# HOUSEKEEPING ----

# **************************************************************

## Center yourself ----

### Where are we???????????

here::i_am("01_get_data.R")

# **************************************************************

## Packages ----

### General 

require(here)

### Data mgmt & processing

require(tidyverse)

### File mgmt

require(usethis)
require(xml2)
require(fs)

# **************************************************************

## Load custom functions ----

source(here("00_functions.R"))

# **************************************************************

## And set script-wide vars ----

### Map projection

map_crs <- 26913 # NAD83 / UTM zone 13N

map_proj <- "EPSG:26913"

### Email for LF download

em <- "john.kemper@usu.edu"

# **************************************************************


# //////////////////////////////////////////////////////////////////////////////
# //////////////////////////////////////////////////////////////////////////////


# File set up ----

## Set server location ----

### Find all the drives on the system

system_drives <- system("wmic logicaldisk get name,providername", 
                        intern = TRUE) %>%
  str_trim()

#### Find which one is the shared Belmont drive

belmont_drive <- read_table(system_drives) %>%
  rename_all(tolower) %>%
  filter(str_detect(providername, "blshare")) %>%
  mutate(name = paste0(name, "/")) %>%
  .$name

#### Get all the folder names within the GIS folder
#### on the Belmont drive
#### (where most of our data live)

belmont_dirs <- fs::dir_ls(path = paste0(belmont_drive, 
                                         "GIS_Datasets"))

# **************************************************************
# **************************************************************

## Set fire file location ----

### Find all where the MTBS files live
### in the burn severity folder

mtbs_dir <- belmont_dirs[str_detect(belmont_dirs, "MTBS")]

### We could set this up to be run, probably, simply a pointer to a list of files

mtbs_files <- dir_ls(mtbs_dir)

# **************************************************************

# //////////////////////////////////////////////////////////////////////////////
# //////////////////////////////////////////////////////////////////////////////

# !!!!!!!!!!!!!!!!!
# CAREFUL!!! WE ONLY WANT TO RUN THE BELOW IF WE HAVE NOT FILTERED BY CANOPY COVERAGE
# !!!!!!!!!!!!!!!!!

# **************************************************************


## First, we want to filter out each fire by the canopy cover %.
## Essentially here we are going to remove any fire that is >20% non-foresteed
## we are definiing non-forested using landfire canopy cover
## by determining the percentage of a fire that is comprised of LF CC
## pixels that indicate CC is 0% or 0% < CC < 10%. 
## A reference for this threshold is here: 
## https://www.fs.usda.gov/rm/pubs_series/rmrs/gtr/rmrs_gtr415.pdf

# Filter fires by CC ----

forested_fires_files <- fs::as_fs_path(character(0))

for(i in 1:length(mtbs_files)){
  
  # ******************************************************
  
    ##### Ticker (tick, tick tick) 
  
    cat(crayon::cyan("\nReading fire file", i, ":", mtbs_files[i], "\n"))
  
  # ******************************************************
      
    ## Metadata ----
    
    fire_meta_table <- get_fire_metadata(mtbs_files[i])
    
    # ******************************************************
    
    # Fire Boundary ----
    fire_boundry <- get_fire_boundry(mtbs_files[i], u_crs = map_crs)

    buff_boundry <- buffer_boundry(fire_boundry)
    
    # ******************************************************

    ## Stream mask ----
    stream_mask <- get_streammask(mtbs_files[i], u_crs = map_crs)
    
    # ******************************************************

    ## dNBR file ----
    dnbr <- get_processed_dnbr(fire_file = mtbs_files[i],
                               bound = buff_boundry,
                               str_mask = stream_mask,
                               u_proj = map_proj)
    
    # ******************************************************

    # Check canopy coverage ----

    canopy_cov <- get_lf(bound = buff_boundry,
                         str_mask = stream_mask,
                         fire_meta_dat = fire_meta_table,
                         snap_rast = dnbr,
                         u_crs = map_crs,
                         cc_only = TRUE)
    
    # ******************************************************

    # Now make a file list that is only the files that are fully forested
    if(canopy_cov == TRUE){

      forested_fires_files <- as_fs_path(c(forested_fires_files, mtbs_files[i]))

    } else if(canopy_cov == FALSE) {cat(crayon::red("Non-forested fire")) 
                                        next}
    
    # ******************************************************


}

# **************************************************************

## Write/Read ---- 

### Write to file

# write_csv(tibble(file_path = forested_fires_files),
#           here("output-data", "forested_fires_files.csv"))

### Read from file

forested_fires_files <- read_csv(here("output-data", 
                                      "forested_fires_files.csv"))

forested_fires_files <- as_fs_path(forested_fires_files$file_path)

#which(str_detect(forested_fires_files$file_path, "co3925110844620110807"))

# **************************************************************

# //////////////////////////////////////////////////////////////////////////////
# //////////////////////////////////////////////////////////////////////////////

# **************************************************************

# Get all data ----

for(j in 1:length(forested_fires_files)){
  
  
  cat(crayon::cyan("\nReading fire file", j, ":", forested_fires_files[j], "\n"))
  
  # ******************************************************
  
  ## Metadata ----
  
  fire_meta_table <- get_fire_metadata(forested_fires_files[j])
  
  # ******************************************************
  
  # Fire Boundry ----
  fire_boundry <- get_fire_boundry(forested_fires_files[j], u_crs = map_crs)
  
  buff_boundry <- buffer_boundry(fire_boundry)
  
  big_buff_boundry <- buffer_boundry_big(fire_boundry)
  
  # ******************************************************
  
  # Stream mask ----
  stream_mask <- get_streammask(forested_fires_files[j], u_crs = map_crs)
  
  # ******************************************************
  
  # dNBR file ----
  dnbr <- get_processed_dnbr(fire_file = forested_fires_files[j],
                             bound = buff_boundry,
                             str_mask = stream_mask,
                             u_proj = map_proj)
  
  dnbr_df <-  terra::as.data.frame(dnbr, xy = TRUE) %>%
    rename(dnbr = 3)
  
  # ******************************************************
  
  # NPP (Net Primary Productivity) data ----
  npp_df <- get_npp(fire_meta_dat = fire_meta_table,
                    bound = buff_boundry,
                    str_mask = stream_mask,
                    snap_rast = dnbr)
  
  if(is.null(npp_df)) {
    
    cat(crayon::red("\nNPP data retrieved is not ten years, skipping\n"))
    
    next
    
  }
  
  # ******************************************************
  
  # Landfire data ----
  lf_df <- get_lf(bound = buff_boundry,
                       str_mask = stream_mask,
                       fire_meta_dat = fire_meta_table,
                       snap_rast = dnbr,
                       u_crs = map_crs,
                       cc_only = FALSE)
  
  

  # ******************************************************
  
  # ERC (Energy Release Component) ----
  ### This is fire weather 
  erc <- get_erc(fire_meta_dat = fire_meta_table,
                    bound = buff_boundry)
  
  # ******************************************************
  
  
  # DEM ----
  
  ## Raster Only ----
  dem_rast_big_buff <- get_dem(server_loc = belmont_dirs,
                      boundry = big_buff_boundry,
                      str_mask = stream_mask,
                      snap_rast = dnbr,
                      values_only = FALSE)
  
  ## Elevation values ----
  elev_df <- get_dem(server_loc = belmont_dirs,
                     boundry = buff_boundry,
                     str_mask = stream_mask,
                     snap_rast = dnbr,
                     values_only = TRUE)
  
  # ******************************************************
  
  # Slope & Aspect ----
  slope_asp_df <- get_slope_asp(dem_r = dem_rast_big_buff, 
                               boundry = buff_boundry,
                               str_mask = stream_mask,
                               snap_rast = dnbr)
  
  
  # ******************************************************
  
  # HSP (Hiearchical Slope Position) ----
  hsp_df <- get_hsp(dem_r = dem_rast_big_buff, 
                    boundry = buff_boundry, 
                    str_mask = stream_mask, 
                    snap_rast = dnbr)
  
  # ******************************************************
  
  # HLI (Heat Load Index) ----
  hli_df <- get_hli(dem_r = dem_rast_big_buff, 
                    boundry = buff_boundry, 
                    str_mask = stream_mask, 
                    snap_rast = dnbr)
  
  # ******************************************************
  
  # Bind together and write to file
  
  ## Bind
  combo <- inner_join(dnbr_df,
                      npp_df,
                      by = c("x", "y")) %>% 
    inner_join(., lf_df,
               by = c("x", "y")) %>%
    mutate(erc = erc) %>%
    inner_join(., slope_asp_df,
               by = c("x", "y")) %>%
    inner_join(., elev_df,
               by = c("x", "y")) %>%
    inner_join(., hsp_df,
               by = c("x", "y")) %>%
    inner_join(., hli_df,
               by = c("x", "y")) %>%
    mutate(fire_name = fire_meta_table$fire_name,
           fire_id = fire_meta_table$fire_id) %>%
    relocate(fire_name, .before = 1) %>%
    relocate(fire_id, .before = fire_name) %>%
    as_tibble()  
  

  outfile <- paste0(str_replace_all(fire_meta_table$fire_name, "/", "_"), 
                    "_", 
                    fire_meta_table$fire_id,
                    ".csv")
  
  write_csv(combo, here("output-data/fire-features", outfile))
  
  ## And write a summary metadata table for each fire
  
  if(j == 1){
    
    write_csv(fire_meta_table, here("output-data", 
                                    "all_fires_metadata.csv"),
              append = FALSE)
    
  } else if(j > 1) {
    
    
    write_csv(fire_meta_table, here("output-data", 
                                    "all_fires_metadata.csv"),
              append = TRUE)
    
  }
  

  
}



# FIN

# //////////////////////////////////////////////////////////////////////////////
# //////////////////////////////////////////////////////////////////////////////



