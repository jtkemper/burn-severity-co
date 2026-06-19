# //////////////////////////////////////////////////////////////////////////////
  
# This script does WHAAAAAAAAAAAAT

#**Inputs**
  
  
  
#**Outputs**
  
  
  
# //////////////////////////////////////////////////////////////////////////////
# //////////////////////////////////////////////////////////////////////////////

# **************************************************************

# Housekeeping ----

## Packages ----

### General 

require(here)

### Data mgmt & processing

require(tidyverse)

### File mgmt

require(boxr)
require(usethis)
require(xml2)
require(fs)

### Env. mgmt

require(reticulate) # Rbinder to Pyton env.
require(rlang) # Error handling

### Spatial stuff

require(terra) # For rasters
require(sf) # For shapefiles
require(rgee) # For Goolge Earth Engine
require(spatialEco) # For some topography stuff

### Data acquisition

require(rlandfire) # For landfire data
require(rvest) # For HTML stuff
require(elevatr) # For 

### Viz

require(ggthemes)
require(cols4all)
require(tmap)
require(mapview)
require(shapviz)
require(wacolors)

### Random forest

require(xgboost)
require(lightgbm)
require(fastshap)
require(rsample)
require(tidymodels)
require(bonsai)




# **************************************************************

## Center yourself ----

### Where are we???????????

here::i_am("00_functions.R")

# **************************************************************

## Activate GEE ----

#### GEE (this is bastardized Python lmao)

import("ee")

#### Initialize

ee$Initialize()

# **************************************************************

# //////////////////////////////////////////////////////////////////////////////
# //////////////////////////////////////////////////////////////////////////////


# Functions ----

## Data prep ----

#fire_file <- mtbs_files[22]

#rm(fire_file)

## This function retrieves metadata from a specified metadata folder
## that comes with the MTBS bundle for each fire event 

get_fire_metadata <- function(fire_file) {
  
  cat(crayon::green("\nReading metadata\n"))
  
  ### Find the metadata file(s)
  
  fire_meta_file <- dir_ls(fire_file, glob = "*.xml") 
  
  ### If there's more than one metadata file
  ### Ignore the one that is a "Supplementary" file
  ### Because MTBS loves to give extra information sometimes
  ### And make the fire_meta_file just the one that is NOT the supplementary 
  ### To do this we read each one in, check the root name, and the have a logical
  ### as to whether or not it is the one we want (i.e., root name not Supplementary XXXX)
  ### then we extract the file path that we DO want and read it back in
  
  root_list <- list()
  
  for(j in 1:length(fire_meta_file)){
    
    
    fire_metadata <- xml2::read_xml(fire_meta_file[j])
    
    root_name <- xml_name(xml_root(fire_metadata))

    root_list[[j]] <- tibble(desired_meta_file = ifelse(root_name == "metadata",
                                                          TRUE,
                                                          FALSE),
                               meta_file_path = fire_meta_file[j])
    
  }
  
    root_tibble <- bind_rows(root_list)
    
    fire_meta_file <- root_tibble %>%
      filter(desired_meta_file == TRUE) %>%
      .$meta_file_path %>% 
      as_fs_path()
    
    
    ### Now that we have the file we want
    ### Extract some information from that file
    
    fire_metadata <- xml2::read_xml(fire_meta_file)
    
    #### First, fire event ID
    
    fire_id <- fire_metadata %>%
      xml_find_all("//idinfo/citation/citeinfo/title") %>%
      xml_text() %>%
      str_extract(., "[^-]*$") %>%
      str_trim(.) %>%
      tibble() %>%
      rename(fire_id = 1)
    
  #### Then, fire ignition data
  
  fire_ig_date <- fire_metadata %>%
    xml_find_all("//idinfo/timeperd/timeinfo/sngdate/caldate") %>%
    xml_text() %>%
    tibble(ignition_date = .) %>%
    mutate(ignition_date = lubridate::mdy(ignition_date)) %>%
    mutate(ignition_year = year(ignition_date))
  
  #### Then, fire name (if known) & size
  
  ##### Get node location of supplementary info 
  ##### (Where fire name is located)
  
  suppl_info_node <- fire_metadata %>%
    xml_find_all("//idinfo/descript/supplinf") 
  
  #### Extract all the supplementary info
  
  suppl_info <- xml_text(suppl_info_node)
  
  ##### Get just the info we want 
  ##### (fire name and size)
  
  name_and_size <- read_delim(suppl_info, delim = "\n") %>%
    rename(params = 1) %>%
    filter(str_detect(params, "Name|Perimeter")) %>%
    separate(params, 
             into = c("param", "value"),
             sep = ":") %>%
    mutate(across(everything(), tolower))%>%
    mutate(param = str_remove(param, "\\(.*\\)")) %>%
    mutate(across(everything(), str_trim)) %>%
    mutate(across(everything(), ~str_replace_all(., " ", "_"))) %>%
    pivot_wider(names_from = param, values_from  = value) %>%
    mutate(acres_within_fire_perimeter = as.numeric(acres_within_fire_perimeter))
  
  #### Combine metadata into one table

  fire_meta_table <- bind_cols(fire_id, fire_ig_date, name_and_size)
  
  #### Return
  
  return(fire_meta_table)
  
  
  
  
}

# ******************************************************************************
# ******************************************************************************

## Now a function to retrieve the files for the fire boundary and stream mask
## This is an internal function that we don't really need, because all we need
## is the projected 

get_fire_boundry <- function(fire_file, u_crs) {
  
  cat(crayon::green("\nReading fire boundry\n"))
  
  #### Get fire boundry file
  fire_bdnry_file <- dir_ls(fire_file, glob = "*.shp") %>%
    .[str_detect(., "burn_bndy")]
  
  
  ### Download files 
  
  ### Import shape files to R
  
  #### Fire boundary
  
  fire_bndry <- st_read(fire_bdnry_file) %>%
    st_as_sf() %>%
    st_transform(., crs = u_crs)
  
  return(fire_bndry)
  
}

# ******************************************************************************
# ******************************************************************************

### And a function to get the stream mask from the fire file
### Important! If it does not exist we must return a null

get_streammask <- function(fire_file, u_crs) {
  
  cat(crayon::green("\nReading stream mask\n"))
  
  ### Get file location
  
  sm_file <- dir_ls(fire_file, glob = "*.shp") %>%
    .[str_detect(., "mask")]
  
  ### Read in stream mask
  
  sm <- st_read(sm_file) %>%
    st_as_sf() %>%
    st_transform(., crs = map_crs)
  
  #### Check if there is a stream mask present 
  #### This is important for masking values later 

  
  return(sm)
  
  
  
}

# ******************************************************************************
# ******************************************************************************

## This function buffers the fire boundry inward to avoid edge effects
## On the boundry of the fire

buffer_boundry <- function(bndry_file){
  
  cat(crayon::green("\nBuffering fire boundry\n"))
  
    ### Add inward 100 m buffer
  
  fire_buffer <- st_buffer(bndry_file, dist = -100)
  
  ##### And remove empty or invalid geometries or those with NA dimensions
  
  fire_buffer <- fire_buffer[!sf::st_is_empty(fire_buffer), ]
  
  fire_buffer <- sf::st_make_valid(fire_buffer)
  
  fire_buffer <- fire_buffer[!is.na(sf::st_dimension(fire_buffer)), ]
  
  
  
  

}

# ******************************************************************************
# ******************************************************************************

# ******************************************************************************
# ******************************************************************************

## This function buffers the fire boundry OUTWARD to allow for calculation of 
## paramaters that use a moving window and need pixels that technically fall outside
## the fire boundry 

buffer_boundry_big <- function(bndry_file){
  
  cat(crayon::green("\nBuffering fire boundry with the BIG BOYYYY\n"))
  
  ### Add outward 1K buffer
  
  fire_outward_big_buff <- st_buffer(bndry_file, dist = 1000)
  
  return(fire_outward_big_buff)
  
  
}

# ******************************************************************************
# ******************************************************************************

# Function to mask whatever raster by a stream mask, if provided

mask_by_stream <- function(r = NULL,
                           str_mask = NULL) {
  
    
    #### Check to see if the stream mask is already a terra SpatVector
    #### If not, make a SpatVector
    
    if(inherits(str_mask, "SpatVector") == FALSE) {
      
      str_mask <- vect(str_mask)
      
    } else str_mask <- str_mask
    
    ####### Check we are in the right projection 
    
    c_msk <- terra::crs(str_mask, describe = TRUE)$code
    
    c_r <- terra::crs(r, describe = TRUE)$code
    
    if(c_msk != c_r){
      
      cat(crayon::red("\nReproj stream mask\n"))
      
      str_mask <- terra::project(str_mask, c_r)
      
    }
    
    #### Then mask it by the stream mask 
    
    r_masked <- terra::mask(r, str_mask, inverse = TRUE)
    
    return(r_masked)
    
  }
  
  
# ******************************************************************************
# ******************************************************************************  

# Now a function to retrieve the dnbr file provided by MTBS
# clip it to the fire boundry
# And mask it by the stream mask 
# And make sure it is in our desired projection


get_processed_dnbr <- function(fire_file = NULL,
                     bound,
                     str_mask,
                     u_proj
                     ) {
  
  cat(crayon::green("\n Reading dNBR \n"))
  
  ## First, download the dnbr file within the MTBS file
  
  ### Find the file
  dnbr_files <- dir_ls(fire_file, glob = "*.tif") %>%
    .[str_detect(., "nbr") & (!str_detect(., "nbr6") | !str_detect(., "6")) &
        !str_detect(., "rdnbr")]
  
  ### Read it in
  
  dnbr <- rast(dnbr_files)
  
  ### Change it to numeric
  
  dnbr <- as.numeric(dnbr)
  
  dnbr <- terra::project(dnbr, u_proj)
  
  ## Now, clip it to the fire boundry
  ## And mask it by the stream file (if it exists)
  
  #### Clip to the buffered fire boundary
  
  dnbr_clipped <- terra::crop(dnbr, bound, mask = TRUE)
  
  #### And mask out the stream, if a stream mask is present
  
  stream_mask_present <- if(nrow(str_mask) == 0) FALSE else TRUE
  
  if(stream_mask_present == TRUE){
    
    dnbr_clipped <- mask_by_stream(dnbr_clipped, str_mask)
    
  }
  
  return(dnbr_clipped)

  
}


# ******************************************************************************
# ******************************************************************************

 # rm(bound, str_mask, fire_meta_dat,dnbr_r, u_crs, cc_only, snap_rast, 
 #    lf_ndd, lf_dd)
 
# bound = buff_boundry
# str_mask = stream_mask
# fire_meta_dat = fire_meta_table
# dnbr_r = dnbr
# u_crs = map_crs
# cc_only = FALSE
# snap_rast = dnbr



get_lf <- function(bound, 
                   str_mask,
                   fire_meta_dat,
                   snap_rast,
                   u_crs,
                   cc_only = FALSE){
  
  cat(crayon::green("\nGetting LANDFIRE data\n"))
  
  ## Figure out which version we are after 
  
  which_lf_gen <- get_lf_version(fire_meta_dat)
  
  ## Now figure out which variables we need from where
  
  ### If it is 2001 or 2008-2014, we'll download things from the Belmont server
  ### If it is 2016, 2020-present, we'll download things from LANDFIRE directly
  ### using the R tool
  
  #### First, a variable location dictionary/lookup table 
  all_lf_vars <- read_csv(here("input-data/all_lf_vars.csv"))
  
  #### Then, Get the specify LANDFIRE variables needed and their locations 
  #### For a specific fire 
  #### Note: If path == "direct_download" we'll need to download these with 
  #### the R tool 
  
  lf_vars <- all_lf_vars %>%
    mutate(diff_date = year(fire_meta_table$ignition_date) - fire_available_year) %>%
    filter(diff_date >= 0) %>%
    dplyr::group_by(abbrev) %>%
    slice_min(diff_date) 
  
  #### Finally, divide up the variables we need by their locations
  #### Local-ish (really,on the Belmont server) - lf_vars_ndd
  #### Or via the R package - lf_vars_dd
  
  lf_vars_dd <- lf_vars %>% filter(path == "direct_download")
  
  lf_vars_ndd <- lf_vars %>% filter(path != "direct_download")
  
  #### Finally, finally, if we're only trimming by Canopy Cover
  #### We want to restrict our search to canopy cover 
  if(cc_only == TRUE){
    
    lf_vars_dd <- lf_vars_dd %>%
      filter(str_detect(layer_name, "CC") & !str_detect(layer_name, "FCC"))
    
    lf_vars_ndd <- lf_vars_ndd %>%
      filter(str_detect(layer_name, "CC") & !str_detect(layer_name, "FCC"))
    

    
  } 
  
  ## And now, get the landfire variables
  ## First, get the LF variables that are available via the API 
  ## using the rlandfire package 
  
  if(nrow(lf_vars_dd) > 0){ 
    
    lf_dd <- get_lf_dd(lf_vars = lf_vars_dd,
                       bound = bound,
                       str_mask = str_mask, 
                       snap_rast = snap_rast,
                       u_crs = u_crs,
                       cc_o = cc_only)
  } #else if(lf_vars_dd == 0) {lf_dd <- lf_vars_dd}
  
  ## Then, get the landfire variables that we have stored locally-ish
  ## (Really on the Belmont server)
    ### We had to do this because they are no longer served online by USFS
  
  
  if(nrow(lf_vars_ndd) > 0){ 
    
    lf_ndd <- get_lf_ndd(lf_vars = lf_vars_ndd,
                         bound = bound,
                         str_mask = str_mask, 
                         snap_rast = snap_rast,
                         u_crs = u_crs,
                         cc_o = cc_only)
  } #else if(lf_vars_ndd == 0) {lf_ndd <- lf_vars_ndd}
  
  
  #### Bind together
  
  if(nrow(lf_vars_dd) > 0 & nrow(lf_vars_ndd) > 0) {
    
    lf_df <- inner_join(lf_dd, lf_ndd, 
                        by = c("x", "y"))
    
  } else if(nrow(lf_vars_dd) == 0) {
    
    lf_df <- lf_ndd
    
    
    
  } else if(nrow(lf_vars_ndd) == 0){
    
    lf_df <- lf_dd
    
  }
  
  return(lf_df)
  
  
  # if(cc_only == TRUE){
  #   
  #   #### Extract the value of "forested" from whichever
  #   #### landfire function returned a real value
  #   
  #   forested <- keep(list(lf_dd, lf_ndd), ~ nrow(.x) > 0)[[1]]
  #   
  #   return(forested)
  #   
  #   
  # } else if(cc_only == FALSE){
  #   
  #   
  #   
  #   
  # }
  # 
  
  
  
  
  
  
}
  

# ******************************************************************************
# ******************************************************************************

## This is a function to tell us which landfire version we are after for each file
## As a function of what year the fire burned 
## This is important because depending on the version of the landfire file we are
## after, it will live in a very different place

get_lf_version <- function(fire_meta_df) {

  
  ### LANDFIRE Version table
  
  #### First, let's get a table that links the version of LANDFIRE to the year
  #### it was generated 
  #### This is necessary because LANDFIRE stores things by version number, 
  #### not by the year that data pertains to
  #### The year it pertains to in terms of on the ground conditions available to fire
  #### is, generally, the version year + 1 (except for 2001 and 2016, which are the basemaps)
  #### In general, the version year reflects disturbances up AND INCLUDING that year
  #### For example, the LF 2024 Update is so named because it includes disturbance events from the year 2024. 
  #### See: https://landfire.gov/sites/default/files/documents/README_LF_FileNames.txt
  
  #### So here the first thing we are doing is just to see which version we are after
  #### Because that determines whether we download from the web OR from our local server
  
  lf_version_tbl <- read_csv("input-data/lf_version_tbl.csv")
  
  
  #### Get which version year we are after for our specific fire
  
  which_lf_gen <- fire_meta_df %>%
    inner_join(., lf_version_tbl,
               join_by(closest(ignition_year > fire_available_year))) 
  
  
  which_lf_gen <- as.integer(str_extract(which_lf_gen$version_code, "^\\d+"))
  
  return(which_lf_gen)
  
  
  
}

# ******************************************************************************
# ******************************************************************************
#### This is a function that downloads LANDFIRE data using the landfire API
#### and rlandfire package
#### It downloads whatever our landfire variables of interest are
#### (as declared by the lf_vars argument)
#### Clips them to the our specified fire boundary and masks out streamlines
#### (if present) and then reprojects in our coordinate system of choice
#### will snapping to a reference raster (usually our dnbr raster)
#### It also takes an argument for whether (or not) we just want to
#### retrieve canopy cover, which is done to filter out the non-forested fire
#### locations

get_lf_dd <- function(lf_vars, 
                      bound, 
                      str_mask, 
                      snap_rast,
                      u_crs,
                      cc_o){
  
  
  cat(crayon::yellow("Getting LANDFIRE data from API"))
  
  ### Declare the AOI  
  
  lf_aoi <- getAOI(bound)
  
  ### Find the LF variables from the version that was released closest to 
  ### (but not after) the ignition date of each fire 
  
  ### Download vars
  
  lf_path <- tempfile(fileext = ".zip")
  
  tmpdir2 <- tempfile()
  
  landfireAPIv2(products = lf_vars$layer_name,
                email = em,
                aoi = lf_aoi,
                path = lf_path,
                projection = u_crs,
                method = "libcurl")
  
  
  ### Unzip and load the file
  
  #### Unzip
  
  utils::unzip(lf_path, exdir = tmpdir2)
  
  #### Load locally
  lf_files <- list.files(tmpdir2, pattern = ".tif$", 
                         full.names = TRUE, 
                         recursive = TRUE)
  
  
  lf <- terra::rast(lf_files)
  
  ##### Clip & Mask it 
  
  ###### Clip & mask to the boundry
  
  lf_trim <- crop(lf, bound, mask=TRUE)
  
  ###### Mask to streams/water (if present)
  
  stream_mask_present <- if(nrow(str_mask) == 0) FALSE else TRUE
  
  if(stream_mask_present == TRUE){
    
    lf_trim <- mask_by_stream(lf_trim, str_mask)

    
  }
  
  ##### Project
    lf_trim <- terra::project(lf_trim, snap_rast, method = "near")
  
  
  
  ##### Check re-proj is what we want
  
  check_p <- terra::crs(lf_trim, describe = TRUE)
  
  if(check_p$code != as.character(map_crs)){
    
    #stop("LANDFIRE projections don't match MTBS projections")
    
    rlang::abort("LANDFIRE projections don't match MTBS projections")
    
  }
  
  ## Now, only if we are ONLY checking canopy coverage, we want to 
  ## Just see if more than 80% of the fire area was unforested
  ## Prior to fire
  ## And return that 
  
  if(cc_o == TRUE) {
    
    cc_name <- names(lf_trim)[str_detect(names(lf_trim), "CC") & 
                                !str_detect(names(lf_trim), "FCC")]
    
    if(length(cc_name) > 0) {
      
      ### Extract the canopy cover layer and calculate zonal stats
      lf_cc <- lf_trim[[cc_name]]
      
      ##### First we need to make the fire boudnary a simple feature
      bound_sf <- st_as_sf(bound)
      
      ##### And remove empty or invalid geometries or those with NA dimensions
      bound_sf <- bound_sf[!sf::st_is_empty(bound_sf), ]
      
      bound_sf <- sf::st_make_valid(bound_sf)
      
      bound_sf <- bound_sf[!is.na(sf::st_dimension(bound_sf)), ]
      
      
      ##### Now we can calculate the zonal stats
      
      cc_frac <- exactextractr::exact_extract(lf_cc, 
                                              bound_sf, 
                                              fun = "frac")
      
      ##### And then the area of each boundry "sub-area"
      ##### This is important to do if the fire has multiple
      ##### non-contiguous zones
      ###### First make the boundary a vector
      
      bound_v <- vect(bound_sf)
      
      bound_v_rp <- project(bound_v, lf_cc)
      
      ###### Calculate area of each sub-polygon
      ###### And the percent of the total fire area they make up
      
      sub_size <- expanse(bound_v_rp, 
                          unit = "km")[expanse(bound_v_rp) >0]
      
      sub_size_df <- tibble(sub_size_km2 = sub_size) %>%
        mutate(total_size = sum(sub_size_km2)) %>%
        mutate(pct_area = sub_size_km2/total_size)
      
      ##### And add up how much of the fire is 0% or <10% forested 
      ##### By using a weighted percentage based on the percent of the total fire
      ##### area contained within any sub-area (i.e., non-contiguous burned polygons)
      cc_forest_thresh <- cc_frac %>% 
        as_tibble() %>%
        bind_cols(., sub_size_df %>%
                    dplyr::select(pct_area)) %>%
        pivot_longer(cols = -pct_area,
                     values_to = "fraction_cc",
                     names_to = "cat") %>%
        mutate(weighted_pct = fraction_cc*pct_area) %>%
        filter(cat %in% c("frac_0", "frac_5")) %>%
        dplyr::group_by(cat) %>%
        summarise(frac_less_ten = sum(weighted_pct)) %>%
        dplyr::ungroup() %>%
        summarise(frac_less_ten = sum(frac_less_ten))
    
      
      ### If more than 90% of the fire is unforested, move to our next loop
      if(cc_forest_thresh$frac_less_ten >= 0.9){
        
        forested_fire <- FALSE
        
      } else if(cc_forest_thresh$frac_less_ten < 0.9){
        
        forested_fire <- TRUE
        
      } else {
        forested_fire <- NA
      }
      
    }
    
    return(forested_fire)
       
  } else if(cc_o == FALSE){
    
    
    ##### Extract for data frame
    
    lf_trim_df <- terra::as.data.frame(lf_trim, xy = TRUE)
    
    ##### Rename 
    
    ###### Set names vector
    
    names_v <- setNames(lf_vars$abbrev, lf_vars$layer_name)
    
    ###### Use as the new names
    ###### What this does is for each column name it checks to see if it corresponds
    ###### To one of the names in our names vector
    ###### And, for the one that does, it renames it based on the value of that 
    ###### corresponding column (so the column that it shares the name with) in our
    ###### names vector, which is the new name that we wish to give it
    
    lf_trim_df2 <- lf_trim_df %>%
      rename_with(~ {
        new_names <- .x
        for (i in seq_along(names_v)) {
          hit <- str_detect(.x, names(names_v)[i])
          new_names[hit] <- names_v[i]
        }
        new_names
      }) %>%
      rename_all(tolower)
    
    
    
    
    lf_trim_df2 <- lf_trim_df2 %>%
      mutate(across(any_of(c("evh", "evt", "evc", "fbfm40", "fccs")), 
                    ~as.factor(.))) %>%
      mutate(across(any_of(c("cbd", "cbh", "cc", "ch")), 
                    ~as.integer(.)))
            
    return(lf_trim_df2)
    
    
          }


}



# ******************************************************************************
# ******************************************************************************
## Now a function to retrieve landfire data from our locally(ish) stored rasters
## which live on the Belmont server
## This function takes several arguments: 
##### **lf_vars** is a dataframe specifies the specific landfire variables desired
##### **bound** is a vector that is the fire boundary we wish to clip things to
##### **str_mask** is a streamline file provided with mtbs that masks out water, if present
##### **u_crs** is the user-specified coordinate system (a string)
##### **cc_o** is a Boolean variable about whether we want to only retrieve canopy cover
## This function overall retrieves the data of interest, clips it to a specified 
## boundary file, masks out streams/water (if present), and snaps and re-projects
## to a specified coordinate system and reference raster 

# lf_vars = lf_vars_ndd
# bound = buff_boundry
# str_mask = stream_mask
# fire_meta_dat = fire_meta_table
# snap_rast = dnbr
# u_crs = map_crs
# cc_o = FALSE
# i = 1

get_lf_ndd <- function(lf_vars, 
                       bound, 
                       str_mask, 
                       snap_rast,
                       u_crs,
                       cc_o) {
  
  cat(crayon::yellow("Getting LANDFIRE data from Belmont server"))
  
  if(cc_o == TRUE){
    
    lf_vars <- lf_vars %>% filter(abbrev == "CC")
    
  }
  
  ### Declare a list where we are going to save the various LF vars
  
  r_df <- list()
  
  t_l <- list()
  
  for(i in 1:nrow(lf_vars)) {
    
      ### Get variable name
      ### Print counter
      
      lfvar <- lf_vars$abbrev[i] %>% tolower()
      
      cat(crayon::yellow("\nExtracting", lfvar, "\n"))
      
      
      #### First, get the specific raster file
      #### And the coordinate system of that file 
      
      r <- terra::rast(lf_vars$path[i])
      
      crs_r <- paste0("epsg:",
                      terra::crs(r, describe=TRUE)$code)
      
      
      ##### Project the fire boundary so that it is 
      ##### in whatever projection the LF raster is in 
      ##### and then vectorize the fire boundary for more convenient use in terra
      ##### We are doing this because projecting the LF data, which is a large
      ##### CONUS scale raster, would take more time 
      ##### We just need to reproject the final products to our ultimate map projection
      ##### Which here is set by the user
      
      bound_v <- vect(bound)

      ##### Make sure the fire boundary is in the same projection as the raster
      ##### This is important for masking because we don't want to project
      ##### The entire CONUS raster 
      
      bound_v_rp <- project(bound_v, r)
      
      ##### Clip & mask raster by (reprojected) boundary vector
      
      r_trim <- crop(r, bound_v_rp, mask=TRUE)
      
      ##### Mask to streams (if present)
      
      stream_mask_present <- if(nrow(str_mask) == 0) FALSE else TRUE
      
      
      if(stream_mask_present == TRUE){
        
        #### Check to see if the stream mask is already a terra SpatVector
        #### If not, make a SpatVector
        
        if(inherits(str_mask, "SpatVector") == FALSE) {
          
          str_mask <- vect(str_mask)
          
        } else str_mask <- str_mask
        
        ### Project the stream mask
        ### And mask the raster
        
        str_mask_rp <- project(str_mask, r)
        
        r_trim <- mask(r_trim, str_mask_rp, inverse = TRUE)
        
      }
      
      ##### If all we're after is canopy coverage in the fire as a whole
      ##### We want to just determine the percentage of the fire that is 
      ##### less than 10% forested 
      ##### (Functionally, this is the percent of the fire boundary occupied
      ##### by pixels that have canopy_cover < 10%)
      ##### These show up in the data as 0 (no cc) or 5 (0% < cc < 10%)
      
      if(cc_o == "TRUE"){
        
            #### Calculate zonal stats on fire boundary
            #### (Area occupied by each canopy cover class)
            
            ##### First we need to make the fire boudnary a simple feature
            bound_v_rp_sf <- st_as_sf(bound_v_rp)
            
            ##### And remove empty or invalid geometries or those with NA dimensions
            bound_v_rp_sf <- bound_v_rp_sf[!sf::st_is_empty(bound_v_rp_sf), ]
            
            bound_v_rp_sf <- sf::st_make_valid(bound_v_rp_sf)
            
            bound_v_rp_sf <- bound_v_rp_sf[!is.na(sf::st_dimension(bound_v_rp_sf)), ]
            
            
            ##### Now we can calculate the zonal stats
            
            cc_frac <- exactextractr::exact_extract(r_trim, 
                                                    bound_v_rp_sf, 
                                                    fun = "frac")
            
            ##### And then the area of each boundry "sub-area"
            ##### This is important to do if the fire has multiple
            ##### non-contiguous zones
            sub_size <- expanse(bound_v_rp, 
                                unit = "km")[expanse(bound_v_rp) >0]
            
            sub_size_df <- tibble(sub_size_km2 = sub_size) %>%
              mutate(total_size = sum(sub_size_km2)) %>%
              mutate(pct_area = sub_size_km2/total_size)
            
            ##### And add up how much of the fire is 0% or <10% forested 
            
            cc_forest_thresh <- cc_frac %>% 
              as_tibble() %>%
              bind_cols(., sub_size_df %>%
                          dplyr::select(pct_area)) %>%
              pivot_longer(cols = -pct_area,
                           values_to = "fraction_cc",
                           names_to = "cat") %>%
              mutate(weighted_pct = fraction_cc*pct_area) %>%
              filter(cat %in% c("frac_0", "frac_5")) %>%
              dplyr::group_by(cat) %>%
              summarise(frac_less_ten = sum(weighted_pct)) %>%
              dplyr::ungroup() %>%
              summarise(frac_less_ten = sum(frac_less_ten))
            
            
            ### If more than 90% of the fire is unforested, move to our next loop
            if(cc_forest_thresh$frac_less_ten > 0.9){
              
                  forested_fire <- FALSE
                  
                } else if(cc_forest_thresh$frac_less_ten <= 0.9){
                  
                  forested_fire <- TRUE
                  
                } else {
                  forested_fire <- NA
                }
            
            return(forested_fire)
      
    
        
        
      } else if(cc_o == FALSE) {
        
        
        
        
        ##### Re-project to the original projection (NAD83 UTM 13N)
        ##### Make sure we are using the clipped dnbr layer as our reference
        ##### to avoid issues with misalignment 
        ##### And make sure we are using nearest neighbor because technically
        ##### Landfire data is not continuous
        
        r_trim_rp <- project(r_trim, snap_rast, method = "near")
        
        ##### Check re-proj is what we want
        
        check_p <- terra::crs(r_trim_rp, describe = TRUE)
        
        if(check_p$code != as.character(map_crs)){
          
          #stop("LANDFIRE projections don't match MTBS projections")
          
          rlang::abort("LANDFIRE projections don't match MTBS projections")
          
        }
        
        

          #### A few landfire variables are categorical, a few are numeric
          #### If we are looking at a categorical vars
          #### we want to keep the data categorical
          #### If it's not one of those, we want to make it numeric
          #### Let's start with the IF NOT CATEGORICAL
          
          
          if(!lfvar %in% c("evh", "evt", "evc", "fbfm40", "fccs")) {
            

              #### Now make sure it is numeric
              #### And rename it by the accepted LANDFIRE abbreviation
            
                r_trim2_rp <- as.numeric(r_trim_rp)
                
                names(r_trim2_rp) <- lfvar
                
                t_l[[i]] <- r_trim2_rp
                
                r_df[[i]] <- terra::as.data.frame(r_trim2_rp, xy = TRUE) %>%
                  mutate(across(all_of(lfvar), ~as.integer(.)))
                
                
              } else{
                

                #### Now make sure it it stays categorical 
                #### And rename it by the accepted LANDFIRE abbreviation
                
                ###### First, get the "integer" codes for the categorical variable
                ###### This keeps things simple for the later model
                ###### (doesn't muddy the waters with names and stuff)
                ###### But then makes sure the "integers" stay categorical
                
                r_trim2_rp <- as.factor(r_trim_rp)
                
                names(r_trim2_rp) <- lfvar
                
                t_l[[i]] <- r_trim2_rp
                
                ###### And rename
                
                r_df[[i]] <- terra::as.data.frame(r_trim2_rp, xy = TRUE) 
                
                
              }
          

        }
    
  }
  
  if(cc_o == FALSE) {
    
    #### Join all together
    t_l_stack <- reduce(t_l, c)
    
    r_df_all <- r_df %>%
      reduce(inner_join, by = c("x", "y"))
    
    return(r_df_all)
    
  }

}

# ******************************************************************************


# Now a function to get NPP (Net Primary Productivity) data from Google Earth
# Engine. This data tells us essentially how green the vegetation in the fire was
# It requires inputs of as follows
## fire_meta_dat – fire metadata table that has fire year
## bound - fire boundary (buffered or otherwise, usualy buffered inward by 1 km)
## str_mask - a stream mask file, if provided
## dnbr - a dnbr raster for each file, which will be used to snap rasters 

# fire_meta_dat <- fire_meta_table
# bound <- buff_boundry
# str_mask <- stream_mask
# snap_rast <- dnbr
# 
# plot(buffer_boundry_big())


get_npp <- function(fire_meta_dat = NULL,
                    bound = NULL,
                    str_mask = NULL,
                    snap_rast = NULL) {
  
    ### Tell 'em 
    cat(crayon::green("\nGetting NPP data from GEE\n"))
  
  
    ### First, we want to calculate ten years pre-fire to filter out the NPP data we need
    ### We want to composite mean NPP over these ten years
    
    npp_period_start <- as.character(year(fire_meta_dat$ignition_date) - 10) %>%
      paste0(., "-01-01")
    
    
    npp_period_end <- as.character(year(fire_meta_dat$ignition_date)) %>%
      paste0(., "-01-01")
    
    
    ### Retrieve NPP for those years from Google Earth Engine
    
    npp_conus <- ee$ImageCollection('UMT/NTSG/v2/LANDSAT/NPP')$
      filterDate(ee$Date(npp_period_start), 
                 ee$Date(npp_period_end))$
      select("annualNPP") 
    
    ### Check to make sure we are getting ten years
    
    if(npp_conus$size()$getInfo() != 10) {
      
      #rlang::abort("NPP data retrieved is not ten years")
      
      return(NULL)
      
    }
    
    
    npp_conus$first()$select("annualNPP")
    
    ### Check the dates on the images to make sure it is right
    #### Note that this dataset stores dates as part of each images index 
    #### rather than is system:start_time or system:end_time, which is often 
    #### the standard container for such info
    #### We want to make sure the last date of the images is the year before the
    #### fire 
    
    if(last(npp_conus$aggregate_array("system:index")$getInfo()) != 
      fire_meta_dat$ignition_year - 1) {
      
      rlang::abort("NPP data retrieved is for the wrong year(s)")
      
    }
    
    
    #### Now, let's composite all ten images (ten years) into one by taking the 
    #### mean NPP at each pixel across the ten years
    #### It is faster to do it for the CONUS-wide image and then clip down to the fire
    #### (rather than vice versa, since clipping each image in the collection requires a loop)
    
    
    ### Get the mean at each pixel for all years in the window
    
    npp_tenyr_mean <- npp_conus$reduce(ee$Reducer$mean())
    
    ### Reproject to original projection
    ### Essential!!! GEE alters the projection often when doing summarising calcs
    
    #### First get a reference projection from our downloaded ERC data
    
    rp <- npp_conus$first()$select("annualNPP")$projection()
    
    #### Extract the GEE projection
    gee_proj <- rp$getInfo()$crs
    
    #### Reproject the 10-yr mean raster
    
    npp_tenyr_mean_rp <- npp_tenyr_mean$reproject(rp)
    
    ### Finally, clip to mean raster to fire boundary
    
    #### Convert fire perimeter to an Google Earth object
    
    fire_bndry_ee <- rgee::sf_as_ee(bound %>%
                                         mutate(Ig_Date = as.character(Ig_Date)),
                                       proj = gee_proj) 
    
    #### Then clip
    
    npp_tenyr_mean <- npp_tenyr_mean_rp$clip(fire_bndry_ee)
    

    
    
    # **************************************************************************
    
    ### Now, we want to bring the data local for use in the RF algorithm
    
    #### Add lat longs to NPP data first
    
    ll <- npp_tenyr_mean$pixelLonLat()
    
    #### Then, make sure the lat-longs are clipped to the fire boundary
    
    npp_ll <- npp_tenyr_mean$addBands(ll$select("longitude", 
                                                   "latitude")$clip(fire_bndry_ee))
    
    #### Then, create a mask from the fire boundary
    #### And mask the lat-longs with it
    #### Together, this ensure that we are remove any lat-long values 
    #### outside of the fire boundary
    #### Because otherwise, we get a mismatch between the number of 
    #### NPP values within the fire boundary
    #### which we've already clipped, and the number of lat-long values, 
    #### which $pixelLatLong creates
    #### using a bounding box
    
    msk <- npp_tenyr_mean$select("annualNPP_mean")$mask()
    
    #### Mask lat-longs 
    
    npp_ll_masked <- npp_ll$updateMask(msk)
    
    #### Reduce the masked image to a list of values
    #### This is necessary because the pixel values are not actually otherwise exposed
    #### To be manipulated as arrays, data.tables, etc.
    #### So here we are creating a dictionary where each key is a band name (lat, long, NPP)
    #### And each value is a list of values 
    #### We can then $get to convert them into EE ComputedObjects, which can in turn be used
    #### finally to extract values by using $getInfo
    #### Basically, the toList() reducer collects pixel values into manageable lists,
    #### which can be accessed using $get, which turns them into ComputedObjects,
    #### and then we can
    #### use getInfo to fetch the data from those Objects (which are on EE servers) and
    #### bring them into our local R session.
    #### This is all necessary because Pixels inside EE images are not directly accessible. 
    #### Images represent large raster grids managed server-side.
    
    px <- npp_ll_masked$reduceRegion(
      
      reducer = ee$Reducer$toList(),
      geometry = fire_bndry_ee$geometry(),
      scale =  npp_tenyr_mean$projection()$nominalScale()$getInfo(),
      maxPixels = 1e13
      
    )
    
    #### Access lists of pixel values using the dictionary (px)
    #### And then fetch the pixel values from the EE servers 
    
    lat <- px$get("latitude")$getInfo()
    lon <- px$get("longitude")$getInfo()
    npp <- unlist(px$get("annualNPP_mean")$getInfo())
    
    #### Turn into a tibble for use in the ML workflow
    
    npp_vect <- tibble(lon = lon, 
                       lat = lat,
                       npp = npp) 
    
    #### Turn that into a local raster for plotting purposes & to enable re-projection
    
    npp_rast <- terra::rast(npp_vect, crs = gee_proj)
    
    #### Reproject
    
    npp_rast_reproj <- terra::project(npp_rast, snap_rast)
    
    #### Mask to streams/water (if present)
    ##### First check if stream mask is present
    
    stream_mask_present <- if(nrow(str_mask) == 0) FALSE else TRUE
    
    ##### If it is, then mask it 
    ##### But also check to make sure it is in the same projection
    
    if(stream_mask_present == TRUE){
      
      npp_rast_reproj <- mask_by_stream(npp_rast_reproj, str_mask)
      
    } else npp_rast_reproj <- npp_rast_reproj
    
    #### Turn into a dataframe for RF modeling
    names(npp_rast_reproj) <- "npp"
    
    npp_reproj_df <- terra::as.data.frame(npp_rast_reproj, xy = TRUE)
    
    
    #### And return it
    return(npp_reproj_df)
  
  
}

# ******************************************************************************
# ******************************************************************************

### Now a function to get fire weather
### We don't really need a stream mask for this because
### The pixels are big (4 km x 4 km, I think)
### And so masking out the stream area doesn't actually do much
### Or a snap raster because we are only calculating a mean for the entire fire
### And not actually exporting pixel-by-pixel data
### So we don't need to snap to our project projection
### We are doing all operations in the original projection

get_erc <- function(fire_meta_dat = NULL,
                    bound = NULL) {
  
  ### Tell 'em 
  cat(crayon::green("\nGetting ERC data from GEE\n"))
  
  ### Get dates of +/- 15 days from ignition
  ### We want to calculate mean ERC for the fire
  ### across these 15 days 
  
  erc_period_start <-  as.character(fire_meta_dat$ignition_date - 15)
  erc_period_end <-  as.character(fire_meta_dat$ignition_date + 15) 
  
  
  ### Now get daily ERC for each of the dates within the 30 day window 
  ### around the ignition date (in total, 31 day window)
  
  erc_all <- ee$ImageCollection('IDAHO_EPSCOR/GRIDMET') $
    filterDate(ee$Date(erc_period_start), 
               ee$Date(erc_period_end)$advance(1, "day")) $ ### Ensures that end date is inclusive 
    select("erc") 
  
  #### Get a standard projection (aka CRS Code) from our downloaded ERC data
  
  meta_d <- erc_all$first()$select("erc")$projection()$getInfo()
  
  gee_crs <- standardize_gee_crs(meta_d)
  

  #### Convert fire perimeter to an Google Earth object
  
  fire_bndry_ee <- rgee::sf_as_ee(bound %>%
                                    mutate(Ig_Date = as.character(Ig_Date)),
                                  proj = gee_crs) 
  
  
  ### Clip each erc image to the fire of interest
  
  erc_clip <- erc_all$
    map(function(img){img$clip(fire_bndry_ee)})
  
  ### Get the mean at each pixel for all days in the window
  
  erc_window_mean <- erc_clip$reduce(ee$Reducer$mean())
  
  ### Now reproject it into the original projection of the ERC data
  ### from the Idaho Mesonet
  ### GEE may use a default internal projection if each image in the collection
  ### doesn't have identical resolution or projection or don't align completely 
  
  #### First get a reference projection from our downloaded ERC data
  
  ref_proj <- erc_clip$first()$select("erc")$projection()
  
  #### Reproject the 31-day mean raster
  
  erc_window_mean_reproj <- erc_window_mean$reproject(ref_proj)
  
  #### Then, calculate the resolution of that re-projected raster
  #### Which we will need to determine the mean of the whole thing
  
  erc_scale_reproj <- erc_window_mean_reproj$projection()$nominalScale()$getInfo()
  
  #### Now, get the overall mean of the raster 
  
  per_fire_mean_dict <- erc_window_mean_reproj$reduceRegion(
    
    reducer = ee$Reducer$mean(), 
    geometry = fire_bndry_ee$geometry(), ### Region over which to get mean
    scale = erc_scale_reproj, ### The resolution of the input raster
    maxPixels = 1e13
    
    
    
  )
  
  ### Extract it
  
  overall_mean_erc <- per_fire_mean_dict$get("erc_mean")$getInfo()
  
  
  
  
}

# ******************************************************************************
# ******************************************************************************

# This is a function that helps extract a standard CRS from GEE objects
# that do not have one. Functionally, what this does is check if it has one
# And, if it doesn't, it checks if it is WGS84 (which is a common GEE projection)
# If its not that, we just return an unknown (we should build more functionality here)

standardize_gee_crs <- function(meta_d_list = NULL) {
  
  ### First, find if there is a standard CRS code listed
  
  if(is.null(meta_d_list$crs)){
    
    proj_string <- meta_d_list$wkt
    
  } else if(!is.null(meta_d_list$crs)){
    
    proj_string <- meta_d_list$crs
    
    return(proj_string)
    
    
  }
  
  ### If not, now let's detect if it is WGS84
  ### Which is common for GEE objects
  
  crs_obj <- st_crs(proj_string)
  
  # Define detection rules 
  # The first three check if its a Geographic Coordinate System
  # The second two check the ellipsoid to see if it is the 
  # WGS84 ellipsoid parameters
  # We are checking the semi-major axis and the inverse flattening
  # which, according to https://reference.org/facts/WGS84/V9wSJnjX 
  # (see table under WGS84 heading)
  # are 6378137 m and 298.257223563, respectively, for WGS84
  
  rules <- tibble(
    test = c(
      "GEOGCS",
      'UNIT\\["degree"',
      'AXIS\\["Longitude"',
      "6378137",
      "298\\.257223563"
    )
  )
  
  # Evaluate all detection rules
  matches <- rules %>%
    mutate(result = str_detect(proj_string, test))
  
  if (all(matches$result)) {
    cat(crayon::blue("\nDetected WGS84 geographic CRS → assigning EPSG:4326\n"))
    
    crs_obj <- st_crs("EPSG:4326")
    
    proj_string <- crs_obj$input
    
    return(proj_string)
    
  } else
    
    cat(crayon::red("\nNo EPSG match detected → keeping original WKT\n"))
  
  return(crs_obj)
  
}


# ******************************************************************************
# ******************************************************************************

## This a function to retrieve a DEM
## Note that this pulls from the west-wide DEM that lives on the Belmont server

get_dem <- function(server_loc = NULL,
                          boundry = NULL, 
                          str_mask = NULL,
                          snap_rast = NULL,
                          values_only = FALSE) {
  
  ### Tell 'em 
  cat(crayon::green("\nGetting DEM data from server\n"))
  
  
  #### First, we need to get the DEM 
  
  ##### Find the dir
  
  dem_dir <- server_loc %>%
    tibble(path = .) %>%
    filter(str_detect(path, "DEM")) %>%
    .$path
  
  ##### Get the file
  
  dem_file <- dir_ls(dem_dir, glob = "*.tif")
  
  ##### Get fire boundry in the same projection
  ##### As the DEM, so we don't have to reproject 
  ##### the entire DEM
  
  dem_30m <- rast(dem_file)
  
  dem_crs <- crs(dem_30m, describe = TRUE)$code %>%
    as.numeric(.)
  
  bound_same_as_dem <- st_transform(boundry, 
                                      crs = dem_crs)
  
  #### Mask and crop the DEM to the specific fire boundary 
  
  clipped_dem <- crop(dem_30m, bound_same_as_dem, mask=TRUE)
  
  #### Re-project to make sure it is in the same projection as everything
  #### And double check
  
  clipped_dem_proj <- project(clipped_dem, snap_rast)
  
  
  # *****************************************************
  
  ##### Double-check our projections are okay
  pr <- terra::crs(clipped_dem_proj, describe = TRUE)
  
  if(pr$code != as.character(map_crs)){
    
    
    rlang::abort("DEM projections don't match MTBS projections")
    
  }
  
  # *****************************************************
  
  #### Now, we will need to check if there's a stream mask
  stream_mask_present <- if(nrow(str_mask) == 0) FALSE else TRUE
  
  #### Extract to df if we want the values only
  #### Otherwise return the actual raster
  
  if(values_only == TRUE) {
    
    ### Tell 'em 
    cat(crayon::yellow("\nExtracting Elevation Data\n"))
    
    #### Mask by stream mask if present 
        
        if(stream_mask_present == TRUE){
          
          clipped_dem_proj <- mask_by_stream(clipped_dem_proj, str_mask)
          
          
        } else clipped_dem_proj <- clipped_dem_proj
    
    #### Transform to datafamre
    
    dem_df <- terra::as.data.frame(clipped_dem_proj, xy = TRUE) %>%
      rename_all(tolower)
    
    return(dem_df)
    
  } else if(values_only == FALSE) {
    
    ### Tell 'em 
    cat(crayon::yellow("\nReturning DEM raster\n"))
    
    #### Project ONLY the clipped DEM using our coordinate system
    #### Not our snapped raster
    #### This protects the DEM from being cut down too early
    #### Importantly we will have to snap things in other functions 
    #### Using this dem
    
    clipped_dem_other_p <- project(clipped_dem, map_proj)
    
     return(clipped_dem_other_p)
    
  }

  
  
}

# ******************************************************************************
# ******************************************************************************

# Now a function to get slope and aspect from a outward buffered DEM
# Clip it to the smaller, inward boundary
# And mask it with a stream mask

get_slope_asp <- function(dem_r = NULL,
                          boundry = NULL, 
                          str_mask = NULL,
                          snap_rast = NULL) {
  
  ### Tell 'em 
  cat(crayon::green("\nGetting slope & aspect values from input DEM\n"))
  

  # //////////////////////////////////////////////
  
  # SLOPE
  
  #### Now caclulate slope
  
  slope <- terrain(dem_r, v = "slope", neighbors = 8, unit = "degrees")
  
  
  #### Now clip to the inward buffer 
  
  slope_fire_buffer <- crop(slope, boundry, mask=TRUE)
  
  
  # //////////////////////////////////////////////
  
  # ASPECT
  
  #### Now caclulate slope
  
  aspect <- terrain(dem_r, v = "aspect", 
                    neighbors = 8, unit = "degrees")
  
  
  #### Now clip to the inward buffer 
  
  aspect_fire_buffer <- crop(aspect, boundry, mask=TRUE)
  
  

  # //////////////////////////////////////////////
  
  # Combine into one raster 
  s_a_rast <- c(slope_fire_buffer, 
                          aspect_fire_buffer)
  
  # And project 
  s_a_rast <- project(s_a_rast, snap_rast)
  
  
  # Mask by stream mask if present 
  
  ## First check if mask is present
  
  stream_mask_present <- if(nrow(str_mask) == 0) FALSE else TRUE
  
  ##  Then do the masking (or not)
  
  if(stream_mask_present == TRUE){
    
    s_a_rast <- mask_by_stream(s_a_rast, str_mask)
    

  } else s_a_rast <- s_a_rast
  
  # //////////////////////////////////////////////
  
  # Extract and return data as a dataframe with x-y coords
  
  s_a_df <- terra::as.data.frame(s_a_rast, xy = TRUE)
  
  return(s_a_df)
  
  
  
}

# ******************************************************************************
# ******************************************************************************

# Now a function to get hierarchical slope position 
# using an outward buffered DEM (!!!! This is essential - HSP uses a moving window
# for calculations and so requires many pixels outside the boundary to get a complete
# raster!!!!)
# Also clips to the inward fire buffer and masks by a stream mask

get_hsp <- function(dem_r = NULL,
                    boundry = NULL, 
                    str_mask = NULL,
                    snap_rast = NULL){
  
  ### Tell 'em 
  cat(crayon::green("\nCalculating HSP values from input DEM\n"))
  
  
  #### IMPORTANT!!!!!!!!!!!!!!
  #### we want the dem clipped to the outward buffered boundary, not just the 
  #### inward buffered boundary, because that influences the calculations
  
  #### A bigger buffer allows us
  #### to calculate slope over a greater spatial area
  #### which means we are able to calculate values for more pixels
  #### And not artificially restrict ourselves to inward buffered area
  #### (we can clip to that area later)
  
  
  
  #### Re-project to make sure it is in the same projection as everything
  c_dem_r <- terra::crs(dem_r, describe = TRUE)$code
  
  c_bound <- terra::crs(boundry, describe = TRUE)$code
  
  if(c_dem_r != c_bound){
    
    cat(crayon::red("\nReproj dem\n"))
    
    dem_r <- terra::project(dem_r, snap_rast)
    
  }
  
  if(terra::expanse(dem_r)$area < sum(expanse(vect(boundry)))){
    
    rlang::abort("\nERROR: Too small for full HSP calc. Use DEM made with bigger buffer\n")
    
      }
  
  #### Now do the HSP calculation
  hsp <- spatialEco::hsp(dem_r, min.scale = 3, max.scale = 27) # These are default scales
  
  #### And clip to the inward buffer 
  
  hsp_fire_buffer <- crop(hsp, boundry, mask=TRUE)
  
  #### And snap it to the dnbr raster
  
  hsp_fire_buffer <- project(hsp_fire_buffer, snap_rast)
  
  #### Mask by stream mask if present 
  
  ##### First check
  
  stream_mask_present <- if(nrow(str_mask) == 0) FALSE else TRUE
  
  ##### Then do the masking (or not)
  
  if(stream_mask_present == TRUE){
    
    hsp_fire_buffer <- mask_by_stream(hsp_fire_buffer, str_mask)
    
    
  } else hsp_fire_buffer <- hsp_fire_buffer
  
  
  #### And extract the values
  
  hsp_fire_buffer_df <- terra::as.data.frame(hsp_fire_buffer, xy = TRUE) %>%
    rename(hsp = 3)
  
  return(hsp_fire_buffer_df)
  
  
}

# ******************************************************************************
# ******************************************************************************

# Now finally a function to get Heat Load Index (or Environmental Cooling Potential)
# maybe (?) Klimas et al. seems to call it both things
# We are calculating HLI from McCune & Keon, 2002
# This function takes a DEM, a fire boundry file, a streammask file, and a raster to which
# to snap everything (this last one must be consistent across all files)

get_hli <- function(dem_r = NULL,
                    boundry = NULL, 
                    str_mask = NULL,
                    snap_rast = NULL){
  
  ### Tell 'em 
  cat(crayon::green("\nCalculating HSP values from input DEM\n"))
  
  ### Importantly, we need this in a geographic coordinate system
  ### (Here, WGS84) because the HLI calculation requires lat-longs in degrees or radians
  
  dem_r_proj <- project(dem_r, "EPSG:4326")
  
  ### Get slope and aspect
  
  sa <- terrain(dem_r_proj, v = c("slope", "aspect"), unit = "radians")
  
  ### Calculate HLI from McCune & Keon, 2002
  ### The more complex one 
  ### (see second page of the paper)
  
  hli_df <- sa %>% terra::as.data.frame(., xy = TRUE) %>%
    mutate(y_rad = y*pi/180,
           folded_aspect = abs(pi - abs(aspect - (5*pi/4)))) %>%
    mutate(hli = -1.467+1.582*cos(y_rad)*cos(slope)-1.5*cos(folded_aspect)*sin(slope)*sin(y_rad)-0.262*sin(y_rad)*sin(slope) + 0.607*sin(folded_aspect)*sin(slope)) %>%
    dplyr::select(x, y, hli) 
  
  #### Turn back into a raster
  
  hli_r <- rast(hli_df, crs = dem_r_proj)
  
  
  #### Project to UTMs and snap it to the dnbr raster
  
  hli_r_reproj <- terra::project(hli_r, snap_rast)
  
  #### Clip to the inward buffered fire area
  
  hli_r_reproj_buff <- crop(hli_r_reproj, boundry, mask=TRUE)
  
  #### Mask by stream mask if present 
  
  ##### First check
  
  stream_mask_present <- if(nrow(str_mask) == 0) FALSE else TRUE
  
  ##### Then do the masking (or not)
  
  if(stream_mask_present == TRUE){
    
    hli_r_reproj_buff <- mask_by_stream(hli_r_reproj_buff, str_mask)
    
    
  } else hli_r_reproj_buff <- hli_r_reproj_buff
  
  
  
  #### And extract 
  
  hli_r_reproj_buff_df <- terra::as.data.frame(hli_r_reproj_buff, xy = TRUE)
  
  #### And return
  return(hli_r_reproj_buff_df)
  
  
  
  
}

# ******************************************************************************
# ******************************************************************************

# //////////////////////////////////////////////////////////////////////////////


## Modeling & Tuning ----

# A function to read in the files that contain dnbr and 

read_fire_files <- function(f){
  
  read_csv(f) %>%
    mutate(fire_name = as.character(fire_name))
  
}

# ******************************************************************************
# ******************************************************************************

# A very basic function for predicting using a model and some new data
# This is required to calculate values using fastshap 

p_func <- function(model, newdata){
  
  predict(model, newdata)
  
}

# ******************************************************************************
# ******************************************************************************

# Now a very big function to run an XGBoost model and calculate some 
# SHAP values 

### We can't use XGBoost well with categorical data. Let's just use 
### LightGBM 
### (We could also use ranger just to see)

# i = 1
# 
# train_df = dnbr_and_drivers %>%
#   filter(usu_fire_index != i)  %>%
#   dplyr::select(!usu_fire_index)
# 
# test_df = dnbr_and_drivers %>%
#   filter(usu_fire_index == i)  %>%
#   dplyr::select(!usu_fire_index)
# 
# tgt_var = "dnbr"
# 
# rounds = 10
# stopping = 5
# explain = FALSE

run_xgb <- function(train_df = NULL,
                    test_df = NULL,
                    tgt_var = "dnbr",
                    rounds = 100,
                    stopping = 20,
                    explain = FALSE) {
  
  set.seed(913)
  
  cat(crayon::cyan("\nSplitting Data, Defining Inputs\n"))
  
  
  # First, Set up train test matrices
  
  train_preds <- data.matrix(train_df %>%
                               dplyr::select(!c("fire_name", "fire_id", all_of(tgt_var))))
  
  
  train_tgt <- train_df[[tgt_var]]

  
  test_preds <- data.matrix(test_df %>%
                            dplyr::select(!c("fire_name", "fire_id", all_of(tgt_var))))
  
  test_tgt <- test_df[[tgt_var]]
  
  ## Define xgb test and train data sets 
  
  xgb_train = xgb.DMatrix(data = train_preds, label = train_tgt, enable_categorical = TRUE)
  
  xgb_test = xgb.DMatrix(data = test_preds, label = test_tgt)
  
  
  #### Set up a watchlist
  
  watchlist = list(train=xgb_train, test=xgb_test)
  
  cat(crayon::cyan("\nTraining Model\n"))
  
  ### Train the model
  xgb_mod <- xgb.train(data = xgb_train, 
                       max.depth = 12,
                       watchlist = watchlist, 
                       nrounds = rounds,
                       early_stopping_rounds = stopping,
                       subsample = 0.8,
                       eta = 0.01,
                       eval_metric = "mae"
                       #gamma = 10
  )
  
  cat(crayon::cyan("\nMaking Predictions\n"))
  
  stats_and_imp <- list()
  
  ### Predict on test dataset
  pred_var_name <- paste0("predicted_", tgt_var)
  
  predicted <- predict(xgb_mod,
                       xgb_test,
                       iterationrange = c(1, 
                                          xgb_mod$best_iteration)) %>%
    as_tibble() %>%
    rename(!!pred_var_name := 1) 
  
  
  #### Bind predictions to test values
  #### And calculate some initial errors
  obs_var_name <- paste0("observed_", tgt_var)
  
  predicted_observed <- bind_cols(test_df %>%
                                    dplyr::rename(!!obs_var_name := !!tgt_var),
                                  predicted) 
  
  
  predicted_observed_err <- predicted_observed %>%
    mutate(raw_err = .data[[pred_var_name]] - .data[[obs_var_name]]) %>%
    mutate(sqrerr = raw_err^2,
           abs_error = abs(raw_err),
           abs_pct_error = abs_error/.data[[obs_var_name]]*100)
  
  stats_and_imp[[1]] <- predicted_observed_err
  
  #### And some summary errors
  #### And baseline predictions (mean of the prediction values)
  
  error_summary <- predicted_observed_err %>%
    dplyr::group_by(fire_id) %>%
    summarise(mae = mean(abs_error),
              mape = mean(abs_pct_error),
              rmse = sqrt(mean(sqrerr)),
              pbias = hydroGOF::pbias(.data[[pred_var_name]],
                                      .data[[obs_var_name]]),
              nse = hydroGOF::NSE(.data[[pred_var_name]],
                                  .data[[obs_var_name]])) %>%
    rename(test_fire_id = fire_id) %>%
    dplyr::ungroup()
  
  baseline <- mean(predicted_observed_err[[pred_var_name]])
  
  error_summary <- error_summary %>%
    mutate(!!paste0("mean_", pred_var_name) := baseline)
  
  stats_and_imp[[2]] <- error_summary
  
  
  ## Get variable importance 
  
  cat(crayon::cyan("\nCalculating Var Importance & SHAPS\n"))
  
  ### Classic variable importance
  
  xgb_var_imp <- xgb.importance(model= xgb_mod)
  
  stats_and_imp[[3]] <- xgb_var_imp
  
  ## And now get SHAP values
  ## Note that this takes a longggggggggggggg time
  
  if(explain == TRUE) {
    

    cat(crayon::green("\nSHAP values\n"))
    
    ### SHAP variable importance
    
    # ex_xg <- fastshap::explain(
    #   xgb_mod,
    #   #X = preds,
    #   pred_wrapper = p_func,
    #   newdata = test_preds,
    #   #baseline = baseline,
    #   adjust = TRUE,
    #   exact = TRUE,
    #   shap_only = FALSE
    # )
    
    #### Then remove extraneous column from shap matrix
    
    # ex_xg$shapley_values <- ex_xg$shapley_values[, colnames(ex_xg$shapley_values) != "BIAS"]
    # 
    # stats_and_imp[[4]] <- ex_xg
    
    cat(crayon::green("\nSHAP viz\n"))
    
    #### And then get and return some files key for visualization
    
    shv <- shapviz::shapviz(xgb_mod, 
                            X_pred = as.matrix(test_preds))
    
    
    
    
    stats_and_imp[[4]] <- shv
    
    
  }
  
  #### And return it all
  return(stats_and_imp)
  
  
}

# ******************************************************************************
# ******************************************************************************

# A function to run the LightGBM algorithm 

# i = 1
# #
# 
# train_valid
# 
# train_df = train_valid %>%
#   filter(group_index != i)  %>%
#   dplyr::select(!c(usu_fire_index,
#                    group_index))
# 
# test_df = train_valid %>%
#   filter(group_index == i)  %>%
#   dplyr::select(!c(usu_fire_index,
#                    group_index))
# 
# 
# tgt_var = "dnbr"
# 
# objective = "tuning"
# 
# hyperparams = NULL
# 
# hyperparams = model_grid[i, ]
# 
# explain = FALSE




run_lgbm <- function(train_df = NULL,
                     test_df = NULL,
                     tgt_var = "dnbr",
                     objective = "regression",
                     application = "testing",
                     hyperparams = NULL,
                     alpha = NULL,
                     explain = FALSE,
                     plot_shaps = FALSE) {
  
  
  set.seed(913)
  
  cat(crayon::cyan("\nSplitting Data, Defining Inputs\n"))
  
  # *************************************
  
  # First, Set up the training and testing matrices 
  
  ## Training 
  
  ### Remove ID cols
  
  train_df <- train_df %>%
    dplyr::select(order(colnames(.)))
  
  ### Transform to matrix and declare target variables
  
  train_preds <- data.matrix(train_df %>% 
                               dplyr::select(!c("fire_name", "fire_id", 
                                                all_of(tgt_var))))
  
  
  train_tgt <- train_df[[tgt_var]]
  
  ## Testing
  
  ### Remove ID cols by subsetting to the same data in the training dataframe
  
  test_df <- test_df %>%
    ungroup() %>%
    dplyr::select(colnames(train_df)) %>%
    dplyr::select(order(colnames(.)))
  
  ### Transform to matrix and declare target variables
  
  test_preds <- data.matrix(test_df %>% 
                              dplyr::select(!c("fire_name", "fire_id", 
                                               all_of(tgt_var))))
  
  
  test_tgt <- test_df[[tgt_var]]
  
  # *************************************
  
  
  # Two, define the lgb test and train data sets 
  
  ## First, we want to find the categorical features
  cat_features <- train_df %>%
    dplyr::select(where(is.factor)) %>%
    names()
  
  ## Now, transform the data matrices in lgb-specific data
  
  train_lgbm <- lgb.Dataset(data = train_preds,
                            categorical_feature = cat_features,
                            label = train_tgt)
  
  test_lgbm <- lgb.Dataset(data = test_preds,
                           categorical_feature = cat_features,
                           label = test_tgt)

  # *************************************
  
  # Three, declare the hyperparameters
  # Make them basically the defaults if none are provided
  # But if some are via a dataframe, use those
  
  if(is.null(hyperparams)) {
    
    params <- list(objective = objective,
                   #metric = "mae",
                   #learning_rate = 0.1,
                   early_stopping_rounds = 100,
                   num_threads = 0
                   #early_stopping_min_delta = 2,
                   #nrounds = 1500
                   
    )
  
  } else {
    
    params <- list(objective = objective,
                   #metric = "mae",
                   metric = "None",
                   num_leaves = hyperparams$num_leaves,
                   min_data_in_leaf = hyperparams$min_n,
                   nrounds = hyperparams$trees,
                   bagging_fraction = hyperparams$sample_size,
                   bagging_freq = 1,
                   feature_fraction = hyperparams$mtry_prop,
                   learning_rate = hyperparams$learn_rate,
                   num_threads = 0
                   )
    
    ### If we're tuning, use early stopping
    
          if(application == "tuning") {
            
            params <- c(params, list(early_stopping_rounds = 40))

            
          }
          
    
    
    
  }
  
  ### Add a parameter of which "quantile" to focus on for the quantile regression
  
  if(objective == "quantile") {
    
    params <- c(params, alpha = alpha)
    
  }
  

  # *************************************
  

  
  # *************************************
  
  #Five, train the model
  
  set.seed(913)
  
  cat(crayon::cyan("\nTraining Model\n"))
  
  ## Set validation dataset for early stopping ONLY IF TUNING
  
  if(application == "tuning") {
    val = list("valid" = test_lgbm)
  } else if(application == "testing") {val = list()}
  
  model_lgbm <- lgb.train(params,
                          data = train_lgbm,
                          valids = val,
                          eval = round_mae,
                          verbose = 1L)
  
  # *************************************
  
  # Five, make predictions on test dataset
  
  preds_and_imp <- list()
  
  ### Predict on test dataset
  pred_var_name <- paste0("predicted_", tgt_var)
  
  predicted <- predict(model_lgbm,
                       newdata = test_preds,
                       type = "response"
                       ) %>%
                       as_tibble() %>%
    rename(!!pred_var_name := 1) 
  
  
  #### Bind predictions to test values
  obs_var_name <- paste0("observed_", tgt_var)
  
  predicted_observed <- bind_cols(test_df %>%
                                    dplyr::rename(!!obs_var_name := !!tgt_var),
                                  predicted) 
  

  
  preds_and_imp[[1]] <- predicted_observed
  
  
  # *************************************
  
  # Six, get variable importance
  
  ## Get variable importance 
  ## But only if we're testing
  ## We don't need to do if we're just tuning 
  
  if(application == "testing"){
    
    cat(crayon::cyan("\nCalculating Var Importance\n"))
    
    
    var_imp <- lgb.importance(model_lgbm)
    
    #### Save to list
    
    preds_and_imp[[2]] <- var_imp
    
    
  }
  
  # *************************************
  
  # Seven, get SHAPs if so desired
  
  if(explain == TRUE) {
    
    
    cat(crayon::cyan("\nDo SHAPS\n"))
    
    ### SHAP variable importance
    
    cat(crayon::yellow("\nCalculating SHAPs\n"))
    
    #### And then get and return some files key for visualization
    
    ##### SHAP vals
    
    shv <- shapviz::shapviz(model_lgbm, 
                            X_pred = test_preds)
    
    
    ##### And extract baselines & others for saving
    ##### And save them 
    
    shv_b <- tibble(shap_baseline = get_baseline(shv))
    
    preds_and_imp[[3]] <- shv_b
    
    shv_shap <- get_shap_values(shv) %>% as_tibble()
    
    preds_and_imp[[4]] <- shv_shap
    
    shv_feats <- get_feature_values(shv) %>% as_tibble()
    
    preds_and_imp[[5]] <- shv_feats
    
    ##### And plot & save plots
    
    ###### Plot
    
    if(plot_shaps == TRUE) {
      
        cat(crayon::yellow("\nPlotting SHAPs\n"))
        
        shap_bar <- sv_importance(shv, 
                                  kind = "bar",
                                  show_numbers = TRUE) + 
          theme_bw() +
          geom_col(fill = "tomato3", color = "tomato4") +
          labs(title = as.character(test_df$fire_name[1])) + 
          theme(
            legend.position = "bottom",
            plot.background = element_blank(),
            legend.background = element_blank(), #transparent legend bg
            legend.box.background = element_blank(),
            legend.key = element_blank(),
            axis.text = element_text(size = 20),
            axis.title = element_text(size = 18),
            legend.text = element_text(size = 18),
            legend.title = element_text(size = 22),
          ) 
        
        # ******** Save
        
        ggsave(here("plots", 
                    "shap-plots",
                    paste0("shap_bar_",
                           test_df$fire_id[1],
                           ".png")),
               shap_bar,
               width = 6.5,
               height = 6.5, 
               dpi = 300)
        
        ###### Plot Beeswarm
        
        shap_bee <- sv_importance(shv, 
                                  kind = "beeswarm",
                                  show_numbers = TRUE,
                                  max_display = 8,
                                  number_size = 6) +
          theme_bw() +
          cols4all::scale_colour_continuous_c4a_seq(palette = "-kovesi.bk_rd_yl") + 
          guides(color = guide_colorbar(direction = "horizontal",
                                        barwidth = 12,
                                        barheight = 0.7,
                                        title.position = "left",
                                        title.theme = element_text(margin = margin(r = 24)))) + 
          labs(title = as.character(test_df$fire_name[1])) + 
          theme(
            legend.position = "bottom",
            plot.background = element_blank(),
            legend.background = element_blank(), #transparent legend bg
            legend.box.background = element_blank(),
            legend.key = element_blank(),
            axis.text = element_text(size = 20),
            axis.title = element_text(size = 18),
            legend.text = element_text(size = 18),
            legend.title = element_text(size = 22),
          ) 
        
        ###### Save Beeswarm
        
        ggsave(here("plots", 
                    "shap-plots",
                    paste0("shap_beeswarm_",
                           test_df$fire_id[1],
                           ".png")),
               shap_bee,
               width = 6.5,
               height = 6.5, 
               dpi = 300)
        
      
      
      
    }
    

    #### Get shaps using LightGBMs native built in function
    

    # shv_nat <- predict(model_lgbm,
    #                    newdata = test_preds,
    #                    type = "contrib")
    # 
    
    
  }
  
  # *************************************
  
  # Finally, return
  
  return(preds_and_imp)
  
  
  # *************************************
  
  


  
}


# ******************************************************************************
# ******************************************************************************

# Our little custom evaluation function


round_mae <- function(preds, dtrain) {
  
  labels <- lightgbm::get_field(dtrain, "label")
  
  r_mae <- round(mean(abs(preds - labels)),0)
  
  return(list(
    name = "rounded_mae",
    value = r_mae,
    higher_better = FALSE
  ))
}



# ******************************************************************************
# ******************************************************************************

#evaluate_preds(preds_df = predicted_observed)


evaluate_preds <- function(preds_df = NULL,
                           tgt_var = "dnbr") {
  
  # *************************************
  
  #### Print to file what we are doing
  cat(crayon::cyan("\nCalculating performance metrics & things of that nature\n"))
  
  #### A list to hold stuff
  preds_and_perf_metrics <- list()
  
  #### And some variable names to make things easier
  
  pred_var_name <- paste0("predicted_", tgt_var)
  
  obs_var_name <- paste0("observed_", tgt_var)
  
  # *************************************
  
  #### 1, add in some simple error metrics
  
  preds_exp <- preds_df %>%
    mutate(raw_err = .data[[pred_var_name]] - .data[[obs_var_name]]) %>%
    mutate(sqrerr = raw_err^2,
           abs_error = abs(raw_err),
           abs_pct_error = abs_error/.data[[obs_var_name]]*100)
  
  #### Add to list
  
  preds_and_perf_metrics[[1]] <- preds_exp
  
  ### Then calculate a mean baseline prediction
  
  baseline <- mean(preds_exp[[pred_var_name]])
  
  # *************************************
  
  # 2, Calculate some summary error metrics
  
  ### Calculate 'em
  
  error_summary_by_fire <- preds_exp %>%
    dplyr::group_by(fire_id) %>%
    summarise(mae = mean(abs_error),
              mape = mean(abs_pct_error),
              rmse = sqrt(mean(sqrerr)),
              pbias = hydroGOF::pbias(.data[[pred_var_name]],
                                      .data[[obs_var_name]]),
              nse = hydroGOF::NSE(.data[[pred_var_name]],
                                  .data[[obs_var_name]])) %>%
    rename(test_fire_id = fire_id) %>%
    dplyr::ungroup() %>%
    mutate(!!paste0("mean_", pred_var_name) := baseline)
  
  error_summary_overall <- preds_exp %>%
    dplyr::ungroup() %>%
    summarise(mae = mean(abs_error),
              mape = mean(abs_pct_error),
              rmse = sqrt(mean(sqrerr)),
              pbias = hydroGOF::pbias(.data[[pred_var_name]],
                                      .data[[obs_var_name]]),
              nse = hydroGOF::NSE(.data[[pred_var_name]],
                                  .data[[obs_var_name]])) %>%
    dplyr::ungroup() %>%
    mutate(!!paste0("mean_", pred_var_name) := baseline)
  

  
  ### Add to list
  
  preds_and_perf_metrics[[2]] <- error_summary_by_fire
  
  preds_and_perf_metrics[[3]] <- error_summary_overall
  
  # *************************************
  
  ### And return it all
  return(preds_and_perf_metrics)
  
  # *************************************
  
  
}



# ******************************************************************************
# ******************************************************************************

## Tuning ----

# Now a function to tune our hyperparameters 

#model_df <- train_valid

split_data <- function(model_df = NULL) {
  
  #### Make splits for tuning 
  #### By splitting into 5-fold cross validation
  #### Where each testing dataset has twenty fires 
  ### and each training has eighty fires
  
  
  splits <- rsample::group_vfold_cv(model_df,
                                    group = usu_fire_index, 
                                    balance = "groups",
                                    v = 5)

  return(splits)

}


### And outward (helpful for calculations of slope, etc.)

# fire_outward_buff <- st_buffer(bndry_file, dist = 100)
# 
# fire_outward_big_buff <- st_buffer(bndry_file, dist = 1000)
# 
# buffers <- list()
# 
# buffers[1] <- fire_buffer
# 
# buffers[2] <- fire_outward_buff
# 
# buffers[3] <- fire_outward_big_buff


