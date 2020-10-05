##extract the data from fish sampling on Leach. WCT point shows up but the 

library(tidyverse)
library(readxl)
library(sf)
require(RCurl)
library(XML)
library(openxlsx)


##needed to convert from xls to xlsx with excel 
path = "./data/leach_CB18_357885_1562099801969_2098704592.xls"


# path <- "./data/bulkley_Attachment2_20190424.xlsx"

search_list <-  path %>% 
  readxl::excel_sheets() 

##function to trim up sheet and get names (was previously source from altools package)
at_trim_xlsheet2 <- function(df, column_last = ncol(df)) {
  df %>%
    dplyr::select(1:column_last) %>% ##get rid of the extra columns.  should be more abstract
    janitor::row_to_names(which.max(complete.cases(.))) %>%
    janitor::clean_names() %>%
    janitor::remove_empty(., which = "rows")
}


fish_data_submission = path %>% 
  readxl::excel_sheets() %>% 
  purrr::set_names() %>% 
  purrr::map(read_excel, 
             path = path, 
             .name_repair = janitor::make_clean_names) %>% 
  purrr::set_names(janitor::make_clean_names(names(.))) %>% 
  purrr::map(at_trim_xlsheet2) %>% #moved to functions from https://github.com/NewGraphEnvironment/altools to reduce dependencies
  purrr::map(plyr::colwise(type.convert))

# fish_data_submission <- path %>% 
#   readxl::excel_sheets() %>% 
#   purrr::set_names() %>% 
#   purrr::map(read_excel, 
#              path = path, 
#              .name_repair = janitor::make_clean_names) %>% 
#   purrr::set_names(janitor::make_clean_names(names(.))) %>% 
#   purrr::map(at_trim_xlsheet2)


site_location_data <- fish_data_submission %>% 
  purrr::pluck("step_1_ref_and_loc_info") %>% 
  dplyr::filter(!is.na(site_number)) %>% 
  mutate(survey_date = janitor::excel_numeric_to_date(as.numeric(survey_date)))

##we need to join the site data with the fish info

fish_coll_data <- fish_data_submission %>% 
  purrr::pluck("step_2_fish_coll_data")

fish_sampling_data <- left_join(site_location_data,
                                select(fish_coll_data, reference_number,species,total_number, comments), #species from "species_code"
                                by = c('reference_number')) %>% 
  st_as_sf(coords = c('utm_easting','utm_northing'), remove = F, crs = 26910)
  
##burn to th geopackage for now
sf::st_write(fish_sampling_data, "gis/elk_planning_2020.gpkg", "leach_2018", delete_layer = T)

#########################################################################################################

##lets extract morrisey data
#http://a100.gov.bc.ca/pub/acat/public/viewReport.do?reportId=49729

url = "http://a100.gov.bc.ca/appsdata/acat/documents/r49729/CB13-90128_FDIS_data_1448998987478_8992517770.xls"

path <- paste0(getwd(), "/data/", basename(url))

download.file(url = url, destfile = path, mode = "wb")

# path_new <- gsub('xls', 'xlsx', path)


##seems like we need to convert to a xlsx -  we need to do this manually or it doesn't work!!!!
# file.rename(path, path_new)

##now start using the new path name
# path <-  path_new

# search_list <-  path %>% 
#   readxl::excel_sheets() 

##function to trim up sheet and get names (was previously source from altools package)
at_trim_xlsheet2 <- function(df, column_last = ncol(df)) {
  df %>%
    dplyr::select(1:column_last) %>% ##get rid of the extra columns.  should be more abstract
    janitor::row_to_names(which.max(complete.cases(.))) %>%
    janitor::clean_names() %>%
    janitor::remove_empty(., which = "rows")
}


fish_data_submission = path %>% 
  readxl::excel_sheets() %>% 
  purrr::set_names() %>% 
  purrr::map(read_excel, 
             path = path, 
             .name_repair = janitor::make_clean_names) %>% 
  purrr::set_names(janitor::make_clean_names(names(.))) %>% 
  purrr::map(at_trim_xlsheet2) %>% #moved to functions from https://github.com/NewGraphEnvironment/altools to reduce dependencies
  purrr::map(plyr::colwise(type.convert))

# fish_data_submission <- path %>% 
#   readxl::excel_sheets() %>% 
#   purrr::set_names() %>% 
#   purrr::map(read_excel, 
#              path = path, 
#              .name_repair = janitor::make_clean_names) %>% 
#   purrr::set_names(janitor::make_clean_names(names(.))) %>% 
#   purrr::map(at_trim_xlsheet2)


site_location_data <- fish_data_submission %>% 
  purrr::pluck("step_1_ref_and_loc_info") %>% 
  dplyr::filter(!is.na(site_number)) %>% 
  mutate(survey_date = janitor::excel_numeric_to_date(as.numeric(survey_date)))

##we need to join the site data with the fish info

fish_coll_data <- fish_data_submission %>% 
  purrr::pluck("step_2_fish_coll_data")

fish_sampling_data <- left_join(site_location_data,
                                select(fish_coll_data, reference_number,species,total_number, comments), #species from "species_code"
                                by = c('reference_number')) %>% 
  st_as_sf(coords = c('utm_easting','utm_northing'), remove = F, crs = 26910)

##burn to th geopackage for now
sf::st_write(fish_sampling_data, "gis/elk_planning_2020.gpkg", "lotic_2013", delete_layer = T)

#############################################################################################
##masse 2016 http://a100.gov.bc.ca/pub/acat/public/viewReport.do?reportId=52717

url <- "http://a100.gov.bc.ca/appsdata/acat/documents/r52717/CB16_235734_1505700005793_5694834224.xlsx"

path <- paste0(getwd(), "/data/", basename(url))

download.file(url = url, destfile = path, mode = "wb")

fish_data_submission = path %>% 
  readxl::excel_sheets() %>% 
  purrr::set_names() %>% 
  purrr::map(read_excel, 
             path = path, 
             .name_repair = janitor::make_clean_names) %>% 
  purrr::set_names(janitor::make_clean_names(names(.))) %>% 
  purrr::map(at_trim_xlsheet2) %>% #moved to functions from https://github.com/NewGraphEnvironment/altools to reduce dependencies
  purrr::map(plyr::colwise(type.convert))

site_location_data <- fish_data_submission %>% 
  purrr::pluck("step_1_ref_and_loc_info") %>% 
  dplyr::filter(!is.na(site_number)) %>% 
  mutate(survey_date = janitor::excel_numeric_to_date(as.numeric(survey_date)))

##we need to join the site data with the fish info

fish_coll_data <- fish_data_submission %>% 
  purrr::pluck("step_2_fish_coll_data")

fish_sampling_data <- left_join(site_location_data,
                                select(fish_coll_data, reference_number,species,total_number, comments), #species from "species_code"
                                by = c('reference_number')) %>% 
  st_as_sf(coords = c('utm_easting','utm_northing'), remove = F, crs = 26910)

##burn to th geopackage for now
sf::st_write(fish_sampling_data, "gis/elk_planning_2020.gpkg", "masse_2016", delete_layer = T)

######interior 2010 for morrisey
path <- paste0(getwd(), "/data/interior_2010_Fish_Site_Mtd.xlsx")

fish_data_submission = path %>% 
  readxl::excel_sheets() %>% 
  purrr::set_names() %>% 
  purrr::map(read_excel, 
             path = path, 
             .name_repair = janitor::make_clean_names) %>% 
  purrr::set_names(janitor::make_clean_names(names(.))) %>% 
  pluck(1)


##turn into spatial object
fish_data_submission <- fish_data_submission %>% 
  st_as_sf(coords = c('utm_easting','utm_northing'), remove = F, crs = 26910)

##burn to th geopackage for now
sf::st_write(fish_data_submission, "gis/elk_planning_2020.gpkg", "interior_2010", delete_layer = T)
