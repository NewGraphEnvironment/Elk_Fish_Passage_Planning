##this script will download planning report tables from report I drafted in 2016 and make spatial files ready for the field

library(tidyverse)
library(sf)
library(RCurl)
library(XML)
library(data.table)
# library(readxl)
# library(googledrive)
library(tabulizer)
library(janitor)


##grab the pdf report from ecocat and put in a file so we access the tables

url <- 'http://a100.gov.bc.ca/appsdata/acat/documents/r50900/F-F16-24-FinalReport-Masse-FishPassage_1475094712285_5091517509.pdf'
filename <- basename(url)

##download the file
# download.file(url = url, destfile = paste(getwd(), "/data/", filename,
#                                           sep = ""), mode = "wb")

##cut out the table with the sites to look at
##this is the path to the data
path <- paste(getwd(), "/data/", filename, sep = "")

#you would run with this the first time
page <- 23
tab_trim <- tabulizer::locate_areas(path, page)

##since we have done this before though - numbers below are results
# top      left    bottom     right 
# 94.68161  50.20628 511.14350 740.42152
# tab_trim = list(c(94.68161,  50.20628, 511.14350, 740.42152))

##extract the tables useing the areas you defined
table <- tabulizer::extract_tables(path,
                                      pages = page,
                                      method = "lattice",
                                      area = tab_trim) %>% 
  set_names(page)

test_view <- table %>% 
  map(as_tibble)

##make a table and remove empty columns and rows
table2 <- table %>% 
  pluck(1) %>% 
  as_tibble() %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names() %>% 
  dplyr::mutate_all(na_if,"") %>% 
  janitor::remove_empty()

##lets get the rest of the table except the last page - use page 25 (19 in the pdf) as the template so that we get all the data
page <- 25


tab_trim <- tabulizer::locate_areas(path, page)

##since we have done this before though - numbers below are results
# top      left    bottom     right 
# 74.78475  49.52018 537.90135 741.10762 
# tab_trim = list(c(74.78475,  49.52018, 537.90135, 741.10762))

##this is all the pages we need
page <- 24:26

##extract the tables useing the areas you defined
table <- tabulizer::extract_tables(path,
                                   pages = page,
                                   method = "lattice",
                                   area = tab_trim) %>% 
  set_names(page)

##make a function to make tables and remove empty columns and rows
make_table <- function(table_input){ 
  table_input %>% 
  # pluck(1) %>% 
  as_tibble() %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names() %>% 
  dplyr::mutate_all(na_if,"") %>% 
  janitor::remove_empty()
}

tables <- table %>% 
  map_df(make_table)

##get the last page of the table
page <- 27

tab_trim <- tabulizer::locate_areas(path, page)

##extract the tables useing the areas you defined
table <- tabulizer::extract_tables(path,
                                   pages = page,
                                   method = "lattice",
                                   area = tab_trim) %>% 
  set_names(page)

##since we have done this before though - numbers below are results
# top      left    bottom     right 
# 74.09865  50.20628 225.04036 739.04933 
# tab_trim = list(c(74.09865,  50.20628, 225.04036, 739.04933))

##make a table and remove empty columns and rows
table3 <- table %>% 
  pluck(1) %>% 
  as_tibble() %>% 
  janitor::row_to_names(1) %>% 
  janitor::clean_names() %>% 
  dplyr::mutate_all(na_if,"") %>% 
  janitor::remove_empty()


##join all 3 tables together and fix the names
tables_all <- bind_rows(
  table2,
  tables,
  table3
) %>% 
  rename(watershed_group = x, hgi_km = x2hgi_km, road_tenure = x3road_tenure) %>% 
  dplyr::filter(watershed_group == 'ELKR') ##keep only the watershed in

##turn into a spatial object
table_sf <- sf::st_as_sf(tables_all, coords = c("easting", "northing"), 
                         crs = 26910, remove = F)

##burn to a geopackage for now
##make a gis file
sub_dir_exists <- "gis"                    # Name of already existing folder
dir.create(file.path(getwd(), sub_dir_exists)) 



sf::st_write(table_sf, "gis/elk_planning_2020.gpkg", "irvine_columbia_2016", delete_layer = T)

##as we go through the file in qgis and look at it with the habitat confirmations and in the context of the model we can remove crossings that we do not want to look at
##add a column for our filtering
tables_all_cleaned <- tables_all %>% 
  mutate(keep_2020 = dplyr::case_when(id_pscis %like% 'Michel24' |
                                        id_pscis %like% 'Michel27' |
                                        id_pscis %like% 'Michel51' |
                                        id_pscis %like% 'Morrisey06' ~ 'No',
                                      T ~ NA_character_),
         notes_2020 = dplyr::case_when(id_pscis %ilike% 'Michel24' ~ 'same as MICH016. Completed in Masse 2015',
                                       id_pscis %ilike% 'Morrisey19' ~ 'looks good Morrisey21 just up the road',
                                       id_pscis %ilike% 'Michel27' |
                                         id_pscis %like%'Michel51'
                                         ~ 'Roberts Ck non-fish in model due to steep gradients at mouth',
                                       id_pscis %ilike% 'Morrisey06' ~ 'same as MORR019',
                                       T ~ NA_character_))

##turn into a spatial object
table_sf <- sf::st_as_sf(tables_all_cleaned, coords = c("easting", "northing"), 
                         crs = 26910, remove = F)

##rewrite with the new info added 
sf::st_write(table_sf, "gis/elk_planning_2020.gpkg", "irvine_columbia_2016", delete_layer = T)

