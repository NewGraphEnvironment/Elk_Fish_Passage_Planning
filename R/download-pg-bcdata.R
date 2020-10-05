##update 20191028
##author Al Irvine
##tweaks download only specific chunks of PSCIS data and visualize
##add the "last updated" info to the data so we can query whether to bother updating at a later date


# remotes::install_github("bcgov/bcdata")
# remotes::install_github("bcgov/bcmaps")
# library(bcmaps)
{
  library(bcdata)
  library(sf)
  library(RPostgreSQL)
  library(tidyverse)  
  options(timeout=180)##increase your timeout limit to allow download of bigger files
}

bcdc_get_record("7ecfafa6-5e18-48cd-8d9b-eae5b5ea2881")
bcdc_tidy_resources("7ecfafa6-5e18-48cd-8d9b-eae5b5ea2881")


##here we type our search term in to look for layers. Could use bcdc_browse() 
bcdc_search("pscis-assessments", type = "Geographic", n = 83)
bcdc_get_record("7ecfafa6-5e18-48cd-8d9b-eae5b5ea2881")

#ecosections-ecoregion-ecosystem-classification-of-british-columbia : ccc01f43-860d-4583-8ba4-e72d8379441e
#utm-zones-of-british-columbia
#water-rights-licences-public :5549cae0-c2b1-4b96-9777-529d9720803c
#pscis-assessments : 7ecfafa6-5e18-48cd-8d9b-eae5b5ea2881

##should start a lookup table for these layers
get_this <- bcdc_tidy_resources("pscis-assessments") %>% 
  filter(bcdata_available == T)  %>% 
  pull(package_id)
  
##name the layer you want to download
get_this <- "7ecfafa6-5e18-48cd-8d9b-eae5b5ea2881"


bcdatapg <- function(get_this, crs = 3005)
{
    # drv <- dbDriver("PostgreSQL")
    conn <- dbConnect(dbDriver("PostgreSQL"), 
                      dbname = "postgis",
                      host = "localhost", 
                      port = "5432",
                      user = "postgres", 
                      password = "postgres")
  date_stamp <- bcdc_get_record(get_this)[["metadata_modified"]]
  # check_date_stamp <- dbGetQuery(conn, paste0("select d"))  ##in progress - we need to see if there is a "last_updated" column 
  ## and if there is we compare to the date_stamp. If they are the same don't download - throw out the info or just move on.
  dl <- bcdc_get_data(get_this)
  layer_name <- dl$id[1]
  schema_name <- tolower(word(layer_name,1,sep = "\\."))
  table_name <- tolower(word(layer_name,2,sep = "\\."))
  names(dl) <- tolower(names(dl))
  dl <- dl %>% 
    rename(geom = geometry) %>% 
    mutate(last_updated = date_stamp)
  classes <-  c("sf", "tbl_df", "tbl", "data.frame")
  class(dl) <- classes
  dbExecute(conn, paste0("CREATE SCHEMA IF NOT EXISTS ", schema_name,";"))
  dbWriteTable(conn, c(schema_name, table_name), value = dl, overwrite = TRUE)
  dbExecute(conn, paste0("ALTER TABLE ", schema_name, ".", table_name, " ALTER COLUMN geom TYPE geometry;"))
  dbExecute(conn, paste0("ALTER TABLE ", schema_name, ".", table_name, " ADD PRIMARY KEY (",'"objectid"',");")) ##assign primary key
  #create a spatial index 
  dbExecute(conn, paste0("CREATE INDEX ON ", schema_name, ".", table_name, " USING GIST (geom)"))
  dbExecute(conn,
                    paste0("ALTER TABLE ", schema_name, ".", table_name, " ALTER COLUMN geom
           Type geometry(Point, ", crs, ")
           USING ST_SetSRID(geom, ", crs, ");"))
  dbDisconnect(conn)
  # rm(conn, drv, dl, classes, dsn_database, dsn_hostname, dsn_port, dsn_pwd,dsn_uid,
  #    layer_name) 
} 


bcdatapg(get_this = get_this)


##if you want to set the crs I think you can do this. Might be glitchy though.
dbGetQuery(conn, 
           "ALTER TABLE whse_fish.pscis_assessment_svw
           ALTER COLUMN geom
           Type geometry(Point, 3005)
           USING ST_SetSRID(geom, 3005);")

##check to see that you have a unique identifier
# names(dl)
# n_distinct(dl$objectid) == nrow(dl)
# n_distinct(dl$objectid)
# nrow(dl)

# ##look at your duplicates
# duplicates <- dl %>%
#   group_by(objectid) %>%
#   filter(n() > 1) 
# 
# ##remove duplicates in a timely fashion
# dl <- dl %>%
#   group_by(objectid) %>%
#   slice(1) 
# 
# ##remove duplicated rows (way slow)
# dl <- dl %>%
#   distinct(id, .keep_all = TRUE)

##add a column for the FID to use as the primary key if you don't have a unique ID (i.e. objectid)
# dl <- mutate(dl, fid = as.integer(rownames(dl))) %>%
#   select(fid, everything())
# names(dl)




##_________looking for layers and getting info about them____________________________________
library(jsonlite)

search_term <- "env-regional-boundaries"

bcdc_api_url = "https://catalogue.data.gov.bc.ca/api/3/action/"
wfs_url = "https://openmaps.gov.bc.ca/geo/pub/wfs"

##build the search url for the api
search_url <- paste0(bcdc_api_url, "package_search?q=", search_term)

##this is requesting the info about the files 
search_return <- fromJSON(readLines(search_url), flatten = TRUE)


##let's isolate the layer we are looking for 
search_return <- search_return$result 
search_return <- search_return$results
#matches$layer_name
names(search_return)
search_return$name
target_layer_info <- filter(search_return, name == get_this) ##this refers to the search term at the top - might fuck you up a bit....
target_layer_info$object_name
#layer$id

##see when the data was modified last
target_layer_info$record_last_modified

##here we can preview on imap
bcdc_get_record("pscis-habitat-confirmations") [["preview_map_service_url"]] ##get resource id
test <- bcdc_query_geodata("1915c8e2-3c8f-494f-bfc8-ecc013d494c5") %>%  collect()