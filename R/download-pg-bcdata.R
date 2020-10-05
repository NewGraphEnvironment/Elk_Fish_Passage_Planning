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

# bcdc_get_record("7ecfafa6-5e18-48cd-8d9b-eae5b5ea2881")
# bcdc_tidy_resources("7ecfafa6-5e18-48cd-8d9b-eae5b5ea2881")
# 
# 
# ##here we type our search term in to look for layers. Could use bcdc_browse() 
bcdc_search("pscis", type = "Geographic", n = 83)
bcdc_get_record("572595ab-0a25-452a-a857-1b6bb9c30495")
# 
# #ecosections-ecoregion-ecosystem-classification-of-british-columbia : ccc01f43-860d-4583-8ba4-e72d8379441e
# #utm-zones-of-british-columbia
# #water-rights-licences-public :5549cae0-c2b1-4b96-9777-529d9720803c
# #pscis-assessments : 7ecfafa6-5e18-48cd-8d9b-eae5b5ea2881
# 
# ##should start a lookup table for these layers
# get_this <- bcdc_tidy_resources("pscis-assessments") %>% 
#   filter(bcdata_available == T)  %>% 
#   pull(package_id)
  
##name the layer you want to download
get_this <- "1596afbf-f427-4f26-9bca-d78bceddf485"

get <- get_this

##define what happens when we go to download
bcdata_download <- function(get = get_this, crs = 3005){
  conn <- dbConnect(dbDriver("PostgreSQL"), 
                    dbname = "postgis",
                    host = "localhost", 
                    port = "5432",
                    user = "postgres", 
                    password = "postgres") 
  dl <- bcdc_get_data(get)
  date_stamp <- bcdc_get_record(get)[["metadata_modified"]]
  object_name <- bcdc_get_record(get)[["object_name"]]
  schema_name <- tolower(word(object_name,1,sep = "\\."))
  table_name <- tolower(word(object_name,2,sep = "\\."))
  names(dl) <- tolower(names(dl))
  dl <- dl %>% 
    rename(geom = geometry) %>% 
    mutate(last_updated = date_stamp)
  classes <-  c("sf", "tbl_df", "tbl", "data.frame")
  class(dl) <- classes
  res <- dbSendQuery(conn, paste0("CREATE SCHEMA IF NOT EXISTS ", schema_name,";"))
  dbClearResult(res)
  dbWriteTable(conn, c(schema_name, table_name), value = dl, overwrite = TRUE)
  res <- dbSendQuery(conn, paste0("ALTER TABLE ", schema_name, ".", table_name, " ALTER COLUMN geom TYPE geometry;"))
  dbClearResult(res)
  res <- dbSendQuery(conn, paste0("ALTER TABLE ", schema_name, ".", table_name, " ADD PRIMARY KEY (",'"objectid"',");")) ##assign primary key
  dbClearResult(res)
  #create a spatial index 
  res <- dbSendQuery(conn, paste0("CREATE INDEX ON ", schema_name, ".", table_name, " USING GIST (geom)"))
  dbClearResult(res)
  res <- dbSendQuery(conn,
            paste0("ALTER TABLE ", schema_name, ".", table_name, " ALTER COLUMN geom
           Type geometry(Point, ", crs, ")
           USING ST_SetSRID(geom, ", crs, ");"))
  dbClearResult(res)
  dbDisconnect(conn)
}

##define a function that tests for date stamp before download
bcdatapg <- function(get = get_this)
{
  # drv <- dbDriver("PostgreSQL")
  conn <- dbConnect(dbDriver("PostgreSQL"), 
                    dbname = "postgis",
                    host = "localhost", 
                    port = "5432",
                    user = "postgres", 
                    password = "postgres")
  date_stamp <- bcdc_get_record(get)[["metadata_modified"]]
  object_name <- bcdc_get_record(get)[["object_name"]]
  schema_name <- tolower(word(object_name,1,sep = "\\."))
  table_name <- tolower(word(object_name,2,sep = "\\."))
  # check_date_stamp <- dbGetQuery(conn, paste0("select d"))  ##in progress - we need to see if there is a "last_updated" column 
  ## and if there is we compare to the date_stamp. If they are the same don't download - throw out the info or just move on.
  pg_table_colunms <- dbGetQuery(conn, paste0("SELECT column_name 
                                              FROM information_schema.columns 
                                              WHERE table_schema = '", schema_name,
                                              "' AND table_name = '", table_name, "';")) %>% 
    dplyr::pull()
  
  if('last_updated' %in% pg_table_colunms) {
    if(dbGetQuery(conn, paste0("SELECT DISTINCT x.last_updated FROM ", schema_name,
                               ".", table_name, " x;")) != date_stamp){
      dbDisconnect(conn)
      bcdata_download()
    } else {
      print('this table is up to date already')
      }
    } else {
      bcdata_download()
  }
# rm(conn, drv, dl, classes, dsn_database, dsn_hostname, dsn_port, dsn_pwd,dsn_uid,
#    layer_name) 
} 

bcdatapg()


