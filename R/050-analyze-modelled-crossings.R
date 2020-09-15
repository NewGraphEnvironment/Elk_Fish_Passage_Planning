##here we would like to see crossings that have sig habitat taht have not been assessed yet.

library(DBI)
library(tidyverse)
library(sf)
library(RPostgres)
library(data.table)

# db connection
conn <- dbConnect(
  RPostgres::Postgres(),
  dbname = 'postgis',
  host = 'localhost',
  port = 5432,
  user = 'postgres',
  password = 'postgres'
)

##list column names in a table
dbGetQuery(conn,
           "SELECT column_name,data_type 
           FROM information_schema.columns 
           WHERE table_name='pscis_model_combined'")

df <- dbGetQuery(conn, "SELECT
  a.* 
  FROM fish_passage.pscis_model_combined
  WHERE ")
  
  
query <- "SELECT * FROM fish_passage.pscis_model_combined WHERE watershed_group_code IN ('ELKR')"


df <-  sf::st_read(conn, query = query) %>% 
  filter(is.na(pscis_stream_crossing_id) &
         fish_habitat != "FISH HABITAT - INFERRED - 220-300PCT" &
           fish_habitat != "FISH HABITAT - INFERRED - 150-220PCT" &
           uphab_gross_sub15 > 1000) %>% 
  select(-fid)
  
##burn to th geopackage for now
sf::st_write(df, "gis/elk_planning_2020.gpkg", "xings_1km", delete_layer = T)

##lets do one greater than 5km and summarize by mapsheet
df <-  sf::st_read(conn, query = query) %>% 
  filter(is.na(pscis_stream_crossing_id) &
           fish_habitat != "FISH HABITAT - INFERRED - 220-300PCT" &
           fish_habitat != "FISH HABITAT - INFERRED - 150-220PCT" &
           uphab_gross_sub15 > 5000) %>% 
  select(-fid) %>% 
  group_by(dbm_mof_50k_grid_map_tile) %>% 
  summarise(n = n())


##looks like 114 wins but it just mapped weird and does not have alot going on. ex. Andy Good
##109 is second up but the combined output misses a bunch of bridges

##lets try to match the points to 
