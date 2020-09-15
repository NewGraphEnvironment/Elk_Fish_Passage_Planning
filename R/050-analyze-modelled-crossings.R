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

##list tables in a schema  
dbGetQuery(conn,
           "SELECT table_name 
           FROM information_schema.tables 
           WHERE table_schema='whse_fish'")


  
query <- "SELECT * FROM fish_passage.pscis_model_combined WHERE watershed_group_code IN ('ELKR')"


df <-  sf::st_read(conn, query = query) %>% 
  filter(is.na(pscis_stream_crossing_id) &
         fish_habitat != "FISH HABITAT - INFERRED - 220-300PCT" &
           fish_habitat != "FISH HABITAT - INFERRED - 150-220PCT" &
           uphab_gross_sub15 > 1000) %>% 
  select(-fid)
  
##burn to th geopackage for now
sf::st_write(df, "gis/elk_planning_2020.gpkg", "xings_1km", delete_layer = T)

# ##lets do one greater than 5km and summarize by mapsheet
# df <-  sf::st_read(conn, query = query) %>% 
#   filter(is.na(pscis_stream_crossing_id) &
#            fish_habitat != "FISH HABITAT - INFERRED - 220-300PCT" &
#            fish_habitat != "FISH HABITAT - INFERRED - 150-220PCT" &
#            uphab_gross_sub15 > 5000) %>% 
#   select(-fid) %>% 
#   group_by(dbm_mof_50k_grid_map_tile) %>% 
#   summarise(n = n())


##looks like 114 wins but it just mapped weird and does not have alot going on. ex. Andy Good
##109 is second up but the combined output misses a bunch of bridges

##lets try to match the points to PSCIS
# add a unique id
df$misc_point_id <- seq.int(nrow(df))


# load to database
st_write(obj = df, dsn = conn, Id(schema= "working", table = "misc_points"))

# sf doesn't automagically create a spatial index or a primary key
res <- dbSendQuery(conn, "CREATE INDEX ON working.misc_points USING GIST (geom)")
dbClearResult(res)
res <- dbSendQuery(conn, "ALTER TABLE working.misc_points ADD PRIMARY KEY (misc_point_id)")
dbClearResult(res)

pscis_info <- dbGetQuery(conn, "SELECT
  a.misc_point_id,
  b.stream_crossing_id,
  ST_Distance(ST_Transform(a.geom,3005), b.geom) AS distance
FROM
  working.misc_points AS a
CROSS JOIN LATERAL
  (SELECT *
   FROM whse_fish.pscis_assessment_svw
   ORDER BY
     a.geom <-> geom
   LIMIT 1) AS b")

##join pscis id back to the dataframe
df_joined <- left_join(df,
                           pscis_info,
                           by = c('misc_point_id')) %>% 
  mutate(matched = case_when(distance < 50 ~ 'yes',
                             T ~ 'no')) 

##burn to th geopackage for now
sf::st_write(df_joined, "gis/elk_planning_2020.gpkg", "xings_1km", delete_layer = T)

##have a look at which mapsheets now stand out with xings with more than 3km hab upstream <15%
view <- df_joined %>% 
  filter(matched == 'no' &
           uphab_gross_sub15 > 3000) %>% 
  group_by(dbm_mof_50k_grid_map_tile) %>% 
  summarise(n = n())


##lets join with the pscis data and screen for high or mod priority crossings

