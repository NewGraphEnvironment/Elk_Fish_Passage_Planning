require(RCurl)
library(XML)


##increase your timeout limit to allow download of bigger files
options(timeout=180)


##now get the new maps
# setwd("C:/Users/allan/OneDrive/New_Graph/Current/2019-023_Bulkley_fish_passage/field_maps/assessment/planning_maps")
url = "https://hillcrestgeo.ca/outgoing/fishpassage/projects/elk/"

filenames <- getURL(url,verbose=TRUE,ftp.use.epsv=TRUE, dirlistonly = TRUE)
filenames <- getHTMLLinks(filenames, xpQuery = "//a/@href[contains(., '.pdf')]") #https://stackoverflow.com/questions/32213591/list-files-on-http-ftp-server-in-r  

##this could prb just be an lapply but not going to look at it now. just recycle from last project and go to bed.
for (filename in filenames) {
  download.file(paste(url, filename, sep = ""), paste(getwd(), "/field_maps/", filename,
                                                      sep = ""), mode = "wb")
}

##now get the new maps morixw
setwd("C:/Users/allan/OneDrive/New_Graph/Current/2019-023_Bulkley_fish_passage/field_maps/assessment/planning_maps")
url = "https://hillcrestgeo.ca/outgoing/fishpassage/projects/morice/mapping/"

filenames <- getURL(url,verbose=TRUE,ftp.use.epsv=TRUE, dirlistonly = TRUE)
filenames <- getHTMLLinks(filenames, xpQuery = "//a/@href[contains(., '.pdf')]") #https://stackoverflow.com/questions/32213591/list-files-on-http-ftp-server-in-r  

##this could prb just be an lapply but not going to look at it now. just recycle from last project and go to bed.
for (filename in filenames) {
  download.file(paste(url, filename, sep = ""), paste(getwd(), "/", filename,
                                                      sep = ""), mode = "wb")
}

