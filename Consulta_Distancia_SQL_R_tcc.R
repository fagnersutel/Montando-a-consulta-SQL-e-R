setwd("~/OneDrive/r-files/AffinityPropagationClustering/")
filenames <- list.files(path = "~/OneDrive/r-files/AffinityPropagationClustering/geo/") 
filenames
setwd("~/OneDrive/r-files/AffinityPropagationClustering/geo/") 
data <- do.call("rbind", lapply(filenames, read.csv, header = TRUE, sep = ";")) 
setwd("~/OneDrive/r-files/AffinityPropagationClustering/")
head(data)
names(data)
library(geosphere)
library(sqldf)
coord_sub_set = sqldf("SELECT *, (6371 * acos(cos( radians(-30.053831)) * cos(radians(lat)) * cos(radians(long) - radians(-51.191810)) + sin(radians(-30.053831)) * sin(radians(lat)))) AS distancia FROM data GROUP BY lat HAVING distancia < 0.2 ORDER BY distancia ASC ")
dim(data)
dim(coord_sub_set)
coord_sub_set
distm(c(-30.053831, -51.191810), c(-30.05412, -51.19247), fun = distHaversine)
View(coord_sub_set)


library(leaflet.extras)
pal <- colorFactor(
  palette = 'Dark2'
)

data = data[complete.cases(data), ]

leaflet(data) %>%
  addTiles(group="OSM") %>% 
  addCircles(~long, ~lat, weight = 0.1, radius=30, color="red",
             stroke = TRUE, fillOpacity = 0.8) %>% 
  addLegend("topright", colors= "blue", labels=paste("alvaras", sep = " "), title="Cluster")
