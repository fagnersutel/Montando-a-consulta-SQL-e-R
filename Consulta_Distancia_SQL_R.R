#https://stackoverflow.com/questions/45775654/find-lat-lon-within-a-radius-from-a-known-centered-point-in-r
coord <- cbind("longitude" = c(-2.141177, -2.09096, -2.118894, -2.14009, -2.113988, -2.123892, -2.144685, -2.220046, -2.114343, -2.285314, -2.092354, 
                               -2.149571, -2.126605, -2.097045, -2.183441, -2.166915, -2.133863, -2.100909, -2.10677, -2.251495, -2.118894, -2.14009, 
                               -2.123201, -2.114343, -2.140327, -2.148826, -2.120553, -2.133902, -2.094246, -2.11317, -2.251495, -2.09096, -2.212955, 
                               -2.118894, -2.183501, -2.14009, -2.249217, -2.123201, -2.114343, -2.092354, -2.148826, -2.120553, -2.117338, -2.116486, 
                               -2.094981, -2.232998, -2.118894, -2.14009, -2.123201, -2.104092, -2.114343, -2.148826, -2.175179, -2.090713, -2.09096, 
                               -2.118894, -2.14009), 
               "latitude" = c(57.16278, 57.18079, 57.12292, 57.13763, 57.13855, 57.13585, 57.17207, 57.1915, 57.09301, 57.20138, 57.14279, 57.15334, 
                              57.15615, 57.10443, 57.15051, 57.15089, 57.15201, 57.18968, 57.1567, 57.19315, 57.12292, 57.13763, 57.12686, 57.09301, 
                              57.15676, 57.17355, 57.12507, 57.16279, 57.1718, 57.14125, 57.19315, 57.18079, 57.10941, 57.12292, 57.19596, 57.13763, 
                              57.10063, 57.12686, 57.09301, 57.14279, 57.17355, 57.12507, 57.15301, 57.14484, 57.13614, 57.14629, 57.12292, 57.13763, 
                              57.12686, 57.14485, 57.09301, 57.17355, 57.15079, 57.14755, 57.18079, 57.12292, 57.13763))

coord
dim(coord)
str(coord) 
  
coord_df <- data.frame(coord, within_5km = geosphere::distHaversine(coord, c(-2.106472, 57.14455)) / 1000 < 5)
table(coord_df$within_5km)
dim(coord_df)
# convert m to km, check < 5
coord_df
str(coord_df)
dim(coord_df)
table(coord_df$within_5km)

coord_df <- data.frame(coord, within_5km = geosphere::distHaversine(coord, c(-2.106472, 57.14455)) < 1000)
table(coord_df$within_5km)
coord_df

#https://stackoverflow.com/questions/32363998/function-to-calculate-geospatial-distance-between-two-points-lat-long-using-r
library(geosphere)
library(sqldf)
distm(c(-2.106472, 57.14455), c(-2.104092, 57.14485), fun = distHaversine)




coord = as.data.frame(coord)
class(coord)
coord
sqldf("SELECT * FROM coord")

sqldf("SELECT *, (6371 * acos(cos( radians(-30.053831)) * cos(radians(latitude)) * cos(radians(longitude) - radians(-51.191810)) + sin(radians(-30.053831)) * sin(radians(latitude)))) AS distancia FROM coord")



coord
dim(coord)
coord_sub_set = sqldf("SELECT *, (6371 * acos(cos( radians(-30.053831)) * cos(radians(latitude)) * cos(radians(longitude) - radians(-51.191810)) + sin(radians(-30.053831)) * sin(radians(latitude)))) AS distancia FROM coord GROUP BY latitude HAVING distancia < 10700 ORDER BY distancia ASC ")
dim(coord_sub_set)
coord_sub_set
distm(c(-2.106472, 57.14455), c(-2.249217, 57.10063), fun = distHaversine)




setwd("~/OneDrive/r-files/AffinityPropagationClustering/")
filenames <- list.files(path = "~/OneDrive/r-files/AffinityPropagationClustering/geo/") 
filenames
setwd("~/OneDrive/r-files/AffinityPropagationClustering/geo/") 
data <- do.call("rbind", lapply(filenames, read.csv, header = TRUE, sep = ";")) 
setwd("~/OneDrive/r-files/AffinityPropagationClustering/")
head(data)
names(data)
head(data)
data1 = data[, 1:2]
coord_sub_set = sqldf("SELECT *, (6371 * acos(cos( radians(-30.053831)) * cos(radians(lat)) * cos(radians(long) - radians(-51.191810)) + sin(radians(-30.053831)) * sin(radians(lat)))) AS distancia FROM data GROUP BY lat HAVING distancia < 0.2 ORDER BY distancia ASC ")
dim(data)
dim(coord_sub_set)
coord_sub_set
data1
coord_df <- data.frame(data1, within_5km = geosphere::distHaversine(data1, c(-30.053831, -51.191810)) < 0.500)
dim(coord_df)




inside.circle = function(center, radius, p)
{#Begin Function
  
  #Error Checks
  
  if ((length(center)) !=2) 
  {stop ("First argument must be a vector of length two.")}
  
  if ( radius<=0 )
  {stop ("Second argument is not a positive number. Radius must be positive.")}
  
  if ( (length(radius)) !=1 )
  {stop ("Second argument must contain only 1 element.") }
  
  #Create lengths for later use.
  p.1 = length(p[,1])
  x.coord = numeric(p.1)
  y.coord = numeric(p.1)
  
  #Below will calculate the distance using the distance formula and check if it is within the circle. 
  #It will iterate through p[i,1] and p[i,2] and check the distance across these points and evaluate via
  # the if() statement.If the if() is true, x.coord and y.coord will take on those points and they will 
  #represent the points in the circle. 
  
  for (i in 1:p.1)
  {#for
    #In an x-y Cartesian coordinate system, the circle with centre coordinates (a, b) and radius r is the set 
    #of all points (x, y) such that:
    if ((p[i,1] - center[1])^2 + (p[i,2] - center[2])^2 <= radius^2)
    {#if
      #Allocatio of points in circle.
      x.coord[i] = p[i,1]
      y.coord[i] = p[i,2]
    }#if
  }#for
  
  
  #Get valid values and cbind to get vector of points in circle.
  x.coord = x.coord[x.coord != 0]
  y.coord = y.coord[y.coord != 0]
  coordinates = cbind(x.coord,y.coord)
  
  #Get plot values and the parameters for the equation of a circle.
  theta = seq(0,2*pi,length = 2000)
  a = center[1]
  b = center[2]
  x = a + (radius*cos(theta))
  y = b + (radius*sin(theta))
  
  #Plot cirlce and add points 
  plot(x,y, type = 'l', xlim = c(0,max(p[,1])), ylim = c(0,max(p[,2])), main = "Plot of circle with given parameters", ylab = "Y", xlab = "X")
  points(p[,1], p[,2], col = "blue")
  
  #The points inside the cirlce are red.
  points(x.coord,y.coord, col = "red")
  print(coordinates)
  return(coordinates)
  
}#End Function

inside.circle(c(-2.140090, 57.13763), 0.01, coord)





setwd("~/OneDrive/r-files/AffinityPropagationClustering/")
filenames <- list.files(path = "~/OneDrive/r-files/AffinityPropagationClustering/geo/") 
filenames
setwd("~/OneDrive/r-files/AffinityPropagationClustering/geo/") 
data <- do.call("rbind", lapply(filenames, read.csv, header = TRUE, sep = ";")) 
setwd("~/OneDrive/r-files/AffinityPropagationClustering/")
head(data)
names(data)
coord_sub_set = sqldf("SELECT *, (6371 * acos(cos( radians(-30.053831)) * cos(radians(lat)) * cos(radians(long) - radians(-51.191810)) + sin(radians(-30.053831)) * sin(radians(lat)))) AS distancia FROM data GROUP BY lat HAVING distancia < 0.2 ORDER BY distancia ASC ")
dim(data)
dim(coord_sub_set)
coord_sub_set
