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
  
coord_df <- data.frame(coord, within_5km = geosphere::distHaversine(coord, c(-2.106472, 57.14455)) / 1000 < 10)
# convert m to km, check < 5
coord_df
str(coord_df)
dim(coord_df)
table(coord_df$within_5km)

coord_df <- data.frame(coord, within_5km = geosphere::distHaversine(coord, c(-2.106472, 57.14455)) < 500)
table(coord_df$within_5km)
coord_df

#https://stackoverflow.com/questions/32363998/function-to-calculate-geospatial-distance-between-two-points-lat-long-using-r
library(geosphere)
distm(c(-2.106472, 57.14455), c(-2.104092, 57.14485), fun = distHaversine)
coord_df$dist = apply(coor,1,sum)



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
