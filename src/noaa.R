## write merged_weather.dest to csv to use in Power BI
write.csv(merged_weather.dest,"C:\\Users\\isaac\\Documents\\GitHub\\TAMIDS20\\data\\meteo_weather.csv", row.names = FALSE, col.names=TRUE)

mateo_nearby_stations()

monitors <- "ASN00003003"
weather = meteo_pull_monitors(monitors, date_min = "2018-01-01", date_max="2019-12-31", var=c("all"))


names(airport_coords)[1] = "id"
names(airport_coords)[2] = "latitude"
names(airport_coords)[3] = "longitude"
mateo_stations = meteo_nearby_stations(airport_coords, lat_colname = "latitude", lon_colname="longitude" )

monitors = as.vector(meteo_stations_trimmed$ID)
weather = meteo_pull_monitors(monitors, date_min = "2018-01-01", date_max="2019-12-31", var=c("all"))