#####Load libraries #####

### define libraries needed 
libs <- c("extrafont", "ggspatial", "tidyverse", "sf", "giscoR", "marmap", "ggplot2", "rnaturalearth", "ggrepel") 

### define libraries already installed
installed_libs <- libs %in% rownames(installed.packages())

### install libraries that are not installed already
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

### load libraries needed
invisible(lapply(libs, library, character.only = T))



##### Format data #####

data <- read.table("C:/Users/pohlmann/Desktop/Home_Office/Projekte/2022_M185/2024_R_Gibraltar/data/station_data.csv", sep = ";", header = T)

data_new <- data %>% mutate(long_d = as.numeric(gsub("(d).*", "", long)),
                            long_m = as.numeric(gsub(".*d", "", gsub("(m).*", "", long))),
                            lat_d = as.numeric(gsub("(d).*", "", lat)),
                            lat_m = as.numeric(gsub(".*d", "", gsub("(m).*", "", lat))),
                            lat_dec = lat_d + lat_m/60,
                            long_dec = (long_d + long_m/60)*-1,
                            station = gsub(".*_", "", station),
                            catch = as.numeric(ifelse(station == "41-8", "2", ifelse(station == "41-9", "2", ifelse(station == "41-11", "13", "0")))),
                            catch_bin = ifelse(catch < 2, "no", "yes")) %>%
                            group_by(station) %>% 
  arrange(datetime) %>% 
  select(-long_d, -long_m, -lat_d, -lat_m)

points_start <- data_new %>% group_by(station) %>% slice_head()
points_end <- data_new %>% group_by(station) %>% slice(n())

### font_import()  #only needed once
loadfonts(device = "win")

##### Create maps #####

### load country boundaries in high res
countries <- gisco_get_countries(resolution = "01", epsg = "4326", country = c("Gibraltar", "Spain", "Morocco", "Portugal", "Andorra", "France", "Algeria"))


#load map data
#rivers <- ne_download(scale = 10, returnclass = "sf", type ="rivers_lake_centerlines", category = "physical")
#rivers_eu <- ne_download(scale = 10, returnclass = "sf", type ="rivers_europe", category = "physical")
#lakes <- ne_download(scale = 10, returnclass = "sf", type ="lakes", category = "physical")
#lakes_eu <- ne_download(scale = 10, returnclass = "sf", type ="lakes_europe", category = "physical")
#oceans <- ne_download(scale = 10, returnclass = "sf", type ="ocean", category = "physical")
#countries <- ne_countries(scale = 50, returnclass = "sf")

### load raster bathymetric data
bat_b <- getNOAA.bathy(-12, 4, 32, 46, res = 1, keep = TRUE, path = "./data/")
bat_big <- as.xyz(bat_b) %>% mutate(V3 = ifelse(V3 >= 0, 0, V3))
all_big <- as.xyz(bat_b)
bat_s <- getNOAA.bathy(-8, -3, 33, 39, res = 0.25, keep = TRUE, path = "./data/")
bat_small <- as.xyz(bat_s) %>% mutate(V3 = ifelse(V3 >= 0, 0, V3))
all_small <- as.xyz(bat_s) 

  
### plot large map
map_big <- ggplot() +
  #geom_tile(data = all_big, aes(x = V1, y =V2, fill = V3)) + #for low res use geom_raster and interpolate = T
  #scale_fill_etopo() +
  #geom_sf(color = "transparent", fill = "white", data = oceans) +
  #scale_fill_continuous(limits=c(-1500, 0), breaks = seq(-1500, 0, by= 500)) +
  geom_sf(color = "gray25", fill = "grey90", data = countries) +
  geom_rect(mapping = aes(xmin = -6, xmax = -5.3, ymin = 35.7, ymax = 36.2), color = "black", alpha = 0) +
  coord_sf(xlim = c(-9.5, -3), ylim = c(34, 40), expand = T) +
  theme_void() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

# save big map
ggsave("big_nice.png", map_big, path = "./maps/", width = 5, height = 6, device='png', dpi=300) 


### plot zoomed map

# define function to convert decimal degrees to min°sec'
decimal_to_dms_N <- function(decimal_degree) {
  degrees <- floor(decimal_degree)
  minutes <- as.character((decimal_degree - degrees) * 60/100)
  minutes_2 <- ifelse(minutes == "0", "0.00", minutes)
  return(paste0(degrees, "°", str_sub((minutes_2),3,4), "'", "N"))
}

decimal_to_dms_W <- function(decimal_degree) {
  degrees <- floor(decimal_degree*-1)
  minutes <- floor((decimal_degree*-1 - degrees) * 60)
  return(paste0(degrees, "°", minutes, "'", "W"))
}

# plot map
map_small <- ggplot() +
  geom_raster(data = bat_small, aes(x = V1, y =V2, fill = V3), interpolate = T, show.legend = F) + #or use geom_tile without "interpolate"
  scale_fill_continuous(limits=c(-1500, 0), breaks = seq(-1500, 0, by= 500)) +
  #geom_path(data = data_new, aes(x = long_dec, y = lat_dec, group = station, color = catch_bin), arrow = arrow(type = "closed", length = unit(0.2, "cm")), linewidth = 0.6) +
  #scale_color_manual(values = my_col) +
  geom_sf(color = "gray25", fill = "gray90", data = countries) +
  #geom_sf(color = "black", fill = "white", data = oceans) +
  geom_point(data = points_end, aes(x = long_dec, y = lat_dec), size = 1.5, alpha = 0.7, shape = 16, show.legend = F, colour = "black") +
  geom_label_repel(aes(label = station, x = long_dec, y = lat_dec), data = points_end, size = 4, fontface = "bold", box.padding = 0.5, max.overlaps = 15) +
  geom_point(aes(x = mean(c(max(points_start$long_dec), min(points_start$long_dec))), y = mean(c(max(points_start$lat_dec)), min(points_start$lat_dec))), shape = 1, size = 3.5, show.legend = F, colour = "black") +
  #geom_rect(mapping = aes(xmin = min(data_new$long_dec), xmax = max(data_new$long_dec), ymin = min(data_new$lat_dec), ymax = max(data_new$lat_dec)), fill = "red", color = "transparent", alpha = 0.6) +
  #geom_ellipse(aes(x0 = mean(c(max(data_new$long_dec), min(data_new$long_dec))),
                   #y0 = mean(c(max(data_new$lat_dec), min(data_new$lat_dec))), 
                   #a = max(data_new$long_dec) - mean(c(max(data_new$long_dec), min(data_new$long_dec))), 
                   #b = max(data_new$lat_dec) - mean(c(max(data_new$lat_dec), min(data_new$lat_dec))), 
                   #angle = 0), 
                   #fill = "red", colour = "transparent", alpha = 0.7, show.legend = F) +
  coord_sf(xlim = c(-6, -5.3), ylim = c(36.2, 35.7), expand = T) +
  #geom_rect(mapping = aes(xmin = -5.5, xmax = -5.28, ymin = 35.69, ymax = 35.78), fill = "white", color = "black", linewidth = 0.5) +
  scale_x_continuous(labels = function(x) sapply(x, decimal_to_dms_W)) +
  scale_y_continuous(labels = function(y) sapply(y, decimal_to_dms_N)) +
  annotation_scale(location = "br", width_hint = 0.2, pad_y = unit(0.12, "in"), pad_x = unit(0.12, "in")) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.02, "in"), pad_y = unit(0.24, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme_bw() +
  labs(fill = "Depth (m)") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(face = "bold", size = 16, family = "serif", , angle = 45, hjust = 1),
        axis.text.y = element_text(face = "bold", size = 16, family = "serif"))

# save zoomed map      
ggsave("small_nice.png", map_small, path = "./maps/", width = 7.8, height = 6, device='png', dpi=300) 



                                                                           