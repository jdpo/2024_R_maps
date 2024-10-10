#####Load libraries #####

### define libraries needed 
libs <- c("extrafont", "ggspatial", "tidyverse", "sf", "giscoR", "marmap", "ggplot2", "rnaturalearth", "ggrepel", "readxl", "stringr") #list of needeed libraries 

### define libraries already installed
installed_libs <- libs %in% rownames(installed.packages()) #create a list of all installed libraries

### compare the list of needed libraries with installed libraries and install ones that are missing
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

### load all libraries needed (Note: You need to first install a library (permanent) and then load it in the script (only for the session) to use it)
invisible(lapply(libs, library, character.only = T))




##### Format data #####

# read data from sheet 1 and transform degrees and minutes to decimal degrees
data <- read_xlsx("data/M185_Data-for-maps.xlsx", sheet = 1) %>% # read data 
  separate(col = lat, into = c("lat.deg","lat.min"), sep = "\\°") %>% # seperate lat degrees and minutes at the ° sign
  separate(col = long, into = c("long.deg","long.min"), sep = "\\°") %>% # seperate long degrees and minutes at the ° sign
  mutate(lat = as.numeric(lat.deg)+(as.numeric(lat.min)/60),  # calculate minutes as decimal degrees and add to the degrees
         long = as.numeric(long.deg)+(as.numeric(long.min)/60), # calculate minutes as decimal degrees and add to the degrees
         long = long*-1) %>% #longitude is positive in the file, but on the x-axis these are negative values, hence multiply by -1
  select(stat, lat, long, density) # remove rdundant columns
  
# create subsets for plots
data_zero <- data %>% filter(density == 0) # create a dataframe with all stations where catch was zero (to ease different display on map)
data_plus <- data %>% filter(density != 0) # create a dataframe with all station where catch was not zero (to ease different display on map)
data_label <- data %>% 
  mutate(label = sub("\\-.*", "", stat)) %>% # stations include a grear number, here extract everything before "-" to get only station number (named "label") 
  group_by(label) %>% #group data by "label"
  slice_head() #since at some stations gears were applied several times, some stations have multiple rows. Thus they'd receive multiple lables. Slice_head() takes only the first value of a group, hence every station is only there once after this step.

# read data from sheet 2, same as above just for sheet 2
data_red <- read_xlsx("data/M185_Data-for-maps.xlsx", sheet = 2)%>% 
  separate(col = lat, into = c("lat.deg","lat.min"), sep = "\\°") %>% 
  separate(col = long, into = c("long.deg","long.min"), sep = "\\°") %>%
  mutate(lat = as.numeric(lat.deg)+(as.numeric(lat.min)/60),  
         long = as.numeric(long.deg)+(as.numeric(long.min)/60),
         long = long*-1) %>%   
  select(stat, lat, long, density)

#create subset for plots, same as above just for sheet 2
data_red_zero <- data_red %>% filter(density == 0)
data_red_plus <- data_red %>% filter(density != 0)
data_red_label <- data_red %>% 
  mutate(label = sub("\\-.*", "", stat)) %>% 
  group_by(label) %>% 
  slice_head()



### import fonts (just for looks)

# font_import()  #remove hashtag in very front and run only once (takes long), then add hashtag again
loadfonts(device = "win") #this loads the fonts - needs to be loaded each session




##### Create maps #####

### load country boundaries in high res - gisco  is better resolution than naturalearth, but not needed here
#countries <- gisco_get_countries(resolution = "01", epsg = "4326", country = c("Gibraltar", "Spain", "Morocco", "Portugal", "Andorra", "France", "Algeria"))


### Load shapefiles (sf) for different things from naturalearth (disabled the ones not needed)
#load map data
#rivers <- ne_download(scale = 10, returnclass = "sf", type ="rivers_lake_centerlines", category = "physical")
#rivers_eu <- ne_download(scale = 10, returnclass = "sf", type ="rivers_europe", category = "physical")
#lakes <- ne_download(scale = 10, returnclass = "sf", type ="lakes", category = "physical")
#lakes_eu <- ne_download(scale = 10, returnclass = "sf", type ="lakes_europe", category = "physical")
oceans <- ne_download(scale = 10, returnclass = "sf", type ="ocean", category = "physical") #loads sf with ocean/marine borders
countries <- ne_countries(scale = 50, returnclass = "sf") #loads sf with country borders
land <- ne_download(scale = "medium", returnclass = "sf", type ="land", category = "physical") #loads sf with land borders


### load raster bathymetric data
bat_b <- getNOAA.bathy(-14, 4, 32, 50, res = 1, keep = TRUE, path = "./data/") #load bathymetric data for the relevant region
all_big <- as.xyz(bat_b) #convert NOAA data to a dataframe with lat, long, and elevation (=V3) - use if topography should be shown as well
bat_big <- as.xyz(bat_b) %>% mutate(V3 = ifelse(V3 >= 0, 0, V3)) #convert as above but then change all values >0 to 0 to reduce the colour range for the legend - use if only bathymetry is shown and countries have separate fill


  
### plot full map with text repel
map_full <- ggplot() + #create an empty plot
  geom_tile(data = bat_big, aes(x = V1, y =V2, fill = V3), show.legend = FALSE) + #add bathymetry to the plot (show.legend turns legend on/off) 
  scale_fill_continuous(type = "gradient", low = "dodgerblue4",high = "lightblue") + #manually define the colour graadient 
  #scale_fill_etopo() + #preset alternative colour scale, this includes colours for values above 0 for nice maps (then use all_big and no other shapefiles)
  #scale_fill_continuous(limits=c(-1500, 0), breaks = seq(-1500, 0, by= 500)) + #can be used to set manual limits to the depth values and edit breaks, note that all values below -1500 should be set to -1500 otherwise they'll be grey
  geom_sf(color = "black", data = land) + #add sf with land borders to plot, no fill needed (done in countries) - somewhat redundant but allow to have different e.g. colour of country vs kland borders
  geom_sf(color = "gray55", fill = "oldlace", data = countries) + #add sf with country border and fill for land/country
  #geom_sf(fill = "aliceblue", data = oceans) + #can be used instead of geom_tile when uniformly coloured ocean is wanted
  coord_sf(xlim = c(-12, 2), ylim = c(34, 48), expand = T) + #define x/y limits of the map
  geom_point (data = data_plus, aes(x = long, y = lat, size = density), shape = 16, color = "red", alpha = 0.5) + #add points to the map, here size reflects the value of density
  geom_point (data = data_zero, aes(x = long, y = lat), shape = 1, color = "red", size = 1.2, stroke = 0.4) + #add points for zero stations with different symbol (could be done in one row, but let's keep it simple...)
  theme_bw() + #set a very simple scheme for aesthetic reasons
  geom_text_repel(data = data_label, aes(label = label, x = long, y = lat), size = 2, fontface = "bold", nudge_x = .5, segment.color = "black", #add lables and define textsize etc.(note: only the data and aes() is NEEDED)
                  color = "white",     # text color
                  bg.color = "grey30", # text shadow color
                  bg.r = 0.15,          # text shadow radius
                  direction = "y", #set reference scale for adjusting lables
                  vjust = 0.3, #adjust label position vertically
                  hjust = 1, #adjust label position horizontally
                  max.overlaps = 12, box.padding = 0.05) + #some settings to play with for different distance to the points etc. 
  annotation_scale(location = "br", width_hint = 0.2, height = unit(0.08, "in"), pad_y = unit(0.07, "in"), pad_x = unit(0.05, "in")) + #add a scale and edit size/position
  annotation_north_arrow(location = "br", which_north = "true", #add a northfacing arrow
                         pad_x = unit(-.03 , "in"), pad_y = unit(0.16, "in"), #edit arrow position
                         height = unit(.6, "in"), width = unit(.6, "in"), #edit arrow size
                         style = north_arrow_fancy_orienteering) + #edit arrow style
  theme(axis.title.x = element_blank(), #remove x-axis title
        axis.title.y = element_blank(), #remove y-axis title
        legend.position = c(.015, .99), #position legend
        legend.justification = c("left", "top"), #define legend orientation
        legend.margin = margin(3, 3, 3, 3), #define legend margins
        legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid', linewidth = 0.2), #edit legend cosmetics (here, add black line around legend)
        legend.title=element_blank()) #remove legend title


# save big map
ggsave("M185_full.png", map_full, path = "./maps/", width = 5, height = 6, device='png', dpi=300) #save as png


#plot same map with dataset used in analysis - see above, same code with one exception
map_reduced <- ggplot() +
  geom_tile(data = bat_big, aes(x = V1, y =V2, fill = V3), show.legend = FALSE) + #add bathymetry on the graph (show.legend turns legend on/off)
  scale_fill_continuous(type = "gradient", low = "dodgerblue4",high = "lightblue") +
  #scale_fill_etopo() + #alternative colour scale, this includes colours for values above 0 for nice maps (then use all_big and no other shapefiles)
  #scale_fill_continuous(limits=c(-1500, 0), breaks = seq(-1500, 0, by= 500)) + #can be used to set manual limits to the depth values and edit legend breaks
  geom_sf(color = "black", data = land) +
  geom_sf(color = "gray55", fill = "oldlace", data = countries) +
  #geom_sf(fill = "aliceblue", data = oceans) +
  coord_sf(xlim = c(-12, 2), ylim = c(34, 48), expand = T) +
  geom_point (data = data_plus, aes(x = long, y = lat, size = density), shape = 16, color = "red", alpha = 0) + #the dataset displayed here is reduced and has a different range, thus the legend is not consistent - here, I add the full dataset to the plot but turn transparency to 100% so the range is identical but the data not shown; result: consistent legends:)
  geom_point (data = data_red_plus, aes(x = long, y = lat, size = density), shape = 16, color = "red", alpha = 0.5) +
  geom_point (data = data_red_zero, aes(x = long, y = lat), shape = 1, color = "red", size = 1.2, stroke = 0.4) +
  theme_bw() +
  geom_text_repel(data = data_red_label, aes(label = label, x = long, y = lat), size = 2, fontface = "bold", nudge_x = .5, segment.color = "black",
                  color = "white",     # text color
                  bg.color = "grey30", # shadow color
                  bg.r = 0.15,          # shadow radius
                  direction = "y",
                  vjust = 0.3,
                  hjust = 1, 
                  max.overlaps = 12, box.padding = 0.05) +
  annotation_scale(location = "br", width_hint = 0.2, height = unit(0.08, "in"), pad_y = unit(0.07, "in"), pad_x = unit(0.05, "in")) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(-.03 , "in"), pad_y = unit(0.16, "in"),
                         height = unit(.6, "in"), width = unit(.6, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = c(.015, .99),
        legend.justification = c("left", "top"),
        legend.margin = margin(3, 3, 3, 3),
        legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid', linewidth = 0.2),
        legend.title=element_blank())

# save big map
ggsave("M185_reduced.png", map_reduced, path = "./maps/", width = 5, height = 6, device='png', dpi=300)
