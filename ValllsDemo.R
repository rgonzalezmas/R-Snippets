# install the packages if necessary
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("feedeR")) install.packages("feedeR")
if(!require("fs")) install.packages("fs")
if(!require("lubridate")) install.packages("lubridate")
if(!require("fs")) install.packages("fs")
if(!require("tmap")) install.packages("tmap")
if(!require("classInt")) install.packages("classInt")
if(!require("showtext")) install.packages("showtext")
if(!require("sysfonts")) install.packages("sysfonts")

# load packages
library(feedeR)
library(sf) 
library(fs)
library(tidyverse)
library(lubridate)
library(classInt)
library(tmap)


url <- "http://www.catastro.minhap.es/INSPIRE/buildings/ES.SDGC.bu.atom.xml"

# import RSS feed with provincial links
prov_enlaces <- feed.extract(url)
str(prov_enlaces) # object is a list
#List of 4
#$ title  : chr "Download service of Buildings. Territorial Office"
#$ link   : chr "http://www.catastro.minhap.es/INSPIRE/buildings/ES.SDGC.BU.atom.xml"
#$ updated: POSIXct[1:1], format: "2021-03-04"
#$ items  : tibble [52 × 5] (S3: tbl_df/tbl/data.frame)
#..$ title      : chr [1:52] "Territorial office 02 Albacete" "Territorial office 03 Alicante" "Territorial office 04 Almería" "Territorial office 05 Avila" ...
#..$ date       : POSIXct[1:52], format: "2021-03-04" "2021-03-04" "2021-03-04" "2021-03-04" ...
#..$ link       : chr [1:52] "http://www.catastro.minhap.es/INSPIRE/buildings/02/ES.SDGC.bu.atom_02.xml" "http://www.catastro.minhap.es/INSPIRE/buildings/03/ES.SDGC.bu.atom_03.xml" "http://www.catastro.minhap.es/INSPIRE/buildings/04/ES.SDGC.bu.atom_04.xml" "http://www.catastro.minhap.es/INSPIRE/buildings/05/ES.SDGC.bu.atom_05.xml" ...
#..$ description: chr [1:52] "\n\t\t  " "\n\t\t  " "\n\t\t  " "\n\t\t  " ...
#..$ hash       : chr [1:52] "d21ebb7975e59937" "bdba5e149f09e9d8" "03bcbcc7c5be2e17" "8a154202dd778143" ...
prov_enlaces_tab <- as_tibble(prov_enlaces$items)
prov_enlaces_tab
#1 "Territorial office 02 … 2021-03-04 00:00:00 http://www.catastro.minhap.es/INSPIRE/buildings/… "\n\t\t  "  d21ebb7975e…
# 2 "Territorial office 03 … 2021-03-04 00:00:00 http://www.catastro.minhap.es/INSPIRE/buildings/… "\n\t\t  "  bdba5e149f0…
#3 "Territorial office 04 … 2021-03-04 00:00:00 http://www.catastro.minhap.es/INSPIRE/buildings/… "\n\t\t  "  03bcbcc7c5b…
# 4 "Territorial office 05 … 2021-03-04 00:00:00 http://www.catastro.minhap.es/INSPIRE/buildings/… "\n\t\t  "  8a154202dd7…
#5 "Territorial office 06 … 2021-03-04 00:00:00 http://www.catastro.minhap.es/INSPIRE/buildings/… "\n\t\t  "  7d3fd37631f…
# 6 "Territorial office 07 … 2021-03-04 00:00:00 http://www.catastro.minhap.es/INSPIRE/buildings/… "\n\t\t  "  9c08741f1c5…
#7 "Territorial office 08 … 2021-03-04 00:00:00 http://www.catastro.minhap.es/INSPIRE/buildings/… "\n\t\t  "  ff722b15e11…
# 8 "Territorial office 09 … 2021-03-04 00:00:00 http://www.catastro.minhap.es/INSPIRE/buildings/… "\n\t\t  "  b431aa61bd3…
#9 "Territorial office 10 … 2021-03-04 00:00:00 http://www.catastro.minhap.es/INSPIRE/buildings/… "\n\t\t  "  f79c6562d93…
#10 "Territorial office 11 … 2021-03-04 00:00:00 http://www.catastro.minhap.es/INSPIRE/buildings/… "\n\t\t  "  d702a6a8c58…




# filter the province and get the RSS link
val_atom <- filter(prov_enlaces_tab, str_detect(title, "Tarragona")) %>% pull(link)

# import the RSS
val_enlaces <- feed.extract(val_atom)

# get the table with the download links
val_enlaces_tab <- val_enlaces$items

#  filter the table with the name of the city
val_link <- filter(val_enlaces_tab, str_detect(title, "VALLS")) %>% pull(link)
val_link
## [1] "http://www.catastro.minhap.es/INSPIRE/Buildings/43/43163-VALLS/A.ES.SDGC.BU.43163.zip"

# create a temporary file
temp <- tempfile()

# download the data
download.file(URLencode(val_link), temp)

# unzip to a folder called buildings
unzip(temp, exdir = "buildings_valls")



# get the path with the file
file_val <- dir_ls("buildings_valls", regexp = "building.gml")

# import the data
buildings_val <- st_read(file_val)
#Reading layer `Building' from data source `/home/rgonzalez/devel/r/buildings_valls/A.ES.SDGC.BU.43163.building.gml' using driver `GML'
#Simple feature collection with 6748 features and 24 fields
#Geometry type: MULTIPOLYGON
#Dimension:     XY
#Bounding box:  xmin: 348762.1 ymin: 4567959 xmax: 356719.3 ymax: 4577808
#CRS:           25831




buildings_val <- mutate(buildings_val, 
                        end = str_replace(end, "^-", "0000") %>% 
                          ymd_hms() %>% as_date()
                )

buildings_val <- mutate(buildings_val, 
                        beginning = str_replace(beginning, "^-", "0000") %>% 
                          ymd_hms() %>% as_date()
)
##Warning message:
#Problem with `mutate()` column `beginning`.
#ℹ `beginning = str_replace(beginning, "^-", "0000") %>% ymd_hms() %>% as_date()`.
#ℹ  3 failed to parse.

#font download
sysfonts::font_add_google("Montserrat", "Montserrat")

#use showtext for fonts
showtext::showtext_auto() 
# limit the period after 1815
filter(buildings_val, beginning >= "1815-01-01") %>%
  ggplot(aes(beginning)) + 
  geom_density(fill = "#2166ac", alpha = 0.7) +
  scale_x_date(date_breaks = "20 year", 
               date_labels = "%Y") +
  #scale_y_date()
  theme_minimal() +
  theme(title = element_text(family = "Montserrat"),
        axis.text = element_text(family = "Montserrat")) +
  labs(y = "",x = "", title = "Evolution of urban development")



# get the coordinates of Valencia
ciudad_point <- tmaptools::geocode_OSM('Valls', 
                                       as.sf = TRUE)

#  project the points with EPGS on Cadastro File
ciudad_point <- st_transform(ciudad_point, 25831)

# create the buffer (2 km)
point_bf <- st_buffer(ciudad_point, 2500)


# get the intersection between the buffer and the building
buildings_val25 <- st_intersection(buildings_val, point_bf)
## Warning: attribute variables are assumed to be spatially constant
## throughout all geometries


# Filter errors
buildings_val25 <- subset(buildings_val25,beginning!='0-01-01')



# find 13 classes
#br <- classIntervals(year(buildings_val25$beginning),15,"equal",unique=TRUE)
br <- classIntervals(year(buildings_val25$beginning), n=13, style="kmeans", dataPrecision=0)

#br <- classIntervals(year(buildings_val25$beginning), 20, "fixed",unique=TRUE)

## Warning in classIntervals(year(buildings_val25$beginning), 15, "quantile"):
## var has missing values, omitted in finding classes
# create labels
lab <- names(print(br, under = "<", over = ">", cutlabels = FALSE))
## style: kmeans
##one of 3.172597e+16 possible partitions of this variable into 13 classes
##< 1873 1873 - 1908 1908 - 1925 1925 - 1937 1937 - 1948 1948 - 1956 1956 - 1962 1962 - 1970 1970 - 1978 1978 - 1987 
#22        1525          75          41          79         168         132         597         643         573 
##1987 - 1997 1997 - 2006      > 2006 
##653         542         191
# categorize the year
buildings_val25 <- mutate(buildings_val25, 
                          yr_cl = cut(year(beginning), br$brks, labels = lab, include.lowest = TRUE))




# colours
col_spec <- RColorBrewer::brewer.pal(11, "Spectral")

# colour ramp function
col_spec_fun <- colorRampPalette(col_spec)


#crear el mapa
ggplot(buildings_val25) +
  geom_sf(aes(fill = yr_cl), colour = "transparent") +
  scale_fill_manual(values = col_spec_fun(13)) + # adapta al número clases
labs(title = "VALLS", fill = "") +
  guides(fill = guide_legend(keywidth = .7, keyheight = 2.7)) +
  theme_void(base_family = "Montserrat") +
  theme(panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
       legend.justification = .5,
    legend.text = element_text(colour = "white", size = 12),
     plot.title = element_text(colour = "white", hjust = .5, size = 60,
                              margin = margin(t = 30)),
  plot.caption = element_text(colour = "white",
                               margin = margin(b = 20), hjust = .5, size = 16),
 plot.margin = margin(r = 40, l = 40))




#tmap_save(m, "choropleth.png", height = 7) # height interpreted in inches

# dynamic map
#mapa tmap de Santiago
m <-   tm_shape(buildings_val25) +
  tm_polygons("yr_cl", 
              border.col = "transparent",
              palette = col_spec_fun(13), # adapta al número clases
              textNA = "Without data",
              title = "")
tmap_leaflet(m)






