library(tidyverse)
library(tmap)
library(rgdal)
library(broom)
library(mapview)
library(crosstalk)
library(sf)
library(sp)
library(spdep)
library(car)
library(fs)
library(janitor)
library(here)

shape <- st_read(here::here("Census 2010 Tracts for San Francisco",
                          "geo_export_757ee74c-2883-48ae-8ea4-dbfa0de6d111.shp")) %>%
  st_transform(., 7131) 


Graffiti <- read_csv("Graffiti.csv")
  
Graffiti2<-Graffiti%>%
  separate(., Point, c("A", "B"), sep = ",")

Graffiti2$A<-parse_number(Graffiti2$A) ## leading $ and grouping character , ignored
Graffiti2$B<-parse_number(Graffiti2$B) ## leading $ and grouping character , ignored

Graffiti3<- Graffiti2%>%
  filter(A !=	0 )%>%
  filter(B != 0)%>%
  st_as_sf(., coords = c("B", "A"), 
           crs = 4326)

Graffiti4<- Graffiti3%>%
  filter(str_detect(Closed, "2019"))%>%
  #filter(str_detect(`Request Type`, "Building"))%>%
  st_transform(., crs=7131)

# spatial filter

Graffiti_within <- Graffiti4[shape, ,op=st_intersects]

tmap_mode("plot")
tm_shape(shape) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(Graffiti4) +
  tm_dots(col = "blue")

points_sf_joined <- shape%>%
  st_join(Graffiti4)%>%
  add_count(geoid10)%>%
  janitor::clean_names()%>%
  #calculate area
  mutate(area=st_area(.))%>%
  #then density of the points per ward
  mutate(density=n/area)%>%
  dplyr::select(geoid10 , neighborhood, density)%>%
  group_by(geoid10) %>%         
  summarise(geoid10 = first(geoid10),
            neighborhood= first(neighborhood),
            density= first(density))

library(readr)
library(janitor)

census_family <- read_csv(here::here("Household and Families", "ACSST5Y2019.S1101_data_with_overlays_2021-12-13T004557.csv"), skip=1)

census_family2 <- census_family%>%
  clean_names()

census_family3 <- shape %>%
  mutate(joiner = paste("1400000US", geoid10, sep=""))%>%
  dplyr::filter(geoid10 !='06075980401') 




library(sf)
points_sf_joined <- census_family3%>%
  st_join(Graffiti_within)%>%
  add_count(namelsad10)%>%
  janitor::clean_names()%>%
  #calculate area
  mutate(area=st_area(.))%>%
  #then density of the points per ward
  mutate(density=n/area)%>%
  #select density and some other variables 
  dplyr::select(density, namelsad10, joiner, n)

points_sf_joined<- points_sf_joined %>%                    
  group_by(joiner) %>%         
  summarise(density = first(density),
            wardname= first(namelsad10),
            Graffiticount= first(n))



tm_shape(points_sf_joined) +
  tm_polygons("density",
              style="jenks",
              palette="PuOr",
              midpoint=NA,
              popup.vars=c("wardname", "density"),
              title="Graffiti Density")

library(spdep)

coordsW <- points_sf_joined%>%
  st_centroid()%>%
  st_geometry()

plot(coordsW,axes=TRUE)

SFWard_nb <- points_sf_joined %>%
  poly2nb(., queen=T) 


summary(SFWard_nb)

#plot them
plot(SFWard_nb, st_geometry(coordsW), col="pink")
#add a map underneath
plot(points_sf_joined$geometry, add=T)

#create a spatial weights matrix from these weights
SFward.lw <- SFWard_nb %>%
  nb2mat(., style="B")

sum(SFward.lw)

sum(SFward.lw[,1])

SFward.lw <- SFWard_nb %>%
  nb2listw(., style="C")

I_SFWard_Global_Density <- points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  moran.test(., SFward.lw)

I_SFWard_Global_Density

C_SFWard_Global_Density <- 
  points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  geary.test(., SFward.lw)

C_SFWard_Global_Density

G_SFWard_Global_Density <- 
  points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  globalG.test(., SFward.lw)

G_SFWard_Global_Density

#use the localmoran function to generate I for each ward in the city

I_SFWard_Local_count <- points_sf_joined %>%
  pull(Graffiticount) %>%
  as.vector()%>%
  localmoran(., SFward.lw)%>%
  as_tibble()

I_SFWard_Local_Density <- points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  localmoran(., SFward.lw)%>%
  as_tibble()

#what does the output (the localMoran object) look like?
slice_head(I_SFWard_Local_Density, n=5)

points_sf_joined <- points_sf_joined %>%
  mutate(Graffiti_count_I = as.numeric(I_SFWard_Local_count$Ii))%>%
  mutate(Graffiti_count_Iz =as.numeric(I_SFWard_Local_count$Z.Ii))%>%
  mutate(density_I =as.numeric(I_SFWard_Local_Density$Ii))%>%
  mutate(density_Iz =as.numeric(I_SFWard_Local_Density$Z.Ii))

breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)


tm_shape(points_sf_joined) +
  tm_polygons("Graffiti_count_Iz",
              style="fixed",
              breaks=breaks1,
              midpoint=NA,
              title="Local Moran's I, Graffiti in London")

Gi_SFWard_Local_Density <- points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  localG(., SFward.lw)

head(Gi_SFWard_Local_Density)

points_sf_joined <- points_sf_joined %>%
  mutate(density_G = as.numeric(Gi_SFWard_Local_Density))


#now plot on an interactive map
tm_shape(points_sf_joined) +
  tm_polygons("density_G",
              style="fixed",
              breaks=breaks1,
              midpoint=NA,
              title="Gi*, Graffiti in London")


