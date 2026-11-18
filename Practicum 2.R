#Author: Leah Kim 
#Date: 11/13/2026
#Topic: Practicum 2 

#Load packages
install.packages('tidycensus')
library(tidyverse)
library(tidycensus)
library(dplyr)
library(sf)
library(ggrepel)

#Set working directory and load data 
setwd('/Users/leahkim/Desktop/academics ✩ ° ｡⋆/PAI 741/Assignments /Practicum 2')
counties <- read_sf('/Users/leahkim/Desktop/academics ✩ ° ｡⋆/PAI 741/Data for HW/cb_2024_us_county_500k')
highpoints <- read_csv('/Users/leahkim/Desktop/academics ✩ ° ｡⋆/PAI 741/Data for HW/nyshighpoints.csv')

#Filter counties data to just include NY 
nycounties <- counties |>
  filter(STUSPS == "NY")
names(counties) 

#Q1: Fig 1 draw a map of NY counties 
fig1 <- nycounties |>
  ggplot() + 
  geom_sf(fill = "white", color = "black", linewidth = 0.2) + 
  labs(title = "NYS Counties") + 
  theme_void()
fig1

#Q2: St. Lawrence is the largest county by area in NYS. New York has the smallest county by area. 

#Q3, Q4, Q5, Q6: Plot the high-points in NYS 
#Turn Long/Lat into sf object
highpoints_sf <- highpoints |>
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) 

#Make high and lowest points objects 
highest <- highpoints_sf |> slice_max(Elevation_Feet, n = 1, with_ties = FALSE)
lowest <- highpoints_sf |> slice_min(Elevation_Feet, n = 1, with_ties = FALSE)
highest
#The highest point is in Essex County, called Mount Marcy.
lowest
#The lowest point is in Kings County, called Battle Hill

#Plot Fig 2: High and Low Points in NYS Counties 
fig2 <- ggplot() + 
  geom_sf(data = nycounties, fill = "white", color = "black", linewidth = 0.2) + 
  geom_sf(data = highpoints_sf, aes(fill = Elevation_Feet),
          shape = 21, size = 3) + 
  scale_fill_gradientn(colors = c("darkgreen", "yellowgreen", "yellow", 
                                  "saddlebrown", "white", "red"),
                       name = "Elevation (ft") +
  geom_label_repel(data = highest, 
                   aes(label = paste0(High_Point_Name, "\n", format(Elevation_Feet, big.mark = ","), " ft"),
                                 geometry = geometry), 
             stat = "sf_coordinates",
             nudge_x = 1.5, nudge_y = 0.5, size =3) + 
  labs(title = "Highest Points in NYS Counties") + 
  theme_void()
fig2

#Q7: County population in NYS 
#Load data and key 
census_api_key("a91e257d8f0ff66ff6f0d638b058e0d459e851e1", install=TRUE, overwrite=TRUE)
readRenviron("~/.Renviron")
file.edit("~/.Renviron")
Sys.setenv("CENSUS_API_KEY" = "a91e257d8f0ff66ff6f0d638b058e0d459e851e1")
nchar(Sys.getenv("a91e257d8f0ff66ff6f0d638b058e0d459e851e1"))

#Create NY population R object
ny_pop <- get_acs(geography = "county", 
                  variables = c(population = "B01003_001"), 
                  state = 36, 
                  year = 2023,
                  key = "a91e257d8f0ff66ff6f0d638b058e0d459e851e1") |>
  select(GEOID, population = estimate)

#Merge the NY counties and population data 
ny_data <- nycounties |>
  left_join(ny_pop, by = "GEOID")

#Plot Fig 3: Population by County in NYS
fig3 <- ny_data |>
  ggplot(mapping = aes(fill = population)) + 
  geom_sf(color = "white", linewidth = 0.1) +
  scale_fill_gradient(low= "lightgoldenrod", high = "sienna3", 
                      labels = format, name = "Population") + 
  labs(title = "Population by County, in NYS") + 
  theme_void()
fig3

#Q8: Calculating Population Density and Shade-in
#Need to mutate and divide by 2.59e6 to get square miles since it's in meters right now. 
ny_data <- ny_data |>
  mutate(area_sqmi = ALAND/2.59e6, 
         pop_dens = population/area_sqmi)

#Fig 4: Density Shading 
fig4 <- ny_data |>
  ggplot(mapping = aes(fill = pop_dens)) + 
  geom_sf(color = "white", linewidth = 0.1) + 
  scale_fill_gradient(low = "lavender", high = "purple4", 
                      trans = "log10", labels = format,
                      name = "People per sq mi") + 
  labs(title = "Population Density by County, in NYS") + 
  theme_void()
fig4

#Q9: Which areas have the most and least populous and dense 
#Population 
ny_data |>
  st_drop_geometry() |>
  select(NAME, population) |>
  arrange(desc(population)) |>
  slice(c(1, n()))
#The largest county by population is Kings County and the smallest county by population is Hamilton County. 

#Density 
ny_data |>
  st_drop_geometry() |>
  select(NAME, pop_dens) |>
  arrange(desc(pop_dens)) |>
  slice(c(1, n()))
#The county with the most population density is New York and the one with the least amount of population density is Hamilton. 

#BONUS Q10: Presidential Vote 2000-2020 
#Load new data 
load('/Users/leahkim/Desktop/academics ✩ ° ｡⋆/PAI 741/Data for HW/dataverse_shareable_presidential_county_returns_1868_2020.Rdata')
ls()
ny_vote <- pres_elections_release
names(ny_vote)
head(ny_vote)
unique(ny_vote$election_year)

#Mutate and filter the voting data 
ny_vote <- ny_vote |>
  filter(election_year %in% c(2000, 2004, 2008, 2012, 2016, 2020)) |>
  mutate(GEOID = str_pad(as.character(fips), 5, pad = "0")) |>
  filter(str_starts(GEOID, "36")) |>
  mutate(vote_margin = (republican_raw_votes - democratic_raw_votes)/
           (republican_raw_votes + democratic_raw_votes) * 100)

#Make a voting sf object 
vote_sf <- nycounties |>
  left_join(ny_vote, by = c("GEOID" = "fips"))

#Plot Fig 5: Election Margin in NYS Counties 
fig5 <- vote_sf |>
  ggplot(mapping = aes(fill = vote_margin)) + 
  geom_sf(color = "white", linewidth = 0.2) + 
  facet_wrap( ~ election_year, nrow=2) + 
  scale_fill_gradient2(low = "dodgerblue3", mid = "white", high = "firebrick",
                       midpoint = 0, name = "Republican margin (pts)") + 
  labs(title = "Bipartisan Presidential Vote by NYS Counties, 2000-2024",
       caption = "Red: more Republican; Blue: more Democratic") + 
  theme_void()
fig5

#Save all plots 
dir.create("figures", showWarnings = FALSE)
ggsave("figures/figure1.png", plot = fig1, width = 8, height = 5, dpi = 300)
ggsave("figures/figure2.png", plot = fig2, width = 8, height = 5, dpi = 300)
ggsave("figures/figure3.png", plot = fig3, width = 8, height = 5, dpi = 300)
ggsave("figures/figure4.png", plot = fig4, width = 8, height = 5, dpi = 300)
ggsave("figures/figure5.png", plot = fig5, width = 8, height = 5, dpi = 300)