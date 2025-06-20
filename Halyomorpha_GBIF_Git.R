# Install Wallace from CRAN
install.packages("scrubr")
#######################################
## DOWNLOAD AND CLEAN DATA FROM GBIF ##
#######################################
library(rgbif)
library(scrubr)
library(maps)
# IF YOU HAVE ONLY ONE SPECIES ----
myspecies <- c("Halyomorpha halys")
european_codes <- c('AL','AD','AT','BY','BE','BA','BG','HR','CY','CZ','DK','EE','FI','FR','DE','GR','HU','IS','IE','IT','XK','LV','LI','LT','LU','MT','MD','MC','ME','NL','MK','NO','PL','PT','RO','RU','SM','RS','SK','SI','ES','SE','CH','UA','GB','VA')
# download GBIF occurrence data for this species; this takes time if there are many data points!
gbif_data <- occ_data(scientificName = myspecies, country = european_codes, hasCoordinate = TRUE, limit = 25000)
# take a look at the downloaded data:
gbif_data

# Count the occurrences of each factor in my_column
library(dplyr)
library(purrr)


names(gbif_data)  # Check if elements are named by country codes
# Extract and count records from each country's data
country_counts <- map_dfr(gbif_data, ~ .x$data, .id = "country_code") %>%
  count(country_code, name = "occurrence_count") %>%
  arrange(desc(occurrence_count))


country_counts <- country_counts %>% filter(!is.na(country_code))

library(countrycode)
country_counts <- country_counts %>%
  mutate(country_name = countrycode(country_code, "iso2c", "country.name"))


###
yearly_counts <- map_dfr(gbif_data, ~ .x$data, .id = "country_code") %>%
  mutate(year = lubridate::year(as.Date(eventDate))) %>%
  filter(!is.na(year), !year %in% c(1900, 1980)) %>%  # Exclude specific years
  count(year, name = "occurrence_count") %>%
  arrange(year)

# Then plot as before
ggplot(yearly_counts, aes(x = year, y = occurrence_count)) +
  geom_col(fill = "steelblue") +
  labs(title = "",
       x = "Year",
       y = "Number of Occurrences") +
  theme_minimal()

ggplot(yearly_counts, aes(x = year, y = occurrence_count)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = occurrence_count),  # Add text labels
            vjust = -0.5,                   # Position above the bars
            size = 3,                       # Adjust text size
            color = "black") +
  labs(title = "",
       x = "Year",
       y = "Number of Occurrences") +
  theme_minimal() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))  # Add space for labels

gg<-ggplot(yearly_counts, aes(x = year, y = occurrence_count)) +
  geom_col(fill = "grey") +
  geom_text(
    aes(label = occurrence_count),
    vjust = -0.5,
    size = 3,
    color = "black"
  ) +
  labs(
    x = "Year",
    y = "Number of occurrences"
  ) +
  theme_minimal() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_x_continuous(
    breaks = yearly_counts$year,  # Force every year as a break
    labels = yearly_counts$year   # Ensure labels match breaks
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate for readability
  )


gg
ggsave("Yearly.jpeg", gg, dpi = 900)

###
yearly_counts2 <- yearly_counts %>%
  arrange(year) %>%  # Ensure data is sorted by year
  mutate(cumulative_occurrences = cumsum(occurrence_count))

ggplot(yearly_counts2, aes(x = year, y = cumulative_occurrences)) +
  geom_col(fill = "steelblue") +
  geom_text(
    aes(label = cumulative_occurrences),
    vjust = -0.5,
    size = 3,
    color = "black"
  ) +
  labs(
    title = "Cumulative H. halys Occurrences Over Time",
    x = "Year",
    y = "Cumulative Number of Occurrences"
  ) +
  theme_minimal() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

#####

library(ggplot2)
gg<-ggplot(country_counts, aes(x = reorder(country_name, occurrence_count), y = occurrence_count)) +
  geom_col() +
  coord_flip() +  # Horizontal bars
  labs(x = "Country", y = "Occurrences") +
  theme_minimal()
gg

setwd("C://Users/grego/OneDrive/Halyomorpha")

write.csv2(country_counts,"country_counts.csv")
ggsave("CountryCount.jpeg", gg, dpi = 900)

library(sf)
# Combine all occurrences from the list into one dataframe
occ_all <- bind_rows(lapply(gbif_data, function(x) x$data), .id = "country_code")

# Keep only records with coordinates in Europe
europe_occ <- occ_all %>%
  filter(
    !is.na(decimalLongitude), 
    !is.na(decimalLatitude),
    decimalLongitude >= -25 & decimalLongitude <= 40,  # Europe's approximate bbox
    decimalLatitude >= 35 & decimalLatitude <= 75
  ) %>%
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)  # Convert to spatial object

library(rnaturalearth)
library(rnaturalearthdata)
# Download Europe map (countries or coastlines)
europe_map <- ne_countries(scale = "medium", continent = "europe", returnclass = "sf")
gg2<-ggplot() +
  geom_sf(data = europe_map, fill = "lightgray", color = "white") +  # Base map
  geom_sf(data = europe_occ, color = "red", size = 1, alpha = 0.5) +  # Occurrences
  labs(title = "Occurrences of Halyomorpha halys in Europe") +
  theme_minimal() +
  coord_sf(
    xlim = c(-25, 40), ylim = c(35, 75),  # Focus on Europe
    expand = FALSE
  )
ggsave("EuropeOccurence.jpeg", gg2, dpi = 900)

library(leaflet)

leaflet(europe_occ) %>%
  addProviderTiles("CartoDB.Positron") %>%  # Base map style
  addCircleMarkers(
    lng = ~decimalLongitude, 
    lat = ~decimalLatitude,
    radius = 3,
    color = "darkred",
    popup = ~paste("Country:", country, "<br>Date:", eventDate)  # Show info on click
  ) %>%
  setView(lng = 10, lat = 50, zoom = 4)  # Center on Europe

library(ggdensity)

ggplot() +
  geom_sf(data = europe_map, fill = "lightgray") +
  geom_hdr(
    data = as.data.frame(europe_occ), 
    aes(x = st_coordinates(europe_occ)[,1], y = st_coordinates(europe_occ)[,2]),
    probs = c(0.5, 0.9),  # Contour levels
    fill = "red", alpha = 0.3
  ) +
  coord_sf(xlim = c(-25, 40), ylim = c(35, 75))

###OPTION LOL
ggplot() +
  geom_sf(data = europe_map) +
  geom_sf(data = europe_occ, aes(color = country_code)) +
  facet_wrap(~country_code)  # One map per country

####Invasion front
library(dplyr)
library(sf)
library(lubridate)

# Combine data and extract year from eventDate
occ_all2 <- bind_rows(lapply(gbif_data, function(x) x$data), .id = "country_code") %>%
  filter(
    !is.na(decimalLongitude),
    !is.na(decimalLatitude),
    !is.na(year)  # Ensure year exists
  ) %>%
  mutate(
    year = as.numeric(year),  # Extract year from date
    decade = floor(year / 10) * 10  # Optional: Group by decade
  ) %>%
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)


occ_2 <- occ_all2 %>%
  filter(!year %in% c(1980, 1900))  # Exclude specific years
occ_2 <- occ_2[!occ_2$year %in% c(1980, 1900), ]

gg3<-ggplot() +
  geom_sf(data = europe_map, fill = "lightgray") +
  geom_sf(data = occ_2, aes(color = year), size = 1, alpha = 0.7) +
  scale_color_viridis_c(option = "plasma", name = "Year") +  # Color gradient
  labs(title = "Invasion Front of Halyomorpha halys in Europe") +
  coord_sf(xlim = c(-25, 40), ylim = c(35, 75)) +
  theme_minimal()
ggsave("InvasionEuropeOccurence.jpeg", gg3, dpi = 900)


###Mise en gif
library(gganimate)

# Create base plot
p <- ggplot() +
  geom_sf(data = europe_map, fill = "lightgray") +
  geom_sf(data = occ_2, color = "red", size = 1) +
  labs(title = "Year: {frame_time}") +
  coord_sf(xlim = c(-25, 40), ylim = c(35, 75)) +
  theme_minimal()

# Animate by year
anim <- p +
  transition_time(year) +
  shadow_mark(past = TRUE)  # Keep past points visible

# Save animation
anim2<-animate(anim, fps = 2, width = 800, height = 600)
gganimate::anim_save("C://Users/grego/OneDrive/Halyomorpha/Review/invasion_front.gif", anim2)


###Invasion as contours
library(ggridges)

# Calculate yearly convex hulls (invasion front)
yearly_hulls <- occ_2 %>%
  group_by(year) %>%
  summarise(geometry = st_convex_hull(st_union(geometry))) %>%
  filter(st_area(geometry) > 0)  # Remove empty years

# Plot expanding fronts
ggplot() +
  geom_sf(data = europe, fill = "lightgray") +
  geom_sf(data = yearly_hulls, aes(fill = year), alpha = 0.3) +
  scale_fill_viridis_c(name = "Year") +
  coord_sf(xlim = c(-25, 40), ylim = c(35, 75))



# Get unique values
unique_values <- unique(occ_all2$year)

# Print the result
print(unique_values)







# Get Europe map
europe <- rnaturalearth::ne_countries(scale = "medium", continent = "europe", returnclass = "sf")




# Count records by country
country_counts <- gbif_data %>%
  count(country, name = "occurrence_count") %>%
  arrange(desc(occurrence_count))  # Sort descending

# View results
print(country_counts)










