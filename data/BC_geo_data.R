# Install if needed
install.packages(c("bcmaps", "sf", "dplyr", "ggplot2"))

remotes::install_github("bcgov/bcmapsdata")

# Load packages
library(bcmaps)
library(bcmapsdata)
library(sf)
library(dplyr)
library(ggplot2)

# Load BC major lakes and filter for Okanagan Lake
lakes <- bc_major_lakes()
okanagan_lake <- lakes %>% filter(grepl("Okanagan", lake_name, ignore.case = TRUE))


Okanagan Lake Reach Breaks (multiple)
ID: 360cbecd-1311-4114-a91f-c6c1f89a1db5
Name: okanagan-lake-reach-breaks

record <- bcdc_get_record("360cbecd-1311-4114-a91f-c6c1f89a1db5")

reach_breaks <- bcdc_query_geodata("360cbecd-1311-4114-a91f-c6c1f89a1db5") %>%
  collect()

resources <- bcdc_tidy_resources("360cbecd-1311-4114-a91f-c6c1f89a1db5")
print(resources)

# Full download of the Kokanee shore spawner dataset
kokanee_spawner_reaches <- bcdc_query_geodata("360cbecd-1311-4114-a91f-c6c1f89a1db5") %>%
  collect()

plot(st_geometry(kokanee_spawners), main = "Kokanee Shore Spawner Locations")


9: Kokanee Shore Spawner Data - Okanagan Region (multiple, wms, kml)
ID: 5e99f3c0-3d02-4e27-b25f-a5a8ad704450
Name: kokanee-shore-spawner-data-okanagan-region

library(bcdata)
library(sf)

# Full download of the Kokanee shore spawner dataset
kokanee_spawners <- bcdc_query_geodata("5e99f3c0-3d02-4e27-b25f-a5a8ad704450") %>%
  collect()

plot(st_geometry(kokanee_spawners), main = "Kokanee Shore Spawner Locations")

#check projcetion
st_crs(kokanee_spawners)

#put into lat-long if needed.
kokanee_spawners_wgs <- st_transform(kokanee_spawners, crs = 4326)


# For flat table (drops geometry)
write.csv(st_drop_geometry(kokanee_spawners_wgs), "kokanee_spawner_data.csv", row.names = FALSE)

# Or save full spatial object as shapefile or GeoJSON
st_write(kokanee_spawners_wgs, "kokanee_spawner_data.geojson")


library(bcdata)

# Get the full metadata record
record <- bcdc_get_record("5e99f3c0-3d02-4e27-b25f-a5a8ad704450")

# Print the metadata
record

record$contact
record$contact[[1]]
name: Ministry of Forests, Lands, Natural Resource Operations and Rural Development
email: john.doe@gov.bc.ca



