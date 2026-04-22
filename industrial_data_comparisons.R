library(tidyverse)
library(sf)
library(rmapshaper)
library(psrcelmer)
library(units)
library(leaflet)
library(DT)
library(scales)

source("functions.R")

# Inputs ------------------------------------------------------------------
wgs84 <- 4326
spn <- 2285

vacant_2013 <- 0.001
redevelopment_b_2013 <- 2.50
redevelopment_c_2013 <- 5.00

cpi_w_apr <- 1.39879952

vacant_threshold <- vacant_2013*cpi_w_apr
redevel_threshold <- redevelopment_c_2013*cpi_w_apr

urban_deduction <- 1.0
suburban_deduction <- 1.0

recalculate_ili_zoning_layer <- "no"
recalculate_county_parcel_files <- "no"

final_dec <- -1

# Gross Industrial Lands
gross_lands_inv_file <- "Y:/Industrial Lands/2022 Industrial Lands Analysis Update/Data/GIS/v108/ILI_Zoning_Update.gdb"
gross_lands_inv_lyr <- "ILI_20231221"

# Exclusion Files from CAI work
exclusion_path <- "Y:/Industrial Lands/2015 Industrial Lands Analysis/GIS/Net Supply"
wetlands_file <- file.path(exclusion_path, "FourCntyWetlandFloodway.shp")
tiera_file <- file.path(exclusion_path, "Manual_Exclusions_TierA.shp")
tierb_file <- file.path(exclusion_path, "Manual_Exclusions_TierB.shp")
tierc_file <- file.path(exclusion_path, "Manual_Exclusions_TierC.shp")
public_use_file <- file.path(exclusion_path, "Public Use Exclusions.shp")

# Parcel Files
parcel_size_threshold <- 0.05
king_file <- "parcels23_kin_dissolve.shp"
kitsap_file <- "parcels23_kit_dissolve.shp"
pierce_file <- "parcels23_pierce_base_dissolve.shp"
snohomish_file <- "parcels23_snoh_dissolve.shp"
parcel_data_file <- "parcels_all.csv"
parcel_building_file <- "buildings_all.csv"

# Order for tables
county_ord <- c("King", "Kitsap", "Pierce", "Snohomish", "Region")
numeric_columns <- c("Zoned",
                     "Limited",
                     "Water",
                     "Wetlands",
                     "Tier A",
                     "Tier B",
                     "Tier C",
                     "Public")

# Spatial Layers
mic <- st_read_elmergeo("micen") |> select(mic="mic") |>st_transform(spn)
city <- st_read_elmergeo("cities") |> select(city="city_name") |> st_transform(spn)
rgeo <- st_read_elmergeo("regional_geographies") |> select(rgeo="class_desc") |> st_transform(spn)

# Industrial Lands Layer -----------------------------------------
if (recalculate_ili_zoning_layer == "yes") {
  
  # Zoned Industrial Lands
  zoned_industrial_lands <- st_read(gross_lands_inv_file, gross_lands_inv_lyr) |>
    select(county_code = "County_FLU", county_name = "County", category="Ind_Category", "Acres", "Total_Acres", "Land_Acres", "Water_Acres") |>
    st_transform(crs = spn) 
  
  zoned_industrial_lands <- zoned_industrial_lands |> 
    mutate(total_acreage = as.numeric(set_units(st_area(zoned_industrial_lands), "acre"))) |>
    mutate(county_name = case_when(
      is.na(county_name) ~ county_code,
      !(is.na(county_name)) ~ county_name)) |>
    mutate(county_name = case_when(
      county_name %in% c("King", "Kitsap", "Pierce", "Snohomish") ~ county_name,
      county_name == "033" ~ "King",
      county_name == "035" ~ "Kitsap",
      county_name == "053" ~ "Pierce",
      county_name == "061" ~ "Snohomish")) |> 
    select("county_name", "category", "total_acreage") |>
    filter(total_acreage > 0) |>
    st_zm() |>
    rowid_to_column("index")
  
  saveRDS(zoned_industrial_lands, "data/zoned_industrial_lands.rds")
  
  # Limited, Airport and Military Exclusion
  industrial_lands_remove_limited <- st_read(gross_lands_inv_file, gross_lands_inv_lyr) |>
    filter(Ind_Code %in% c(1,2)) |>
    select(county_code = "County_FLU", county_name = "County", category="Ind_Category", "Acres", "Total_Acres", "Land_Acres", "Water_Acres") |>
    st_transform(crs = spn) 
  
  industrial_lands_remove_limited <- industrial_lands_remove_limited |> 
    mutate(total_acreage = as.numeric(set_units(st_area(industrial_lands_remove_limited), "acre"))) |>
    mutate(county_name = case_when(
      is.na(county_name) ~ county_code,
      !(is.na(county_name)) ~ county_name)) |>
    mutate(county_name = case_when(
      county_name %in% c("King", "Kitsap", "Pierce", "Snohomish") ~ county_name,
      county_name == "033" ~ "King",
      county_name == "035" ~ "Kitsap",
      county_name == "053" ~ "Pierce",
      county_name == "061" ~ "Snohomish")) |> 
    select("county_name", "category", "total_acreage") |>
    filter(total_acreage > 0) |>
    st_zm() |>
    rowid_to_column("index")
  
  exclude_lyr <- st_read(gross_lands_inv_file, gross_lands_inv_lyr) |>
    filter(!(Ind_Code %in% c(1,2))) |>
    select(county_code = "County_FLU", county_name = "County", category="Ind_Category", "Acres", "Total_Acres", "Land_Acres", "Water_Acres") |>
    st_transform(crs = spn)
  
  exclude_lyr <- exclude_lyr |> 
    mutate(total_acreage = as.numeric(set_units(st_area(exclude_lyr), "acre"))) |>
    mutate(county_name = case_when(
      is.na(county_name) ~ county_code,
      !(is.na(county_name)) ~ county_name)) |>
    mutate(county_name = case_when(
      county_name %in% c("King", "Kitsap", "Pierce", "Snohomish") ~ county_name,
      county_name == "033" ~ "King",
      county_name == "035" ~ "Kitsap",
      county_name == "053" ~ "Pierce",
      county_name == "061" ~ "Snohomish")) |> 
    select("county_name", "category", "total_acreage") |>
    filter(total_acreage > 0) |>
    st_zm() |>
    rowid_to_column("index")
  
  saveRDS(industrial_lands_remove_limited, "data/industrial_lands_remove_limited.rds")
  saveRDS(exclude_lyr, "data/limited_exclusion.rds")
  
  # Remove Largest Water Bodies
  water_bodies <- st_read_elmergeo(layer_name = "largest_waterbodies") |> st_transform(crs = spn)
  
  intersection <- st_intersection(industrial_lands_remove_limited, water_bodies)
  
  intersection <- intersection |>
    mutate(total_acreage = as.numeric(set_units(st_area(intersection), "acre"))) |>
    select("index", "county_name", "category", "total_acreage")
  
  industrial_lands_remove_water <- ms_erase(industrial_lands_remove_limited, intersection) 
  
  industrial_lands_remove_water <- industrial_lands_remove_water |>
    mutate(total_acreage = as.numeric(set_units(st_area(industrial_lands_remove_water), "acre"))) |>
    select("index", "county_name", "category", "total_acreage") 
  
  saveRDS(industrial_lands_remove_water, "data/industrial_lands_remove_water.rds")
  saveRDS(intersection, "data/water_lyr.rds")
  
  # Remove Wetlands
  wetlands <- st_read(wetlands_file) |> st_make_valid() |> st_transform(crs = spn)
  
  intersection <- st_intersection(industrial_lands_remove_water, wetlands)
  
  intersection <- intersection |>
    mutate(total_acreage = as.numeric(set_units(st_area(intersection), "acre"))) |>
    select("index", "county_name", "category", "total_acreage")
  
  industrial_lands_remove_wetlands <- ms_erase(industrial_lands_remove_water, intersection) 
  
  industrial_lands_remove_wetlands <- industrial_lands_remove_wetlands |>
    mutate(total_acreage = as.numeric(set_units(st_area(industrial_lands_remove_wetlands), "acre"))) |>
    select("index", "county_name", "category", "total_acreage") 
  
  saveRDS(industrial_lands_remove_wetlands, "data/industrial_lands_remove_wetlands.rds")
  saveRDS(intersection, "data/wetlands_lyr.rds")
  
  # Remove Tier A
  tiera <- st_read(tiera_file) |> st_make_valid() |> st_transform(crs = spn) 
  
  intersection <- st_intersection(industrial_lands_remove_wetlands, tiera)  
  
  intersection <- intersection |> 
    mutate(total_acreage = as.numeric(set_units(st_area(intersection), "acre"))) |> 
    select("index", "county_name", "category", "total_acreage")  
  
  industrial_lands_remove_tiera <- ms_erase(industrial_lands_remove_wetlands, intersection)   
  
  industrial_lands_remove_tiera <- industrial_lands_remove_tiera |> 
    mutate(total_acreage = as.numeric(set_units(st_area(industrial_lands_remove_tiera), "acre"))) |> 
    select("index", "county_name", "category", "total_acreage") 
  
  saveRDS(industrial_lands_remove_tiera, "data/industrial_lands_remove_tiera.rds")
  saveRDS(intersection, "data/tiera_lyr.rds")
  
  # Remove Tier B
  tierb <- st_read(tierb_file) |> st_make_valid() |> st_transform(crs = spn) 
  
  intersection <- st_intersection(industrial_lands_remove_tiera, tierb)  
  
  intersection <- intersection |> 
    mutate(total_acreage = as.numeric(set_units(st_area(intersection), "acre"))) |> 
    select("index", "county_name", "category", "total_acreage")  
  
  industrial_lands_remove_tierb <- ms_erase(industrial_lands_remove_tiera, intersection)   
  
  industrial_lands_remove_tierb <- industrial_lands_remove_tierb |> 
    mutate(total_acreage = as.numeric(set_units(st_area(industrial_lands_remove_tierb), "acre"))) |> 
    select("index", "county_name", "category", "total_acreage") 
  
  saveRDS(industrial_lands_remove_tierb, "data/industrial_lands_remove_tierb.rds")
  saveRDS(intersection, "data/tierb_lyr.rds")
  
  # Remove Tier C
  tierc <- st_read(tierc_file) |> st_make_valid() |> st_transform(crs = spn) 
  
  intersection <- st_intersection(industrial_lands_remove_tierb, tierc)  
  
  intersection <- intersection |> 
    mutate(total_acreage = as.numeric(set_units(st_area(intersection), "acre"))) |> 
    select("index", "county_name", "category", "total_acreage")  
  
  industrial_lands_remove_tierc <- ms_erase(industrial_lands_remove_tierb, intersection)   
  industrial_lands_remove_tierc <- industrial_lands_remove_tierc |> 
    mutate(total_acreage = as.numeric(set_units(st_area(industrial_lands_remove_tierc), "acre"))) |> 
    select("index", "county_name", "category", "total_acreage") 
  
  saveRDS(industrial_lands_remove_tierc, "data/industrial_lands_remove_tierc.rds")
  saveRDS(intersection, "data/tierc_lyr.rds")
  
  # Remove Public Use
  public <- st_read(public_use_file) |> st_make_valid() |> st_transform(crs = spn) 
  
  intersection <- st_intersection(industrial_lands_remove_tierc, public)  
  
  intersection <- intersection |> 
    mutate(total_acreage = as.numeric(set_units(st_area(intersection), "acre"))) |> 
    select("index", "county_name", "category", "total_acreage")  
  
  industrial_lands_remove_public <- ms_erase(industrial_lands_remove_tierc, intersection)   
  
  industrial_lands_remove_public <- industrial_lands_remove_public |> 
    mutate(total_acreage = as.numeric(set_units(st_area(industrial_lands_remove_public), "acre"))) |> 
    select("index", "county_name", "category", "total_acreage") 
  
  saveRDS(industrial_lands_remove_public, "data/industrial_lands_remove_public.rds")
  saveRDS(intersection, "data/public_lyr.rds")
  
  industrial_lands_gross_acreage <- industrial_lands_remove_public
  rm(water_bodies, wetlands, tiera, tierb, tierc, public, intersection, exclude_lyr,
     zoned_industrial_lands, industrial_lands_remove_limited, industrial_lands_remove_water,
     industrial_lands_remove_wetlands, industrial_lands_remove_tiera,
     industrial_lands_remove_tierb, industrial_lands_remove_tierc, industrial_lands_remove_public)
  
} else {
  
  industrial_lands_gross_acreage <- readRDS("data/industrial_lands_remove_public.rds")
  
}

# Parcel File Processing ----------------------------------------------------
if (recalculate_county_parcel_files == "yes") {
  
  # Open parcels, calculate area, standardize attribute names and add City, Regional Geography and MIC to each record and combine
  king_parcels <- parcels_by_county(p_file = king_file, p_county = 33, p_id = "PIN")
  kitsap_parcels <- parcels_by_county(p_file = kitsap_file, p_county = 35, p_id = "RP_ACCT_ID")
  pierce_parcels <- parcels_by_county(p_file = pierce_file, p_county = 53, p_id = "TaxParcelN")
  snohomish_parcels <- parcels_by_county(p_file = snohomish_file, p_county = 61, p_id = "PARCEL_ID")
  region_parcels <- bind_rows(king_parcels, kitsap_parcels, pierce_parcels, snohomish_parcels)
  saveRDS(region_parcels, "data/cleaned_regional_parcels.rds")
  rm(king_parcels, kitsap_parcels, pierce_parcels, snohomish_parcels)
  
} else {
  
  region_parcels <- readRDS("data/cleaned_regional_parcels.rds")
  
}

# Add Improvement Value and other Industrial lands data to parcels
region_parcels <- add_parcel_data()
region_parcels_points <- st_centroid(region_parcels) 

# Clean Industrial Lands layer to use for intersection
industrial_zones <- industrial_lands_gross_acreage |> st_transform(crs = spn) |> select("index", "category")

# Intersect parcels with Industrial Lands layer using polygons
p_poly <- st_intersection(region_parcels, industrial_zones)
p_poly <- p_poly |> mutate(parcel_acreage = as.numeric(set_units(st_area(p_poly), "acre")))
saveRDS(p_poly, "data/industrial_parcels_based_on_polygons.rds")

# Intersect parcels with Industrial Lands layer using centroid of parcels - don't trim
p_point <- st_intersection(region_parcels_points, industrial_zones)
p_point_poly <- region_parcels |> filter(parcel_id_fips %in% unique(p_point$parcel_id_fips))
saveRDS(p_point_poly, "data/industrial_parcels_based_on_centroids_not_trimmed.rds")

# Intersect parcels with Industrial Lands layer using centroid of parcels - trim
p_point_poly_trim <- region_parcels |> filter(parcel_id_fips %in% unique(p_point$parcel_id_fips))
p_point_poly_trim <- st_intersection(p_point_poly_trim, industrial_zones)
p_point_poly_trim <- p_point_poly_trim |> mutate(parcel_acreage = as.numeric(set_units(st_area(p_point_poly_trim), "acre")))
saveRDS(p_point_poly_trim, "data/industrial_parcels_based_on_centroids_trimmed.rds")

# Summarise Parcel Acreage ----------------------------------------
il_acres_centroids <- p_point_poly |> 
  st_drop_geometry() |>
  mutate(adjusted_acreage = adjustement_factor * parcel_acreage) |>
  filter(development_flag != "Not Redevelopable") |>
  group_by(county_name, development_flag) |>
  summarise(acres = round(sum(adjusted_acreage), final_dec)) |>
  as_tibble() |>
  pivot_wider(names_from = development_flag, values_from = acres) |>
  select(County = "county_name", "Vacant", "Redevelopable") |>
  mutate(Total = Vacant + Redevelopable)

region <- il_acres_centroids |> 
  mutate(County = "Region") |>
  group_by(County) |>
  summarise(Vacant = sum(Vacant), Redevelopable = sum(Redevelopable), Total = sum(Total)) |>
  as_tibble()

il_acres_centroids <- bind_rows(il_acres_centroids, region)
saveRDS(il_acres_centroids, "data/ili_summary_centroids_not_trimmed.rds")
rm(region)

il_acres_centroids_adj <- p_point_poly_trim |> 
  st_drop_geometry() |>
  mutate(adjusted_acreage = adjustement_factor * parcel_acreage) |>
  filter(development_flag != "Not Redevelopable") |>
  group_by(county_name, development_flag) |>
  summarise(acres = round(sum(adjusted_acreage), final_dec)) |>
  as_tibble() |>
  pivot_wider(names_from = development_flag, values_from = acres) |>
  select(County = "county_name", "Vacant", "Redevelopable") |>
  mutate(Total = Vacant + Redevelopable)

region <- il_acres_centroids_adj |> 
  mutate(County = "Region") |>
  group_by(County) |>
  summarise(Vacant = sum(Vacant), Redevelopable = sum(Redevelopable), Total = sum(Total)) |>
  as_tibble()

il_acres_centroids_adj <- bind_rows(il_acres_centroids_adj, region)
saveRDS(il_acres_centroids_adj, "data/ili_summary_centroids_trimmed.rds")
rm(region)

il_acres_polygons <- p_poly |> 
  st_drop_geometry() |>
  mutate(adjusted_acreage = adjustement_factor * parcel_acreage) |>
  filter(development_flag != "Not Redevelopable") |>
  group_by(county_name, development_flag) |>
  summarise(acres = round(sum(adjusted_acreage), final_dec)) |>
  as_tibble() |>
  pivot_wider(names_from = development_flag, values_from = acres) |>
  select(County = "county_name", "Vacant", "Redevelopable") |>
  mutate(Total = Vacant + Redevelopable)

region <- il_acres_polygons |> 
  mutate(County = "Region") |>
  group_by(County) |>
  summarise(Vacant = sum(Vacant), Redevelopable = sum(Redevelopable), Total = sum(Total)) |>
  as_tibble()

il_acres_polygons <- bind_rows(il_acres_polygons, region)
saveRDS(il_acres_polygons, "data/ili_summary_polygons.rds")
rm(region)

il_acres_polygons_mic <- p_poly |> 
  st_drop_geometry() |>
  mutate(adjusted_acreage = adjustement_factor * parcel_acreage) |>
  filter(development_flag != "Not Redevelopable") |>
  group_by(mic, development_flag) |>
  summarise(acres = round(sum(adjusted_acreage), final_dec)) |>
  as_tibble() |>
  pivot_wider(names_from = development_flag, values_from = acres) |>
  select(MIC = "mic", "Vacant", "Redevelopable") |>
  mutate(Total = Vacant + Redevelopable)

region <- il_acres_polygons_mic |> 
  mutate(MIC = "Region") |>
  group_by(MIC) |>
  summarise(Vacant = sum(Vacant), Redevelopable = sum(Redevelopable), Total = sum(Total)) |>
  as_tibble()

il_acres_polygons_mic <- bind_rows(il_acres_polygons_mic, region)
saveRDS(il_acres_polygons_mic, "data/ili_summary_polygons_mic.rds")
rm(region)

il_acres_polygons_category <- p_poly |> 
  st_drop_geometry() |>
  mutate(adjusted_acreage = adjustement_factor * parcel_acreage) |>
  filter(development_flag != "Not Redevelopable") |>
  group_by(category, development_flag) |>
  summarise(acres = round(sum(adjusted_acreage), final_dec)) |>
  as_tibble() |>
  pivot_wider(names_from = development_flag, values_from = acres) |>
  select(Category = "category", "Vacant", "Redevelopable") |>
  mutate(Total = Vacant + Redevelopable)

region <- il_acres_polygons_category |> 
  mutate(Category = "Region") |>
  group_by(Category) |>
  summarise(Vacant = sum(Vacant), Redevelopable = sum(Redevelopable), Total = sum(Total)) |>
  as_tibble()

il_acres_polygons_category <- bind_rows(il_acres_polygons_category, region)
saveRDS(il_acres_polygons_category, "data/ili_summary_polygons_category.rds")
rm(region)
