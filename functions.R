# Maps --------------------------------------------------------------------

create_map <- function(il_lyr, ex_lyr=NULL, ex_nm="Exclusion Layer") {
  
  m <- leaflet() |>
    
    addProviderTiles(providers$CartoDB.Positron) |>
    
    addLayersControl(baseGroups = c("Base Map"),
                     overlayGroups = c("Industrial Lands", ex_nm),
                     options = layersControlOptions(collapsed = TRUE)) |>
    
    addPolygons(data = il_lyr,
                fillColor = "#EB4584",
                weight = 1,
                opacity = 1.0,
                color = "#EB4584",
                dashArray = "1",
                fillOpacity = 1.0,
                group="Industrial Lands")
  
  if (!(is.null(ex_lyr))) {
     
   m <- m |>
     
     addPolygons(data = ex_lyr,
                 fillColor = "#00A7A0",
                 weight = 1,
                 opacity = 1.0,
                 color = "#00A7A0",
                 dashArray = "1",
                 fillOpacity = 1.0,
                 group=ex_nm) 
     
  }
  
  # Add in Region Button
  m <- m |>
    
    addEasyButton(easyButton(
      icon="fa-globe", title="Region",
      onClick=JS("function(btn, map){map.setView([47.615,-122.257],8.5); }"))) |>
    
    setView(-122.257, 47.615, zoom = 8.5)
  
  return(m)
  
}

# Data Tables -------------------------------------------------------------

create_summary_df <- function(lyr) {
  
  tbl_by_county <- lyr |>
    st_drop_geometry() |>
    group_by(county_name) |>
    summarise(acres = round(sum(total_acreage), 0)) |>
    as_tibble()
  
  region <- tbl_by_county |> 
    mutate(county_name = "Region") |>
    group_by(county_name) |>
    summarise(acres = round(sum(acres), 0)) |>
    as_tibble()
  
  tbl_by_county <- bind_rows(tbl_by_county, region) |> 
    mutate(county_name = factor(county_name, levels=county_ord)) |> 
    arrange(county_name) |>
    rename(County = "county_name", `Acres` = "acres")
  
  return(tbl_by_county)
  
}

# Pretty Tables for Presentation ------------------------------------------

create_html_table <- function(tbl){
  
  final_tbl <- datatable(tbl,
                         #container = summary_container,
                         #colnames = c('Group', rep(c('Estimate', 'Share'), num_years)),
                         options = list(pageLength = 15,
                                        dom = 'rt',
                                        #buttons = c('csv', 'excel'),
                                        columnDefs = list(list(className = 'dt-center', targets=1:1))
                         ),
                         #extensions = 'Buttons',
                         filter = 'none',
                         rownames = FALSE) |>
    formatCurrency("Acres", "", digits = 0)
  
  return(final_tbl)
  
}

create_final_html_table <- function(tbl, num_cols = numeric_columns, fs="10pt"){
  
  final_tbl <- datatable(tbl,
                         options = list(pageLength = 15,
                                        initComplete = htmlwidgets::JS(
                                          "function(settings, json) {",
                                          paste0("$(this.api().table().container()).css({'font-size': '", fs, "'});"),
                                          "}"),
                                        dom = 'rt',
                                        columnDefs = list(list(className = 'dt-center', targets=1:8))
                         ),
                         filter = 'none',
                         rownames = FALSE) |>
    formatCurrency(num_cols, "", digits = 0)
  
  return(final_tbl)
  
}

# Parcel Summarization ----------------------------------------------------
industrial_parcels_by_county <- function(p_file, p_thresh, p_county, p_id, il_lyr = industrial_lands_gross_acreage) {
  
  parcel_file <- file.path("data/county-files", p_file)
  parcel_threshold <- p_thresh
  
  parcels <- st_read(parcel_file) |> 
    st_make_valid() |> 
    st_transform(crs = spn)
  
  parcels <- parcels |> 
    mutate(parcel_acreage = as.numeric(set_units(st_area(parcels), "acre")))
  
  # Use Parcel Points for City, Regional Geography and MIC assignment
  parcel_points <- st_centroid(parcels) |> select(all_of(p_id))
  
  # Add MIC name
  p <- st_intersection(parcel_points, mic) |> select(all_of(p_id), mic) |> st_drop_geometry()
  parcels <- left_join(parcels, p, by = p_id) |> mutate(mic = replace_na(mic, "Not in a MIC"))
  
  # Add Rgeo name
  p <- st_intersection(parcel_points, rgeo) |> select(all_of(p_id), rgeo) |> st_drop_geometry()
  parcels <- left_join(parcels, p, by = p_id) |> mutate(rgeo = replace_na(rgeo, "Rural"))
  
  # Add City name
  p <- st_intersection(parcel_points, city) |> select(all_of(p_id), city) |> st_drop_geometry()
  parcels <- left_join(parcels, p, by = p_id) |> mutate(city = replace_na(city, "Unincorporated"))
  
  industrial_zones <- il_lyr |> 
    filter(county_name == p_county) |> 
    st_transform(crs = spn) |>
    select("county_name") |>
    st_union() |>
    st_sf() |>
    mutate(county_name = p_county) 
  
  industrial <- st_intersection(parcels, industrial_zones) |>
    select(parcel_id = all_of(p_id), "parcel_acreage", "county_name", "mic", "rgeo", "city")
  
  industrial_parcels <- industrial |> 
    mutate(total_acreage = as.numeric(set_units(st_area(industrial), "acre"))) |>
    mutate(parcel_share = total_acreage/parcel_acreage) |>
    filter(total_acreage >= parcel_threshold) |>
    st_drop_geometry() |>
    select("parcel_id", "county_name", "total_acreage", "parcel_share", "mic", "rgeo", "city") |>
    mutate(county_id = case_when(
      county_name == "King" ~ 33,
      county_name == "Kitsap" ~ 35,
      county_name == "Pierce" ~ 53,
      county_name == "Snohomish" ~ 61)) |> 
    mutate(parcel_integer = as.numeric(parcel_id)) |>
    drop_na() |>
    mutate(parcel_fips = paste0(county_id, parcel_integer)) |>
    select("parcel_fips", "total_acreage", "parcel_share", "mic", "rgeo", "city")
  
  return(industrial_parcels)
  
}

parcels_by_county <- function(p_file, p_county, p_id) {
  
  parcel_file <- file.path("data/county-files", p_file)
  
  # Read in Parcel file
  print(str_glue("Opening {p_file} parcel file."))
  parcels <- st_read(parcel_file) |> 
    st_make_valid() |> 
    st_transform(crs = spn)
  
  # Calculate acreage for parcels
  print(str_glue("Calculating parcel acreage for {p_file}."))
  parcels <- parcels |> mutate(parcel_acreage = as.numeric(set_units(st_area(parcels), "acre")))
  
  # Clean up Parcel ID using County ID to ensure unique values when counties are combined
  print(str_glue("Creating unique parcel_fips_id for {p_file} to use in joins."))
  parcels <- parcels |>
    select(parcel_id_fips = all_of(p_id), "parcel_acreage") |>
    mutate(county_id = p_county, parcel_integer = as.numeric(parcel_id_fips)) |>
    drop_na() |>
    mutate(parcel_id_fips = as.numeric(paste0(county_id, parcel_integer))) |>
    mutate(county_name = case_when(
      county_id == 33 ~ "King",
      county_id == 35 ~ "Kitsap",
      county_id == 53 ~ "Pierce",
      county_id == 61 ~ "Snohomish")) |>
    select(-"parcel_integer")
  
  # Use Parcel Points for City, Regional Geography and MIC assignment
  print(str_glue("Creating centroid file for {p_file} to join names to."))
  parcel_points <- st_centroid(parcels) |> select("parcel_id_fips")
  
  # Add MIC name
  print(str_glue("Adding MIC names to {p_file}."))
  p <- st_intersection(parcel_points, mic) |> select("parcel_id_fips", mic) |> st_drop_geometry()
  parcels <- left_join(parcels, p, by = c("parcel_id_fips")) |> mutate(mic = replace_na(mic, "Not in a MIC"))
  
  # Add Rgeo name
  print(str_glue("Adding Regional Geographies to {p_file}."))
  p <- st_intersection(parcel_points, rgeo) |> select("parcel_id_fips", rgeo) |> st_drop_geometry()
  parcels <- left_join(parcels, p, by = c("parcel_id_fips")) |> mutate(rgeo = replace_na(rgeo, "Rural"))
  
  # Add City name
  print(str_glue("Adding City names to {p_file}."))
  p <- st_intersection(parcel_points, city) |> select("parcel_id_fips", city) |> st_drop_geometry()
  parcels <- left_join(parcels, p, by = c("parcel_id_fips")) |> mutate(city = replace_na(city, "Unincorporated"))
  
  # Remove any duplicate parcel ids 
  print(str_glue("Removing any duplicate parcel ids from {p_file}."))
  parcels <- parcels |> distinct(parcel_id_fips, .keep_all = TRUE)
  
  return(parcels)
  
}

add_parcel_data <- function(p_file=region_parcels, p_data=parcel_data_file, p_buildings=parcel_building_file) {
  
  data_file <- file.path("data", p_data)
  buildings_file <- file.path("data", p_buildings)
  
  # Get Land Use Code and Parcel SQFT
  print(str_glue("Getting Parcel Use Code and Size from UrbanSim Parcel Data."))
  d <- read_tsv(data_file) |> 
    mutate(parcel_integer = as.numeric(parcel_id_fips)) |>
    drop_na() |>
    mutate(parcel_id_fips = as.numeric(paste0(county_id, parcel_integer))) |>
    select("parcel_id_fips", "land_use_type_id", "gross_sqft")
  
  # Get Improvement Value by Parcel
  print(str_glue("Getting Parcel Improvement values from UrbanSim Buildings Data."))
  b <- read_tsv(buildings_file) |>
    mutate(parcel_integer = as.numeric(parcel_id_fips)) |>
    drop_na() |>
    mutate(parcel_id_fips = as.numeric(paste0(county_id, parcel_integer))) |>
    mutate(improvement_value = as.integer(improvement_value)) |>
    mutate(improvement_value = replace_na(improvement_value, 0)) |>
    group_by(parcel_id_fips) |>
    summarise(improvement_value = sum(improvement_value)) |>
    as_tibble()
  
  # Combine Land Use Code, Size and Improvement Value
  print(str_glue("Combining Use Code, Size and Improvment Values for Parcels."))
  parcels <- left_join(d, b, by="parcel_id_fips") |> 
    mutate(improvement_value = replace_na(improvement_value, 0))
  
  # Add Parcel Data to Combined Region Parcels file
  print(str_glue("Adding parcel data to regional parcel file and cleaning up"))
  p <- left_join(p_file, parcels, by="parcel_id_fips") |>
    drop_na() |>
    mutate(gross_sqft = as.numeric(gross_sqft)) |>
    mutate(impval = improvement_value / gross_sqft) |>
    mutate(development_flag = case_when(
      impval < vacant_threshold ~ "Vacant",
      impval < redevel_threshold ~ "Redevelopable",
      impval >= redevel_threshold ~ "Not Redevelopable")) |>
    mutate(urban = case_when(
      rgeo == "Metro" & city != "Bremerton" ~ "Y",
      rgeo == "Metro" & city == "Bremerton" ~ "N")) |>
    mutate(urban = replace_na(urban, "N")) |>
    mutate(adjustement_factor = case_when(
      urban == "Y" ~ urban_deduction,
      urban == "N" ~ suburban_deduction)) |>
    mutate(land_use_type_id = as.integer(land_use_type_id)) |>
    mutate(land_use_type_id = replace_na(land_use_type_id, 0)) |>
    mutate(use_code = case_when(
      mic != "Not in a MIC" & land_use_type_id == 6 ~ "Y",
      mic == "Not in a MIC" & land_use_type_id == 6 ~ "N",
      !(land_use_type_id %in% c(0, 2, 6, 7, 8, 19, 22, 23, 29)) ~ "Y",
      land_use_type_id %in% c(0, 2, 6, 7, 8, 19, 22, 23, 29) ~ "N")) |>
    filter(use_code == "Y")
  
  return(p)  
  
}
