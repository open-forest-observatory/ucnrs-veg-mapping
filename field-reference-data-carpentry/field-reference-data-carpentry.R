# Purpose: Process and compile the raw entered field data (field observations datasheets, Emlid RTK
# base and rover position data, Emlid RTK base corrections) into analysis-ready geospatial data
# frames

library(sf)
library(tidyverse)
library(readxl)
library(here)

# Set data directory. For now assuming it's in the root of this repo and named 'ucnrs-data'. Note
# this will not work if run from the command line because it depends on here::here() detecting the
# current opened folder in the IDE.
datadir = here("ucnrs-data")

# --- Load data ---

# Field survey observations
survey_obs = read_excel(file.path(datadir,
                                  "field-reference", "unprocessed",
                                  "field-reference-data-entry-2023.xlsx"),
                        sheet = "points") |>
    select(reserve, polygon_id = polygon_id, point_id, everything())

# GCP point ID and description
gcps = read_excel(file.path(datadir,
                            "field-reference", "unprocessed",
                            "field-reference-data-entry-2023.xlsx"),
                  sheet = "polygons") |>
    select(reserve, polygon_id = polygon, gcp)

# Base station corrections
base = read_csv(file.path(datadir,
                          "field-reference", "unprocessed",
                          "base-corrections.csv")) |>
    select(polygon_id, lat_uncorr, lon_uncorr, lat_corr, lon_corr)

# Field survey point locations (recorded using Emlids)
locs1 = read_csv(file.path(datadir,
                          "field-reference", "unprocessed",
                          "emlid-points",
                          "BORR.csv")) |>
    mutate(reserve = "B")

locs2 = read_csv(file.path(datadir,
                          "field-reference", "unprocessed",
                          "emlid-points",
                          "Hastings.csv")) |>
    mutate(reserve = "H")

locs3 = read_csv(file.path(datadir,
                          "field-reference", "unprocessed",
                          "emlid-points",
                          "quail.csv")) |>
    mutate(reserve = "Q")

survey_locs = bind_rows(locs1, locs2, locs3) |>
    select(point_id = Name, reserve,
           lon = Longitude, lat = Latitude,
           position_solution = "Solution status",
           position_samples = Samples,
           lon_rms = "Easting RMS",
           lat_rms = "Northing RMS")


# --- Get the x,y shifts implied by the base station location corrections, to apply to field plot
# locs and GCP locs ---

# Get base station locs as sf object and project to EPSG 3310 so it's in meters
base_uncorr = st_as_sf(base, coords = c("lon_uncorr", "lat_uncorr"), crs = 4326) |>
    st_transform(3310)

coords_uncorr = st_coordinates(base_uncorr)

base_corr = st_as_sf(base, coords = c("lon_corr", "lat_corr"), crs = 4326) |>
    st_transform(3310)

coords_corr = st_coordinates(base_corr)

base$shift_x = coords_corr[, 1] - coords_uncorr[, 1]
base$shift_y = coords_corr[, 2] - coords_uncorr[, 2]


# --- Standardize the naming of the relational columns so we can merge tables ---

survey_obs = survey_obs |>
    mutate(across(c(reserve, polygon_id), toupper))

gcps = gcps |>
    mutate(across(c(reserve, polygon_id), toupper))

base = base |>
    mutate(across(c(polygon_id), toupper))

survey_locs = survey_locs |>
    mutate(across(c(reserve), toupper))

survey_obs[survey_obs$polygon_id == "BO BURN 001, 002, 003, 004, 005", "polygon_id"] =
    "BO BURN 001-005"
survey_obs[survey_obs$polygon_id == "BO BURN GO1, GO4, GO5, UNBURN GO1, GO4, GO5", "polygon_id"] =
    "BO BURN G01, G04, G05, UNBURN G04, G05"
survey_obs[survey_obs$polygon_id == "BURN UNBURN GRASS 2 GRASS 3", "polygon_id"] =
    "BURN UNBURN GRASS 2 & 3"


# --- Prepare the survey point location data ---

# Merge point coords into point obs
survey_w_locs = left_join(survey_obs, survey_locs, by = c("point_id", "reserve"))

# Pull in the coordinate correction shifts into the survey point locations based on the polygon ID
base_foc = base |>
    select(polygon_id, shift_x, shift_y)

survey_w_locs = left_join(survey_w_locs, base_foc, by = "polygon_id")

# --- Shift the survey point locations by the correction shifts ---
survey_w_locs_sf = st_as_sf(survey_w_locs, coords = c("lon", "lat"), crs = 4326) |>
  st_transform(3310)

coords = st_coordinates(survey_w_locs_sf)
survey_w_locs$x_uncorr = coords[, 1]
survey_w_locs$y_uncorr = coords[, 2]

survey_w_locs$x_corr = survey_w_locs$x_uncorr + survey_w_locs$shift_x
survey_w_locs$y_corr = survey_w_locs$y_uncorr + survey_w_locs$shift_y

survey_w_locs = survey_w_locs |>
  select(-lon, -lat, -shift_x, -shift_y, -x_uncorr, -y_uncorr) |>
  st_as_sf(coords = c("x_corr", "y_corr"), crs = 3310)

write_filepath = file.path(datadir, "field-reference", "for-analysis", "survey.gpkg")
dir.create(dirname(write_filepath))

st_write(survey_w_locs, write_filepath,
         delete_dsn = TRUE)


# --- Prepare the GCP location data ---

# Split the 'gcp' field into an Emlid point ID and a description (currently they're combined)
gcps = gcps |>
    mutate(point_id = str_extract(gcp, pattern = "(#[0-9]+, )|(#[0-9]+ )"),
           point_id = str_remove_all(point_id, pattern = "#|,| "),
           point_id = as.numeric(point_id),
           gcp_descr = str_remove_all(gcp, pattern = "#[0-9]+, |#[0-9]+ ")) |>
    select(-gcp)

# Merge GCP descriptions wtih GCP locs
gcps_w_locs = left_join(gcps, survey_locs, by = c("point_id", "reserve"))

# Convert coords to meters (EPSG 3310)
gcps_w_locs_sf = st_as_sf(gcps_w_locs, coords = c("lon", "lat"), crs = 4326) |>
    st_transform(3310)
coords = st_coordinates(gcps_w_locs_sf)
gcps_w_locs$x_uncorr = coords[, 1]
gcps_w_locs$y_uncorr = coords[, 2]

# Pull in the shifts learned from the base corrections
gcps_w_locs = left_join(gcps_w_locs, base_foc, by = "polygon_id")

# Apply the shifts
gcps_w_locs$x_corr = gcps_w_locs$x_uncorr + gcps_w_locs$shift_x
gcps_w_locs$y_corr = gcps_w_locs$y_uncorr + gcps_w_locs$shift_y

# Drop intermediate columns, convert to spatial, and write
gcps_w_locs_corr = gcps_w_locs |>
  select(-lon, -lat, -shift_x, -shift_y, -x_uncorr, -y_uncorr) |>
  st_as_sf(coords = c("x_corr", "y_corr"), crs = 3310)

gcps_w_locs_corr = st_as_sf(gcps_w_locs_corr, coords = c("x_corr", "y_corr"), crs = 3310)

write_filepath = file.path(datadir, "field-reference", "for-analysis", "gcps.gpkg")
dir.create(dirname(write_filepath))

st_write(gcps_w_locs_corr, write_filepath,
         delete_dsn = TRUE)
