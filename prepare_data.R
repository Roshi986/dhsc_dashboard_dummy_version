# Μap data
if ("ltla.geojson" %in% list.files()) {
  ltla <- select(st_read("ltla.geojson"), -area_name) %>%
    mutate_if(is.factor, as.character)
} else {
  # https://geoportal.statistics.gov.uk/datasets/counties-and-unitary-authorities-december-2019-boundaries-uk-buc/data
  st_read("https://opendata.arcgis.com/datasets/b216b4c8a4e74f6fb692a1785255d777_0.geojson") %>% 
    select(area_code = ctyua19cd, area_name = ctyua19nm, long, lat, st_areashape) %>% 
    filter(str_detect(area_code, "^E")) %>% 
    # dissolve polygons for Hackney and City of London / Cornwall and Isles of Scilly 
    mutate(
      area_code = as.character(area_code),
      area_name = as.character(area_name),
      area_name = 
        case_when(
          area_name %in% c("Cornwall", "Isles of Scilly") ~ "Cornwall and Isles of Scilly",
          area_name %in% c("City of London", "Hackney") ~ "Hackney and City of London", 
          TRUE ~ area_name
        ),
      area_code = 
        case_when(
          area_name == "Cornwall and Isles of Scilly" ~ "E06000052",
          area_name == "Hackney and City of London" ~ "E09000012",
          area_code == "E10000002" ~ "E06000060", # https://data.gov.uk/dataset/7f1e8c5a-7e8c-4b88-bec2-d070b8b7bdf7/local-authority-district-to-county-april-2020-lookup-in-england
          TRUE ~ area_code
        ),
      long = 
        case_when(
          area_name == "Cornwall and Isles of Scilly" ~ -4.64249,
          area_name == "Hackney and City of London" ~ -0.06045,
          TRUE ~ long
        ),
      lat = 
        case_when(
          area_name == "Cornwall and Isles of Scilly" ~ 50.45023,
          area_name == "Hackney and City of London" ~ 51.55492,
          TRUE ~ lat
        )
    ) %>% 
    group_by(area_name, area_code, long, lat) %>% 
    summarise(st_areashape = sum(st_areashape)) %>% 
    select(-st_areashape) %>% 
    st_write("ltla.geojson", delete_dsn = TRUE)
  
  ltla <- select(st_read("ltla.geojson"), -area_name) %>%
    mutate_if(is.factor, as.character)
}

# Population data
current_year <- as.numeric(substr(Sys.Date(), 1, 4))
data_name <- paste0("ukmidyearestimates", 
  current_year - 1, current_year, "ladcodes.xls")

if (data_name %in% list.files()) {
  pop_file <- "ukmidyearestimates20192020ladcodes.xls"
} else {
  data_link <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid2019april2020localauthoritydistrictcodes/"
  pop_file <- tempfile()
  GET(paste0(data_name, data_link), write_disk(pop_file))
  readLines(pop_file)
}

# Reactive table data
table_data <- read_excel(pop_file, sheet = "MYE2 - Persons", skip = 4) %>%
  clean_names %>%
  rename(
    area_name = name, 
    area_code = code, 
    geography = geography1
  ) %>%
  filter(area_code %in% ltla$area_code) %>%
  melt %>%
  rename(age = variable, count = value) %>%
  filter(str_length(area_code) == 9) %>%
  mutate(age = as.character(age)) %>%
  mutate_at(
    'age', 
    ~ case_when(
      grepl('all_ages', .) ~ .,
      TRUE ~ sub('x', '', .)
    )
  ) %>%
  # The ltla data merge Hackney and City of London; and Crownall and Isles of Scilly. Do the same here
  mutate(
    area_name = 
      case_when(
        grepl('london', area_name, ignore.case = TRUE) ~ 
          'Hackney and City of London',
        grepl('hackney', area_name, ignore.case = TRUE) ~ 
          'Hackney and City of London',
        grepl('scilly', area_name, ignore.case = TRUE) ~ 
          'Cornwall and Isles of Scilly',
        grepl('cornwall', area_name, ignore.case = TRUE) ~ 
          'Cornwall and Isles of Scilly',
        TRUE ~ area_name
      ),
    area_code = 
      case_when(
        grepl('london', area_name, ignore.case = TRUE) ~ 
          'E09000012',
        grepl('hackney', area_name, ignore.case = TRUE) ~ 
          'E09000012',
        grepl('scilly', area_name, ignore.case = TRUE) ~ 
          'E06000052',
        grepl('cornwall', area_name, ignore.case = TRUE) ~ 
          'E06000052',
        TRUE ~ area_code
      )
  ) %>%
  select(-geography) %>%
  # We need the grouping and the sum here to merge data from the newely-define areas 'Hackney and City of London' and 'Cornwall and Isles of Scilly'
  group_by(area_code, age) %>%
  mutate(count = sum(count)) %>%
  ungroup %>%
  distinct %>%
  mutate_at('age',
    .funs = ~
      case_when(
        . == 'all_ages' ~ 'all_ages',
        . %in% 0:9 ~ '0-9',
        . %in% 10:19 ~ '10-19',
        . %in% 20:29 ~ '20-29',
        . %in% 30:39 ~ '30-39',
        . %in% 40:49 ~ '40-49',
        . %in% 50:59 ~ '50-59',
        . %in% 60:69 ~ '60-69',
        . %in% 70:79 ~ '70-79',
        TRUE ~ '80+',
      )
  ) %>%
  # We need the grouping and the sum here to merge data from different ages to their corresponding age-range categories that we have defined
  group_by(area_code, age) %>%
  mutate(count = sum(count)) %>%
  distinct %>%
  ungroup %>%
  mutate_if(is.factor, as.character) %>%
  spread(age, count) %>%
  mutate(all_ages_prop = round(all_ages / sum(all_ages) * 100, 2)) %>%
  select(area_code, area_name, all_ages, all_ages_prop, everything())

# Data for reactive table and map
sf <- ltla %>% 
  left_join(table_data, by = c("area_code")) %>% 
  # Tooltip data
  mutate(
    popup = 
      str_c(
        area_name, "<br/>",
        #
        "Population: ",
        format(all_ages, big.mark=","),
        "<br/>",
        #
        "% England population: ",
        all_ages_prop,
        "<br/>"
      ) %>% 
      map(HTML)
  )

# Colour gradient for area populations. Will be used to colour in the areas on the map
pal <- colorRampPalette(c('orange', 'blue'))
sf <- sf %>%
  as_tibble %>%
  select(all_ages) %>%
  distinct %>%
  arrange_all %>%
  #mutate(all_ages_colour = c(pal(nrow(.) - 1), "#808080")) %>%
  mutate(all_ages_colour = pal(nrow(.))) %>%
  right_join(sf, by = 'all_ages') %>%
  select(3:ncol(.), all_ages_colour, all_ages) %>%
  relocate(all_ages_prop, .after = last_col()) %>%
  st_as_sf # Convert back to 'simple feature' for the leaflet package to be able to read it to make the map in the main script

# Population pyramid data
ons_pop <- read_excel(pop_file, sheet = "MYE2 - Males", skip = 4) %>%
  mutate(gender = 'male') %>%
  bind_rows(
    read_excel(pop_file, sheet = "MYE2 - Females", skip = 4) %>%
      mutate(gender = 'female')
  ) %>%
  clean_names %>%
  rename(
    area_name = name, 
    area_code = code, 
    geography = geography1
  ) %>%
  select(-all_ages)  %>%
  filter(area_code %in% ltla$area_code) %>%
  melt %>%
  rename(age = variable, count = value) %>%
  filter(str_length(area_code) == 9) %>%
  mutate(age = as.integer(sub('x', '', age))) %>%
  # The ltla data merge Hackney and City of London; and Crownall and Isles of Scilly. Do the same here
  mutate(
    area_name = 
      case_when(
        grepl('london', area_name, ignore.case = TRUE) ~ 
          'Hackney and City of London',
        grepl('hackney', area_name, ignore.case = TRUE) ~ 
          'Hackney and City of London',
        grepl('scilly', area_name, ignore.case = TRUE) ~ 
          'Cornwall and Isles of Scilly',
        grepl('cornwall', area_name, ignore.case = TRUE) ~ 
          'Cornwall and Isles of Scilly',
        TRUE ~ area_name
      ),
    area_code = 
      case_when(
        grepl('london', area_name, ignore.case = TRUE) ~ 
          'E09000012',
        grepl('hackney', area_name, ignore.case = TRUE) ~ 
          'E09000012',
        grepl('scilly', area_name, ignore.case = TRUE) ~ 
          'E06000052',
        grepl('cornwall', area_name, ignore.case = TRUE) ~ 
          'E06000052',
        TRUE ~ area_code
      )
  ) %>%
  select(-geography) %>%
  # We need the grouping and the sum here to merge data from the newely-define areas 'Hackney and City of London' and 'Cornwall and Isles of Scilly'
  group_by(area_code, gender, age) %>%
  mutate(count = sum(count)) %>%
  ungroup %>%
  distinct %>%
  mutate_at('age',
    .funs = ~
      case_when(
        . %in% 0:9 ~ '0-9',
        . %in% 10:19 ~ '10-19',
        . %in% 20:29 ~ '20-29',
        . %in% 30:39 ~ '30-39',
        . %in% 40:49 ~ '40-49',
        . %in% 50:59 ~ '50-59',
        . %in% 60:69 ~ '60-69',
        . %in% 70:79 ~ '70-79',
        TRUE ~ '80+',
      )
  ) %>%
  # We need the grouping and the sum here to merge data from different ages to their corresponding age-range categories that we have defined
  group_by(area_code, gender, age) %>%
  mutate(count = sum(count)) %>%
  distinct %>%
  # Express age and gender counts as a proportion of the population in the local authority
  group_by(area_code) %>%
  mutate(prop = count / sum(count) * 100) %>%
  ungroup %>%
  mutate_if(is.factor, as.character)
