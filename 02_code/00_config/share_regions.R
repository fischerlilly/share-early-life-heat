# Canonical SHARE-region definitions used by exposure construction and maps.
# Official NUTS geometries provide the component boundaries; components listed
# in the same row are dissolved before any E-OBS grid intersections are made.

italy_share_regions <- data.table::data.table(
  share_code = sprintf("R%02d", 1:20),
  birth_region_code = 1601:1620,
  weather_region_id = c(
    "ITF1", "ITF5", "ITF6", "ITF3", "ITH5", "ITH4", "ITI4", "ITC3",
    "ITC4", "ITI3", "ITF2", "ITC1", "ITF4", "ITG2", "ITG1", "ITI1",
    "IT_TR_AA", "ITI2", "ITC2", "ITH3"
  ),
  region_name = c(
    "Abruzzo", "Basilicata", "Calabria", "Campania", "Emilia-Romagna",
    "Friuli-Venezia Giulia", "Lazio", "Liguria", "Lombardy", "Marche",
    "Molise", "Piedmont", "Apulia", "Sardinia", "Sicily", "Tuscany",
    "Trentino-Alto Adige", "Umbria", "Aosta Valley", "Veneto"
  ),
  short_label = c(
    "Abruzzo", "Basilicata", "Calabria", "Campania", "Emilia-Romagna",
    "Friuli-V.G.", "Lazio", "Liguria", "Lombardy", "Marche",
    "Molise", "Piedmont", "Apulia", "Sardinia", "Sicily", "Tuscany",
    "Trentino-A.A.", "Umbria", "Aosta V.", "Veneto"
  ),
  nuts_ids = c(
    "ITF1", "ITF5", "ITF6", "ITF3", "ITH5", "ITH4", "ITI4", "ITC3",
    "ITC4", "ITI3", "ITF2", "ITC1", "ITF4", "ITG2", "ITG1", "ITI1",
    "ITH1|ITH2", "ITI2", "ITC2", "ITH3"
  )
)

croatia_share_regions <- data.table::data.table(
  share_code = sprintf("R%02d", 1:20),
  early_life_region_code = 4701:4720,
  weather_region_id = c(
    "HR_ZAGREB", "HR064", "HR028", "HR027", "HR062", "HR063", "HR021",
    "HR031", "HR032", "HR022", "HR023", "HR024", "HR033", "HR025",
    "HR034", "HR026", "HR035", "HR036", "HR037", "HR061"
  ),
  region_name = c(
    "Zagreb County and Zagreb", "Krapina-Zagorje", "Sisak-Moslavina",
    "Karlovac", "Varazdin", "Koprivnica-Krizevci", "Bjelovar-Bilogora",
    "Primorje-Gorski Kotar", "Lika-Senj", "Virovitica-Podravina",
    "Pozega-Slavonia", "Brod-Posavina", "Zadar", "Osijek-Baranja",
    "Sibenik-Knin", "Vukovar-Srijem", "Split-Dalmatia", "Istria",
    "Dubrovnik-Neretva", "Medimurje"
  ),
  short_label = c(
    "Zagreb", "Krapina-Zag.", "Sisak-Moslav.", "Karlovac", "Varazdin",
    "Koprivnica-Kr.", "Bjelovar-Bil.", "Primorje-G.K.", "Lika-Senj",
    "Virovitica-Pod.", "Pozega-Slav.", "Brod-Pos.", "Zadar",
    "Osijek-Bar.", "Sibenik-Knin", "Vukovar-Sr.", "Split-Dalm.",
    "Istria", "Dubrovnik-Ner.", "Medimurje"
  ),
  nuts_ids = c(
    "HR050|HR065", "HR064", "HR028", "HR027", "HR062", "HR063", "HR021",
    "HR031", "HR032", "HR022", "HR023", "HR024", "HR033", "HR025",
    "HR034", "HR026", "HR035", "HR036", "HR037", "HR061"
  )
)

build_share_region_geometries <- function(country, nuts_file, crs = 3035) {
  country <- match.arg(tolower(country), c("italy", "croatia"))
  definition <- if (country == "italy") italy_share_regions else croatia_share_regions
  country_code <- if (country == "italy") "IT" else "HR"
  nuts_level <- if (country == "italy") 2L else 3L

  if (!file.exists(nuts_file)) stop("NUTS geometry file not found: ", nuts_file)
  if (nrow(definition) != 20L || anyDuplicated(definition$share_code) ||
      anyDuplicated(definition$weather_region_id)) {
    stop("The SHARE-region definition must contain 20 unique region identifiers.")
  }

  lookup <- data.table::rbindlist(lapply(seq_len(nrow(definition)), function(i) {
    data.table::data.table(
      NUTS_ID = strsplit(definition$nuts_ids[i], "|", fixed = TRUE)[[1]],
      share_code = definition$share_code[i]
    )
  }))
  if (anyDuplicated(lookup$NUTS_ID)) stop("A NUTS component is assigned more than once.")

  nuts <- sf::st_read(nuts_file, quiet = TRUE)
  nuts <- nuts[nuts$CNTR_CODE == country_code & nuts$LEVL_CODE == nuts_level, ]
  missing_components <- setdiff(lookup$NUTS_ID, nuts$NUTS_ID)
  if (length(missing_components)) {
    stop("NUTS components missing from the boundary file: ",
         paste(missing_components, collapse = ", "))
  }

  nuts <- merge(nuts[, "NUTS_ID"], lookup, by = "NUTS_ID", all.x = FALSE)
  nuts <- sf::st_make_valid(sf::st_transform(nuts, crs))
  regions <- nuts |>
    dplyr::group_by(share_code) |>
    dplyr::summarise(.groups = "drop")
  regions <- merge(regions, definition, by = "share_code", all.x = TRUE)
  regions <- sf::st_make_valid(regions)
  regions <- regions[order(regions$share_code), ]

  if (nrow(regions) != 20L || any(sf::st_is_empty(regions)) ||
      any(!sf::st_is_valid(regions)) || anyNA(regions$weather_region_id)) {
    stop("Failed to construct 20 complete and valid SHARE-region geometries.")
  }
  regions
}
