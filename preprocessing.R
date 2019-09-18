# download fender_2017_lk with the template "Eisenzeit" from the linked database
# as a .csv file. then do the preprocessing

dta <- read_csv(
  "Daten/fender_2017_lk.csv",
  col_types = cols(
    `Corine-Code` = col_character(),
    `HÃ¶he_SRTM1_puffer50m` = col_character(),
    wkb_geom = col_skip(),
    wkb_geom_wgs84 = col_skip(),
    Hangausrichtung_SRTM1_puffer50m = col_character(),
    Umfeldanalyse_km2 = col_character(),
    Temperatur_Jahr = col_character()
  )
)
head(dta)

# save as "Daten/Daten_mit_Epoche.RDS" we are only interested in the lng_wgs84 and lat_wgs84