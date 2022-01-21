
###### Declare functions ######
pos_capital <- function(
  cities = data.table("CITY" = c(""), "CAPITAL" = 0, "LONG" = c(), "LAT" = c())
) {
  cities %>%
    filter(CAPITAL == 1) %>%
    select(CITY, CAPITAL, LONG, LAT) %>%
    return()
}

find_dist <- function(
  city_1 = data.table("CITY" = c(""), "LONG" = c(), "LAT" = c()),
  city_2 = data.table("CITY" = c(""), "LONG" = c(), "LAT" = c())
) {
  d_long <- as.numeric(city_1["LONG"]) - as.numeric(city_2["LONG"])
  d_lat <- as.numeric(city_1["LAT"]) - as.numeric(city_2["LAT"])
  sqrt(d_long ^ 2 + d_lat ^ 2) %>%
    return()
}

find_dist_to_cap <- function(
  cities = data.table("CITY" = c(""), "CAPITAL" = 0, "LONG" = c(), "LAT" = c())
) {
  df_cap <- pos_capital(cities = cities)
  d <- apply(cities, 1, function(x) {
    l <- rbind(x, df_cap) %>%
      apply(., 1, function(y) find_dist(city_1 = x, city_2 = y))
    return(min(l[-1]))
  })
  return(d)
}

find_region <- function(cities = data.table("STATE" = c(""))) {
  cities %>%
    mutate(
      REGION = case_when(
        STATE == "AC" ~ "N",
        STATE == "AL" ~ "NE",
        STATE == "AP" ~ "N",
        STATE == "AM" ~ "N",
        STATE == "BA" ~ "NE",
        STATE == "CE" ~ "NE",
        STATE == "ES" ~ "SE",
        STATE == "GO" ~ "CO",
        STATE == "MA" ~ "NE",
        STATE == "MT" ~ "CO",
        STATE == "MS" ~ "CO",
        STATE == "MG" ~ "SE",
        STATE == "PA" ~ "N",
        STATE == "PB" ~ "NE",
        STATE == "PR" ~ "S",
        STATE == "PE" ~ "NE",
        STATE == "PI" ~ "NE",
        STATE == "RJ" ~ "SE",
        STATE == "RN" ~ "NE",
        STATE == "RS" ~ "S",
        STATE == "RO" ~ "N",
        STATE == "RR" ~ "N",
        STATE == "SC" ~ "S",
        STATE == "SP" ~ "SE",
        STATE == "SE" ~ "NE",
        STATE == "TO" ~ "N",
        STATE == "DF" ~ "CO"
      )
    )
}

north = c("N", "NE")
south = c("CO", "S", "SE")
