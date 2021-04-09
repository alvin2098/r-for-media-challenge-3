### --- R Challange 3, Alvin Aziz, 08.04.2021 --- ###
library(assertthat)
library(dplyr)
library(tidyr)

### Challenge III
# X Load both the 2020 election results ('wahlergebnisse.rds') and stadtteil_profile ('stadtteil_profil.rds').
# X Calculate each parties' election result per district (valid ballots are basis for this calculation).
# * Hint: investigate the function `across()` for applying calculation on multiple columns at once.
# X Calculate the ratio of people with a migration background in the total population of each district.
# X Compare migration ratio to results of the AfD
# * Compare the voter turnout to both other variables.
# * Join the two data sets.
# * Arrange by the AFD's results in descending order. 
# * Prepare to discuss in the next session!
# * Hint: the final table must have the following columns: stadtteil, mig_ratio, turn_out, afd.


wahlergebnisse = readRDS("./wahlergebnisse.rds")
stadtteil_profil = readRDS("./stadtteil_profil.rds")

stadtteil <- wahlergebnisse %>% 
  select(1, 9:22)

stadtteil[is.na(stadtteil)] = 0

stadtteil <- mutate_if(stadtteil, is.numeric, ~ . * 100 / wahlergebnisse$gultige_stimmen)

mig_ratio <- data.frame(stadtteil_profil$stadtteil, stadtteil_profil$bevolkerung_mit_migrations_hintergrund * 100 / stadtteil_profil$bevolkerung)

migXafd <- stadtteil %>% 
  select(bezeichnung, af_d) %>% 
  arrange(desc(2)) %>% 
  left_join(mig_ratio, by = c("bezeichnung" = "stadtteil_profil.stadtteil"))
  

### --- ToDos --- ###
colnames(stadtteil)[1] <- "stadtteil"

migrantsPerDistrict <- stadtteil_profil %>% 
  select(1, 10)

stadtteil <- stadtteil %>% 
  full_join(migrantsPerDistrict, by = c("stadtteil" = "stadtteil"))
  
# combined 
  # select("stadtteil", "mig_ratio", "afd", "turn_out")


if (
  assert_that(
    has_name(combined, "stadtteil"), msg = "Spalte 'stadtteil' fehlt"
  ) &
  assert_that(
    has_name(combined, "mig_ratio"), msg = "Spalte 'mig_ratio' fehlt"
  ) &
  assert_that(
    has_name(combined, "afd"), msg = "Spalte 'afd' fehlt"
  ) &
  assert_that(
    has_name(combined, "turn_out"), msg = "Spalte 'turn_out' fehlt"
  ) &
  assert_that(
    openssl::md5(paste(combined$stadtteil, collapse = ", ")) == "072ab9abd1f677ded727744ce0fc9f42",
    msg = "Spalte 'stadtteil' enthält einen Fehler"
  ) &
  assert_that(
    openssl::md5(paste(combined$afd, collapse = ", ")) == "9e37002645e55b6bb397622eb8984e21",
    msg = "Spalte 'afd' enthält einen Fehler"
  ) &
  assert_that(
    openssl::md5(paste(combined$mig_ratio, collapse = ", ")) == "222086dd76fcbefb0cdce33ca561ae10",
    msg = "Spalte 'mig_ratio' enthält einen Fehler"
  ) &
  assert_that(
    openssl::md5(paste(combined$turn_out, collapse = ", ")) == "5f4281dded9968151702c6533fba4fec",
    msg = "Spalte 'turn_out' fehlt"
  )
) {
  writeLines("10/10 Points. Congrats!")
}
