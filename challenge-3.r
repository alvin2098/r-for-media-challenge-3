### --- R Challange 3, Alvin Aziz, 08.04.2021 --- ###
library(assertthat)
library(tidyverse)
library(dplyr)
library(tidyr)

### Challenge III
# X Load both the 2020 election results ('wahlergebnisse.rds') and stadtteil_profile ('stadtteil_profil.rds').
# X Calculate each parties' election result per district (valid ballots are basis for this calculation).
# * Hint: investigate the function `across()` for applying calculation on multiple columns at once.
# X Calculate the ratio of people with a migration background in the total population of each district.
# X Compare migration ratio to results of the AfD
# X Compare the voter turnout to both other variables.
# X Join the two data sets.
# X Arrange by the AFD's results in descending order. 
# * Prepare to discuss in the next session!
# * Hint: the final table must have the following columns: stadtteil, mig_ratio, turn_out, afd.

### --- Load results --- ###
wahlergebnisse = readRDS("./wahlergebnisse.rds")
stadtteil_profil = readRDS("./stadtteil_profil.rds")

### --- Each parties' election results per district --- ###
resultPerDist <- wahlergebnisse %>% 
  select(1, 9:22)
resultPerDist[is.na(resultPerDist)] = 0
resultPerDist <- mutate_if(resultPerDist, is.numeric, ~ . / wahlergebnisse$gultige_stimmen)

### --- Ratio of people with a migration background and the population of each district)
mig_ratio <- data.frame(stadtteil_profil$stadtteil, stadtteil_profil$bevolkerung_mit_migrations_hintergrund / stadtteil_profil$bevolkerung)

### --- Migration ratio compared to votes for AfD --- ###
migXafd <- resultPerDist %>% 
  select(bezeichnung, af_d) %>% 
  left_join(mig_ratio, by = c("bezeichnung" = "stadtteil_profil.stadtteil")) %>% 
  rename(
    afd = af_d,
    stadtteil = bezeichnung
    )

### --- Voter turnouts and joined data set --- ###
combined <- migXafd %>% 
  select(1:3) %>% 
  mutate(turn_out = wahlergebnisse$wahlende / wahlergebnisse$wahlberechtigte_insgesamt) %>% 
  arrange(desc(afd))
  
colnames(combined)[3] <- "mig_ratio"
  


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
