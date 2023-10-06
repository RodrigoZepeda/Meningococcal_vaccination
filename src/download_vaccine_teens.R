rm(list = ls())
pacman::p_load(RSocrata, tidyverse, foreach, glue)

# Get your token by going to https://dev.socrata.com/foundry/data.cdc.gov/wcwi-x3uk and signing up.
# My suggestion is to edit the project's .Renviron by: `usethis::edit_r_environ(scope = "project")`
# and putting the following
# CDC_PASSWORD="your_password"
# CDC_TOKEN="your_api_token"
# CDC_EMAIL="your_email"
#https://data.cdc.gov/Teen-Vaccinations/Vaccination-Coverage-among-Adolescents-13-17-Years/ee48-w5t6
# We loop through the following vector which contains the name of the dataset year and the link
# to the API

url <- "https://data.cdc.gov/resource/ee48-w5t6.csv"

df <- read.socrata(
  url,
  app_token = Sys.getenv("CDC_TOKEN"),
  email     = Sys.getenv("CDC_EMAIL"),
  password  = Sys.getenv("CDC_PASSWORD")
)

teen_vaccine <- df |>
  filter(vaccine == "Meningococcal Conjugate") |>
  filter(geography == "United States") |>
  filter(dimension == "13-17 Years") |>
  select(year_season, coverage_estimate, matches("X_95_ci")) |>
  separate(X_95_ci, c("lower_ci","upper_ci"), sep = "to")

teen_vaccine |>
  write_rds(file.path("data","processed","vaccination_from_survey.rds"))
