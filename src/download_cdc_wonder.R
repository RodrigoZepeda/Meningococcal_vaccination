rm(list = ls())
pacman::p_load(RSocrata, tidyverse, foreach, glue)

# Get your token by going to https://dev.socrata.com/foundry/data.cdc.gov/wcwi-x3uk and signing up.
# My suggestion is to edit the project's .Renviron by: `usethis::edit_r_environ(scope = "project")`
# and putting the following
# CDC_PASSWORD="your_password"
# CDC_TOKEN="your_api_token"
# CDC_EMAIL="your_email"

# We loop through the following vector which contains the name of the dataset year and the link
# to the API

data_files_nndss <- c(
  "NNDSS - Table I. infrequently reported notifiable diseases - 2014" = "wcwi-x3uk.csv",
  "NNDSS - Table I. infrequently reported notifiable diseases - 2015" = "pb4z-432k.csv",
  "NNDSS - Table I. infrequently reported notifiable diseases - 2016" = "dwqk-w36f.csv",
  "NNDSS - Table I. infrequently reported notifiable diseases - 2017" = "45b4-9j7u.csv",
  "NNDSS - Table I. infrequently reported notifiable diseases - 2018" = "5fyu-rtk3.csv"
)

nndss <- foreach (i = 1:length(data_files_nndss), .combine = bind_rows) %do% {

  #Wait a few seconds
  seconds <- runif(1, 0, 60)
  cli::cli_alert_info("Waiting {seconds}sec")
  Sys.sleep(seconds)

  entry <- data_files_nndss[i]
  cli::cli_alert_info("Processing {names(entry)}")

  url   <- glue("https://data.cdc.gov/resource/{entry}")
  df <- read.socrata(
    url,
    app_token = Sys.getenv("CDC_TOKEN"),
    email     = Sys.getenv("CDC_EMAIL"),
    password  = Sys.getenv("CDC_PASSWORD")
  )

  df <- df |>
    mutate(filename = !!names(entry)) |>
    mutate(url = !!url) |>
    filter(str_detect(disease, "Meningococcal"))

  if (i == 1){
    df <- df |>
      select(disease, mmwr_year, mmwr_week, current_week, cum_2014,
             total_cases_reported_2013, total_cases_reported_2012,
             total_cases_reported_2011, total_cases_reported_2010,
             total_cases_reported_2009) |>
      rename(cummulative = cum_2014)

    df_previous <- df |>
      pivot_longer(cols = starts_with("total_cases")) |>
      select(-mmwr_year, -current_week, -cummulative) |>
      mutate(mmwr_year = as.numeric(str_remove_all(name, "total_cases_reported_"))) |>
      select(-name) |>
      rename(cummulative = value)

    df <- df |>
      select(-starts_with("total_cases")) |>
      bind_rows(df_previous)

  } else if (i == 2){
    df <- df |>
      select(disease, mmwr_year, mmwr_week, current_week, cum_2015) |>
      rename(cummulative = cum_2015)
  } else if (i == 3){
    df <- df |>
      select(disease, mmwr_year, mmwr_week, current_week, cum_2016) |>
      rename(cummulative = cum_2016)
  } else if (i == 4){
    df <- df |>
      select(disease, mmwr_year, mmwr_week, current_week, cum_2017) |>
      rename(cummulative = cum_2017)
  } else if (i == 5){
    df <- df |>
      select(disease, mmwr_year, mmwr_week, current_week, cum_2018) |>
      rename(cummulative = cum_2018)
  }

  df <- df |>
    mutate(cummulative = replace_na(cummulative, 0)) |>
    mutate(current_week = replace_na(current_week, 0))

}


nndss2 <- nndss |>
  mutate(disease = case_when(
    str_detect(disease, "[Ss]erogroup B") ~ "Serogroup B",
    str_detect(disease, "[Oo]ther serogroup") ~ "Other Serogroup",
    str_detect(disease, "[Uu]nknown serogroup") ~ "Unknown Serogroup",
    str_detect(disease, "A, C, Y, and") ~ "ACWY Serogroups",
    str_detect(disease, "ACWY") ~ "ACWY Serogroups"
  )) |>
  left_join(
    tibble(date = seq(ymd("2008-12-01"), ymd("2019-01-01"), by = "1 day")) |>
      mutate(mmwr_week = epiweek(date)) |>
      mutate(mmwr_year = epiyear(date)) |>
      distinct(mmwr_year,mmwr_week, .keep_all = T),
    by = join_by(mmwr_year, mmwr_week)
  ) |>
  filter(mmwr_year >= 2014)

nndss2 |>
  write_rds(file.path("data","processed","wonder_socrata_api.rds"))


