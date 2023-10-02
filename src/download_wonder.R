#' Process wonder downloads data from the CDC-Wonder database
#'

rm(list = ls())
pacman::p_load(lubridate, tidyverse, glue, cli, beepr, httr)

#' @title Get maximum epiweek
#' @description Function that returns the maximum number of epidemiological
#' weeks for a certain year
#'
#' @param year The year for which we obtain the maximum number of epidemiological weeks
get_max_epiweek <- function(year){
  if (year > 2023){
    cli::cli_abort("No design for 2024 or later. Please program the download")
  }
  if (year < epiyear(today())){
    return(max(epiweek(seq(ymd(glue("{year}-12-20")), ymd(glue("{year}-12-31")), by = "1 day"))))
  } else if (year == epiyear(today())) {
    if (wday(today()) > 5){
      return(epiweek(today()) - 1)
    } else {
      return(epiweek(today()) - 2)
    }
  } else {
    cli::cli_abort("Year {year} hasn't happened yet")
  }
}

#' @title Download meningococcal data for a certain period
#' @description Function for downloading the data on meningococcal diseasefrom the Wonder CDC
#' site \url{https://wonder.cdc.gov/nndss/nndss_weekly_tables_menu.asp?mmwr_year=2022&mmwr_week=01}
#'
#' @note
#' Last updated on September 29th 2023. CDC format changes through the years so be certain that
#' it makes sense
#'
#' @param verbose Boolean indicating whether to print updates
download_cdc_wonder <- function(years, tables, url_type = c("2017","2016", "1996-2014"),
                                request = "Export", verbose = TRUE){

  for (year in years) {
    max_epiweek <- get_max_epiweek(year)
    if (verbose) cli::cli_progress_bar(glue("Downloading {year} files"), total = max_epiweek)
    for (epiweek in 1:max_epiweek) {
      if (verbose) cli::cli_progress_update(set = epiweek)
      if (epiweek < 10) epiweek <- paste0("0", epiweek)
      for (tablenum in tables) {
        fpath <- file.path("data", "cdc_wonder", glue("{year}-{epiweek}-table{tablenum}.txt"))
        if (!file.exists(fpath)) {
          if (url_type[1] == "2017"){
            url <- glue(
              "https://wonder.cdc.gov/nndss/static/{year}/{epiweek}/{year}-{epiweek}-table{tablenum}.txt"
            )
          } else if (url_type[1] == "2016"){
            url <- glue(
              "https://wonder.cdc.gov/nndss/nndss_reps.asp?mmwr_year={year}&mmwr_week={epiweek}&mmwr_table={tablenum}&request={request}"
            )
          } else if (url_type[1] == "2015"){
            url <- glue(
              "https://wonder.cdc.gov/nndss/nndss_reps.asp?mmwr_year={year}&mmwr_week={epiweek}&mmwr_table={tablenum}&request={request}"
            )
          } else if (url_type[1] == "1996-2014") {
            url <- glue(
              "https://wonder.cdc.gov/nndss/nndss_weekly_tables_1995_2014.asp?mmwr_year={year}&mmwr_week={epiweek}&mmwr_table={tablenum}&request={request}"
            )
          }
          cli::cli_alert("Trying {.url {url}}")

          if (url_type[1] != "2017"){
            response <- url |> POST(body = list(), encode = "form", progress = progress(), verbose())
            writeBin(content(response), fpath)
          } else {
            download.file(url = url, quiet = TRUE, destfile = fpath, method = "curl")
          }
          if (file.size(fpath) <= 0){
            cli::cli_abort("Problem downloading file {fpath}")
          }
        }
      }
    }
  }

  if (verbose) cli::cli_progress_done(result = glue("Downloaded all {year} files"))
}

#' @title Download meningococcal data for a certain period
#' @description Function for downloading the data on meningococcal diseasefrom the Wonder CDC
#' site \url{https://wonder.cdc.gov/nndss/nndss_weekly_tables_menu.asp?mmwr_year=2022&mmwr_week=01}
#'
#' @note
#' Last updated on September 29th 2023. CDC format changes through the years so be certain that
#' it makes sense
#'
#' @param verbose Boolean indicating whether to print updates
download_cdc_anual <- function(years, verbose = TRUE){

  for (year in years) {
    fpath <- file.path("data", "cdc_wonder", glue("{year}-table4.txt"))
    if (!file.exists(fpath)) {
      if (verbose) cli::cli_alert("Downloading {year} anual report")
      url <- glue(
        "https://wonder.cdc.gov/nndss/static/{year}/annual/{year}-table4.txt"
      )
      download.file(url = url, quiet = TRUE, destfile = fpath, method = "curl")
      if (file.size(fpath) <= 0){
        cli::cli_abort("Problem downloading file {fpath}")
      }
    }
  }
}



download_2019_2022 <- function(verbose = TRUE){
  download_cdc_wonder(years = 2019:2022, tables = c("1w","1x"), verbose = verbose)
}

download_2023 <- function(verbose = TRUE){
  download_cdc_wonder(years = 2023, tables = c("920","930","940","950","960"), verbose = verbose)
}

download_2018 <- function(verbose = TRUE){
  download_cdc_wonder(years = 2018, tables = c("2M"), verbose = verbose)
}

download_2017 <- function(verbose = TRUE){
  download_cdc_wonder(years = 2017, tables = c("2K"), verbose = verbose)
}

download_2016 <- function(verbose = TRUE){
  download_cdc_wonder(years = 2016, tables = c("2H"), verbose = verbose, url_type = "2016")
}

download_2015 <- function(verbose = TRUE){
  download_cdc_wonder(years = 2015, tables = c("2I"), verbose = verbose, url_type = "2016")
}

download_2010_2014 <- function(verbose = TRUE){
  download_cdc_wonder(years = 2010:2014, tables = c("2G"), verbose = verbose, url_type = "1996-2014")
}

download_2007_2009 <- function(verbose = TRUE){
  download_cdc_wonder(years = 2007:2009, tables = c("2D"), verbose = verbose, url_type = "1996-2014")
}

download_2006 <- function(verbose = TRUE){
  download_cdc_wonder(years = 2006, tables = c("2E"), verbose = verbose, url_type = "1996-2014")
}

download_2005 <- function(verbose = TRUE){
  download_cdc_wonder(years = 2005, tables = c("2F"), verbose = verbose, url_type = "1996-2014")
}

download_2002_2004 <- function(verbose = TRUE){
  download_cdc_wonder(years = 2002:2004, tables = c("2E"), verbose = verbose, url_type = "1996-2014")
}

download_1997_2001 <- function(verbose = TRUE){
  download_cdc_wonder(years = 1997:2001, tables = c("3B"), verbose = verbose, url_type = "1996-2014")
}

download_1996 <- function(verbose = TRUE){
  download_cdc_wonder(years = 1996, tables = c("2B"), verbose = verbose, url_type = "1996-2014")
}

download_all <- function(verbose = TRUE, beep = TRUE){
  download_cdc_anual(2016:2020, verbose = verbose)
  download_1996(verbose = verbose)
  download_1997_2001(verbose = verbose)
  download_2002_2004(verbose = verbose)
  download_2005(verbose = verbose)
  download_2006(verbose = verbose)
  download_2007_2009(verbose = verbose)
  download_2010_2014(verbose = verbose)
  download_2015(verbose = verbose)
  download_2016(verbose = verbose)
  download_2017(verbose = verbose)
  download_2018(verbose = verbose)
  download_2019_2022(verbose = verbose)
  download_2023(verbose = verbose)
  if (beep) beepr::beep(5)
}

#Download the files----

download_all()

