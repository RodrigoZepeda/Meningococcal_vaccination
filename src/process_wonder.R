#------------------------------------PROCESS WONDER DATASETS----------------------------------------
# Process the data downloaded by `download_wonder` and generate the three different datasets:
# Author: Rodrigo Zepeda-Tello rodrigo.zepeda [in] columbia.edu
rm(list = ls())
pacman::p_load(tidyverse, readr, foreach, janitor, glue, lubridate)

#' @title Process tabbed line
#' @param n_cols
#' @return A string with all the tabs required
#' @examples
#' process_line_to_cols("a\t\t1\t", 4)
#' process_line_to_cols("a\t\t1\t", 3)
#' process_line_to_cols("a\t\t1\t", 8)
process_line_to_cols <- function(line, n_cols = NULL){
  if (!is.null(n_cols)){
    n_cols_detected <- str_count(line,"\t") + 1
    if (n_cols_detected < n_cols){
      line <- paste0(line, paste0(rep("\t", n_cols - n_cols_detected), collapse = ""), collapse = "")
    }
  }

  return(line)
}

#' @title Read tab delim data
#' @param file_path The path to wonder_cdc text file
#' @return A vector of characters containing the data_lines
#' @examples
#' read_cdc_wonder_file_contents(file_path = "data/cdc_wonder/1996-01-table2B.txt")
#' read_cdc_wonder_file_contents(file_path = "data/cdc_wonder/2016-table4.txt")
read_cdc_wonder_file_contents <- function(file_path, pattern = "tab delimited data:", n_cols = NULL){

  # Initialize a flag to indicate when to start and stop reading
  start_reading <- FALSE
  stop_reading <- FALSE

  # Initialize a vector to store the lines
  data_lines <- c()

  # Read the file line by line
  con <- file(file_path, "r")
  while (TRUE) {
    line <- readLines(con, n = 1, encoding = "WINDOWS-1252")

    line <- suppressWarnings(str_conv(line, "UTF-8"))

    if (nchar(line) == 0 & start_reading) {
      stop_reading <- TRUE
    } else if (grepl(pattern, line)) {
      start_reading <- TRUE
    }

    if (start_reading && !stop_reading) {
      line <- process_line_to_cols(line, n_cols)
      data_lines <- c(data_lines, line)
    }

    if (start_reading && stop_reading) {
      break
    }
  }

  # Close the file connection
  close(con)

  return(data_lines[2:length(data_lines)])
}

#' @title Read the column names of a cdc wonder file
#' @param file_path The path to wonder_cdc text file
#' @return A vector of characters containing the column names of the file
#' @examples
#' read_colnames_cdc_wonder(file_path = "data/cdc_wonder/1996-01-table2B.txt")
#' read_colnames_cdc_wonder(file_path = "data/cdc_wonder/2019-01-table1x.txt")
#' read_colnames_cdc_wonder(file_path = "data/cdc_wonder/2016-table4.txt")
read_colnames_cdc_wonder <- function(file_path){
  read_cdc_wonder_file_contents(file_path = file_path, pattern = "column labels") |>
    str_remove_all("�")
}

#' @title Read the epidemiological week and year of a CDC wonder file name
#' @param file_path The path to wonder_cdc text file
#' @return A vector of numbers containing the epidemiological year and week of the file
#' @examples
#' read_epiweek_epiyear_wonder(file_path = "data/cdc_wonder/1996-01-table2B.txt")
#' read_epiweek_epiyear_wonder(file_path = "data/cdc_wonder/2016-table4.txt")
read_epiweek_epiyear_wonder <- function(file_path){
  split_epiweek <- str_split_1(basename(file_path), "-")
  if (length(split_epiweek) == 2){
    return(as.numeric(split_epiweek[1])) #Yearly case
  } else {
    return(as.numeric(split_epiweek[1:2])) #Weekly case
  }
}

#' @title Read a CDC wonder file
#' @param file_path Path to the file
#' @return A tibble containing the cdc wonder file
#' @examples
#' read_cdc_wonder(file_path = "data/cdc_wonder/1996-01-table2B.txt")
#' read_cdc_wonder("data/cdc_wonder/2023-03-table930.txt")
#' read_cdc_wonder(file_path = "data/cdc_wonder/2016-table4.txt", subset_cols = F)
#' read_cdc_wonder(file_path = "data/cdc_wonder/2016-table4.txt", subset_cols = F)
#' read_cdc_wonder(file_path = "data/cdc_wonder/2015-18-table2H.txt" )
#' read_cdc_wonder(file_path = "data/cdc_wonder/2023-38-table930.txt" )
#' read_cdc_wonder(file_path = "data/cdc_wonder/2023-05-table930.txt" )
read_cdc_wonder <- function(file_path, subset_cols = TRUE){

  #Get the colnames
  column_names <- read_colnames_cdc_wonder(file_path)

  #Add tibble year and epiweek
  year_epiweek <- read_epiweek_epiyear_wonder(file_path)

  #Get the data
  db_content   <- read_cdc_wonder_file_contents(file_path, n_cols = length(column_names))

  #Create the tibble
  #https://github.com/tidyverse/vroom/issues/317
  db_tibble <- suppressWarnings({
    read_delim(I(db_content),
                            col_select = 1:length(column_names),
                            col_names = column_names,
                            delim = "\t",
                            na = c("-", ""),
                            #locale = locale(encoding = "ISO-8859-1"),
                            col_types = cols(`Reporting Area` = col_character(),
                                             `Disease` = col_character(),
                                             .default = col_number()))
  })

  # Double check cause some of the cum years end up with less tabs than they should
  # this is a cdc issue
  if (any(str_detect(column_names, "Cum\\b"))){
    db_content_unfiltered   <- read_cdc_wonder_file_contents(file_path, n_cols = NULL)
    #Read one line
    ntabs <- str_count(db_content_unfiltered[1],"\t")
    if (ntabs < length(column_names)){

       cli::cli_alert_danger("Invalid number of tabs. Attempting to fix")
      not_na <- db_tibble |> select(-`Reporting Area`) |> select(where(~any(!is.na(.))))

      #Get the number of not NA columns that are not reporting area
      if (ncol(not_na) == 1){
        colselector <- str_subset(colnames(db_tibble), as.character(year_epiweek[1]))
        db_tibble[,colselector] <- not_na[,1]
      } else if (ncol(not_na) == 2){
        colselector <- str_subset(colnames(db_tibble),
                                  paste0("Previous\\b|", as.character(year_epiweek[1] - 1)))
        db_tibble[,colselector] <- not_na[1:nrow(not_na),1:2]

      } else if (ncol(not_na) == 3){
        colselector <- str_subset(colnames(db_tibble),"Current week|Reporting Area", negate = T)
        db_tibble[,colselector] <- not_na[1:nrow(not_na),1:3]
      }

      if (ncol(not_na) <= 3){
        colselect <- str_subset(colnames(db_tibble), paste0(
          c( paste0(colselector, collapse = "|"), "Reporting"), collapse = "|"), negate = T)
        db_tibble[,colselect] <- NA_real_
      }
    }
  }

  db_tibble <- db_tibble |>
    mutate(epiyear = !!year_epiweek[1])

  db_tibble <- db_tibble |>
    mutate_if(is.character, str_trim)

  if (subset_cols) {
    db_tibble <- db_tibble |>
      mutate(epiweek = !!year_epiweek[2]) |>
      select(matches("Reporting|Meningococcal|epiweek|epiyear"))
  }

  return(db_tibble)

}

#' @title Read the CDC's wonder files
#' @return A tibble containing the cdc wonder files
#' @examples
#' yrly_df <- read_all_cdc_wonder_yearly()
read_all_cdc_wonder_yearly <- function(){

  flist <- list.files(file.path("data","cdc_wonder"), full.names = TRUE,
                      pattern = "*.txt", all.files = TRUE)

  flist <- str_subset(flist, "table4.txt", negate = FALSE)

  db_all <- foreach(i = 1:length(flist), .combine = bind_rows) %do% {

    cli::cli_alert_info("Reading {.file {flist[i]}}")

    read_cdc_wonder(flist[i], subset_cols = FALSE) |>
      filter(str_detect(Disease, "Meningo"))

  }

  db_all <- db_all |>
    select_if(~!all(is.na(.))) |>
    mutate_if(is.numeric, ~replace_na(., 0))

  db_all <- db_all |>
    pivot_longer(cols = matches("Rate|No"),
                 names_to = c("age_category", ".value"),
                 names_pattern = "(.*[0-9]+.*,|Total.*,) (.+)") |>
    mutate(age_category = str_remove_all(age_category, ","))

  return(db_all)
}


#' @title Read the CDC's wonder files by year
#' @param year The year to read
#' @return A tibble containing the cdc wonder files
#' @examples
#' dbf <- read_year_cdc_wonder_weekly(2019)
#' dbf <- read_year_cdc_wonder_weekly(2022)
#' dbf <- read_year_cdc_wonder_weekly(2023)
#' dbf <- read_year_cdc_wonder_weekly(2015)
read_year_cdc_wonder_weekly <- function(year){

  #Get all tables for year
  flist <- list.files(file.path("data","cdc_wonder"), full.names = TRUE,
                      pattern = glue("{year}.*.txt"), all.files = TRUE)

  flist <- str_subset(flist, "table4.txt", negate = T)[1:10]

  #Get the table types
  tbl_types <- unique(str_remove_all(flist, ".*table|.txt"))

  db_all <- foreach(k = 1:length(tbl_types), .combine = full_join) %do% {

    tbl <- tbl_types[k]

    tblist <- list.files(file.path("data","cdc_wonder"), full.names = TRUE,
                         pattern = glue("{year}.*{tbl}.txt"), all.files = TRUE)

    foreach(i = 1:length(tblist), .combine = bind_rows) %do% {

      cli::cli_alert_info("Reading {.file {tblist[i]}}")

      read_cdc_wonder(tblist[i])

    }
  }

  db_all <- db_all |>
    mutate_if(is.numeric, ~replace_na(., 0))

  return(db_all)
}

#' @title Read the CDC's wonder files by tear
#' @param year The year to read
#' @return A tibble containing the cdc wonder files
#' @examples
#' get_yrs()
get_yrs <- function(){
  as.numeric(unique(str_remove_all(list.files(file.path("data","cdc_wonder")),"-.*")))
}

#' @title Read the CDC's wonder files by tear
#' @param year The year to read
#' @return A tibble containing the cdc wonder files
#' @examples
#' read_all_cdc_wonder_weekly()
read_all_cdc_wonder_weekly <- function(force = FALSE, years = get_yrs()){
  for (yr in years){
    cli::cli_h1(yr)
    f_path <- file.path("data","processed",glue("{yr}.rds"))

    if (!file.exists(f_path) | force){
      read_year_cdc_wonder_weekly(year = yr) |>
        write_rds(f_path)
    }
  }
}

#' @title Process the CDC's wonder files
#' @return A tibble containing the cdc wonder files with the totals
#' @examples
#' process_wonder_files()
process_wonder_files <- function(){

  #1996 and 1995----
  db_1996 <- read_rds(file.path("data","processed","1996.rds")) |>
    filter(toupper(`Reporting Area`) == "UNITED STATES") |>
    rename(`Meningococcal disease, All serogroups cummulative current year` =
             `Meningococcal Disease cummulative for 1996`)

  db_1995 <- db_1996 |>
    select(epiweek, epiyear, `Meningococcal Disease cummulative for 1995`) |>
    mutate(epiyear = epiyear - 1) |>
    rename(`Meningococcal disease, All serogroups cummulative current year` =
             `Meningococcal Disease cummulative for 1995`)

  db_1996 <- db_1996 |>
    bind_rows(db_1995) |>
    select(epiweek, epiyear, `Meningococcal disease, All serogroups cummulative current year`)

  #1997----
  db_1997 <- read_rds(file.path("data","processed","1997.rds")) |>
    filter(toupper(`Reporting Area`) == "UNITED STATES") |>
    rename(`Meningococcal disease, All serogroups cummulative current year` =
             `Meningococcal Disease cummulative for 1997`) |>
    select(epiweek, epiyear, `Meningococcal disease, All serogroups cummulative current year`)

  #1998----
  db_1998 <- read_rds(file.path("data","processed","1998.rds")) |>
    filter(toupper(`Reporting Area`) == "UNITED STATES") |>
    rename(`Meningococcal disease, All serogroups cummulative current year` =
             `Meningococcal Disease cummulative for 1998`)|>
    select(epiweek, epiyear, `Meningococcal disease, All serogroups cummulative current year`)

  #1999----
  db_1999 <- read_rds(file.path("data","processed","1999.rds")) |>
    filter(toupper(`Reporting Area`) == "UNITED STATES") |>
    rename(`Meningococcal disease, All serogroups cummulative current year` =
             `Meningococcal Disease cummulative for 1999`)|>
    select(epiweek, epiyear, `Meningococcal disease, All serogroups cummulative current year`)

  #2000----
  db_2000 <- read_rds(file.path("data","processed","2000.rds")) |>
    filter(toupper(`Reporting Area`) == "UNITED STATES") |>
    rename(`Meningococcal disease, All serogroups cummulative current year` =
             `Meningococcal Disease cummulative for 2000`)|>
    select(epiweek, epiyear, `Meningococcal disease, All serogroups cummulative current year`)

  #2001----
  db_2001 <- read_rds(file.path("data","processed","2001.rds")) |>
    filter(toupper(`Reporting Area`) == "UNITED STATES") |>
    rename(`Meningococcal disease, All serogroups cummulative current year` =
             `Meningococcal Disease cummulative for 2001`)|>
    select(epiweek, epiyear, `Meningococcal disease, All serogroups cummulative current year`)

  #2002----
  db_2002 <- read_rds(file.path("data","processed","2002.rds")) |>
    filter(toupper(`Reporting Area`) == "UNITED STATES") |>
    rename(`Meningococcal disease, All serogroups cummulative current year` =
             `Meningococcal Disease cummulative for 2002`)|>
    select(epiweek, epiyear, `Meningococcal disease, All serogroups cummulative current year`)

  #2003----
  db_2003 <- read_rds(file.path("data","processed","2003.rds")) |>
    filter(toupper(`Reporting Area`) == "UNITED STATES") |>
    rename(`Meningococcal disease, All serogroups cummulative current year` =
             `Meningococcal Disease cummulative for 2003`)|>
    select(epiweek, epiyear, `Meningococcal disease, All serogroups cummulative current year`)

  #2004----
  db_2004 <- read_rds(file.path("data","processed","2005.rds")) |>
    filter(toupper(`Reporting Area`) == "UNITED STATES") |>
    mutate(epiyear = 2004) |>
    rename(`Meningococcal disease, All serogroups cummulative current year` =
             `Meningococcal disease, All serogroups cummulative for 2004`)|>
    rename(`Meningococcal disease, Serogroups ACWY cummulative current year` =
             `Meningococcal disease, Serogroup A, C, Y, and W-135 cummulative for 2004`)|>
    rename(`Meningococcal disease, Serogroup B cummulative for current year` =
             `Meningococcal disease, Serogroup B cummulative for 2004`)|>
    rename(`Meningococcal disease, Other serogroups cummulative current year` =
             `Meningococcal disease, Other serogroup cummulative for 2004`)|>
    rename(`Meningococcal disease, Unknown serogroup cummulative current year` =
             `Meningococcal disease, Serogroup unknown cummulative for 2004`)|>
    select(-matches("2005"), - `Reporting Area`)

  #2005----
  db_2005 <- read_rds(file.path("data","processed","2005.rds")) |>
    filter(toupper(`Reporting Area`) == "UNITED STATES") |>
    rename(`Meningococcal disease, All serogroups cummulative current year` =
             `Meningococcal disease, All serogroups cummulative for 2005`)|>
    rename(`Meningococcal disease, Serogroups ACWY cummulative current year` =
             `Meningococcal disease, Serogroup A, C, Y, and W-135 cummulative for 2005`)|>
    rename(`Meningococcal disease, Serogroup B cummulative for current year` =
             `Meningococcal disease, Serogroup B cummulative for 2005`)|>
    rename(`Meningococcal disease, Other serogroups cummulative current year` =
             `Meningococcal disease, Other serogroup cummulative for 2005`)|>
    rename(`Meningococcal disease, Unknown serogroup cummulative current year` =
             `Meningococcal disease, Serogroup unknown cummulative for 2005`)|>
    select(-matches("2004"), - `Reporting Area`)

  #2006----
  db_2006 <- read_rds(file.path("data","processed","2006.rds")) |>
    filter(toupper(`Reporting Area`) == "UNITED STATES") |>
    rename(`Meningococcal disease, All serogroups cummulative current year` =
             `Meningococcal diseases, invasive, all serogroups cummulative for 2006`)|>
    rename(`Meningococcal disease, Unknown serogroup cummulative current year` =
             `Meningococcal diseases, invasive, serogroup unknown cummulative for 2006`) |>
    rename(`Meningococcal disease, All serogroups incidence current week` =
             `Meningococcal diseases, invasive, all serogroups current week`)|>
    rename(`Meningococcal disease, Unknown serogroup incidence current week` =
             `Meningococcal diseases, invasive, serogroup unknown current week`)|>
    select(-matches("invasive"), - `Reporting Area`)

  #2007----
  db_2007 <- read_rds(file.path("data","processed","2007.rds")) |>
    filter(toupper(`Reporting Area`) == "UNITED STATES") |>
    rename(`Meningococcal disease, All serogroups cummulative current year` =
             `Meningococcal diseases, invasive , all serogroups cummulative for 2007`)|>
    rename(`Meningococcal disease, All serogroups incidence current week` =
             `Meningococcal diseases, invasive , all serogroups current week`)|>
    select(-matches("invasive"), - `Reporting Area`)

  #2008----
  db_2008 <- read_rds(file.path("data","processed","2008.rds")) |>
    filter(toupper(`Reporting Area`) == "UNITED STATES") |>
    rename(`Meningococcal disease, All serogroups cummulative current year` =
             `Meningococcal diseases, invasive , all serogroups cummulative for 2008`)|>
    rename(`Meningococcal disease, All serogroups incidence current week` =
             `Meningococcal diseases, invasive , all serogroups current week`)|>
    select(-matches("invasive"), - `Reporting Area`)

  #2009----
  db_2009 <- read_rds(file.path("data","processed","2009.rds")) |>
    filter(toupper(`Reporting Area`) == "UNITED STATES") |>
    rename(`Meningococcal disease, All serogroups cummulative current year` =
             `Meningococcal diseases, invasive , all serogroups cummulative for 2009`)|>
    rename(`Meningococcal disease, All serogroups incidence current week` =
             `Meningococcal diseases, invasive , all serogroups current week`)|>
    select(-matches("invasive"), - `Reporting Area`)

  #2010----
  db_2010 <- read_rds(file.path("data","processed","2010.rds")) |>
    filter(toupper(`Reporting Area`) == "UNITED STATES") |>
    rename(`Meningococcal disease, All serogroups cummulative current year` =
             `Meningococcal diseases, invasive , All groups cummulative for 2010`)|>
    rename(`Meningococcal disease, All serogroups incidence current week` =
             `Meningococcal diseases, invasive , All groups current week`)|>
    select(-matches("invasive"), - `Reporting Area`)

  #2011----
  db_2011 <- read_rds(file.path("data","processed","2011.rds")) |>
    filter(toupper(`Reporting Area`) == "UNITED STATES") |>
    rename(`Meningococcal disease, All serogroups cummulative current year` =
             `Meningococcal diseases, invasive , All groups cummulative for 2011`)|>
    rename(`Meningococcal disease, All serogroups incidence current week` =
             `Meningococcal diseases, invasive , All groups current week`)|>
    select(-matches("invasive"), - `Reporting Area`)

  #2012----
  db_2012 <- read_rds(file.path("data","processed","2012.rds")) |>
    filter(toupper(`Reporting Area`) == "UNITED STATES") |>
    rename(`Meningococcal disease, All serogroups cummulative current year` =
             `Meningococcal diseases, invasive , All groups cummulative for 2012`)|>
    rename(`Meningococcal disease, All serogroups incidence current week` =
             `Meningococcal diseases, invasive , All groups current week`)|>
    select(-matches("invasive"), - `Reporting Area`)

  #2013----
  db_2013 <- read_rds(file.path("data","processed","2013.rds")) |>
    filter(toupper(`Reporting Area`) == "UNITED STATES") |>
    rename(`Meningococcal disease, All serogroups cummulative current year` =
             `Meningococcal diseases, invasive , All groups cummulative for 2013`)|>
    rename(`Meningococcal disease, All serogroups incidence current week` =
             `Meningococcal diseases, invasive , All groups current week`)|>
    select(-matches("invasive"), - `Reporting Area`)

  #2014----
  db_2014 <- read_rds(file.path("data","processed","2014.rds")) |>
    filter(toupper(`Reporting Area`) == "UNITED STATES") |>
    rename(`Meningococcal disease, All serogroups cummulative current year` =
             `Meningococcal diseases, invasive , All groups cummulative for 2014`)|>
    rename(`Meningococcal disease, All serogroups incidence current week` =
             `Meningococcal diseases, invasive , All groups current week`)|>
    select(-matches("invasive"), - `Reporting Area`)

  #2015----
  db_2015 <- read_rds(file.path("data","processed","2015.rds")) |>
    filter(toupper(`Reporting Area`) == "UNITED STATES") |>
    rename(`Meningococcal disease, All serogroups cummulative current year` =
             `Meningococcal diseases, invasive, All groups cummulative for 2015`)|>
    rename(`Meningococcal disease, All serogroups incidence current week` =
             `Meningococcal diseases, invasive, All groups current week`)|>
    select(-matches("invasive"), - `Reporting Area`)

  #2016----
  db_2016 <- read_rds(file.path("data","processed","2016.rds")) |>
    filter(toupper(`Reporting Area`) == "UNITED STATES") |>
    rename(`Meningococcal disease, All serogroups cummulative current year` =
             `Meningococcal disease, invasive, All groups cummulative for 2016`)|>
    rename(`Meningococcal disease, All serogroups incidence current week` =
             `Meningococcal disease, invasive, All groups current week`)|>
    select(-matches("invasive"), - `Reporting Area`)

  #2017----
  db_2017 <- read_rds(file.path("data","processed","2017.rds")) |>
    filter(toupper(`Reporting Area`) == "UNITED STATES") |>
    rename(`Meningococcal disease, All serogroups cummulative current year` =
             `Meningococcal disease(Neisseria meningitidis), All serogroups cummulative for 2017`)|>
    rename(`Meningococcal disease, All serogroups incidence current week` =
             `Meningococcal disease(Neisseria meningitidis), All serogroups current week`)|>
    select(-matches("Neisseria"), - `Reporting Area`)

  #2018----
  db_2018 <- read_rds(file.path("data","processed","2018.rds")) |>
    filter(toupper(`Reporting Area`) == "UNITED STATES") |>
    rename(`Meningococcal disease, All serogroups cummulative current year` =
             `Meningococcal disease, all serogroups cummulative for 2018`)|>
    rename(`Meningococcal disease, All serogroups incidence current week` =
             `Meningococcal disease, all serogroups current week`)|>
    select(-matches("median|maximum|2017"), - `Reporting Area`)

  db_2018_complement <- read_rds(file.path("data","processed","2019.rds")) |>
    filter(`Reporting Area` == "Total") |>
    mutate(epiyear = 2018) |>
    select(-matches("current")) |>
    rename(`Meningococcal disease, Serogroups ACWY cummulative current year` =
             `Meningococcal disease; Serogroups ACWY cummulative YTD for 2018`)|>
    rename(`Meningococcal disease, Serogroup B cummulative for current year` =
             `Meningococcal disease; Serogroup B cummulative YTD for 2018`)|>
    rename(`Meningococcal disease, Other serogroups cummulative current year` =
             `Meningococcal disease (continued); Other serogroups cummulative YTD for 2018`)|>
    rename(`Meningococcal disease, Unknown serogroup cummulative current year` =
             `Meningococcal disease (continued); Unknown serogroup cummulative YTD for 2018`) |>
    select(-matches("median|maximum|2019|2018|(continued)"), - `Reporting Area`)

  db_2018 <- db_2018 |> left_join(db_2018_complement, by = join_by(epiyear, epiweek))

  #2019----
  db_2019 <- read_rds(file.path("data","processed","2019.rds")) |>
    filter(`Reporting Area` == "Total") |>
    rename(`Meningococcal disease, All serogroups cummulative current year` =
             `Meningococcal disease; All serogroups cummulative YTD for 2019`)|>
    rename(`Meningococcal disease, All serogroups incidence current week` =
             `Meningococcal disease; All serogroups current week`)|>
    rename(`Meningococcal disease, Serogroups ACWY cummulative current year` =
             `Meningococcal disease; Serogroups ACWY cummulative YTD for 2019`)|>
    rename(`Meningococcal disease, Serogroups ACWY incidence current week` =
             `Meningococcal disease; Serogroups ACWY current week`)|>
    rename(`Meningococcal disease, Serogroup B cummulative for current year` =
             `Meningococcal disease; Serogroup B cummulative YTD for 2019`)|>
    rename(`Meningococcal disease, Serogroup B incidence current week` =
             `Meningococcal disease; Serogroup B current week`)|>
    rename(`Meningococcal disease, Other serogroups cummulative current year` =
             `Meningococcal disease (continued); Other serogroups cummulative YTD for 2019`)|>
    rename(`Meningococcal disease, Other serogroups incidence current week` =
             `Meningococcal disease (continued); Other serogroups current week`)|>
    rename(`Meningococcal disease, Unknown serogroup cummulative current year` =
             `Meningococcal disease (continued); Unknown serogroup cummulative YTD for 2019`) |>
    rename(`Meningococcal disease, Unknown serogroup incidence current week` =
           `Meningococcal disease (continued); Unknown serogroup current week`) |>
    select(-matches("median|maximum|2018"), - `Reporting Area`)

  #2020----
  db_2020 <- read_rds(file.path("data","processed","2020.rds")) |>
    filter(`Reporting Area` == "Total") |>
    rename(`Meningococcal disease, All serogroups cummulative current year` =
             `Meningococcal disease, All serogroups; Cum YTD 2020 `)|>
    rename(`Meningococcal disease, All serogroups incidence current week` =
             `Meningococcal disease, All serogroups; Current week`)|>
    rename(`Meningococcal disease, Serogroups ACWY cummulative current year` =
             `Meningococcal disease, Serogroups ACWY; Cum YTD 2020 `)|>
    rename(`Meningococcal disease, Serogroups ACWY incidence current week` =
             `Meningococcal disease, Serogroups ACWY; Current week`)|>
    rename(`Meningococcal disease, Serogroup B cummulative for current year` =
             `Meningococcal disease, Serogroup B; Cum YTD 2020 `)|>
    rename(`Meningococcal disease, Serogroup B incidence current week` =
             `Meningococcal disease, Serogroup B; Current week`)|>
    rename(`Meningococcal disease, Other serogroups cummulative current year` =
             `Meningococcal disease, Other serogroups; Cum YTD 2020 `)|>
    rename(`Meningococcal disease, Other serogroups incidence current week` =
             `Meningococcal disease, Other serogroups; Current week`)|>
    rename(`Meningococcal disease, Unknown serogroup cummulative current year` =
             `Meningococcal disease, Unknown serogroup; Cum YTD 2020 `) |>
    rename(`Meningococcal disease, Unknown serogroup incidence current week` =
             `Meningococcal disease, Unknown serogroup; Current week`) |>
    select(-matches("median|maximum|2019|[Pp]revious"), - `Reporting Area`)

  #2021----
  db_2021 <- read_rds(file.path("data","processed","2021.rds")) |>
    filter(`Reporting Area` == "Total") |>
    rename(`Meningococcal disease, All serogroups cummulative current year` =
             `Meningococcal disease, All serogroups; Cum YTD 2021 `)|>
    rename(`Meningococcal disease, All serogroups incidence current week` =
             `Meningococcal disease, All serogroups; Current week`)|>
    rename(`Meningococcal disease, Serogroups ACWY cummulative current year` =
             `Meningococcal disease, Serogroups ACWY; Cum YTD 2021 `)|>
    rename(`Meningococcal disease, Serogroups ACWY incidence current week` =
             `Meningococcal disease, Serogroups ACWY; Current week`)|>
    rename(`Meningococcal disease, Serogroup B cummulative for current year` =
             `Meningococcal disease, Serogroup B; Cum YTD 2021 `)|>
    rename(`Meningococcal disease, Serogroup B incidence current week` =
             `Meningococcal disease, Serogroup B; Current week`)|>
    rename(`Meningococcal disease, Other serogroups cummulative current year` =
             `Meningococcal disease, Other serogroups; Cum YTD 2021 `)|>
    rename(`Meningococcal disease, Other serogroups incidence current week` =
             `Meningococcal disease, Other serogroups; Current week`)|>
    rename(`Meningococcal disease, Unknown serogroup cummulative current year` =
             `Meningococcal disease, Unknown serogroup; Cum YTD 2021 `) |>
    rename(`Meningococcal disease, Unknown serogroup incidence current week` =
             `Meningococcal disease, Unknown serogroup; Current week`) |>
    select(-matches("median|maximum|2020|[Pp]revious"), - `Reporting Area`)

  #2022----
  db_2022 <- read_rds(file.path("data","processed","2022.rds")) |>
    filter(`Reporting Area` == "Total") |>
    rename(`Meningococcal disease, All serogroups cummulative current year` =
             `Meningococcal disease, All serogroups; Cum YTD 2022 `)|>
    rename(`Meningococcal disease, All serogroups incidence current week` =
             `Meningococcal disease, All serogroups; Current week`)|>
    rename(`Meningococcal disease, Serogroups ACWY cummulative current year` =
             `Meningococcal disease, Serogroups ACWY; Cum YTD 2022 `)|>
    rename(`Meningococcal disease, Serogroups ACWY incidence current week` =
             `Meningococcal disease, Serogroups ACWY; Current week`)|>
    rename(`Meningococcal disease, Serogroup B cummulative for current year` =
             `Meningococcal disease, Serogroup B; Cum YTD 2022 `)|>
    rename(`Meningococcal disease, Serogroup B incidence current week` =
             `Meningococcal disease, Serogroup B; Current week`)|>
    rename(`Meningococcal disease, Other serogroups cummulative current year` =
             `Meningococcal disease, Other serogroups; Cum YTD 2022 `)|>
    rename(`Meningococcal disease, Other serogroups incidence current week` =
             `Meningococcal disease, Other serogroups; Current week`)|>
    rename(`Meningococcal disease, Unknown serogroup cummulative current year` =
             `Meningococcal disease, Unknown serogroup; Cum YTD 2022 `) |>
    rename(`Meningococcal disease, Unknown serogroup incidence current week` =
             `Meningococcal disease, Unknown serogroup; Current week`) |>
    select(-matches("median|maximum|2021|[Pp]revious"), - `Reporting Area`)

  #2023----
  db_2023 <- read_rds(file.path("data","processed","2023.rds")) |>
    filter(`Reporting Area` == "Total") |>
    rename(`Meningococcal disease, All serogroups cummulative current year` =
             `Meningococcal disease, All serogroups; Cum YTD 2023 `)|>
    rename(`Meningococcal disease, All serogroups incidence current week` =
             `Meningococcal disease, All serogroups; Current week`)|>
    rename(`Meningococcal disease, Serogroups ACWY cummulative current year` =
             `Meningococcal disease, Serogroups ACWY; Cum YTD 2023 `)|>
    rename(`Meningococcal disease, Serogroups ACWY incidence current week` =
             `Meningococcal disease, Serogroups ACWY; Current week`)|>
    rename(`Meningococcal disease, Serogroup B cummulative for current year` =
             `Meningococcal disease, Serogroup B; Cum YTD 2023 `)|>
    rename(`Meningococcal disease, Serogroup B incidence current week` =
             `Meningococcal disease, Serogroup B; Current week`)|>
    rename(`Meningococcal disease, Other serogroups cummulative current year` =
             `Meningococcal disease, Other serogroups; Cum YTD 2023 `)|>
    rename(`Meningococcal disease, Other serogroups incidence current week` =
             `Meningococcal disease, Other serogroups; Current week`)|>
    rename(`Meningococcal disease, Unknown serogroup cummulative current year` =
             `Meningococcal disease, Unknown serogroup; Cum YTD 2023 `) |>
    rename(`Meningococcal disease, Unknown serogroup incidence current week` =
             `Meningococcal disease, Unknown serogroup; Current week`) |>
    select(-matches("median|maximum|2022|[Pp]revious"), - `Reporting Area`)

    #Join all the databases----
    db <- db_1995
    for (yr in 1996:2023){
      db <- eval(parse(text = glue("db |> bind_rows(db_{yr})")))
    }

    db |>
      write_rds(file.path("data","processed","weekly_meningococcal_data_cdc_wonder.rds"))

    #Process the yearly data
    read_all_cdc_wonder_yearly() |>
      write_rds(file.path("data","processed","yearly_agegroup_meningococcal_data_cdc_wonder.rds"))

    #Process the serotype data from wonderly
    process_serotypes()

}

#' @title Process the CDC's wonder files for serology
#' @return A tibble containing the cdc wonder files with the totals
#' @examples
#' process_serotypes(force = T)
#'
process_serotypes <- function(force = FALSE){

  serotype_path <- file.path("data","processed","serotype_database.rds")
  if (file.exists(serotype_path) & !force){
    #Download the processed data
    socrata <- read_rds(file.path("data","processed","wonder_socrata_api.rds"))
    wonder  <- read_rds(file.path("data","processed","weekly_meningococcal_data_cdc_wonder.rds"))
    wonder  <- wonder |> filter(epiyear > max(socrata$mmwr_year))
    wonder  <- wonder |>
      rename(mmwr_week = epiweek) |>
      rename(mmwr_year = epiyear)

    wonder_cummulative <- wonder |>
      select(-matches("current week")) |>
      pivot_longer(cols = matches("Meningococcal"), values_to = "cummulative", names_to = "disease") |>
      mutate(disease = case_when(
        str_detect(disease, "[Ss]erogroup B") ~ "Serogroup B",
        str_detect(disease, "[Oo]ther serogroup") ~ "Other Serogroup",
        str_detect(disease, "[Uu]nknown serogroup") ~ "Unknown Serogroup",
        str_detect(disease, "A, C, Y, and") ~ "ACWY Serogroups",
        str_detect(disease, "ACWY") ~ "ACWY Serogroups",
        str_detect(disease, "All serogroups") ~ "All Serogroups (total)",
      ))

    wonder_week <- wonder |>
      select(-matches("current year")) |>
      pivot_longer(cols = matches("Meningococcal"), values_to = "current_week", names_to = "disease") |>
      mutate(disease = case_when(
        str_detect(disease, "[Ss]erogroup B") ~ "Serogroup B",
        str_detect(disease, "[Oo]ther serogroup") ~ "Other Serogroup",
        str_detect(disease, "[Uu]nknown serogroup") ~ "Unknown Serogroup",
        str_detect(disease, "A, C, Y, and") ~ "ACWY Serogroups",
        str_detect(disease, "ACWY") ~ "ACWY Serogroups",
        str_detect(disease, "All serogroups") ~ "All Serogroups (total)",
      ))

    wonder_pivot <- wonder_week |>
      full_join(wonder_cummulative, by = join_by(mmwr_week, mmwr_year, disease)) |>
      left_join(
        tibble(date = seq(ymd("2008-12-01"), today(), by = "1 day")) |>
          mutate(mmwr_week = epiweek(date)) |>
          mutate(mmwr_year = epiyear(date)) |>
          distinct(mmwr_year,mmwr_week, .keep_all = T),
        by = join_by(mmwr_year, mmwr_week)
      )

    socrata <- socrata |>
      bind_rows(wonder_pivot)

    socrata |>
      write_rds(serotype_path)
  }
}

#Process the files
read_all_cdc_wonder_weekly(force = F) #Read the files
process_wonder_files() #Consolidate files
process_serotypes(force = T)
