rm(list = ls())
pacman::p_load(tidyverse, ggtext, MetBrewer, lubridate, ggstream, ggformula)
#AGE PLOTS------
age_m <- read_rds(
  file.path("data","processed","yearly_agegroup_meningococcal_data_cdc_wonder.rds")) |>
  mutate(age_category = factor(age_category, ordered = TRUE,
                               levels = c("<1 yr", "1-4 yrs", "5-14 yrs", "15-24 yrs",
                                          "25-39 yrs", "40-64 yrs", ">=65 yrs", "Total")))

age_m |>
  filter(age_category != "Total") |>
  ggplot() +
  geom_col(aes(x = epiyear, y = Rate, fill = age_category), position = "dodge") +
  theme_bw() +
  scale_fill_manual("Age category", values = met.brewer("Hokusai1", 7)) +
  labs(
    x = "Epidemiological year",
    y = "Rate per 100,000 individuals",
    title = "Meningococcal disease by age group (rates)",
    subtitle = "CDC Wonder"
  ) +
  theme(
    plot.background = element_rect(fill = "#3D6D72", color = NULL),
    panel.background = element_rect(fill = "#3D6D72"),
    panel.border = element_rect(color = "#DBDBDB", linetype = "solid", linewidth = 2),
    axis.text = element_text(color = "#DBDBDB"),
    axis.ticks = element_line(color = "#DBDBDB"),
    axis.line = element_line(color = "#DBDBDB", linewidth = 2),
    plot.title = element_text(color = "#DBDBDB", face = "bold"),
    plot.subtitle = element_text(color = "#DBDBDB", face = "italic"),
    axis.title =  element_text(color = "#DBDBDB"),
    panel.grid.major = element_line(color = "#1A2F31", linetype = "dotted"),
    panel.grid.minor = element_line(color = "#DBDBDB", linetype = "dotted"),
    legend.background = element_rect(fill = "#DBDBDB"),
    legend.position = "bottom"
  )
ggsave(file.path("images","age_group_cdc_wonder.pdf"), width = 8, height = 4, bg = "#3D6D72")

age_total <- age_m |>
  filter(age_category != "Total") |>
  group_by(epiyear) |>
  summarise(Total = sum(No.))

age_m |>
  filter(age_category != "Total") |>
  left_join(age_total) |>
  mutate(pct = `No.`/Total) |>
  ggplot() +
  geom_col(aes(x = epiyear, y = pct, fill = age_category), position = "dodge") +
  theme_bw() +
  scale_fill_manual("Age category", values = met.brewer("Hokusai1", 7)) +
  labs(
    x = "Epidemiological year",
    y = "Percent of overall cases in age group",
    title = "Meningococcal disease by age group (percent of cases)",
    subtitle = "CDC Wonder"
  ) +
  theme(
    plot.background = element_rect(fill = "#3D6D72", color = NULL),
    panel.background = element_rect(fill = "#3D6D72"),
    panel.border = element_rect(color = "#DBDBDB", linetype = "solid", linewidth = 2),
    axis.text = element_text(color = "#DBDBDB"),
    axis.ticks = element_line(color = "#DBDBDB"),
    axis.line = element_line(color = "#DBDBDB", linewidth = 2),
    plot.title = element_text(color = "#DBDBDB", face = "bold"),
    plot.subtitle = element_text(color = "#DBDBDB", face = "italic"),
    axis.title =  element_text(color = "#DBDBDB"),
    panel.grid.major = element_line(color = "#1A2F31", linetype = "dotted"),
    panel.grid.minor = element_line(color = "#DBDBDB", linetype = "dotted"),
    legend.background = element_rect(fill = "#DBDBDB"),
    legend.position = "bottom"
  ) +
  scale_y_continuous(labels = scales::percent_format())
ggsave(file.path("images","age_pct_group_cdc_wonder.pdf"), width = 8, height = 4, bg = "#3D6D72")

#CASE PLOTS------
case_m <- read_rds(file.path("data","processed","weekly_meningococcal_data_cdc_wonder.rds")) |>
  left_join(
    tibble(date = seq(ymd("1994/12/01"), today(), by = "1 day")) |>
      mutate(epiweek = epiweek(date)) |>
      mutate(epiyear = epiyear(date)) |>
      distinct(epiweek, epiyear, .keep_all = TRUE),
    by = join_by(epiweek, epiyear)
  )

weekly_data <- case_m |>
  filter(!is.na(`Meningococcal disease, All serogroups incidence current week`)) |>
  summarise(min(date)) |>
  pull()

case_m <- case_m |>
  arrange(date) |>
  mutate(`Meningococcal disease, All serogroups incidence current week` =
           if_else(
             is.na(`Meningococcal disease, All serogroups incidence current week`) & epiweek == 1,
             `Meningococcal disease, All serogroups cummulative current year`,
             `Meningococcal disease, All serogroups incidence current week`)) |>
  mutate(`Meningococcal disease, Serogroups ACWY incidence current week` =
           if_else(
             is.na(`Meningococcal disease, Serogroups ACWY incidence current week`) & epiweek == 1,
             `Meningococcal disease, Serogroups ACWY cummulative current year`,
             `Meningococcal disease, Serogroups ACWY incidence current week`)) |>
  mutate(`Meningococcal disease, Serogroup B incidence current week` =
           if_else(
             is.na(`Meningococcal disease, Serogroup B incidence current week`) & epiweek == 1,
             `Meningococcal disease, Serogroup B cummulative for current year`,
             `Meningococcal disease, Serogroup B incidence current week`)) |>
  mutate(`Meningococcal disease, Unknown serogroup incidence current week` =
           if_else(
             is.na(`Meningococcal disease, Unknown serogroup incidence current week`) & epiweek == 1,
             `Meningococcal disease, Unknown serogroup cummulative current year`,
             `Meningococcal disease, Unknown serogroup incidence current week`)) |>
  mutate(`Meningococcal disease, Other serogroups incidence current week` =
           if_else(
             is.na(`Meningococcal disease, Other serogroups incidence current week`) & epiweek == 1,
             `Meningococcal disease, Other serogroups cummulative current year`,
             `Meningococcal disease, Other serogroups incidence current week`)) |>
  mutate(`Meningococcal disease, All serogroups incidence current week` =
           if_else(
             is.na(`Meningococcal disease, All serogroups incidence current week`) & epiyear == lag(epiyear),
             `Meningococcal disease, All serogroups cummulative current year` - lag(`Meningococcal disease, All serogroups cummulative current year`, default = 0),
                   `Meningococcal disease, All serogroups incidence current week`)) |>
  mutate(`Meningococcal disease, Serogroups ACWY incidence current week` =
           if_else(
             is.na(`Meningococcal disease, Serogroups ACWY incidence current week`) & epiyear == lag(epiyear),
             `Meningococcal disease, Serogroups ACWY cummulative current year` - lag(`Meningococcal disease, Serogroups ACWY cummulative current year`, default = 0),
             `Meningococcal disease, Serogroups ACWY incidence current week`)) |>
  mutate(`Meningococcal disease, Serogroup B incidence current week` =
           if_else(
             is.na(`Meningococcal disease, Serogroup B incidence current week`) & epiyear == lag(epiyear),
             `Meningococcal disease, Serogroup B cummulative for current year` - lag(`Meningococcal disease, Serogroup B cummulative for current year`, default = 0),
             `Meningococcal disease, Serogroup B incidence current week`)) |>
  mutate(`Meningococcal disease, Unknown serogroup incidence current week` =
           if_else(
             is.na(`Meningococcal disease, Unknown serogroup incidence current week`) & epiyear == lag(epiyear),
             `Meningococcal disease, Unknown serogroup cummulative current year` - lag(`Meningococcal disease, Unknown serogroup cummulative current year`, default = 0),
             `Meningococcal disease, Unknown serogroup incidence current week`)) |>
  mutate(`Meningococcal disease, Other serogroups incidence current week` =
           if_else(
             is.na(`Meningococcal disease, Other serogroups incidence current week`) & epiyear == lag(epiyear),
             `Meningococcal disease, Other serogroups cummulative current year` - lag(`Meningococcal disease, Other serogroups cummulative current year`, default = 0),
             `Meningococcal disease, Other serogroups incidence current week`))
case_m <- case_m |>
  filter(`Meningococcal disease, All serogroups incidence current week` >= 0)

case_m |> write_excel_csv(file.path("data","paper","incidence.csv"))
case_m |> write_rds(file.path("data","paper","incidence.rds"))

ggplot(case_m) +
  geom_col(aes(x = date, y = `Meningococcal disease, All serogroups incidence current week`),
           fill = met.brewer("Hokusai1", 7)[6]) +
  theme_bw() +
  labs(
    x = "Date",
    y = "Weekly incident cases",
    title = "Meningococcal disease by serogroup (incidence)",
    subtitle = "CDC Wonder / Socrata-API (NNDSS - Table I)",
    caption = glue("Weekly incidence data starts on {weekly_data}. Previous incidence cases recovered from weekly cummulative cases")
  ) +
  theme(
    plot.background = element_rect(fill = "#3D6D72", color = NULL),
    panel.background = element_rect(fill = "#3D6D72"),
    panel.border = element_rect(color = "#DBDBDB", linetype = "solid", linewidth = 2),
    axis.text = element_text(color = "#DBDBDB"),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    axis.ticks = element_line(color = "#DBDBDB"),
    axis.line = element_line(color = "#DBDBDB", linewidth = 2),
    plot.title = element_text(color = "#DBDBDB", face = "bold"),
    plot.subtitle = element_text(color = "#DBDBDB", face = "italic"),
    plot.caption = element_text(color = "#DBDBDB", face = "italic"),
    axis.title =  element_text(color = "#DBDBDB"),
    panel.grid.major = element_line(color = "#1A2F31", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill = "#DBDBDB"),
    legend.position = "bottom"
  ) +
  scale_y_continuous(label = scales::comma_format()) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")
ggsave(file.path("images","weekly_incidence.pdf"), width = 8, height = 4, bg = "#3D6D72")


case_m |>
  filter(epiyear >= 2014) |>
  ggplot() +
  geom_col(aes(x = date, y = `Meningococcal disease, All serogroups incidence current week`),
           fill = met.brewer("Hokusai1", 7)[3]) +
  theme_bw() +
  labs(
    x = "Date",
    y = "Weekly incident cases",
    title = "2014-2023",
  ) +
  theme(
    plot.background = element_rect(fill = "#3D6D72", color = NULL),
    panel.background = element_rect(fill = "#3D6D72"),
    panel.border = element_rect(color = "#DBDBDB", linetype = "solid", linewidth = 2),
    axis.text = element_text(color = "#DBDBDB"),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    axis.ticks = element_line(color = "#DBDBDB"),
    axis.line = element_line(color = "#DBDBDB", linewidth = 2),
    plot.title = element_text(color = "#DBDBDB", face = "bold"),
    plot.subtitle = element_text(color = "#DBDBDB", face = "italic"),
    plot.caption = element_text(color = "#DBDBDB", face = "italic"),
    axis.title =  element_text(color = "#DBDBDB"),
    panel.grid.major = element_line(color = "#1A2F31", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill = "#DBDBDB"),
    legend.position = "bottom"
  ) +
  scale_y_continuous(label = scales::comma_format()) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")
ggsave(file.path("images","subplot_incidence.pdf"), width = 8, height = 4, bg = "#3D6D72")

#Serogroup plots-----
yr_2023 <- case_m |>
  filter(epiyear == 2023) |>
  select(matches("epiweek|epiyear|week"))

pivot <- case_m |>
  select(date, epiyear, epiweek, matches("week"), -matches("All serogroups")) |>
  pivot_longer(cols = matches("Meningococcal"), names_to = "Class", values_to = "Cases") |>
  filter(epiyear >= 2018) |>
  mutate(Cases = replace_na(Cases, 0)) |>
  mutate(Cases = if_else(Cases < 0, 0, Cases)) |>
  mutate(disease = case_when(
    str_detect(Class, "Serogroup B") ~ "Serogroup B",
    str_detect(Class, "Other serogroups") ~ "Other Serogroup",
    str_detect(Class, "Unknown serogroup") ~ "Unknown Serogroup",
    str_detect(Class, "Serogroups ACWY") ~ "ACWY Serogroups"
  ))


socrata <- read_rds("data/processed/serotype_database.rds") |>
  mutate(disease = factor(disease, ordered = T,
                          levels = rev(c("All Serogroups (total)", "ACWY Serogroups",
                                         "Serogroup B","Other Serogroup","Unknown Serogroup"))))
socrata |> write_excel_csv(file.path("data","paper","serotypes.csv"))
socrata |> write_rds(file.path("data","paper","serotypes.rds"))


socrata |>
  filter(disease != "All Serogroups (total)") |>
  ggplot() +
  geom_col(aes(x = date, y = current_week, fill = disease),
           position = "stack", color = "#1A2F31", linewidth = 0.05) +
  theme_bw() +
  scale_fill_manual("Serogroup", values = rev(met.brewer("Hokusai1", 7)[c(2,3,4,6)])) +
  labs(
    x = NULL,
    y = "Weekly incident cases",
    title = "Meningococcal disease by serogroup",
    subtitle = "CDC Wonder / Socrata-API (NNDSS - Table I)"
  ) +
  theme(
    plot.background = element_rect(fill = "#3D6D72", color = NULL),
    panel.background = element_rect(fill = "#3D6D72"),
    panel.border = element_rect(color = "#DBDBDB", linetype = "solid", linewidth = 2),
    axis.text = element_text(color = "#DBDBDB"),
    axis.ticks = element_line(color = "#DBDBDB"),
    axis.line = element_line(color = "#DBDBDB", linewidth = 2),
    plot.title = element_text(color = "#DBDBDB", face = "bold"),
    plot.subtitle = element_text(color = "#DBDBDB", face = "italic"),
    axis.title =  element_text(color = "#DBDBDB"),
    panel.grid.major = element_line(color = "#1A2F31", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill = "#DBDBDB"),
    legend.position = "bottom"
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")
ggsave(file.path("images","serogroups.pdf"), width = 8, height = 4, bg = "#3D6D72")

socrata_filtered <- socrata |>
  filter(disease != "All Serogroups (total)") |>
  filter(disease != "Unknown Serogroup")

socrata_filtered <- socrata_filtered |> as_tibble() |> distinct()

ggplot(socrata_filtered) +
  geom_stream(aes(date, current_week, fill = disease), type = "proportional",
              color = "#1A2F31", bw = 0.25) +
  theme_bw() +
  scale_fill_manual("Serogroup", values = rev(met.brewer("Hokusai1", 7)[c(2,3,4)])) +
  labs(
    x = "",
    y = "Percent of cases by serogroup",
    title = "Meningococcal disease by serogroup (percent) in the US",
    subtitle = "CDC Wonder / Socrata-API"
  ) +
  theme(
    plot.background = element_rect(fill = "#3D6D72", color = NULL),
    panel.background = element_rect(fill = "#3D6D72"),
    panel.border = element_rect(color = "#DBDBDB", linetype = "solid", linewidth = 2),
    axis.text = element_text(color = "#DBDBDB"),
    axis.ticks = element_line(color = "#DBDBDB"),
    axis.line = element_line(color = "#DBDBDB", linewidth = 2),
    plot.title = element_text(color = "#DBDBDB", face = "bold"),
    plot.subtitle = element_text(color = "#DBDBDB", face = "italic"),
    axis.title =  element_text(color = "#DBDBDB"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill = "#DBDBDB"),
    legend.position = "bottom"
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = scales::percent_format())
ggsave(file.path("images","serogroups_percent.pdf"), width = 8, height = 4, bg = "#3D6D72")

socrata_pct <- socrata_filtered |>
  group_by(disease) |>
  mutate(smoothed_week = zoo::rollmean(current_week, 7, na.rm = T, na.pad = T)) |>
  ungroup() |>
  group_by(date) |>
  mutate(total_week_smooth = sum(smoothed_week)) |>
  mutate(total_week = sum(current_week)) |>
  ungroup() |>
  mutate(week_pct = current_week / total_week) |>
  mutate(week_pct_smooth = smoothed_week/total_week_smooth) |>
  filter(total_week > 0) |>
  filter(str_detect(disease,"ACWY"))

quantiles <- socrata_pct |>
  reframe(qs = quantile(week_pct_smooth, probs = c(0.1, 0.9), na.rm = T)) |>
  pull()

median_val <- socrata_pct |>
  summarise(qs = median(week_pct_smooth, na.rm = T)) |>
  pull()

ggplot(socrata_pct) +
  geom_rect(aes(xmin = as.Date(-Inf), xmax = as.Date(Inf), ymin = !!quantiles[1], ymax = !!quantiles[2]),
            fill = met.brewer("Hokusai1", 7)[7], alpha = 0.5, color = met.brewer("Hokusai1", 7)[7],
            lwd = 1, linetype = "dashed") +
  geom_hline(aes(yintercept = median_val), linetype = "dashed", color = "#DBDBDB") +
  geom_point(aes(x = date, y = week_pct_smooth), color = met.brewer("Hokusai1", 7)[2], size = 3) +
  geom_point(aes(x = date, y = week_pct_smooth), color = "#DBDBDB", size = 1) +
  annotate("label", x = ymd("2022/01/01"), y = 0,
           label = glue("80% of smoothed-data\nin [{round(quantiles[1],2)},{round(quantiles[2],2)}]"),
           fill = "#DBDBDB", color = "#3D6D72", vjust = 0) +
  theme_bw() +
  labs(
    x = "",
    y = "Percent of cases of serogroups ACWY",
    title = "Positivity of serogroups A, C, W, and Y - Meningococcal disease",
    subtitle = "CDC Wonder / Socrata-API",
    caption = "4-week rolling mean"
  ) +
  theme(
    plot.background = element_rect(fill = "#3D6D72", color = NULL),
    panel.background = element_rect(fill = "#3D6D72"),
    panel.border = element_rect(color = "#DBDBDB", linetype = "solid", linewidth = 2),
    axis.text = element_text(color = "#DBDBDB"),
    axis.ticks = element_line(color = "#DBDBDB"),
    axis.line = element_line(color = "#DBDBDB", linewidth = 2),
    plot.title = element_text(color = "#DBDBDB", face = "bold"),
    plot.subtitle = element_text(color = "#DBDBDB", face = "italic"),
    plot.caption = element_text(color = "#DBDBDB", face = "italic"),
    axis.title =  element_text(color = "#DBDBDB"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill = "#DBDBDB"),
    legend.position = "bottom"
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = scales::percent_format())
ggsave(file.path("images","serogroups_percent_rollmean.pdf"), width = 8, height = 4, bg = "#3D6D72")


ggplot(socrata_pct) +
  geom_histogram(aes(x = week_pct_smooth, y = after_stat(density)),
                 fill = met.brewer("Hokusai1", 7)[2],
                 color = "#DBDBDB", lwd = 1, bins = 25) +
  theme_bw() +
  labs(
    x = "",
    y = "Percent of cases of serogroups ACWY",
    title = "Positivity of serogroups A, C, W, and Y - Meningococcal disease",
    subtitle = "CDC Wonder / Socrata-API",
    caption = "4-week rolling mean"
  ) +
  theme(
    plot.background = element_rect(fill = "#3D6D72", color = NULL),
    panel.background = element_rect(fill = "#3D6D72"),
    panel.border = element_rect(color = "#DBDBDB", linetype = "solid", linewidth = 2),
    axis.text = element_text(color = "#DBDBDB"),
    axis.ticks = element_line(color = "#DBDBDB"),
    axis.line = element_line(color = "#DBDBDB", linewidth = 2),
    plot.title = element_text(color = "#DBDBDB", face = "bold"),
    plot.subtitle = element_text(color = "#DBDBDB", face = "italic"),
    plot.caption = element_text(color = "#DBDBDB", face = "italic"),
    axis.title =  element_text(color = "#DBDBDB"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill = "#DBDBDB"),
    legend.position = "bottom"
  ) +
  scale_x_continuous(labels = scales::percent_format())
ggsave(file.path("images","serogroups_histogram.pdf"), width = 8, height = 4, bg = "#3D6D72")


#Vaccination plots----
vaccination <- read_rds(file.path("data","processed","vaccination_from_survey.rds"))
vaccination |> write_excel_csv(file.path("data","paper","vaccination_rates.csv"))
vaccination |> write_rds(file.path("data","paper","vaccination_rates.rds"))

vaccination |>
  mutate(year_season = as.numeric(year_season)) |>
  mutate(across(matches("ci"), as.numeric)) |>
  ggplot() +
  geom_errorbar(aes(x = year_season, ymin = lower_ci/100, ymax = upper_ci/100),
                color = met.brewer("Hokusai1", 7)[2]) +
  geom_line(aes(x = year_season, y = coverage_estimate/100), linetype = "solid",
             color = met.brewer("Hokusai1", 7)[7], lwd = 0.5) +
  geom_point(aes(x = year_season, y = coverage_estimate/100),
             color = met.brewer("Hokusai1", 7)[2], size = 3) +
  geom_point(aes(x = year_season, y = coverage_estimate/100),
             color = "#DBDBDB", size = 1) +
  theme_bw() +
  labs(
    x = "",
    y = "Coverage with at least 1 dose",
    title = "Vaccination coverage of at least one dose of MenACWY vaccine",
    subtitle = "National Center for Immunization and Respiratory Diseases (NCIRD)",
  ) +
  theme(
    plot.background = element_rect(fill = "#3D6D72", color = NULL),
    panel.background = element_rect(fill = "#3D6D72"),
    panel.border = element_rect(color = "#DBDBDB", linetype = "solid", linewidth = 2),
    axis.text = element_text(color = "#DBDBDB"),
    axis.ticks = element_line(color = "#DBDBDB"),
    axis.line = element_line(color = "#DBDBDB", linewidth = 2),
    plot.title = element_text(color = "#DBDBDB", face = "bold"),
    plot.subtitle = element_text(color = "#DBDBDB", face = "italic"),
    plot.caption = element_text(color = "#DBDBDB", face = "italic"),
    axis.title =  element_text(color = "#DBDBDB"),
    panel.grid.major = element_line(color = "#1A2F31", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill = "#DBDBDB"),
    legend.position = "bottom"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_continuous(breaks = 2000:2024)
ggsave(file.path("images","vaccination.pdf"), width = 8, height = 4, bg = "#3D6D72")
