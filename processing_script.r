library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(openxlsx)
library(htmlwidgets)
library(htmltools)
library(plotly)

# reading metadata
df_ReadingSession <- read.csv(unz("data.zip", "Kindle.Devices.ReadingSession/Kindle.Devices.ReadingSession.csv"))
df_ReadingInsights <- read.csv(unz("data.zip", "Kindle.ReadingInsights/datasets/Kindle.reading-insights-sessions_with_adjustments/Kindle.reading-insights-sessions_with_adjustments.csv"))

# importing sideloaded metadata
df_DocumentMetadata <- read.csv(unz("data.zip", "Kindle.KindleDocs/datasets/Kindle.KindleDocs.DocumentMetadata/Kindle.KindleDocs.DocumentMetadata.csv"))

# importing Kindle store metadata
df_ULI <- read.csv(unz("data.zip", "Kindle.UnifiedLibraryIndex/datasets/Kindle.UnifiedLibraryIndex.CustomerRelationshipIndex/Kindle.UnifiedLibraryIndex.CustomerRelationshipIndex.csv"))
df_Whispersync <- read.csv(unz("data.zip", "Digital.Content.Whispersync/whispersync.csv"))

# producing metadata lookup dataframe -----------------------------
l_AllBooks <- unique(df_ReadingSession$ASIN)

# first, sideloaded
df_SideloadedMetadata <- df_DocumentMetadata %>%
  filter(DocumentId %in% l_AllBooks) %>%
  select(ASIN = DocumentId, Title, DocumentProvider) %>%
  rename(Author = DocumentProvider)

# second, kindle store detailed
df_KindleStore1 <- df_ULI %>%
  filter(ASIN %in% l_AllBooks) %>%
  select(ASIN = ASIN, Product.Name, Sortable.Author.Name) %>%
  rename(
    Author = Sortable.Author.Name,
    Title = Product.Name
  )

# third, kindle store books from whispersync data
df_KindleStore2 <- df_Whispersync %>%
  filter(ASIN %in% l_AllBooks) %>%
  select(ASIN = ASIN, Product.Name) %>%
  rename(Title = Product.Name)

# Create df_MetadataLookup from sideloaded metadata
# function to record Not Available as NAs for accuate coalesce
na_if_not_available <- function(df) {
  df %>%
    mutate(across(c(Title, Author), ~ na_if(., "Not Available")))
}

df_SideloadedMetadata <- na_if_not_available(df_SideloadedMetadata)
df_KindleStore1 <- na_if_not_available(df_KindleStore1)
df_KindleStore2 <- df_KindleStore2 %>% # doesn't contain author details
  mutate(Title = na_if(Title, "Not Available"))

# Deduplicate metadata sources to ensure one row per ASIN
df_SideloadedMetadata <- df_SideloadedMetadata %>%
  distinct(ASIN, .keep_all = TRUE)
df_KindleStore1 <- df_KindleStore1 %>%
  distinct(ASIN, .keep_all = TRUE)
df_KindleStore2 <- df_KindleStore2 %>%
  distinct(ASIN, .keep_all = TRUE)

# joining all metadata together, in order of priority

df_ASINs <- data.frame(ASIN = l_AllBooks)
df_merged <- df_ASINs %>%
  left_join(df_SideloadedMetadata, by = "ASIN") %>%
  left_join(df_KindleStore1, by = "ASIN", suffix = c("", ".ks1")) %>%
  left_join(df_KindleStore2, by = "ASIN", suffix = c("", ".ks2")) %>%
  mutate(
    Title = coalesce(Title, Title.ks1, Title.ks2),
    Author = coalesce(Author, Author.ks1)
  ) %>%
  select(ASIN, Title, Author)

# compiling reading insights ----------------------------------------
# see https://jakelee.co.uk/analysing-5-years-of-amazon-kindle-reading/

# calculate total reading hours
s_TotalHours <- sum(df_ReadingInsights$total_reading_milliseconds) / 3600000
s_TotalPages <- sum(as.numeric(df_ReadingSession$number_of_page_flips), na.rm = TRUE)

# total and average reading minutes per year

df_ReadingSession$start_timestamp_p <- ymd_hms(df_ReadingSession$start_timestamp, tz = "UTC")

df_ReadPerYear <- df_ReadingSession %>%
  filter(!is.na(start_timestamp_p), !is.na(total_reading_millis)) %>%
  mutate(
    year = year(start_timestamp_p), # from lubridate
    reading_minutes = as.numeric(total_reading_millis) / 60000
  ) %>%
  group_by(year) %>%
  summarise(
    total_minutes = sum(reading_minutes, na.rm = TRUE),
    avg_minutes   = mean(reading_minutes, na.rm = TRUE),
    session_count = n()
  )

p_ReadPerYear <- ggplot(df_ReadPerYear, aes(x = year, y = total_minutes)) +
  geom_col() +
  labs(
    title = "Total reading time per year (mins)",
    x = "Years", y = "Minutes",
    caption = "Data from Amazon. Analysis by Will Dixon based on a design by Jake Lee."
  )

# total and average reading time per hour of day
df_ReadPerHour <- df_ReadingSession %>%
  filter(!is.na(start_timestamp_p), !is.na(total_reading_millis)) %>%
  mutate(
    hour_of_day = hour(start_timestamp_p), # from lubridate
    reading_minutes = as.numeric(total_reading_millis) / 60000
  ) %>%
  group_by(hour_of_day) %>%
  summarise(
    total_minutes = sum(reading_minutes, na.rm = TRUE),
    avg_minutes   = mean(reading_minutes, na.rm = TRUE),
    session_count = n()
  )

p_ReadPerHour <- ggplot(df_ReadPerHour, aes(x = hour_of_day, y = total_minutes)) +
  geom_col() +
  labs(
    title = "Total reading time per hour of day (mins)",
    x = "Hour", y = "", color = "Metric",
    caption = "Data from Amazon. Analysis by Will Dixon based on a design by Jake Lee."
  )
# pages read per day throughout time, quite inefficient.
# initial filtering
df_ReadingSession$end_timestamp <- as.POSIXct(df_ReadingSession$end_timestamp)
df_ReadPerDay <- df_ReadingSession %>%
  filter(!is.na(end_timestamp), !is.na(number_of_page_flips))

df_ReadPerWeek <- df_ReadingSession %>%
  filter(!is.na(end_timestamp), !is.na(number_of_page_flips), !is.na(total_reading_millis)) %>%
  mutate(
    end_timestamp = as.POSIXct(end_timestamp),
    week = floor_date(end_timestamp, unit = "week"),
    total_reading_millis = as.numeric(total_reading_millis) / 60000 # convert ms to minutes
  ) %>%
  group_by(week) %>%
  summarise(
    total_page_flips = sum(as.numeric(number_of_page_flips), na.rm = TRUE),
    total_reading_minutes = sum(total_reading_millis, na.rm = TRUE)
  ) %>%
  ungroup()

# This was a rather crude way of chopping down my dataset for some anomalous
# entries. The number 5 is quite aribtrary. Apologies.
df_ReadPerWeek_anomexcl <- df_ReadPerWeek %>%
  mutate(year = year(week)) %>%
  group_by(year) %>%
  filter(n() >= 5) %>%
  ungroup()

p_ReadPerWeek <- ggplot(df_ReadPerWeek_anomexcl, aes(x = week, y = total_reading_minutes)) +
  geom_line() +
  labs(
    title = "Time spent reading as an average per week",
    subtitle = "For years that have >5 weeks of reading data",
    x = "Week", y = "",
    caption = "Data from Amazon. Analysis by Will Dixon based on a design by Jake Lee."
  )
# average time spent reading per month in a year
df_ReadPerMonth <- df_ReadPerWeek_anomexcl %>%
  mutate(month = month(week)) %>%
  group_by(year, month) %>%
  summarise(monthly_total_minutes = sum(total_reading_minutes, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(month) %>%
  summarise(avg_monthly_minutes = mean(monthly_total_minutes, na.rm = TRUE)) %>%
  mutate(month_label = month.abb[month]) %>%
  mutate(month_label = factor(month_label, levels = month.abb))

# plots the average across each month
p_ReadPerMonth <- ggplot(df_ReadPerMonth, aes(x = month_label, y = avg_monthly_minutes, group = 1)) +
  geom_col() +
  labs(
    title = "Average monthly reading time",
    subtitle = "Averaged across all years with >5 weeks of reading data",
    x = "Month",
    y = "Average Total Reading Minutes",
    caption = "Data from Amazon. Analysis by Will Dixon based on a design by Jake Lee."
  )

# exporting reading insights (temporary) ---------------------------------------------------
# creating data export for excel

wb <- createWorkbook()

addWorksheet(wb, "ReadPerYear")
writeData(wb, "ReadPerYear", df_ReadPerYear)
addWorksheet(wb, "ReadPerHour")
writeData(wb, "ReadPerHour", df_ReadPerHour)
addWorksheet(wb, "ReadPerWeek")
writeData(wb, "ReadPerWeek", df_ReadPerWeek_anomexcl)
addWorksheet(wb, "ReadPerMonth")
writeData(wb, "ReadPerMonth", df_ReadPerMonth)

saveWorkbook(wb, "export.xlsx", overwrite = TRUE)


## BELOW IS WIP --------------------------------------------------------------------------

# creating an interactive output
# Convert ggplot objects to interactive Plotly objects
p1_interactive <- ggplotly(p_ReadPerYear)
p2_interactive <- ggplotly(p_ReadPerHour)
p3_interactive <- ggplotly(p_ReadPerWeek)
p4_interactive <- ggplotly(p_ReadPerMonth)

# turn all of the interactive plots into a html page
combined_plots <- tagList(
  h1("Interactive Kindle Reading Insights"),
  h2("Total Reading Time per Year"),
  p1_interactive,
  hr(),
  h2("Total Reading Time per Hour"),
  p2_interactive,
  hr(),
  h2("Weekly Average Reading Time"),
  p3_interactive,
  hr(),
  h2("Monthly Average Reading Time"),
  p4_interactive
)

# save visualisations to a html file for now
saveWidget(browsable(combined_plots), file = "plots.html", selfcontained = TRUE)

# open the exported file
browseURL("plots.html")
