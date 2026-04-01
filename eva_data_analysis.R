#load the desired packages
library(tidyverse) #tidyverse "contains" ggplot2
library(jsonlite)
library(lubridate)

#files
input_file  <- "./eva_data.json"
output_file <- "./eva_data.csv"
graph_file  <- "./cumulative_eva_graph.png"

#1)Read JSON array into a tibble
eva_tbl <- jsonlite::fromJSON(input_file) %>% 
  as_tibble()

# 2) Convert types + drop missing duration/date
eva_tbl <- eva_tbl %>% 
  mutate(
    eva  = as.numeric(eva),
    date = ymd_hms(date, quiet = TRUE) ) %>% 
  filter(!is.na(duration), duration != "", !is.na(date))

#3) Write CSV (index=False equivalent)
readr::write_csv(eva_tbl, output_file)

# 4) Sort by date
eva_tbl <- eva_tbl %>% 
  arrange(date)


# 5) duration_hours + cumulative_time
eva_tbl <- eva_tbl %>% 
  mutate(
    duration_hours = {
      parts <- str_split(duration, ":", n = 2, simplify = TRUE)
      as.numeric(parts[, 1]) + as.numeric(parts[, 2]) / 60
    },
    cumulative_time = cumsum(duration_hours)
  )
# 6) Plot and save
cumulative_spacetime_plot <- ggplot(eva_tbl, aes(x = date, y = cumulative_time)) +
  geom_point() +
  geom_line() +
  labs(
    x = "Year",
    y = "Total time spent in space to date (hours)"
  ) +
  theme_minimal()

ggsave(graph_file, plot = cumulative_spacetime_plot, width = 9, height = 5, dpi = 300)
print(cumulative_spacetime_plot)


