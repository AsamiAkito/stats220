library(tidyverse)
library(lubridate)
library(ggthemes)
library(stringr)

logged_data <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQNfffI7pW3SpAGlO3bBGx6RQBoG4CL9OrQ7sChMHXvrgWci22OVJ4MPqxwPWAexEj4PD3XyQ76WA9N/pub?output=csv") %>%
  rename(
    game = "Do you play Game?",
    play_time = "How many hours do you usually play games?",
    platform = "On what platform do you usually play games?"
  ) %>%
  mutate(
    Timestamp = mdy_hms(Timestamp),
    play_intensity = cut(play_time,
                         breaks = c(0, 10, 30, Inf),
                         labels = c("Casual", "Moderate", "Hardcore")),
    platform_pc = str_detect(platform, regex("PC", ignore_case = TRUE)),
    platform_mobile = str_detect(platform, regex("mobile", ignore_case = TRUE)),
    platform_console = str_detect(platform, regex("PS|Xbox|Switch", ignore_case = TRUE))
  )

time_plot <- logged_data %>%
  mutate(weekday = wday(Timestamp, label = TRUE)) %>%
  count(weekday) %>%
  ggplot(aes(x = weekday, y = n)) +
  geom_line(color = "#1F77B4", group = 1, linewidth = 1.5) +
  geom_point(color = "#FF7F0E", size = 3) +
  labs(title = "Data Collection Timeline",
       x = "Day of Week",
       y = "Observations Collected") +
  theme_economist()

platform_intensity <- logged_data %>%
  separate_rows(platform, sep = ",") %>%
  mutate(platform = str_trim(platform)) %>%
  filter(platform != "") %>%
  group_by(platform, play_intensity) %>%
  summarise(count = n(), .groups = "drop") %>%
  ggplot(aes(x = platform, y = play_intensity, fill = count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#F0E442", high = "#D55E00") +
  labs(title = "Gaming Intensity by Platform",
       x = "Platform",
       y = "Play Intensity") +
  theme_minimal()

duration_plot <- logged_data %>%
  ggplot(aes(x = play_intensity, y = play_time, fill = play_intensity)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Play Time Distribution by Intensity Level",
       x = "Intensity Category",
       y = "Hours/Week") +
  coord_flip()

platform_summary <- logged_data %>%
  separate_rows(platform, sep = ",") %>%
  mutate(platform = str_trim(platform)) %>%
  group_by(platform) %>%
  summarise(avg_play_time = mean(play_time, na.rm = TRUE)) %>%
  arrange(desc(avg_play_time))

platform_plot <- ggplot(platform_summary, aes(x = reorder(platform, avg_play_time), y = avg_play_time)) +
  geom_col(fill = "#66C2A5") +
  labs(title = "Average Weekly Play Time per Platform", x = "Platform", y = "Avg Hours/Week") +
  theme_minimal()

ggsave("plot1.png", time_plot, width = 8, height = 5)
ggsave("plot2.png", platform_intensity, width = 7, height = 5)
ggsave("plot3.png", duration_plot, width = 6, height = 6)
ggsave("plot4.png", platform_plot, width = 7, height = 5)