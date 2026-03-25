library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(patchwork)
library(scales)
library(mgcv)

df <- read.csv("data/analysis_2024.csv") |>
  mutate(
    date       = as.Date(date),
    month_name = factor(month(date, label = TRUE, abbr = TRUE),
                        levels = month.abb),
    season = case_when(
      month %in% c(12,1,2) ~ "Winter",
      month %in% c(3,4,5)  ~ "Spring",
      month %in% c(6,7,8)  ~ "Summer",
      TRUE                  ~ "Fall"
    ) |> factor(levels = c("Winter","Spring","Summer","Fall")),
    weekday_name = factor(weekdays(date),
                          levels = c("Monday","Tuesday","Wednesday",
                                     "Thursday","Friday","Saturday","Sunday"))
  )

theme_clean <- theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 12))

# fig1 - time series
ts_mh <- ggplot(df, aes(date, mental_health)) +
  geom_line(color = "#4E79A7", linewidth = 0.4, alpha = 0.7) +
  geom_smooth(method = "loess", span = 0.2, se = TRUE,
              color = "#E15759", fill = "#E15759", alpha = 0.15) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  labs(title = "A  Daily Mental Health Crisis Calls (EDP + ALTMEN)",
       x = NULL, y = "Daily count") +
  theme_clean

ts_sui <- ggplot(df, aes(date, suicide)) +
  geom_col(fill = "#F28E2B", alpha = 0.7, width = 1) +
  geom_smooth(method = "loess", span = 0.3, se = TRUE,
              color = "#59A14F", fill = "#59A14F", alpha = 0.2) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  labs(title = "B  Daily Suicide-Related Calls (JUMPDN + JUMPUP)",
       x = NULL, y = "Daily count") +
  theme_clean

ts_pm <- ggplot(df, aes(date, pm25)) +
  geom_point(color = "#76B7B2", size = 1.2, alpha = 0.8) +
  geom_smooth(method = "loess", span = 0.3, se = FALSE,
              color = "#B07AA1", linewidth = 1) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  labs(title = "C  Daily PM2.5 (μg/m³) — measured days only (n = 124)",
       x = "2024", y = "PM2.5 (μg/m³)") +
  theme_clean

p1 <- ts_mh / ts_sui / ts_pm
ggsave("figures/fig1_timeseries.png", p1, width = 10, height = 9, dpi = 150)

# fig2 - seasonal boxplots
season_mh <- ggplot(df, aes(season, mental_health, fill = season)) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.8) +
  scale_fill_manual(values = c("#4E79A7","#59A14F","#E15759","#F28E2B")) +
  labs(title = "A  Mental Health Calls by Season",
       x = NULL, y = "Daily count") +
  theme_clean + theme(legend.position = "none")

season_sui <- ggplot(df, aes(season, suicide, fill = season)) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.8) +
  scale_fill_manual(values = c("#4E79A7","#59A14F","#E15759","#F28E2B")) +
  labs(title = "B  Suicide Calls by Season",
       x = NULL, y = "Daily count") +
  theme_clean + theme(legend.position = "none")

season_pm <- ggplot(df, aes(season, pm25, fill = season)) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.8) +
  scale_fill_manual(values = c("#4E79A7","#59A14F","#E15759","#F28E2B")) +
  labs(title = "C  PM2.5 by Season",
       x = NULL, y = "μg/m³") +
  theme_clean + theme(legend.position = "none")

p2 <- season_mh | season_sui | season_pm
ggsave("figures/fig2_seasonal.png", p2, width = 11, height = 4.5, dpi = 150)

# fig3 - pm25 scatter
scat_mh <- ggplot(df, aes(pm25, mental_health)) +
  geom_point(aes(color = season), alpha = 0.45, size = 1.2) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 6),
              color = "black", linewidth = 1, se = TRUE, fill = "gray70", alpha = 0.3) +
  scale_color_manual(values = c("#4E79A7","#59A14F","#E15759","#F28E2B")) +
  labs(title = "A  PM2.5 vs Mental Health Calls",
       x = "PM2.5 (μg/m³)", y = "Daily count", color = "Season") +
  theme_clean

scat_sui <- ggplot(df, aes(pm25, suicide)) +
  geom_point(aes(color = season), alpha = 0.45, size = 1.2) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 6),
              color = "black", linewidth = 1, se = TRUE, fill = "gray70", alpha = 0.3) +
  scale_color_manual(values = c("#4E79A7","#59A14F","#E15759","#F28E2B")) +
  labs(title = "B  PM2.5 vs Suicide Calls",
       x = "PM2.5 (μg/m³)", y = "Daily count", color = "Season") +
  theme_clean

p3 <- scat_mh | scat_sui
ggsave("figures/fig3_scatter_pm25.png", p3, width = 11, height = 5, dpi = 150)

# fig4 - temp scatter
temp_mh <- ggplot(df, aes(tmean, mental_health)) +
  geom_point(aes(color = season), alpha = 0.4, size = 1.2) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 8),
              color = "black", linewidth = 1, se = TRUE, fill = "gray70", alpha = 0.3) +
  scale_color_manual(values = c("#4E79A7","#59A14F","#E15759","#F28E2B")) +
  labs(title = "A  Temperature vs Mental Health Calls",
       x = "Mean temperature (°C)", y = "Daily count", color = "Season") +
  theme_clean

temp_sui <- ggplot(df, aes(tmean, suicide)) +
  geom_point(aes(color = season), alpha = 0.4, size = 1.2) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 8),
              color = "black", linewidth = 1, se = TRUE, fill = "gray70", alpha = 0.3) +
  scale_color_manual(values = c("#4E79A7","#59A14F","#E15759","#F28E2B")) +
  labs(title = "B  Temperature vs Suicide Calls",
       x = "Mean temperature (°C)", y = "Daily count", color = "Season") +
  theme_clean

p4 <- temp_mh | temp_sui
ggsave("figures/fig4_scatter_temp.png", p4, width = 11, height = 5, dpi = 150)

# fig5 - day of week
dow_mh <- df |>
  group_by(weekday_name) |>
  summarise(mean_mh = mean(mental_health), se = sd(mental_health)/sqrt(n()))

dow_sui <- df |>
  group_by(weekday_name) |>
  summarise(mean_sui = mean(suicide), se = sd(suicide)/sqrt(n()))

p5a <- ggplot(dow_mh, aes(weekday_name, mean_mh)) +
  geom_col(fill = "#4E79A7", alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_mh - se, ymax = mean_mh + se), width = 0.3) +
  labs(title = "A  Mental Health Calls by Day of Week",
       x = NULL, y = "Mean daily count") +
  theme_clean + theme(axis.text.x = element_text(angle = 30, hjust = 1))

p5b <- ggplot(dow_sui, aes(weekday_name, mean_sui)) +
  geom_col(fill = "#F28E2B", alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_sui - se, ymax = mean_sui + se), width = 0.3) +
  labs(title = "B  Suicide Calls by Day of Week",
       x = NULL, y = "Mean daily count") +
  theme_clean + theme(axis.text.x = element_text(angle = 30, hjust = 1))

p5 <- p5a | p5b
ggsave("figures/fig5_dayofweek.png", p5, width = 10, height = 4.5, dpi = 150)

# quick summary
cat("\n=== Descriptive Statistics ===\n")
df |> select(mental_health, suicide, pm25, tmean, tmax, precip) |>
  summary() |> print()

cat("\nCorrelation with mental_health:\n")
cor(df[,c("mental_health","suicide","pm25","tmean","tmax","is_weekend")],
    use = "complete.obs") |> round(3) |> print()
