# geom_pointless — Project README

## Research Question

Do short-term PM2.5 exposures predict increases in mental health-related EMS incidents
(psychiatric crises and suicide-related calls) in New York City in 2024?

---

## Data Sources

| File | Source | Description |
|------|--------|-------------|
| `data/ems_mh_2024.json` | [NYC Open Data — EMS Incident Dispatch Data](https://data.cityofnewyork.us/Public-Safety/EMS-Incident-Dispatch-Data/76xm-jjuj) | Daily EMS call counts by call type, filtered to mental health and suicide-related categories, 2024 |
| `data/pm25_2024.csv` | [EPA AQS API](https://aqs.epa.gov/data/api) | PM2.5 measurements (parameter 88101) from 7 monitoring stations across 4 NYC boroughs (Manhattan, Queens, Bronx, Brooklyn), 2024 |
| `data/weather_2024.csv` | [Open-Meteo Historical API](https://open-meteo.com) | Daily weather at NYC Central Park coordinates (40.78°N, 73.97°W): max/min/mean temperature, precipitation, wind speed, max relative humidity, 2024 |
| `data/analysis_2024.csv` | — | Merged dataset used for analysis (see below) |

---

## Data Preprocessing

Preprocessing was done in Python.

**EMS:** Pulled from the Socrata API with aggregation by date and call type. Two outcome variables were constructed:
- `mental_health`: daily total of `EDP`, `EDPC`, `EDPM`, `EDPE`, `EDPT`, `ALTMEN` calls (Emotionally Disturbed Person and Altered Mental Status)
- `suicide`: daily total of `JUMPDN`, `JUMPUP` calls (jump from height)

**PM2.5:** Downloaded from EPA AQS API for 4 NYC counties (FIPS: 005, 047, 061, 081). Only records with `validity_indicator = "Y"` were kept. On days with multiple station readings, the city-wide mean was taken. Since EPA FRM monitors sample every 3 days rather than daily, only days with actual measurements were kept (n = 124) — no interpolation was applied.

**Weather:** No cleaning needed.

**Merging:** The three datasets were joined by date, using PM2.5 measurement days as the index (n = 124). Four additional variables were created: `month`, `weekday`, `is_weekend`, and `heat_day` (1 if daily mean temp ≥ 90th percentile).

Final dataset: 124 rows × 14 columns, no missing values, covering 2024-01-01 to 2024-12-29.

---

## EDA

All plots were made in R (`eda.R`).

**fig1_timeseries.png**
Time series of all three variables across 2024. Mental health calls show a clear seasonal pattern with a spring peak. PM2.5 has a few sharp spikes (likely wildfire smoke events) but is otherwise low and stable.

**fig2_seasonal.png**
Boxplots by season for mental health calls, suicide calls, and PM2.5. Mental health calls are highest in spring; suicide calls are lowest in winter. PM2.5 runs highest in summer.

**fig3_scatter_pm25.png**
PM2.5 against each outcome, colored by season, with a GAM smooth. The relationship looks fairly flat for mental health calls; there is a slight positive trend for suicide calls. The seasonal coloring helps check whether apparent associations are just season effects.

**fig4_scatter_temp.png**
Temperature against each outcome. Both show a positive trend (r = 0.23 for mental health, r = 0.26 for suicide), with the effect leveling off at higher temperatures. Temperature is a clear confounder.

**fig5_dayofweek.png**
Mean calls by day of week. Mental health calls drop noticeably on weekends (~425 vs ~510 on weekdays), with a correlation of –0.69 with `is_weekend`. Day of week needs to be controlled for in any regression.

### Descriptive Statistics (n = 124)

| Variable | Min | Mean | Max |
|----------|-----|------|-----|
| mental_health (calls/day) | 368 | 486 | 601 |
| suicide (calls/day) | 1 | 5.3 | 14 |
| PM2.5 (μg/m³) | 1.8 | 6.8 | 23.2 |
| Mean temperature (°C) | –9.5 | 13.0 | 29.2 |

---

## File Structure

```
project/
├── data/
│   ├── ems_mh_2024.json        # raw EMS data from NYC Open Data API
│   ├── pm25_2024.csv           # raw PM2.5 data from EPA AQS API
│   ├── weather_2024.csv        # raw weather data from Open-Meteo
│   └── analysis_2024.csv       # merged dataset for analysis
├── figures/
│   ├── fig1_timeseries.png
│   ├── fig2_seasonal.png
│   ├── fig3_scatter_pm25.png
│   ├── fig4_scatter_temp.png
│   └── fig5_dayofweek.png
├── eda.R
├── README.md
└── final_project_geom_pointless.docx
```
