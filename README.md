# Soil-Respiration-Spatial-Dataset-of-China
##To explore the response of Rs to temperature at the spatial scale in China, we averaged the annual values from each observation site, 
creating an annual dataset ("Annual Dataset.csv") and a monthly dataset ("Monthly Dataset.csv").
##We divided the entire temperature range into six intervals (-10 to 0°C, 0 to 5°C, 5 to 10°C, 10 to 13°C, 13 to 20°C, and 20 to 25°C). Basing the
monthly dataset, we randomly selected one site within each interval. 12 consecutive months of respiration data are available for these six sites.
So a new dataframe ("new.csv") is obtained, which is used to demonstrate Jensen's inequality (Figure 6).
##("-10-25.csv") is composed of annual-monthly scale datasets with common temperature intervals(-10 to 25°C).
##In order to test the relationship between annual soil respiration (Rs) and mean annual temperature (MAT) across different regions. We combined 
SRDB and the annual dataset ("Annual Dataset.csv") to form a global annual spatial dataset ("Global annual spatial dataset.csv").
