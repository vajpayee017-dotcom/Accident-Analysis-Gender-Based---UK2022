# =============================================================================
# Are Women Worse Drivers Than Men?
# Let's find out using UK road data from 2022.
# =============================================================================

# First, get the tools we need.
install.packages("stats19")   # to download official UK crash data
install.packages("dplyr")     # to clean and work with data
library(stats19)
library(dplyr)

# -----------------------------------------------------------------------------
# 1. Download the vehicle data for 2022
# -----------------------------------------------------------------------------

# Grab the vehicle table – every vehicle involved in a reported accident.
vehicles_2022 <- get_stats19(year = 2022, type = "vehicle")

# Check what columns we have.
colnames(vehicles_2022)

# See all vehicle types – we only care about cars.
vehicles_2022 %>%
  count(vehicle_type) %>%
  arrange(desc(n))

# -----------------------------------------------------------------------------
# 2. Keep only cars, and look at driver gender
# -----------------------------------------------------------------------------

# Filter for cars.
cars_2022 <- vehicles_2022 %>%
  filter(vehicle_type == "Car")

# Quick check – only cars left?
cars_2022 %>%
  count(vehicle_type)

# Raw accident counts by gender (not adjusted for miles yet).
cars_2022 %>%
  count(sex_of_driver)

# Some genders are "Not known". Maybe left‑hand drive cars? Let's check.
vehicles_2022 %>%
  filter(vehicle_type == "Car") %>%
  filter(sex_of_driver == "Not known") %>%
  count(vehicle_left_hand_drive)
# Not really – we'll just drop them.

# -----------------------------------------------------------------------------
# 3. Clean up: keep only Male/Female
# -----------------------------------------------------------------------------

cars_clean <- cars_2022 %>%
  filter(sex_of_driver %in% c("Male", "Female"))

# Confirm counts.
cars_clean %>%
  count(sex_of_driver)

# -----------------------------------------------------------------------------
# 4. Get mileage and driver numbers – the "how much they drive" data
# -----------------------------------------------------------------------------

# Average miles per person per year (National Travel Survey).
miles_female_perhead <- 3806   # women
miles_male_perhead   <- 4593   # men

# Licensed drivers (DVLA, 2022).
drivers_female <- 19000000     # 19 million
drivers_male   <- 22000000     # 22 million

# Total miles driven by each gender.
total_miles_female <- miles_female_perhead * drivers_female
total_miles_male   <- miles_male_perhead   * drivers_male

# Look at those totals.
total_miles_female   # 72.3 billion miles
total_miles_male     # 101.0 billion miles

# -----------------------------------------------------------------------------
# 5. Accident counts (from cleaned data)
# -----------------------------------------------------------------------------

accidents_female <- 43502
accidents_male   <- 69905

# -----------------------------------------------------------------------------
# 6. Accident rates per billion miles – the fair comparison
# -----------------------------------------------------------------------------

rate_female <- accidents_female / total_miles_female * 1e9
rate_male   <- accidents_male   / total_miles_male   * 1e9

# Check them.
rate_female   # 601.6
rate_male     # 691.8

# Already interesting – female rate is lower.

# -----------------------------------------------------------------------------
# 7. Statistical test: is the difference real?
# -----------------------------------------------------------------------------

# Test if women's rate > men's (the original question).
prop.test(
  x = c(accidents_female, accidents_male),
  n = c(round(total_miles_female / 1e6), round(total_miles_male / 1e6)),
  alternative = "greater",
  correct = FALSE
)
# p ≈ 1 – no evidence.

# Test if women's rate < men's.
prop.test(
  x = c(accidents_female, accidents_male),
  n = c(round(total_miles_female / 1e6), round(total_miles_male / 1e6)),
  alternative = "less",
  correct = FALSE
)
# p < 2.2e-16 – overwhelming evidence women's rate is lower.

# -----------------------------------------------------------------------------
# 8. Bottom line
# -----------------------------------------------------------------------------
# After accounting for miles driven, women have a lower accident rate (601.6 vs 691.8).
# So no, women aren't worse drivers – the data says the opposite.

