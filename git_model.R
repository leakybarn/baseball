library(dplyr)
library(nnet)
library(mgcv)
library(MatchIt)
library(cobalt)
library(ggplot2)

#### PREPARE DATA ####

#Read CSV files
team_data_2021 <- read.csv("t_mobile_park_sun_2021.csv")
team_data_2022 <- read.csv("t_mobile_park_sun_2022.csv")
team_data_2023 <- read.csv("t_mobile_park_sun_2023.csv")

#Combine
team_data <- bind_rows(team_data_2021, team_data_2022, team_data_2023)

#Scale
team_data$release_speed_scaled <- scale(team_data$release_speed)
team_data$release_spin_rate_scaled <- scale(team_data$release_spin_rate)
team_data$air_density_scaled <- scale(team_data$air_density)
team_data$wind_speed_scaled <- scale(team_data$wind_speed)
team_data$wind_deg_scaled <- scale(team_data$wind_deg)
team_data$temp_scaled <- scale(team_data$temp)
team_data$humidity_scaled <- scale(team_data$humidity)
team_data$pressure_scaled <- scale(team_data$pressure)
team_data$astuff_plus_scaled <- scale(team_data$astuff_plus)

#Factor
team_data$pitch_class <- as.factor(team_data$pitch_class)
team_data$zone_class <- as.factor(team_data$zone_class)
team_data$sun_is_up <- as.factor(team_data$sun_is_up)
team_data$times_faced <- as.factor(team_data$times_faced)
team_data$leverage_category <- as.factor(team_data$leverage_category)
team_data$starting_pitcher <- as.factor(team_data$starting_pitcher)
team_data$p_throws <- as.factor(team_data$p_throws)
team_data$stand <- as.factor(team_data$stand)
team_data$strikes <- as.factor(team_data$strikes)
team_data$weather_main <- as.factor(team_data$weather_main)
team_data$month <- as.factor(team_data$month)
team_data$game_year <- as.factor(team_data$game_year)

#Remove NAs
team_data <- team_data %>%
  filter(complete.cases(
    sunlight_category, altitude, azimuth,
    temp, pressure, humidity, wind_speed,
    leverage_category, strikes, starting_pitcher, times_faced, p_throws, stand,
    pitch_class, zone_class, release_speed, release_spin_rate, astuff_plus,
    month, game_year, batter, pitcher
  ))

#Filter 
team_data <- team_data %>%
  filter(
    swing == 1,
    inning < 9,
    night_game == 1,
    !month %in% c(3, 10),  # Simplify the month condition
    !(game_date %in% c("2022-06-18", "2022-08-06")), # remove specific doubleheaders for Mariners
    astuff_plus_scaled >-5 & astuff_plus_scaled < 5 #This (broadly) removes anomalous pitches
  ) %>%
  mutate(
    home_batting = ifelse(batting_team == home_team, 1, 0),
    roof = ifelse(other_weather == "Roof Closed", 1, 0)
  )

#### MODELS ####

#Unmatched model
initial_model <- gam(
  whiff ~ te(altitude,azimuth) +
    
    te(air_density_scaled, wind_speed_scaled) +
    te(wind_speed_scaled, wind_deg_scaled, by = sun_is_up) +
    
    te(astuff_plus_scaled, wind_speed_scaled) + 
    
    s(air_density_scaled) +
    s(wind_speed_scaled) + 
    s(wind_deg_scaled) +
    
    other_weather +
    
    starting_pitcher + times_faced + p_throws + stand + leverage + home_batting+
    
    astuff_plus_scaled + pitch_class + zone_class + strikes +
    
    month + game_year +
    
    te(batter, bs = "re")
  ,
  family = binomial,
  data = team_data  
)
summary(initial_model)

#Matched model
matched_data <- matchit(
  sun_is_up ~ air_density_scaled + wind_speed_scaled + wind_deg_scaled, 
  data = team_data, 
  method = "nearest", 
  distance = "logit", 
  caliper = 0.1 
)
team_data_matched <- match.data(matched_data)
love.plot(matched_data)

matched_model <- gam(
  whiff ~ te(altitude,azimuth) +
    
    te(air_density_scaled, wind_speed_scaled) +
    te(wind_speed_scaled, wind_deg_scaled, by = sun_is_up) +
    
    te(astuff_plus_scaled, wind_speed_scaled) + 
    
    s(air_density_scaled) +
    s(wind_speed_scaled) + 
    s(wind_deg_scaled) +
    
    other_weather+
    
    starting_pitcher + times_faced + p_throws + stand + home_batting +
    
    astuff_plus_scaled + pitch_class + zone_class + strikes +
    
    month + game_year +
    
    s(batter, bs = "re")
  ,
  family = binomial,
  data = team_data_matched
)
summary(matched_model)
