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
    astuff_plus_scaled >-5 & astuff_plus_scaled < 5 #This (broadly) removes anomalous pitches
  ) %>%
  mutate(
    home_batting = ifelse(batting_team == home_team, 1, 0),
    roof = ifelse(other_weather == "Roof Closed", 1, 0)
    )






#### ESTABLISH WEIGHTS FOR WIND SPEED AND AIR DENSITY ####

#Fit multinomial log using sunlight_category
propensity_model <- multinom(
  sunlight_category ~ air_density_scaled + wind_speed_scaled,
  data = team_data
)

#Propensity scores
team_data <- team_data %>%
  mutate(
    propensity_scores = predict(propensity_model, type = "probs")
  )

#IPW
team_data <- team_data %>%
  mutate(
    reweights = 1 / propensity_scores[cbind(1:nrow(team_data), as.numeric(as.factor(sunlight_category)))]
  ) %>%
  mutate(
    reweights = reweights / mean(reweights, na.rm = TRUE) 
  )


#### MODELS ####

#Unweighted model
initial_model <- gam(
  whiff ~ te(altitude,azimuth) + #sun position 
    
    s(air_density_scaled) + s(wind_speed_scaled) + roof +  #environment

    leverage_category + strikes + starting_pitcher*times_faced + p_throws*stand + home_batting + #situationals
    
    astuff_plus_scaled + pitch_class + zone_class + #pitch level
    
    month + game_year + night_game + #time level
    
    s(batter, bs = "re") #random effects for batter
  ,
  family = binomial,
  data = team_data  
)
summary(initial_model)

#Weighted model
final_model <- gam(
  whiff ~ te(altitude,azimuth) + #sun position 
    
    s(air_density_scaled) + s(wind_speed_scaled) + roof +  #environment
    
    leverage_category + strikes + starting_pitcher*times_faced + p_throws*stand + home_batting + #situationals
    
    astuff_plus_scaled + pitch_class + zone_class + #pitch level
    
    month + game_year + night_game + #time level
    
    s(batter, bs = "re") #random effects for batter
  ,
  family = binomial,
  weights = reweights,
  data = team_data
)
summary(final_model)


#### PREDICT AND PLOT ####

#Predict whiffs
team_data <- team_data %>%
  mutate(predicted_whiff_unweighted = predict(initial_model, newdata = team_data, type = "response"),
         predicted_whiff_weighted = predict(final_model, newdata = team_data, type = "response"))


#Plot predicted whiff rate by altitude
ggplot(team_data, aes(x = altitude, y = predicted_whiff_weighted, color = predicted_whiff_weighted)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "#E20074", span = .5) + #This chart is brought to you by T-Mobile--what can Magenta do for you
  scale_color_gradientn(colors = c("#0C2C56", "#C4CED4", "#005C5C")) + 
  labs(
    title = "Estimated whiff rate by altitude",
    subtitle = "T-Mobile Park (2021-2023)",
    x = "Altitude (degrees)",
    y = "Whiff probability (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  guides(color = "none")
















#### NIGHT GAMES ####

#Filter for games starting after 6 pm PDT
team_data_night <- team_data %>%
  filter(night_game == 1)


#### ESTABLISH WEIGHTS FOR WIND SPEED AND AIR DENSITY ####

#Fit binomial log using sun_is_up
propensity_model <- glm(
  sun_is_up ~ air_density_scaled + wind_speed_scaled,
  family = binomial,
  data = team_data_night
)

#Propensity scores
team_data_night <- team_data_night %>%
  mutate(propensity_scores = predict(propensity_model, type = "response"))

#IPW
team_data_night <- team_data_night %>%
  mutate(
    reweights = ifelse(
      sun_is_up == 1,
      1 / propensity_scores,        # For sun_is_up = 1
      1 / (1 - propensity_scores)   # For sun_is_up = 0
    )
  ) %>%
  mutate(reweights = reweights / mean(reweights, na.rm = TRUE))



#### MODELS ####

#Unweighted model
initial_model <- gam(
  whiff ~ te(altitude,azimuth) + #sun position 
    
    s(air_density_scaled) + s(wind_speed_scaled) + roof +  #environment
    
    leverage_category + strikes + starting_pitcher*times_faced + p_throws*stand + home_batting + #situationals
    
    astuff_plus_scaled + pitch_class + zone_class + #pitch level
    
    month + game_year + night_game + #time level
    
    s(batter, bs = "re") #random effects for batter
  ,
  family = binomial,
  data = team_data_night  
)
summary(initial_model)

#Weighted model
final_model <- gam(
  whiff ~ te(altitude,azimuth) + #sun position 
    
    s(air_density_scaled) + s(wind_speed_scaled) + roof +  #environment
    
    leverage_category + strikes + starting_pitcher*times_faced + p_throws*stand + home_batting + #situationals
    
    astuff_plus_scaled + pitch_class + zone_class + #pitch level
    
    month + game_year +  #time level
    
    s(batter, bs = "re") #random effects for batter
  ,
  family = binomial,
  weights = reweights,
  data = team_data_night
)
summary(final_model)


#### PREDICT AND PLOT ####

#Predict whiffs
team_data_night <- team_data_night %>%
  mutate(predicted_whiff_unweighted = predict(initial_model, newdata = team_data_night, type = "response"),
         predicted_whiff_weighted = predict(final_model, newdata = team_data_night, type = "response"))


#Plot predicted whiff rate by altitude
ggplot(team_data_night, aes(x = altitude, y = predicted_whiff_weighted, color = predicted_whiff_weighted)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "#E20074", span = .5) +
  scale_color_gradientn(colors = c("#0C2C56", "#C4CED4", "#005C5C")) + 
  labs(
    title = "Estimated whiff rate by altitude",
    subtitle = "T-Mobile Park, night games (2021-2023)",
    x = "Altitude (degrees)",
    y = "Whiff Probability (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  guides(color = "none")
