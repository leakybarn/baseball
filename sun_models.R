rm(list = ls(all.names = TRUE))
library(baseballr)
library(DBI)
library(tidyverse)
library(zoo)
library(mgcv)
library(gt)
library(caret)
library(sandwich)
library(lmtest)
library(lmerTest)
library(car)
library(lme4)

### FOR WRITING THE CSV
#sun_data <- swings_select %>%
#  filter(venue_name != "Guaranteed Rate Field" & venue_name != "Angel Stadium", 
#         game_year %in% c("2021", "2022", "2023") & 
#           month != 3 & month != 10) %>%
#  select(venue_name, batter, pitcher, batting_team, fielding_team, game_year, 
#         month, startTime_local, night_game, altitude, azimuth, sun_is_up, 
#         sunlight_category, pa, bip, swing, whiff, k, woba_value_true, 
#         estimated_woba_using_speedangle, xwoba_sum, inning, leverage_category, 
#         times_faced, starting_pitcher, p_throws, stand, strikes, pitch_class, 
#         zone_class, release_speed, release_spin_rate, temp, pressure, 
#         humidity, wind_speed, air_density)

#write.csv(sun_data, "sun_data.csv", row.names = FALSE)

sun_data <- read.csv("sun_data.csv") #enter your own path

swings<- sun_data %>%
  filter(swing == 1,
         venue_name == "T-Mobile Park",
         night_game == 1,
         inning <9) %>%
  mutate(mariners = ifelse(batting_team == "SEA", 1, 0))

str(swings)
table(swings$whiff)
colSums(is.na(swings))


# Scale
swings$release_speed <- scale(swings$release_speed)
swings$release_spin_rate <- scale(swings$release_spin_rate)
swings$air_density <- scale(swings$air_density)
swings$wind_speed <- scale(swings$wind_speed)
swings$temp <- scale(swings$temp)
swings$humidity <- scale(swings$humidity)
swings$pressure <- scale(swings$pressure)

# Convert pitch controls to factors
swings$pitch_class <- as.factor(swings$pitch_class)
swings$zone_class <- as.factor(swings$zone_class)


# Convert situation controls to factors
swings$mariners <- as.factor(swings$mariners)
swings$times_faced <- as.factor(swings$times_faced) 
swings$leverage_category <- as.factor(swings$leverage_category)
swings$p_throws <- as.factor(swings$p_throws)
swings$stand <- as.factor(swings$stand)
swings$strikes <- as.factor(swings$strikes) 

# Convert time controls to factors
swings$month <- as.factor(swings$month)
swings$game_year <- as.factor(swings$game_year)


# GAMM
model_gam <- gam(whiff ~ te(altitude, azimuth) + 
                   
                   pitch_class + zone_class + release_speed + release_spin_rate + #pitch controls
                   mariners + times_faced + leverage_category + p_throws + stand + strikes + #situation controls
                   month + game_year + #time controls
                   temp + humidity + pressure + wind_speed + #weather controls
                   s(batter, bs = "re") + s(pitcher, bs = "re") #random effects
                 ,
                 family = binomial,
                 data = swings)

summary(model_gam)
#plot(model_gam, shade = TRUE)


control <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
model_glmm <- glmer(whiff ~ sun_is_up +  
                      
                      pitch_class + zone_class + release_speed + release_spin_rate + 
                      mariners + times_faced + leverage_category + strikes + p_throws + stand + 
                      month + 
                      game_year +
                      air_density + wind_speed + 
                      (1 | batter) + (1 | pitcher)
                    , 
                    family = binomial,
                    
                    data = swings,
                    control = control)

summary(model_glmm)
vif(model_glmm) 













pas <- sun_data %>%
  filter(pa == 1,
         venue_name == "T-Mobile Park",
         night_game == 1,
         inning <9) %>%
  mutate(mariners = ifelse(batting_team == "SEA", 1, 0))

str(pas)
table(pas$k)
colSums(is.na(pas))


#Scale
pas$release_speed <- scale(pas$release_speed)
pas$release_spin_rate <- scale(pas$release_spin_rate)
pas$air_density <- scale(pas$air_density)
pas$wind_speed <- scale(pas$wind_speed)
pas$temp <- scale(pas$temp)
pas$humidity <- scale(pas$humidity)
pas$pressure <- scale(pas$pressure)

# Convert pitch controls to factors
pas$pitch_class <- as.factor(pas$pitch_class)
pas$zone_class <- as.factor(pas$zone_class)


# Convert situation controls to factors
pas$mariners <- as.factor(pas$mariners)
pas$times_faced <- as.factor(pas$times_faced)  
pas$leverage_category <- as.factor(pas$leverage_category)
pas$p_throws <- as.factor(pas$p_throws)
pas$stand <- as.factor(pas$stand)
pas$strikes <- as.factor(pas$strikes)
pas$starting_pitcher <- as.factor(pas$starting_pitcher)

# Convert time controls to factors
pas$month <- as.factor(pas$month)
pas$game_year <- as.factor(pas$game_year)



model_gam_k <- gam(k ~ te(altitude, azimuth) + 
                     
                     mariners + times_faced + leverage_category + starting_pitcher + 
                     p_throws + stand + 
                     month + game_year + 
                     temp + humidity + pressure  + wind_speed +
                     s(batter, bs = "re") + s(pitcher, bs = "re") 
                   ,
                   family = binomial,
                   data = pas)

summary(model_gam_k)
#plot(model_gam_k, shade = TRUE)



control <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
model_glmm_k <- glmer(k ~ sun_is_up +  
                      mariners + times_faced + leverage_category + starting_pitcher + 
                      p_throws + stand + 
                      month + game_year + 
                      temp + humidity + pressure  + wind_speed +
                      (1 | batter) + (1 | pitcher)
                    , 
                    family = binomial,
                    
                    data = pas,
                    control = control)

summary(model_glmm_k)
vif(model_glmm_k)