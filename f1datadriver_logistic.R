# Activate libraries
library(lubridate)
library(nnet)
library(ggplot2)

# Import Data
circuits = read.csv('circuits.csv')
constructors = read.csv('constructors.csv')
drivers = read.csv('drivers.csv')
races = read.csv ('races.csv')
results = read.csv('results.csv')

# Merge Data into One Data Frame
df = merge(x = races, y = results, by = c("raceId")) 
df = merge(x = df, y = drivers, by = c("driverId"))
df = merge(x = df, y = circuits, by = c("circuitId"))
df = merge(x = df, y = constructors, by = c("constructorId"))

# Remove Unnecessary Columns
df = subset(df, select=-c(time.x, url.x, fp1_date, fp1_time,
                          fp2_date, fp2_time, fp3_date, fp3_time,
                          quali_date, quali_time, sprint_date, sprint_time,
                          url.y))

# Include races only since 2018
df <- df[df$year %in% df$year[df$year>2018],]

# Account for Racing Point becoming Aston Martin
# and Renault becoming Alpine 
# and Toro Rosso becoming AlphaTauri
# Constructor IDs:
# Racing Point 211
# Aston Martin 117
# Renault 4
# Alpine 214
# Toro Rosso 5
# AlphaTauri 213
df$constructorId[df$constructorId == 211] <- 117
df$constructorId[df$constructorId == 4] <- 214
df$constructorId[df$constructorId == 5] <- 213
df$constructorRef[df$constructorRef == "racing_point"] <- "williams"
df$constructorRef[df$constructorRef == "renault"] <- "alpine"
df$constructorRef[df$constructorRef == "toro_rosso"] <- "alphatauri"

# Drop former drivers (raced since cut off, but not in 2022)
# Aitken 851
# Fittipaldi 104 & 850
# Giovinazzi 841
# Grosjean 154
# Hulkenberg 807
# Kubica 9
# Kvyat 826
# Mazepin 853
# Raikkonen 8
df <- subset(df, driverId!=(851))
df <- subset(df, driverId!=(104))
df <- subset(df, driverId!=(850))
df <- subset(df, driverId!=(841))
df <- subset(df, driverId!=(154))
df <- subset(df, driverId!=(807))
df <- subset(df, driverId!=(9))
df <- subset(df, driverId!=(826))
df <- subset(df, driverId!=(853))
df <- subset(df, driverId!=(8))


# Generate features for win and podium rates for each driver
df$podium <- ifelse(df$positionOrder<=3, 1, 0)
df$win <- ifelse(df$positionOrder<=1, 1, 0)

wins <- data.frame(tapply(df$win, df$surname, sum))
wins$names  <- rownames(wins)
podiums <- data.frame(tapply(df$podium, df$surname, sum))
podiums$names <- rownames(podiums)
race_count <- data.frame(tapply(df$raceId, df$surname, length))
race_count$names <- rownames(race_count)

records = merge(x = wins, y = podiums, by = c("names")) 
records = merge(x = records, y = race_count, by = c("names"))
colnames(records) = c("surname", "wins", "podiums", "races")

records$winrate = records$wins/records$races
records$podiumrate = records$podiums/records$races
colnames(records) = c("surname", "wins", "podiums", "races")

records$winrate = records$wins/records$races
records$podiumrate = records$podiums/records$races

driver_records <- data.frame(records)

# Constructor 2021 Results
constructorRef <- c("mercedes", "red_bull", "ferrari", "mclaren",
                "alpine", "alphatauri", "aston_martin", "williams",
                "alfa", "haas")
team_rank_2021 <- c(1,2,3,4,5,6,7,8,9,10)
team_points_2021 <- c(613.5, 585.5, 323.5, 275, 155,
                      142, 77, 23, 13, 0)

teams_2021 <- data.frame(constructorRef, team_rank_2021, team_points_2021)

df = merge(x = df, y = teams_2021, by = c("constructorRef"))
df = merge(x = df, y = driver_records, by = c("surname"))

# Create a table matching current drivers and constuctors 
current_drivers = data.frame(sort(driver_records$surname))

# Manually enter the team for each current driver
current_teams = c("williams", "alpine", "alfa", "alphatauri", "mercedes",
                  "williams", "ferrari", "haas", "mclaren", "alpine",
                  "red_bull", "mclaren", "mercedes", "ferrari", "haas",
                  "aston_martin", "alphatauri", "red_bull", "aston_martin", "alfa")

driver_teams = cbind(current_drivers, current_teams)
colnames(driver_teams) = c("surname", "constructorRef")

driver_teams = merge(x = driver_teams, y = teams_2021, by = c("constructorRef"))


# Multinomial Logistic Regression Model
# y = binary 1/0 win/loss 
# Regress dependent variable (y) on independent variables (features)
mlogit <- multinom(win ~ winrate + podiumrate + team_points_2021, data = df)

summary(mlogit)

#Record linear regression coefficients including intercept (betas)
intercept <- coef(mlogit)[1]
b_winrate <- coef(mlogit)[2]
b_podiumrate <- coef(mlogit)[3]
b_team_point_2021 <- coef(mlogit)[4]

#Put the coefficients (betas) into a table with the independent variables (X's)
output = subset(driver_records, select=-c(wins, podiums, races))
output = merge(x = output, y = driver_teams, by = c("surname"))
output$b_winrate = b_winrate
output$b_podiumrate = b_podiumrate
output$b_teampoints = b_team_point_2021
output$intercept = intercept

#Calculate logit for each driver
output$logit = output$intercept +
  output$b_winrate * output$winrate +
  output$b_podiumrate * output$podiumrate +
  output$b_teampoints * output$team_points_2021

#Convert logit estimates to win probability for each driver
output$n = exp(output$logit)
output$sum_n = sum(output$n)
output$prob = output$n / output$sum_n
output = subset(output, select=c(surname, prob))


#Account for spelling error in data for Sergio Perez
output[11,1] = "Perez"

#Make bar chart of win probability by driver
plot = ggplot(data=output, aes(x=reorder(surname, -prob), y=prob)) +
  geom_bar(stat="identity", fill="blue1") +
  xlab("Driver") +
  ylab("Next Race Win Probability") +
  ggtitle("Win Probability by Driver: Next Race") +
  theme(plot.title=element_text(hjust=0.5)) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  theme(panel.background = element_blank())

plot







