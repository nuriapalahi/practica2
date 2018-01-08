library(ggplot2)
library(lubridate)
library(scales)
library(plyr)
library(readr)

train <- read_csv("../input/train.csv")

train$season  <- factor(train$season, labels = c("Primavera", "Estiu", "Tardor", "hivern"))
train$weather <- factor(train$weather, labels = c("Bo", "Normal", "Dolent", "Molt dolent"))
train$hour    <- factor(hour(ymd_hms(train$datetime)))
train$times   <- as.POSIXct(strftime(ymd_hms(train$datetime), format="%H:%M:%S"), format="%H:%M:%S")
train$Weekday <- wday(ymd_hms(train$datetime), label=TRUE)

season_summary <- ddply(train,.(season,hour),
                        summarise, count = mean(count))
ggplot(train, aes(x = hour, y = count, colour = season)) +
  geom_point(data = season_summary, aes(group = season)) +
  geom_line(data = season_summary, aes(group = season)) +
  scale_x_discrete("hora") +
  scale_y_continuous("total") +
  theme_minimal() +
  ggtitle("Gent lloga més bicis a la tardor que a la primavera.\n") + 
  theme(plot.title=element_text(size=18))

weather_summary <- ddply(train,.(weather,hour),
                         summarise, count = mean(count))
ggplot(train, aes(x = hour, y = count, colour = weather)) +
  geom_point(data = weather_summary, aes(group = weather)) +
  geom_line(data = weather_summary, aes(group = weather)) +
  scale_x_discrete("hora") +
  scale_y_continuous("total") +
  theme_minimal() +
  ggtitle("Hi han més lloguer quan el temps es bo.\n") + 
  theme(plot.title=element_text(size=18))


day_summary <- ddply(train,.(Weekday,hour),
                     summarise, count = mean(count))
ggplot(train, aes(x = hour, y = count, colour = Weekday)) +
  geom_point(data = day_summary, aes(group=Weekday)) +
  geom_line(data = day_summary, aes(group=Weekday)) +
  scale_x_discrete("hora") +
  scale_y_continuous("total") +
  theme_minimal() +
  ggtitle("Es lloguen més bicis als matins/vespres en dies laborables, i per dies sencers als caps de setmana\n")


weather_prob <- ddply(train,.(season, hour),
                      summarise, Good = mean(weather == "Bo"),
                      Normal = mean(weather == "Normal"),
                      Bad = mean(weather == "Dolent"),
                      Very_bad = mean(weather == "Molt dolent"))


ggplot(train, aes(x = hour, y = Good, colour = season)) +
  geom_point(data = weather_prob, aes(group = season)) +
  geom_line(data = weather_prob, aes(group = season)) +
  scale_x_discrete("Hora") +
  scale_y_continuous("Bon temps") +
  theme_minimal() +
  ggtitle("La possibilitat de bon temps és sempre alta. \n") + 
  theme(plot.title=element_text(size=18))

ggplot(train, aes(x = hour, y = Normal, colour = season)) +
  geom_point(data = weather_prob, aes(group = season)) +
  geom_line(data = weather_prob, aes(group = season)) +
  scale_x_discrete("Hora") +
  scale_y_continuous("Nomal") +
  theme_minimal() +
  ggtitle("La possibilitat de temps normal es major a la primavera. \n") + 
  theme(plot.title=element_text(size=18))

ggplot(train, aes(x = hour, y = Bad, colour = season)) +
  geom_point(data = weather_prob, aes(group = season)) +
  geom_line(data = weather_prob, aes(group = season)) +
  scale_x_discrete("Hour") +
  scale_y_continuous("Mal temps") +
  theme_minimal() +
  ggtitle("La possibilitat de mal temps és major a l'estiu i hivern. \n") + 
  theme(plot.title=element_text(size=18))

