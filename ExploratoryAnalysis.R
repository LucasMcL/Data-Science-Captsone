# Look at checkin data
generate_week_hours <- function(start = as.POSIXct("11/15/15 00:00", format = "%m/%d/%y %H:%M")){
  #Generate vector of POSIXct hours for one week
  hours <- as.POSIXct(rep(NA, 168)); hours[1] <- start
  for(i in 1:(length(hours) - 1)){
    hours[i + 1] <- hours[i] + 60*60
  }
  hours
}
hours <- generate_week_hours()
jazz.total <- sum(rowSums(check.rbc[check.rbc$Jazz == "Jazz", 3:170], na.rm = TRUE))
notjazz.total <-  sum(rowSums(check.rbc[check.rbc$Jazz == "Not Jazz", 3:170], na.rm = TRUE))
jazz.avgs <- colSums(check.rbc[check.rbc$Jazz == "Jazz", 3:170], na.rm = TRUE) / jazz.total * 100
notjazz.avgs <- colSums(check.rbc[check.rbc$Jazz == "Not Jazz", 3:170], na.rm = TRUE) / notjazz.total * 100

# % of checkin times for each hour of week
df <- data.frame(Checkins = c(jazz.avgs, notjazz.avgs),
                 Time = c(hours, hours),
                 Jazz = factor(c(rep("Jazz", 168), rep("Not Jazz", 168))))
g <- ggplot(df, aes(x = Time, y = Checkins, color = Jazz))
g1 <- g + geom_line() +
  scale_x_datetime(breaks = date_breaks("24 hours"), labels = date_format("%A")) + 
  xlab("Day") + ggtitle("Average Number of Checkins/Hour by Day")

oneday.jazz <- rep(NA, 24)
for(i in 1:24){
  oneday.jazz[i] <- sum(jazz.avgs[i], jazz.avgs[i + 24], jazz.avgs[i + 24*2],
                        jazz.avgs[i + 24*3], jazz.avgs[i + 24*4], jazz.avgs[i+ 24*5],
                        jazz.avgs[i + 24*6])
}
oneday.notjazz <- rep(NA, 24)
for(i in 1:24){
  oneday.notjazz[i] <- sum(notjazz.avgs[i], notjazz.avgs[i + 24], notjazz.avgs[i + 24*2],
                           notjazz.avgs[i + 24*3], notjazz.avgs[i + 24*4], notjazz.avgs[i+ 24*5],
                           notjazz.avgs[i + 24*6])
}

df2 <- data.frame(Checkins = c(oneday.jazz, oneday.notjazz),
                 Time = c(hours[1:24], hours[1:24]),
                 Jazz = factor(c(rep("Jazz", 24), rep("Not Jazz", 24))))
g <- ggplot(df2, aes(x = Time, y = Checkins, color = Jazz))
g2 <- g + geom_line() +
  scale_x_datetime(breaks = date_breaks("3 hours"), labels = date_format("%H:%M")) +
  xlab("Hour") + ggtitle("Average Number of Checkins/Hour")


## Closing time
g <- ggplot(rbc, aes(Jazz, weekday.close))
g3 <- g + geom_boxplot() + xlab("Category") + ylab("Time") + ggtitle("Weekday Closing Times")

g <- ggplot(rbc, aes(Jazz, weekend.close))
g4 <- g + geom_boxplot() + xlab("Category") + ylab("Time") + ggtitle("Weekend Closing Times")

## Alcohol
n.jazz <- sum(rbc$Jazz == "Jazz")
n.notjazz <- sum(rbc$Jazz == "Not Jazz")
alc.percent <- c(sum(rbc[rbc$Jazz == "Jazz", ]$Alcohol == "full_bar" |
                       rbc[rbc$Jazz == "Jazz", ]$Alcohol == "beer_and_wine", na.rm = TRUE) / n.jazz,
                 sum(rbc[rbc$Jazz == "Not Jazz", ]$Alcohol == "full_bar" |
                       rbc[rbc$Jazz == "Not Jazz", ]$Alcohol == "beer_and_wine", na.rm = TRUE) / n.notjazz)
kid.percent <- c(sum(rbc[rbc$Jazz == "Jazz", ]$Good_for_Kids, na.rm = TRUE) / n.jazz,
                 sum(rbc[rbc$Jazz == "Not Jazz", ]$Good_for_Kids, na.rm = TRUE) / n.notjazz)
classy.percent <- c(sum(rbc[rbc$Jazz == "Jazz", ]$Ambience.classy, na.rm = TRUE) / n.jazz,
                    sum(rbc[rbc$Jazz == "Not Jazz", ]$Ambience.classy, na.rm = TRUE) / n.notjazz)
upscale.percent <- c(sum(rbc[rbc$Jazz == "Jazz", ]$Ambience.upscale, na.rm = TRUE) / n.jazz,
                   sum(rbc[rbc$Jazz == "Not Jazz", ]$Ambience.upscale, na.rm = TRUE) / n.notjazz)
df1 <- data.frame(Jazz = rep(factor(c("Jazz", "Not Jazz")), 2),
                  x = factor(c("Serves Alcohol", "Serves Alcohol", "Good for Kids", "Good for Kids")),
                  y = c(alc.percent, kid.percent)
)
g <- ggplot(df1, aes(x = x, y = y * 100, fill = Jazz))
g5 <- g + geom_bar(stat = "identity", position = "dodge") + xlab("Category") + ylab("Percent") +
  ggtitle("Alcohol")

df2 <- data.frame(Jazz = rep(factor(c("Jazz", "Not Jazz")), 2),
                  x = factor(c("Classy", "Classy", "Upscale", "Upscale")),
                  y = c(classy.percent, upscale.percent)
)
g <- ggplot(df2, aes(x = x, y = y * 100, fill = Jazz))
g6 <- g + geom_bar(stat = "identity", position = "dodge") + xlab("Category") + ylab("Percent") +
  ggtitle("Ambience")

topcities <- names(table(rbc$City_Region)[1:10])
city.freq <- table(rbc$City_Region)
city.perc <- rep(NA, 10)
for(i in 1:10){
  city.perc[i] <- sum(rbc[rbc$City_Region %in% topcities[i], ]$Jazz == "Jazz") / city.freq[i] * 100
}
names(city.perc) <- topcities
city.perc <- sort(city.perc, decreasing = TRUE)

df3 <- data.frame(x = as.factor(names(city.perc)),
                  y = city.perc)
df3$x <- reorder(df3$x, rev(df3$y))
g <- ggplot(df3, aes(x = x, y = y))
g7 <- g + geom_bar(stat = "identity") + xlab("City-Region") + ylab("Percent") + 
  ggtitle("Percentage of jazz establishments by city")

## Linear Model
fit <- lm(stars ~ Jazz, data = rbc)
coef <- summary(fit)$coefficients





