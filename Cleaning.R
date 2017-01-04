## This searches for reviews with the word "jazz" in them and
## subsets the business dataframe to include only those
## corresponding businesses
index <- grep("[Jj]azz", review$text, value = FALSE)
bus.index <- unique(review$business_id[index])
bus.jazz <- bus[bus$business_id %in% bus.index, ]

## This looks at the most common business categories for which
## the word "jazz" appeared in a review
jazz.cat <- unlist(bus.jazz$categories)
jazz.cat <- as.data.frame(table(jazz.cat))
jazz.cat <- arrange(jazz.cat, desc(Freq))

## This code creates the rbc dataset, which is business data for
## all businesses that fall into categories restaurant, bar, and coffee
rest.index <- sapply(bus$categories, is.element, el = "Restaurants")
bar.index <- sapply(bus$categories, is.element, el = "Bars")
coffee.index <- sapply(bus$categories, is.element, el = "Coffee & Tea")
rbc <- bus[rest.index | bar.index | coffee.index, ] #rbc = [r]estaurant [b]ars [c]offee

old.rbc <- rbc #In case of emergency
# Cleaning
rbc <- select(rbc, -contains("Hair Types"),
              -contains("Dietary Restrictions"),
              -contains("Parking"))
names <- names(rbc)
names <- gsub("attributes.", "", names)
names <- gsub("hours.", "", names)
names <- gsub(" ", "_", names)
names(rbc) <- names
for(i in 14:27){rbc[, i] <- as.POSIXct(rbc[, i], format = "%H:%M")}
rbc <- rbc[, ord <-c(1:13, 25, 24, 19, 18, 15, 14, 21, 20, 23, 22, 17, 16, 27, 26, 28:length(rbc))]
rbc <- mutate(rbc, Sunday = as.numeric(Sunday.close - Sunday.open, units = "hours"),
              Monday = as.numeric(Monday.close - Monday.open, units = "hours"),
              Tuesday = as.numeric(Tuesday.close - Tuesday.open, units = "hours"),
              Wednesday = as.numeric(Wednesday.close - Wednesday.open, units = "hours"),
              Thursday = as.numeric(Thursday.close - Thursday.open, units = "hours"),
              Friday = as.numeric(Friday.close - Friday.open, units = "hours"),
              Saturday = as.numeric(Saturday.close - Saturday.open, units = "hours")
              )

fix_closing_times <- function(data, columns = 86:92){
  #Given index of columns with hours open data, fixes closing times
  for(i in columns){
    column_to_fix <- paste(names(data)[i], ".close", sep = "")
    column_index <- which(names(data) %in% column_to_fix)
    ind <- which(data[ ,i] < 0)
    data[ind, column_index] <- data[ind, column_index] + 60*60*24
  }
  data
}

rbc <- fix_closing_times(rbc)
rbc <- mutate(rbc, Sunday = as.numeric(Sunday.close - Sunday.open, units = "hours"),
              Monday = as.numeric(Monday.close - Monday.open, units = "hours"),
              Tuesday = as.numeric(Tuesday.close - Tuesday.open, units = "hours"),
              Wednesday = as.numeric(Wednesday.close - Wednesday.open, units = "hours"),
              Thursday = as.numeric(Thursday.close - Thursday.open, units = "hours"),
              Friday = as.numeric(Friday.close - Friday.open, units = "hours"),
              Saturday = as.numeric(Saturday.close - Saturday.open, units = "hours"))
#Redo the hours open with correct closing times now.

rbc$Category <- NA
rbc[sapply(rbc$categories, is.element, el = "Bars"), ]$Category <- "Bar"
rbc[sapply(rbc$categories, is.element, el = "Coffee & Tea"), ]$Category <- "Coffee"
rbc[sapply(rbc$categories, is.element, el = "Restaurants"), ]$Category <- "Restaurant"
rbc$Category <- as.factor(rbc$Category); rbc$Category <- relevel(rbc$Category, "Restaurant")

#Lump suburbs into their respective urban areas
rbc$City_Region <- rbc$city
ind <- rbc$city %in% c("Scottsdale", "Mesa", "Tempe", "Chandler", "Glendale",
                       "Gilbert", "Peoria", "Surprise", "Goodyear", "Avondale")
rbc[ind, ]$City_Region <- "Phoenix"
rbc[rbc$city %in% c("Henderson", "North Las Vegas"), ]$City_Region <- "Las Vegas"
rbc[rbc$city %in% "Montréal", ]$City_Region <- "Montreal"
rbc[rbc$city %in% "Matthews", ]$City_Region <- "Charlotte"
rbc[rbc$city %in% "Kitchener", ]$City_Region <- "Waterloo"
rbc[rbc$city %in% "Urbana", ]$City_Region <- "Champaign"
topcities <- names(sort(table(rbc$City_Region), decreasing = TRUE)[1:10])
rbc[!(rbc$City_Region %in% topcities), ]$City_Region <- NA
rbc$City_Region <- as.factor(rbc$City_Region)
table <- sort(table(rbc$City_Region), decreasing = TRUE)
rbc$City_Region <- factor(rbc$City_Region, names(table)) #Relevel by frequency

# Convert Variables to factor
rbc$Alcohol <- as.factor(rbc$Alcohol)
rbc$Noise_Level <- as.factor(rbc$Noise_Level)
rbc$Attire <- as.factor(rbc$Attire)
rbc$Good_For_Dancing <- as.factor(rbc$Good_For_Dancing)
# Add jazz categorical variable
index <- grep("[Jj]azz", review$text, value = FALSE)
jazz.index <- unique(review$business_id[index])
rbc$Jazz <- "Not Jazz"
rbc[rbc$business_id %in% jazz.index, ]$Jazz <- "Jazz"
rbc$Jazz <- as.factor(rbc$Jazz)
# Add average closing time variables
weekday.closenames <- c("Sunday.close", "Monday.close", "Tuesday.close", "Wednesday.close", "Thursday.close")
weekday.close <- rep(NA, nrow(rbc))
for(i in 1:nrow(rbc)){
  weekdays <- rbc[i, names(rbc) %in% weekday.closenames]
  weekday.close[i] <- mean(c(weekdays[1, 1], weekdays[1, 2], weekdays[1, 3],
                         weekdays[1, 4], weekdays[1, 5]))
}
rbc$weekday.close <- as.POSIXct(weekday.close, origin = "1970-01-01")
weekend.closenames <- c("Friday.close", "Saturday.close")
weekend.close <- rep(NA, nrow(rbc))
for(i in 1:nrow(rbc)){
  weekends <- rbc[i, names(rbc) %in% weekend.closenames]
  weekend.close[i] <- mean(c(weekends[1, 1], weekends[1, 2]))
  print(i)
}
rbc$weekend.close <- as.POSIXct(weekend.close, origin = "1970-01-01")
close <- rep(NA, nrow(rbc))
for(i in 1:nrow(rbc)){
  days <- rbc[i, names(rbc) %in% c(weekday.closenames, weekend.closenames)]
  close[i] <- mean(c(days[1, 1], days[1, 2], days[1, 3], days[1, 4],
                     days[1, 5], days[1, 6], days[1, 7]))
  print(i)
}
rbc$close <- as.POSIXct(close, origin = "1970-01-01")


##Data Cleaning for checkin data
names(check) <- gsub("checkin_info.", "", names(check))
times <- names(check)[-1:-2]
times <- strsplit(times, "-")
for(i in 1:length(times)){
  if(nchar(times[[i]][1]) == 1) times[[i]][1] <- paste("0", times[[i]][1], sep = "")
}
for(i in 1:length(times)) times[[i]] <- paste(times[[i]][2], ".", times[[i]][1], sep = "")
times <- unlist(times); times <- as.numeric(times)
names(check) <- c(names(check)[c(1, 2)], times)
check <- check[, c(1:2, order(times) + 2)]
check.rbc <- check[check$business_id %in% rbc$business_id, ]
check.rbc$Jazz <- "Not Jazz"
check.rbc[check.rbc$business_id %in% jazz.index, ]$Jazz <- "Jazz"
check.rbc$Jazz <- as.factor(check.rbc$Jazz)





