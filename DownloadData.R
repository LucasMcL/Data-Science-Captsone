# This script downloads and unpacks the Yelp data into a directory named data
# Zipped file is removed

file_URL <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/yelp_dataset_challenge_academic_dataset.zip"
date_download <- date()

download.file(file_URL, destfile = "zipped_data.zip")
unzip("zipped_data.zip")
file.rename("yelp_dataset_challenge_academic_dataset", "data")
file.remove("zipped_data.zip")

##Now, the data are loaded into R
check <- stream_in(file("./data/yelp_academic_dataset_checkin.json"))
check <- flatten(check) # Nested data frame with same dims

bus <- stream_in(file("./data/yelp_academic_dataset_business.json"))
bus <- flatten(bus) # Nested data frame with same dims

tip <- stream_in(file("./data/yelp_academic_dataset_tip.json"))
tip <- flatten(tip) # Nested data frame with same dims

user <- stream_in(file("./data/yelp_academic_dataset_user.json"))
user <- flatten(user)

review <- stream_in(file("./data/yelp_academic_dataset_review.json"))


## Code to save all these R objects to avoid downloading again
saveRDS(check, "./R_objects/check.rds")
saveRDS(bus, "./R_objects/bus.rds")
saveRDS(tip, "./R_objects/tip.rds")
saveRDS(user, "./R_objects/user.rds")
saveRDS(review, "./R_objects/review.rds")

## Code to read all these R objects back in
check <- readRDS("./R_objects/check.rds")
bus <- readRDS("./R_objects/bus.rds")
review <- readRDS("./R_objects/review.rds")
tip <- readRDS("./R_objects/tip.rds")
user <- readRDS("./R_objects/user.rds")
