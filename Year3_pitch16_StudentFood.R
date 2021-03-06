
## Thesis DSS
## Isabelle van Olphen - u308140
## Preprocessing pitch data
## Multidimensional drpfromts analyses to calculate maximum recurrence rates

#-------------------------------------------------------------------------------
# LOAD LIBRARIES
library("dplyr")
library("tidyr")
library("crqa")
library("ggplot2")

#-------------------------------------------------------------------------------
# DEiA 2019-2020
# PITCH 16 - StudentFood

# Read in the OpenFace data of the pitcher and three jury members---------------
year3_pitch16_StudentFood <- read.delim('/2019-2020_1-4_StudentFood.csv',
                                  sep = ',', stringsAsFactors = FALSE) # provide relative path

year3_judge1_StudentFood <- read.delim('/2019-2020_1-4_StudentFood_Judge1.csv',
                                 sep = ',', stringsAsFactors = FALSE) # provide relative path

year3_judge2_StudentFood <- read.delim('/2019-2020_1-4_StudentFood_Judge2.csv',
                                 sep = ',', stringsAsFactors = FALSE) # provide relative path

year3_judge3_StudentFood <- read.delim('/2019-2020_1-4_StudentFood_Judge3-Cropped.csv',
                                 sep = ',', stringsAsFactors = FALSE) # provide relative path


# Check the dimensions of the data files
dim(year3_pitch16_StudentFood) # 15367 x 714
dim(year3_judge1_StudentFood) # 15365 x 714
dim(year3_judge2_StudentFood) # 15365 x 714
dim(year3_judge3_StudentFood) # 15365 x 714


# Preprocessing and EDA --------------------------------------------------------

# Select the relevant variables
year3_pitch16_StudentFood <- year3_pitch16_StudentFood %>%
  select(frame, timestamp, AU06_c, AU12_c, AU25_c)

year3_judge1_StudentFood <- year3_judge1_StudentFood %>%
  select(frame, timestamp, AU06_c, AU12_c, AU25_c)

year3_judge2_StudentFood <- year3_judge2_StudentFood %>%
  select(frame, timestamp, AU06_c, AU12_c, AU25_c)

year3_judge3_StudentFood <- year3_judge3_StudentFood %>%
  select(frame, timestamp, AU06_c, AU12_c, AU25_c)


# Explore data files
head(year3_pitch16_StudentFood)
head(year3_judge1_StudentFood)
head(year3_judge2_StudentFood)
head(year3_judge3_StudentFood)


# Summarize data files
summary(year3_pitch16_StudentFood) # This one has NA values
summary(year3_judge1_StudentFood)
summary(year3_judge2_StudentFood)
summary(year3_judge3_StudentFood)


# Look into distribution of year3_pitch16_StudentFood
table(year3_pitch16_StudentFood$frame, useNA = 'always')
table(year3_pitch16_StudentFood$timestamp, useNA = 'always')
table(year3_pitch16_StudentFood$AU06_c, useNA = 'always')
table(year3_pitch16_StudentFood$AU12_c, useNA = 'always')
table(year3_pitch16_StudentFood$AU25_c, useNA = 'always')


# function for finding NA values
find_NAs <- function(values) {
  which(is.na(values))
}

find_NAs(year3_pitch16_StudentFood$frame) # NA values at index 15366 15367
find_NAs(year3_pitch16_StudentFood$timestamp) # NA values at index 15366 15367
find_NAs(year3_pitch16_StudentFood$AU06_c)
find_NAs(year3_pitch16_StudentFood$AU12_c)
find_NAs(year3_pitch16_StudentFood$AU25_c)


year3_pitch16_StudentFood <- year3_pitch16_StudentFood %>%
  drop_na # If necessary; delete rows missing values


# Cut out only the pitch data (without Q&A)
year3_pitch16_StudentFood <- slice(year3_pitch16_StudentFood, 1:5475)
year3_judge1_StudentFood <- slice(year3_judge1_StudentFood, 1:5475)
year3_judge2_StudentFood <- slice(year3_judge2_StudentFood, 1:5475)
year3_judge3_StudentFood <- slice(year3_judge3_StudentFood, 1:5475)


# Plot distribution of AUs to check their distribution--------------------------
# and check for possible outliers-----------------------------------------------

# AUs pitcher 1
ggplot(year3_pitch16_StudentFood, aes(x = AU06_c , y = frame)) + 
  geom_point() # AU06
ggplot(year3_pitch16_StudentFood, aes(x = AU12_c , y = frame)) +
  geom_point() # AU12
ggplot(year3_pitch16_StudentFood, aes(x = AU25_c , y = frame)) +
  geom_point() # AU25

# AUs judge 1
ggplot(year3_judge1_StudentFood, aes(x = AU06_c , y = frame)) +
  geom_point() # AU06
ggplot(year3_judge1_StudentFood, aes(x = AU12_c , y = frame)) +
  geom_point() # AU12
ggplot(year3_judge1_StudentFood, aes(x = AU25_c , y = frame)) +
  geom_point() # AU25

# AUs judge 2
ggplot(year3_judge2_StudentFood, aes(x = AU06_c , y = frame)) +
  geom_point() # AU06
ggplot(year3_judge2_StudentFood, aes(x = AU12_c , y = frame)) +
  geom_point() # AU12
ggplot(year3_judge2_StudentFood, aes(x = AU25_c , y = frame)) +
  geom_point() # AU25

# AUs judge 3
ggplot(year3_judge3_StudentFood, aes(x = AU06_c , y = frame)) + # very few present
  geom_point() # AU06
ggplot(year3_judge3_StudentFood, aes(x = AU12_c , y = frame)) + # very few present
  geom_point() # AU12
ggplot(year3_judge3_StudentFood, aes(x = AU25_c , y = frame)) +
  geom_point() # AU25

# There are no outliers detected




# DRPFROMTS---------------------------------------------------------------------

# Change name of all columns in pitch data file to combine them for drpfromts---

year3_pitch16_StudentFood <- dplyr::rename(year3_pitch16_StudentFood,
                                     frame_p = frame,
                                     timestamp_p = timestamp,
                                     AU06_c_p = AU06_c,
                                     AU12_c_p = AU12_c,
                                     AU25_c_p = AU25_c)


# Join the data set into one for the drpfromts
ts_pitch16_judge1 <- bind_cols(year3_pitch16_StudentFood,
                               year3_judge1_StudentFood)

ts_pitch16_judge2 <- bind_cols(year3_pitch16_StudentFood,
                               year3_judge2_StudentFood)

ts_pitch16_judge3 <- bind_cols(year3_pitch16_StudentFood,
                               year3_judge3_StudentFood)

ts_pitch16_judge1$AU06_c_p <- as.factor(ts_pitch16_judge1$AU06_c_p)
ts_pitch16_judge1$AU12_c_p <- as.factor(ts_pitch16_judge1$AU12_c_p)
ts_pitch16_judge1$AU25_c_p <- as.factor(ts_pitch16_judge1$AU25_c_p)
ts_pitch16_judge1$AU06_c <- as.factor(ts_pitch16_judge1$AU06_c)
ts_pitch16_judge1$AU12_c <- as.factor(ts_pitch16_judge1$AU12_c)
ts_pitch16_judge1$AU25_c <- as.factor(ts_pitch16_judge1$AU25_c)

ts_pitch16_judge2$AU06_c_p <- as.factor(ts_pitch16_judge2$AU06_c_p)
ts_pitch16_judge2$AU12_c_p <- as.factor(ts_pitch16_judge2$AU12_c_p)
ts_pitch16_judge2$AU25_c_p <- as.factor(ts_pitch16_judge2$AU25_c_p)
ts_pitch16_judge2$AU06_c <- as.factor(ts_pitch16_judge2$AU06_c)
ts_pitch16_judge2$AU12_c <- as.factor(ts_pitch16_judge2$AU12_c)
ts_pitch16_judge2$AU25_c <- as.factor(ts_pitch16_judge2$AU25_c)

ts_pitch16_judge3$AU06_c_p <- as.factor(ts_pitch16_judge3$AU06_c_p)
ts_pitch16_judge3$AU12_c_p <- as.factor(ts_pitch16_judge3$AU12_c_p)
ts_pitch16_judge3$AU25_c_p <- as.factor(ts_pitch16_judge3$AU25_c_p)
ts_pitch16_judge3$AU06_c <- as.factor(ts_pitch16_judge3$AU06_c)
ts_pitch16_judge3$AU12_c <- as.factor(ts_pitch16_judge3$AU12_c)
ts_pitch16_judge3$AU25_c <- as.factor(ts_pitch16_judge3$AU25_c)



# Code from Van den Broek (2018)
drpdfromts <- function (t1, t2, ws)
{
  drpd = vector()
  t1 = as.character(as.matrix(t1))
  t2 = as.character(as.matrix(t2))
  for (ix in (ws + 1):2) {
    #print(ix)
    y = t2[ix:length(t2)]
    #print(y)
    x = t1[1:length(y)]
    #print(x)
    drpd = c(drpd, sum(y == x)/length(y)) # negative side
  }
  drpd = c(drpd, sum(t1 == t2)/length(t1)) # central diagonal
  #print(sum(t1 == t2))
  #print(length(t1))
  for (i in 2:(ws + 1)) {
    x = t1[i:length(t1)]
    y = t2[1:length(x)]
    drpd = c(drpd, sum(y == x)/length(y)) # positive side
    print(drpd)
  }
  return(list(profile = drpd))
}




# DCRP & Maximum recurrence rate judge 1
pitch16_drpfromts1 <- drpdfromts(cbind(ts_pitch16_judge1$AU06_c_p,
                                       ts_pitch16_judge1$AU12_c_p,
                                       ts_pitch16_judge1$AU25_c_p),
                                 cbind(ts_pitch16_judge1$AU06_c,
                                       ts_pitch16_judge1$AU12_c,
                                       ts_pitch16_judge1$AU25_c),
                                 25)


profile <- pitch16_drpfromts1$profile

maxrec_pitch16_judge1 <- round((max(profile) * 100), 2) # Maximum recurrence is 59.53 %

plot(seq(-25, 25, 1), profile, type = "l", lwd = 5, xlab = "Lag", ylab = "Recurrence")




# DCRP & Maximum recurrence rate judge 2
pitch16_drpfromts2 <- drpdfromts(cbind(ts_pitch16_judge2$AU06_c_p,
                                       ts_pitch16_judge2$AU12_c_p,
                                       ts_pitch16_judge2$AU25_c_p),
                                 cbind(ts_pitch16_judge2$AU06_c,
                                       ts_pitch16_judge2$AU12_c,
                                       ts_pitch16_judge2$AU25_c),
                                 25)

profile <- pitch16_drpfromts2$profile

maxrec_pitch16_judge2 <- round((max(profile) * 100), 2) # Maximum recurrence is 73.05 %

plot(seq(-25, 25, 1), profile, type = "l", lwd = 5, xlab = "Lag", ylab = "Recurrence")




# DCRP & Maximum recurrence rate judge 3
pitch16_drpfromts3 <- drpdfromts(cbind(ts_pitch16_judge3$AU06_c_p,
                                       ts_pitch16_judge3$AU12_c_p,
                                       ts_pitch16_judge3$AU25_c_p),
                                 cbind(ts_pitch16_judge3$AU06_c,
                                       ts_pitch16_judge3$AU12_c,
                                       ts_pitch16_judge3$AU25_c),
                                 25)

profile <- pitch16_drpfromts3$profile

maxrec_pitch16_judge3 <- round((max(profile) * 100), 2) # Maximum recurrence is 45.79 %

plot(seq(-25, 25, 1), profile, type = "l", lwd = 5, xlab = "Lag", ylab = "Recurrence")



# Reference for drpdfromts code-------------------------------------------------

# Van den Broek, T. (2018). Early development and temporal responsivity: A cross-cultural study from the Netherlands, urban Mozambique, and rural Mozambique (Master's thesis). Retrieved from http://arno.uvt.nl/show.cgi?fid=147170

