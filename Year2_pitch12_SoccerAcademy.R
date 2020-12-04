
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
# 2019-2020 I
# PITCH 11 - Soccer Acadamy

# Read in the OpenFace data of the pitcher and three jury members---------------
year2_pitch12_SoccerAcademy <- read.delim('/DEiA_5_SoccerAcademy.csv',
                                    sep = ',', stringsAsFactors = FALSE) # provide relative path

year2_judge1_SoccerAcademy <- read.delim('/DEiA_5_SoccerAcademy_Judge1-Cropped.csv',
                                    sep = ',', stringsAsFactors = FALSE) # provide relative path

year2_judge2_SoccerAcademy <- read.delim('/DEiA_5_SoccerAcademy_Judge2.csv',
                                    sep = ',', stringsAsFactors = FALSE) # provide relative path

year2_judge3_SoccerAcademy <- read.delim('/DEiA_5_SoccerAcademy_Judge3-Cropped.csv',
                                    sep = ',', stringsAsFactors = FALSE) # provide relative path


# Check the dimensions of the data files
dim(year2_pitch12_SoccerAcademy) # 32667 x 714
dim(year2_judge1_SoccerAcademy) # 32647 x 714
dim(year2_judge2_SoccerAcademy) # 32665 x 714
dim(year2_judge3_SoccerAcademy) # 32658 x 714


# Preprocessing and EDA --------------------------------------------------------

# Select the relevant variables
year2_pitch12_SoccerAcademy <- year2_pitch12_SoccerAcademy %>%
  select(frame, timestamp, AU06_c, AU12_c, AU25_c)

year2_judge1_SoccerAcademy <- year2_judge1_SoccerAcademy %>%
  select(frame, timestamp, AU06_c, AU12_c, AU25_c)

year2_judge2_SoccerAcademy <- year2_judge2_SoccerAcademy %>%
  select(frame, timestamp, AU06_c, AU12_c, AU25_c)

year2_judge3_SoccerAcademy <- year2_judge3_SoccerAcademy %>%
  select(frame, timestamp, AU06_c, AU12_c, AU25_c)


# Explore data files
head(year2_pitch12_SoccerAcademy)
head(year2_judge1_SoccerAcademy)
head(year2_judge2_SoccerAcademy)
head(year2_judge3_SoccerAcademy)


# Summarize data files
summary(year2_pitch12_SoccerAcademy) # This one has NA values
summary(year2_judge1_SoccerAcademy)
summary(year2_judge2_SoccerAcademy)
summary(year2_judge3_SoccerAcademy)


# Look into distribution of year2_pitch12_SoccerAcademy
table(year2_pitch12_SoccerAcademy$frame, useNA = 'always')
table(year2_pitch12_SoccerAcademy$timestamp, useNA = 'always')
table(year2_pitch12_SoccerAcademy$AU06_c, useNA = 'always')
table(year2_pitch12_SoccerAcademy$AU12_c, useNA = 'always')
table(year2_pitch12_SoccerAcademy$AU25_c, useNA = 'always')


# function for finding NA values
find_NAs <- function(values) {
  which(is.na(values))
}

find_NAs(year2_pitch12_SoccerAcademy$frame) # NA values at index 32666 32667
find_NAs(year2_pitch12_SoccerAcademy$timestamp) # NA values at index 32666 32667
find_NAs(year2_pitch12_SoccerAcademy$AU06_c)
find_NAs(year2_pitch12_SoccerAcademy$AU12_c)
find_NAs(year2_pitch12_SoccerAcademy$AU25_c)


year2_pitch12_SoccerAcademy <- year2_pitch12_SoccerAcademy %>%
  drop_na # If necessary; delete rows missing values


# Cut out only the pitch data (without Q&A)
year2_pitch12_SoccerAcademy <- slice(year2_pitch12_SoccerAcademy, 1:7700)
year2_judge1_SoccerAcademy <- slice(year2_judge1_SoccerAcademy, 1:7700)
year2_judge2_SoccerAcademy <- slice(year2_judge2_SoccerAcademy, 1:7700)
year2_judge3_SoccerAcademy <- slice(year2_judge3_SoccerAcademy, 1:7700)


# Plot distribution of AUs to check their distribution--------------------------
# and check for possible outliers-----------------------------------------------

# AUs pitcher 1
ggplot(year2_pitch12_SoccerAcademy, aes(x = AU06_c , y = frame)) + # very few present
  geom_point() # AU06
ggplot(year2_pitch12_SoccerAcademy, aes(x = AU12_c , y = frame)) + # very few present
  geom_point() # AU12
ggplot(year2_pitch12_SoccerAcademy, aes(x = AU25_c , y = frame)) +
  geom_point() # AU25

# AUs judge 1
ggplot(year2_judge1_SoccerAcademy, aes(x = AU06_c , y = frame)) +
  geom_point() # AU06
ggplot(year2_judge1_SoccerAcademy, aes(x = AU12_c , y = frame)) +
  geom_point() # AU12
ggplot(year2_judge1_SoccerAcademy, aes(x = AU25_c , y = frame)) +
  geom_point() # AU25

# AUs judge 2
ggplot(year2_judge2_SoccerAcademy, aes(x = AU06_c , y = frame)) + # Very few present
  geom_point() # AU06
ggplot(year2_judge2_SoccerAcademy, aes(x = AU12_c , y = frame)) + # very few present
  geom_point() # AU12
ggplot(year2_judge2_SoccerAcademy, aes(x = AU25_c , y = frame)) +
  geom_point() # AU25

# AUs judge 3
ggplot(year2_judge3_SoccerAcademy, aes(x = AU06_c , y = frame)) +
  geom_point() # AU06
ggplot(year2_judge3_SoccerAcademy, aes(x = AU12_c , y = frame)) + # very few present
  geom_point() # AU12
ggplot(year2_judge3_SoccerAcademy, aes(x = AU25_c , y = frame)) +
  geom_point() # AU25

# There are no outliers detected




# DRPFROMTS---------------------------------------------------------------------

# Change name of all columns in pitch data file to combine them for drpfromts---

year2_pitch12_SoccerAcademy <- dplyr::rename(year2_pitch12_SoccerAcademy,
                                        frame_p = frame,
                                        timestamp_p = timestamp,
                                        AU06_c_p = AU06_c,
                                        AU12_c_p = AU12_c,
                                        AU25_c_p = AU25_c)


# Join the data set into one for the drpfromts
ts_pitch12_judge1 <- bind_cols(year2_pitch12_SoccerAcademy,
                               year2_judge1_SoccerAcademy)

ts_pitch12_judge2 <- bind_cols(year2_pitch12_SoccerAcademy,
                               year2_judge2_SoccerAcademy)

ts_pitch12_judge3 <- bind_cols(year2_pitch12_SoccerAcademy,
                               year2_judge3_SoccerAcademy)

ts_pitch12_judge1$AU06_c_p <- as.factor(ts_pitch12_judge1$AU06_c_p)
ts_pitch12_judge1$AU12_c_p <- as.factor(ts_pitch12_judge1$AU12_c_p)
ts_pitch12_judge1$AU25_c_p <- as.factor(ts_pitch12_judge1$AU25_c_p)
ts_pitch12_judge1$AU06_c <- as.factor(ts_pitch12_judge1$AU06_c)
ts_pitch12_judge1$AU12_c <- as.factor(ts_pitch12_judge1$AU12_c)
ts_pitch12_judge1$AU25_c <- as.factor(ts_pitch12_judge1$AU25_c)

ts_pitch12_judge2$AU06_c_p <- as.factor(ts_pitch12_judge2$AU06_c_p)
ts_pitch12_judge2$AU12_c_p <- as.factor(ts_pitch12_judge2$AU12_c_p)
ts_pitch12_judge2$AU25_c_p <- as.factor(ts_pitch12_judge2$AU25_c_p)
ts_pitch12_judge2$AU06_c <- as.factor(ts_pitch12_judge2$AU06_c)
ts_pitch12_judge2$AU12_c <- as.factor(ts_pitch12_judge2$AU12_c)
ts_pitch12_judge2$AU25_c <- as.factor(ts_pitch12_judge2$AU25_c)

ts_pitch12_judge3$AU06_c_p <- as.factor(ts_pitch12_judge3$AU06_c_p)
ts_pitch12_judge3$AU12_c_p <- as.factor(ts_pitch12_judge3$AU12_c_p)
ts_pitch12_judge3$AU25_c_p <- as.factor(ts_pitch12_judge3$AU25_c_p)
ts_pitch12_judge3$AU06_c <- as.factor(ts_pitch12_judge3$AU06_c)
ts_pitch12_judge3$AU12_c <- as.factor(ts_pitch12_judge3$AU12_c)
ts_pitch12_judge3$AU25_c <- as.factor(ts_pitch12_judge3$AU25_c)



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
pitch12_drpfromts1 <- drpdfromts(cbind(ts_pitch12_judge1$AU06_c_p,
                                       ts_pitch12_judge1$AU12_c_p,
                                       ts_pitch12_judge1$AU25_c_p),
                                 cbind(ts_pitch12_judge1$AU06_c,
                                       ts_pitch12_judge1$AU12_c,
                                       ts_pitch12_judge1$AU25_c),
                                 25)


profile <- pitch12_drpfromts1$profile

maxrec_pitch12_judge1 <- round((max(profile) * 100), 2) # Maximum recurrence is 75.66 %

plot(seq(-25, 25, 1), profile, type = "l", lwd = 5, xlab = "Lag", ylab = "Recurrence")




# DCRP & Maximum recurrence rate judge 2
pitch12_drpfromts2 <- drpdfromts(cbind(ts_pitch12_judge2$AU06_c_p,
                                       ts_pitch12_judge2$AU12_c_p,
                                       ts_pitch12_judge2$AU25_c_p),
                                 cbind(ts_pitch12_judge2$AU06_c,
                                       ts_pitch12_judge2$AU12_c,
                                       ts_pitch12_judge2$AU25_c),
                                 25)

profile <- pitch12_drpfromts2$profile

maxrec_pitch12_judge2 <- round((max(profile) * 100), 2) # Maximum recurrence is 83.65 %

plot(seq(-25, 25, 1), profile, type = "l", lwd = 5, xlab = "Lag", ylab = "Recurrence")




# DCRP & Maximum recurrence rate judge 3
pitch12_drpfromts3 <- drpdfromts(cbind(ts_pitch12_judge3$AU06_c_p,
                                       ts_pitch12_judge3$AU12_c_p,
                                       ts_pitch12_judge3$AU25_c_p),
                                 cbind(ts_pitch12_judge3$AU06_c,
                                       ts_pitch12_judge3$AU12_c,
                                       ts_pitch12_judge3$AU25_c),
                                 25)

profile <- pitch12_drpfromts3$profile

maxrec_pitch12_judge3 <- round((max(profile) * 100), 2) # Maximum recurrence is 79.32 %

plot(seq(-25, 25, 1), profile, type = "l", lwd = 5, xlab = "Lag", ylab = "Recurrence")



# Reference for drpdfromts code-------------------------------------------------

# Van den Broek, T. (2018). Early development and temporal responsivity: A cross-cultural study from the Netherlands, urban Mozambique, and rural Mozambique (Master's thesis). Retrieved from http://arno.uvt.nl/show.cgi?fid=147170

