
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
# PITCH 14 - Choos3Wisely

# Read in the OpenFace data of the pitcher and three jury members---------------
year3_pitch14_Choos3Wisely <- read.delim('/2019-2020_1-2_Choos3Wisely.csv',
                                   sep = ',', stringsAsFactors = FALSE) # provide relative path

year3_judge1_Choos3Wisely <- read.delim('/2019-2020_1-2_Choos3Wisely_Judge1.csv',
                                  sep = ',', stringsAsFactors = FALSE) # provide relative path

year3_judge2_Choos3Wisely <- read.delim('/2019-2020_1-2_Choos3Wisely_Judge2-Cropped.csv',
                                  sep = ',', stringsAsFactors = FALSE) # provide relative path

year3_judge3_Choos3Wisely <- read.delim('/2019-2020_1-2_Choos3Wisely_Judge3-Cropped.csv',
                                  sep = ',', stringsAsFactors = FALSE) # provide relative path


# Check the dimensions of the data files
dim(year3_pitch14_Choos3Wisely) # 15376 x 714
dim(year3_judge1_Choos3Wisely) # 15374 x 714
dim(year3_judge2_Choos3Wisely) # 15374 x 714
dim(year3_judge3_Choos3Wisely) # 15374 x 714


# Preprocessing and EDA --------------------------------------------------------

# Select the relevant variables
year3_pitch14_Choos3Wisely <- year3_pitch14_Choos3Wisely %>%
  select(frame, timestamp, AU06_c, AU12_c, AU25_c)

year3_judge1_Choos3Wisely <- year3_judge1_Choos3Wisely %>%
  select(frame, timestamp, AU06_c, AU12_c, AU25_c)

year3_judge2_Choos3Wisely <- year3_judge2_Choos3Wisely %>%
  select(frame, timestamp, AU06_c, AU12_c, AU25_c)

year3_judge3_Choos3Wisely <- year3_judge3_Choos3Wisely %>%
  select(frame, timestamp, AU06_c, AU12_c, AU25_c)


# Explore data files
head(year3_pitch14_Choos3Wisely)
head(year3_judge1_Choos3Wisely)
head(year3_judge2_Choos3Wisely)
head(year3_judge3_Choos3Wisely)


# Summarize data files
summary(year3_pitch14_Choos3Wisely) # This one has NA values
summary(year3_judge1_Choos3Wisely)
summary(year3_judge2_Choos3Wisely)
summary(year3_judge3_Choos3Wisely)


# Look into distribution of year3_pitch14_Choos3Wisely
table(year3_pitch14_Choos3Wisely$frame, useNA = 'always')
table(year3_pitch14_Choos3Wisely$timestamp, useNA = 'always')
table(year3_pitch14_Choos3Wisely$AU06_c, useNA = 'always')
table(year3_pitch14_Choos3Wisely$AU12_c, useNA = 'always')
table(year3_pitch14_Choos3Wisely$AU25_c, useNA = 'always')


# function for finding NA values
find_NAs <- function(values) {
  which(is.na(values))
}

find_NAs(year3_pitch14_Choos3Wisely$frame) # NA values at index 15375 15376
find_NAs(year3_pitch14_Choos3Wisely$timestamp) # NA values at index 15375 15376
find_NAs(year3_pitch14_Choos3Wisely$AU06_c)
find_NAs(year3_pitch14_Choos3Wisely$AU12_c)
find_NAs(year3_pitch14_Choos3Wisely$AU25_c)


year3_pitch14_Choos3Wisely <- year3_pitch14_Choos3Wisely %>%
  drop_na # If necessary; delete rows missing values


# Cut out only the pitch data (without Q&A)
year3_pitch14_Choos3Wisely <- slice(year3_pitch14_Choos3Wisely, 1:4450)
year3_judge1_Choos3Wisely <- slice(year3_judge1_Choos3Wisely, 1:4450)
year3_judge2_Choos3Wisely <- slice(year3_judge2_Choos3Wisely, 1:4450)
year3_judge3_Choos3Wisely <- slice(year3_judge3_Choos3Wisely, 1:4450)


# Plot distribution of AUs to check their distribution--------------------------
# and check for possible outliers-----------------------------------------------

# AUs pitcher 1
ggplot(year3_pitch14_Choos3Wisely, aes(x = AU06_c , y = frame)) + 
  geom_point() # AU06
ggplot(year3_pitch14_Choos3Wisely, aes(x = AU12_c , y = frame)) + # Not present at all
  geom_point() # AU12
ggplot(year3_pitch14_Choos3Wisely, aes(x = AU25_c , y = frame)) +
  geom_point() # AU25

# AUs judge 1
ggplot(year3_judge1_Choos3Wisely, aes(x = AU06_c , y = frame)) +
  geom_point() # AU06
ggplot(year3_judge1_Choos3Wisely, aes(x = AU12_c , y = frame)) +
  geom_point() # AU12
ggplot(year3_judge1_Choos3Wisely, aes(x = AU25_c , y = frame)) +
  geom_point() # AU25

# AUs judge 2
ggplot(year3_judge2_Choos3Wisely, aes(x = AU06_c , y = frame)) +
  geom_point() # AU06
ggplot(year3_judge2_Choos3Wisely, aes(x = AU12_c , y = frame)) +
  geom_point() # AU12
ggplot(year3_judge2_Choos3Wisely, aes(x = AU25_c , y = frame)) +
  geom_point() # AU25

# AUs judge 3
ggplot(year3_judge3_Choos3Wisely, aes(x = AU06_c , y = frame)) + # Not present at all
  geom_point() # AU06
ggplot(year3_judge3_Choos3Wisely, aes(x = AU12_c , y = frame)) + # Not present at all
  geom_point() # AU12
ggplot(year3_judge3_Choos3Wisely, aes(x = AU25_c , y = frame)) +
  geom_point() # AU25

# There are no outliers detected. but a few AUs do not occur at all.




# DRPFROMTS---------------------------------------------------------------------

# Change name of all columns in pitch data file to combine them for drpfromts---

year3_pitch14_Choos3Wisely <- dplyr::rename(year3_pitch14_Choos3Wisely,
                                      frame_p = frame,
                                      timestamp_p = timestamp,
                                      AU06_c_p = AU06_c,
                                      AU12_c_p = AU12_c,
                                      AU25_c_p = AU25_c)


# Join the data set into one for the drpfromts
ts_pitch14_judge1 <- bind_cols(year3_pitch14_Choos3Wisely,
                               year3_judge1_Choos3Wisely)

ts_pitch14_judge2 <- bind_cols(year3_pitch14_Choos3Wisely,
                               year3_judge2_Choos3Wisely)

ts_pitch14_judge3 <- bind_cols(year3_pitch14_Choos3Wisely,
                               year3_judge3_Choos3Wisely)

ts_pitch14_judge1$AU06_c_p <- as.factor(ts_pitch14_judge1$AU06_c_p)
ts_pitch14_judge1$AU12_c_p <- as.factor(ts_pitch14_judge1$AU12_c_p)
ts_pitch14_judge1$AU25_c_p <- as.factor(ts_pitch14_judge1$AU25_c_p)
ts_pitch14_judge1$AU06_c <- as.factor(ts_pitch14_judge1$AU06_c)
ts_pitch14_judge1$AU12_c <- as.factor(ts_pitch14_judge1$AU12_c)
ts_pitch14_judge1$AU25_c <- as.factor(ts_pitch14_judge1$AU25_c)

ts_pitch14_judge2$AU06_c_p <- as.factor(ts_pitch14_judge2$AU06_c_p)
ts_pitch14_judge2$AU12_c_p <- as.factor(ts_pitch14_judge2$AU12_c_p)
ts_pitch14_judge2$AU25_c_p <- as.factor(ts_pitch14_judge2$AU25_c_p)
ts_pitch14_judge2$AU06_c <- as.factor(ts_pitch14_judge2$AU06_c)
ts_pitch14_judge2$AU12_c <- as.factor(ts_pitch14_judge2$AU12_c)
ts_pitch14_judge2$AU25_c <- as.factor(ts_pitch14_judge2$AU25_c)

ts_pitch14_judge3$AU06_c_p <- as.factor(ts_pitch14_judge3$AU06_c_p)
ts_pitch14_judge3$AU12_c_p <- as.factor(ts_pitch14_judge3$AU12_c_p)
ts_pitch14_judge3$AU25_c_p <- as.factor(ts_pitch14_judge3$AU25_c_p)
ts_pitch14_judge3$AU06_c <- as.factor(ts_pitch14_judge3$AU06_c)
ts_pitch14_judge3$AU12_c <- as.factor(ts_pitch14_judge3$AU12_c)
ts_pitch14_judge3$AU25_c <- as.factor(ts_pitch14_judge3$AU25_c)



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
pitch14_drpfromts1 <- drpdfromts(cbind(ts_pitch14_judge1$AU06_c_p,
                                       ts_pitch14_judge1$AU12_c_p,
                                       ts_pitch14_judge1$AU25_c_p),
                                 cbind(ts_pitch14_judge1$AU06_c,
                                       ts_pitch14_judge1$AU12_c,
                                       ts_pitch14_judge1$AU25_c),
                                 25)


profile <- pitch14_drpfromts1$profile

maxrec_pitch14_judge1 <- round((max(profile) * 100), 2) # Maximum recurrence is 65.64 %

plot(seq(-25, 25, 1), profile, type = "l", lwd = 5, xlab = "Lag", ylab = "Recurrence")




# DCRP & Maximum recurrence rate judge 2
pitch14_drpfromts2 <- drpdfromts(cbind(ts_pitch14_judge2$AU06_c_p,
                                       ts_pitch14_judge2$AU12_c_p,
                                       ts_pitch14_judge2$AU25_c_p),
                                 cbind(ts_pitch14_judge2$AU06_c,
                                       ts_pitch14_judge2$AU12_c,
                                       ts_pitch14_judge2$AU25_c),
                                 25)

profile <- pitch14_drpfromts2$profile

maxrec_pitch14_judge2 <- round((max(profile) * 100), 2) # Maximum recurrence is 69.88 %

plot(seq(-25, 25, 1), profile, type = "l", lwd = 5, xlab = "Lag", ylab = "Recurrence")




# DCRP & Maximum recurrence rate judge 3
pitch14_drpfromts3 <- drpdfromts(cbind(ts_pitch14_judge3$AU06_c_p,
                                       ts_pitch14_judge3$AU12_c_p,
                                       ts_pitch14_judge3$AU25_c_p),
                                 cbind(ts_pitch14_judge3$AU06_c,
                                       ts_pitch14_judge3$AU12_c,
                                       ts_pitch14_judge3$AU25_c),
                                 25)

profile <- pitch14_drpfromts3$profile

maxrec_pitch14_judge3 <- round((max(profile) * 100), 2) # Maximum recurrence is 83.82 %

plot(seq(-25, 25, 1), profile, type = "l", lwd = 5, xlab = "Lag", ylab = "Recurrence")



# Reference for drpdfromts code-------------------------------------------------

# Van den Broek, T. (2018). Early development and temporal responsivity: A cross-cultural study from the Netherlands, urban Mozambique, and rural Mozambique (Master's thesis). Retrieved from http://arno.uvt.nl/show.cgi?fid=147170

