
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
# 2018-2019
# PITCH 4 - RecognEyes

# Read in the OpenFace data of the pitcher and three jury members---------------
year1_pitch4_RecognEyes <- read.delim('/2018-2019_4_RecognEyes.csv',
                                     sep = ',', stringsAsFactors = FALSE) # provide relative path

year1_judge1_RecognEyes <- read.delim('/2018-2019_4_RecognEyes_Judge1-Cropped.csv',
                                     sep = ',', stringsAsFactors = FALSE) # provide relative path

year1_judge2_RecognEyes <- read.delim('/2018-2019_4_RecognEyes_Judge2-Cropped.csv',
                                     sep = ',', stringsAsFactors = FALSE) # provide relative path

year1_judge3_RecognEyes <- read.delim('/2018-2019_4_RecognEyes_Judge3-Cropped.csv',
                                     sep = ',', stringsAsFactors = FALSE) # provide relative path


# Check the dimensions of the data files
dim(year1_pitch4_RecognEyes) # 17277 x 714
dim(year1_judge1_RecognEyes) # 17268 x 714
dim(year1_judge2_RecognEyes) # 17268 x 714
dim(year1_judge3_RecognEyes) # 17275 x 714


# Preprocessing and EDA --------------------------------------------------------

# Select the relevant variables
year1_pitch4_RecognEyes <- year1_pitch4_RecognEyes %>%
  select(frame, timestamp, AU06_c, AU12_c, AU25_c)

year1_judge1_RecognEyes <- year1_judge1_RecognEyes %>%
  select(frame, timestamp, AU06_c, AU12_c, AU25_c)

year1_judge2_RecognEyes <- year1_judge2_RecognEyes %>%
  select(frame, timestamp, AU06_c, AU12_c, AU25_c)

year1_judge3_RecognEyes <- year1_judge3_RecognEyes %>%
  select(frame, timestamp, AU06_c, AU12_c, AU25_c)


# Explore data files
head(year1_pitch4_RecognEyes)
head(year1_judge1_RecognEyes)
head(year1_judge2_RecognEyes)
head(year1_judge3_RecognEyes)


# Summarize data files
summary(year1_pitch4_RecognEyes) # This one has NA values
summary(year1_judge1_RecognEyes)
summary(year1_judge2_RecognEyes)
summary(year1_judge3_RecognEyes)


# Look into distribution of year1_pitch4_RecognEyes
table(year1_pitch4_RecognEyes$frame, useNA = 'always')
table(year1_pitch4_RecognEyes$timestamp, useNA = 'always')
table(year1_pitch4_RecognEyes$AU06_c, useNA = 'always')
table(year1_pitch4_RecognEyes$AU12_c, useNA = 'always')
table(year1_pitch4_RecognEyes$AU25_c, useNA = 'always')


# function for finding NA values
find_NAs <- function(values) {
  which(is.na(values))
}

find_NAs(year1_pitch4_RecognEyes$frame) # NA values at index 17276 17277
find_NAs(year1_pitch4_RecognEyes$timestamp) # NA values at index 17276 17277
find_NAs(year1_pitch4_RecognEyes$AU06_c)
find_NAs(year1_pitch4_RecognEyes$AU12_c)
find_NAs(year1_pitch4_RecognEyes$AU25_c)


year1_pitch4_RecognEyes <- year1_pitch4_RecognEyes %>%
  drop_na # If necessary; delete rows missing values


# Cut out only the pitch data (without Q&A)
year1_pitch4_RecognEyes <- slice(year1_pitch4_RecognEyes, 26:5150)
year1_judge1_RecognEyes <- slice(year1_judge1_RecognEyes, 26:5150)
year1_judge2_RecognEyes <- slice(year1_judge2_RecognEyes, 26:5150)
year1_judge3_RecognEyes <- slice(year1_judge3_RecognEyes, 26:5150)


# Plot distribution of AUs to check their distribution--------------------------
# and check for possible outliers-----------------------------------------------

# AUs pitcher 1
ggplot(year1_pitch4_RecognEyes, aes(x = AU06_c , y = frame)) +
  geom_point() # AU06
ggplot(year1_pitch4_RecognEyes, aes(x = AU12_c , y = frame)) + 
  geom_point() # AU12
ggplot(year1_pitch4_RecognEyes, aes(x = AU25_c , y = frame)) +
  geom_point() # AU25

# AUs judge 1
ggplot(year1_judge1_RecognEyes, aes(x = AU06_c , y = frame)) +
  geom_point() # AU06
ggplot(year1_judge1_RecognEyes, aes(x = AU12_c , y = frame)) + 
  geom_point() # AU12
ggplot(year1_judge1_RecognEyes, aes(x = AU25_c , y = frame)) + # very few present
  geom_point() # AU25

# AUs judge 2
ggplot(year1_judge2_RecognEyes, aes(x = AU06_c , y = frame)) +
  geom_point() # AU06
ggplot(year1_judge2_RecognEyes, aes(x = AU12_c , y = frame)) + # very few present
  geom_point() # AU12
ggplot(year1_judge2_RecognEyes, aes(x = AU25_c , y = frame)) +
  geom_point() # AU25

# AUs judge 3
ggplot(year1_judge3_RecognEyes, aes(x = AU06_c , y = frame)) + # very few present
  geom_point() # AU06
ggplot(year1_judge3_RecognEyes, aes(x = AU12_c , y = frame)) + # very few present
  geom_point() # AU12
ggplot(year1_judge3_RecognEyes, aes(x = AU25_c , y = frame)) +
  geom_point() # AU25

# There are no outliers detected




# DRPFROMTS---------------------------------------------------------------------

# Change name of all columns in pitch data file to combine them for drpfromts---

year1_pitch4_RecognEyes <- dplyr::rename(year1_pitch4_RecognEyes,
                                        frame_p = frame,
                                        timestamp_p = timestamp,
                                        AU06_c_p = AU06_c,
                                        AU12_c_p = AU12_c,
                                        AU25_c_p = AU25_c)


# Join the data set into one for the drpfromts
ts_pitch4_judge1 <- bind_cols(year1_pitch4_RecognEyes,
                              year1_judge1_RecognEyes)

ts_pitch4_judge2 <- bind_cols(year1_pitch4_RecognEyes,
                              year1_judge2_RecognEyes)

ts_pitch4_judge3 <- bind_cols(year1_pitch4_RecognEyes,
                              year1_judge3_RecognEyes)

ts_pitch4_judge1$AU06_c_p <- as.factor(ts_pitch4_judge1$AU06_c_p)
ts_pitch4_judge1$AU12_c_p <- as.factor(ts_pitch4_judge1$AU12_c_p)
ts_pitch4_judge1$AU25_c_p <- as.factor(ts_pitch4_judge1$AU25_c_p)
ts_pitch4_judge1$AU06_c <- as.factor(ts_pitch4_judge1$AU06_c)
ts_pitch4_judge1$AU12_c <- as.factor(ts_pitch4_judge1$AU12_c)
ts_pitch4_judge1$AU25_c <- as.factor(ts_pitch4_judge1$AU25_c)

ts_pitch4_judge2$AU06_c_p <- as.factor(ts_pitch4_judge2$AU06_c_p)
ts_pitch4_judge2$AU12_c_p <- as.factor(ts_pitch4_judge2$AU12_c_p)
ts_pitch4_judge2$AU25_c_p <- as.factor(ts_pitch4_judge2$AU25_c_p)
ts_pitch4_judge2$AU06_c <- as.factor(ts_pitch4_judge2$AU06_c)
ts_pitch4_judge2$AU12_c <- as.factor(ts_pitch4_judge2$AU12_c)
ts_pitch4_judge2$AU25_c <- as.factor(ts_pitch4_judge2$AU25_c)

ts_pitch4_judge3$AU06_c_p <- as.factor(ts_pitch4_judge3$AU06_c_p)
ts_pitch4_judge3$AU12_c_p <- as.factor(ts_pitch4_judge3$AU12_c_p)
ts_pitch4_judge3$AU25_c_p <- as.factor(ts_pitch4_judge3$AU25_c_p)
ts_pitch4_judge3$AU06_c <- as.factor(ts_pitch4_judge3$AU06_c)
ts_pitch4_judge3$AU12_c <- as.factor(ts_pitch4_judge3$AU12_c)
ts_pitch4_judge3$AU25_c <- as.factor(ts_pitch4_judge3$AU25_c)



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
pitch4_drpfromts1 <- drpdfromts(cbind(ts_pitch4_judge1$AU06_c_p,
                                      ts_pitch4_judge1$AU12_c_p,
                                      ts_pitch4_judge1$AU25_c_p),
                                cbind(ts_pitch4_judge1$AU06_c,
                                      ts_pitch4_judge1$AU12_c,
                                      ts_pitch4_judge1$AU25_c),
                                25)


profile <- pitch4_drpfromts1$profile

maxrec_pitch4_judge1 <- round((max(profile) * 100), 2) # Maximum recurrence is 52.63 %

plot(seq(-25, 25, 1), profile, type = "l", lwd = 5, xlab = "Lag", ylab = "Recurrence")




# DCRP & Maximum recurrence rate judge 2
pitch4_drpfromts2 <- drpdfromts(cbind(ts_pitch4_judge2$AU06_c_p,
                                      ts_pitch4_judge2$AU12_c_p,
                                      ts_pitch4_judge2$AU25_c_p),
                                cbind(ts_pitch4_judge2$AU06_c,
                                      ts_pitch4_judge2$AU12_c,
                                      ts_pitch4_judge2$AU25_c),
                                25)

profile <- pitch4_drpfromts2$profile

maxrec_pitch4_judge2 <- round((max(profile) * 100), 2) # Maximum recurrence is 70.39 %

plot(seq(-25, 25, 1), profile, type = "l", lwd = 5, xlab = "Lag", ylab = "Recurrence")




# DCRP & Maximum recurrence rate judge 3
pitch4_drpfromts3 <- drpdfromts(cbind(ts_pitch4_judge3$AU06_c_p,
                                      ts_pitch4_judge3$AU12_c_p,
                                      ts_pitch4_judge3$AU25_c_p),
                                cbind(ts_pitch4_judge3$AU06_c,
                                      ts_pitch4_judge3$AU12_c,
                                      ts_pitch4_judge3$AU25_c),
                                25)

profile <- pitch4_drpfromts3$profile

maxrec_pitch4_judge3 <- round((max(profile) * 100), 2) # Maximum recurrence is 72.17 %

plot(seq(-25, 25, 1), profile, type = "l", lwd = 5, xlab = "Lag", ylab = "Recurrence")



# Reference for drpdfromts code-------------------------------------------------

# Van den Broek, T. (2018). Early development and temporal responsivity: A cross-cultural study from the Netherlands, urban Mozambique, and rural Mozambique (Master's thesis). Retrieved from http://arno.uvt.nl/show.cgi?fid=147170

