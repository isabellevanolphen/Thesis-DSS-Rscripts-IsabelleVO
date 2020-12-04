
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
# PITCH 6 - Fit Point

# Read in the OpenFace data of the pitcher and three jury members---------------
year1_pitch6_FitPoint <- read.delim('/2018-2019_6_FitPoint.csv',
                                  sep = ',', stringsAsFactors = FALSE) # provide relative path

year1_judge1_FitPoint <- read.delim('/2018-2019_6_FitPoint_Judge1-Cropped.csv',
                                  sep = ',', stringsAsFactors = FALSE) # provide relative path

year1_judge2_FitPoint <- read.delim('/2018-2019_6_FitPoint_Judge2-Cropped.csv',
                                  sep = ',', stringsAsFactors = FALSE) # provide relative path

year1_judge3_FitPoint <- read.delim('/2018-2019_6_FitPoint_Judge3-Cropped.csv',
                                  sep = ',', stringsAsFactors = FALSE) # provide relative path


# Check the dimensions of the data files
dim(year1_pitch6_FitPoint) # 22991 x 714
dim(year1_judge1_FitPoint) # 22989 x 714
dim(year1_judge2_FitPoint) # 22989 x 714
dim(year1_judge3_FitPoint) # 22989 x 714


# Preprocessing and EDA --------------------------------------------------------

# Select the relevant variables
year1_pitch6_FitPoint <- year1_pitch6_FitPoint %>%
  select(frame, timestamp, AU06_c, AU12_c, AU25_c)

year1_judge1_FitPoint <- year1_judge1_FitPoint %>%
  select(frame, timestamp, AU06_c, AU12_c, AU25_c)

year1_judge2_FitPoint <- year1_judge2_FitPoint %>%
  select(frame, timestamp, AU06_c, AU12_c, AU25_c)

year1_judge3_FitPoint <- year1_judge3_FitPoint %>%
  select(frame, timestamp, AU06_c, AU12_c, AU25_c)


# Explore data files
head(year1_pitch6_FitPoint)
head(year1_judge1_FitPoint)
head(year1_judge2_FitPoint)
head(year1_judge3_FitPoint)


# Summarize data files
summary(year1_pitch6_FitPoint) # This one has NA values
summary(year1_judge1_FitPoint)
summary(year1_judge2_FitPoint)
summary(year1_judge3_FitPoint)


# Look into distribution of year1_pitch6_FitPoint
table(year1_pitch6_FitPoint$frame, useNA = 'always')
table(year1_pitch6_FitPoint$timestamp, useNA = 'always')
table(year1_pitch6_FitPoint$AU06_c, useNA = 'always')
table(year1_pitch6_FitPoint$AU12_c, useNA = 'always')
table(year1_pitch6_FitPoint$AU25_c, useNA = 'always')


# function for finding NA values
find_NAs <- function(values) {
  which(is.na(values))
}

find_NAs(year1_pitch6_FitPoint$frame) # NA values at index 22990 22991
find_NAs(year1_pitch6_FitPoint$timestamp) # NA values at index 22990 22991
find_NAs(year1_pitch6_FitPoint$AU06_c)
find_NAs(year1_pitch6_FitPoint$AU12_c)
find_NAs(year1_pitch6_FitPoint$AU25_c)


year1_pitch6_FitPoint <- year1_pitch6_FitPoint %>%
  drop_na # If necessary; delete rows missing values


# Cut out only the pitch data (without Q&A)
year1_pitch6_FitPoint <- slice(year1_pitch6_FitPoint, 126:5125)
year1_judge1_FitPoint <- slice(year1_judge1_FitPoint, 126:5125)
year1_judge2_FitPoint <- slice(year1_judge2_FitPoint, 126:5125)
year1_judge3_FitPoint <- slice(year1_judge3_FitPoint, 126:5125)


# Plot distribution of AUs to check their distribution--------------------------
# and check for possible outliers-----------------------------------------------

# AUs pitcher 1
ggplot(year1_pitch6_FitPoint, aes(x = AU06_c , y = frame)) +
  geom_point() # AU06
ggplot(year1_pitch6_FitPoint, aes(x = AU12_c , y = frame)) +
  geom_point() # AU12
ggplot(year1_pitch6_FitPoint, aes(x = AU25_c , y = frame)) +
  geom_point() # AU25

# AUs judge 1
ggplot(year1_judge1_FitPoint, aes(x = AU06_c , y = frame)) +
  geom_point() # AU06
ggplot(year1_judge1_FitPoint, aes(x = AU12_c , y = frame)) + 
  geom_point() # AU12
ggplot(year1_judge1_FitPoint, aes(x = AU25_c , y = frame)) +
  geom_point() # AU25

# AUs judge 2
ggplot(year1_judge2_FitPoint, aes(x = AU06_c , y = frame)) +
  geom_point() # AU06
ggplot(year1_judge2_FitPoint, aes(x = AU12_c , y = frame)) + # very few present
  geom_point() # AU12
ggplot(year1_judge2_FitPoint, aes(x = AU25_c , y = frame)) +
  geom_point() # AU25

# AUs judge 3
ggplot(year1_judge3_FitPoint, aes(x = AU06_c , y = frame)) +
  geom_point() # AU06
ggplot(year1_judge3_FitPoint, aes(x = AU12_c , y = frame)) + # very few present
  geom_point() # AU12
ggplot(year1_judge3_FitPoint, aes(x = AU25_c , y = frame)) + # very few present
  geom_point() # AU25

# There are no outliers detected




# DRPFROMTS---------------------------------------------------------------------

# Change name of all columns in pitch data file to combine them for drpfromts---

year1_pitch6_FitPoint <- dplyr::rename(year1_pitch6_FitPoint,
                                     frame_p = frame,
                                     timestamp_p = timestamp,
                                     AU06_c_p = AU06_c,
                                     AU12_c_p = AU12_c,
                                     AU25_c_p = AU25_c)


# Join the data set into one for the drpfromts
ts_pitch6_judge1 <- bind_cols(year1_pitch6_FitPoint,
                              year1_judge1_FitPoint)

ts_pitch6_judge2 <- bind_cols(year1_pitch6_FitPoint,
                              year1_judge2_FitPoint)

ts_pitch6_judge3 <- bind_cols(year1_pitch6_FitPoint,
                              year1_judge3_FitPoint)

ts_pitch6_judge1$AU06_c_p <- as.factor(ts_pitch6_judge1$AU06_c_p)
ts_pitch6_judge1$AU12_c_p <- as.factor(ts_pitch6_judge1$AU12_c_p)
ts_pitch6_judge1$AU25_c_p <- as.factor(ts_pitch6_judge1$AU25_c_p)
ts_pitch6_judge1$AU06_c <- as.factor(ts_pitch6_judge1$AU06_c)
ts_pitch6_judge1$AU12_c <- as.factor(ts_pitch6_judge1$AU12_c)
ts_pitch6_judge1$AU25_c <- as.factor(ts_pitch6_judge1$AU25_c)

ts_pitch6_judge2$AU06_c_p <- as.factor(ts_pitch6_judge2$AU06_c_p)
ts_pitch6_judge2$AU12_c_p <- as.factor(ts_pitch6_judge2$AU12_c_p)
ts_pitch6_judge2$AU25_c_p <- as.factor(ts_pitch6_judge2$AU25_c_p)
ts_pitch6_judge2$AU06_c <- as.factor(ts_pitch6_judge2$AU06_c)
ts_pitch6_judge2$AU12_c <- as.factor(ts_pitch6_judge2$AU12_c)
ts_pitch6_judge2$AU25_c <- as.factor(ts_pitch6_judge2$AU25_c)

ts_pitch6_judge3$AU06_c_p <- as.factor(ts_pitch6_judge3$AU06_c_p)
ts_pitch6_judge3$AU12_c_p <- as.factor(ts_pitch6_judge3$AU12_c_p)
ts_pitch6_judge3$AU25_c_p <- as.factor(ts_pitch6_judge3$AU25_c_p)
ts_pitch6_judge3$AU06_c <- as.factor(ts_pitch6_judge3$AU06_c)
ts_pitch6_judge3$AU12_c <- as.factor(ts_pitch6_judge3$AU12_c)
ts_pitch6_judge3$AU25_c <- as.factor(ts_pitch6_judge3$AU25_c)



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
pitch6_drpfromts1 <- drpdfromts(cbind(ts_pitch6_judge1$AU06_c_p,
                                      ts_pitch6_judge1$AU12_c_p,
                                      ts_pitch6_judge1$AU25_c_p),
                                cbind(ts_pitch6_judge1$AU06_c,
                                      ts_pitch6_judge1$AU12_c,
                                      ts_pitch6_judge1$AU25_c),
                                25)


profile <- pitch6_drpfromts1$profile

maxrec_pitch6_judge1 <- round((max(profile) * 100), 2) # Maximum recurrence is 52.21 %

plot(seq(-25, 25, 1), profile, type = "l", lwd = 5, xlab = "Lag", ylab = "Recurrence")




# DCRP & Maximum recurrence rate judge 2
pitch6_drpfromts2 <- drpdfromts(cbind(ts_pitch6_judge2$AU06_c_p,
                                      ts_pitch6_judge2$AU12_c_p,
                                      ts_pitch6_judge2$AU25_c_p),
                                cbind(ts_pitch6_judge2$AU06_c,
                                      ts_pitch6_judge2$AU12_c,
                                      ts_pitch6_judge2$AU25_c),
                                25)

profile <- pitch6_drpfromts2$profile

maxrec_pitch6_judge2 <- round((max(profile) * 100), 2) # Maximum recurrence is 62.59 %

plot(seq(-25, 25, 1), profile, type = "l", lwd = 5, xlab = "Lag", ylab = "Recurrence")




# DCRP & Maximum recurrence rate judge 3
pitch6_drpfromts3 <- drpdfromts(cbind(ts_pitch6_judge3$AU06_c_p,
                                      ts_pitch6_judge3$AU12_c_p,
                                      ts_pitch6_judge3$AU25_c_p),
                                cbind(ts_pitch6_judge3$AU06_c,
                                      ts_pitch6_judge3$AU12_c,
                                      ts_pitch6_judge3$AU25_c),
                                25)

profile <- pitch6_drpfromts3$profile

maxrec_pitch6_judge3 <- round((max(profile) * 100), 2) # Maximum recurrence is 61.16 %

plot(seq(-25, 25, 1), profile, type = "l", lwd = 5, xlab = "Lag", ylab = "Recurrence")



# Reference for drpdfromts code-------------------------------------------------

# Van den Broek, T. (2018). Early development and temporal responsivity: A cross-cultural study from the Netherlands, urban Mozambique, and rural Mozambique (Master's thesis). Retrieved from http://arno.uvt.nl/show.cgi?fid=147170

