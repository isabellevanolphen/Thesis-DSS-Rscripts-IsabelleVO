
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
# PITCH 2 - FLIPR

# Read in the OpenFace data of the pitcher and three jury members---------------
year1_pitch2_FLIPR <- read.delim('/2018-2019_2_FLIPR.csv',
                                 sep = ',', stringsAsFactors = FALSE) # provide relative path

year1_judge1_FLIPR <- read.delim('/2018-2019_2_FLIPR_Judge1-Cropped.csv',
                                 sep = ',', stringsAsFactors = FALSE) # provide relative path

year1_judge2_FLIPR <- read.delim('/2018-2019_2_FLIPR_Judge2-Cropped.csv',
                                 sep = ',', stringsAsFactors = FALSE) # provide relative path

year1_judge3_FLIPR <- read.delim('/2018-2019_2_FLIPR_Judge3.csv',
                                 sep = ',', stringsAsFactors = FALSE) # provide relative path


# Check the dimensions of the data files
dim(year1_pitch2_FLIPR) # 23741 x 714 ---> this one is different
dim(year1_judge1_FLIPR) # 23732 x 714
dim(year1_judge2_FLIPR) # 23732 x 714
dim(year1_judge3_FLIPR) # 23732 x 714


# Preprocessing and EDA --------------------------------------------------------

# Select the relevant variables
year1_pitch2_FLIPR <- year1_pitch2_FLIPR %>%
  select(frame, timestamp, AU06_c, AU12_c, AU25_c)

year1_judge1_FLIPR <- year1_judge1_FLIPR %>%
  select(frame, timestamp, AU06_c, AU12_c, AU25_c)

year1_judge2_FLIPR <- year1_judge2_FLIPR %>%
  select(frame, timestamp, AU06_c, AU12_c, AU25_c)

year1_judge3_FLIPR <- year1_judge3_FLIPR %>%
  select(frame, timestamp, AU06_c, AU12_c, AU25_c)


# Explore data files
head(year1_pitch2_FLIPR)
head(year1_judge1_FLIPR)
head(year1_judge2_FLIPR)
head(year1_judge3_FLIPR)


# Summarize data files
summary(year1_pitch2_FLIPR) # This one has NA values
summary(year1_judge1_FLIPR)
summary(year1_judge2_FLIPR)
summary(year1_judge3_FLIPR)


# Look into distribution of year1_pitch2_FLIPR
table(year1_pitch2_FLIPR$frame, useNA = 'always')
table(year1_pitch2_FLIPR$timestamp, useNA = 'always')
table(year1_pitch2_FLIPR$AU06_c, useNA = 'always')
table(year1_pitch2_FLIPR$AU12_c, useNA = 'always')
table(year1_pitch2_FLIPR$AU25_c, useNA = 'always')


# function for finding NA values
find_NAs <- function(values) {
  which(is.na(values))
}

find_NAs(year1_pitch2_FLIPR$frame) # NA values at index 18525 18526
find_NAs(year1_pitch2_FLIPR$timestamp) # NA values at index 18525 18526
find_NAs(year1_pitch2_FLIPR$AU06_c)
find_NAs(year1_pitch2_FLIPR$AU12_c)
find_NAs(year1_pitch2_FLIPR$AU25_c)


year1_pitch2_FLIPR <- year1_pitch2_FLIPR %>%
  drop_na # If necessary; delete rows missing values


# Cut out only the pitch data (without Q&A)
year1_pitch2_FLIPR <- slice(year1_pitch2_FLIPR, 101:4900)
year1_judge1_FLIPR <- slice(year1_judge1_FLIPR, 101:4900)
year1_judge2_FLIPR <- slice(year1_judge2_FLIPR, 101:4900)
year1_judge3_FLIPR <- slice(year1_judge3_FLIPR, 101:4900)



# Plot distribution of AUs to check their distribution--------------------------
# and check for possible outliers-----------------------------------------------

# AUs pitcher 1
ggplot(year1_pitch2_FLIPR, aes(x = AU06_c , y = frame)) +
  geom_point() # AU06
ggplot(year1_pitch2_FLIPR, aes(x = AU12_c , y = frame)) + 
  geom_point() # AU12
ggplot(year1_pitch2_FLIPR, aes(x = AU25_c , y = frame)) +
  geom_point() # AU25

# AUs judge 1
ggplot(year1_judge1_FLIPR, aes(x = AU06_c , y = frame)) +
  geom_point() # AU06
ggplot(year1_judge1_FLIPR, aes(x = AU12_c , y = frame)) + 
  geom_point() # AU12
ggplot(year1_judge1_FLIPR, aes(x = AU25_c , y = frame)) +
  geom_point() # AU25

# AUs judge 2
ggplot(year1_judge2_FLIPR, aes(x = AU06_c , y = frame)) +
  geom_point() # AU06
ggplot(year1_judge2_FLIPR, aes(x = AU12_c , y = frame)) + 
  geom_point() # AU12
ggplot(year1_judge2_FLIPR, aes(x = AU25_c , y = frame)) +
  geom_point() # AU25

# AUs judge 3
ggplot(year1_judge3_FLIPR, aes(x = AU06_c , y = frame)) +
  geom_point() # AU06
ggplot(year1_judge3_FLIPR, aes(x = AU12_c , y = frame)) + 
  geom_point() # AU12
ggplot(year1_judge3_FLIPR, aes(x = AU25_c , y = frame)) +
  geom_point() # AU25

# There are no outliers detected




# DRPFROMTS---------------------------------------------------------------------

# Change name of all columns in pitch data file to combine them for drpfromts---

year1_pitch2_FLIPR <- dplyr::rename(year1_pitch2_FLIPR,
                                           frame_p = frame,
                                           timestamp_p = timestamp,
                                           AU06_c_p = AU06_c,
                                           AU12_c_p = AU12_c,
                                           AU25_c_p = AU25_c)


# Join the data set into one for the drpfromts
ts_pitch2_judge1 <- bind_cols(year1_pitch2_FLIPR,
                              year1_judge1_FLIPR)

ts_pitch2_judge2 <- bind_cols(year1_pitch2_FLIPR,
                              year1_judge2_FLIPR)

ts_pitch2_judge3 <- bind_cols(year1_pitch2_FLIPR,
                              year1_judge3_FLIPR)

ts_pitch2_judge1$AU06_c_p <- as.factor(ts_pitch2_judge1$AU06_c_p)
ts_pitch2_judge1$AU12_c_p <- as.factor(ts_pitch2_judge1$AU12_c_p)
ts_pitch2_judge1$AU25_c_p <- as.factor(ts_pitch2_judge1$AU25_c_p)
ts_pitch2_judge1$AU06_c <- as.factor(ts_pitch2_judge1$AU06_c)
ts_pitch2_judge1$AU12_c <- as.factor(ts_pitch2_judge1$AU12_c)
ts_pitch2_judge1$AU25_c <- as.factor(ts_pitch2_judge1$AU25_c)

ts_pitch2_judge2$AU06_c_p <- as.factor(ts_pitch2_judge2$AU06_c_p)
ts_pitch2_judge2$AU12_c_p <- as.factor(ts_pitch2_judge2$AU12_c_p)
ts_pitch2_judge2$AU25_c_p <- as.factor(ts_pitch2_judge2$AU25_c_p)
ts_pitch2_judge2$AU06_c <- as.factor(ts_pitch2_judge2$AU06_c)
ts_pitch2_judge2$AU12_c <- as.factor(ts_pitch2_judge2$AU12_c)
ts_pitch2_judge2$AU25_c <- as.factor(ts_pitch2_judge2$AU25_c)

ts_pitch2_judge3$AU06_c_p <- as.factor(ts_pitch2_judge3$AU06_c_p)
ts_pitch2_judge3$AU12_c_p <- as.factor(ts_pitch2_judge3$AU12_c_p)
ts_pitch2_judge3$AU25_c_p <- as.factor(ts_pitch2_judge3$AU25_c_p)
ts_pitch2_judge3$AU06_c <- as.factor(ts_pitch2_judge3$AU06_c)
ts_pitch2_judge3$AU12_c <- as.factor(ts_pitch2_judge3$AU12_c)
ts_pitch2_judge3$AU25_c <- as.factor(ts_pitch2_judge3$AU25_c)



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
pitch2_drpfromts1 <- drpdfromts(cbind(ts_pitch2_judge1$AU06_c_p,
                                      ts_pitch2_judge1$AU12_c_p,
                                      ts_pitch2_judge1$AU25_c_p),
                                cbind(ts_pitch2_judge1$AU06_c,
                                      ts_pitch2_judge1$AU12_c,
                                      ts_pitch2_judge1$AU25_c),
                                25)

profile <- pitch2_drpfromts1$profile

maxrec_pitch2_judge1 <- round((max(profile) * 100), 2) # Maximum recurrence is 26.30 %

plot(seq(-25, 25, 1), profile, type = "l", lwd = 5, xlab = "Lag", ylab = "Recurrence")




# DCRP & Maximum recurrence rate judge 2
pitch2_drpfromts2 <- drpdfromts(cbind(ts_pitch2_judge2$AU06_c_p,
                                      ts_pitch2_judge2$AU12_c_p,
                                      ts_pitch2_judge2$AU25_c_p),
                                cbind(ts_pitch2_judge2$AU06_c,
                                      ts_pitch2_judge2$AU12_c,
                                      ts_pitch2_judge2$AU25_c),
                                25)

profile <- pitch2_drpfromts2$profile

maxrec_pitch2_judge2 <- round((max(profile) * 100), 2) # Maximum recurrence is 83.55 %

plot(seq(-25, 25, 1), profile, type = "l", lwd = 5, xlab = "Lag", ylab = "Recurrence")




# DCRP & Maximum recurrence rate judge 3
pitch2_drpfromts3 <- drpdfromts(cbind(ts_pitch2_judge3$AU06_c_p,
                                      ts_pitch2_judge3$AU12_c_p,
                                      ts_pitch2_judge3$AU25_c_p),
                                cbind(ts_pitch2_judge3$AU06_c,
                                      ts_pitch2_judge3$AU12_c,
                                      ts_pitch2_judge3$AU25_c),
                                25)

profile <- pitch2_drpfromts3$profile

maxrec_pitch2_judge3 <- round((max(profile) * 100), 2) # Maximum recurrence is 83.15 %

plot(seq(-25, 25, 1), profile, type = "l", lwd = 5, xlab = "Lag", ylab = "Recurrence")



# Reference for drpdfromts code-------------------------------------------------

# Van den Broek, T. (2018). Early development and temporal responsivity: A cross-cultural study from the Netherlands, urban Mozambique, and rural Mozambique (Master's thesis). Retrieved from http://arno.uvt.nl/show.cgi?fid=147170