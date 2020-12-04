
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
# PITCH 5 - HOTIDY

# Read in the OpenFace data of the pitcher and three jury members---------------
year1_pitch5_HOTIDY <- read.delim('/2018-2019_5_HOTIDY.csv',
                                      sep = ',', stringsAsFactors = FALSE) # provide relative path

year1_judge1_HOTIDY <- read.delim('/2018-2019_5_HOTIDY_Judge1-Cropped.csv',
                                      sep = ',', stringsAsFactors = FALSE) # provide relative path

year1_judge2_HOTIDY <- read.delim('/2018-2019_5_HOTIDY_Judge2-Cropped.csv',
                                      sep = ',', stringsAsFactors = FALSE) # provide relative path

year1_judge3_HOTIDY <- read.delim('/2018-2019_5_HOTIDY_Judge3-Cropped.csv',
                                      sep = ',', stringsAsFactors = FALSE) # provide relative path


# Check the dimensions of the data files
dim(year1_pitch5_HOTIDY) # 21347 x 714
dim(year1_judge1_HOTIDY) # 21345 x 714
dim(year1_judge2_HOTIDY) # 21345 x 714
dim(year1_judge3_HOTIDY) # 21345 x 714


# Preprocessing and EDA --------------------------------------------------------

# Select the relevant variables
year1_pitch5_HOTIDY <- year1_pitch5_HOTIDY %>%
  select(frame, timestamp, AU06_c, AU12_c, AU25_c)

year1_judge1_HOTIDY <- year1_judge1_HOTIDY %>%
  select(frame, timestamp, AU06_c, AU12_c, AU25_c)

year1_judge2_HOTIDY <- year1_judge2_HOTIDY %>%
  select(frame, timestamp, AU06_c, AU12_c, AU25_c)

year1_judge3_HOTIDY <- year1_judge3_HOTIDY %>%
  select(frame, timestamp, AU06_c, AU12_c, AU25_c)


# Explore data files
head(year1_pitch5_HOTIDY)
head(year1_judge1_HOTIDY)
head(year1_judge2_HOTIDY)
head(year1_judge3_HOTIDY)


# Summarize data files
summary(year1_pitch5_HOTIDY) # This one has NA values
summary(year1_judge1_HOTIDY)
summary(year1_judge2_HOTIDY)
summary(year1_judge3_HOTIDY)


# Look into distribution of year1_pitch5_HOTIDY
table(year1_pitch5_HOTIDY$frame, useNA = 'always')
table(year1_pitch5_HOTIDY$timestamp, useNA = 'always')
table(year1_pitch5_HOTIDY$AU06_c, useNA = 'always')
table(year1_pitch5_HOTIDY$AU12_c, useNA = 'always')
table(year1_pitch5_HOTIDY$AU25_c, useNA = 'always')


# function for finding NA values
find_NAs <- function(values) {
  which(is.na(values))
}

find_NAs(year1_pitch5_HOTIDY$frame) # NA values at index 21346 21347
find_NAs(year1_pitch5_HOTIDY$timestamp) # NA values at index 21346 21347
find_NAs(year1_pitch5_HOTIDY$AU06_c)
find_NAs(year1_pitch5_HOTIDY$AU12_c)
find_NAs(year1_pitch5_HOTIDY$AU25_c)


year1_pitch5_HOTIDY <- year1_pitch5_HOTIDY %>%
  drop_na # If necessary; delete rows missing values


# Cut out only the pitch data (without Q&A)
year1_pitch5_HOTIDY <- slice(year1_pitch5_HOTIDY, 76:4850)
year1_judge1_HOTIDY <- slice(year1_judge1_HOTIDY, 76:4850)
year1_judge2_HOTIDY <- slice(year1_judge2_HOTIDY, 76:4850)
year1_judge3_HOTIDY <- slice(year1_judge3_HOTIDY, 76:4850)


# Plot distribution of AUs to check their distribution--------------------------
# and check for possible outliers-----------------------------------------------

# AUs pitcher 1
ggplot(year1_pitch5_HOTIDY, aes(x = AU06_c , y = frame)) +
  geom_point() # AU06
ggplot(year1_pitch5_HOTIDY, aes(x = AU12_c , y = frame)) + # Very few present
  geom_point() # AU12
ggplot(year1_pitch5_HOTIDY, aes(x = AU25_c , y = frame)) +
  geom_point() # AU25

# AUs judge 1
ggplot(year1_judge1_HOTIDY, aes(x = AU06_c , y = frame)) +
  geom_point() # AU06
ggplot(year1_judge1_HOTIDY, aes(x = AU12_c , y = frame)) + 
  geom_point() # AU12
ggplot(year1_judge1_HOTIDY, aes(x = AU25_c , y = frame)) + # very few present
  geom_point() # AU25

# AUs judge 2
ggplot(year1_judge2_HOTIDY, aes(x = AU06_c , y = frame)) +
  geom_point() # AU06
ggplot(year1_judge2_HOTIDY, aes(x = AU12_c , y = frame)) + # very few present
  geom_point() # AU12
ggplot(year1_judge2_HOTIDY, aes(x = AU25_c , y = frame)) +
  geom_point() # AU25

# AUs judge 3
ggplot(year1_judge3_HOTIDY, aes(x = AU06_c , y = frame)) + # very few present
  geom_point() # AU06
ggplot(year1_judge3_HOTIDY, aes(x = AU12_c , y = frame)) + # very few present
  geom_point() # AU12
ggplot(year1_judge3_HOTIDY, aes(x = AU25_c , y = frame)) +
  geom_point() # AU25

# There are no outliers detected




# DRPFROMTS---------------------------------------------------------------------

# Change name of all columns in pitch data file to combine them for drpfromts---

year1_pitch5_HOTIDY <- dplyr::rename(year1_pitch5_HOTIDY,
                                         frame_p = frame,
                                         timestamp_p = timestamp,
                                         AU06_c_p = AU06_c,
                                         AU12_c_p = AU12_c,
                                         AU25_c_p = AU25_c)


# Join the data set into one for the drpfromts
ts_pitch5_judge1 <- bind_cols(year1_pitch5_HOTIDY,
                              year1_judge1_HOTIDY)

ts_pitch5_judge2 <- bind_cols(year1_pitch5_HOTIDY,
                              year1_judge2_HOTIDY)

ts_pitch5_judge3 <- bind_cols(year1_pitch5_HOTIDY,
                              year1_judge3_HOTIDY)

ts_pitch5_judge1$AU06_c_p <- as.factor(ts_pitch5_judge1$AU06_c_p)
ts_pitch5_judge1$AU12_c_p <- as.factor(ts_pitch5_judge1$AU12_c_p)
ts_pitch5_judge1$AU25_c_p <- as.factor(ts_pitch5_judge1$AU25_c_p)
ts_pitch5_judge1$AU06_c <- as.factor(ts_pitch5_judge1$AU06_c)
ts_pitch5_judge1$AU12_c <- as.factor(ts_pitch5_judge1$AU12_c)
ts_pitch5_judge1$AU25_c <- as.factor(ts_pitch5_judge1$AU25_c)

ts_pitch5_judge2$AU06_c_p <- as.factor(ts_pitch5_judge2$AU06_c_p)
ts_pitch5_judge2$AU12_c_p <- as.factor(ts_pitch5_judge2$AU12_c_p)
ts_pitch5_judge2$AU25_c_p <- as.factor(ts_pitch5_judge2$AU25_c_p)
ts_pitch5_judge2$AU06_c <- as.factor(ts_pitch5_judge2$AU06_c)
ts_pitch5_judge2$AU12_c <- as.factor(ts_pitch5_judge2$AU12_c)
ts_pitch5_judge2$AU25_c <- as.factor(ts_pitch5_judge2$AU25_c)

ts_pitch5_judge3$AU06_c_p <- as.factor(ts_pitch5_judge3$AU06_c_p)
ts_pitch5_judge3$AU12_c_p <- as.factor(ts_pitch5_judge3$AU12_c_p)
ts_pitch5_judge3$AU25_c_p <- as.factor(ts_pitch5_judge3$AU25_c_p)
ts_pitch5_judge3$AU06_c <- as.factor(ts_pitch5_judge3$AU06_c)
ts_pitch5_judge3$AU12_c <- as.factor(ts_pitch5_judge3$AU12_c)
ts_pitch5_judge3$AU25_c <- as.factor(ts_pitch5_judge3$AU25_c)



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
pitch5_drpfromts1 <- drpdfromts(cbind(ts_pitch5_judge1$AU06_c_p,
                                      ts_pitch5_judge1$AU12_c_p,
                                      ts_pitch5_judge1$AU25_c_p),
                                cbind(ts_pitch5_judge1$AU06_c,
                                      ts_pitch5_judge1$AU12_c,
                                      ts_pitch5_judge1$AU25_c),
                                25)


profile <- pitch5_drpfromts1$profile

maxrec_pitch5_judge1 <- round((max(profile) * 100), 2) # Maximum recurrence is 70.30 %

plot(seq(-25, 25, 1), profile, type = "l", lwd = 5, xlab = "Lag", ylab = "Recurrence")




# DCRP & Maximum recurrence rate judge 2
pitch5_drpfromts2 <- drpdfromts(cbind(ts_pitch5_judge2$AU06_c_p,
                                      ts_pitch5_judge2$AU12_c_p,
                                      ts_pitch5_judge2$AU25_c_p),
                                cbind(ts_pitch5_judge2$AU06_c,
                                      ts_pitch5_judge2$AU12_c,
                                      ts_pitch5_judge2$AU25_c),
                                25)

profile <- pitch5_drpfromts2$profile

maxrec_pitch5_judge2 <- round((max(profile) * 100), 2) # Maximum recurrence is 80.03 %

plot(seq(-25, 25, 1), profile, type = "l", lwd = 5, xlab = "Lag", ylab = "Recurrence")




# DCRP & Maximum recurrence rate judge 3
pitch5_drpfromts3 <- drpdfromts(cbind(ts_pitch5_judge3$AU06_c_p,
                                      ts_pitch5_judge3$AU12_c_p,
                                      ts_pitch5_judge3$AU25_c_p),
                                cbind(ts_pitch5_judge3$AU06_c,
                                      ts_pitch5_judge3$AU12_c,
                                      ts_pitch5_judge3$AU25_c),
                                25)

profile <- pitch5_drpfromts3$profile

maxrec_pitch5_judge3 <- round((max(profile) * 100), 2) # Maximum recurrence is 82.36 %

plot(seq(-25, 25, 1), profile, type = "l", lwd = 5, xlab = "Lag", ylab = "Recurrence")



# Reference for drpdfromts code-------------------------------------------------

# Van den Broek, T. (2018). Early development and temporal responsivity: A cross-cultural study from the Netherlands, urban Mozambique, and rural Mozambique (Master's thesis). Retrieved from http://arno.uvt.nl/show.cgi?fid=147170

