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
# PITCH 10 - YoungBoosters

# Read in the OpenFace data of the pitcher and three jury members---------------
year2_pitch10_YoungBoosters<- read.delim('/DEiA_3_YoungBoosters.csv',
                               sep = ',', stringsAsFactors = FALSE) # provide relative path

year2_judge1_YoungBoosters <- read.delim('/DEiA_3_YoungBoosters_Judge1-Cropped.csv',
                                sep = ',', stringsAsFactors = FALSE) # provide relative path

year2_judge2_YoungBoosters <- read.delim('/DEiA_3_YoungBoosters_Judge2-Cropped.csv',
                                sep = ',', stringsAsFactors = FALSE) # provide relative path

year2_judge3_YoungBoosters <- read.delim('/DEiA_3_YoungBoosters_Judge3.csv',
                                sep = ',', stringsAsFactors = FALSE) # provide relative path


# Check the dimensions of the data files
dim(year2_pitch10_YoungBoosters) # 25977 x 714
dim(year2_judge1_YoungBoosters) # 25968 x 714
dim(year2_judge2_YoungBoosters) # 25968 x 714
dim(year2_judge3_YoungBoosters) # 25975 x 714


# Preprocessing and EDA --------------------------------------------------------

# Select the relevant variables
year2_pitch10_YoungBoosters <- year2_pitch10_YoungBoosters %>%
  select(frame, timestamp, AU06_c, AU12_c, AU25_c)

year2_judge1_YoungBoosters <- year2_judge1_YoungBoosters %>%
  select(frame, timestamp, AU06_c, AU12_c, AU25_c)

year2_judge2_YoungBoosters <- year2_judge2_YoungBoosters %>%
  select(frame, timestamp, AU06_c, AU12_c, AU25_c)

year2_judge3_YoungBoosters <- year2_judge3_YoungBoosters %>%
  select(frame, timestamp, AU06_c, AU12_c, AU25_c)


# Explore data files
head(year2_pitch10_YoungBoosters)
head(year2_judge1_YoungBoosters)
head(year2_judge2_YoungBoosters)
head(year2_judge3_YoungBoosters)


# Summarize data files
summary(year2_pitch10_YoungBoosters) # This one has NA values
summary(year2_judge1_YoungBoosters)
summary(year2_judge2_YoungBoosters)
summary(year2_judge3_YoungBoosters)


# Look into distribution of year2_pitch10_YoungBoosters
table(year2_pitch10_YoungBoosters$frame, useNA = 'always')
table(year2_pitch10_YoungBoosters$timestamp, useNA = 'always')
table(year2_pitch10_YoungBoosters$AU06_c, useNA = 'always')
table(year2_pitch10_YoungBoosters$AU12_c, useNA = 'always')
table(year2_pitch10_YoungBoosters$AU25_c, useNA = 'always')


# function for finding NA values
find_NAs <- function(values) {
  which(is.na(values))
}

find_NAs(year2_pitch10_YoungBoosters$frame) # NA values at index 25976 25977
find_NAs(year2_pitch10_YoungBoosters$timestamp) # NA values at index 25976 25977
find_NAs(year2_pitch10_YoungBoosters$AU06_c)
find_NAs(year2_pitch10_YoungBoosters$AU12_c)
find_NAs(year2_pitch10_YoungBoosters$AU25_c)


year2_pitch10_YoungBoosters <- year2_pitch10_YoungBoosters %>%
  drop_na # If necyear2_pitch10_YoungBoostersessary; delete rows missing values


# Cut out only the pitch data (without Q&A)
year2_pitch10_YoungBoosters <- slice(year2_pitch10_YoungBoosters, 51:4225)
year2_judge1_YoungBoosters <- slice(year2_judge1_YoungBoosters, 51:4225)
year2_judge2_YoungBoosters <- slice(year2_judge2_YoungBoosters, 51:4225)
year2_judge3_YoungBoosters <- slice(year2_judge3_YoungBoosters, 51:4225)


# Plot distribution of AUs to check their distribution--------------------------
# and check for possible outliers-----------------------------------------------

# AUs pitcher 1
ggplot(year2_pitch10_YoungBoosters, aes(x = AU06_c , y = frame)) + 
  geom_point() # AU06
ggplot(year2_pitch10_YoungBoosters, aes(x = AU12_c , y = frame)) +
  geom_point() # AU12
ggplot(year2_pitch10_YoungBoosters, aes(x = AU25_c , y = frame)) +
  geom_point() # AU25

# AUs judge 1
ggplot(year2_judge1_YoungBoosters, aes(x = AU06_c , y = frame)) + # very few present
  geom_point() # AU06
ggplot(year2_judge1_YoungBoosters, aes(x = AU12_c , y = frame)) + # very few present
  geom_point() # AU12
ggplot(year2_judge1_YoungBoosters, aes(x = AU25_c , y = frame)) +
  geom_point() # AU25

# AUs judge 2
ggplot(year2_judge2_YoungBoosters, aes(x = AU06_c , y = frame)) +
  geom_point() # AU06
ggplot(year2_judge2_YoungBoosters, aes(x = AU12_c , y = frame)) +
  geom_point() # AU12
ggplot(year2_judge2_YoungBoosters, aes(x = AU25_c , y = frame)) + # very few present
  geom_point() # AU25

# AUs judge 3
ggplot(year2_judge3_YoungBoosters, aes(x = AU06_c , y = frame)) +
  geom_point() # AU06
ggplot(year2_judge3_YoungBoosters, aes(x = AU12_c , y = frame)) + # very few present
  geom_point() # AU12
ggplot(year2_judge3_YoungBoosters, aes(x = AU25_c , y = frame)) +
  geom_point() # AU25

# There are no outliers detected]




# DRPFROMTS---------------------------------------------------------------------

# Change name of all columns in pitch data file to combine them for drpfromts---

year2_pitch10_YoungBoosters <- dplyr::rename(year2_pitch10_YoungBoosters,
                                   frame_p = frame,
                                   timestamp_p = timestamp,
                                   AU06_c_p = AU06_c,
                                   AU12_c_p = AU12_c,
                                   AU25_c_p = AU25_c)


# Join the data set into one for the drpfromts
ts_pitch10_judge1 <- bind_cols(year2_pitch10_YoungBoosters,
                              year2_judge1_YoungBoosters)

ts_pitch10_judge2 <- bind_cols(year2_pitch10_YoungBoosters,
                              year2_judge2_YoungBoosters)

ts_pitch10_judge3 <- bind_cols(year2_pitch10_YoungBoosters,
                              year2_judge3_YoungBoosters)

ts_pitch10_judge1$AU06_c_p <- as.factor(ts_pitch10_judge1$AU06_c_p)
ts_pitch10_judge1$AU12_c_p <- as.factor(ts_pitch10_judge1$AU12_c_p)
ts_pitch10_judge1$AU25_c_p <- as.factor(ts_pitch10_judge1$AU25_c_p)
ts_pitch10_judge1$AU06_c <- as.factor(ts_pitch10_judge1$AU06_c)
ts_pitch10_judge1$AU12_c <- as.factor(ts_pitch10_judge1$AU12_c)
ts_pitch10_judge1$AU25_c <- as.factor(ts_pitch10_judge1$AU25_c)

ts_pitch10_judge2$AU06_c_p <- as.factor(ts_pitch10_judge2$AU06_c_p)
ts_pitch10_judge2$AU12_c_p <- as.factor(ts_pitch10_judge2$AU12_c_p)
ts_pitch10_judge2$AU25_c_p <- as.factor(ts_pitch10_judge2$AU25_c_p)
ts_pitch10_judge2$AU06_c <- as.factor(ts_pitch10_judge2$AU06_c)
ts_pitch10_judge2$AU12_c <- as.factor(ts_pitch10_judge2$AU12_c)
ts_pitch10_judge2$AU25_c <- as.factor(ts_pitch10_judge2$AU25_c)

ts_pitch10_judge3$AU06_c_p <- as.factor(ts_pitch10_judge3$AU06_c_p)
ts_pitch10_judge3$AU12_c_p <- as.factor(ts_pitch10_judge3$AU12_c_p)
ts_pitch10_judge3$AU25_c_p <- as.factor(ts_pitch10_judge3$AU25_c_p)
ts_pitch10_judge3$AU06_c <- as.factor(ts_pitch10_judge3$AU06_c)
ts_pitch10_judge3$AU12_c <- as.factor(ts_pitch10_judge3$AU12_c)
ts_pitch10_judge3$AU25_c <- as.factor(ts_pitch10_judge3$AU25_c)



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
pitch10_drpfromts1 <- drpdfromts(cbind(ts_pitch10_judge1$AU06_c_p,
                                      ts_pitch10_judge1$AU12_c_p,
                                      ts_pitch10_judge1$AU25_c_p),
                                cbind(ts_pitch10_judge1$AU06_c,
                                      ts_pitch10_judge1$AU12_c,
                                      ts_pitch10_judge1$AU25_c),
                                25)


profile <- pitch10_drpfromts1$profile

maxrec_pitch10_judge1 <- round((max(profile) * 100), 2) # Maximum recurrence is 55.70 %

plot(seq(-25, 25, 1), profile, type = "l", lwd = 5, xlab = "Lag", ylab = "Recurrence")




# DCRP & Maximum recurrence rate judge 2
pitch10_drpfromts2 <- drpdfromts(cbind(ts_pitch10_judge2$AU06_c_p,
                                      ts_pitch10_judge2$AU12_c_p,
                                      ts_pitch10_judge2$AU25_c_p),
                                cbind(ts_pitch10_judge2$AU06_c,
                                      ts_pitch10_judge2$AU12_c,
                                      ts_pitch10_judge2$AU25_c),
                                25)

profile <- pitch10_drpfromts2$profile

maxrec_pitch10_judge2 <- round((max(profile) * 100), 2) # Maximum recurrence is 57.40 %

plot(seq(-25, 25, 1), profile, type = "l", lwd = 5, xlab = "Lag", ylab = "Recurrence")




# DCRP & Maximum recurrence rate judge 3
pitch10_drpfromts3 <- drpdfromts(cbind(ts_pitch10_judge3$AU06_c_p,
                                      ts_pitch10_judge3$AU12_c_p,
                                      ts_pitch10_judge3$AU25_c_p),
                                cbind(ts_pitch10_judge3$AU06_c,
                                      ts_pitch10_judge3$AU12_c,
                                      ts_pitch10_judge3$AU25_c),
                                25)

profile <- pitch10_drpfromts3$profile

maxrec_pitch10_judge3 <- round((max(profile) * 100), 2) # Maximum recurrence is 63.02 %

plot(seq(-25, 25, 1), profile, type = "l", lwd = 5, xlab = "Lag", ylab = "Recurrence")



# Reference for drpdfromts code-------------------------------------------------

# Van den Broek, T. (2018). Early development and temporal responsivity: A cross-cultural study from the Netherlands, urban Mozambique, and rural Mozambique (Master's thesis). Retrieved from http://arno.uvt.nl/show.cgi?fid=147170

