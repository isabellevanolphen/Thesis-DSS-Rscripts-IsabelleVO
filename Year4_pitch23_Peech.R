
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
# 2019-2020 II
# PITCH 23 - Peech

# Read in the OpenFace data of the pitcher and three jury members---------------
year4_pitch23_Peech <- read.delim('/2019-2020_2-4_Peech.csv',
                                  sep = ',', stringsAsFactors = FALSE) # provide relative path

year4_judge1_Peech <- read.delim('/2019-2020_2-4_Peech_Judge1.csv',
                                 sep = ',', stringsAsFactors = FALSE) # provide relative path

year4_judge2_Peech <- read.delim('/2019-2020_2-4_Peech_Judge2-Cropped.csv',
                                 sep = ',', stringsAsFactors = FALSE) # provide relative path

year4_judge3_Peech <- read.delim('/2019-2020_2-4_Peech_Judge3-Cropped.csv',
                                 sep = ',', stringsAsFactors = FALSE) # provide relative path


# Check the dimensions of the data files
dim(year4_pitch23_Peech) # 17854 x 714
dim(year4_judge1_Peech) # 17852 x 714
dim(year4_judge2_Peech) # 17852 x 714
dim(year4_judge3_Peech) # 17852 x 714


# Preprocessing and EDA --------------------------------------------------------

# Select the relevant variables
year4_pitch23_Peech <- year4_pitch23_Peech  %>%
  select(frame, timestamp, AU06_c, AU12_c, AU25_c)

year4_judge1_Peech <- year4_judge1_Peech %>%
  select(frame, timestamp, AU06_c, AU12_c, AU25_c)

year4_judge2_Peech <- year4_judge2_Peech %>%
  select(frame, timestamp, AU06_c, AU12_c, AU25_c)

year4_judge3_Peech <- year4_judge3_Peech %>%
  select(frame, timestamp, AU06_c, AU12_c, AU25_c)


# Explore data files
head(year4_pitch23_Peech)
head(year4_judge1_Peech)
head(year4_judge2_Peech)
head(year4_judge3_Peech)


# Summarize data files
summary(year4_pitch23_Peech) # This one has NA values
summary(year4_judge1_Peech)
summary(year4_judge2_Peech)
summary(year4_judge3_Peech)


# Look into distribution of year4_pitch23_Peech
table(year4_pitch23_Peech$frame, useNA = 'always')
table(year4_pitch23_Peech$timestamp, useNA = 'always')
table(year4_pitch23_Peech$AU06_c, useNA = 'always')
table(year4_pitch23_Peech$AU12_c, useNA = 'always')
table(year4_pitch23_Peech$AU25_c, useNA = 'always')


# function for finding NA values
find_NAs <- function(values) {
  which(is.na(values))
}

find_NAs(year4_pitch23_Peech$frame) # NA values at index 22988 22989
find_NAs(year4_pitch23_Peech$timestamp) # NA values at index 22988 22989
find_NAs(year4_pitch23_Peech$AU06_c)
find_NAs(year4_pitch23_Peech$AU12_c)
find_NAs(year4_pitch23_Peech$AU25_c)


year4_pitch23_Peech  <- year4_pitch23_Peech  %>%
  drop_na # If necessary; delete rows missing values


# Cut out only the pitch data (without Q&A)
year4_pitch23_Peech <- slice(year4_pitch23_Peech, 1:6450)
year4_judge1_Peech <- slice(year4_judge1_Peech, 1:6450)
year4_judge2_Peech <- slice(year4_judge2_Peech, 1:6450)
year4_judge3_Peech <- slice(year4_judge3_Peech, 1:6450)


# Plot distribution of AUs to check their distribution--------------------------
# and check for possible outliers-----------------------------------------------

# AUs pitcher 1
ggplot(year4_pitch23_Peech, aes(x = AU06_c , y = frame)) + # very few present
  geom_point() # AU06
ggplot(year4_pitch23_Peech, aes(x = AU12_c , y = frame)) + # very few present
  geom_point() # AU12
ggplot(year4_pitch23_Peech, aes(x = AU25_c , y = frame)) +
  geom_point() # AU25

# AUs judge 1
ggplot(year4_judge1_Peech, aes(x = AU06_c , y = frame)) +
  geom_point() # AU06
ggplot(year4_judge1_Peech, aes(x = AU12_c , y = frame)) + # very few present
  geom_point() # AU12
ggplot(year4_judge1_Peech, aes(x = AU25_c , y = frame)) +
  geom_point() # AU25

# AUs judge 2
ggplot(year4_judge2_Peech, aes(x = AU06_c , y = frame)) +
  geom_point() # AU06
ggplot(year4_judge2_Peech, aes(x = AU12_c , y = frame)) +
  geom_point() # AU12
ggplot(year4_judge2_Peech, aes(x = AU25_c , y = frame)) +
  geom_point() # AU25

# AUs judge 3
ggplot(year4_judge3_Peech, aes(x = AU06_c , y = frame)) +
  geom_point() # AU06
ggplot(year4_judge3_Peech, aes(x = AU12_c , y = frame)) +
  geom_point() # AU12
ggplot(year4_judge3_Peech, aes(x = AU25_c , y = frame)) +
  geom_point() # AU25

# There are no outliers detected




# DRPFROMTS---------------------------------------------------------------------

# Change name of all columns in pitch data file to combine them for drpfromts---

year4_pitch23_Peech <- dplyr::rename(year4_pitch23_Peech,
                                     frame_p = frame,
                                     timestamp_p = timestamp,
                                     AU06_c_p = AU06_c,
                                     AU12_c_p = AU12_c,
                                     AU25_c_p = AU25_c)


# Join the data set into one for the drpfromts
ts_pitch23_judge1 <- bind_cols(year4_pitch23_Peech,
                               year4_judge1_Peech)

ts_pitch23_judge2 <- bind_cols(year4_pitch23_Peech,
                               year4_judge2_Peech)

ts_pitch23_judge3 <- bind_cols(year4_pitch23_Peech,
                               year4_judge3_Peech)

ts_pitch23_judge1$AU06_c_p <- as.factor(ts_pitch23_judge1$AU06_c_p)
ts_pitch23_judge1$AU12_c_p <- as.factor(ts_pitch23_judge1$AU12_c_p)
ts_pitch23_judge1$AU25_c_p <- as.factor(ts_pitch23_judge1$AU25_c_p)
ts_pitch23_judge1$AU06_c <- as.factor(ts_pitch23_judge1$AU06_c)
ts_pitch23_judge1$AU12_c <- as.factor(ts_pitch23_judge1$AU12_c)
ts_pitch23_judge1$AU25_c <- as.factor(ts_pitch23_judge1$AU25_c)

ts_pitch23_judge2$AU06_c_p <- as.factor(ts_pitch23_judge2$AU06_c_p)
ts_pitch23_judge2$AU12_c_p <- as.factor(ts_pitch23_judge2$AU12_c_p)
ts_pitch23_judge2$AU25_c_p <- as.factor(ts_pitch23_judge2$AU25_c_p)
ts_pitch23_judge2$AU06_c <- as.factor(ts_pitch23_judge2$AU06_c)
ts_pitch23_judge2$AU12_c <- as.factor(ts_pitch23_judge2$AU12_c)
ts_pitch23_judge2$AU25_c <- as.factor(ts_pitch23_judge2$AU25_c)

ts_pitch23_judge3$AU06_c_p <- as.factor(ts_pitch23_judge3$AU06_c_p)
ts_pitch23_judge3$AU12_c_p <- as.factor(ts_pitch23_judge3$AU12_c_p)
ts_pitch23_judge3$AU25_c_p <- as.factor(ts_pitch23_judge3$AU25_c_p)
ts_pitch23_judge3$AU06_c <- as.factor(ts_pitch23_judge3$AU06_c)
ts_pitch23_judge3$AU12_c <- as.factor(ts_pitch23_judge3$AU12_c)
ts_pitch23_judge3$AU25_c <- as.factor(ts_pitch23_judge3$AU25_c)


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
pitch23_drpfromts1 <- drpdfromts(cbind(ts_pitch23_judge1$AU06_c_p,
                                       ts_pitch23_judge1$AU12_c_p,
                                       ts_pitch23_judge1$AU25_c_p),
                                 cbind(ts_pitch23_judge1$AU06_c,
                                       ts_pitch23_judge1$AU12_c,
                                       ts_pitch23_judge1$AU25_c),
                                 25)


profile <- pitch23_drpfromts1$profile

maxrec_pitch23_judge1 <- round((max(profile) * 100), 2) # Maximum recurrence is 67.00 %

plot(seq(-25, 25, 1), profile, type = "l", lwd = 5, xlab = "Lag", ylab = "Recurrence")




# DCRP & Maximum recurrence rate judge 2
pitch23_drpfromts2 <- drpdfromts(cbind(ts_pitch23_judge2$AU06_c_p,
                                       ts_pitch23_judge2$AU12_c_p,
                                       ts_pitch23_judge2$AU25_c_p),
                                 cbind(ts_pitch23_judge2$AU06_c,
                                       ts_pitch23_judge2$AU12_c,
                                       ts_pitch23_judge2$AU25_c),
                                 25)

profile <- pitch23_drpfromts2$profile

maxrec_pitch23_judge2 <- round((max(profile) * 100), 2) # Maximum recurrence is 52.32 %

plot(seq(-25, 25, 1), profile, type = "l", lwd = 5, xlab = "Lag", ylab = "Recurrence")




# DCRP & Maximum recurrence rate judge 3
pitch23_drpfromts3 <- drpdfromts(cbind(ts_pitch23_judge3$AU06_c_p,
                                       ts_pitch23_judge3$AU12_c_p,
                                       ts_pitch23_judge3$AU25_c_p),
                                 cbind(ts_pitch23_judge3$AU06_c,
                                       ts_pitch23_judge3$AU12_c,
                                       ts_pitch23_judge3$AU25_c),
                                 25)

profile <- pitch23_drpfromts3$profile

maxrec_pitch23_judge3 <- round((max(profile) * 100), 2) # Maximum recurrence is 58.05 %

plot(seq(-25, 25, 1), profile, type = "l", lwd = 5, xlab = "Lag", ylab = "Recurrence")



# Reference for drpdfromts code-------------------------------------------------

# Van den Broek, T. (2018). Early development and temporal responsivity: A cross-cultural study from the Netherlands, urban Mozambique, and rural Mozambique (Master's thesis). Retrieved from http://arno.uvt.nl/show.cgi?fid=147170
