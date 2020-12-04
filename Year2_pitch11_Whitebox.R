
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
# PITCH 11 - Whitebox

# Read in the OpenFace data of the pitcher and three jury members---------------
year2_pitch11_Whitebox <- read.delim('/DEiA_4_Whitebox.csv',
                                         sep = ',', stringsAsFactors = FALSE) # provide relative path

year2_judge1_Whitebox <- read.delim('/DEiA_4_Whitebox_Judge1-Cropped.csv',
                                         sep = ',', stringsAsFactors = FALSE) # provide relative path

year2_judge2_Whitebox <- read.delim('/DEiA_4_Whitebox_Judge2-Cropped.csv',
                                         sep = ',', stringsAsFactors = FALSE) # provide relative path

year2_judge3_Whitebox <- read.delim('/DEiA_4_Whitebox_Judge3.csv',
                                         sep = ',', stringsAsFactors = FALSE) # provide relative path


# Check the dimensions of the data files
dim(year2_pitch11_Whitebox) # 20760 x 714
dim(year2_judge1_Whitebox) # 20751 x 714
dim(year2_judge2_Whitebox) # 20751 x 714
dim(year2_judge3_Whitebox) # 20758 x 714


# Preprocessing and EDA --------------------------------------------------------

# Select the relevant variables
year2_pitch11_Whitebox <- year2_pitch11_Whitebox %>%
  select(frame, timestamp, AU06_c, AU12_c, AU25_c)

year2_judge1_Whitebox <- year2_judge1_Whitebox %>%
  select(frame, timestamp, AU06_c, AU12_c, AU25_c)

year2_judge2_Whitebox <- year2_judge2_Whitebox %>%
  select(frame, timestamp, AU06_c, AU12_c, AU25_c)

year2_judge3_Whitebox <- year2_judge3_Whitebox %>%
  select(frame, timestamp, AU06_c, AU12_c, AU25_c)


# Explore data files
head(year2_pitch11_Whitebox)
head(year2_judge1_Whitebox)
head(year2_judge2_Whitebox)
head(year2_judge3_Whitebox)


# Summarize data files
summary(year2_pitch11_Whitebox) # This one has NA values
summary(year2_judge1_Whitebox)
summary(year2_judge2_Whitebox)
summary(year2_judge3_Whitebox)


# Look into distribution of year2_pitch11_Whitebox
table(year2_pitch11_Whitebox$frame, useNA = 'always')
table(year2_pitch11_Whitebox$timestamp, useNA = 'always')
table(year2_pitch11_Whitebox$AU06_c, useNA = 'always')
table(year2_pitch11_Whitebox$AU12_c, useNA = 'always')
table(year2_pitch11_Whitebox$AU25_c, useNA = 'always')


# function for finding NA values
find_NAs <- function(values) {
  which(is.na(values))
}

find_NAs(year2_pitch11_Whitebox$frame) # NA values at index 20759 20760
find_NAs(year2_pitch11_Whitebox$timestamp) # NA values at index 20759 20760
find_NAs(year2_pitch11_Whitebox$AU06_c)
find_NAs(year2_pitch11_Whitebox$AU12_c)
find_NAs(year2_pitch11_Whitebox$AU25_c)


year2_pitch11_Whitebox <- year2_pitch11_Whitebox %>%
  drop_na # If necessary; delete rows missing values


# Cut out only the pitch data (without Q&A)
year2_pitch11_Whitebox <- slice(year2_pitch11_Whitebox, 51:5075)
year2_judge1_Whitebox <- slice(year2_judge1_Whitebox, 51:5075)
year2_judge2_Whitebox <- slice(year2_judge2_Whitebox, 51:5075)
year2_judge3_Whitebox <- slice(year2_judge3_Whitebox, 51:5075)


# Plot distribution of AUs to check their distribution--------------------------
# and check for possible outliers-----------------------------------------------

# AUs pitcher 1
ggplot(year2_pitch11_Whitebox, aes(x = AU06_c , y = frame)) + 
  geom_point() # AU06
ggplot(year2_pitch11_Whitebox, aes(x = AU12_c , y = frame)) + # very few present
  geom_point() # AU12
ggplot(year2_pitch11_Whitebox, aes(x = AU25_c , y = frame)) +
  geom_point() # AU25

# AUs judge 1
ggplot(year2_judge1_Whitebox, aes(x = AU06_c , y = frame)) +
  geom_point() # AU06
ggplot(year2_judge1_Whitebox, aes(x = AU12_c , y = frame)) +
  geom_point() # AU12
ggplot(year2_judge1_Whitebox, aes(x = AU25_c , y = frame)) +
  geom_point() # AU25

# AUs judge 2
ggplot(year2_judge2_Whitebox, aes(x = AU06_c , y = frame)) + # Very few present
  geom_point() # AU06
ggplot(year2_judge2_Whitebox, aes(x = AU12_c , y = frame)) +
  geom_point() # AU12
ggplot(year2_judge2_Whitebox, aes(x = AU25_c , y = frame)) +
  geom_point() # AU25

# AUs judge 3
ggplot(year2_judge3_Whitebox, aes(x = AU06_c , y = frame)) + # very few present
  geom_point() # AU06
ggplot(year2_judge3_Whitebox, aes(x = AU12_c , y = frame)) + # very few present
  geom_point() # AU12
ggplot(year2_judge3_Whitebox, aes(x = AU25_c , y = frame)) +
  geom_point() # AU25

# There are no outliers detected




# DRPFROMTS---------------------------------------------------------------------

# Change name of all columns in pitch data file to combine them for drpfromts---

year2_pitch11_Whitebox <- dplyr::rename(year2_pitch11_Whitebox,
                                             frame_p = frame,
                                             timestamp_p = timestamp,
                                             AU06_c_p = AU06_c,
                                             AU12_c_p = AU12_c,
                                             AU25_c_p = AU25_c)


# Join the data set into one for the drpfromts
ts_pitch11_judge1 <- bind_cols(year2_pitch11_Whitebox,
                               year2_judge1_Whitebox)

ts_pitch11_judge2 <- bind_cols(year2_pitch11_Whitebox,
                               year2_judge2_Whitebox)

ts_pitch11_judge3 <- bind_cols(year2_pitch11_Whitebox,
                               year2_judge3_Whitebox)

ts_pitch11_judge1$AU06_c_p <- as.factor(ts_pitch11_judge1$AU06_c_p)
ts_pitch11_judge1$AU12_c_p <- as.factor(ts_pitch11_judge1$AU12_c_p)
ts_pitch11_judge1$AU25_c_p <- as.factor(ts_pitch11_judge1$AU25_c_p)
ts_pitch11_judge1$AU06_c <- as.factor(ts_pitch11_judge1$AU06_c)
ts_pitch11_judge1$AU12_c <- as.factor(ts_pitch11_judge1$AU12_c)
ts_pitch11_judge1$AU25_c <- as.factor(ts_pitch11_judge1$AU25_c)

ts_pitch11_judge2$AU06_c_p <- as.factor(ts_pitch11_judge2$AU06_c_p)
ts_pitch11_judge2$AU12_c_p <- as.factor(ts_pitch11_judge2$AU12_c_p)
ts_pitch11_judge2$AU25_c_p <- as.factor(ts_pitch11_judge2$AU25_c_p)
ts_pitch11_judge2$AU06_c <- as.factor(ts_pitch11_judge2$AU06_c)
ts_pitch11_judge2$AU12_c <- as.factor(ts_pitch11_judge2$AU12_c)
ts_pitch11_judge2$AU25_c <- as.factor(ts_pitch11_judge2$AU25_c)

ts_pitch11_judge3$AU06_c_p <- as.factor(ts_pitch11_judge3$AU06_c_p)
ts_pitch11_judge3$AU12_c_p <- as.factor(ts_pitch11_judge3$AU12_c_p)
ts_pitch11_judge3$AU25_c_p <- as.factor(ts_pitch11_judge3$AU25_c_p)
ts_pitch11_judge3$AU06_c <- as.factor(ts_pitch11_judge3$AU06_c)
ts_pitch11_judge3$AU12_c <- as.factor(ts_pitch11_judge3$AU12_c)
ts_pitch11_judge3$AU25_c <- as.factor(ts_pitch11_judge3$AU25_c)



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
pitch11_drpfromts1 <- drpdfromts(cbind(ts_pitch11_judge1$AU06_c_p,
                                       ts_pitch11_judge1$AU12_c_p,
                                       ts_pitch11_judge1$AU25_c_p),
                                 cbind(ts_pitch11_judge1$AU06_c,
                                       ts_pitch11_judge1$AU12_c,
                                       ts_pitch11_judge1$AU25_c),
                                 25)


profile <- pitch11_drpfromts1$profile

maxrec_pitch11_judge1 <- round((max(profile) * 100), 2) # Maximum recurrence is 73.16 %

plot(seq(-25, 25, 1), profile, type = "l", lwd = 5, xlab = "Lag", ylab = "Recurrence")




# DCRP & Maximum recurrence rate judge 2
pitch11_drpfromts2 <- drpdfromts(cbind(ts_pitch11_judge2$AU06_c_p,
                                       ts_pitch11_judge2$AU12_c_p,
                                       ts_pitch11_judge2$AU25_c_p),
                                 cbind(ts_pitch11_judge2$AU06_c,
                                       ts_pitch11_judge2$AU12_c,
                                       ts_pitch11_judge2$AU25_c),
                                 25)

profile <- pitch11_drpfromts2$profile

maxrec_pitch11_judge2 <- round((max(profile) * 100), 2) # Maximum recurrence is 76.06 %

plot(seq(-25, 25, 1), profile, type = "l", lwd = 5, xlab = "Lag", ylab = "Recurrence")




# DCRP & Maximum recurrence rate judge 3
pitch11_drpfromts3 <- drpdfromts(cbind(ts_pitch11_judge3$AU06_c_p,
                                       ts_pitch11_judge3$AU12_c_p,
                                       ts_pitch11_judge3$AU25_c_p),
                                 cbind(ts_pitch11_judge3$AU06_c,
                                       ts_pitch11_judge3$AU12_c,
                                       ts_pitch11_judge3$AU25_c),
                                 25)

profile <- pitch11_drpfromts3$profile

maxrec_pitch11_judge3 <- round((max(profile) * 100), 2) # Maximum recurrence is 81.79 %

plot(seq(-25, 25, 1), profile, type = "l", lwd = 5, xlab = "Lag", ylab = "Recurrence")



# Reference for drpdfromts code-------------------------------------------------

# Van den Broek, T. (2018). Early development and temporal responsivity: A cross-cultural study from the Netherlands, urban Mozambique, and rural Mozambique (Master's thesis). Retrieved from http://arno.uvt.nl/show.cgi?fid=147170

