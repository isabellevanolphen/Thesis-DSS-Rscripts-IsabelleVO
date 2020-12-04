
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
# PITCH 21 - Recipe-Me

# Read in the OpenFace data of the pitcher and three jury members---------------
year4_pitch21_RecipeMe <- read.delim('/2019-2020_2-2_Recipe-Me.csv',
                                      sep = ',', stringsAsFactors = FALSE) # provide relative path

year4_judge1_RecipeMe <- read.delim('/2019-2020_2-2_Recipe-Me_Judge1.csv',
                                     sep = ',', stringsAsFactors = FALSE) # provide relative path

year4_judge2_RecipeMe <- read.delim('/2019-2020_2-2_Recipe-Me_Judge2-Cropped.csv',
                                    sep = ',', stringsAsFactors = FALSE) # provide relative path

year4_judge3_RecipeMe <- read.delim('/2019-2020_2-2_Recipe-Me_Judge3-Cropped.csv',
                                     sep = ',', stringsAsFactors = FALSE) # provide relative path


# Check the dimensions of the data files
dim(year4_pitch21_RecipeMe) # 22989 x 714
dim(year4_judge1_RecipeMe) # 22987 x 714
dim(year4_judge2_RecipeMe) # 22987 x 714
dim(year4_judge3_RecipeMe) # 22987 x 714


# Preprocessing and EDA --------------------------------------------------------

# Select the relevant variables
year4_pitch21_RecipeMe <- year4_pitch21_RecipeMe  %>%
  select(frame, timestamp, AU06_c, AU12_c, AU25_c)

year4_judge1_RecipeMe <- year4_judge1_RecipeMe %>%
  select(frame, timestamp, AU06_c, AU12_c, AU25_c)

year4_judge2_RecipeMe <- year4_judge2_RecipeMe %>%
  select(frame, timestamp, AU06_c, AU12_c, AU25_c)

year4_judge3_RecipeMe <- year4_judge3_RecipeMe %>%
  select(frame, timestamp, AU06_c, AU12_c, AU25_c)


# Explore data files
head(year4_pitch21_RecipeMe)
head(year4_judge1_RecipeMe)
head(year4_judge2_RecipeMe)
head(year4_judge3_RecipeMe)


# Summarize data files
summary(year4_pitch21_RecipeMe) # This one has NA values
summary(year4_judge1_RecipeMe)
summary(year4_judge2_RecipeMe)
summary(year4_judge3_RecipeMe)


# Look into distribution of year4_pitch21_RecipeMe
table(year4_pitch21_RecipeMe$frame, useNA = 'always')
table(year4_pitch21_RecipeMe$timestamp, useNA = 'always')
table(year4_pitch21_RecipeMe$AU06_c, useNA = 'always')
table(year4_pitch21_RecipeMe$AU12_c, useNA = 'always')
table(year4_pitch21_RecipeMe$AU25_c, useNA = 'always')


# function for finding NA values
find_NAs <- function(values) {
  which(is.na(values))
}

find_NAs(year4_pitch21_RecipeMe$frame) # NA values at index 22988 22989
find_NAs(year4_pitch21_RecipeMe$timestamp) # NA values at index 22988 22989
find_NAs(year4_pitch21_RecipeMe$AU06_c)
find_NAs(year4_pitch21_RecipeMe$AU12_c)
find_NAs(year4_pitch21_RecipeMe$AU25_c)


year4_pitch21_RecipeMe  <- year4_pitch21_RecipeMe  %>%
  drop_na # If necessary; delete rows missing values


# Cut out only the pitch data (without Q&A)
year4_pitch21_RecipeMe <- slice(year4_pitch21_RecipeMe, 1:9275)
year4_judge1_RecipeMe <- slice(year4_judge1_RecipeMe, 1:9275)
year4_judge2_RecipeMe <- slice(year4_judge2_RecipeMe, 1:9275)
year4_judge3_RecipeMe <- slice(year4_judge3_RecipeMe, 1:9275)


# Plot distribution of AUs to check their distribution--------------------------
# and check for possible outliers-----------------------------------------------

# AUs pitcher 1
ggplot(year4_pitch21_RecipeMe, aes(x = AU06_c , y = frame)) + # very few present
  geom_point() # AU06
ggplot(year4_pitch21_RecipeMe, aes(x = AU12_c , y = frame)) + # very few present
  geom_point() # AU12
ggplot(year4_pitch21_RecipeMe, aes(x = AU25_c , y = frame)) +
  geom_point() # AU25

# AUs judge 1
ggplot(year4_judge1_RecipeMe, aes(x = AU06_c , y = frame)) +
  geom_point() # AU06
ggplot(year4_judge1_RecipeMe, aes(x = AU12_c , y = frame)) + # very few present
  geom_point() # AU12
ggplot(year4_judge1_RecipeMe, aes(x = AU25_c , y = frame)) +
  geom_point() # AU25

# AUs judge 2
ggplot(year4_judge2_RecipeMe, aes(x = AU06_c , y = frame)) +
  geom_point() # AU06
ggplot(year4_judge2_RecipeMe, aes(x = AU12_c , y = frame)) +
  geom_point() # AU12
ggplot(year4_judge2_RecipeMe, aes(x = AU25_c , y = frame)) +
  geom_point() # AU25

# AUs judge 3
ggplot(year4_judge3_RecipeMe, aes(x = AU06_c , y = frame)) +
  geom_point() # AU06
ggplot(year4_judge3_RecipeMe, aes(x = AU12_c , y = frame)) +
  geom_point() # AU12
ggplot(year4_judge3_RecipeMe, aes(x = AU25_c , y = frame)) +
  geom_point() # AU25

# There are no outliers detected




# DRPFROMTS---------------------------------------------------------------------

# Change name of all columns in pitch data file to combine them for drpfromts---

year4_pitch21_RecipeMe <- dplyr::rename(year4_pitch21_RecipeMe,
                                         frame_p = frame,
                                         timestamp_p = timestamp,
                                         AU06_c_p = AU06_c,
                                         AU12_c_p = AU12_c,
                                         AU25_c_p = AU25_c)


# Join the data set into one for the drpfromts
ts_pitch21_judge1 <- bind_cols(year4_pitch21_RecipeMe,
                               year4_judge1_RecipeMe)

ts_pitch21_judge2 <- bind_cols(year4_pitch21_RecipeMe,
                               year4_judge2_RecipeMe)

ts_pitch21_judge3 <- bind_cols(year4_pitch21_RecipeMe,
                               year4_judge3_RecipeMe)

ts_pitch21_judge1$AU06_c_p <- as.factor(ts_pitch21_judge1$AU06_c_p)
ts_pitch21_judge1$AU12_c_p <- as.factor(ts_pitch21_judge1$AU12_c_p)
ts_pitch21_judge1$AU25_c_p <- as.factor(ts_pitch21_judge1$AU25_c_p)
ts_pitch21_judge1$AU06_c <- as.factor(ts_pitch21_judge1$AU06_c)
ts_pitch21_judge1$AU12_c <- as.factor(ts_pitch21_judge1$AU12_c)
ts_pitch21_judge1$AU25_c <- as.factor(ts_pitch21_judge1$AU25_c)

ts_pitch21_judge2$AU06_c_p <- as.factor(ts_pitch21_judge2$AU06_c_p)
ts_pitch21_judge2$AU12_c_p <- as.factor(ts_pitch21_judge2$AU12_c_p)
ts_pitch21_judge2$AU25_c_p <- as.factor(ts_pitch21_judge2$AU25_c_p)
ts_pitch21_judge2$AU06_c <- as.factor(ts_pitch21_judge2$AU06_c)
ts_pitch21_judge2$AU12_c <- as.factor(ts_pitch21_judge2$AU12_c)
ts_pitch21_judge2$AU25_c <- as.factor(ts_pitch21_judge2$AU25_c)

ts_pitch21_judge3$AU06_c_p <- as.factor(ts_pitch21_judge3$AU06_c_p)
ts_pitch21_judge3$AU12_c_p <- as.factor(ts_pitch21_judge3$AU12_c_p)
ts_pitch21_judge3$AU25_c_p <- as.factor(ts_pitch21_judge3$AU25_c_p)
ts_pitch21_judge3$AU06_c <- as.factor(ts_pitch21_judge3$AU06_c)
ts_pitch21_judge3$AU12_c <- as.factor(ts_pitch21_judge3$AU12_c)
ts_pitch21_judge3$AU25_c <- as.factor(ts_pitch21_judge3$AU25_c)


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
pitch21_drpfromts1 <- drpdfromts(cbind(ts_pitch21_judge1$AU06_c_p,
                                       ts_pitch21_judge1$AU12_c_p,
                                       ts_pitch21_judge1$AU25_c_p),
                                 cbind(ts_pitch21_judge1$AU06_c,
                                       ts_pitch21_judge1$AU12_c,
                                       ts_pitch21_judge1$AU25_c),
                                 25)


profile <- pitch21_drpfromts1$profile

maxrec_pitch21_judge1 <- round((max(profile) * 100), 2) # Maximum recurrence is 88.95 %

plot(seq(-25, 25, 1), profile, type = "l", lwd = 5, xlab = "Lag", ylab = "Recurrence")




# DCRP & Maximum recurrence rate judge 2
pitch21_drpfromts2 <- drpdfromts(cbind(ts_pitch21_judge2$AU06_c_p,
                                       ts_pitch21_judge2$AU12_c_p,
                                       ts_pitch21_judge2$AU25_c_p),
                                 cbind(ts_pitch21_judge2$AU06_c,
                                       ts_pitch21_judge2$AU12_c,
                                       ts_pitch21_judge2$AU25_c),
                                 25)

profile <- pitch21_drpfromts2$profile

maxrec_pitch21_judge2 <- round((max(profile) * 100), 2) # Maximum recurrence is 75.47 %

plot(seq(-25, 25, 1), profile, type = "l", lwd = 5, xlab = "Lag", ylab = "Recurrence")




# DCRP & Maximum recurrence rate judge 3
pitch21_drpfromts3 <- drpdfromts(cbind(ts_pitch21_judge3$AU06_c_p,
                                       ts_pitch21_judge3$AU12_c_p,
                                       ts_pitch21_judge3$AU25_c_p),
                                 cbind(ts_pitch21_judge3$AU06_c,
                                       ts_pitch21_judge3$AU12_c,
                                       ts_pitch21_judge3$AU25_c),
                                 25)

profile <- pitch21_drpfromts3$profile

maxrec_pitch21_judge3 <- round((max(profile) * 100), 2) # Maximum recurrence is 70.55 %

plot(seq(-25, 25, 1), profile, type = "l", lwd = 5, xlab = "Lag", ylab = "Recurrence")



# Reference for drpdfromts code-------------------------------------------------

# Van den Broek, T. (2018). Early development and temporal responsivity: A cross-cultural study from the Netherlands, urban Mozambique, and rural Mozambique (Master's thesis). Retrieved from http://arno.uvt.nl/show.cgi?fid=147170
