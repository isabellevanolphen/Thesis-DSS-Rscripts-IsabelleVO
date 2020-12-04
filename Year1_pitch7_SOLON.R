
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
# PITCH 7 - SOLON

# Read in the OpenFace data of the pitcher and three jury members---------------
year1_pitch7_SOLON <- read.delim('C:/Users/isabe/OneDrive/Documenten/MA Data Science and Society/MA Thesis/Data/Data 2018-2019/7 SOLON/2018-2019_7_SOLON.csv',
                                    sep = ',', stringsAsFactors = FALSE)

year1_judge1_SOLON <- read.delim('C:/Users/isabe/OneDrive/Documenten/MA Data Science and Society/MA Thesis/Data/Data 2018-2019/7 SOLON/2018-2019_7_SOLON_Judge1-Cropped.csv',
                                    sep = ',', stringsAsFactors = FALSE)

year1_judge2_SOLON <- read.delim('C:/Users/isabe/OneDrive/Documenten/MA Data Science and Society/MA Thesis/Data/Data 2018-2019/7 SOLON/2018-2019_7_SOLON_Judge2-Cropped.csv',
                                    sep = ',', stringsAsFactors = FALSE)

year1_judge3_SOLON <- read.delim('C:/Users/isabe/OneDrive/Documenten/MA Data Science and Society/MA Thesis/Data/Data 2018-2019/7 SOLON/2018-2019_7_SOLON_Judge3-Cropped.csv',
                                    sep = ',', stringsAsFactors = FALSE)


# Check the dimensions of the data files
dim(year1_pitch7_SOLON) # 21834 x 714
dim(year1_judge1_SOLON) # 21832 x 714
dim(year1_judge2_SOLON) # 21832 x 714
dim(year1_judge3_SOLON) # 21832 x 714


# Preprocessing and EDA --------------------------------------------------------

# Select the relevant variables
year1_pitch7_SOLON <- year1_pitch7_SOLON %>%
  select(frame, timestamp, AU06_c, AU12_c, AU25_c)

year1_judge1_SOLON <- year1_judge1_SOLON %>%
  select(frame, timestamp, AU06_c, AU12_c, AU25_c)

year1_judge2_SOLON <- year1_judge2_SOLON %>%
  select(frame, timestamp, AU06_c, AU12_c, AU25_c)

year1_judge3_SOLON <- year1_judge3_SOLON %>%
  select(frame, timestamp, AU06_c, AU12_c, AU25_c)


# Explore data files
head(year1_pitch7_SOLON)
head(year1_judge1_SOLON)
head(year1_judge2_SOLON)
head(year1_judge3_SOLON)


# Summarize data files
summary(year1_pitch7_SOLON) # This one has NA values
summary(year1_judge1_SOLON)
summary(year1_judge2_SOLON)
summary(year1_judge3_SOLON)


# Look into distribution of year1_pitch7_SOLON
table(year1_pitch7_SOLON$frame, useNA = 'always')
table(year1_pitch7_SOLON$timestamp, useNA = 'always')
table(year1_pitch7_SOLON$AU06_c, useNA = 'always')
table(year1_pitch7_SOLON$AU12_c, useNA = 'always')
table(year1_pitch7_SOLON$AU25_c, useNA = 'always')


# function for finding NA values
find_NAs <- function(values) {
  which(is.na(values))
}

find_NAs(year1_pitch7_SOLON$frame) # NA values at index 21833 21834
find_NAs(year1_pitch7_SOLON$timestamp) # NA values at index21833 21834
find_NAs(year1_pitch7_SOLON$AU06_c)
find_NAs(year1_pitch7_SOLON$AU12_c)
find_NAs(year1_pitch7_SOLON$AU25_c)


year1_pitch7_SOLON <- year1_pitch7_SOLON %>%
  drop_na # If necessary; delete rows missing values


# Cut out only the pitch data (without Q&A)
year1_pitch7_SOLON <- slice(year1_pitch7_SOLON, 576:8225)
year1_judge1_SOLON <- slice(year1_judge1_SOLON, 576:8225)
year1_judge2_SOLON <- slice(year1_judge2_SOLON, 576:8225)
year1_judge3_SOLON <- slice(year1_judge3_SOLON, 576:8225)


# Plot distribution of AUs to check their distribution--------------------------
# and check for possible outliers-----------------------------------------------

# AUs pitcher 1
ggplot(year1_pitch7_SOLON, aes(x = AU06_c , y = frame)) + # very few present
  geom_point() # AU06
ggplot(year1_pitch7_SOLON, aes(x = AU12_c , y = frame)) + # very few present
  geom_point() # AU12
ggplot(year1_pitch7_SOLON, aes(x = AU25_c , y = frame)) +
  geom_point() # AU25

# AUs judge 1
ggplot(year1_judge1_SOLON, aes(x = AU06_c , y = frame)) +
  geom_point() # AU06
ggplot(year1_judge1_SOLON, aes(x = AU12_c , y = frame)) + 
  geom_point() # AU12
ggplot(year1_judge1_SOLON, aes(x = AU25_c , y = frame)) +
  geom_point() # AU25

# AUs judge 2
ggplot(year1_judge2_SOLON, aes(x = AU06_c , y = frame)) +
  geom_point() # AU06
ggplot(year1_judge2_SOLON, aes(x = AU12_c , y = frame)) + # very few present
  geom_point() # AU12
ggplot(year1_judge2_SOLON, aes(x = AU25_c , y = frame)) +
  geom_point() # AU25

# AUs judge 3
ggplot(year1_judge3_SOLON, aes(x = AU06_c , y = frame)) +
  geom_point() # AU06
ggplot(year1_judge3_SOLON, aes(x = AU12_c , y = frame)) + # very few present
  geom_point() # AU12
ggplot(year1_judge3_SOLON, aes(x = AU25_c , y = frame)) +
  geom_point() # AU25

# There are no outliers detected




# DRPFROMTS---------------------------------------------------------------------

# Change name of all columns in pitch data file to combine them for drpfromts---

year1_pitch7_SOLON <- dplyr::rename(year1_pitch7_SOLON,
                                       frame_p = frame,
                                       timestamp_p = timestamp,
                                       AU06_c_p = AU06_c,
                                       AU12_c_p = AU12_c,
                                       AU25_c_p = AU25_c)


# Join the data set into one for the drpfromts
ts_pitch7_judge1 <- bind_cols(year1_pitch7_SOLON,
                              year1_judge1_SOLON)

ts_pitch7_judge2 <- bind_cols(year1_pitch7_SOLON,
                              year1_judge2_SOLON)

ts_pitch7_judge3 <- bind_cols(year1_pitch7_SOLON,
                              year1_judge3_SOLON)

ts_pitch7_judge1$AU06_c_p <- as.factor(ts_pitch7_judge1$AU06_c_p)
ts_pitch7_judge1$AU12_c_p <- as.factor(ts_pitch7_judge1$AU12_c_p)
ts_pitch7_judge1$AU25_c_p <- as.factor(ts_pitch7_judge1$AU25_c_p)
ts_pitch7_judge1$AU06_c <- as.factor(ts_pitch7_judge1$AU06_c)
ts_pitch7_judge1$AU12_c <- as.factor(ts_pitch7_judge1$AU12_c)
ts_pitch7_judge1$AU25_c <- as.factor(ts_pitch7_judge1$AU25_c)

ts_pitch7_judge2$AU06_c_p <- as.factor(ts_pitch7_judge2$AU06_c_p)
ts_pitch7_judge2$AU12_c_p <- as.factor(ts_pitch7_judge2$AU12_c_p)
ts_pitch7_judge2$AU25_c_p <- as.factor(ts_pitch7_judge2$AU25_c_p)
ts_pitch7_judge2$AU06_c <- as.factor(ts_pitch7_judge2$AU06_c)
ts_pitch7_judge2$AU12_c <- as.factor(ts_pitch7_judge2$AU12_c)
ts_pitch7_judge2$AU25_c <- as.factor(ts_pitch7_judge2$AU25_c)

ts_pitch7_judge3$AU06_c_p <- as.factor(ts_pitch7_judge3$AU06_c_p)
ts_pitch7_judge3$AU12_c_p <- as.factor(ts_pitch7_judge3$AU12_c_p)
ts_pitch7_judge3$AU25_c_p <- as.factor(ts_pitch7_judge3$AU25_c_p)
ts_pitch7_judge3$AU06_c <- as.factor(ts_pitch7_judge3$AU06_c)
ts_pitch7_judge3$AU12_c <- as.factor(ts_pitch7_judge3$AU12_c)
ts_pitch7_judge3$AU25_c <- as.factor(ts_pitch7_judge3$AU25_c)



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
pitch7_drpfromts1 <- drpdfromts(cbind(ts_pitch7_judge1$AU06_c_p,
                                      ts_pitch7_judge1$AU12_c_p,
                                      ts_pitch7_judge1$AU25_c_p),
                                cbind(ts_pitch7_judge1$AU06_c,
                                      ts_pitch7_judge1$AU12_c,
                                      ts_pitch7_judge1$AU25_c),
                                25)


profile <- pitch7_drpfromts1$profile

maxrec_pitch7_judge1 <- round((max(profile) * 100), 2) # Maximum recurrence is 56.45 %

plot(seq(-25, 25, 1), profile, type = "l", lwd = 5, xlab = "Lag", ylab = "Recurrence")




# DCRP & Maximum recurrence rate judge 2
pitch7_drpfromts2 <- drpdfromts(cbind(ts_pitch7_judge2$AU06_c_p,
                                      ts_pitch7_judge2$AU12_c_p,
                                      ts_pitch7_judge2$AU25_c_p),
                                cbind(ts_pitch7_judge2$AU06_c,
                                      ts_pitch7_judge2$AU12_c,
                                      ts_pitch7_judge2$AU25_c),
                                25)

profile <- pitch7_drpfromts2$profile

maxrec_pitch7_judge2 <- round((max(profile) * 100), 2) # Maximum recurrence is 74.30 %

plot(seq(-25, 25, 1), profile, type = "l", lwd = 5, xlab = "Lag", ylab = "Recurrence")




# DCRP & Maximum recurrence rate judge 3
pitch7_drpfromts3 <- drpdfromts(cbind(ts_pitch7_judge3$AU06_c_p,
                                      ts_pitch7_judge3$AU12_c_p,
                                      ts_pitch7_judge3$AU25_c_p),
                                cbind(ts_pitch7_judge3$AU06_c,
                                      ts_pitch7_judge3$AU12_c,
                                      ts_pitch7_judge3$AU25_c),
                                25)

profile <- pitch7_drpfromts3$profile

maxrec_pitch7_judge3 <- round((max(profile) * 100), 2) # Maximum recurrence is 84.00 %

plot(seq(-25, 25, 1), profile, type = "l", lwd = 5, xlab = "Lag", ylab = "Recurrence")



# Reference for drpdfromts code-------------------------------------------------

# Van den Broek, T. (2018). Early development and temporal responsivity: A cross-cultural study from the Netherlands, urban Mozambique, and rural Mozambique (Master's thesis). Retrieved from http://arno.uvt.nl/show.cgi?fid=147170

