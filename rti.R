# Load packages----
library(tidyverse)
library(hms)
library(ggstatsplot)
library(ggpubr)
library(EnvStats)
library(Hmisc)

# Exclusion criteria----
#Days 1 and 2 were removed in the 'exclusion' csv files
#T\Also, there was removal of 4 rats from the entirety of the 'exclusion' csv files
#(d2, j1, k2, l2) because these subjects did not meet the minimum inclusion
#criteria for 2/3 of the experiments (control, grasping, stroking)
#The conclusion of each section has two outputs:
#1. All rat data with no exclusions
#2. Rat data indicating aforementioned exclusions

## Control Inclusion Criteria:
#From restraint habituation days 3-4: At least 1/2 trials or more

## Grasping Inclusion Criteria:
#From experiment days 3-6: At least 10/20 trials or more

## Stroking Inclusion Criteria:
#From experiment days 3-6: At least 10/20 trials or more

# For getting clean control data run below----
#this is control data from days 3 and 4 of restraint habituation where rats
#were not stroked, grasped, or had tail taped down. During the experiment,
#ideally, rats had to face forward in the restrainer for at least 3 total
#minutes. This did not have to be consecutive. A perfect day for a rat
#would consist of a rat having 6 completed 30 second trials.

## Loading raw control data
rti.control.data.uncleaned <- read_csv("data/rti/rti-experiment-control-uncleaned.csv")

## Creating new column for time the trial began AFTER video started recording
#(i.e. row trial.recording.start)
#changes global options so we get 3 digits for the milliseconds
options(digits.secs = 3)
#changing the data frame columns from character into POSIXct
rti.control.data.uncleaned$trial.recording.start <-
  as.POSIXct(rti.control.data.uncleaned$trial.recording.start,
             tz = "EST",
             format = "%m/%d/%Y %H:%M:%OS"
             )
rti.control.data.uncleaned$video.recording.start <-
  as.POSIXct(rti.control.data.uncleaned$video.recording.start,
             tz = "EST",
             format = "%m/%d/%Y %H:%M:%OS"
             )

#confirm that the columns are now POSIXct
sapply(rti.control.data.uncleaned, class)

## Creating a new column with the trial time since start time of actual video recording
#(i.e trial.recording.start minus video.recording.start)
#as_hms changes the numeric output of difftime to %H:%M:%SO
rti.control.data.uncleaned <-
  rti.control.data.uncleaned %>%
  mutate(time.since.video.start =
           as_hms(difftime(trial.recording.start, video.recording.start,
                           units = "secs")
                  )
         )

## Getting rid of the + at the beginning of column trial.recording.duration
rti.control.data.uncleaned$trial.recording.duration <-
  str_sub(rti.control.data.uncleaned$trial.recording.duration,-12,-1)
#as_hms changes the numeric output of trial.recording.duration to %H:%M:%SO
rti.control.data.uncleaned$trial.recording.duration <-
  as_hms(rti.control.data.uncleaned$trial.recording.duration)

## Mutating new subject column from file.name column
rti.control.data.uncleaned$subject <-
  str_sub(rti.control.data.uncleaned$file.name,-6,-5)

## Arranging the order of columns and arranging by video.recording.start
rti.control.data.uncleaned <-
  select(rti.control.data.uncleaned,
         subject,
         sex,
         day,
         video.recording.start,
         trial.recording.start,
         trial.recording.duration,
         time.since.video.start,
         elongation
         ) %>%
  arrange(video.recording.start)

## Mutating new trial.completion column from trial.recording.duration column
rti.control.data.uncleaned <-
  rti.control.data.uncleaned %>%
  mutate(trial.completion =
           trial.recording.duration > 30)

## 1. Save rti.control.long.data.cleaned output
write_csv(rti.control.data.uncleaned, path = "data_output/rti/rti-control-data-cleaned.csv")

## 2. Save rti.control.long.data.cleaned output WITH EXCLUDED RATS
#only keep rows where subject is NOT 'j1' or 'k2' or 'l1' or 'l2'

#rti-experiment-control-cleaned-representive.csv was manually created from
#rti-control-data-cleaned.csv
#Any part of the video must have a minute consecutively elapse without rat
#turning around. Then, take the following 30 seconds of average elongation.
rti.control.data.cleaned.representive <-
  read_csv("data_output/rti/rti-control-data-cleaned-representive.csv")
rti.control.data.cleaned.representive %>%
  filter(subject!='d2' &
           subject!='j1' &
           subject!='k2' &
           subject!='l2'
         ) %>% 
  write_csv(path = "data_output/rti/rti-control-data-cleaned-representive-exclusions.csv")

# For getting clean long data run below---------------------------
## Load data into a data frame
rti.long <- read_csv("data/rti/rti-longdata-uncleaned.csv")

#view head of uncleaned long data
head(rti.long)

#Get rid of subject id in trial column
str_sub(rti.long$trial, -9, -7) <- ""

## Adding column indicating testing order for first six days then next six days
#AB = stroking (days 1-6) then grasping (days 7-12)
#BA = grasping (days 1-6) then stroking (days 7-12)
rti.long <- rti.long %>% 
  mutate(testorder = case_when(
    endsWith(subject, "a1") ~ "AB",
    endsWith(subject, "a2") ~ "BA",
    endsWith(subject, "b1") ~ "BA",
    endsWith(subject, "b2") ~ "AB",
    endsWith(subject, "c1") ~ "BA",
    endsWith(subject, "c2") ~ "BA",
    endsWith(subject, "d1") ~ "AB",
    endsWith(subject, "d2") ~ "AB",
    endsWith(subject, "e1") ~ "BA",
    endsWith(subject, "e2") ~ "AB",
    endsWith(subject, "f1") ~ "BA",
    endsWith(subject, "f2") ~ "AB",
    endsWith(subject, "g1") ~ "AB",
    endsWith(subject, "g2") ~ "BA",
    endsWith(subject, "h1") ~ "AB",
    endsWith(subject, "h2") ~ "AB",
    endsWith(subject, "i1") ~ "BA",
    endsWith(subject, "i2") ~ "BA",
    endsWith(subject, "j1") ~ "AB",
    endsWith(subject, "j2") ~ "BA",
    endsWith(subject, "k1") ~ "BA",
    endsWith(subject, "k2") ~ "AB",
    endsWith(subject, "l1") ~ "AB",
    endsWith(subject, "l2") ~ "BA"
    ))

#Since we have a column indicating testing order, next up, I'm tightening up
#the days so there isn't as much empty space in the data.
#Changing days 7-12 to days 1-6 in trial column
rti.long$trial <-
  str_replace_all(rti.long$trial,
                  c("day7" = "day1", "day8" = "day2", "day9" = "day3",
                    "day10" = "day4", "day11" = "day5", "day12"= "day6"
                    )
                  )

#Changing 7-12 to 1-6 in day column
rti.long$day <-
  str_replace_all(rti.long$day,
                  c("7" = "1", "8" = "2", "9" = "3",
                    "10" = "4", "11" = "5", "12"= "6"
                    )
                  )

#get rid of condition and day information in trial column
str_sub(rti.long$trial, 0, -2) <- ""

#add experiment column specifying which conditions belong to which experiment
rti.long <- rti.long %>% 
  mutate(experiment = case_when(
    endsWith(condition, "Synchronous") ~ "stroking",
    endsWith(condition, "Asynchronous") ~ "stroking",
    endsWith(condition, "Real Tail") ~ "grasping",
    endsWith(condition, "Fake Tail") ~ "grasping"
  ))

#ordering columns for rti.long
rti.long <- rti.long %>%
  select(subject,
         sex,
         experiment,
         condition,
         testorder,
         day,
         trial,
         elongation
         )

## 1. Save long data cleaned output
write_csv(rti.long, path = "data_output/rti/rti-data-long-cleaned.csv")

## 2. Save long data cleaned output WITH EXCLUSIONS
#only keep rows where day is NOT 1 and 2 and
#where subject is NOT 'j1' or 'k2' or 'l2'
rti.long %>%
  filter(day!='1' &
           day!='2' &
           subject!='d2' &
           subject!='j1' &
           subject!='k2' &
           subject!='l2') %>%
  write_csv(path = "data_output/rti/rti-data-long-cleaned-exclusions.csv")


# For getting clean wide data run below----
## Load data into a data frame
rti.wide <- read_csv("data/rti/rti-longdata-uncleaned.csv")

#view head of cleaned long data
head(rti.wide)

#get rid of subject id in trial column
str_sub(rti.wide$trial, -9, -7) <- ""

#changing days 7-12 to days 1-6 in trial column
rti.wide$trial <-
  str_replace_all(rti.wide$trial,
                  c("day7" = "day1", "day8" = "day2", "day9" = "day3",
                    "day10" = "day4", "day11" = "day5", "day12"= "day6"
                    )
                  )

#changing 7-12 to 1-6 in day column
rti.wide$day <- str_replace_all(rti$day, c("7" = "1", "8" = "2", "9" = "3",
                                      "10" = "4", "11" = "5", "12"= "6"))

#adding a column indicating testing order for first six days then next six days
#AB = stroking then grasping
#BA = grasping then stroking
rti.wide <- rti.wide %>% 
  mutate(testorder = case_when(
    endsWith(subject, "a1") ~ "AB",
    endsWith(subject, "a2") ~ "BA",
    endsWith(subject, "b1") ~ "BA",
    endsWith(subject, "b2") ~ "AB",
    endsWith(subject, "c1") ~ "BA",
    endsWith(subject, "c2") ~ "BA",
    endsWith(subject, "d1") ~ "AB",
    endsWith(subject, "d2") ~ "AB",
    endsWith(subject, "e1") ~ "BA",
    endsWith(subject, "e2") ~ "AB",
    endsWith(subject, "f1") ~ "BA",
    endsWith(subject, "f2") ~ "AB",
    endsWith(subject, "g1") ~ "AB",
    endsWith(subject, "g2") ~ "BA",
    endsWith(subject, "h1") ~ "AB",
    endsWith(subject, "h2") ~ "AB",
    endsWith(subject, "i1") ~ "BA",
    endsWith(subject, "i2") ~ "BA",
    endsWith(subject, "j1") ~ "AB",
    endsWith(subject, "j2") ~ "BA",
    endsWith(subject, "k1") ~ "BA",
    endsWith(subject, "k2") ~ "AB",
    endsWith(subject, "l1") ~ "AB",
    endsWith(subject, "l2") ~ "BA"
  ))

#making two data frames that I will join later on
#one data frame (rti) focuses on combining experimental conditions
#another data frame (rti.condition.separate) keeps each condition column separate
rti.condition.separate <- rti.wide %>%
  select(subject, trial, elongation) %>% 
  pivot_wider(names_from = trial,
              values_from = elongation)
rti.condition.separate <- rti.condition.separate[order(rti.condition.separate$subject),]

#changing "synch" and "asynch" to "stroking"
#and changing "fake" and "real" to "grasping"
rti.wide$trial <- str_replace_all(rti.wide$trial,
                             c("asynch" = "stroking", "synch" = "stroking",
                               "real" = "grasping", "fake" = "grasping"))

#remove day and condition column
rti.wide <- rti.wide %>% select(subject, sex, testorder, trial, elongation, -day, -condition)

#change data from long to wide
rti.wide <- rti.wide %>%
  pivot_wider(names_from = trial,
              values_from = elongation)

#subject in alphabetical order
rti.wide <- rti.wide[order(rti.wide$subject),]

#see wide data
head(rti.wide)

#add a column indicating stroking condition
rti.wide <- mutate(rti.wide, stroking = case_when(
  endsWith(subject, "a1") ~ "synchronous",
  endsWith(subject, "a2") ~ "asynchronous",
  endsWith(subject, "b1") ~ "synchronous",
  endsWith(subject, "b2") ~ "synchronous",
  endsWith(subject, "c1") ~ "synchronous",
  endsWith(subject, "c2") ~ "asynchronous",
  endsWith(subject, "d1") ~ "asynchronous",
  endsWith(subject, "d2") ~ "asynchronous",
  endsWith(subject, "e1") ~ "synchronous",
  endsWith(subject, "e2") ~ "asynchronous",
  endsWith(subject, "f1") ~ "asynchronous",
  endsWith(subject, "f2") ~ "synchronous",
  endsWith(subject, "g1") ~ "asynchronous",
  endsWith(subject, "g2") ~ "asynchronous",
  endsWith(subject, "h1") ~ "asynchronous",
  endsWith(subject, "h2") ~ "synchronous",
  endsWith(subject, "i1") ~ "synchronous",
  endsWith(subject, "i2") ~ "synchronous",
  endsWith(subject, "j1") ~ "asynchronous",
  endsWith(subject, "j2") ~ "synchronous",
  endsWith(subject, "k1") ~ "asynchronous",
  endsWith(subject, "k2") ~ "synchronous",
  endsWith(subject, "l1") ~ "synchronous",
  endsWith(subject, "l2") ~ "asynchronous"
))

#add a column indicating grasping condition
rti.wide <- mutate(rti.wide, grasping = case_when(
  endsWith(subject, "a1") ~ "fake",
  endsWith(subject, "a2") ~ "real",
  endsWith(subject, "b1") ~ "real",
  endsWith(subject, "b2") ~ "real",
  endsWith(subject, "c1") ~ "fake",
  endsWith(subject, "c2") ~ "real",
  endsWith(subject, "d1") ~ "fake",
  endsWith(subject, "d2") ~ "real",
  endsWith(subject, "e1") ~ "fake",
  endsWith(subject, "e2") ~ "real",
  endsWith(subject, "f1") ~ "fake",
  endsWith(subject, "f2") ~ "fake",
  endsWith(subject, "g1") ~ "real",
  endsWith(subject, "g2") ~ "real",
  endsWith(subject, "h1") ~ "real",
  endsWith(subject, "h2") ~ "fake",
  endsWith(subject, "i1") ~ "fake",
  endsWith(subject, "i2") ~ "real",
  endsWith(subject, "j1") ~ "real",
  endsWith(subject, "j2") ~ "fake",
  endsWith(subject, "k1") ~ "fake",
  endsWith(subject, "k2") ~ "fake",
  endsWith(subject, "l1") ~ "fake",
  endsWith(subject, "l2") ~ "real"
))

#move new stroking column and new grasping column to after testorder column
rti.wide <- rti.wide %>% relocate(stroking, .after = testorder)
rti.wide <- rti.wide %>% relocate(grasping, .after = stroking)

## Load rti-mass.csv into a data frame
#mass is in grams
rti.mass <- read_csv("data/rti/rti-mass.csv")

#add mass columns to cleaned wide data
rti.wide <- rti.wide %>% inner_join(rti.mass)

#move new mass columns to after testorder column
rti.wide <- rti.wide %>% relocate("3.7.2022.MASS", .after = testorder)
rti.wide <- rti.wide %>% relocate("3.9.2022.MASS", .after = "3.7.2022.MASS")
rti.wide <- rti.wide %>% relocate("3.14.2022.MASS", .after = "3.9.2022.MASS")
rti.wide <- rti.wide %>% relocate("3.16.2022.MASS", .after = "3.14.2022.MASS")
rti.wide <- rti.wide %>% relocate("3.21.2022.MASS", .after = "3.16.2022.MASS")
rti.wide <- rti.wide %>% relocate("3.23.2022.MASS", .after = "3.21.2022.MASS")
rti.wide <- rti.wide %>% relocate("3.25.2022.MASS", .after = "3.23.2022.MASS")

#mutate 6 new columns (avg elongation for each day of stroking)
stroking.day1 <- rti.wide %>%
  select(stroking.day1.trial1, stroking.day1.trial2, stroking.day1.trial3, stroking.day1.trial4, stroking.day1.trial5) 
rti.wide <- mutate(rti.wide, stroking.day1.avg = rowMeans(stroking.day1, na.rm = TRUE))

stroking.day2 <- rti.wide %>%
  select(stroking.day2.trial1, stroking.day2.trial2, stroking.day2.trial3, stroking.day2.trial4, stroking.day2.trial5) 
rti.wide <- mutate(rti.wide, stroking.day2.avg = rowMeans(stroking.day2, na.rm = TRUE))

stroking.day3 <- rti.wide %>%
  select(stroking.day3.trial1, stroking.day3.trial2, stroking.day3.trial3, stroking.day3.trial4, stroking.day3.trial5) 
rti.wide <- mutate(rti.wide, stroking.day3.avg = rowMeans(stroking.day3, na.rm = TRUE))

stroking.day4 <- rti.wide %>%
  select(stroking.day4.trial1, stroking.day4.trial2, stroking.day4.trial3, stroking.day4.trial4, stroking.day4.trial5) 
rti.wide <- mutate(rti.wide, stroking.day4.avg = rowMeans(stroking.day4, na.rm = TRUE))

stroking.day5 <- rti.wide %>%
  select(stroking.day5.trial1, stroking.day5.trial2, stroking.day5.trial3, stroking.day5.trial4, stroking.day5.trial5) 
rti.wide <- mutate(rti.wide, stroking.day5.avg = rowMeans(stroking.day5, na.rm = TRUE))

stroking.day6 <- rti.wide %>%
  select(stroking.day6.trial1, stroking.day6.trial2, stroking.day6.trial3, stroking.day6.trial4, stroking.day6.trial5) 
rti.wide <- mutate(rti.wide, stroking.day6.avg = rowMeans(stroking.day6, na.rm = TRUE))

#mutate 6 new columns (avg elongation for each day of grasping)
grasping.day1 <- rti.wide %>%
  select(grasping.day1.trial1, grasping.day1.trial2, grasping.day1.trial3, grasping.day1.trial4, grasping.day1.trial5) 
rti.wide <- mutate(rti.wide, grasping.day1.avg = rowMeans(grasping.day1, na.rm = TRUE))

grasping.day2 <- rti.wide %>%
  select(grasping.day2.trial1, grasping.day2.trial2, grasping.day2.trial3, grasping.day2.trial4, grasping.day2.trial5) 
rti.wide <- mutate(rti.wide, grasping.day2.avg = rowMeans(grasping.day2, na.rm = TRUE))

grasping.day3 <- rti.wide %>%
  select(grasping.day3.trial1, grasping.day3.trial2, grasping.day3.trial3, grasping.day3.trial4, grasping.day3.trial5) 
rti.wide <- mutate(rti.wide, grasping.day3.avg = rowMeans(grasping.day3, na.rm = TRUE))

grasping.day4 <- rti.wide %>%
  select(grasping.day4.trial1, grasping.day4.trial2, grasping.day4.trial3, grasping.day4.trial4, grasping.day4.trial5) 
rti.wide <- mutate(rti.wide, grasping.day4.avg = rowMeans(grasping.day4, na.rm = TRUE))

grasping.day5 <- rti.wide %>%
  select(grasping.day5.trial1, grasping.day5.trial2, grasping.day5.trial3, grasping.day5.trial4, grasping.day5.trial5) 
rti.wide <- mutate(rti.wide, grasping.day5.avg = rowMeans(grasping.day5, na.rm = TRUE))

grasping.day6 <- rti.wide %>%
  select(grasping.day6.trial1, grasping.day6.trial2, grasping.day6.trial3, grasping.day6.trial4, grasping.day6.trial5) 
rti.wide <- mutate(rti.wide, grasping.day6.avg = rowMeans(grasping.day6, na.rm = TRUE))

#mutate 1 new column (avg elongation for stroking total)
stroking.total <- rti.wide %>%
  select(stroking.day1.avg, stroking.day2.avg, stroking.day3.avg, stroking.day4.avg, stroking.day5.avg, stroking.day6.avg) 
rti.wide <- mutate(rti.wide, stroking.total.avg = rowMeans(stroking.total, na.rm = TRUE))

#mutate 1 new column (avg elongation for grasping total)
grasping.total <- rti.wide %>%
  select(grasping.day1.avg, grasping.day2.avg, grasping.day3.avg, grasping.day4.avg, grasping.day5.avg, grasping.day6.avg) 
rti.wide <- mutate(rti.wide, grasping.total.avg = rowMeans(grasping.total, na.rm = TRUE))

#Creating 28 new columns:
#average synchronous stroking per day (6 columns)
#average asynchronous stroking per day (6 columns)
#average real tail grasping per day (6 columns)
#average fake tail grasping per day (6 columns)
#average synchronous stroking overall (1 column)
#average asynchronous stroking overall (1 column)
#average real tail grasping overall (1 column)
#average fake tail grasping overall (1 column)

#mutate 6 new columns (avg elongation for each day of synchronous stroking)
synch.day1 <- rti.condition.separate %>%
  select(synch.day1.trial1, synch.day1.trial2, synch.day1.trial3, synch.day1.trial4, synch.day1.trial5) 
rti.wide <- mutate(rti.wide, synch.day1.avg = rowMeans(synch.day1, na.rm = TRUE))

synch.day2 <- rti.condition.separate %>%
  select(synch.day2.trial1, synch.day2.trial2, synch.day2.trial3, synch.day2.trial4, synch.day2.trial5) 
rti.wide <- mutate(rti.wide, synch.day2.avg = rowMeans(synch.day2, na.rm = TRUE))

synch.day3 <- rti.condition.separate %>%
  select(synch.day3.trial1, synch.day3.trial2, synch.day3.trial3, synch.day3.trial4, synch.day3.trial5) 
rti.wide <- mutate(rti.wide, synch.day3.avg = rowMeans(synch.day3, na.rm = TRUE))

synch.day4 <- rti.condition.separate %>%
  select(synch.day4.trial1, synch.day4.trial2, synch.day4.trial3, synch.day4.trial4, synch.day4.trial5) 
rti.wide <- mutate(rti.wide, synch.day4.avg = rowMeans(synch.day4, na.rm = TRUE))

synch.day5 <- rti.condition.separate %>%
  select(synch.day5.trial1, synch.day5.trial2, synch.day5.trial3, synch.day5.trial4, synch.day5.trial5) 
rti.wide <- mutate(rti.wide, synch.day5.avg = rowMeans(synch.day5, na.rm = TRUE))

synch.day6 <- rti.condition.separate %>%
  select(synch.day6.trial1, synch.day6.trial2, synch.day6.trial3, synch.day6.trial4, synch.day6.trial5) 
rti.wide <- mutate(rti.wide, synch.day6.avg = rowMeans(synch.day6, na.rm = TRUE))

#mutate 6 new columns (avg elongation for each day of asynchronous stroking)
asynch.day1 <- rti.condition.separate %>%
  select(asynch.day1.trial1, asynch.day1.trial2, asynch.day1.trial3, asynch.day1.trial4, asynch.day1.trial5) 
rti.wide <- mutate(rti.wide, asynch.day1.avg = rowMeans(asynch.day1, na.rm = TRUE))

asynch.day2 <- rti.condition.separate %>%
  select(asynch.day2.trial1, asynch.day2.trial2, asynch.day2.trial3, asynch.day2.trial4, asynch.day2.trial5) 
rti.wide <- mutate(rti.wide, asynch.day2.avg = rowMeans(asynch.day2, na.rm = TRUE))

asynch.day3 <- rti.condition.separate %>%
  select(asynch.day3.trial1, asynch.day3.trial2, asynch.day3.trial3, asynch.day3.trial4, asynch.day3.trial5) 
rti.wide <- mutate(rti.wide, asynch.day3.avg = rowMeans(asynch.day3, na.rm = TRUE))

asynch.day4 <- rti.condition.separate %>%
  select(asynch.day4.trial1, asynch.day4.trial2, asynch.day4.trial3, asynch.day4.trial4, asynch.day4.trial5) 
rti.wide <- mutate(rti.wide, asynch.day4.avg = rowMeans(asynch.day4, na.rm = TRUE))

asynch.day5 <- rti.condition.separate %>%
  select(asynch.day5.trial1, asynch.day5.trial2, asynch.day5.trial3, asynch.day5.trial4, asynch.day5.trial5) 
rti.wide <- mutate(rti.wide, asynch.day5.avg = rowMeans(asynch.day5, na.rm = TRUE))

#
asynch.day6 <- rti.condition.separate %>%
  select(asynch.day6.trial1, asynch.day6.trial2, asynch.day6.trial3, asynch.day6.trial4, asynch.day6.trial5) 
rti.wide <- mutate(rti.wide, asynch.day6.avg = rowMeans(asynch.day6, na.rm = TRUE))

#mutate 6 new columns (avg elongation for each day of real tail grasping)
real.day1 <- rti.condition.separate %>%
  select(real.day1.trial1, real.day1.trial2, real.day1.trial3, real.day1.trial4, real.day1.trial5) 
rti.wide <- mutate(rti.wide, real.day1.avg = rowMeans(real.day1, na.rm = TRUE))

real.day2 <- rti.condition.separate %>%
  select(real.day2.trial1, real.day2.trial2, real.day2.trial3, real.day2.trial4, real.day2.trial5) 
rti.wide <- mutate(rti.wide, real.day2.avg = rowMeans(real.day2, na.rm = TRUE))

real.day3 <- rti.condition.separate %>%
  select(real.day3.trial1, real.day3.trial2, real.day3.trial3, real.day3.trial4, real.day3.trial5) 
rti.wide <- mutate(rti.wide, real.day3.avg = rowMeans(real.day3, na.rm = TRUE))

real.day4 <- rti.condition.separate %>%
  select(real.day4.trial1, real.day4.trial2, real.day4.trial3, real.day4.trial4, real.day4.trial5) 
rti.wide <- mutate(rti.wide, real.day4.avg = rowMeans(real.day4, na.rm = TRUE))

real.day5 <- rti.condition.separate %>%
  select(real.day5.trial1, real.day5.trial2, real.day5.trial3, real.day5.trial4, real.day5.trial5) 
rti.wide <- mutate(rti.wide, real.day5.avg = rowMeans(real.day5, na.rm = TRUE))

real.day6 <- rti.condition.separate %>%
  select(real.day6.trial1, real.day6.trial2, real.day6.trial3, real.day6.trial4, real.day6.trial5) 
rti.wide <- mutate(rti.wide, real.day6.avg = rowMeans(real.day6, na.rm = TRUE))

#mutate 6 new columns (avg elongation for each day of fake tail grasping)
fake.day1 <- rti.condition.separate %>%
  select(fake.day1.trial1, fake.day1.trial2, fake.day1.trial3, fake.day1.trial4, fake.day1.trial5) 
rti.wide <- mutate(rti.wide, fake.day1.avg = rowMeans(fake.day1, na.rm = TRUE))

fake.day2 <- rti.condition.separate %>%
  select(fake.day2.trial1, fake.day2.trial2, fake.day2.trial3, fake.day2.trial4, fake.day2.trial5) 
rti.wide <- mutate(rti.wide, fake.day2.avg = rowMeans(fake.day2, na.rm = TRUE))

fake.day3 <- rti.condition.separate %>%
  select(fake.day3.trial1, fake.day3.trial2, fake.day3.trial3, fake.day3.trial4, fake.day3.trial5) 
rti.wide <- mutate(rti.wide, fake.day3.avg = rowMeans(fake.day3, na.rm = TRUE))

fake.day4 <- rti.condition.separate %>%
  select(fake.day4.trial1, fake.day4.trial2, fake.day4.trial3, fake.day4.trial4, fake.day4.trial5) 
rti.wide <- mutate(rti.wide, fake.day4.avg = rowMeans(fake.day4, na.rm = TRUE))

fake.day5 <- rti.condition.separate %>%
  select(fake.day5.trial1, fake.day5.trial2, fake.day5.trial3, fake.day5.trial4, fake.day5.trial5) 
rti.wide <- mutate(rti.wide, fake.day5.avg = rowMeans(fake.day5, na.rm = TRUE))

fake.day6 <- rti.condition.separate %>%
  select(fake.day6.trial1, fake.day6.trial2, fake.day6.trial3, fake.day6.trial4, fake.day6.trial5) 
rti.wide <- mutate(rti.wide, fake.day6.avg = rowMeans(fake.day6, na.rm = TRUE))

#mutate 1 new column (avg elongation for synchronous total)
synch.total <- rti.wide %>%
  select(synch.day1.avg, synch.day2.avg, synch.day3.avg, synch.day4.avg, synch.day5.avg, synch.day6.avg) 
rti.wide <- mutate(rti.wide, synch.total.avg = rowMeans(synch.total, na.rm = TRUE))

#mutate 1 new column (avg elongation for asynchronous total)
asynch.total <- rti.wide %>%
  select(asynch.day1.avg, asynch.day2.avg, asynch.day3.avg, asynch.day4.avg, asynch.day5.avg, asynch.day6.avg) 
rti.wide <- mutate(rti.wide, asynch.total.avg = rowMeans(asynch.total, na.rm = TRUE))

#mutate 1 new column (avg elongation for fake tail total)
fake.total <- rti.wide %>%
  select(fake.day1.avg, fake.day2.avg, fake.day3.avg, fake.day4.avg, fake.day5.avg, fake.day6.avg) 
rti.wide <- mutate(rti.wide, fake.total.avg = rowMeans(fake.total, na.rm = TRUE))

#mutate 1 new column (avg elongation for real tail total)
real.total <- rti.wide %>%
  select(real.day1.avg, real.day2.avg, real.day3.avg, real.day4.avg, real.day5.avg, real.day6.avg) 
rti.wide <- mutate(rti.wide, real.total.avg = rowMeans(real.total, na.rm = TRUE))

## 1. Save wide data cleaned output
write_csv(rti.wide, path = "data_output/rti/rti-data-wide-cleaned.csv")

## 2. Save wide data cleaned output WITH EXCLUSIONS
#only keep columns where day is NOT 1 and 2 and
#where subject is NOT 'j1' or 'k2' or 'l2'
rti.wide %>%
  filter(subject!='d2' &
           subject!='j1' &
           subject!='k2' &
           subject!='l2') %>%
  select(-contains('day1'),
         -contains('day2')
         ) %>% 
  write_csv(path = "data_output/rti/rti-data-wide-cleaned-exclusions.csv")

# For getting box plots of cleaned exclusions subjects run below----
## Mutating the names of experiments so that they are capitalized
#I'm doing this because of facet grid strip.text.x in ggplot
rti.data.long.cleaned.exclusions <- 
  read_csv("data_output/rti/rti-data-long-cleaned-exclusions.csv")

rti.data.long.cleaned.exclusions <- mutate_if(rti.data.long.cleaned.exclusions, 
                                   is.character,
                                   str_replace_all,
                                   pattern = "grasping",
                                   replacement = "Grasping")
rti.data.long.cleaned.exclusions <- mutate_if(rti.data.long.cleaned.exclusions, 
                                   is.character,
                                   str_replace_all,
                                   pattern = "stroking",
                                   replacement = "Stroking")

#convert day variable to character
rti.data.long.cleaned.exclusions$day <- as.character(rti.data.long.cleaned.exclusions$day)

rti.plot.point.per.trial.exclusions.cleaned <- ggplot(rti.data.long.cleaned.exclusions, aes(x = subject, y = elongation)) +
  geom_violin(alpha = 0) +
  geom_point(aes(color = day),
             shape = 20,
             size = 3,
             alpha = 0.3,
             position = position_jitterdodge(jitter.width=0)) +
  facet_grid(~experiment + condition,
             scales = "free", space = "free") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, color="#9C1F2E", fill="#9C1F2E") +
  stat_summary(fun.data = mean_cl_normal,  
               geom = "errorbar", width = 0.5, color = "#9C1F2E") +
  theme_bw(base_size = 10) +
  theme(text = element_text(family="arial"),
        axis.text.x = element_text(face="bold", 
                                   size=11),
        axis.text.y = element_text(face="bold", 
                                   size=11),
        panel.spacing = unit(.05, "lines"),
        panel.border = element_rect(color = "#0D0D0D", fill = NA, size = .05),
        strip.text = element_text(face = "bold", size = 25, colour = "#9C1F2E"),
        strip.text.x = element_text(size = 15),
        strip.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(face = "bold", size = 11),
        legend.title = element_blank(),
        plot.title = element_text(lineheight = 0.9),
        axis.title.y = element_text(face = "bold", margin = margin(r = 15))) +
  guides(colour = guide_legend(override.aes = list(size=10))) +
  stat_n_text() + 
  labs(x = "Condition", y = "Body Elongation (%)") +
  scale_y_continuous(breaks = seq(55,95,5)) +
  stat_mean_sd_text(vjust = 0.7)

ggsave("rti.plot.point.per.trial.exclusions.cleaned.png",
       plot = last_plot(),
       width = 36.5,
       height = 8.3,
       path = "graph_output/rti")

# For getting visualizations run below----
## Mutating the names of experiments so that they are capitalized
#I'm doing this because of facet grid strip.text.x in ggplot
rti.data.long.cleaned <- read_csv("data_output/rti/rti-data-long-cleaned.csv")

rti.data.long.cleaned <- mutate_if(rti.data.long.cleaned, 
            is.character,
            str_replace_all,
            pattern = "grasping",
            replacement = "Grasping")
rti.data.long.cleaned <- mutate_if(rti.data.long.cleaned, 
            is.character,
            str_replace_all,
            pattern = "stroking",
            replacement = "Stroking")

## ggplot code where each point is a trial
#ggplot colors
#black hex: #0D0D0D
#grey hex: #7F7F7F
#scarlet hex: #9C1F2E
rti.plot.point.per.trial <- ggplot(rti.data.long.cleaned, aes(x = condition, y = elongation)) +
  geom_point(aes(fill = sex, color = sex), shape = 20,
             alpha = 0.3,
             position = position_jitterdodge()) +
  geom_violin(alpha = 0) +
  facet_grid(~experiment,
             scales = "free", space = "free") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, color="#9C1F2E", fill="#9C1F2E") +
  stat_summary(fun.data = mean_cl_normal,  
               geom = "errorbar", width = 0.5, color = "#9C1F2E") +
  theme_bw(base_size = 10) +
  theme(text = element_text(family="arial"),
        axis.text.x = element_text(face="bold", 
                                   size=11),
        axis.text.y = element_text(face="bold", 
                                   size=11),
        panel.spacing = unit(.05, "lines"),
        panel.border = element_rect(color = "#0D0D0D", fill = NA, size = .05),
        strip.text = element_text(face = "bold", size = 25, colour = "#9C1F2E"),
        strip.text.x = element_text(size = 15),
        strip.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(face = "bold", size = 11),
        legend.title = element_blank(),
        plot.title = element_text(lineheight = 0.9),
        axis.title.y = element_text(face = "bold", margin = margin(r = 15))) +
  guides(colour = guide_legend(override.aes = list(size=10))) +
  stat_n_text() + 
  labs(x = "Condition", y = "Body Elongation (%)") +
  scale_y_continuous(breaks = seq(55,95,5)) +
  stat_mean_sd_text(vjust = 0.7) +
  geom_signif(comparisons = list(c("Asynchronous", "Synchronous")), annotations = c("*"),
              y_position = 83, tip_length = 0.02, vjust = 0.4,
              map_signif_level = TRUE, textsize = 9, family="arial")

---
  
## ggplot code where each point is one rat
# ggplot colors
#black hex: #0D0D0D
#grey hex: #7F7F7F
#scarlet hex: #9C1F2E
#I have to make a new rti.long.data.per.experiment so that has each rat has one
#data point per experiment. NOTE: NO DATA ARE EXCLUDED
rti.wide <- read_csv("data_output/rti/rti-data-wide-cleaned.csv")

rti.total.per.experiment.wide <- rti.wide %>% 
  select(subject,
         sex,
         testorder,
         stroking,
         grasping,
         asynch.total.avg,
         synch.total.avg,
         fake.total.avg,
         real.total.avg
         )

#create a long format table of rti.total.per.experiment that groups by condition
rti.total.per.experiment.long <- pivot_longer(rti.total.per.experiment.wide,
                                              cols = "asynch.total.avg":"real.total.avg",
                                              names_to = "condition",
                                values_to = "elongation", values_drop_na = TRUE)

#to retain the facet grid, I added another column that indicates experiment
rti.total.per.experiment.long <- rti.total.per.experiment.long %>% 
  mutate(experiment = case_when(
  endsWith(condition, "asynch.total.avg") ~ "Stroking",
  endsWith(condition, "synch.total.avg") ~ "Stroking",
  endsWith(condition, "fake.total.avg") ~ "Grasping",
  endsWith(condition, "real.total.avg") ~ "Grasping"
  ))

#changing names (e.g. synch.total.avg to Synchronous) in condition column
rti.total.per.experiment.long$condition <- str_replace_all(
  rti.total.per.experiment.long$condition, c(
    "asynch.total.avg" = "Asynchronous",
    "synch.total.avg" = "Synchronous",
    "fake.total.avg" = "Fake Tail",
    "real.total.avg" = "Real Tail"
    ))

#ggplot point per rat
rti.plot.point.per.rat <- ggplot(rti.total.per.experiment.long, aes(x = condition, y = elongation)) +
  geom_point(aes(fill = sex, color = sex), shape = 20, size = 3,
             alpha = 0.3,
             position = position_jitterdodge()) +
  geom_violin(alpha = 0) +
  facet_grid(~experiment,
             scales = "free", space = "free") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, color="#9C1F2E", fill="#9C1F2E") +
  stat_summary(fun.data = mean_cl_normal,  
               geom = "errorbar", width = 0.5, color = "#9C1F2E") +
  theme_bw(base_size = 10) +
  theme(text = element_text(family="arial"),
        axis.text.x = element_text(face="bold", 
                                   size=11),
        axis.text.y = element_text(face="bold", 
                                   size=11),
        panel.spacing = unit(.05, "lines"),
        panel.border = element_rect(color = "#0D0D0D", fill = NA, size = .05),
        strip.text = element_text(face = "bold", size = 25, colour = "#9C1F2E"),
        strip.text.x = element_text(size = 15),
        strip.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(face = "bold", size = 11),
        legend.title = element_blank(),
        plot.title = element_text(lineheight = 0.9),
        axis.title.y = element_text(face = "bold", margin = margin(r = 15))) +
  guides(colour = guide_legend(override.aes = list(size=10))) +
  stat_n_text() + 
  labs(x = "Condition", y = "Body Elongation (%)") +
  scale_y_continuous(breaks = seq(50,95,5)) +
  stat_mean_sd_text(vjust = 0.7)


## Testing Order: rti.total.per.experiment split by testing order
split.testorder <- group_split(rti.total.per.experiment.long, rti.total.per.experiment.long$testorder)

### AB test order (stroking then grasping)
ggplot(split.testorder[[1]], aes(x = condition, y = elongation)) +
  geom_point(aes(fill = sex, color = sex), shape = 20, size = 3,
             alpha = 0.3,
             position = position_jitterdodge()) +
  geom_violin(alpha = 0) +
  facet_grid(~experiment,
             scales = "free", space = "free") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, color="#9C1F2E", fill="#9C1F2E") +
  stat_summary(fun.data = mean_cl_normal,  
               geom = "errorbar", width = 0.5, color = "#9C1F2E") +
  theme_bw(base_size = 10) +
  theme(text = element_text(family="arial"),
        axis.text.x = element_text(face="bold", 
                                   size=11),
        axis.text.y = element_text(face="bold", 
                                   size=11),
        panel.spacing = unit(.05, "lines"),
        panel.border = element_rect(color = "#0D0D0D", fill = NA, size = .05),
        strip.text = element_text(face = "bold", size = 25, colour = "#9C1F2E"),
        strip.text.x = element_text(size = 15),
        strip.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(face = "bold", size = 11),
        legend.title = element_blank(),
        plot.title = element_text(lineheight = 0.9),
        axis.title.y = element_text(face = "bold", margin = margin(r = 15))) +
  guides(colour = guide_legend(override.aes = list(size=10))) +
  stat_n_text() + 
  labs(x = "Condition", y = "Body Elongation (%)") +
  scale_y_continuous(breaks = seq(50,95,5)) +
  stat_mean_sd_text(vjust = 0.7)

### BA test order (grasping then stroking)
ggplot(split.testorder[[2]], aes(x = condition, y = elongation)) +
  geom_point(aes(fill = sex, color = sex), shape = 20, size = 3,
             alpha = 0.3,
             position = position_jitterdodge()) +
  geom_violin(alpha = 0) +
  facet_grid(~experiment,
             scales = "free", space = "free") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 4, color="#9C1F2E", fill="#9C1F2E") +
  stat_summary(fun.data = mean_cl_normal,  
               geom = "errorbar", width = 0.5, color = "#9C1F2E") +
  theme_bw(base_size = 10) +
  theme(text = element_text(family="arial"),
        axis.text.x = element_text(face="bold", 
                                   size=11),
        axis.text.y = element_text(face="bold", 
                                   size=11),
        panel.spacing = unit(.05, "lines"),
        panel.border = element_rect(color = "#0D0D0D", fill = NA, size = .05),
        strip.text = element_text(face = "bold", size = 25, colour = "#9C1F2E"),
        strip.text.x = element_text(size = 15),
        strip.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(face = "bold", size = 11),
        legend.title = element_blank(),
        plot.title = element_text(lineheight = 0.9),
        axis.title.y = element_text(face = "bold", margin = margin(r = 15))) +
  guides(colour = guide_legend(override.aes = list(size=10))) +
  stat_n_text() + 
  labs(x = "Condition", y = "Body Elongation (%)") +
  scale_y_continuous(breaks = seq(50,95,5)) +
  stat_mean_sd_text(vjust = 0.7)

# For getting statistical analyses run below----
library(jmv)
rti.wide.data.exclusions <- read_csv("data_output/rti/rti-data-wide-cleaned-exclusions.csv")

## Stroking by day repeated measures ANOVA
stroking.results <- anovaRM(
  data = rti.wide.data.exclusions,
  rm = list(
    list(
      label="Day",
      levels=c("1", "2", "3", "4"))),
  rmCells = list(
    list(
      measure="stroking.day3.avg",
      cell="1"),
    list(
      measure="stroking.day4.avg",
      cell="2"),
    list(
      measure="stroking.day5.avg",
      cell="3"),
    list(
      measure="stroking.day6.avg",
      cell="4")),
  bs = stroking,
  rmTerms = ~ Day,
  bsTerms = ~ stroking,
  spherTests = TRUE,
  spherCorr = c("none", "GG"),
  leveneTest = TRUE,
  postHoc = list(
    "Day",
    "stroking"),
  groupSumm = TRUE)

### save stroking results as a .txt file
capture.output(stroking.results, file = "analysis/rti/stroking.results.txt", append = TRUE)

## Grasping by day repeated measures ANOVA
grasping.results <- anovaRM(
  data = rti.wide.data.exclusions,
  rm = list(
    list(
      label="Day",
      levels=c("1", "2", "3", "4"))),
  rmCells = list(
    list(
      measure="grasping.day3.avg",
      cell="3"),
    list(
      measure="grasping.day4.avg",
      cell="4"),
    list(
      measure="grasping.day5.avg",
      cell="5"),
    list(
      measure="grasping.day6.avg",
      cell="6")),
  bs = grasping,
  rmTerms = ~ Day,
  bsTerms = ~ grasping,
  spherTests = TRUE,
  spherCorr = c("none", "GG"),
  leveneTest = TRUE,
  groupSumm = TRUE)

### save grasping results as a .txt file
capture.output(grasping.results, file = "analysis/rti/grasping.results.txt", append = TRUE)

## Control by sex independent samples T-test
rti.control.data.cleaned.representive.exclusions <- read_csv("data_output/rti/rti-control-data-cleaned-representive-exclusions.csv")

control.results <- ttestIS(
  formula = elongation ~ sex,
  data = rti.control.data.cleaned.representive.exclusions,
  vars = elongation,
  welchs = TRUE,
  norm = TRUE,
  eqv = TRUE,
  meanDiff = TRUE,
  ci = TRUE,
  effectSize = TRUE,
  desc = TRUE,
  plots = TRUE)

### save control results as a .txt file
capture.output(control.results, file = "analysis/rti/control.results.txt", append = TRUE)

## Multivariate linear mixed effects model
library(lme4)

rti.data.long.cleaned.exclusions <-
  read_csv("data_output/rti/rti-data-long-cleaned-exclusions.csv")

### Stroking multivariate linear mixed effects model
lmer.stroking.results <- summary(lmer(elongation~testorder*sex + (1|subject) + (1|day),
             data = filter(rti.data.long.cleaned.exclusions, experiment == 'stroking')))

#### Save stroking lmer results as a .txt file
capture.output(lmer.stroking.results, file = "analysis/rti/lmer.stroking.results.txt", append = TRUE)

### Grasping multivariate linear mixed effects model
lmer.grasping.results <- summary(lmer(elongation~testorder*sex + (1|subject) + (1|day),
                             data = filter(rti.data.long.cleaned.exclusions, experiment == 'grasping')))

#### Save grasping lmer results as a .txt file
capture.output(lmer.grasping.results, file = "analysis/rti/lmer.grasping.results.txt", append = TRUE)

### Control multivariate linear mixed effects model
lmer.control.results <- summary(lmer(elongation~sex + (1|subject) + (1|day),
                                      data = rti.control.data.cleaned.representive.exclusions))

#### Save control lmer results as a .txt file
capture.output(lmer.control.results, file = "analysis/rti/lmer.control.results.txt", append = TRUE)
