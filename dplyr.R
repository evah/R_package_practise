
# reference http://genomicsclass.github.io/book/pages/dplyr_tutorial.html

library(dplyr)
library(downloader)

url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/msleep_ggplot2.csv"
filename <- "msleep_ggplot2.csv"
if (!file.exists(filename)) download(url,filename)
msleep <- read.csv("msleep_ggplot2.csv")
head(msleep)

# Important dplyr verbs to remember
# dplyr verbs	Description
# select()	select columns
# filter()	filter rows
# arrange()	re-order or arrange rows
# mutate()	create new columns
# summarise()	summarise values
# group_by()	allows for group operations in the “split-apply-combine” concept

sleepData <- select(msleep, name, sleep_total)
head(sleepData)

head(select(msleep, -name))

#select range of column
head(select(msleep, name:order))

head(select(msleep, starts_with("sl")))

# ends_with() = Select columns that end with a character string
# contains() = Select columns that contain a character string
# matches() = Select columns that match a regular expression
# one_of() = Select columns names that are from a group of names

filter(msleep, sleep_total >= 16)
filter(msleep, sleep_total >= 16, bodywt >= 1)
filter(msleep, order %in% c("Perissodactyla", "Primates"))

#Pipe operator: %>%
# Before we go any futher, let’s introduce the pipe operator: %>%. 
# dplyr imports this operator from another package (magrittr). 
# This operator allows you to pipe the output from one function to the input of another function.
# Instead of nesting functions (reading from the inside to the outside), 
# the idea of of piping is to read the functions from left to right.

head(select(msleep, name, sleep_total))

msleep %>% 
  select(name, sleep_total) %>% 
  head

msleep %>% arrange(order) %>% head

msleep %>% 
  select(name, order, sleep_total) %>%
  arrange(order, sleep_total) %>% 
  head

msleep %>% 
  select(name, order, sleep_total) %>%
  arrange(order, sleep_total) %>% 
  filter(sleep_total >= 16)

msleep %>% 
  select(name, order, sleep_total) %>%
  arrange(order, desc(sleep_total)) %>% 
  filter(sleep_total >= 16)

# Create new columns using mutate()
# The mutate() function will add new columns to the data frame. 
# Create a new column called rem_proportion which is the ratio of rem sleep to total amount of sleep.

msleep %>% 
  mutate(rem_proportion = sleep_rem / sleep_total) %>%
  head

msleep %>% 
  mutate(rem_proportion = sleep_rem / sleep_total, 
         bodywt_grams = bodywt * 1000) %>%
  head

# The summarise() function will create summary statistics for a given column in the data frame such as finding the mean. 
# For example, to compute the average number of hours of sleep, apply the mean() function to the column sleep_total and call the summary value avg_sleep.
msleep %>% 
  summarise(avg_sleep = mean(sleep_total))

msleep %>% 
  summarise(avg_sleep = mean(sleep_total), 
            min_sleep = min(sleep_total),
            max_sleep = max(sleep_total),
            total = n())

msleep %>% 
  group_by(order) %>%
  summarise(avg_sleep = mean(sleep_total), 
            min_sleep = min(sleep_total), 
            max_sleep = max(sleep_total),
            total = n())


