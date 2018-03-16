#
# Find the stop_words in Bulgaria

# All stop words https://github.com/stopwords-iso/stopwords-iso
# Bulgarian https://github.com/stopwords-iso/stopwords-bg

# Use this:
# All https://github.com/stopwords-iso
# Bulgarian stop words https://github.com/stopwords-iso/stopwords-bg/blob/master/stopwords-bg.txt
# https://www.rdocumentation.org/packages/stopwords/versions/0.9.0
# Download them from github
#install.packages("devtools")
#devtools::install_github("davnn/stopwords")
bg_stop_words<- stopwords::stopwords("bg", source = "stopwords-iso")

# Some of the letters are unrecognizable. Keep only letters that seem Bulgarian.
filtered_bg_s_w <- tail(bg_stop_words, 259) 
# the size of the Bulgarian stop-words is 259 (half the size of the previous list). 

library(data.table)
filtered_bg_s_w <- data.table(filtered_bg_s_w)

# Another filter is necessary since words like "bad", "thank you" and "like" are among the 
# current BG stop words list.


### We need to remove words with indexes:

## c(17, 33, 60, 64, 66, 67, 77, 87, 89, 94, 106, 124, 125,
# 135, 142, 143, 165, 166, 167, 168, 200, 201, 209, 214, 247, 248, 257, 259)

# or equivalently,

## c("thank you", "time", "day", "good{3 forms}", "cheap", "life", "slowing down", "asleep", "said",
# "easy{2 forms}, "month", "wet", "please/excuse me", "new{3 forms}", "news", "equal{2 forms}",
# "son/blue", "laughter", "tomorrow", "like", "fist", "cool")



bgn_stop_words <- filtered_bg_s_w[-c(17, 33, 60, 64, 66, 67, 77, 87, 89, 94, 106, 124, 125, 135,
                           142, 143, 165, 166, 167, 168, 200, 201, 209, 214, 247, 248, 257, 259)]

# change the default encoding in R
options(encoding = "utf-8")

## add stop words such as "бих", "бъда", ...

add_stop_words <- data.table(filtered_bg_s_w = c("бих", "бъда", "бил", "бихме"))
bgn_s_w <- rbind(bgn_stop_words, add_stop_words)

setnames(bgn_s_w,"filtered_bg_s_w","word")












