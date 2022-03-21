# DISCLAIMER: I'm doing a lot of copying
# of Rex Deng's submission for this assignment

### Tidyverse
# Loading libraries
library(fivethirtyeight)
library(tidyverse)

# URL to the data that you've used.
# url <- 'https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv'
# I'm too lazy to bother with the `connection buffer size` issue, so we're doing this directly

# Creating the `polls` and `Endorsements` objects. Not sure why we want a capital "E", but alright
polls <- read.csv("president_primary_polls_feb2020.csv")
Endorsements <- endorsements_2020 # from the fiverthirtyeight package

# Changing the `endorsee` variable to `candidate_name` in `Endorsements`
Endorsements <- Endorsements %>% 
  rename(candidate_name = endorsee)

# Making `Endorsements` a tibble
Endorsements <- as_tibble(Endorsements)

# Creating our pool of candidates, and then filtering `polls` to only include them.
# We're also subsetting the data to just five variables:
# `candidate_name`, `sample_size`, `start_date`, `party`, and `pct`

candidates <- c("Amy Klobuchar", "Bernard Sanders", "Elizabeth Warren", "Joseph R. Biden Jr.",
                "Michael Bloomberg", "Pete Buttigieg")

polls <- polls %>% 
  filter(candidate_name %in% candidates) %>% 
  dplyr::select(candidate_name, sample_size, start_date, party, pct)

# Making sure the names match up across data sets -- this means changing
# "Joe Biden" to "Joeseph R. Biden Jr." and "Bernie Sanders" to "Bernard Sanders"

Endorsements <- Endorsements %>% 
  mutate(candidate_name = ifelse(candidate_name == "Joe Biden", "Joseph R. Biden Jr.",
                                 ifelse(candidate_name == "Bernie Sanders", "Bernard Sanders",
                                        candidate_name)))

# And making sure we've captured every candidate
intersect(unique(polls$candidate_name),  unique(Endorsements$candidate_name))
# That seems to work, although it looks like Bloomberg is not in `Endorsements`

# Joining the datasets by `candidate_name`
polls_endorse <- left_join(polls, Endorsements,
                           by = "candidate_name")

# Counting the number of endorsements
# We're pulling from the initial `Endorsements` because the joined data has duplicates
endorse_count <- Endorsements %>% 
  filter(candidate_name %in% candidates) %>% 
  group_by(candidate_name) %>% 
  summarise(count_endorsements = sum(!is.na(endorser)))

# Plotting: we're condensing all of these into a single step
p <- ggplot(data = endorse_count,
            aes(x = candidate_name,
                y = count_endorsements))+
  geom_bar(stat = "identity")+
  labs(title = "Count of Endorsements of Democratic Candidates",
       x = "Candidate",
       y = "Count of Endorsements")+
  theme_dark() # As a theme_minimal() purist, this pains me
ggsave("PS4_endorsement_counts.png",
       plot = p)

### Text-as-Data with Tidyverse
# Clearing the environment, since we aren't reusing anything
# from part 1
rm(list = ls())

# Libraries
library(tidyverse)
library(tm)
library(lubridate)
library(wordcloud)

# Getting our data
trump_tweets_url <- 'https://politicaldatascience.com/PDS/Datasets/trump_tweets.csv'
tweets <- read_csv(trump_tweets_url)

# Separating `created_at` where date and times are separate columns
tweets$date <- sapply(strsplit(tweets$created_at, " "), `[[`, 1)
tweets$date <- as.Date(tweets$date,
                       format = "%m/%d/%y")

tweets$time <- sapply(strsplit(tweets$created_at, " "), `[[`, 2)

# Reporting the range
range(tweets$date)

# Removing retweets and displaying Trump's `top 5` most popular arnd retweeted tweets.
topfive <- tweets %>% 
  filter(is_retweet == FALSE) %>% 
  slice_max(retweet_count, n = 5)

# Creating the `corpus`
# Of all the things I copied from Rex, this is this is easily the most-copied
corpus <- VCorpus(VectorSource(tweets$text))
writeLines(head(strwrap(corpus[[1]]), 10)) # Checking that we pulled the content correctly

# Removing whitespace, numbers, and other text cleaning
# `addspace` finds whatever pattern we want and replaces it with a space
addspace <- content_transformer(function(x, pattern){
  return(gsub(pattern, " ", x))
})

# For instance, changing `-` to whitespace
corpus <- tm_map(corpus, addspace, "-")

# Removing patterns -- basically the opposite of `addspace()`
removepattern <- content_transformer(function(x, pattern){
  return(gsub(pattern, "", x))
})

# using it to remove URLs
corpus <- tm_map(corpus, removepattern, "?(f|ht)(tp)(s?)(://)(.*)(.|/])(.*)")

# and to remove the other stuff
corpus <- tm_map(corpus, removepattern, "'")
corpus <- tm_map(corpus, removepattern, "'")
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))

# changing the case
corpus <- tm_map(corpus, content_transformer(tolower))

# And checking, again, that it worked
writeLines(head(strwrap(corpus[[1]]), 10)) # lol this is such a mess of a tweet how did this guy become president (i know it's a retweet but still)

# Making the `wordcloud`, where words have a minimum of 3 appearances
pal = brewer.pal(9, "BuGn")
wc <- wordcloud(corpus, min.freq = 3, random.order = TRUE, random.color = TRUE, max.words = 50, colors = pal)

# Making our DTM with `control = list(weighting = weighTfIdf)`
library(tidytext) # we need this for tidy()
DTM <- DocumentTermMatrix(corpus,
                          control = list(weighting = weightTfIdf))
dat <- tidy(DTM)

# Finally, getting our top 50 words with the highest tf.idf scores, and a lfb of 0,8
dat_top50 <- dat %>% 
  slice_max(count, n = 50)
head(dat_top50)
