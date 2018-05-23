# Set up
library(tidytext)
library(dplyr)
library(stringr)
library(ggplot2)
library(rvest)

# Use the `read_html` function to load this webpage:
# https://www.washington.edu/students/crscat/info.html
page <- read_html("https://www.washington.edu/students/crscat/info.html")

# Extract the text of each course title from the page by using the `html_nodes`
# function to identify the *bold* elements ('b'), then
# Extract the text by passing those element to the `html_text` function
course_titles <- page %>% html_nodes("b") %>% html_text()

# Extract the *descriptions* of each course in the same process as above, searching for
# paragraphs (p)
descriptions <- page %>% html_nodes("p") %>% html_text()

# Create a dataframe by combinding your course titles and descriptions (skip the first description)
classes <- data.frame(title = course_titles, description = descriptions[2:length(descriptions)], stringsAsFactors = FALSE)

# How many courses are in the catalogue?
num_courses <- nrow(classes) # 46

# Create a tidytext sturcture of all words using the `unnest_tokens` function
all_words <- classes %>% unnest_tokens(word, description)

# Which words do we most commonly use to describe informatics courses?
word_count <- all_words %>%
  group_by(word) %>%
  summarize(count = n()) %>%
  arrange(-count)

# Create a set of stop words by adding (more) irrelevant words to the stop_words dataframe
more_stop_words <- data.frame(
  word = c("course", "info", "information"),
  lexicon = "custom"
)

all_stop_words <- rbind(stop_words, more_stop_words)

# Remove stop words by performing an anti_join with the stop_words dataframe
no_stop_words <- word_count %>%
  anti_join(all_stop_words, by = "word")

# Which non stop-words are most common?
non_stop_count <- no_stop_words %>%
  arrange(-count)

# Use ggplot to make a horizontal bar chart of the word frequencies of non-stop words
non_stop_count %>%
  filter(count > 10) %>%
  mutate(word = reorder(word, count)) %>%
  ggplot(aes(word, count)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# For wordcloud -- for fun
library("wordcloud")
library("RColorBrewer")

# TermDocumentMatrix
wordcloud(
  words = non_stop_count$word, freq = non_stop_count$count, min.freq = 1,
  max.words = 200, random.order = FALSE, rot.per = 0.35,
  colors = brewer.pal(8, "Dark2")
)
