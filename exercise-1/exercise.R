# Exercise-1
# Developed from: http://tidytextmining.com/

# Set up (install packages that you don't have)
library(janeaustenr)
library(tidytext)
library(dplyr)
library(stringr)
library(ggplot2)

# Load booksinto a dataframe using the austen_books() function
original_books <- austen_books() 

# How many books are in the dataset?
num_books <- length(unique(original_books$book))

# Which book has the most lines?
book_lines <- original_books %>% 
                group_by(book) %>% 
                summarize(lines = n())

# Use the unnest_tokens function to generate the full list of words
all_words <- original_books %>%
  unnest_tokens(word, text)

# Which words are most common (regardless of which book them come from)?
word_count <- all_words %>% 
                group_by(word) %>% 
                summarize(count = n()) %>% 
                arrange(-count)

# Remove stop words by performing an anti_join with the stop_words dataframe
no_stop_words <- all_words %>% 
                  anti_join(stop_words, by="word")

# Which non stop-words are most common?
non_stop_count <- no_stop_words %>%
                    group_by(word) %>% 
                    summarize(count = n()) %>% 
                    arrange(-count)

# Use ggplot to make a horizontal bar chart of the word frequencies of non-stop words
no_stop_words %>% 
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
