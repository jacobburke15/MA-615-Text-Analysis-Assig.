
## chapter 3 (Peter Pan)


library(knitr)
library(tidyverse)
library(tidytext)
library(dplyr)
library(gutenbergr)


## downloading our book of choice, "Peter Pan", with Gutenberg_download ID of 16

book_words <- gutenberg_download(16) %>%
  unnest_tokens(word, text) %>%
  count(word, sort = TRUE) %>%
  ungroup()

## since we only have one book, our total words is just one value (51741)

total_words <- book_words %>% 
  summarize(total = sum(n))

total_words

## Therefore, to graph the proportions of book words to it's total book words, we'll input 
## one extra column into book_words 

book_words <- book_words %>% mutate(total = 47707)

## Now we can plot the proportions of words found below

ggplot(book_words, aes(n/total)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009)


## Zipf's law states that the frequency that a word appears is inversely proportional to its rank.

## Generating a rank for each word in the Peter Pan book, along with it's normalized term frequency

freq_by_rank <- book_words %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total)

freq_by_rank

## Here we can now plot the ranks of the words based on their term frequencies 
freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = "red")) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

## here we can see that term frequency and the rank of a word has an inverse relationship, ie. following 
## zipf's law stating that the frequency that a word appears is inversely proportional to its rank


## getting a subset of the ranked words, between rank = 10, and rank = 500

rank_subset <- freq_by_rank %>% 
  filter(rank < 500,
         rank > 10)

## log linear regression using rank as a predictor for term frequency outcome 

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)

## again, looking at the log10(rank) coefficient, this further justifies Zipf's law, as we see that 
## as log10(rank) increases, our log10(term frequency) will drop - Ie. the inverse relationship. 

## adding our regression line to our plot: 

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = "red")) + 
  geom_abline(intercept = -0.589, slope = -1.127, color = "gray20", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

## here we can see the regression line added as a dashed line


## Now, we are going to use the bind_tf_idf function 

## have to mutate this column in to abide by the arguments the bind_tf_idf function passes through 

book_words <- book_words %>% mutate(book = "Peter Pan")

## applying function 

book_words <- book_words %>%
  bind_tf_idf(word, book, n)
book_words

book_words

## problem here

## from here you can see that tf is ranked of values, however idf and tf_idf is zero in this case, as 
## our document value is just 1, given that we are only working with one book currently

## ranking in terms of 'tf'

book_words %>%
  select(-total) %>%
  arrange(desc(tf))

## Some of the values for idf are the same for different terms because there are 6 documents in this corpus and we are seeing the numerical value for $\ln(6/1)$, $\ln(6/2)$, etc.

## ----plotseparate, dependson = "plot_austen", fig.height=10, fig.width=9, fig.cap="Highest tf-idf words in each of Jane Austen's Novels"----
book_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(book) %>% 
  top_n(15) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") +
  coord_flip()

## ----eval = FALSE--------------------------------------------------------
# library(gutenbergr)
#  physics <- gutenberg_download(c(37729, 14725, 13476, 5001),
#                                meta_fields = "author")

## ----physics, echo = FALSE-----------------------------------------------
load("tidy-text-mining-master/data/physics.rda")

## ----physics_words, dependson = "physics"--------------------------------
physics_words <- physics %>%
  unnest_tokens(word, text) %>%
  count(author, word, sort = TRUE) %>%
  ungroup()

physics_words

## ----physicsseparate, dependson = "plot_physics", fig.height=7, fig.width=8, fig.cap="Highest tf-idf words in each physics texts"----
plot_physics <- physics_words %>%
  bind_tf_idf(word, author, n) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  mutate(author = factor(author, levels = c("Galilei, Galileo",
                                            "Huygens, Christiaan", 
                                            "Tesla, Nikola",
                                            "Einstein, Albert")))

plot_physics %>% 
  group_by(author) %>% 
  top_n(15, tf_idf) %>% 
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = author)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~author, ncol = 2, scales = "free") +
  coord_flip()

## ----dependson = "physics"-----------------------------------------------
library(stringr)

physics %>% 
  filter(str_detect(text, "eq\\.")) %>% 
  select(text)

## ----dependson = "physics"-----------------------------------------------
physics %>% 
  filter(str_detect(text, "K1")) %>% 
  select(text)

## ----dependson = "physics"-----------------------------------------------
physics %>% 
  filter(str_detect(text, "AK")) %>% 
  select(text)

## ----mystopwords, dependson = "plot_physics", fig.height=7, fig.width=8, fig.cap="Highest tf-idf words in classic physics texts"----
mystopwords <- data_frame(word = c("eq", "co", "rc", "ac", "ak", "bn", 
                                   "fig", "file", "cg", "cb", "cm"))
physics_words <- anti_join(physics_words, mystopwords, by = "word")
plot_physics <- physics_words %>%
  bind_tf_idf(word, author, n) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(author) %>% 
  top_n(15, tf_idf) %>%
  ungroup %>%
  mutate(author = factor(author, levels = c("Galilei, Galileo",
                                            "Huygens, Christiaan",
                                            "Tesla, Nikola",
                                            "Einstein, Albert")))

ggplot(plot_physics, aes(word, tf_idf, fill = author)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~author, ncol = 2, scales = "free") +
  coord_flip()

