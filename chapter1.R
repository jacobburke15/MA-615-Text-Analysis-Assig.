

library(tidyverse)
library(tidytext)
library(knitr)
library(scales)
library(janeaustenr)
library(dplyr)
library(stringr)
Peter.Pan <- read.delim("/Users/heguo/Desktop/615/Peter Pan.txt")
Peter.Pan$book<-rep("Petter Pan",length(Peter.Pan$The.Project.Gutenberg.EBook.of.Peter.Pan..by.James.M..Barrie))
colnames(Peter.Pan)[colnames(Peter.Pan)=="The.Project.Gutenberg.EBook.of.Peter.Pan..by.James.M..Barrie"] <- "text"
Peter.Pan$text<-as.character(Peter.Pan$text)
original_Peter.Pan <- Peter.Pan %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()

## ----tidy_books_raw, dependson = "original_books"------------------------
## library(tidytext)

tidy_Peter.Pan <- original_Peter.Pan %>%
  unnest_tokens(word, text)

## ----tidy_books, dependson = "tidy_books_raw"----------------------------
data(stop_words)

length(stop_words$lexicon)

unique(stop_words$lexicon)


tidy_books <- tidy_books %>%
  anti_join(stop_words)


## ----dependson = "tidy_books"--------------------------------------------
tidy_Peter.Pan %>%
  count(word, sort = TRUE) 

## ----plotcount, dependson = "tidy_books", fig.width=6, fig.height=5, fig.cap="The most common words in Jane Austen's novels"----
## library(ggplot2)

tidy_Peter.Pan %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

## To learn more about gutenbergr, check out the [package's tutorial at rOpenSci](https://ropensci.org/tutorials/gutenbergr_tutorial.html), where it is one of rOpenSci's packages for data access.


tidy_Peter.Pan.1<- Peter.Pan  %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

## ----dependson = "tidy_hgwells"------------------------------------------
tidy_Peter.Pan.1 %>%
  count(word, sort = TRUE)

library(gutenbergr)

hgwells <- gutenberg_download(c(35, 36, 5230, 159))

## ----hgwells, echo = FALSE-----------------------------------------------
load("tidy-text-mining-master/data/hgwells.rda")

## ----tidy_hgwells, dependson = "hgwells"---------------------------------
tidy_hgwells <- hgwells %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

## ----dependson = "tidy_hgwells"------------------------------------------
tidy_hgwells %>%
  count(word, sort = TRUE)

## ----eval = FALSE--------------------------------------------------------

bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))

## ----echo = FALSE--------------------------------------------------------
load("tidy-text-mining-master/data/bronte.rda")

## ----tidy_bronte, dependson = "bronte"-----------------------------------
tidy_bronte <- bronte %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

## ----dependson = "tidy_bronte"-------------------------------------------
tidy_bronte %>%
  count(word, sort = TRUE)




frequency.Peter.Pan <- bind_rows(mutate(tidy_Peter.Pan, author = "James Matthew Barrie"),
                                 mutate(tidy_hgwells, author = "H.G. Wells"), 
                                 mutate(tidy_bronte, author = "Brontë Sisters")) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(author, proportion)%>% 
  gather(author, proportion, `Brontë Sisters`:`H.G. Wells`)


# expect a warning about rows with missing values being removed
ggplot(frequency.Peter.Pan, aes(x = proportion, y = `James Matthew Barrie`, color = abs(`James Matthew Barrie` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "James Matthew Barrie", x = NULL)

## ----cor_test, dependson = "frequency"-----------------------------------
cor.test(data = frequency.Peter.Pan[frequency.Peter.Pan$author == "Brontë Sisters",],
         ~ proportion + `James Matthew Barrie`)
cor.test(data = frequency.Peter.Pan[frequency.Peter.Pan$author == "H.G. Wells",], 
         ~ proportion + `James Matthew Barrie`)


