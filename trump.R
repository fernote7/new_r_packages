## http://varianceexplained.org/r/op-ed-text-analysis/

# setup
library(tidyverse)
library(tidytext)
library(rvest)
theme_set(theme_light())

url <- "https://www.nytimes.com/2018/09/05/opinion/trump-white-house-anonymous-resistance.html"

# tail(-1) removes the first paragraph, which is an editorial header
op_ed <- read_html(url) %>%
  html_nodes(".e2kc3sl0") %>%
  html_text() %>%
  tail(-1) %>%
  data_frame(text = .)
  
  
library(rtweet)

cabinet_accounts <- lists_members(owner_user = "CSPAN", slug = "the-cabinet")
staff <- lists_members(slug = "white-house-staff", owner = "digiphile")

# Find unique screen names from either account
accounts <- unique(c(cabinet_accounts$screen_name, staff$screen_name))

# Download ~3200 from each account
tweets <- map_df(accounts, get_timeline, n = 3200)

# When multiple tweets across accounts are identical (common in government
# accounts), use distinct() to keep only the earliest
reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"

tweet_words <- tweets %>%
  filter(!is_retweet) %>%
  arrange(created_at) %>%
  distinct(text, .keep_all = TRUE) %>%
  select(screen_name, status_id, text) %>%
  mutate(text = str_replace_all(text, "https?://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg) %>%
  filter(str_detect(word, "[a-z]"))
  
  tweet_words %>%
  filter(!word %in% stop_words$word) %>%
  count(word, sort = TRUE) %>%
  head(16) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  coord_flip() +
  labs(y = "# of uses among staff Twitter accounts")
  
  
  # Combine in the op_ed wordswith the name "OP-ED" 
op_ed_words <- op_ed %>%
  unnest_tokens(word, text) %>%
  count(word)

word_counts <- tweet_words %>%
  count(screen_name, word, sort = TRUE) %>%
  bind_rows(op_ed_words %>% mutate(screen_name = "OP-ED"))

# Compute TF-IDF using "word" as term and "screen_name" as document
word_tf_idf <- word_counts %>%
  bind_tf_idf(word, screen_name, n) %>%
  arrange(desc(tf_idf))

word_tf_idf


library(drlib)

selected <- c("realDonaldTrump", "mike_pence", "DeptVetAffairs", "KellyannePolls")

word_tf_idf %>%
  filter(screen_name %in% selected) %>%
  group_by(screen_name) %>%
  top_n(12, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, tf_idf, screen_name)) %>%
  ggplot(aes(word, tf_idf, fill = screen_name)) +
  geom_col(show.legend = FALSE) +
  scale_x_reordered() +
  coord_flip() +
  facet_wrap(~ screen_name, scales = "free_y") +
  labs(x = "",
       y = "TF-IDF vectors of this word for this user",
       title = "TF-IDF: top words for selected staff members")
       
       
library(widyr)

# Find similarities between screen names
# upper = FALSE specifies that we don't want both A-B and B-A matches
word_tf_idf %>%
  pairwise_similarity(screen_name, word, tf_idf, upper = FALSE, sort = TRUE)
  
  
# Look only at the similarity of the op-ed to other documents
op_ed_similarity <- word_tf_idf %>%
  pairwise_similarity(screen_name, word, tf_idf, sort = TRUE) %>%
  filter(item1 == "OP-ED")
  
library(drlib)

op_ed_similarity %>%
  head(12) %>%
  mutate(item2 = reorder(item2, similarity)) %>%
  ggplot(aes(item2, similarity)) +
  geom_col() +
  scale_x_reordered() +
  coord_flip() +
  facet_wrap(~ item1, scales = "free_y") +
  labs(x = "",
       y = "Cosine similarity between TF-IDF vectors",
       subtitle = "Based on 69 selected staff accounts",
       title = "Twitter accounts using words similar to NYTimes op-ed")
       
       
  # This takes a little R judo, but it's worth the effort

# First we normalize the TF-IDF vector for each screen name,
# necessary for cosine similarity
tf_idf <- word_tf_idf %>%
  group_by(screen_name) %>%
  mutate(normalized = tf_idf / sqrt(sum(tf_idf ^ 2))) %>%
  ungroup()

# Then we join the op-ed words with the full corpus, and find
# the product of their TF-IDF with it in other documents
word_combinations <- tf_idf %>%
  filter(screen_name == "OP-ED") %>%
  select(-screen_name) %>%
  inner_join(tf_idf, by = "word", suffix = c("_oped", "_twitter")) %>%
  filter(screen_name != "OP-ED") %>%
  mutate(contribution = normalized_oped * normalized_twitter) %>%
  arrange(desc(contribution)) %>%
  select(screen_name, word, tf_idf_oped, tf_idf_twitter, contribution)
# Get the scores from the six most similar
word_combinations %>%
  filter(screen_name %in% head(op_ed_similarity$item2)) %>%
  mutate(screen_name = reorder(screen_name, -contribution, sum),
         word = reorder_within(word, contribution, screen_name)) %>%
  group_by(screen_name) %>%
  top_n(12, contribution) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, contribution, screen_name)) %>%
  ggplot(aes(word, contribution, fill = screen_name)) +
  geom_col(show.legend = FALSE) +
  scale_x_reordered() +
  facet_wrap(~ screen_name, scales = "free_y") +
  coord_flip() +
  labs(x = "",
       y = "Contribution to similarity score",
       title = "What caused each Twitter account to be similar to the article",
       subtitle = "For the 6 accounts with the highest similarity score")
