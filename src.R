library(tidygraph)
library(ggraph)
library(tidyverse)
library(magrittr)
library(tidytext)
library(widyr)

netflix_titles <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-20/netflix_titles.csv')

sample_netflix_titles <- netflix_titles %>%
  sample_n(30)

titles_node <- sample_netflix_titles %>%
  distinct(title) %>%
  rowid_to_column("id")

word_frequency_by_title <- sample_netflix_titles %>%
  select(title, description) %>%
  unnest_tokens(word, description) %>%
  count(title, word, sort = TRUE)

title_description_tf_idf <- word_frequency_by_title %>%
  bind_tf_idf(word, title , n)

title_similarity <- title_description_tf_idf %>%
  pairwise_similarity(title, word, tf_idf, sort = TRUE)

titles_edges <- title_similarity %>%
  left_join(titles_node, by = c("item1" = "title")) %>%
  rename(from = id)

titles_edges %<>%
  left_join(titles_node, by = c("item2" = "title")) %>%
  rename(to = id)

titles_edges %<>%
  select(from, to, similarity)

titles_graph <- tbl_graph(nodes = titles_node, edges = titles_edges, directed = TRUE)

ggraph(titles_graph, layout = "fr") +
  geom_edge_link(aes(colour = similarity)) +
  geom_node_point(shape = 21, size = 5, fill = "red") +
  geom_node_text(aes(label = title), vjust = -1.1) +
  scale_edge_colour_gradient(low = "gray70", high = "red") +
  labs(edges_width = "similarity", title = "Description similarity among Netflix titles",
       caption = "Source: Flixable | Author: Victor Navarro") +
  theme_graph()

