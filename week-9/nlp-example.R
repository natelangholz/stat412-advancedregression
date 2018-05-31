https://github.com/statsmaths/useR2017_nlp

knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
options(dplyr.print_max = 6, dplyr.print_min = 6)
options(width = 68)

#packages
library(cleanNLP)
library(dplyr)
library(readr)
library(stringi)
library(ggplot2)
library(tokenizers)
library(cleanNLP)
library(magrittr)


letranger <- stri_c(
  "Aujourd'hui, maman est morte. Ou peut-être hier, je ne sais pas.",
  "J'ai reçu un télégramme de l'asile: «Mère décédée. Enterrement demain.",
  "Sentiments distingués.» Cela ne veut rien dire. C'était peut-être",
  "hier.", collapse = " ")

stri_wrap(letranger)

stri_split(letranger, fixed = " ")

init_tokenizers(locale = "fr")

letranger_anno <- run_annotators(letranger, as_strings = TRUE)
letranger_anno

letranger_tokens <- get_combine(letranger_anno)
letranger_tokens

letranger_tokens$word


paths <- dir("data/holmes_stories", full.names = TRUE)
sh_meta <- data_frame(id = as.integer(seq_along(paths)),
                      story = stri_sub(basename(paths), 4, -5))
sh_meta %>% print(n = 5)

#part 2
init_tokenizers(locale = "en_GB")

sh_anno <- run_annotators(paths)

sh_tokens <- get_combine(sh_anno)

sh_tokens %<>% mutate(id = as.integer(gsub("doc","",id)))
sh_tokens

sh_tokens %>% filter(id == 1) %>% filter(sid == 10) %>% print(n = 12)

## ---- eval = FALSE-------------------------------------------------------
## sh_tokens %>%
##   group_by(id) %>%
##   mutate(percentage_loc = sid / max(sid)) %>%
##   filter(word %in% c("Watson", "Holmes")) %>%
##   ggplot(aes(percentage_loc, id)) +
##     geom_point(aes(color = word), alpha = 0.5) +
##     geom_hline(yintercept = 44)

library(viridis)
sh_tokens %>%
  group_by(id) %>%
  mutate(percentage_loc = sid / max(sid)) %>%
  filter(word %in% c("Watson", "Holmes")) %>%
  ggplot(aes(percentage_loc, id)) +
  geom_point(aes(color = word), alpha = 0.5) +
  scale_color_viridis(discrete=TRUE, end = 0.7, option = "C") +
  geom_hline(yintercept = 44)

## sh_tokens %>%
##   mutate(sentence_end = word %in% c(".", "?", "!")) %>%
##   group_by(id) %>%
##   summarize(mean_sentence_len = n() / sum(sentence_end)) %>%
##   ggplot(aes(id, mean_sentence_len)) +
##     geom_line() +
##     geom_point()

sh_tokens %>%
  mutate(sentence_end = sh_tokens$word %in% c(".", "?", "!")) %>%
  group_by(id) %>%
  summarize(mean_sentence_len = n() / sum(sentence_end)) %>%
  ggplot(aes(id, mean_sentence_len)) +
  geom_line() +
  geom_point()

## sh_tokens %>%
##   count(id, word, sort = TRUE)

sh_tokens %>%
  count(id, word, sort = TRUE) %>%
  ungroup()

## ---- echo = FALSE-------------------------------------------------------
temp <- sh_tokens %>%
  count(id, word, sort = TRUE) %>%
  ungroup()
arrange(temp, id)[5:10,]

## ------------------------------------------------------------------------
stopwords <- readLines("data/stopwords_en.txt")
sample(stopwords, 25L)

## ------------------------------------------------------------------------
sh_toptokens <- sh_tokens %>%
  filter(!(tolower(word) %in% stopwords)) %>%
  count(id, word, sort = TRUE) %>%
  group_by(id) %>%
  top_n(n = 10, n) %>%
  left_join(sh_meta, by = "id")

## ---- echo=FALSE---------------------------------------------------------
sh_toptokens <- ungroup(sh_toptokens)

## ------------------------------------------------------------------------
sh_toptokens %>% filter(id == 1) %>% print(n = Inf)

## ------------------------------------------------------------------------
sh_toptokens %>% filter(id == 44) %>% print(n = Inf)

## ------------------------------------------------------------------------
sh_propn <- sh_tokens %>%
  filter(!(tolower(word) %in% stopwords)) %>%
  filter((tolower(word) != word)) %>%
  count(id, word, sort = TRUE) %>%
  group_by(id) %>%
  top_n(n = 10, n) %>%
  left_join(sh_meta, by = "id")

## ---- echo=FALSE---------------------------------------------------------
sh_propn <- ungroup(sh_propn)

## ------------------------------------------------------------------------
sh_propn %>% filter(id == 1) %>% print(n = Inf)

## ------------------------------------------------------------------------
sh_propn %>% filter(id == 44) %>% print(n = Inf)

## ------------------------------------------------------------------------
holmes_watson <- c("Sherlock", "Holmes", "John", "Watson")
sh_topchar <- sh_tokens %>%
  filter(stri_length(word) > 4) %>%
  filter(!(word %in% holmes_watson)) %>%
  filter(!(tolower(word) %in% stopwords)) %>%
  filter((tolower(word) != word)) %>%
  count(id, word) %>%
  left_join(sh_meta, by = "id") %>%
  group_by(id) %>%
  top_n(n = 1, n)

## ---- echo = FALSE-------------------------------------------------------
sh_topchar <- ungroup(sh_topchar)

## ------------------------------------------------------------------------
sh_topchar %>% filter(id %in% c(1, 45)) %>%
  print(n = Inf)

## ---- eval = FALSE-------------------------------------------------------
## sh_tokens %>%
##   group_by(id) %>%
##   mutate(percentage_loc = sid / max(sid)) %>%
##   semi_join(sh_topchar, by = c("id", "word")) %>%
##   ggplot(aes(percentage_loc, id)) +
##     geom_point()

## ---- echo = FALSE-------------------------------------------------------
sh_tokens %>%
  group_by(id) %>%
  mutate(percentage_loc = sid / max(sid)) %>%
  semi_join(sh_topchar, by = c("id", "word")) %>%
  ggplot(aes(percentage_loc, id)) +
  geom_point()

## ------------------------------------------------------------------------
sh_theme <- sh_tokens %>%
  filter(!(tolower(word) %in% stopwords)) %>%
  filter((tolower(word) == word)) %>%
  count(id, word) %>%
  group_by(id) %>%
  top_n(n = 10, n) %>%
  left_join(sh_meta, by = "id")

## ---- echo = FALSE-------------------------------------------------------
sh_theme <- ungroup(sh_theme)

## ------------------------------------------------------------------------
sh_theme %>% filter(id == 1) %>% print(n = 10)

## ------------------------------------------------------------------------
word_frequency %>% print(n = 9)

## ------------------------------------------------------------------------
sh_wordfreq <- sh_tokens %>%
  mutate(lemma = tolower(word)) %>%
  inner_join(word_frequency, by = c("lemma" = "word")) %>%
  filter(frequency < 0.01) %>%
  filter((tolower(word) == word)) %>%
  count(id, word) %>%
  group_by(id) %>%
  top_n(n = 10, n) %>%
  left_join(sh_meta, by = "id") %>%
  arrange(id, desc(n))

## ---- echo = FALSE-------------------------------------------------------
sh_wordfreq <- ungroup(sh_wordfreq)

## ------------------------------------------------------------------------
sh_wordfreq %>% filter(id == 1) %>% print(n = 12)

#############################


## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
options(dplyr.print_max = 6, dplyr.print_min = 6)
options(width = 68)

## ---- message=FALSE------------------------------------------------------
library(cleanNLP)
library(dplyr)
library(readr)
library(stringi)
library(ggplot2)
theme_set(theme_minimal())

## ---- eval = FALSE-------------------------------------------------------
## library(cleanNLP)
## init_spaCy(model_name = "en")
## anno <- run_annotators(paths)
## nlp <- get_combine(anno)

## ---- eval = FALSE-------------------------------------------------------
## library(cleanNLP)
## init_coreNLP(language = "en")
## anno <- run_annotators(paths)
## nlp <- get_combine(anno)

## ---- echo = FALSE, message = FALSE--------------------------------------
nlp <- read_csv("data/sh_nlp.csv.gz")

## ---- echo = FALSE-------------------------------------------------------
nlp

## ---- message = FALSE----------------------------------------------------
paths <- dir("data/holmes_stories", full.names = TRUE)
sh_meta <- data_frame(id = seq_along(paths),
                      story = stri_sub(basename(paths), 4, -5))
sh_nlp <- read_csv("data/sh_nlp.csv.gz")

## ---- eval = FALSE-------------------------------------------------------
## sh_nlp %>%
##   group_by(id, sid) %>%
##   mutate(sentence_end = tid == max(tid)) %>%
##   group_by(id) %>%
##   summarize(mean_sentence_len = n() / sum(sentence_end)) %>%
##   ggplot(aes(id, mean_sentence_len)) +
##     geom_line() +
##     geom_point()

## ---- echo = FALSE-------------------------------------------------------
sh_nlp %>%
  group_by(id, sid) %>%
  mutate(sentence_end = tid == max(tid)) %>%
  group_by(id) %>%
  summarize(mean_sentence_len = n() / sum(sentence_end)) %>%
  ggplot(aes(id, mean_sentence_len)) +
  geom_line() +
  geom_point()

## ------------------------------------------------------------------------
sh_nlp %>% filter(tolower(word) != lemma) %>%
  select(word, lemma) %>% print(n = 10)

## ------------------------------------------------------------------------
sh_lemmafr <- sh_nlp %>%
  left_join(word_frequency, by = c("lemma" = "word")) %>%
  filter(!is.na(frequency)) %>%
  filter(frequency < 0.01) %>%
  filter((tolower(word) == word)) %>%
  count(id, lemma) %>%
  group_by(id) %>%
  top_n(n = 10, n) %>%
  left_join(sh_meta, by = "id") %>%
  arrange(id, desc(n))

## ---- echo = FALSE-------------------------------------------------------
sh_lemmafr <- ungroup(sh_lemmafr)

## ------------------------------------------------------------------------
sh_lemmafr %>% filter(id == 1) %>% print(n = 12)

## ------------------------------------------------------------------------
sh_topchar <- sh_nlp %>%
  filter(upos == "PROPN") %>%
  count(id, word) %>%
  group_by(id) %>%
  top_n(n = 10, n) %>%
  left_join(sh_meta, by = "id") %>%
  arrange(id, desc(n))

## ---- echo = FALSE-------------------------------------------------------
sh_topchar <- ungroup(sh_topchar)

## ------------------------------------------------------------------------
sh_topchar %>% filter(id == 1) %>% print(n = Inf)

## ------------------------------------------------------------------------
sh_compound <- sh_nlp %>%
  filter(upos == "PROPN") %>%
  group_by(id, sid) %>%
  mutate(d = tid - lag(tid) - 1) %>%
  mutate(d = ifelse(is.na(d), 1, d)) %>%
  ungroup() %>%
  mutate(d = cumsum(d)) %>%
  group_by(d) %>%
  summarize(id = first(id), sid = first(sid),
            tid = first(tid),
            thing = stri_c(word, collapse = " ")) %>%
  select(-d) %>%
  inner_join(sh_nlp, by = c("id", "sid", "tid"))

## ------------------------------------------------------------------------
sh_compound %>% select(id, thing) %>% print(n = 10)

## ------------------------------------------------------------------------
results <- sh_nlp %>%
  select(id, entity, entity_type) %>%
  filter(!is.na(entity))

## ------------------------------------------------------------------------
results %>% print(n = 10)

## ------------------------------------------------------------------------
sh_nerchar <- sh_nlp %>%
  select(id, entity, entity_type) %>%
  filter(!is.na(entity)) %>%
  filter(entity_type == "PERSON") %>%
  count(id, entity) %>%
  group_by(id) %>%
  top_n(n = 10, n) %>%
  left_join(sh_meta, by = "id") %>%
  arrange(id, desc(n))

## ------------------------------------------------------------------------
sh_nerchar <- ungroup(sh_nerchar)

## ------------------------------------------------------------------------
sh_nerchar %>% filter(id == 1) %>% print(n = Inf)

## ------------------------------------------------------------------------
sh_nlp %>% filter(id == 1, sid == 1) %>%
  select(word, source, relation, word_source)

## ------------------------------------------------------------------------
sh_whatchar <- sh_nlp %>%
  filter(relation == "nsubj") %>%
  filter(upos == "PROPN") %>%
  count(id, word, lemma_source) %>%
  filter(n > 1)

## ---- echo = FALSE-------------------------------------------------------
sh_whatchar <- ungroup(sh_whatchar)

## ------------------------------------------------------------------------
sh_whatchar %>% print(n = 12)



##################################
#Part 3


## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
options(dplyr.print_max = 6, dplyr.print_min = 6)
options(width = 68)

## ---- message=FALSE------------------------------------------------------
library(cleanNLP)
library(dplyr)
library(readr)
library(stringi)
library(ggplot2)
library(topicmodels)
library(glmnet)
library(ggrepel)
library(viridis)
library(magrittr)
theme_set(theme_minimal())

## ---- message = FALSE----------------------------------------------------
sotu_nlp <- read_csv("data/sotu.csv.gz")
sotu_meta <- read_csv("data/sotu_meta.csv")

## ------------------------------------------------------------------------
sotu_nlp %>%
  count(id, sid) %$%
  quantile(n, seq(0,1,0.1))

## ------------------------------------------------------------------------
sotu_nlp %>%
  filter(upos == "NOUN") %>%
  count(lemma) %>%
  top_n(n = 40, n) %>%
  use_series(lemma)

## ---- eval = FALSE-------------------------------------------------------
## sotu_nlp %>%
##   count(id) %>%
##   group_by(id) %>%
##   left_join(sotu_meta, by = "id") %>%
##   ggplot(aes(year, n)) +
##     geom_line(color = grey(0.8)) +
##     geom_point(aes(color = sotu_type)) +
##     geom_smooth()

## ---- echo = FALSE, message = FALSE--------------------------------------
sotu_nlp %>%
  count(id) %>%
  group_by(id) %>%
  left_join(sotu_meta, by = "id") %>%
  ggplot(aes(year, n)) +
  geom_line(color = grey(0.8)) +
  geom_point(aes(color = sotu_type)) +
  geom_smooth() +
  scale_color_viridis(discrete=TRUE, end = 0.7, option = "C") +
  theme(axis.text.x = element_text(size = 12),
        legend.position="bottom",
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)) +
  xlab("Year") +
  ylab("Number of words") +
  labs(color = "SOTU Address type")

## ------------------------------------------------------------------------
summary_2001 <- sotu_nlp %>%
  left_join(sotu_meta, by = "id") %>%
  filter(year == 2001, relation == "dobj") %>%
  left_join(word_frequency, by = "word") %>%
  filter(frequency < 0.001) %>%
  select(id, word, word_source) %$%
  sprintf("%s => %s", word_source, word)

## ------------------------------------------------------------------------
summary_2001

## ---- echo = FALSE-------------------------------------------------------
summary_2002 <- sotu_nlp %>%
  left_join(sotu_meta, by = "id") %>%
  filter(year == 2002, relation == "dobj") %>%
  left_join(word_frequency, by = "word") %>%
  filter(frequency < 0.001) %>%
  select(id, word, word_source) %$%
  sprintf("%s => %s", word_source, word)

## ------------------------------------------------------------------------
head(summary_2002, 34)

## ---- echo = FALSE-------------------------------------------------------
summary_1919 <- sotu_nlp %>%
  left_join(sotu_meta, by = "id") %>%
  filter(year == 1919, relation == "dobj") %>%
  left_join(word_frequency, by = "word") %>%
  filter(frequency < 0.001) %>%
  select(id, word, word_source) %$%
  sprintf("%s => %s", word_source, word)

## ------------------------------------------------------------------------
head(summary_1919,  34)

## ------------------------------------------------------------------------
sotu_tfidf <- sotu_nlp %>%
  filter(pos %in% c("NN", "NNS")) %>%
  get_tfidf(min_df = 0.05, max_df = 0.95,
            type = "tfidf", tf_weight = "dnorm")

## ------------------------------------------------------------------------
head(sotu_tfidf$vocab, 20)
head(sotu_tfidf$id, 20)
dim(sotu_tfidf$tfidf)

## ------------------------------------------------------------------------
sotu_pca <- tidy_pca(sotu_tfidf$tfidf, sotu_meta)
select(sotu_pca, president, party, PC1, PC2)

## ---- eval = FALSE-------------------------------------------------------
## ggplot(sotu_pca, aes(PC1, PC2)) +
##   geom_point(aes(color = cut(year, 10, dig.lab = 4))) +
##   geom_text(data = filter(sotu_pca, !duplicated(president)))

## ---- echo = FALSE, message = FALSE--------------------------------------
ggplot(sotu_pca, aes(PC1, PC2)) +
  geom_point(aes(color = cut(year, 10, dig.lab = 4)), alpha = 0.35,
             size = 4) +
  geom_text_repel(data = filter(sotu_pca, !duplicated(president)),
                  aes(label = president),
                  color = grey(0.4), cex = 3) +
  labs(color = "Year") +
  scale_color_viridis(discrete=TRUE, end = 0.9, option = "C") +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

## ---- eval = FALSE-------------------------------------------------------
## sotu_tf <- sotu_nlp %>%
##   filter(pos %in% c("NN", "NNS")) %>%
##   get_tfidf(min_df = 0.05, max_df = 0.95,
##                       type = "tf", tf_weight = "raw")
## tm <- LDA(sotu_tf$tf, k = 16, control = list(verbose = 1))

## ---- echo = FALSE-------------------------------------------------------
# this takes a few minutes to run and we don't want
# to wait everytime for it to finish, so load from
# cache... also, the results are stochastic (and seem
# to not respect set.seed); this fixes the problem
# as well
sotu_tf <- sotu_nlp %>%
  filter(pos %in% c("NN", "NNS")) %>%
  get_tfidf(min_df = 0.05, max_df = 0.95,
            type = "tf", tf_weight = "raw")
tm <- read_rds("data/sotu_tm.Rds")

## ------------------------------------------------------------------------
terms <- posterior(tm)$terms
topics <- posterior(tm)$topics
topic_df <- data_frame(topic = as.integer(col(topics)),
                       id = sotu_meta$id[as.integer(row(topics))],
                       val = as.numeric(topics)) %>%
  left_join(sotu_meta, by = "id")
top_terms <- apply(terms, 1,
                   function(v) paste(sotu_tf$vocab[order(v,
                                                         decreasing = TRUE)[1:5]], collapse = ", "))
top_terms <- as.character(top_terms)

## ------------------------------------------------------------------------
top_terms

## ---- echo = FALSE-------------------------------------------------------
index <- rank(-1 * tapply(topic_df$year * topic_df$val,
                          topic_df$topic, which.max))
topic_df$topic_new <- index[topic_df$topic]
top_terms_df <- data_frame(top_terms, topic = 1:length(top_terms))
top_terms_df$topic_new <- index[top_terms_df$topic]

## ---- echo = FALSE-------------------------------------------------------
ggplot(topic_df, aes(year, topic_new)) +
  geom_point(aes(size = val, color = factor(topic_new))) +
  geom_text(data = top_terms_df, x = mean(topic_df$year),
            size = 5.5, aes(y = topic_new + 0.4, label = top_terms,
                            color = factor(topic_new)),
            show.legend = FALSE) +
  scale_color_viridis(discrete=TRUE, end = 0.7, option = "C") +
  theme(axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        legend.position="bottom",
        axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 14)) +
  labs(size = "Posterior probability") +
  xlab("Year") +
  guides(colour = FALSE)

## ------------------------------------------------------------------------
df <- sotu_nlp %>%
  left_join(sotu_meta, by = "id") %>%
  filter(president %in% c("Barack Obama", "George W. Bush")) %>%
  mutate(new_id = paste(id, sid, sep = "-")) %>%
  filter(pos %in% c("NN", "NNS"))
mat <- get_tfidf(df, min_df = 0, max_df = 1, type = "tf",
                 tf_weight = "raw", doc_var = "new_id")

## ---- message = FALSE----------------------------------------------------
m2 <- data_frame(new_id = mat$id) %>%
  left_join(df[!duplicated(df$new_id),]) %>%
  mutate(y = as.numeric(president == "Barack Obama")) %>%
  mutate(train = (year %% 2 == 0))

## ---- message = FALSE----------------------------------------------------
model <- cv.glmnet(mat$tf[m2$train,], m2$y[m2$train],
                   family = "binomial", alpha = 0.9)

## ------------------------------------------------------------------------
m2$pred <- predict(model, newx = mat$tf, type = "response",
                   s = model$lambda.1se)
select(m2, new_id, id, sid, president, year, pred)

## ---- eval = FALSE-------------------------------------------------------
## ggplot(m2, aes(factor(year), pred)) +
##   geom_boxplot(aes(fill = president))

## ---- echo = FALSE-------------------------------------------------------
m2$pred <- predict(model, newx = mat$tf, type = "response",
                   s = model$lambda.1se)
ggplot(m2, aes(factor(year),pred)) +
  geom_boxplot(aes(fill = relevel(factor(president), "George W. Bush"))) +
  labs(fill = "President") + xlab("year") + ylab("predicted probability") +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, end = 0.75, option = "C") +
  coord_flip() +
  theme(axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 10)) +
  ylab("Predicted probability") +
  xlab("Year")

## ------------------------------------------------------------------------
beta <- coef(model, s = model[["lambda"]][10])[-1]
sprintf("%s (%d)", mat$vocab, sign(beta))[beta != 0]

## ---- echo = FALSE, message = FALSE--------------------------------------
df <- sotu_nlp %>%
  left_join(sotu_meta) %>%
  filter(president %in% c("William J. Clinton", "George W. Bush")) %>%
  mutate(new_id = paste(id, sid, sep = "-")) %>%
  filter(pos %in% c("NN", "NNS"))
mat <- get_tfidf(df, min_df = 0, max_df = 1, type = "tf",
                 tf_weight = "raw", doc_var = "new_id")
m2 <- data_frame(new_id = mat$id) %>%
  left_join(df[!duplicated(df$new_id),]) %>%
  mutate(y = as.numeric(president == "William J. Clinton")) %>%
  mutate(train = (year %% 2 == 0))
model <- cv.glmnet(mat$tf[m2$train,], m2$y[m2$train], family = "binomial")
m2$pred <- predict(model, newx = mat$tf, type = "response",
                   s = model$lambda.1se)
ggplot(m2, aes(factor(year),pred)) +
  geom_boxplot(aes(fill = relevel(factor(president), "George W. Bush"))) +
  labs(fill = "President") + xlab("year") + ylab("predicted probability") +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, end = 0.75, option = "C") +
  coord_flip() +
  theme(axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 10)) +
  ylab("Predicted probability") +
  xlab("Year")

## ---- message = FALSE----------------------------------------------------
beta <- coef(model, s = model[["lambda"]][9])[-1]
sprintf("%s (%d)", mat$vocab, sign(beta))[beta != 0]

## ---- echo = FALSE, message = FALSE--------------------------------------
df <- sotu_nlp %>%
  left_join(sotu_meta) %>%
  filter(president %in% c("William J. Clinton", "George Washington")) %>%
  mutate(new_id = paste(id, sid, sep = "-")) %>%
  filter(pos %in% c("NN", "NNS"))
mat <- get_tfidf(df, min_df = 0, max_df = 1, type = "tf",
                 tf_weight = "raw", doc_var = "new_id")
m2 <- data_frame(new_id = mat$id) %>%
  left_join(df[!duplicated(df$new_id),]) %>%
  mutate(y = as.numeric(president == "William J. Clinton")) %>%
  mutate(train = (year %% 2 == 0))
model <- cv.glmnet(mat$tf[m2$train,], m2$y[m2$train], family = "binomial")
m2$pred <- predict(model, newx = mat$tf, type = "response",
                   s = model$lambda.1se)
ggplot(m2, aes(factor(year),pred)) +
  geom_boxplot(aes(fill = relevel(factor(president), "George Washington"))) +
  labs(fill = "President") + xlab("year") + ylab("predicted probability") +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6, end = 0.75, option = "C") +
  coord_flip() +
  theme(axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 10)) +
  ylab("Predicted probability") +
  xlab("Year")

## ---- echo = FALSE-------------------------------------------------------
beta <- coef(model, s = model[["lambda"]][12])[-1]
sprintf("%s (%d)", mat$vocab, sign(beta))[beta != 0]

