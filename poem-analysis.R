# First task.
# Text analysis on translation of a poem

# Load the necessary libraries
library(data.table)
library(tidytext)
library(tidyverse)
library(dplyr)
library(ggplot2)

# set wd
setwd("D:/CEU/Winter 2018/Unstructured Text Data/final project")

# Read in the poem "Faith" by the Bulgarian poet Nikola Vapcarov(1909-1942) in 5 languages.
# All relevant csv-s can be found in the Repo.
bg_poem <- read.csv("poem-bg.csv", sep = '|', stringsAsFactors=FALSE)
engl_poem <- read.csv("poem-engl.csv", sep = '|', stringsAsFactors=FALSE)
rus_poem <- read.csv("poem-rus.csv", sep = '|', stringsAsFactors=FALSE)

# Since this approach was not appropriate for German and French language (the letters were not read
# correcly, I need to use alternative approach:
# (Even setting the right locale did not resolve the problem)

poem_german <- c('Sieh - ich lebe',
                 'und atme',
                 'wie jedermann,',
                 'ich arbeite, dichte',
                 '(so gut ich kann).',
                 'Das Leben und ich, wirschauen',
                 'unter gerümpften Brauen uns an,',
                 'und ich schlag mich mit ihm,',
                 'so gut ich nur kann.',
                 #'',
                 'Wir sind miteinander in Zwist,',
                 'doch fasse',
                 'die Wahrheit beim Schopfe:',
                 'Ich kämpfe nicht, weil',
                 'ich das Leben hasse -',
                 'im Gegenteil, ganz in Gegenteil!',
                 'Seibst noch im Sterben,',
                 'selbst unter den Hieben',
                 'seiner stählernen Tatzen werd ich es lieben.',
                 'Ich werd es noch immer lieben!',
                 #'',
                 'Wenn man jetzt den Strick',
                 'um den Hals mir legt,',
                 'mich fragt.',
                 'Willst du leben? Noch eine Stunde?',
                 'Weg mit dem Strick!',
                 'schrei ich auf',
                 'erregt,',
                 'weg mit dem Strick,',
                 'aber schnell, ihr Hunde!',
                 #'',
                 'Bin bereith - da ich',
                 'alles fürs Leben täte -',
                 'in den Himmel',
                 'im Probeflugzeug zu stechen,',
                 'zum einsamen Flug in der Weltraumrakete',
                 'auf die Suche',
                 'nacht fernen Stern',
                 'aufzubrechen.',
                 #'',
                 'Trotz des Stricks',
                 'würd es prickeln',
                 'auf meiner Haut',
                 'beim Anblick, des Himmels,',
                 'der oben blaut.',
                 'Ich empfinde ein Prickeln.',
                 'Lustvoll und scharf,',
                 'weil ich leb,',
                 'weil ich leben und leben darf.',
                 #'',
                 'Und wolltet ihr einmal vielleicht',
                 'meinem Glauben',
                 '- wie viel? -',
                 'nur würde brüllen,',
                 'brüllen vor Schmerzen,',
                 'wie ein Panter brüllt',
                 'mit verwundetem Herzen.',
                 #'',
                 'was bleibt von mir übrig?',
                 'Mich wird sofort',
                 'dieser Diebstahl zerfasern',
                 'mit seinen Krallen.',
                 'Noch klarer:',
                 'bestohlen,',
                 'werd ich an Ort',
                 'und Stelle',
                 '- sofort -',
                 'in ein Nichts zerfallen.',
                 #'',
                 'Ihr denkt wahrscheinlich,',
                 'dass eure Tat',
                 'meinen Glauben',
                 'an selige Tage vernichtet,',
                 'meinen Glauben daran,',
                 'dass die Zeit, die uns naht,',
                 'ein weiseres, schöneres Leben',
                 'errichtet.',
                 #'',
                 'Wie greift ihr ihn an?',
                 'Mit Kugeln? Du musst',
                 'es dir merken:',
                 'Das wird euch misslingen!',
                 'Meinen Glauben schützt eine gepanzerte Brust,',
                 'und Panzergeschosse,',
                 'sie zu durchdringen',
                 'und meinen Glauben auch',
                 'nur zu verwunden,',
                 'sind nich erfunden,',
                 'noch nicht erfunden!')
poem_ger_df <- data.table(line=1:length(poem_german), text=poem_german)

poem_fr <- c("Voil? - je respire,",
             "je travaille, je vis",
             "et j'écris des vers",
             "(à ma façon).",
             "La vie et moi, en fronçant les sourcils,",
             "nous nous mesurons du regard",
             "et je luttle avec elle",
             "autant que je le puis.",
             #"",
             "Avec la vie nous sommes aux prises,",
             "mais ne va pas croire, pas croire",
             "que je hais la vie.",
             "Au contraire, au contraire!",
             "Même si j'allais mourir,",
             "la vie, avec sa brutale",
             "poigne d'acier",
             "je l'aimerais quand même!",
             "je l'aimerais quand même!",
             #"",
             "Supposons qu'à présent on me passe au cou",
             "la corde",
             "et qu'on me demande",
             "Dis, veux-tu vivre une heure encore?",
             "Aussitôt je crierais:",
             "Enlevez!",
             "Enlevez!",
             "Enlevez plus vite",
             "la corde, scélérats!",
             #"",
             "Pour elle - La Vie -",
             "j'aurais tout fait.",
             "J'aurais volé",
             "sur un appareil d'essai dans le ciel,",
             "je serais entré dans une fusée",
             "explosive, tout seul,",
             "j'aurais cherch?",
             "dans l'espace",
             "une inaccessible",
             "planète.",
             #"",
             "J'éprouverais du moins",
             "l'agréable frisson",
             "de voir comment",
             "là-haut",
             "le ciel est bleu.",
             "J'éprouverais du moins",
             "l'agréable frisson",
             "de vivre encore,",
             "d' avoir encore à vivre.",
             #"",
             "Mais voilà, supposons",
             "que vous preniez - Combien? -",
             "rien qu'un grain",
             "de ma foi,",
             "alors je hurlerais",
             "je hurlerais de douleur",
             "comme une panthère",
             "blessée à mort.",
             #"",
             "Alors, de moi",
             "que me resterait-il?",
             "Dès après ce pillage",
             "je serais désemparé.",
             "Et plus clairement",
             "et plus exactement encore.",
             "Dès aprè ce pillage",
             "je ne serais plus rien.",
             #"",
             "Peut-être voules-vous",
             "l'abattre,",
             "ma foi en des jours heureux,",
             "ma foi en demain",
             "qui fera la vie plus belle,",
             "plus pleine de sagesse?",
             #"",
             "Et comment l'attaqueriezvous, s'il vous",
             "plaît?",
             "Avec des balles?",
             "Non! Déplacé!",
             "Pas la peine! - Cela ne vaut rien! -",
             "Elle est cuirassée",
             "solidement dans ma poitrine",
             "et les balles pouvant percer",
             "son armure",
             "ne sont pas inventées!",
             "Ne sont pas inventées!")
poem_fr_df <- data.table(line=1:length(poem_fr), text=poem_fr)

## Find the relevant stop words for Bulgarian, English, French, German & Russian

# The selection and filtering and updating of the Bulgarian stop words was conducted in
# "stop-words-bg.R" file available in the Repo. The file is "bgn_s_w".

# Stop words in different languages
#install.packages("stopwords")
library(stopwords)

# English version is available using "stop_words" df.

# Some data prep is needed in order all remaining files to have the same structure
ger_s_w <- data.frame(stopwords(language = "de"), stringsAsFactors = FALSE)
ger_s_w <- set_names(ger_s_w,"word")
rus_s_w <- data.frame(stopwords(language = "ru"), stringsAsFactors = FALSE)
rus_s_w <- set_names(rus_s_w, "word")
fr_s_w <- data.frame(stopwords(language = "fr"), stringsAsFactors = FALSE)
fr_s_w <- set_names(fr_s_w, "word")


## Transform the poems in 5 languages into tidy text format and count the most frequent words.

## Bulgarian original
tidy_bg <- bg_poem %>%
  unnest_tokens(word, text) %>%
  ungroup()

# Top 10 most frequently used words in the English version
tidy_bg %>% 
  anti_join(bgn_s_w) %>%
  count(word, sort = TRUE) %>%
  head(10)
# visualize all words that occur >= 2 times in the poem.
tidy_bg %>%
  anti_join(bgn_s_w) %>%
  count(word, sort = TRUE) %>%
  filter(n >= 2) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "darkblue") +
  xlab(NULL) +
  ggtitle("Most frequent words in the Bulgarian original") +
  theme_bw() +
  coord_flip() 

# words that appear more than or equal to 2 times
bg_words_2 <- tidy_bg %>%
  anti_join(bgn_s_w) %>%
  count(word, sort = TRUE) %>%
  filter(n>=2)


### Carry on the same steps for the remaining 4 languages

# English
tidy_engl <- engl_poem %>%
  unnest_tokens(word, text) %>%
  ungroup()

# Top 10 most frequently used words in the English version
tidy_engl %>% 
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  head(10)

tidy_engl %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  filter(n >= 2) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "darkgreen") +
  xlab(NULL) +
  ggtitle("Most frequent words in the English translation") +
  theme_bw() +
  coord_flip() 

engl_words_2 <- tidy_engl %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  filter(n>=2)

# Russian
tidy_rus <- rus_poem %>%
  unnest_tokens(word, text) %>%
  ungroup()

# Top 10 most frequently used words in the Russian version
tidy_rus %>%
  anti_join(rus_s_w) %>%
  count(word, sort = TRUE) %>%
  head(10)

tidy_rus %>%
  anti_join(rus_s_w) %>%
  count(word, sort = TRUE) %>%
  filter(n >= 2) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "darkred") +
  xlab(NULL) +
  ggtitle("Most frequent words in the Russian translation") +
  theme_bw() +
  coord_flip() 

# The "б" letter is a part of a auxiliary verb in the Russian language(e.g. the "am" part in
# "I am happy"). Hence, maybe it should be included in the Russian stop word list.

rus_words_2 <- tidy_rus %>%
  anti_join(rus_s_w) %>%
  count(word, sort = TRUE) %>%
  filter(n>=2)

# German
tidy_ger <- poem_ger_df %>%
  unnest_tokens(word, text) %>%
  ungroup()

# Top 10 most frequently used words in the German version
tidy_ger %>%
  anti_join(ger_s_w) %>%
  count(word, sort = TRUE) %>%
  head(10)

tidy_ger %>%
  anti_join(ger_s_w) %>%
  count(word, sort = TRUE) %>%
  filter(n >= 2) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "darkorange") +
  xlab(NULL) +
  ggtitle("Most frequent words in the German translation") +
  theme_bw() +
  coord_flip() 

ger_words_2 <- tidy_ger %>%
  anti_join(ger_s_w) %>%
  count(word, sort = TRUE) %>%
  filter(n>=2)

## French
tidy_fr <- poem_fr_df %>%
  unnest_tokens(word, text) %>%
  ungroup()

# Top 10 most frequently used words in the French version
tidy_fr %>%
  anti_join(fr_s_w) %>%
  count(word, sort = TRUE) %>%
  top_n(10)

tidy_fr %>%
  anti_join(fr_s_w) %>%
  count(word, sort = TRUE) %>%
  filter(n >= 2) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "purple") +
  xlab(NULL) +
  ggtitle("Most frequent words in the French translation") +
  theme_bw() +
  coord_flip() 

fr_words_2 <- tidy_fr %>%
  anti_join(fr_s_w) %>%
  count(word, sort = TRUE) %>%
  filter(n>=2)

# It would be interesting to investigate how many of the words that occur at least twice 
# in the poem "Faith" and are in the original Bulgarian version are actually present in the 
# translated versions. Since a manual translation is necessary from a language to English
# (to serve the purposes of this assignment), an excel document could be found with the 
# above-mentioned computation in the Repo. Words that are in common with the Bulgarian original:
# ~ English translation - 7 words in common;
# ~ Russian translation (Would it be easier to translate into a language that fall into the Slavic
# group similarly as the Bulgarian?) - 4 words in common
# ~ German translation - 4 words in common
# ~ French translation - 9 words in common


#############################################
### Conduct an analysis of a set of documents
#############################################

## Read the set of 11 poems by Vapcarov - both in Bulgarian and English.
# Csv files can be obtained from the Repository in foder "raw_data".
poems_11_bg <- read.csv("Vapcarov_poems_bg.csv", sep = '|', stringsAsFactors=FALSE)
poems_11_bg$poem_nb <- as.numeric(poems_11_bg$poem_nb)
poems_11_bg <- data.table(poems_11_bg)
poems_11_bg <- poems_11_bg[complete.cases(poems_11_bg)]

# row length of the 11 poems
#  1   2   3   4   5   6   7   8   9  10  11 
# 80  51  69  73  56 156 112 175  67  46  30 

poems_11_engl <- read.csv("Vapcarov_poems_engl.csv", sep = '|', stringsAsFactors=FALSE)
poems_11_engl <- data.table(poems_11_engl)
poems_11_engl <- poems_11_engl[complete.cases(poems_11_engl)]

# row length of the 11 poems - English
#  1   2   3   4   5   6   7   8   9  10  11 
# 81  52  68  72  42 158 105 177  68  30  30 

### Extract the topics of a set of 11 poems by Vapcarov.
# We will investigate the possible number of topics.

library(topicmodels)
## LDA on different poems

### Bulgarian original
# Split per words
by_title_bg <- poems_11_bg %>%
  mutate(line_number = row_number()) %>%
  unnest_tokens(word, text) %>%
  ungroup()

# title-word counts  
count_words_bg <- by_title_bg %>%
  anti_join(bgn_s_w) %>%
  count(title, word, sort = TRUE) %>%
  ungroup()

# transform to document term matrix format
term_mat_bg <- count_words_bg %>%
  cast_dtm(title, word, n)

# Let's try with k = 3 for the topic modeling.
poems_lda_bg <- LDA(term_mat_bg, k = 3, control = list(seed = 9876))

# per-topic per-word probabilities
poem_topics_bg <- tidy(poems_lda_bg, matrix = "beta")

# get the top words per topic
top_terms_bg <- poem_topics_bg %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Vizualize
top_terms_bg %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
# Although there is good differentiation:
# ~ topic 1: life, man, the man, song, i was;
# ~ topic 2: land, life, heart, sky, remember, my, we were, song;
# ~ topic 3: life, the engine, friend, sky, my, entered,
# there is an overlap in words lie life, song,sky. Thus, let's try with 2 topics differentiation.

# Assumption - let k = 2.
poems_lda_bg <- LDA(term_mat_bg, k = 2, control = list(seed = 9876))

# per-topic per-word probabilities
poem_topics_bg <- tidy(poems_lda_bg, matrix = "beta")

# get the top words per topic
top_terms_bg <- poem_topics_bg %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Vizualize
top_terms_bg %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
# Acheieved better differentiation:
# ~ topic 1: life, the man, i was, life and song;
# ~ topic 2: land, life, the heart, sky, factory.

# get the probability of each topic existing in each chapter
poems_gamma_bg <- tidy(poems_lda_bg, matrix = "gamma")
poems_gamma_bg %>%
  top_n(15)

# Inriguingly, even though the 11th poem is called "Song" it does not contain the topic 1,
# which includes the word "Song". That is only natural - here Vapcarov conceived the idea in metaphors,
# rather than explicitly stating the word "song".
# Other than that the "Homeland" poem falls into topic 2 - which includes land and sky, etc.
# For a more detailed investigation, plese check the ouput csv "two_topics-two_languages"
# in the output files folder.

### Let's investigate whether we would get the same/similar results if we use the English version.
by_title_engl <- poems_11_engl %>%
  mutate(line_number = row_number()) %>%
  unnest_tokens(word, text) %>%
  ungroup()

# title-word counts  
count_words_engl <- by_title_engl %>%
  anti_join(stop_words) %>%
  count(title, word, sort = TRUE) %>%
  ungroup()

# transform to document term matrix format
term_mat_engl <- count_words_engl %>%
  cast_dtm(title, word, n)

# Let's try with k = 3 for the topic modeling.
poems_lda_engl <- LDA(term_mat_engl, k = 3, control = list(seed = 9876))

# per-topic per-word probabilities
poem_topics_engl <- tidy(poems_lda_engl, matrix = "beta")

# get the top words per topic
top_terms_engl <- poem_topics_engl %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Vizualize
top_terms_engl %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
# Good differentiation, but "life" and "song" words are repeated among categories.

# Assumption - let k = 2.
poems_lda_engl <- LDA(term_mat_engl, k = 2, control = list(seed = 9876))

# per-topic per-word probabilities
poem_topics_engl <- tidy(poems_lda_engl, matrix = "beta")

# get the top words per topic
top_terms_engl <- poem_topics_engl %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Vizualize
top_terms_engl %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# A better differentiation between topics - now only the word "life" is repeated. 
# There are some slight changes compared to the Bulgarian original. 

plot_2 <- top_terms_engl %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  ggtitle("Topic determinants from English translation")

plot_1 <- top_terms_bg %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  ggtitle("Topic determinants from the Bulgarian original")

#install.packages("gridExtra")
library(gridExtra)
grid.arrange(plot_1, plot_2, nrow=1)

# get the probability of each topic existing in each chapter
poems_gamma_engl <- tidy(poems_lda_engl, matrix = "gamma")
poems_gamma_engl

# A quick observations implies that 
# for topic 1 there is word that coinsides - "life/живот‚"
# for topic 2 again there are 3 words that coinside - "life/живот", "land/земя" & "heart/сърце". 

###################
### TF-IDF analysis
###################
# tf-idf on a set of 11 poems by Vapcarov.
# We will compare the most important words in Bulgarian original vs English translation.

# Bulgarian original
poems_words_bg <- poems_11_bg %>%
  unnest_tokens(word,text) %>%
  count(title, word, sort = TRUE) %>%
  ungroup()

total_words_bg <- poems_words_bg %>%
  group_by(title) %>%
  summarize(total = sum(n))

t_w_bg <- left_join(poems_words_bg, total_words_bg)

t_w_bg <- t_w_bg %>%
  bind_tf_idf(word, title, n)

t_w_bg %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(title) %>%
  top_n(2) %>%
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = title)) + 
  geom_col(show.legend = FALSE) + 
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~title, ncol = 2, scales = "free") +
  coord_flip()
# not super successful differentiation when we analyse the Bulgarian original

## TF-IDF with the English translation

poems_words_engl <- poems_11_engl %>%
  unnest_tokens(word,text) %>%
  count(title, word, sort = TRUE) %>%
  ungroup()

total_words_engl <- poems_words_engl %>%
  group_by(title) %>%
  summarize(total = sum(n))

t_w_engl <- left_join(poems_words_engl, total_words_engl)

t_w_engl <- t_w_engl %>%
  bind_tf_idf(word, title, n)

t_w_engl %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(title) %>%
  top_n(2) %>%
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = title)) + 
  geom_col(show.legend = FALSE) + 
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~title, ncol = 2, scales = "free") +
  coord_flip()

# This TF-IDF on the English translation captures better the most important
# terms per poem than the similar analysis conducted on the Bulgarian original.  
# A possible explanation might be the fact that poets often do not state explicitly
# their ideas, rather they try to conceal them using synomim words. However, this "concealing part"
# often cannot be preserved when translated. 

