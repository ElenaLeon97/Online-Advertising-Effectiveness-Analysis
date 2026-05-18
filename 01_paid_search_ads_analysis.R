# Online Advertising Effectiveness
# Script 01: Paid Search Ads Analysis
#
# Expected input:
#   data/campaign_data_short.csv
#
# Main outputs:
#   - Descriptive statistics for clicks
#   - Cleaned text corpus
#   - Term-document matrix
#   - Sentiment and emotion variables
#   - LDA topic model
#   - Regression models for click performance
#   - Non-linear effect visualizations

# -----------------------------
# 1. Load libraries
# -----------------------------

packages <- c(
  "readr", "psych", "tm", "wordcloud2", "dplyr", "sentimentr", "syuzhet",
  "stats", "corrplot", "RColorBrewer", "ggplot2", "openxlsx", "tidyverse",
  "scales", "randomForest", "text2vec", "textmineR", "wordcloud", "ldatuning",
  "data.table", "tidytext", "pacman", "textstem", "SnowballC", "udpipe",
  "quanteda", "regclass", "MASS", "questionr", "ipred", "caret", "survival",
  "fitdistrplus", "car", "ROCR", "rpart", "partykit", "gbm", "stargazer",
  "reshape2", "stopwords"
)

installed <- rownames(installed.packages())
for (pkg in packages) {
  if (!pkg %in% installed) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

# -----------------------------
# 2. Import data
# -----------------------------

data <- read_csv("data/campaign_data_short.csv")

summary(data)
str(data)

mean(data$clicks)
median(data$clicks)
describe(data$clicks)

# -----------------------------
# 3. Text preprocessing
# -----------------------------

corpus <- Corpus(VectorSource(data$Answer))

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)

cleanset <- tm_map(corpus, removeWords, stopwords("english"))

remove_url <- function(x) gsub("http[[:alnum:]]*", "", x)
cleanset <- tm_map(cleanset, content_transformer(remove_url))
cleanset <- tm_map(cleanset, stripWhitespace)

# Remove campaign-specific, brand-specific, and location-specific terms
patterns <- c(
  "danish", "cbm", "christoffel", "wwf", "drc", "scotland", "dansk",
  "denmark", "keyword", "per", "near", "unicef", "red", "cross",
  "franke", "juventute", "switzerland", "quarrier", "caritas", "swiss",
  "keywords", "scottish", "quarriers", "afghanistan", "moritz", "erskine",
  "yemen", "inverclyde", "africa", "chesa", "syria",
  "hotel chesa spuondas", "james shields project", "amazon", "glasgow",
  "spuondas", "even", "towards", "franc", "don", "edinburgh",
  "foremost", "afghan", "lanarkshire", "mozambique", "indonesia",
  "sulawesi", "albania", "dfh", "chf", "dy"
)

cleanset <- tm_map(cleanset, removeWords, patterns)

# Manual stemming / standardization
replacement_map <- list(
  c("helps", "help"),
  c("helped", "help"),
  c("saves", "save"),
  c("saved", "save"),
  c("impairments", "impairment"),
  c("dissabilitites", "dissability"),
  c("dissabilitite", "dissability"),
  c("dissabilities", "dissability"),
  c("persons", "person"),
  c("people", "person"),
  c("areas", "area"),
  c("tips", "tip"),
  c("amounts", "amount"),
  c("children", "child"),
  c("childrens", "child"),
  c("childs", "child"),
  c("cataracts", "cataract"),
  c("careers", "career"),
  c("teenagers", "teenager"),
  c("donations", "donation"),
  c("charities", "charity"),
  c("lives", "life"),
  c("sevices", "service"),
  c("refugees", "refugee"),
  c("activities", "activity"),
  c("years", "year"),
  c("injuries", "injury"),
  c("cards", "card"),
  c("aliens", "alien"),
  c("rations", "ration"),
  c("experienced", "experience"),
  c("experiences", "experience")
)

for (pair in replacement_map) {
  cleanset <- tm_map(cleanset, gsub, pattern = pair[1], replacement = pair[2])
}

# Remove slashes
cleanset <- tm_map(cleanset, gsub, pattern = "/", replacement = "")
cleanset <- tm_map(cleanset, content_transformer(function(x) gsub("\\\\", "", x)))

# -----------------------------
# 4. Tokenization and word frequencies
# -----------------------------

tdm <- TermDocumentMatrix(cleanset)
tdm_matrix <- as.matrix(tdm)

word_freq <- rowSums(tdm_matrix)
word_freq_sorted <- sort(word_freq, decreasing = TRUE)
word_freq_plot <- subset(word_freq_sorted, word_freq_sorted >= 25)

barplot(
  word_freq_plot,
  las = 2,
  col = rainbow(20),
  main = "Top Terms and Frequencies",
  ylab = "Frequency"
)

wordcloud_df <- data.frame(
  word = names(word_freq),
  freq = as.numeric(word_freq)
)

wordcloud2(
  wordcloud_df[wordcloud_df$freq > 5, ],
  size = 0.7,
  shape = "circle",
  rotateRatio = 0.5,
  minSize = 1
)

# -----------------------------
# 5. Sentiment and emotion features
# -----------------------------

data$syuzhet <- get_sentiment(data$Answer)
sentimentr_scores <- sentiment_by(data$Answer)
data <- cbind(data, sentimentr_scores)

data$afinn <- get_sentiment(data$Answer, method = "afinn")
data$bing <- get_sentiment(data$Answer, method = "bing")
nrc_scores <- get_nrc_sentiment(data$Answer)
data <- cbind(data, nrc_scores)

# Compare Syuzhet and SentimentR positive/negative classification
data$pos_syuzhet <- data$syuzhet > 0
data$pos_sentimentr <- data$ave_sentiment > 0
data$hit <- data$pos_sentimentr == data$pos_syuzhet

mean(data$hit) # In the assignment: 96.46%

# Distribution of emotions across campaigns
emotion_columns <- c(
  "anger", "anticipation", "disgust", "fear", "joy",
  "sadness", "surprise", "trust", "negative", "positive"
)

emotion_summary <- colSums(data[, emotion_columns], na.rm = TRUE)
emotion_df <- data.frame(
  emotion = names(emotion_summary),
  count = as.numeric(emotion_summary)
)

ggplot(emotion_df, aes(x = reorder(emotion, -count), y = count, fill = emotion)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Distribution of Emotions throughout Campaigns",
    subtitle = "Using NRC Lexicon",
    x = "Emotion",
    y = "Count"
  ) +
  scale_fill_brewer(palette = "Paired") +
  theme_light()

# Scatter plot: Syuzhet vs SentimentR
ggplot(data, aes(x = syuzhet, y = ave_sentiment)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Scatter Plot of Syuzhet vs. Average Sentiment",
    x = "Syuzhet",
    y = "Average Sentiment"
  ) +
  theme_minimal()

# Top words by Bing sentiment
bing_df <- data
bing_df$cleanset <- sapply(cleanset, as.character)

bing_word_count <- bing_df %>%
  unnest_tokens(output = word, input = cleanset) %>%
  inner_join(get_sentiments("bing"), by = c("word" = "word")) %>%
  count(word, sentiment, sort = TRUE)

bing_top_10 <- bing_word_count %>%
  group_by(sentiment) %>%
  slice_max(order_by = n, n = 10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n))

ggplot(bing_top_10, aes(x = word, y = n, fill = sentiment)) +
  geom_col() +
  facet_wrap(~ sentiment, scales = "free") +
  coord_flip() +
  labs(
    title = "Top 10 Words by Sentiment: Bing Lexicon",
    x = "Word",
    y = "Count"
  ) +
  theme_minimal()

# -----------------------------
# 6. Topic modelling with LDA
# -----------------------------

text <- sapply(cleanset, as.character)
tokens <- data.frame(
  ID = seq_along(text),
  text = text,
  stringsAsFactors = FALSE
)

dtm <- CreateDtm(
  doc_vec = tokens$text,
  doc_names = tokens$ID,
  ngram_window = c(1, 1),
  stopword_vec = c(
    stopwords::stopwords("en"),
    stopwords::stopwords(source = "smart")
  ),
  lower = TRUE,
  remove_punctuation = TRUE,
  remove_numbers = TRUE,
  verbose = FALSE
)

# Remove empty rows and columns
dtm <- dtm[, colSums(dtm) > 0]
dtm <- dtm[rowSums(dtm) > 0, ]

tf <- TermDocFreq(dtm = dtm)
original_tf <- tf[, c("term", "term_freq", "doc_freq")] %>%
  arrange(desc(term_freq))

head(original_tf, 10)

# Optional: determine optimal number of topics
optimal_topics <- FindTopicsNumber(
  dtm,
  topics = seq(from = 2, to = 8, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)

FindTopicsNumber_plot(optimal_topics)

# Fit LDA model with 5 topics
set.seed(222)

model_topics <- FitLdaModel(
  dtm = dtm,
  k = 5,
  iterations = 500,
  burnin = 180,
  alpha = 0.10,
  beta = 0.05,
  smooth = TRUE,
  optimize_alpha = TRUE,
  calc_likelihood = TRUE,
  calc_coherence = TRUE,
  calc_r2 = TRUE,
  cpus = 2
)

plot(model_topics$log_likelihood, type = "l")
model_topics$r2

top_terms <- GetTopTerms(phi = model_topics$phi, M = 20)
print(top_terms)

theta_num <- model_topics$theta
thetas <- as.data.frame(theta_num)
colnames(thetas) <- paste0("topic_", 1:5)

# Topic probability distributions
thetas_long <- reshape2::melt(thetas)

ggplot(thetas_long, aes(x = value, fill = variable)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ variable, scales = "free") +
  labs(
    title = "Distribution of Probabilities Across Topics",
    x = "Probability",
    y = "Density"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# -----------------------------
# 7. Regression analysis
# -----------------------------

keep_rows <- as.integer(rownames(dtm))
topic_data <- data.frame(
  ID = keep_rows,
  thetas
)

sentiment_data <- data %>%
  mutate(ID = row_number()) %>%
  select(
    ID, words, clicks, ave_sentiment, afinn, bing,
    anger, anticipation, disgust, fear, joy, sadness,
    surprise, trust
  )

analysis_df <- merge(topic_data, sentiment_data, by = "ID")

# Correlation matrix
corr_data <- analysis_df[, !names(analysis_df) %in% c("ID")]
corr_matrix <- cor(corr_data, use = "pairwise.complete.obs")

corrplot(
  corr_matrix,
  method = "shade",
  order = "FPC",
  type = "lower",
  diag = FALSE,
  tl.col = "black",
  cl.ratio = 0.2,
  tl.srt = 90,
  col = brewer.pal(6, "RdBu")
)

# Identify high correlations
high_correlation_indices <- which(
  abs(corr_matrix) > 0.7 & corr_matrix != 1,
  arr.ind = TRUE
)

correlated_pairs <- paste(
  rownames(corr_matrix)[high_correlation_indices[, 1]],
  colnames(corr_matrix)[high_correlation_indices[, 2]],
  sep = " - "
)

correlation_info <- data.frame(
  pair = correlated_pairs,
  correlation = corr_matrix[high_correlation_indices]
)

print(correlation_info)

# First full model
first_model <- lm(
  clicks ~ topic_1 + topic_2 + topic_3 + topic_4 + topic_5 +
    words + ave_sentiment + afinn + bing + anger + anticipation +
    disgust + fear + joy + sadness + surprise + trust,
  data = analysis_df
)

summary(first_model)

# Stepwise regression to handle multicollinearity and select variables
full_model <- lm(clicks ~ ., data = analysis_df[, !names(analysis_df) %in% c("ID")])

step_model <- stepAIC(
  full_model,
  direction = "both",
  trace = FALSE
)

summary(step_model)

stargazer(
  step_model,
  title = "Regression Results",
  no.space = FALSE,
  initial.zero = FALSE,
  notes.align = "l",
  notes = "",
  star.cutoffs = c(0.05, 0.01, 0.001),
  omit.stat = "aic",
  type = "text"
)

# -----------------------------
# 8. Interaction effects
# -----------------------------

interaction_model_sentiment <- lm(
  clicks ~ words + ave_sentiment + bing + anger + joy + sadness +
    topic_1 + topic_2 + topic_4 +
    topic_1:ave_sentiment + topic_2:ave_sentiment + topic_4:ave_sentiment,
  data = analysis_df
)

summary(interaction_model_sentiment)

interaction_model_words <- lm(
  clicks ~ words + ave_sentiment + bing + anger + joy + sadness + trust +
    topic_1 + topic_2 + topic_4 +
    topic_1:words + topic_2:words + topic_4:words,
  data = analysis_df
)

summary(interaction_model_words)

# -----------------------------
# 9. Non-linear effects
# -----------------------------

vars_to_check <- setdiff(names(analysis_df), c("ID", "clicks"))

for (var in vars_to_check) {
  formula_text <- paste0("clicks ~ ", var, " + I(", var, "^2)")
  nonlinear_model <- lm(as.formula(formula_text), data = analysis_df)
  quadratic_p_value <- anova(nonlinear_model, test = "Chisq")[2, 5]
  cat("Variable:", var, "- quadratic p-value:", quadratic_p_value, "\n")
}

# Visualize selected non-linear effects

ggplot(analysis_df, aes(x = words, y = clicks)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
  labs(
    title = "Non-linear Relationship: Clicks and Word Count",
    x = "Words",
    y = "Clicks"
  ) +
  theme_classic()

ggplot(analysis_df, aes(x = bing, y = clicks)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
  labs(
    title = "Non-linear Relationship: Clicks and Bing Sentiment",
    x = "Bing Sentiment Score",
    y = "Clicks"
  ) +
  theme_classic()

ggplot(analysis_df, aes(x = anger, y = clicks)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
  labs(
    title = "Non-linear Relationship: Clicks and Anger",
    x = "Anger NRC Score",
    y = "Clicks"
  ) +
  theme_classic()

ggplot(analysis_df, aes(x = sadness, y = clicks)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
  labs(
    title = "Non-linear Relationship: Clicks and Sadness",
    x = "Sadness NRC Score",
    y = "Clicks"
  ) +
  theme_classic()
