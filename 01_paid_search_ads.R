# Clean text ----
## Standardization to lowercase characters
corpus <- tm_map(corpus, tolower)
inspect(corpus[1:5])
## Remove punctuation
corpus <- tm_map(corpus, removePunctuation)
inspect(corpus[1:5])
## Number removal
corpus <- tm_map(corpus, removeNumbers)
inspect(corpus[1:5])
## Stopword removal
cleanset <- tm_map(corpus, removeWords, stopwords('english'))
inspect(cleanset[1:5])
## URL removal
removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:10])
## Whitespace removal
cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:5])
inspect(cleanset[561:565])
## After looking at the bar plot from the tokenization step, we remove more extra words
patterns <- c("danish", "cbm", "christoffel", "wwf", "drc", "scotland", "dansk", "denmark", 
"keyword", "per", "near","cbm", "unicef", "red", "cross", "franke", "juventute", "switzerland", 
"quarrier", "caritas", "swiss", "keywords", "scottish", "quarriers", "afghanistan", "moritz", "erskine", 
"yemen", "inverclyde", "africa", "chesa", "syria", "hotel chesa spuondas", "james shields project", 
"amazon", "glasgow", "spuondas", "Franke","even", "towards", "franc", "don", 
"edinburgh","foremost","afghan", "lanarkshire", "mozambique","indonesia", "sulawesi", "albania", 
"dfh","chf","dy")
cleanset <- tm_map(cleanset, removeWords, patterns)
inspect(cleanset[1:10])
# Libraries
```{r}
# install/load libraries
librarian::shelf(readr,psych,tm,wordcloud2,dplyr,sentimentr,syuzhet,stats,corrplot,RColorBr
ewer,ggplot2,openxlsx,tidyverse,scales,randomForest,text2vec,textmineR,wordcloud,ldatu
ning,data.table,tidytext,ldatuning,pacman,textstem,SnowballC,udpipe, quanteda, regclass, 
MASS, questionr, ipred, caret, survival, fitdistrplus, car, ROCR, rpart, partykit, gbm, 
stargazer)
```
# Data Pre-processing
```{r Import data}
# Import data
data <- read_csv("campaign_data_short.csv")
# First look into the data
summary (data)
str(data)
## Descriptives of clicks variable
mean(data$clicks)
median(data$clicks)
# mean : 231.7
# median: 233.8
# Build corpus
corpus <- Corpus(VectorSource(data$Answer))
inspect(corpus[1:5])
```
```{r Pre-processing}
###########################
### Data Pre-processing ###
###########################

# Create a wordcloud
w <- rowSums(tdm)
w <- data.frame(names(w), w)
colnames(w) <- c('word', 'freq')
wordcloud2(w[which(w$freq > 5),],
           size = 0.7,
           shape = 'circle',
           rotateRatio = 0.5,
           minSize = 1)
```
# Variable exploration
```{r Lexicons}
# Question 5 ----
# Create textual features based on three different off-the-shelf lexicons. How well do these 
perform?
###########################
######## Lexicons #########
###########################
# Compare Methods: syuzhet & sentimentr ----
data$syuzhet <- get_sentiment(data$Answer) # use syuzhet library for classification
temp <- sentiment_by(data$Answer) # use sentimentr library for classification
data <- cbind(data, sentiment_by(data$Answer))
## classify into positive or not
data$pos_syuzhet <- data$syuzhet > 0
data$pos_sentimentr <- data$ave_sentiment > 0
table(data$pos_syuzhet)
table(data$pos_sentimentr)
## let's look at the misclassified texts
data[which(data$pos_sentimentr == 0 & data$pos_syuzhet == 1),1]
## hit rates
data$hit <- (data$pos_sentimentr == data$pos_syuzhet) 
mean(data$hit)
# 96.46
### Some manual corrections.
cleanset <- tm_map(cleanset, gsub, pattern = "dissabilitite", replacement = "dissability")
inspect(cleanset[1:10])
cleanset <- tm_map(cleanset, gsub, pattern = "childrens", replacement = "child")
inspect(cleanset[1:10])
## Remove "/" and "\".
cleanset <- tm_map(cleanset, gsub, pattern = "/", replacement = "")
inspect(cleanset[1:10])
cleanset <- tm_map(cleanset, content_transformer(function(x) gsub("\\\\", "", x)))
inspect(cleanset[1:10])
```
```{r Tokenization}
##########################
#### 3. Tokenization #####
##########################
tdm <- TermDocumentMatrix(cleanset)
tdm
tdm <- as.matrix(tdm)
tdm[1:10, 1:10]
# Bar plot
w <- rowSums(tdm)
w <- sort(w, decreasing = TRUE)
w <- subset(w, w>=25)
barplot(w, las = 2, col = rainbow(20)) #las determines the orientation, 2 = perpendicular to 
the axis, you can also check the documentation
# Through the bar plot, we already identify some words that need to be excluded or 
stemmed/lemmarized: danish, wwf,cbm, drc, scotland, etc.

  theme_minimal() +
  theme(
    plot.title = element_text(color = "black", size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(face = "italic")
  )
# Distribution of Emotions throughout Campaigns (NRC) ----
## Create a new data set
sentiment.df <- data %>%
  mutate(syuzhet = get_sentiment(Answer)) %>% #syuzhet default method
    mutate(sentimentr = sentiment_by(Answer)$ave_sentiment) %>% #sentimentr method; note: 
we only save column 4 (avg_sentiment)
      mutate(afinn = get_sentiment(Answer, method = "afinn")) %>% #afinn method
        mutate(bing = get_sentiment(Answer, method = "bing")) %>% #bing method
          mutate(get_nrc_sentiment(Answer)) #nrc method
## Organise the data for plotting
emo_bar<- colSums(sentiment.df[,c(15:24)])
emo_sum<- data.frame(count=emo_bar,emotions=names(emo_bar))
## Plot the results
ggplot(emo_sum, aes(x = reorder(emotions, -count), y = count, fill = emotions)) +
  geom_bar(stat = 'identity') +
  labs(
    title = "Distribution of Emotions throughout Campaigns",
    subtitle = "Using NRC Lexicon.",
    #caption = "Source: Gapminder dataset",
    x = "Emotion",
    y = "Campaigns"
  ) +
  theme(
    plot.title = element_text(color = "black", size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(face = "italic")
  ) +
  scale_fill_brewer(palette = 'Paired') +
  theme_light()
```
## Plot the results
ggplot(data, aes(x = syuzhet, y = ave_sentiment)) +
  geom_point() +  # Add points for each data point
  geom_smooth(method = "lm", se = FALSE, color="red") +  # Add smoothed line
  labs(x = "Syuzhet", y = "Average Sentiment") +  # Label x and y axes
  ggtitle("Scatter Plot of Syuzhet vs. Average Sentiment") +
  theme_minimal() 
# Top 10 Words by Sentiment (Bing) ----
## Create fresh df for this visualization: bing.df
bing.df <- data
bing.df$cleanset<- sapply(cleanset, as.character)
bing_word_count <- bing.df %>%
  unnest_tokens(output = word, input = cleanset) %>%
  inner_join(get_sentiments('bing'), by = c('word' = 'word')) %>%
  count(word, sentiment, sort = TRUE)
## Select top 10 words by sentiment
bing_top_10 <- bing_word_count %>%
  group_by(sentiment) %>%
  slice_max(order_by = n, n = 10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n))
## Create a bar plot
ggplot(bing_top_10, aes(x = word, y = n, fill = sentiment)) +
  geom_col() +
  facet_wrap(~ sentiment, scales = "free") +
  coord_flip() +
  labs(title = "Top 10 Words by Sentiment (Bing Lexicon)",
       x = "Word",
       y = "Count") +
  scale_fill_manual(values = c("positive" = "darkmagenta", "negative" = "darkred", "neutral" 
= "gray")) +

dtm <- dtm[,colSums(dtm) > 0]
dtm <- dtm[rowSums(dtm) > 0,]
# Explore the basic frequency. ---- 
tf <- TermDocFreq(dtm = dtm)
original_tf <- tf [,c(1:3)]
rownames(original_tf) <- 1:nrow(original_tf)
original_tf <- original_tf %>%
  arrange(desc(term_freq))
head(original_tf, 10)
#     term       term_freq doc_freq
# 1   support        334     220
# 2   refugee        231     132
# 3   child          189     117
# 4   assistance      98      85
# 5   job             98      54
# 6   service         83      75
# 7   life            76      66
# 8   give            74      58
# 9   family          71      44
# 10  care            70      65
## Visualization of frequencies.
### Create a data frame with the top ten words and their frequencies
top_words <- data.frame(
  term = c("support", "refugee", "child", "assistance", "job", "service", "life", "give", "family", 
"care"),
  term_freq = c(334, 231, 189, 98, 98, 83, 76, 74, 71, 70)
)
top_words <- top_words[order(top_words$term_freq, decreasing = TRUE), ]
### Assign different colors to each word
colors <- brewer.pal(10,"Set3")
ggplot(top_words, aes(x = term_freq, y = reorder(term, term_freq), fill = term)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = term_freq), hjust = -0.2, size = 3, color = "black") +  # Add geom_text to 
show frequencies
  scale_fill_manual(values = colors) +
```{r Topic Modeling}
# Question 6. ----
# Conduct topic modeling via LDA. Determine the optimal number of topics, explore, 
interpret, and name the topics and generate a list with the top 20 words per topic.
##########################
####  Topic Modeling  ####
##########################
# Prepare Data ----
## Extract already pre-processed text data from the corpus.
text <- sapply(cleanset, as.character)
tokens <- data.frame(ID = 1:length(text), text = text, stringsAsFactors = FALSE)
## Have a look at the data
str(data)
## Create a document term matrix and remove sparse terms.
dtm <- CreateDtm(doc_vec = tokens$text, # character vector of documents
                 doc_names = tokens$ID, # document names
                 ngram_window = c(1, 1), # minimum and maximum n-gram length
                 stopword_vec = c(stopwords::stopwords("en"), # stopwords from tm
                                  stopwords::stopwords(source = "smart")), # this is the default value
                 lower = TRUE, # lowercase - this is the default value
                 remove_punctuation = TRUE, # punctuation - this is the default
                 remove_numbers = TRUE, # numbers - this is the default
                 verbose = FALSE, # Turn off status bar for this demo
                 ) 
## Keep row number with >0 for later.
rowsum = rowSums(dtm)
ID = data$ID
keep_row_id = as.data.frame(cbind(rowsum, ID))
keep_row_id = keep_row_id[rowsum>0,]

model_topics <- FitLdaModel(dtm = dtm, 
                     k = 5,   #number of topics
                     iterations = 500, # I usually recommend at least 500 iterations
                     burnin = 180,
                     alpha = 0.10,  #Original=0.10 High alpha => each doc has more topics
                     beta = 0.05,   #Original=0.05 High beta => topic contains many words
                     smooth = TRUE,
                     optimize_alpha = TRUE,
                     calc_likelihood = TRUE,
                     calc_coherence = TRUE,
                     calc_r2 = TRUE,
                     cpus = 2) 
model_topics$r2
plot(model_topics$log_likelihood, type = "l")
# Get the top terms of each topic ----
model_topics$top_terms <- GetTopTerms(phi = model_topics$phi, M = 20)
model_topics[["top_terms"]]
# Probabilities ----
## Estimate the probabilities for each unit.
theta_num <- model_topics$theta   # these are the "scores" for each row on each topic.
thetas <- as.data.frame(theta_num)
# Visualization of probability distribution ----
## New data set for visualization purposes.
vis.df <- thetas
colnames(vis.df) <- c("Topic 1", "Topic 2", "Topic 3", "Topic 4", "Topic 5")
## Long format for easier visualization.
thetas_long <- melt(vis.df)
# Create a density plot for each topic
ggplot(thetas_long, aes(x = value, fill = variable)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Distribution of Probabilities Across Topics",
       x = "Probability",
       y = "Density") +
  labs(title = "Top Ten Words and Their Frequencies",
       x = "Frequency",  # Changed x-axis label
       y = "Words") +    # Changed y-axis label
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10, face = "bold"),  # Adjusted axis.text.y
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 12, face = "bold")) + # Adjusted axis.title.y
  guides(fill = FALSE)
## Eliminate words appearing less than 2 times or in more than half of the documents
vocabulary <- tf$term[ tf$term_freq > 2 & tf$doc_freq < nrow(dtm) / 2 ]
dtm = dtm
# Optimal Topic Number ----
# Griffiths2004: based on the coherence of the most probable words in the topics.
# CaoJuan2009: based on the expected log-likelihood of the model.
# Deveaud2014: based on the exclusivity and prevalence of the top words in the topics.
# Arun2010: based on the expected log-likelihood and the coherence of the topics.
optimal1 <- FindTopicsNumber(
  dtm,
  topics = seq(from = 2, to = 8, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)
FindTopicsNumber_plot(optimal1)
# Ideal number is 5.
# LDA models ---
## Run LDA with 5 classes
set.seed(222)

print(correlation_info)
# Extremely high correlation between the 2 sentiments (sentimentr - syuzhet: 0.8388688). So we 
opt for keeping sentimentR, because, unlike syuzhet, it attempts to take into account valence 
shifters (i.e., negators, amplifiers (intensifiers), de-amplifiers (downtoners), and adversative 
conjunctions) while maintaining speed. Also including Bing, Afinn, NCR suffices. 
analysis.df <- analysis.df [,-9]
corr_data <- analysis.df[,-1]
corr_matrix <- cor(corr_data)
## Visualize the correlations.
corrplot(corr_matrix, method = 'shade', order = 'FPC', type = 'lower', 
         diag = FALSE, tl.col = 'black',
         cl.ratio = 0.2, tl.srt = 90, col = brewer.pal(6,"RdBu"))
# First Model ----
firstmodel <- lm(clicks ~ topic_1 + topic_2 + topic_3 + topic_4 + 
                 topic_5 + words + ave_sentiment + afinn + bing + anger +
                 anticipation + disgust + fear + joy + sadness + surprise + 
                 trust, 
                 data = analysis.df)
summary(firstmodel) 
# we do NOT interpret this model, it has problems. Which we address next.
# prob_t4            NA         NA      NA       NA
# Indications of multicollinearity.
# Multicollinearity ----
VIF(firstmodel)
# We get an error: there are aliased coefficients in the model
# We need to drop coefficients.
# Use Stepwise regression to determine the necessary variables ----
full.model <- lm(clicks ~., data = analysis.df[,-1])
step.model <- stepAIC(full.model, direction = "both", trace = FALSE)
summary(step.model)
# New model : lm(formula = clicks ~ words + sentimentr + bing + anger + joy + 
#                sadness + trust, data = analysis.df[, -1])
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 2))
```
```{r}
# Questions 9 ----
# Include your text-based variables in different regression models. What do you learn?
# Create a df for our analysis' purposes.----
topics <- as.data.frame(cbind(keep_row_id, thetas))
temp <- as.data.frame(cbind(ID, sentiment.df[,c(3:5,7,9,12:23)])) # chose word count, 
clicks and sentiments.
analysis.df <- merge (topics, temp, by = "ID")
analysis.df <- analysis.df %>%
  rename(topic_1 = t_1,
         topic_2 = t_2,
         topic_3 = t_3,
         topic_4 = t_4,
         topic_5 = t_5
         )
analysis.df <- analysis.df [,-c(2,11,13,14)]
# Correlation ----
corr_data <- analysis.df[,-1]
corr_matrix <- cor(corr_data)
## Identify correlations higher than |0.7| threshold
high_correlation_indices <- which(abs(corr_matrix) > 0.7 & corr_matrix != 1, arr.ind = 
TRUE)
correlated_pairs <- paste(rownames(corr_matrix)[high_correlation_indices[, 1]], 
                          colnames(corr_matrix)[high_correlation_indices[, 2]], 
                          sep = " - ")
correlation_info <- data.frame(Pair = correlated_pairs, Correlation = 
corr_matrix[high_correlation_indices])

    cat("Analyzing:",var,'\n')
      lmnonlintest <- 
lm(as.formula(paste(analysis.df['clicks'],'~',analysis.df[var],'+',I(analysis.df[var]^2))), data = 
analysis.df)
      # Test for significance of quadratic term using ANOVA
      quadraticterm_pvalue <- anova(lmnonlintest, test = "Chisq")[2,5]
      print(quadraticterm_pvalue)
    } else {
      cat("Cant analyze clicks or id")
    }
  }
})
# We find non-linear effects in word count, bing, anger, sadness.
# Visualization ----
## words
ggplot(analysis.df, aes(x = words, y = clicks)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
  labs(
    title = "Non-linear Relationship: Clicks ~ Word Count",
    x = "Words",
    y = "Clicks"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(color = "black", size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(face = "italic")
  )
## bing
ggplot(analysis.df, aes(x = bing, y = clicks)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
  labs(
    title = "Non-linear Relationship: Clicks ~ Bing",
    x = "Bing Sentiment Score",
    y = "Clicks"
  ) +
## Report results.
stargazer(step.model,
          title = "Regression Results",
          no.space = F,
          initial.zero = F,
          notes.align = "l",
          notes = "",
          star.cutoffs = c(.05, .01, .001),
          omit.stat = "aic",
          type = "text")
# To inspect the topics a bit, we include interraction effects: ----
## With sentiments:
inter.model1 <-  lm(formula = clicks ~ words + ave_sentiment + bing + anger + joy + 
                            sadness + topic_1 + topic_2 + topic_4 + topic_1 * ave_sentiment + 
topic_2 * ave_sentiment + topic_4 * ave_sentiment, data = analysis.df[, -1])
summary(inter.model1)
## With words:
inter.model2 <-  lm(formula = clicks ~ words + ave_sentiment + bing + anger + joy + 
                            sadness + trust + topic_1 + topic_2 + topic_4 + topic_1 * words + 
topic_2 * words + topic_4 * words, data = analysis.df[, -1])
summary(inter.model2)
# The relationship between the number of words and the response variable (clicks) varies 
depending on the topic 1.
```
```{r}
# Question 10. ----
# Do you find any non-linear effects (quadratic, interactions)? Please visualize them.
# Look for non-linear relationships in all of the variables.
varstocheck <- names(analysis.df[,c(-1)])
suppressWarnings({ # surpressing warnings since they are not applicable
  for (var in varstocheck) {
    if (var != "clicks" && var != "id") {

   y = "Clicks"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(color = "black", size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(face = "italic")
  )
# Findings: Clicks rise exponentially as sad sentiment rises.
```
  theme_classic() +
  theme(
    plot.title = element_text(color = "black", size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(face = "italic")
  )
# Findings: As sentiment goes up clicks fall up to 3, then they start slightly rising.
## anger
ggplot(analysis.df, aes(x = anger, y = clicks)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
  labs(
    title = "Non-linear Relationship: Clicks ~ Anger",
    x = "Anger NRC Score",
    y = "Clicks"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(color = "black", size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(face = "italic")
  )
# Findings: Angry sentiment's clicks peak at 3 then fall.
## sadness
ggplot(analysis.df, aes(x = sadness, y = clicks)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
  labs(
    title = "Non-linear Relationship: Clicks ~ Sadness",
    x = "Sadness NRC Score",

