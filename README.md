# Online Advertising Effectiveness Analysis

## Table of Contents
- [Project Overview](#project-overview)
- [Data Sources](#data-sources)
- [Tools Used](#tools-used)
- [Data Cleaning/Preparation](#data-cleaningpreparation)
- [Exploratory Data Analysis](#exploratory-data-analysis)
- [Data Analysis](#data-analysis)
- [Results/Findings](#resultsfindings)
- [Recommendations](#recommendations)
- [Limitations](#limitations)

---

## Project Overview

This project analyzed the effectiveness of online advertising campaigns for charitable organizations, focusing on two digital marketing channels: paid search ads and email communication.

The first part of the project examined how ad copy characteristics, including word count, sentiment, emotions, and topics, influenced the number of clicks received by paid search ads. The second part analyzed an email campaign experiment to evaluate whether image type and personalized headers improved donor response, measured through open rates, click rates, and donation amounts.

The goal was to translate text, behavioral, and campaign performance data into actionable recommendations for improving online engagement and donation outcomes.

---

## Data Sources

- **Paid Search Ad Campaign Data**: A dataset containing 565 paid search ads, including ad text, word count, and number of clicks.
- **Email Campaign A/B Test Data**: A dataset containing email treatment groups, including image type, personalized header usage, donor characteristics, and response variables.
- **Text-Based Features**: Sentiment, emotion, and topic variables generated from campaign text.
- **Customer/Donor Variables**: Age, gender, past visits, past donations, and days since last interaction.
- **Outcome Variables**:
  - Paid search ads: clicks
  - Email communication: opens, clicks, and donation amount

---

## Tools Used

- **R** – Data cleaning, text mining, statistical analysis, and modelling
- **tidyverse / dplyr** – Data manipulation and summarization
- **tm / tidytext** – Text preprocessing and tokenization
- **sentimentr / syuzhet** – Sentiment analysis
- **NRC, Bing, and AFINN lexicons** – Emotion and sentiment feature generation
- **topicmodels / textmineR / ldatuning** – Topic modelling with LDA
- **ggplot2** – Data visualization
- **MASS / stats** – Regression modelling, ANOVA, t-tests, and proportion tests
- **MatchIt** – Propensity score matching for email campaign analysis

---

## Data Cleaning/Preparation

The preparation phase involved transforming raw ad copy and campaign data into analysis-ready datasets.

For the paid search ad analysis, the text data was cleaned and standardized by:

- Converting text to lowercase
- Removing punctuation, numbers, URLs, and stopwords
- Removing irrelevant brand, country, and organization-specific terms
- Standardizing similar words through manual stemming and replacements
- Creating a term-document matrix for text analysis
- Generating sentiment and emotion variables using multiple lexicons
- Applying LDA topic modelling to identify recurring themes across ads

For the email campaign analysis, the data preparation included:

- Checking the distribution of treatment groups
- Summarizing baseline variables such as past donations, past visits, and days since last interaction
- Conducting randomization checks across treatment groups
- Creating dummy variables for image type and personalized header treatments
- Preparing response variables for open rates, click rates, and donation behavior
- Creating propensity scores to adjust for differences between clickers, non-clickers, donors, and non-donors

---

## Exploratory Data Analysis

The exploratory analysis focused on understanding the structure of both campaign datasets and identifying early performance patterns.

For paid search ads, the analysis explored:

- Distribution of clicks
- Most frequent words across ad copy
- Word count patterns
- Sentiment and emotion distribution
- Correlations between clicks and text-based variables
- Topic distribution across ads

For email communication, the analysis explored:

- Open rates, click rates, and donation amounts by treatment group
- Differences between image types: person image, landscape image, and no image
- Differences between emails with and without personalized headers
- Baseline donor behavior across groups
- Past donation distributions by treatment condition

---

## Data Analysis

The project consisted of two main analyses.

### 1. Paid Search Ads Analysis

The paid search analysis evaluated how textual features influenced click performance.

Key methods included:

- Text preprocessing and tokenization
- Sentiment analysis using Syuzhet, SentimentR, Bing, AFINN, and NRC lexicons
- Comparison between sentiment classification methods
- LDA topic modelling to identify five main ad topics:
  - Mental health counseling
  - Personal development
  - Support for children and families
  - Refugee assistance
  - Nature conservation
- Correlation analysis between clicks and generated text features
- Regression modelling to identify significant predictors of clicks
- Stepwise regression to reduce multicollinearity
- Non-linear modelling to test quadratic effects
- Interaction testing between topics, sentiment, and word count

### 2. Email Campaign A/B Test Analysis

The email analysis evaluated whether campaign design choices improved donor response.

The experiment compared:

- Emails with a person image
- Emails with a landscape image
- Emails with no image
- Emails with and without personalized headers

Key methods included:

- Randomization checks
- Treatment group comparisons
- Proportion tests for open and click rates
- T-tests for donation differences
- ANOVA for donation differences across image groups
- Regression modelling with treatment variables and donor characteristics
- Propensity score matching to adjust for differences between clickers, non-clickers, donors, and non-donors

---

## Results/Findings

### Paid Search Ads

- The average number of clicks was 231.7, with a median of 233.8.
- Word count had a non-linear relationship with clicks, with performance increasing up to around 40 words before declining.
- Negative emotions, especially sadness and anger, were associated with higher click rates.
- Excessive anger appeared to reduce effectiveness after a certain point.
- Positive sentiment, especially joy, was associated with lower click performance.
- SentimentR and Bing sentiment scores had a negative relationship with clicks, suggesting that more positive ads did not necessarily generate more engagement.
- Topic modelling showed that ad topic/context influenced click performance.
- No meaningful interaction effects were found in the final interpretation.

### Email Campaign A/B Test

- Emails with images performed better than emails without images.
- Emails with a person image had higher open and click rates than landscape image emails.
- There was no significant difference in donation amount between person-image and landscape-image emails.
- Image-based emails generated higher donation amounts than emails without images.
- Personalized headers did not significantly improve open or click rates.
- Personalized headers were associated with higher average donation amounts.
- Regression models suggested that image type and gender were significant predictors of donation amount.
- After propensity score matching, image type and header remained relevant predictors of donation behavior.

---

## Recommendations

### Paid Search Ads

- Keep paid search ads concise, ideally below 40 words.
- Use emotionally engaging language, but avoid excessive negativity.
- Carefully include sadness-based emotional appeals, as they may increase user engagement.
- Use anger cautiously, since it may increase clicks only up to a certain level.
- Avoid relying too heavily on broadly positive emotional language, as joy and positive sentiment were linked to lower click performance.
- Combine text optimization with topic targeting to improve ad relevance and engagement.

### Email Communication

- Use images in email campaigns, as emails with images showed stronger open, click, and donation performance than emails without images.
- Prioritize person-focused images when the objective is to increase opens and clicks.
- Use personalized headers when the objective is to increase donation amount, even if they do not significantly affect opens or clicks.
- Segment donor communication where possible, since donor characteristics such as gender and past behavior influenced donation outcomes.
- Combine A/B testing with regression analysis to understand both treatment effects and donor-level differences.

---

## Limitations

- The paid search analysis was observational, so the findings show associations rather than causal effects.
- The paid search data did not represent a controlled A/B test.
- Some text preprocessing decisions, such as manual word removal and stemming, may have influenced the topic and sentiment results.
- Sentiment lexicons may not fully capture the emotional meaning of charitable advertising messages.
- The email analysis had stronger experimental logic, but some baseline variables, such as past donations, were not fully balanced across all groups.
- Donation behavior may have been influenced by factors not included in the dataset, such as campaign timing, donor motivation, or external events.
- The analysis was conducted in an academic setting and may not fully reflect a live marketing campaign environment.
