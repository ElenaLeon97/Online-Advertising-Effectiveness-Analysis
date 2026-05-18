library(ggplot2)
library(dplyr)
library(tidyr)
##set working directory
setwd (D:/AAA Studie/DMI/Week 5. Content and 
materials.html/assingment 2)
# Replace the file path with the correct path to your dataset
d <- read.csv(AB_campaign.csv)
# Check the treatment groups distribution
table(d$group)
# Summarize responses
summary(d[,c(open, click, donate)])
# Summarize baseline variables
summary(d[,c(days_since, past_visits, past_donations)]) # Use the 
correct column names from your dataset
#Histogram of past donations by treatment group wihtout outliers
d %>%
  filter(past_donations > 0, past_donations <= 1500) %>% # Exclude 
donations over $1500
  ggplot(aes(x=past_donations, fill=group)) + 
  geom_histogram(binwidth = 25, alpha=0.2, position=identity) +
  xlim(0, max(d$past_donations[d$past_donations <= 1500], na.rm = 
TRUE)) + # Adjust xlim to exclude >3000
  xlab(Past Donations ($)) + ylab(Customers) + 
  labs(title=Distribution of past donations by treatment group 
without outliers above $1500)
# Randomization checks
d %>% group_by(group) %>% summarize(mean(days_since), 
mean(past_visits), mean(past_donations))
# Treatment effects analysis
d %>% group_by(group) %>% summarize(mean(open), 
mean(click), mean(donate))
# Comparison of open rates between treatment types of images
d_treat <- d[d$group != no_image,] 
xtabs(~ group + open, data=d_treat)

# Confirm significance with proportions test
prop.test(xtabs(~ group + open, data=d_treat)[,2:1])
# Visualization: open rates for treatment types of images
mosaicplot(xtabs(~ group + open, data=d_treat), 
           main=Email Opens for different groups)
# Comparison of click rates between treatment types of images
xtabs(~ group + click, data=d_treat)
# Confirm significance with proportions test
prop.test(xtabs(~ group + click, data=d_treat)[,2:1])
# Visualization: barplot of clicks and opens for treatment groups 
excluding no_image
d %>%
  filter(group != no_image) %>%
  group_by(group) %>%
  summarize(open = mean(open), click = mean(click)) %>%
  gather(response, mean, -group) %>%
  ggplot(aes(fill = response, y = mean, x = group)) + 
  geom_bar(position = dodge, stat = identity) + 
  ylab(Response Rate) + xlab(Treatment Group)
# Visualization: barplot of clicks and opens
d %>%
  group_by(group) %>%
  summarize(open = mean(open), click = mean(click)) %>%
  gather(response, mean, -group) %>%
  ggplot(aes(fill = response, y = mean, x = group)) + 
  geom_bar(position = dodge, stat = identity) + 
  ylab(Response Rate) + xlab(Treatment Group)
# Visualization: barplot of donations
d %>%
  group_by(group) %>%
  summarize(mean_donate = mean(donate)) %>%
  ggplot(aes(x = group, y = mean_donate, fill = group)) +
  geom_bar(stat = identity) +
  labs(title = Average Donations by Treatment Group,
       x = Treatment Group,
       y = Average Donation Amount ($)) +
  theme_minimal()
# Perform ANOVA test
anova_result <- aov(donate ~ group, data = d)
# Check the summary of the ANOVA test
summary(anova_result)

# Test significance with a t-test
t.test(donate ~ group, data=d_treat)
t.test(click ~ group, data=d_treat)
t.test(open ~ group, data=d_treat)
# Sample size calculator
sd(d$donate)
power.t.test(sd=sd(d$donate), delta=1, sig.level=0.95, power=0.80)
# LETS DO THINGS FOR THE HEADER
# Compare the past donations in each group
d %>% group_by(header) %>% summarize(mean(donate))
# Check if the proportion of people who have donated in the past is 
similar across groups
d %>% group_by(header) %>% 
summarize(mean(past_donations))
# Histogram of past donations by treatment group without outliers
d %>%
  filter(past_donations > 0, past_donations <= 1500) %>% 
  ggplot(aes(x = past_donations, fill = as.factor(header))) + 
  geom_histogram(binwidth = 25, alpha = 0.2, position = identity) +
  xlim(0, max(d$past_donations[d$past_donations <= 1500], na.rm = 
TRUE)) +
  xlab(Past Donations ($)) + ylab(Customers) + 
  labs(title = Distribution of past donations by header type without 
outliers above $1500)
# Treatment effects analysis
d %>% group_by(header) %>% summarize(mean(open), 
mean(click), mean(donate))
# Comparison of open rates between emails with and without 
personalized header
xtabs(~ header + open, data = d)
# Confirm significance with proportions test
prop.test(xtabs(~ header + open, data = d)[,2:1])
# Comparison of click rates between emails with and without 
personalized header
xtabs(~ header + click, data = d)

# Confirm significance with proportions test
prop.test(xtabs(~ header + click, data = d)[,2:1])
# Visualization: barplot of clicks and opens for emails with and without 
personalized header
d %>% group_by(header) %>% 
  summarize(open = mean(open), click = mean(click)) %>%
  gather(response, mean, -header) %>%
  ggplot(aes(fill = response, y = mean, x = as.factor(header))) + 
  geom_bar(position = dodge, stat = identity) + 
  ylab(Response Rate) + xlab(Header Type)
# Visualization: barplot of donations for emails with and without 
personalized header
d %>% 
  group_by(header) %>% 
  summarize(mean_donate = mean(donate)) %>%
  ggplot(aes(x = as.factor(header), y = mean_donate, fill = 
as.factor(header))) +
  geom_bar(stat = identity) +
  labs(title = Average Donation Amount by Header Type,
       x = Header Type,
       y = Average Donation Amount ($)) +
  theme_minimal()
# Test significance with a t-test
t.test(donate ~ header, data = d)
#LETS DO A REGRESSION MODEL WIHTOUT DOING PRPENSITY 
SCORE MATCHING
# Load required libraries
install.packages(fastDummies)
library(dplyr)
library(tidyr)
library(fastDummies)
# Create dummy variables for the 'group' and 'header' variables
d_with_dummies <- dummy_cols(d, select_columns = c(group, 
header))
# Specify the regression model including donor characteristics
model <- glm(donate ~ group_image_landscape + 
group_image_person + header +
                group_image_landscape:header + 
group_image_person:header +
                age + gender +
                group_image_landscape:age + group_image_person:age +
                header:age + header:gender,
             data = d_with_dummies, family = gaussian)
# Summarize the model
summary(model)

# Load required libraries
library(MatchIt)
library(dplyr)
library(fastDummies)
# Create dummy variables for 'group' and 'header'
d_with_dummies <- dummy_cols(d, select_columns = c(group, 
header))
# Step 1: Create Propensity Score Models
# For clicking on emails
click_model <- glm(click ~ group + header + image + open + 
past_donations + days_since + past_visits + age + gender, data = 
d_with_dummies, family = binomial)
# For making a donation
donate_model <- glm(donate > 0 ~ group + header + image + open 
+ past_donations + days_since + past_visits + age + gender, data 
= d_with_dummies, family = binomial)
# Extract propensity scores
d_with_dummies$click_pscore <- predict(click_model, type = 
response)
d_with_dummies$donate_pscore <- predict(donate_model, type = 
response)
# Step 2: Perform Propensity Score Matching
# Matching for clicking on emails
click_match <- matchit(click ~ click_pscore, data = d_with_dummies, 
method = nearest)
# Matching for making a donation
donate_match <- matchit(donate > 0 ~ donate_pscore, data = 
d_with_dummies, method = nearest)
# Check covariate balance before and after matching
summary(click_match)
summary(donate_match)
# Step 3: Analyze the Effects of Image Type and Personalized Header 
on Donation Amount
# After matching for clicking on emails
click_matched_data <- match.data(click_match)
click_lm <- lm(donate ~ group_image_landscape + 
group_image_person + group_no_image + header + age + gender, 
               data = click_matched_data)
summary(click_lm)
# After matching for making a donation
donate_matched_data <- match.data(donate_match)
donate_lm <- lm(donate ~ group_image_landscape + 
group_image_person + group_no_image + header + age + gender,  
data = donate_matched_data)
summary(donate_lm)

