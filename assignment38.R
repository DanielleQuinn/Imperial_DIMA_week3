# ---- Prepare Your R Workspace ----
# Load Packages
library(readxl)
library(dplyr)

# Import data
data = read_excel("mindset_shampoo.xlsx")

# Look at first few rows of the data
head(data)

# ---- Part One: Analyzing Mindset Metrics ----

# ---- Potential ----
## Calculate the potential of awareness
# 1. Calculate the average awareness
mean(data$awareness)

# 2. Divide the average awareness by 100
## (this value is the "original awareness")
original_awareness = mean(data$awareness)/100
original_awareness

# 3. Subtract from 1 to find potential
potential_awareness = 1 - mean(data$awareness)/100
potential_awareness

## Calculate the potential of consideration
# 1. Calculate the average consideration
mean(data$consideration)

# 2. Divide the average consideration by 100
## (this value is the "original consideration")
original_consideration = mean(data$consideration)/100
original_consideration

# 3. Subtract from 1 to find potential
potential_consideration = 1 - mean(data$consideration)/100
potential_consideration

## Calculate the potential of liking
# 1. Calculate the average liking
mean(data$liking)

# 2. Divide the average liking by 7
## (this value is the "original liking")
original_liking = mean(data$liking)/7
original_liking

# 3. Subtract from 1 to find potential
potential_liking = 1 - mean(data$liking)/7
potential_liking

# ---- Stickiness ----
# Awareness
# 1. Build autoregressive model of awareness
ar(data$awareness)

# 2. Extract coefficients of model
ar(data$awareness)$ar

# 3. Find sum of coefficients to get stickiness
stickiness_awareness = sum(ar(data$awareness)$ar)
stickiness_awareness

# ---- Responsiveness ----
# 1. Create a variable of the lagged value of awareness
data$lag_awareness = dplyr::lag(data$awareness, default = 0)

# Example: Look at the first five values of awareness
data$awareness[1:5]

# Example: Look at the first five values of lag_awareness
data$lag_awareness[1:5]

# 2. Apply a log-linear model
lm(log(awareness + 1) ~ log(lag_awareness + 1) + log(price + 1) + log(promotion + 1) + log(advertising + 1), data = data)

# Interpretation: The resulting coefficient for log(advertising + 1) represents the responsiveness of awareness to advertising, and is equal to 0.05207.
responsiveness_awareness_advert = 0.05207

# Interpretation: The resulting coefficient for log(lag_awareness + 1) represents the carryover, and is equal to 0.04671
carryover_awareness = 0.04671

# ---- Conversion ----
# 1. Create a variable of the lagged value of sales (vol)
data$lag_sales = dplyr::lag(data$vol, default = 0)

# 2. Apply a log-linear model
lm(log(vol + 1) ~ log(lag_sales + 1) + log(awareness) + log(consideration) + log(liking), data = data)

# Interpretation: The resulting coefficient for log(awareness) represents the conversion of awareness, and is equal to 0.8010.
conversion_awareness = 0.8010

# ---- Appeal ----
# Calculate appeal of awareness
appeal_awareness = potential_awareness * 1/(1 - stickiness_awareness) * responsiveness_awareness_advert * conversion_awareness
appeal_awareness

# Now that we have calculated the original value, potential, stickiness, 
# responsiveness, conversion, and appeal of awareness, we would repeat this process 
# for consideration and liking, as well as calculating the original value and 
# responsiveness of our sales. For easy reference, you may want to consider adding the 
# calculated values to a table, like Table 1 in Assignment 3.7.

# ---- Part Two: Evaluating Marketing Strategies ----
# Original advertising level
original_advertising = 1

# New advertising level
new_advertising = 5

# --- New Value ---
new_awareness = original_awareness * (new_advertising/original_advertising) ^ responsiveness_awareness_advert
new_awareness

# ---- Short Run Gain ----
srg_awareness = new_awareness/original_awareness - 1
srg_awareness

# ---- Long Run Gain ----
lrg_awareness = srg_awareness/(1 - carryover_awareness)
lrg_awareness

# Long Run Gain as a Percent
lrg_awareness * 100

# ---- Resulting Conversion ----
new_conversion_awareness = lrg_awareness * conversion_awareness
new_conversion_awareness

# ---- Gains Attributed to Increases in Mindset Metrics ----
# The overall gains that can be attributed to increases in the mindset metrics 
# can be found by applying the steps above to each mindset metric 
# (awareness, consideration, and liking), calculating the resulting conversion of each, 
# and finding the sum of these values. As a reminder, this value can be multipled by 100 to be expressed as a percentage.
