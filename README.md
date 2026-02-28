# emotional-influences-of-true-crime

Understanding Media Consumption Patterns
Analyzed and Designed by Dhruvi Mehta

**Project Overview**

This project examines whether emotional factors influence true crime media consumption in the United States. Specifically, it analyzes the relationship between fear (paranoia), curiosity, and engagement with true crime television and podcasts.
The study applies statistical modeling to determine whether these emotional variables predict differences in media format preference and consumption frequency.

**Research Question**

What is the relationship between fear, curiosity, and the consumption of true crime television and podcasts among U.S. audiences?

**Hypotheses**
Null Hypothesis (H₀)

There is no statistically significant relationship between fear, curiosity, and true crime media consumption.

Alternative Hypothesis (H₁)

There is a statistically significant relationship between emotional factors (fear and curiosity) and true crime media consumption.

**Methodology**
The study uses a quantitative research design.

Statistical Methods
Logistic regression to model the likelihood of TV and podcast consumption

Chi-square tests to assess associations between categorical variables

Spearman’s rank correlation to measure monotonic relationships between ordinal variables

Predicted probability modeling for visualization

Correlation heatmap for selected numeric predictors

Missing values were removed using drop_na() for relevant variables.

**Key Findings**

TV Consumption

Fear significantly predicts TV consumption. Higher fear levels are associated with increased likelihood of watching true crime television. Curiosity shows a weaker and non-significant effect.

Podcast Consumption

Curiosity is more strongly associated with podcast consumption. Fear is not a significant predictor in the logistic model. Correlation tests indicate weak relationships overall.

Emotional Relationships

Fear and curiosity are positively correlated. Associations between emotional variables and frequency of consumption are modest.

Overall, emotional factors influence media engagement, though effects vary by format.

**Visualizations**

Predicted probability plots for TV and podcast consumption

Probability curves by fear and curiosity levels

Correlation heatmap of numeric predictors

Frequency-based gradient visualizations

**Tools**

R

dplyr

tidyverse

ggplot2

corrplot

reshape2

car

patchwork
