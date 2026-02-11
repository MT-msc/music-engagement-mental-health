# Exploring the Impact of Music Engagement on Mental Health Outcomes

## About
This project examines the relationship between music consumption habits and mental health outcomes (anxiety, depression, insomnia, and OCD). It analyzes survey data to explore how factors like music genre preference, listening frequency, age, and musical background relate to self-reported mental health scores.

## Files
- `Project Report.pdf` — Full written report with analysis and discussion
- `Music-mental-health.md` — Rendered version of the analysis from RStudio (viewable on GitHub)
- `Music-mental-health_files/` — Folder containing plot images
- `mxmh_survey_results.csv/` — Folder containing plot images

## Methods
- Data cleaning and recoding
- Exploratory data analysis with heatmap visualization
- Multiple linear regression for each mental health outcome
- Model comparison using AIC (full vs. reduced models)
- Residual diagnostics (Shapiro-Wilk test, Breusch-Pagan test)
- Log transformation to address normality issues

## Key Findings
- Folk, Pop, and Rock genres were significantly associated with higher anxiety scores
- Age was a significant predictor across multiple mental health conditions (negative relationship)
- Daily listening hours were significantly linked to depression, insomnia, and OCD
- Residual normality assumptions were not fully met, suggesting the need for more advanced methods

## Dataset
[Music & Mental Health Survey Results](https://www.kaggle.com/datasets/catherinerasgaitis/mxmh-survey-results) from Kaggle (736 responses)
