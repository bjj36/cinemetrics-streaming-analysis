# Cinemetrics Streaming Data Analysis
Exploratory data analysis and text mining on a fictional streaming dataset using R and tidytext. Completed as assessed coursework for MA22019 Introduction to Data Science at the University of Bath (achieved 78%). The datasets used in this project are entirely fictional and were provided as part of the coursework brief. The streaming platform, movies, and user data are all synthetic.

## Overview
This project analyses a fictional streaming platform dataset for a service called CineMetrics, working across three linked datasets covering movie metadata, user information, and streaming history. The analysis covers data wrangling, exploratory data analysis, and text mining to derive insights about user engagement and content performance.

## Key Techniques
- Data joining, deduplication, and missing value handling.  
- Factor collapsing and genre standardisation using the forcats package.  
- Distribution visualisation with faceted boxplots.  
- Simpson's Paradox identification through subscription and age-tier segmentation.  
- Sentiment analysis using the Bing lexicon.  
- TF-IDF text mining to identify genre-distinctive vocabulary.  

## Structure
coursework_01.qmd — Main analysis file containing all code and written interpretations.  
coursework_01.md — Rendered markdown output.  
coursework_01_files/ — Generated plots and figures.  
data/ — Fictional datasets provided for the coursework (titles, users, streaming history).  

## Requirements
The following R packages are required to run the analysis:
- tidyverse  
- tidytext  
- knitr  
- kableExtra  
