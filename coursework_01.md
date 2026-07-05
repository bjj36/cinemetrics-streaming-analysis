# Coursework 1: MA22019, Introduction to Data Science
bjj36

<script src="coursework_01_files/libs/kePrint-0.0.1/kePrint.js"></script>
<link href="coursework_01_files/libs/lightable-0.0.1/lightable.css" rel="stylesheet" />

<!--- DO NOT DELETE THIS LINE --->

<!--- METADATA ANCHOR: MA22019-CW1-2026 --->

Please read the `coursework_01_instructions.pdf` for the full context,
data dictionary, and detailed task requirements before starting.

``` r
library(tidyverse)
library(tidytext)
library(knitr)

theme_set(theme_minimal() + theme(axis.line=element_line(color = "black")))
```

## Part A: Data Wrangling & EDA

### Task 1: Data Joining

<!--- DO NOT DELETE THIS LINE - TASK 1 ANCHOR --->

``` r
history_raw <- read.csv("data/history.csv")
titles_raw <- read.csv("data/titles.csv")
users_raw <- read.csv("data/users.csv")

history_clean <- distinct(history_raw)
titles_clean <- distinct(titles_raw)
users_clean <- distinct(users_raw)

cinemetrics <- history_clean %>%
  left_join(users_clean, by = "user_id") %>%
  left_join(titles_clean, by = "movie_id")
```

<!--- WRITE YOUR WRITTEN ANSWER BELOW THIS LINE --->

#### Written Interpretation

Duplicates had to be removed so that the same watch event recorded in
the history data frame were not counted multiple times when joining with
other data frames as it would artificially increase the number of rows
and the frequency of certain films being watched. Before removing the
duplicates in titles, cinemetrics has a total row count of 22,208,
whereas it has 19,965 after removal. Removing the duplicates resulted in
2,243 less rows.

### Task 2: Missing Values Handling

<!--- DO NOT DELETE THIS LINE - TASK 2 ANCHOR --->

``` r
missing_summary <- cinemetrics %>%
  summarise_all(~ sum(is.na(.))) %>%
  pivot_longer(everything(), names_to = "Variables", values_to = "Missing_Count")

kable(missing_summary, caption = "Number of missing values per Variable") %>%
  kableExtra::kable_styling(full_width = FALSE, position = "center")
```

| Variables           | Missing_Count |
|:--------------------|--------------:|
| user_id             |             0 |
| movie_id            |             0 |
| watch_date          |             0 |
| watch_duration_mins |           574 |
| engagement_score    |           582 |
| user_rating_100     |           985 |
| review_snippet      |          1939 |
| age                 |            50 |
| subscription_type   |            50 |
| movie_title         |             0 |
| release_year        |             0 |
| budget_usd          |           980 |
| genre_tags          |             0 |

Number of missing values per Variable

``` r
cinemetrics %>%
  summarise(median_user_rating = median(user_rating_100, na.rm = TRUE)) %>%
  kable() %>%
  kableExtra::kable_styling(full_width = FALSE, position = "center")
```

| median_user_rating |
|-------------------:|
|                 71 |

<!--- WRITE YOUR WRITTEN ANSWER BELOW THIS LINE --->

#### Written Interpretation

The generic data overview functions typically are designed to treat text
based “NA”s and other placeholder values as valid text values rather
than as true missing data. This is because NA values are defined as not
being equal to anything, meaning missingness must be detected using
explicit logical checks. Character variables in particular are
frequently not flagged as missing values and may instead be treated as
text, such as the string “NA”.

For example, all 1,939 NA values identified in the table for
review_snippet were not detected in the summary output, as well as the
50 missing values in subscription_type were missed by the standard
missingness checks, demonstrating the difference between placeholder
text and genuine missing data.

### Task 3: Genre Grouping

<!--- DO NOT DELETE THIS LINE - TASK 3 ANCHOR --->

``` r
cinemetrics <- cinemetrics %>%
  mutate(primary_genre = fct_collapse(tolower(genre_tags),
                  Animation = c("animation", "animated", "animation, kids"),
                  Comedy = c("comedy", "comedy, family"),
                  Action = c("action", "action, high-octane"),
                  "Sci-fi" = c("science fiction", "sci-fi", "sci-fi, fantasy"),
                  Drama = c("drama", "drama, emotional", "drama, critically-acclaimed"),
                  Horror = c("horror", "terror", "scary"),
                  Romance = c("romantic comedy", "rom-com", "romantic", "romance"),
                  Documentary = c("documentary", "documentary, real-world")
       ))

cinemetrics <- cinemetrics %>%
  mutate(collapsed_primary_genre = fct_lump_n(primary_genre, n = 5, other_level = "Other"))
```

``` r
genre_tag_summary <- cinemetrics %>%
  group_by(genre_tags) %>%
  summarise(
    avg_user_rating_100 = mean(user_rating_100, na.rm=TRUE),
    Count = n()
  ) 

genre_tag_summary %>%
  filter(genre_tags %in% c("Romance", "romantic comedy")) %>%
  kable(caption = "Average Rating and Count for Romance Sub Genres") %>%
  kableExtra::kable_styling(full_width = FALSE, position = "center")
```

| genre_tags      | avg_user_rating_100 | Count |
|:----------------|--------------------:|------:|
| Romance         |            69.82660 |   991 |
| romantic comedy |            66.35036 |   145 |

Average Rating and Count for Romance Sub Genres

<!--- WRITE YOUR WRITTEN ANSWER BELOW THIS LINE --->

#### Written Interpretation

Due to romantic comedy’s noticeably lower average user rating (66.4)
compared to the romance sub-genre (69.8), by collapsing them together,
the romantic comedy ratings will pull down the average of the aggregated
Romance category which masks the fact that romantic comedies tend to be
rated lower than other Romance sub-genres. This statistical distortion
is known as sample weight imbalance, and causes the loss of specific
subgroup characteristics by treating the rating gap as noise rather than
signal.

### Task 4: Genre Ratings Table

<!--- DO NOT DELETE THIS LINE - TASK 4 ANCHOR --->

``` r
genre_grouped_cinemetrics <- cinemetrics %>%
  group_by(primary_genre)

genre_grouped_cinemetrics %>%
  summarise(avg_user_rating = mean(user_rating_100, na.rm = TRUE)) %>%
  arrange(desc(avg_user_rating)) %>%
  kable(caption = "Average Rating per Primary Genre") %>%
  kableExtra::kable_styling(full_width = FALSE, position = "center")
```

| primary_genre | avg_user_rating |
|:--------------|----------------:|
| Horror        |        79.77719 |
| Comedy        |        70.07359 |
| Drama         |        69.83692 |
| Documentary   |        69.73333 |
| Animation     |        69.69912 |
| Sci-fi        |        69.68587 |
| Romance       |        69.54708 |
| Action        |        69.21661 |

Average Rating per Primary Genre

``` r
genre_grouped_cinemetrics %>%
  summarise(avg_budget = mean(budget_usd, na.rm = TRUE)) %>%
  filter(primary_genre %in% c("Sci-fi", "Horror", "Action")) %>%
  arrange(desc(avg_budget)) %>%
  kable(caption = "Average Budget per Primary Genre") %>%
  kableExtra::kable_styling(full_width = FALSE, position = "center")
```

| primary_genre | avg_budget |
|:--------------|-----------:|
| Sci-fi        |  139588009 |
| Action        |  115543533 |
| Horror        |   10181094 |

Average Budget per Primary Genre

<!--- WRITE YOUR WRITTEN ANSWER BELOW THIS LINE --->

#### Written Interpretation

Based solely on the empirical budget numbers, the Horror genre has both
the highest average user rating (79.8) as well as the lowest average
budget (\$10,181,094), whereas Sci-fi and Action both have considerably
higher budgets, but rank much lower in terms of average rating (6th and
8th respectively). Hence, it can be suggested there is not a positive
relationship between budget and user ratings, as it can be seen that the
higher budget sci-fi and action films both have noticeably low average
ratings.

Since Action is such a general category of films, it is highly probable
that some nuances of sub-genres are being lost when being grouped
together. Collapsing sub-genres into the larger Action category can mask
the poor performance of smaller niche film groups by masking it with
other higher rated sub-categories that contain more data points, thus
weighting the average user rating higher.

### Task 5: Ratings and Engagement Scores

<!--- DO NOT DELETE THIS LINE - TASK 5 ANCHOR --->

``` r
top_10_movies <- cinemetrics %>%
  group_by(movie_id) %>%
  summarise(total_streams = n()) %>%
  slice_max(total_streams, n = 10)

top_10_data <- cinemetrics %>%
  semi_join(top_10_movies, by = "movie_id") %>%
  filter(!is.na(user_rating_100)) %>%
  mutate(movie_title = fct_reorder(movie_title, user_rating_100, .fun = median, .desc = TRUE))

ggplot(top_10_data, aes(x = "", y = user_rating_100)) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot() +
  facet_wrap(~movie_title, nrow = 1,
             labeller = label_wrap_gen(width = 15)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  labs(
    y = "User Rating (out of 100)",
    title = "Distribution of User Ratings for Top 10 Most Streamed Movies"
  ) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank()
  )
```

![](coursework_01_files/figure-commonmark/task-5-code-1.png)

<!--- WRITE YOUR WRITTEN ANSWER BELOW THIS LINE --->

#### Written Interpretation

Up has the lowest individual user rating at 18. However, the
interquartile box is small compared to the other boxes (19.0), implying
that the audience consensus is consistent, with an average rating near
to the median of the whole data frame (70.0 vs 71.0).

``` r
top_10_data %>%
  group_by(movie_id) %>%
  summarise(title = first(movie_title),
            median = median(user_rating_100, na.rm = TRUE),
            IQR = IQR(user_rating_100, na.rm = TRUE)) %>%
  arrange(desc(IQR)) %>%
  kable(caption = "Interquartile Ranges of Ratings for Top 10 Movies") %>%
  kableExtra::kable_styling(full_width = FALSE, position = "center")
```

| movie_id | title           | median |  IQR |
|---------:|:----------------|-------:|-----:|
|       88 | Inside Out      |     71 | 22.5 |
|        1 | The Dark Knight |     69 | 22.0 |
|       87 | WALL-E          |     69 | 22.0 |
|       89 | Titanic         |     69 | 22.0 |
|        7 | Inception       |     70 | 21.0 |
|       16 | Avatar          |     70 | 21.0 |
|        8 | Interstellar    |     70 | 20.0 |
|       85 | Coco            |     71 | 20.0 |
|       86 | Up              |     70 | 19.0 |
|        9 | Dune            |     70 | 18.0 |

Interquartile Ranges of Ratings for Top 10 Movies

#### Empirical Verification

Inside Out has the largest interquartile range (22.5), whereas Dune has
the smallest (18.0).

### Task 6: Engagement Score and Subscription Type

<!--- DO NOT DELETE THIS LINE - TASK 6 ANCHOR --->

``` r
cinemetrics %>%
  filter(!is.na(subscription_type)) %>%
  group_by(subscription_type) %>%
  summarise(avg_engagement = mean(engagement_score, na.rm = TRUE)) %>%
  kable(caption = "Mean Engagement by Subsription Type") %>%
  kableExtra::kable_styling(full_width = FALSE, position = "center")
```

| subscription_type | avg_engagement |
|:------------------|---------------:|
| Basic             |       44.03936 |
| Premium           |       56.47616 |

Mean Engagement by Subsription Type

``` r
quartiles <- quantile(users_raw$age, na.rm = TRUE) # 18, 25, 35, 45, 85

cinemetrics$age_tier <- cut(
    cinemetrics$age, 
    breaks = quartiles,
    include.lowest = TRUE,
    labels = c("18-25", "25-35", "35-45", "45-85")
  )
  
# Make Wide ?
cinemetrics %>% 
  filter(!is.na(subscription_type)) %>%
  group_by(subscription_type, age_tier) %>%
  summarise(avg_engagement = mean(engagement_score, na.rm = TRUE), .groups = "drop") %>%
  kable(caption = "Mean Engagement by Subscription Type and Age Tier") %>%
  kableExtra::kable_styling(full_width = FALSE, position = "center")
```

| subscription_type | age_tier | avg_engagement |
|:------------------|:---------|---------------:|
| Basic             | 18-25    |       28.93870 |
| Basic             | 25-35    |       50.92584 |
| Basic             | 35-45    |       74.33949 |
| Basic             | 45-85    |       80.10280 |
| Premium           | 18-25    |       12.66042 |
| Premium           | 25-35    |       38.23744 |
| Premium           | 35-45    |       55.09623 |
| Premium           | 45-85    |       68.76609 |

Mean Engagement by Subscription Type and Age Tier

``` r
cinemetrics %>% 
  filter(!is.na(subscription_type)) %>%
  group_by(subscription_type, age_tier) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = subscription_type,
    values_from = count
  ) %>%
  kable(caption = "User count for Subsription Type and Age Tier") %>%
  kableExtra::kable_styling(full_width = FALSE, position = "center")
```

| age_tier | Basic | Premium |
|:---------|------:|--------:|
| 18-25    |  4958 |     447 |
| 25-35    |  3523 |    1496 |
| 35-45    |   994 |    3762 |
| 45-85    |   557 |    4178 |

User count for Subsription Type and Age Tier

#### Empirical Verification

The user distribution across age tiers is highly imbalanced between
subscription types. Younger groups are dominated by Basic users, with
4958 Basic vs 447 Premium users in the 18–25 tier and 3523 Basic vs 1496
Premium in the 25–35 tier. In contrast, older tiers are dominated by
Premium users, with 3762 Premium vs 994 Basic in the 35–45 group and
4178 Premium vs 558 Basic in the 45–85 group.

<!--- WRITE YOUR WRITTEN ANSWER BELOW THIS LINE --->

#### Written Interpretation

The contradiction exhibited is that when grouping only by subscription
type, Premium users have a higher overall engagement score (56.48)
compared to Basic users (44.04). However, when the data is segmented by
age_tier, Basic users have higher engagement scores in every age group.

This is an example of Simpson’s Paradox, caused by an imbalance in
subscription counts across age groups. Younger tiers are dominated by
Basic users (4958 Basic vs 447 Premium in the 18–25 group), while older
tiers are dominated by Premium users (4178 Premium vs 558 Basic in the
45–85 group). Moreover, because older users tend to have higher
engagement, as shown by the increase of engagement with increase of age
for both subscription types, and are more likely to use Premium
subscriptions, this imbalance causes the aggregated data to misleadingly
suggest that Premium users are more engaged overall.

------------------------------------------------------------------------

## Part B: Text Data Analysis

### Task 7: Tokenization & Word Frequency

<!--- DO NOT DELETE THIS LINE - TASK 7 ANCHOR --->

``` r
cinemetrics <- cinemetrics %>%
  mutate(review_id = row_number())

tokens_clean <- cinemetrics %>%
  filter(!is.na(review_snippet)) %>%
  select(review_id, primary_genre, review_snippet) %>%
  unnest_tokens(word, review_snippet) %>%
  anti_join(stop_words, by = "word")

top_words <- tokens_clean %>%
  count(word, sort = TRUE) %>%
  slice_head(n = 10)
  
ggplot(top_words, aes(x = reorder(word, n), y = n)) + 
  geom_bar(stat = "identity", width = 0.6) +
  labs(
    x = "Word",
    y = " Word Count",
    title = "Number of Occurances for top 10 words"
  ) + 
  theme(
    axis.title.x = element_text(margin = margin(t = 7))
  )
```

![](coursework_01_files/figure-commonmark/task-7-code-1.png)

<!--- WRITE YOUR WRITTEN ANSWER BELOW THIS LINE --->

#### Written Interpretation

“Film” and “watch” are clearly the two most common words present in
reviews after removing stop words, with 5674 and 5317 occurrences
respectively. However, these words are unhelpful in drawing any
significant conclusions as they will apply to every movie and thus are
irrelevant to any specific subcategory we wish to investigate. Hence,
they are not useful in determining the reasoning behind a rating or
seeing what makes a specific genre’s review unique.

### Task 8: Sentiment Analysis

<!--- DO NOT DELETE THIS LINE - TASK 8 ANCHOR --->

``` r
bing_lexicon <- get_sentiments("bing")

net_sentiment <- tokens_clean %>%
  inner_join(bing_lexicon, by = "word") %>%
  mutate(sentiment = if_else(sentiment == "positive", 1, -1)) %>%
  group_by(review_id) %>%
  summarise(net_sentiment = sum(sentiment), .groups = "drop")

cinemetrics_sentiment <- cinemetrics %>%
  left_join(net_sentiment, by = "review_id") %>%
  group_by(primary_genre) %>%
  summarise(avg_net_sentiment = mean(net_sentiment, na.rm = TRUE),
            avg_user_rating = mean(user_rating_100, na.rm = TRUE),
            .groups = "drop")

ggplot(cinemetrics_sentiment, aes(x = avg_net_sentiment, y = avg_user_rating, colour = primary_genre)) +
  geom_point(size = 3) +
  labs(
    title = "Average Sentiment vs Average User Rating by Genre",
    x = "Average Net Sentiment",
    y = "Average User Rating (out of 100)",
    colour = "Genre"
  )
```

![](coursework_01_files/figure-commonmark/task-8-code-1.png)

<!--- WRITE YOUR WRITTEN ANSWER BELOW THIS LINE --->

#### Written Interpretation

The scatter plot shows all the genres apart from Horror cluster together
with an average net sentiment of 0.771 and an average user rating of
69.7. However, the Horror genre appears as an anomaly with a low average
net sentiment (-1.14) despite having a higher average user rating
(79.8). This probably occurs as the tokens identified in the reviews may
be interpreted as negative by the bing lexicon when taken out of the
context of the Horror genre, while contextually either being positive or
simply descriptive of the films and their contents.

### Task 9: Advanced Text Mining (TF-IDF)

<!--- DO NOT DELETE THIS LINE - TASK 9 ANCHOR --->

``` r
genre_tf_idf <- tokens_clean %>%
  count(primary_genre, word, sort = TRUE) %>%
  bind_tf_idf(word, primary_genre, n) %>%
  arrange(desc(tf_idf))

genre_tf_idf %>%
  group_by(primary_genre) %>%
  slice_max(tf_idf, n = 1, with_ties = FALSE) %>%
  kable(caption = "Top TF_IDF words for each Genre") %>%
  kableExtra::kable_styling(full_width = FALSE, position = "center")
```

| primary_genre | word  |    n |        tf |       idf |    tf_idf |
|:--------------|:------|-----:|----------:|----------:|----------:|
| Action        | watch |  451 | 0.1069227 | 0.1335314 | 0.0142775 |
| Animation     | watch | 1249 | 0.1104919 | 0.1335314 | 0.0147541 |
| Comedy        | watch |  849 | 0.1093649 | 0.1335314 | 0.0146037 |
| Documentary   | watch |  115 | 0.1174668 | 0.1335314 | 0.0156855 |
| Drama         | watch |  934 | 0.1049910 | 0.1335314 | 0.0140196 |
| Horror        | scary |  773 | 0.1335753 | 2.0794415 | 0.2777619 |
| Romance       | watch |  680 | 0.1088697 | 0.1335314 | 0.0145375 |
| Sci-fi        | watch | 1039 | 0.1075013 | 0.1335314 | 0.0143548 |

Top TF_IDF words for each Genre

``` r
genre_tf_idf %>% 
  filter(primary_genre == "Romance") %>%
  slice_max(tf_idf, n = 3) %>%
  kable(caption = "Top 3 TF-IDF scores in Romance Genre") %>%
  kableExtra::kable_styling(full_width = FALSE, position = "center")
```

| primary_genre | word    |   n |        tf |       idf |    tf_idf |
|:--------------|:--------|----:|----------:|----------:|----------:|
| Romance       | watch   | 680 | 0.1088697 | 0.1335314 | 0.0145375 |
| Romance       | average | 359 | 0.0574768 | 0.1335314 | 0.0076750 |
| Romance       | fairly  | 359 | 0.0574768 | 0.1335314 | 0.0076750 |

Top 3 TF-IDF scores in Romance Genre

``` r
genre_tf_idf %>% 
  filter(primary_genre == "Romance") %>%
  slice_max(n, n = 3, with_ties = FALSE) %>%
  kable(caption = "Top 3 frequency word in Romance Genre") %>%
  kableExtra::kable_styling(full_width = FALSE, position = "center")
```

| primary_genre | word    |   n |        tf |       idf |    tf_idf |
|:--------------|:--------|----:|----------:|----------:|----------:|
| Romance       | film    | 697 | 0.1115914 | 0.0000000 | 0.0000000 |
| Romance       | watch   | 680 | 0.1088697 | 0.1335314 | 0.0145375 |
| Romance       | average | 359 | 0.0574768 | 0.1335314 | 0.0076750 |

Top 3 frequency word in Romance Genre

<!--- WRITE YOUR WRITTEN ANSWER BELOW THIS LINE --->

#### Written Interpretation

1.  The TF-IDF score for “film” is 0 in the Romance genre.

2.  IDF is calculated by the log of the number of documents a word
    appears in divided by the total number of documents. With the
    primary genre being the document in this context, because “film”
    appears in at least one review of every genre, the IDF = log(8/8)
    = 0. Hence, the TF-IDF score which is the product of the TF and IDF
    scores respectively, will be 0

3.  “watch”, “average”, and “fairly” have the 3 highest TF-IDF scores in
    the Romance category, whereas the top 3 most commonly occurring
    words are “film”, “watch”, and “average” (“fairly” is tied with
    “average”). This shows that TF-IDF gives a better, more complete
    summary of the genre, as it weighs down words that occur frequently
    across all genres, such as the word “film”, which appears in reviews
    of every genre, instead highlighting words that are unique to each
    genre/document.

### Task 10: Executive Recommendation

<!--- DO NOT DELETE THIS LINE - TASK 10 ANCHOR --->

<!--- WRITE YOUR WRITTEN ANSWER BELOW THIS LINE --->

#### Written Interpretation

Based on the analysis, CineMetrics should prioritise acquiring efficient
genres with low budgets and higher audience ratings. Task 4 shows Horror
achieved the highest average user rating (79.8) despite having the
lowest average budget (\$10.2M) — undercutting Action (\$115.5M) and
Sci-Fi (\$139.6M), suggesting higher spending does not guarantee high
ratings. Furthermore, Task 8 shows that Horror’s sentiment scores are
misleadingly low relative to its ratings, due to lexicon
misinterpretations in how audiences engage, masking true audience
satisfaction. Additionally, Task 6 reveals Basic users, particularly
younger age tiers, have higher engagement per age tier despite Premium
users’ higher overall average (55.48 vs 44.04), suggesting age targeted
campaigns of efficient genres like Horror could promote subscription
upgrades. One limitation is that genre collapsing obscures
high-performing sub-genres. For example, Horror’s 79.8 rating exceeds
its collapsed “Other” group (74.3), implying further sub-genre analysis
may uncover better options.

------------------------------------------------------------------------

<!--- WORD COUNT ANCHOR --->

    **Prose Word Count:** 1178 words
