# Association rule analysis

1. [Description of data](#1-description-of-data)
2. [Description of the problem](#2-description-of-the-problem)
3. [Description of the techniques used](#3-description-of-the-techniques-used)
4. [Descriptions of results](#4-descriptions-of-results)

## 1. Description of data

In this assignment a dataset from the UCI Machine Learning Repository was used.  The dataset is called Contraceptive Method Choice Data Set. 

It is a part of the 1987 National Indonesia Contraceptive Prevalence Survey. This dataset consists of married women who were either not pregnant or did not know whether they are at the time they were interviewed. 

**Dataset Description:**

Number of Rows: 934; Number of Attributes: 9

Attribute Information

1. W-age (numerical) - wife's age 

2. W_edu (categorical) - wife's education. Possible values: Low wife education, Lower-middle wife education, Upper-middle wife education, High wife education 

3. H_edu (categorical) - husband's education  Possible values: Low husband education, Lower-middle husband education, Upper-middle husband education, High husband education  

4. Num_of_child (numerical) - number of children ever born 

5. W_relig (categorical) - wife's religion. Possible values: Religion Islam, Religion non-Islam

6. W_work (categorical) - wife's employment. Possible values: Wife now not working, Wife now working

7. L_index (categorical) - standard-of-living index.  Possible values: Low standard-of-living, Lower-middle standard-of-living, Upper-middle standard-of-living, High standard-of-living

8. Media (categorical) - media exposure. Possible values: Good media, Not good media

9. Contraceptive (class attribute) - a contraceptive method used. Possible values: No-use, Long-term, Short-term

## 2. Description of the problem

To determine the more likely target audience for contraceptive products to make ads in the future based on the analysis. Find is there any correlation between contraceptive use and husband's or wife's education level? Also, can the standard-of-living index show any relationship with a contraceptive method used. Moreover, what is an interrelation of the age use of contraceptive? 

## 3. Description of the techniques used

First of all, we need to read the csv file `data <- read.csv("/Users/.../data.csv", sep = ",")`. Dataset description says that there is no missing value, but to be sure, we double-check it with `plot_missing(data)`

The structure of the data is not in the format that is needed for association rules. So, we have to make some data manipulations before finding the relationships.

To interpret categorical variables, they need to be changed to factors. As such, the following changes were made.

We used `mutate` function from `dplyr` package. H_edu, W_relig, W_work, L_index, Media, Contraceptive columns are being converted to factor column. Function `as.factor` converts column to factor column, for example: `data %>%mutate(W_edu = as.factor(W_edu))`

Also our columns W_age and Num_of_child must be numeric (`W_age <- as.numeric(as.character(data$W_age)`). Afterwards these two columns are bound into dataframe using `bind` function. 

The next step is storing this transaction data into a `.csv file`.

After that, this transaction data was loaded into an object of the transaction class. We used function `read.transactions` of the `arules` package.

The last step is to inspect the rules and interpret them. For instance, an interest was in rules with consequent Contraceptive (Long-term use), so `app<-list(default="lhs",rhs="Long-term")`.

Our parameter specifications are minimal support=0.005 and confidence=0.8 values with 6 items as max of items in a rule and minimum 2 items (`params<-list(support=0.005,confidence=0.8,minlen=2,maxlen=6)`)

## 4. Descriptions of results 

Some interesting rules and patterns are presented below.

{20,Lower-middle wife education} => {No-use} (Conf: 80.0%, Supp: 0.8%)
Most of the young women without high education do not use contraceptives.

{42,Upper-middle standard-of-living} => {No-use} (Conf: 87.5%, Supp: 0.8%) 
Most middle-aged women with upper-middle standard-of-living levels do not use contraceptives.

{42,Upper-middle wife education} => {No-use} (Conf: 85.7%, Supp: 0.7%) 
Most middle-aged women with upper-middle education levels do not use contraceptives.

{1,21,Upper-middle husband education} => {Short-term} (Conf: 83.3%, Supp: 0.6%)
It was hypothesised that almost all young women do not use contraceptives, however analisys shows that after families have their first child, they start to use contraceptives for a short term. It was definitely something new to learn.

{2,20,Wife now not working} => {Short-term} (Conf: 83.3%, Supp: 0.6%)
In addition to the previous rule, if a wife is not employed, a couple starts using contraceptives in the short term. 

{29,3,Wife now not working} => {Short-term} (Conf: 100%, Supp: 0.5%)
If a family has three kids and a wife is in her early adulthood and she is not employed, there is a 100% chance that the couple use contraceptives in the short term.

{31,High standard-of-living,High wife education} => {Long-term} (Conf: 83%, Supp: 0.6%)
Most of the younger middle-aged women with high standard-of-living and high-level education use contraceptives in the long term.

{3,36} => {Long-term} (Conf: 100%, Supp: 0.6%)
If a couple has three kids and a wife in her early middle age, there is a 100% chance that contraceptives are used in the long term. That looks logical based on the previous findings.

## 5. Conclusions

As a result, it can be concluded that young people do not use contraceptives, but at the same time, when they become parents, they start using them for a short period and can be the right target audience for ads. Quite surprising was that most younger middle-aged women are the most likely buyers for such products. 

Also, education level did not correspond with the frequency of contraception using. But standard-of-living is lightly corresponding with usage of contraceptives. This area can be explored in the future.

References:
1. https://www.datacamp.com/community/tutorials/market-basket-analysis-r
2. https://towardsdatascience.com/making-recommendations-using-association-rules-r-programming-1fd891dc8d2e
