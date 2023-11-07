# Turtle-Games
This was the third assignment I completed as part of the LSE Data Analytics Career Accelerator course, for which I achieved a mark of 89% (distinction). I used Python and R for this project.

**1.Background**

Turtle Games (fictitious company) commissioned KW Data Analytics to conduct an analysis of customer trends in order to improve overall sales performance. The marketing department wants to understand how to assess customers by loyalty, how to group customers for targeted campaigns, and how to best use online reviews for marketing and customer engagement. The sales department wants to understand the impact of products on sales and how regional sales can predict Global sales. This will help optimize resource allocation and improve the effectiveness of sales tactics. 

Figure 1. Fishbone diagram showing causes of poor online sales. 
![image](https://github.com/kittyg80/Turtle-Games/assets/116217853/34344040-a0d7-444d-b006-673ad68a6463)

**2.Analytical-Approach**

**2.1	Data ingestion and wrangling**

•	Short code, descriptive function/variable names, concise syntax, and insightful commentary.

•	Data files added to directories and libraries/packages imported. 

•	No unusual data, spelling errors, or duplicates found. Replaced two N/A turtle_sales Year values. 

•	For turtle_sales, Year and Product changed to categorial. New column, Other_sales, calculated. 

**2.2	Turtle_reviews data** (analysed with Python)

2.2.1	Data exploration and outliers

•	No outliers detected for income, spending score, or age. Outliers for loyalty points not removed. 

Figure 2: Boxplot showing outliers for Loyalty points.
![image](https://github.com/kittyg80/Turtle-Games/assets/116217853/9eb39886-48fb-4c1e-8a39-e5b63fe824eb)

•	Gender showed no influence on loyalty points. Basic education had some impact, but observations too low. 

Figure 3: Boxplot showing impact of gender (top) and education(bottom) on Loyalty points.
![image](https://github.com/kittyg80/Turtle-Games/assets/116217853/617d46a5-3d1d-4de1-96db-cd705f8d1d1a)
![image](https://github.com/kittyg80/Turtle-Games/assets/116217853/ccd887ed-94de-435d-b60e-a7bd940425fe)

•	Correlation between loyalty points and income and spending score, but not age.

Figure 4: Correlation matrix showing relationships between numerical variables.
![image](https://github.com/kittyg80/Turtle-Games/assets/116217853/95bc5a5d-4f40-41f5-a5a0-2e7660ec1923)

Figure 5: Scatterplot showing relationship between age and loyalty points.
![image](https://github.com/kittyg80/Turtle-Games/assets/116217853/391cfafd-706b-4348-a57b-1caa22480db2)


2.2.2	Simple Linear regression with OLS method

•	Models created and regression lines fitted for loyalty points versus spending score, income, and age. 

•	Accuracy assessed using R squared and data checked for heteroscedasticity. 

•	Spending score and income showed heteroscedasticity. Columns log transformed and new models created. 

•	Similar models created with machine learning and LinearRegression() function (requested by company). 

Figure 6: Cone shaped scatterplot (heteroscedasticity) for loyalty points vs spending score. 
![image](https://github.com/kittyg80/Turtle-Games/assets/116217853/ce89bf9a-7803-4588-8dc4-e86d437f8caa)

2.2.3	Multiple Linear regression 

•	Model fitted and loyalty points predicted using spending score and income. 

•	Training and test data created (70:30), model fitted using training data, and loyalty points predicted for test data. Accuracy assessed using R squared and other parameters.  

•	Data checked for heteroscedasticity and multicollinearity. 

2.2.4	Clustering of customer groups by income and spending score

•	Income and spending score clusters assessed using pairplots.

•	Elbow and silhouette methods used to determine optimal cluster number (k = 5)

•	K-mean clustering performed using different k values (4-6). 

Figure 7: Pairplots showing distribution of income vs spending score.
![image](https://github.com/kittyg80/Turtle-Games/assets/116217853/476fefd9-0f18-4b8d-b05b-a7f316a7caeb)

Figure 8: Elbow method showing optimal cluster number as five. 
![image](https://github.com/kittyg80/Turtle-Games/assets/116217853/9e1f647a-f33a-4108-9c09-3e1c2ace4c4d)

2.2.5	Sentiment analysis of customer reviews using  natural language processing

•	Text pre-processed and tokenised. Data concatenated from each row, word frequency determined, and wordclouds generated.

Figure 9: Wordcloud for Review column before stopwords removed. 
![image](https://github.com/kittyg80/Turtle-Games/assets/116217853/2c03b5f4-ec29-4b6f-ab30-26e6cfeaded4)

•	Stopwords removed and new wordclouds generated. Most common words extracted, and polarity determined. 

•	Polarity and subjectivity calculated for all reviews and summaries and histograms used to visualise distribution of overall sentiment.

•	Top 250 positive and negative reviews and summaries by polarity identified, with wordclouds and frequency distribution for top terms generated. Certain negative words investigated further. 

•	Top 20 products by negative polarity identified. Certain products investigated further. 

**2.3	Turtle_sales data** (analysed with R) 

2.3.1	Data exploration and outliers 

•	Outliers seen in all sales columns but not removed. 

Figure 10: Boxplot showing outliers for Global sales.
![image](https://github.com/kittyg80/Turtle-Games/assets/116217853/fca4fb6a-3115-425f-951e-2922d2ebe0c5)

•	Histograms used to see distribution of sales columns, and all skewed heavily to the right. 

Figure 11: Histogram showing distribution of NA Sales. 
![image](https://github.com/kittyg80/Turtle-Games/assets/116217853/adebb179-15e8-442f-bb8b-61c89ffa262b)

•	Boxplots assessed distribution and impact of platform, genre, publisher, and year on Global sales. 

•	Barcharts created to see game count by platform, genre, publisher, and year. Faceted barcharts compared global and regional grouped sales by platform, genre, publisher, and year. 

2.3.2	Impact of product on sales

•	Stacked barchart created for top 50 products globally, split by region. 

•	Barcharts created for top 25 products by region, as well as products with ≤0.2% sales. 

2.3.3	Determine the reliability of the data sets

•	Sales columns assessed for normality using Q-Q Plots with references lines for normal distribution.  

•	Data assessed using Shapiro-Wilk tests and checked for skewness and kurtosis. 

•	Correlation between Global sales and NA and EU sales determined. 

2.3.4	Simple Linear regression 

•	Models of Global sales against EU sales and NA sales created and regression lines fitted. 

•	Accuracy determined using R squared, MSE, RMSE, and MAE. 

•	Checked for heteroscedasticity. 

2.3.5	Multiple Linear regression 

•	Four models generated of Global sales versus EU sales and NA sales, and scatterplots showed predicted versus observed values. 

•	Accuracy determined using R squared, adjusted R squared, MSE, RMSE, and MAE.

•	Checked for heteroscedasticity and multicollinearity. 

•	Accuracy of predicted values against observed assessed for the best model. 

**3.	Visualisations and Insights**

3.1	Factors that influence accumulation of loyalty points by customers

•	Simple linear regression showed positive relationships between loyalty points and income and spending score. R squared indicated they were not strong predictors of loyalty points. 

Figure 12: Simple linear regression for loyalty points versus spending score (log transformed). 
![image](https://github.com/kittyg80/Turtle-Games/assets/116217853/8f3fec89-2079-4fc9-94d6-41287bac6858)
![image](https://github.com/kittyg80/Turtle-Games/assets/116217853/4387f80c-c77d-4191-92ee-63e35f794b84)

•	Multiple linear regression with spending score and income had an R squared of 98% and was therefore stronger for predicting loyalty points. Error metrics were lower, with no multicollinearity or heteroscedasticity. 

Table 1: Accuracy parameters of simple versus multiple linear regression models for predicting loyalty points.
![image](https://github.com/kittyg80/Turtle-Games/assets/116217853/92057372-25a9-43c2-9809-2215bfa01527)

3.2	Grouping customers to target specific market segments

K-means clustering created five customer segments: 

•	Low Budget: low income / low spending score

•	Spenders: low income / high spending score

•	Average: average income / average spending score

•	Savers: high income / low spending score

•	Ideal: high income / high spending score

Figure 13: Scatterplot showing the five customer clusters based on spending score and income. 
![image](https://github.com/kittyg80/Turtle-Games/assets/116217853/bacd59b8-cfad-4e59-9311-994b17234c41)


3.3	Using online customer reviews for marketing campaigns

•	Wordcloud and frequency distribution showed most common words in reviews (e.g., 'great,' 'fun') and summaries (e.g., 'stars,' 'five,' 'great').

Figure 14: Wordcloud (no stopwords) (top) and frequency distribution (bottom) for reviews.
![image](https://github.com/kittyg80/Turtle-Games/assets/116217853/33566d40-8e9d-4cce-8e2f-4c816f21173f)
![image](https://github.com/kittyg80/Turtle-Games/assets/116217853/ae934799-ead5-49e6-9baa-5d13c2d8251b)

Figure 15: Wordcloud (no stopwords) (top) and frequency distribution (bottom) for summaries.
![image](https://github.com/kittyg80/Turtle-Games/assets/116217853/a65f3d18-79be-49af-b14f-10f569a6075a)
![image](https://github.com/kittyg80/Turtle-Games/assets/116217853/d9582dba-41bd-4f21-804b-34befd1c2a02)

•	The 250 most positive reviews/summaries identified additional words (e.g., 'perfect,' 'excellent,' 'best,' 'wonderful’). The 20 most positive reviews/summaries printed to identify opportunities to thank customers online.

Figure 16: Wordcloud for top 250 positive reviews by polarity.
![image](https://github.com/kittyg80/Turtle-Games/assets/116217853/96e29451-350c-4bba-899c-05858bba6eec)

Figure 17: Wordcloud for top 250 positive summaries by polarity. 
![image](https://github.com/kittyg80/Turtle-Games/assets/116217853/d72e1977-ba13-46c8-9da9-073198f7ad63)

•	The 250 most negative reviews/summaries identified terms for investigation.  ‘Anger' incorrectly classified as negative, whereas summaries containing 'disappointed' or ‘disappointing' provide opportunities for customer engagement. Top negative reviews/summaries printed to identify opportunities to address product or customer service issues. 

Figure 18: Wordcloud for top 250 negative summaries by polarity.
![image](https://github.com/kittyg80/Turtle-Games/assets/116217853/d2755222-10b4-4213-bb8a-2e8792267700)

•	Products with most negative reviews/summaries identified. Not negative in some cases and others revealed opportunities to improve products, e.g., better quality materials.

Figure 19: Reviews for product 3436 with words related to product quality highlighted.
![image](https://github.com/kittyg80/Turtle-Games/assets/116217853/e5a4aa5e-e6a0-422d-bd7b-4a6b96d2d144)

•	Overall positive sentiment distribution for reviews/summaries. Most reviews had positive polarity, while most summaries were neutral or positive. Average polarity scores were similar (0.21 for reviews, 0.22 for summaries). For subjectivity, review distribution was spread evenly, whereas summaries were more fact based. 

Figure 20: Histograms of distribution of polarity for reviews (top) and summaries (bottom).
![image](https://github.com/kittyg80/Turtle-Games/assets/116217853/5b8794d5-c322-4360-afad-15d64e56ade8)
![image](https://github.com/kittyg80/Turtle-Games/assets/116217853/013596d5-ca91-4c98-821b-ab4a4bc97cf5)

Figure 21: Histograms of distribution of subjectivity for reviews (top) and summaries (bottom).
![image](https://github.com/kittyg80/Turtle-Games/assets/116217853/5be511a6-fb95-47cd-9055-78cfcea19bf2)
![image](https://github.com/kittyg80/Turtle-Games/assets/116217853/c0008fc2-708d-4e3a-8fab-c44e8802a3d5)

**3.4	Analysis of product sales trends**

•	North America had the highest proportion of sales (47%). 

Figure 22: Total sales by region. 
![image](https://github.com/kittyg80/Turtle-Games/assets/116217853/75214793-e971-40c0-a083-8a7138a987a3)

•	Boxplots showing impact of variables on Global sales highlighted different genre had less effect compared to platform, publisher, and year. 

Figure 23: Boxplot showing impact of platform on Global sales.
![image](https://github.com/kittyg80/Turtle-Games/assets/116217853/b06c4acf-2e7b-4e1b-99b0-e83492579e24)

Figure 24: Boxplot showing impact of genre on Global sales.
![image](https://github.com/kittyg80/Turtle-Games/assets/116217853/e8c00e75-d571-43e8-a86d-ab12964979ab)

•	Barcharts show the count of games of platform, publisher, genre, and year. 

Figure 25: Barchart showing count of games by publisher. 
![image](https://github.com/kittyg80/Turtle-Games/assets/116217853/0c51ad6b-6ee0-4bf6-9c43-317c94530248)

•	Global and regional sales compared by platform, genre,  publisher, and year. For platform, Wii accounted for 17% of Global sales, followed by Xbox 360 (14%). Similar trends for EU and NA sales, but DS was top for Other Sales. 

Figure 26: Global sales versus regional sales by platform. 
![image](https://github.com/kittyg80/Turtle-Games/assets/116217853/df4bf14f-f579-4e22-a345-7d0fdaf56db8)

•	For year, trends were similar but a slightly higher proportion of sales of older games in the US highlights a trend for retro gaming.    

Figure 27: Global sales versus regional sales by year. 
![image](https://github.com/kittyg80/Turtle-Games/assets/116217853/b27d3cb2-7662-4575-9f66-34d6c38be354)

•	Impact of product on sales investigated. Products 107 and 515 were top two globally and appeared in top five in all regions, but other top products differed. Regional differences also for products with ≤ 0.2% sales. 

Figure 28: Top 50 products by Global sales split by region.
![image](https://github.com/kittyg80/Turtle-Games/assets/116217853/b4851f3a-ac25-428f-9976-7a798cb82f94)

Figure 29: Top 25 products by NA sales. 
![image](https://github.com/kittyg80/Turtle-Games/assets/116217853/61c45553-820a-48c6-9654-2757efe94d57)

Figure 30: Top 25 products by EU sales.
![image](https://github.com/kittyg80/Turtle-Games/assets/116217853/c6cd6f1b-9fb4-47b5-98b9-3fa8e2af3c44)


**3.5	Reliability of the datasets**

•	Results from Shapiro-Wilk tests indicate that sales columns were significantly non-normal. Distributions were heavily skewed and highly leptokurtic.

Figure 31: Q-Q plot for Global sales showing significant right skew.
![image](https://github.com/kittyg80/Turtle-Games/assets/116217853/5fef7126-44ca-4331-99eb-2edcbaaa3b99)

•	Usually normalise before regression but sales columns have similar scales, and dependent and independent variables are strongly correlated. Therefore, normalisation not performed. 

Figure 32: Correlation matrix for sales columns showing strong positive relationships.
![image](https://github.com/kittyg80/Turtle-Games/assets/116217853/6fbd028c-5c4e-4c4f-8ec5-1038606fc827)


**3.6	Predicting Global sales with regional sales**

•	Simple linear regression models were a good fit. R-squared values acceptable but multiple linear regression performed to generate a better model for predicting Global sales.

Figure 33: Simple linear regression for Global sales versus EU sales. 
![image](https://github.com/kittyg80/Turtle-Games/assets/116217853/02ec5e4e-173f-4b91-930c-d5678eaa9275)

Figure 34: Simple linear regression for Global sales versus NA sales. 
![image](https://github.com/kittyg80/Turtle-Games/assets/116217853/d290f1b5-30e9-4503-97ab-d00d99543972)

•	Four multiple linear regression models created and compared. Based on accuracy parameters,  Model 5 (grouped data by product) was most accurate for predicting Global sales based on EU and NA sales. 

Table 2: Accuracy parameters of simple versus multiple linear regression models for predicting Global sales. 
![image](https://github.com/kittyg80/Turtle-Games/assets/116217853/d625679c-9b27-4726-92be-fa7cccebe133)

Figure 35: Scatterplot showing observed versus predicted Global sales values using Model 5.
![image](https://github.com/kittyg80/Turtle-Games/assets/116217853/ba77523f-38b5-4d58-9708-38583485dbc8)

**4.	Patterns and Predictions**

•	Multiple linear regression model can predict how spending score and income impact loyalty points accumulation. The marketing department can use this to segment customers into "loyalty" groups for targeted campaigns. "Low loyalty" customers (lower income and spending score) require acquisition tactics e.g., increase value of loyalty points towards next purchase. Reward “high loyalty" customers (high income and spending score) e.g., early access to new products or exclusive sales.

•	Marketing teams can use customer clusters for targeted campaigns. Spenders and Ideal are most "loyal" and generate most revenue. Reward them with enhanced loyalty programs, exclusive discounts, and referral schemes. Those with low spending score require acquisition strategies, such as free postage or discounts on next purchases, tailored to different income levels.

•	From sentiment analysis, common words from positive reviews can be integrated into marketing campaigns and search engine optimisation. Turtle Games can thank customers online for positive reviews, particularly loyal customers. Negative reviews can identify pain points, create opportunities to resolve issues, and highlight why products have negative reviews. 

•	Analysis of sales by product will help Turtle Games understand which factors have the most impact on game sales, as well the highest and lowest selling products by region to support sales strategies and streamline local product offerings. 

•	Multiple linear regression for predicting Global sales with regional sales will allow sales teams to make better decisions, optimize resource allocation, and improve effectiveness of sales tactics. 

Areas of further exploration and limitations:

•	Time series data for more accurate sales forecasts. 

•	Relationship between product review sentiment and sales.

•	Addition of sales data for other products (currently only video games). 

•	Data quality issue - reviews of same ID number involve different products.  

