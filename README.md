# Materials for course "Election Forecasting"

**Repository**

This repository provides materials for the course "Election forecasting". You'll find the following code:

- `04-fundamentals-models.r`: An R script that shows how to replicate Gschend and Norpoth's Chancellor model and how to set up an out-of-sample forecast.
- `06-prediction-markets.r`: An R script that provides functions to illustrate how to correct for favorite-longshot bias
- `digital-data-models.r`: An R script that shows how to tap data from Twitter's streaming API using the `streamR` package, how to gather pageviews data from Wikipedia using the `pageviews` package and the `statsgrokse` package, and how to gather Google Trends data using the `gtrendsR` package

In addition, the `data` folder provides a bunch of Stata datasets relevant to build fundamentals- and polling-based forecasting models for the German federal election. They can be imported using either Stata or R. The datasets are:

- `election_results_ltw.dta`: results of all German State elections ("Landtagswahlen") between 1946 and September 2016. Only results from the following parties are included: CDU, CSU, SPD, FDP, Greens, Left, NPD, AfD. The dataset also provides a variable `election` that reports the year of the Bundestag election following the respective state election, as well as a variable `dist` that reports the distance to the next Bundestag election in days. **Use this dataset if you want to forecast state elections or want to use state election results to predict Bundestag election results.**
- `ger_model_df.dta`: results of all German Bundestag elections since 1949, together with many more variables. The units of this dataset are election-party units, i.e. each row represents one party in one election. **Use this dataset if you want to forecast party-specific vote shares at the Bundestag election.**
- `ger_nat.dta`: results of all German Bundestag elections since 1949, together with many more variables. The units of this dataset are elections, i.e. each row represents one election. This dataset can be thought of as a replication dataset for Gschwend/Norpoth. **Use this dataset if you want to build a forecasting model based on Gschwend/Norpoth and/or if you are interested in forecasting coalition vote shares or other national-level quantities, such as turnout.**
- `polls_btw.dta`: polling results for all major parties in Germany since 1953. The units of this dataset are poll-specific party results, i.e. it's a long format where one row reports the polling result for one party in one poll. **Use this dataset if you are interested in aggregating polls.**
- `polls_btw_wide.dta`: polling results for all major parties in Germany since 1953. The units of this dataset are polls, i.e. it's a wide format where one row reports the result of one poll. **Use this dataset if you are interested in aggregating polls.**
- `polls_df_agg.dta`: polling results, aggregated at the election level. The units of this dataset are aggregated polls by election and party. Several time windows are reported, e.g., the average polling performance of party X at election X, 200-230 days before the election. **Use this dataset if you are interested in using aggregated polls in your forecasting model.**


**FAQ**

1. I'm a Stata, not an R guy, but want to learn R. Where to start? 

*Look at the course materials on Moodle, which are not shared here. Alternatively, work through the excellent and accessible "R for Data Science", freely available [here](http://r4ds.had.co.nz/).*

To be continued.


**Course contents**

Forecasting election outcomes has become a popular sport among scholars, pundits, and the media alike. Statistical models that provide timely and precise forecasts are of great interest for campaign organizers and financial contributors who want to target their resources efficiently. In this course, we will study different methods of election forecasting and apply them in the context of upcoming elections. In the first part of the course, we will critically discuss existing approaches of election forecasting, including (1) fundamentals models that exploit indicators of economic performance and political mood, (2) poll-based models that infer the result from public opinion surveys, (3) betting markets, and (4) approaches that draw on new data sources, such as social media data. In the second part of the course, we will turn to a practical implementation of these methods for upcoming elections, including (but not limited to) the 2017 German federal election. To that end, we will develop own statistical models, collect and prepare data and finally generate forecasts using statistical software.

**Instructor** 

Simon Munzert ([website](https://simonmunzert.github.io), [Twitter](https://twitter.com/simonsaysnothin))

