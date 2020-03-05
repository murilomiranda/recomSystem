## Market Basket Analysis

One month's of online transactions in the company was given. The data set includes 9834 transactions was given.

We want to build a **recommendation system** to increase cross-selling in our website. Our goal is to show customers who have added a product to their basket another product they’re likely to want. Because of the nature of our data, the best way to do that is by using a method called **Market Basket Analysis**.

The first step is the **data quality** assessment: we need to make sure that we can trust the data that we’re using. Make sure the information across our datasets (_line_item_, _orders_ and _products_) is consistent, and get rid of anything too strange or to believe.

Then, we will have to build a **transactional dataset**: a table with one row per order and one column per product bought -meaning that the table has as many columns as the biggest order, with lots of NAs.

Finally, we will use the **apriori algorithm** to create if-then rule with our products: “if a customer buys product A, then it’s likely that they’ll buy product B”. Focus on optimizing the model: it will be deployed into production by our engineering team. 