```{r, results='hide', echo=FALSE}
# fsets <- eclat(transact, parameter = list(support = 0.00023), control = list(verbose=FALSE))
# singleItems <- fsets[size(items(fsets)) == 1]
# singleSupport <- quality(singleItems)$support
# names(singleSupport) <- unlist(LIST(items(singleItems), decode = FALSE))
# head(singleSupport, n = 5)
# itemsetList <- LIST(items(fsets), decode = FALSE)
# allConfidence <- quality(fsets)$support / sapply(itemsetList, function(x) max(singleSupport[as.character(x)]))
# 
# quality(fsets) <- cbind(quality(fsets), allConfidence)
# summary(fsets)
```

#### Building a recommendation system
```{r}
# Create unique identifier
line_item_clean <- line_item_clean %>% mutate(order_sku = paste(id_order, sku, sep = ' '))

# Filter out duplicates and drop unique identifier
line_item_clean <- line_item_clean[!duplicated(line_item_clean$order_sku), ] %>% select(-order_sku)
```

```{r}
order_item_matrix <- line_item_clean %>%
  # Select only needed variables
  select(id_order, sku) %>% 
  # Add a column of 1s
  mutate(value = 1) %>%
  # Spread into order-item format
  spread(sku, value, fill = 0) %>%
  select(-id_order) %>%
  # Convert to matrix
  as.matrix() %>%
  # Convert to recommenderlab class 'binaryRatingsMatrix'
  as("binaryRatingMatrix")
```

The data was split into a train and a test set by selecting train = 0.8 for a 80/20 train/test split. The method was set “cross” up with 2-fold cross validation. This means that the data is divided into 2 subsets of equal size, with 80% of the data used for training and the remaining 20% used for evaluation. The models are recursively estimated 2 times, each time using a different train/test split, which ensures that all orders and items are considered for both training and testing. The results can then be averaged to produce a single evaluation set. Selecting given = -1 means that for the test orders ‘all but 1’ randomly selected item is withheld for evaluation.

```{r, warning=FALSE}
# split the data into the training and the test set
transact_rs <- evaluationScheme(order_item_matrix, method = "cross", k = 1, train = 0.8, given = -1)

transact_rs
```

```{r}
algorithms <- list(
  "association rules" = list(name  = "AR", param = list(supp = 0.0001, conf = 0.5)),
  "random items"      = list(name  = "RANDOM",  param = NULL),
  "popular items"     = list(name  = "POPULAR", param = NULL),
  "item-based CF"     = list(name  = "IBCF", param = list(k = 2)),
  "user-based CF"     = list(name  = "UBCF", param = list(method = "Cosine", nn = 500))
)

results <- evaluate(transact_rs, algorithms, type  = "topNList", n = c(1, 5, 10, 15))
#saveRDS(results, "algorithms.RDS")

results <- readRDS("algorithms.RDS")
```

```{r, echo=FALSE}
avg_conf_matr <- function(results) {
  tmp <- results %>%
    getConfusionMatrix()  %>%  
    as.list() 
  as.data.frame(Reduce("+",tmp) / length(tmp)) %>% 
    mutate(n = c(1, 5, 10, 15)) %>%
    select('n', 'precision', 'recall', 'TPR', 'FPR') 
}
```

```{r, warning=FALSE}
# Using map() to iterate function across all models
results_tbl <- results %>% map(avg_conf_matr) %>% 
  # Turning into an unnested tibble
  enframe() %>%
  # Unnesting to have all variables on same level
  unnest()
```

#### ROC curve
```{r}
results_tbl %>%
  ggplot(aes(FPR, TPR, 
             colour = fct_reorder2(as.factor(name), 
                                   FPR, TPR))) +
  geom_line() +
  geom_label(aes(label = n))  +
  labs(title = "ROC curves", colour = "Model") +
  theme_grey(base_size = 14)
```

```{r}
results_tbl %>%
  ggplot(aes(recall, precision, 
             colour = fct_reorder2(as.factor(name),  
                                   precision, recall))) +
  geom_line() +
  geom_label(aes(label = n))  +
  labs(title = "Precision-Recall curves", colour = "Model") +
  theme_grey(base_size = 14)
```


#### Predictions For a New User
```{r}
new_order <- c("APP1215")

kable(line_item_clean %>% filter(sku %in% new_order) %>% select(sku, name_en, short_desc_en, brand, manual_categories) %>% rename(Name = name_en, Description = short_desc_en, Brand = brand, Category = manual_categories) %>% unique() %>% arrange(sku))
```

```{r}
new_order_item_matrx <- line_item_clean %>% 
  # Select item descriptions from retail dataset
  select(sku) %>% unique() %>% 
  # Add a 'value' column with 1's for customer order items
  mutate(value = as.numeric(sku %in% new_order)) %>% 
  # Spread into sparse matrix format
  spread(key = sku, value = value) %>% 
  # Change to a matrix
  as.matrix() %>% 
  # Convert to recommenderlab class 'binaryRatingsMatrix'
  as("binaryRatingMatrix")
```

```{r}
recomm <- Recommender(getData(transact_rs, 'train'), method = "AR", param = list(supp = 0.0001, conf = 0.5))
```


```{r}
pred <- predict(recomm, newdata = new_order_item_matrx, n = 10)
```

```{r}
kable(line_item_clean %>% filter(sku %in% as(pred, 'list')[[1]]) %>% select(sku, name_en, short_desc_en, brand, manual_categories) %>% rename(Name = name_en, Description = short_desc_en, Brand = brand, Category = manual_categories) %>% unique() %>% arrange(sku))
```


#### Building a hybrid recommendation system
```{r}
# split the data into the training and the test set
transact_rs <- evaluationScheme(order_item_matrix, method = "split", 
                                train = 0.8, given = -1, goodRating = 0)
```

```{r}
# train a hydrib recommender model
hybrid_recom <- HybridRecommender(
  Recommender(getData(transact_rs, "train"), method = "POPULAR"),
  Recommender(getData(transact_rs, "train"), method = "IBCF", param = list(normalize = NULL, method = "Cosine")),
  Recommender(getData(transact_rs, "train"), method = "UBCF", param = list(normalize = "Z-score", method = "Euclidean")),
  Recommender(getData(transact_rs, "train"), method = "RANDOM"), 
  weights = c(.2, .3, .3, .2)
)

# Observe the model that is built
print(getModel(hybrid_recom))
```

```{r}
# making predictions
pred <- predict(hybrid_recom, getData(transact_rs, "known"), type = "ratings")
```