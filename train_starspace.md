Train a text classification model
================

# Example of ruimtehol

Wonderful [ruimtehol](https://github.com/bnosac/ruimtehol) package build
on top of Facebook research
\[Starspace\])(<https://github.com/facebookresearch/StarSpace>). R
packge was there before python ;)

``` r
library(ruimtehol)
data("dekamer", package = "ruimtehol")
dekamer$x <- strsplit(dekamer$question, "\\W")
dekamer$x <- sapply(dekamer$x, FUN = function(x) paste(setdiff(x, ""), collapse = " "))
dekamer$x <- tolower(dekamer$x)
dekamer$y <- strsplit(dekamer$question_theme, split = ",")
dekamer$y <- lapply(dekamer$y, FUN=function(x) gsub(" ", "-", x))

set.seed(123456789)
model <- embed_tagspace(x = dekamer$x, y = dekamer$y,
                        dim = 50, 
                        lr = 0.01, epoch = 40, loss = "softmax", adagrad = TRUE, 
                        similarity = "cosine", negSearchLimit = 50,
                        ngrams = 2, minCount = 2)
plot(model)                        
            
text <- c("de nmbs heeft het treinaanbod uitgebreid via onteigening ...",
          "de migranten komen naar europa de asielcentra ...")                   
predict(model, text, k = 3)  
predict(model, "koning filip", k = 10, type = "knn")
predict(model, "koning filip", k = 10, type = "embedding")
```

# Data

Data for text classification taska are out there but not in very
interesting topics. Stanford Uni has collected some [Amazon
reviews](http://snap.stanford.edu/data/amazon/productGraph/categoryFiles/)

``` r
library(jsonlite)
library(dplyr)

out <- lapply(readLines("reviews_Pop.json"), fromJSON)

library(data.table)
outdf <- rbindlist(out, fill=TRUE) %>% unique()

outdf %>% 
  group_by(overall) %>% 
  summarise(n=n())
```

    ## # A tibble: 5 x 2
    ##   overall     n
    ##     <dbl> <int>
    ## 1       1   562
    ## 2       2   242
    ## 3       3   473
    ## 4       4  1356
    ## 5       5  9113

``` r
#Let's add categories
outdfsample <- outdf %>% 
  mutate(pos_neg = if_else(overall>3, 'GOOD', 'BAD')) %>% 
  sample_frac(0.01)

## Split data
## 75% of the sample size
smp_size <- floor(0.75 * nrow(outdfsample))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(outdfsample)), size = smp_size)

train <- outdfsample[train_ind, ]
test <- outdfsample[-train_ind, ]
```

## Training the model

Let’s train our own

``` r
library(ruimtehol)

## TRAIN MODEL
set.seed(1234)
system.time({
  
  model_en <- embed_tagspace(x = train$reviewText, y = train$pos_neg,
                          early_stopping = 0.8, validationPatience = 10,
                          dim = 50,
                          lr = 0.01, epoch = 40, loss = "softmax", adagrad = TRUE,
                          similarity = "cosine", negSearchLimit = 50,
                          ngrams = 2, minCount = 2))
})

plot(model)
```

### Write functions

``` r
make_prediction_en <- function(x) {
  prediction <- predict(model_en, x, type = "labels")
  return(prediction)}

make_prediction_nl <- function(x) {
  prediction <- predict(model, x, type = "labels")
  return(prediction)}
```

### Test peformance

``` r
make_prediction_en(test$reviewText)
```
