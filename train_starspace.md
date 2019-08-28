Train a text classification model
================

# Example of ruimtehol

Wonderful [ruimtehol](https://github.com/bnosac/ruimtehol) package build
on top of Facebook research
[Starspace](https://github.com/facebookresearch/StarSpace). R packge was
there before python ;)

Warning, this might take time, suggesting using VM. Here’s example
straight out from the package.

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


starspace_save_model(model)
```

Try trained
model

``` r
text <- c("de nmbs heeft het treinaanbod uitgebreid via onteigening ...",
          "de migranten komen naar europa de asielcentra ...")                   
predict(model, text, k = 3)  
predict(model, "koning filip", k = 10, type = "knn")
predict(model, "koning filip", k = 10, type = "embedding")
```

# Some other data

Data for text classification tasks are out there but not about very
interesting topics. Stanford Uni has collected some [Amazon
reviews](http://snap.stanford.edu/data/amazon/productGraph/categoryFiles/).

For the multiclassification problem I selected a Kaggle competition
[data](https://www.kaggle.com/jhoward/nb-svm-strong-linear-baseline/data),
from Comment Classification Challenge.

``` r
library(dplyr)
library(readr)
train <- read_csv("train.csv")

train %>% head()
```

    ## # A tibble: 6 x 8
    ##   id    comment_text toxic severe_toxic obscene threat insult identity_hate
    ##   <chr> <chr>        <dbl>        <dbl>   <dbl>  <dbl>  <dbl>         <dbl>
    ## 1 0000… "Explanatio…     0            0       0      0      0             0
    ## 2 0001… D'aww! He m…     0            0       0      0      0             0
    ## 3 0001… Hey man, I'…     0            0       0      0      0             0
    ## 4 0001… "\"\nMore\n…     0            0       0      0      0             0
    ## 5 0001… You, sir, a…     0            0       0      0      0             0
    ## 6 0002… "\"\n\nCong…     0            0       0      0      0             0

Let’s modify a bit for the StarSpace/ruimtehol required
format

``` r
#collapse labels to one column and select only the ones that have at least one label
library(tidyr)
train_mod <- train %>%
  gather(Variable, Value, -id, -comment_text, na.rm = TRUE) %>%
  filter(!Value %in% 0) %>%
  group_by(id,comment_text) %>%
  summarise(labels = toString(Variable))

  #StarSpace format
  train_mod$x <- strsplit(train_mod$comment_text, "\\W")
  train_mod$x <- sapply(train_mod$x, FUN = function(x) paste(setdiff(x, ""), collapse = " "))
  train_mod$x <- tolower(train_mod$x)
  train_mod$y <- strsplit(train_mod$labels, split = ",")
  train_mod$y <- lapply(train_mod$y, FUN=function(x) gsub(" ", "", x))

#train_mod %>% select(x,y) %>% head(10)
```

## Training the models

Let’s train our own

``` r
library(ruimtehol)

## TRAIN MODEL for reviews
set.seed(1234)
system.time({
  
  model_toxic <- embed_tagspace(x = train_mod$x, y = train_mod$y,
                          early_stopping = 0.8, validationPatience = 10,
                          dim = 50,
                          lr = 0.01, epoch = 40, loss = "softmax", adagrad = TRUE,
                          similarity = "cosine", negSearchLimit = 50,
                          ngrams = 2, minCount = 2)
})

plot(model_toxic)

starspace_save_model(model_toxic, file = "toxic_textspace.ruimtehol")
```

### Write functions

``` r
make_prediction_toxic<- function(x) {
  prediction <- predict(model_toxic, x, k = 5) #type = "labels"
  return(prediction)}

make_prediction_toxic2<- function(x) {
  prediction <- predict(model_toxic, x, type = "labels") #
  return(prediction)}


# make_prediction_en <- function(x) {
#   prediction <- predict(model_en, x, type = "labels")
#   return(prediction)}
```

### Test functions

``` r
sometetx <- "FUCK YOUR FILTHY MOTHER IN THE ASS, DRY!"

make_prediction_toxic(sometetx)

make_prediction_toxic2(sometetx)
```
