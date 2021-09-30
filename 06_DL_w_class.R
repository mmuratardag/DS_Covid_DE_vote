
cores <- parallel::detectCores(logical = F)
doParallel::registerDoParallel(cores = cores-1)

load("data/data_class.RData")
colnames(d)

library(tidyverse)
library(fastDummies)
cat_var <- colnames(d[,c(2:4,6,8:9,66)])
df <- dummy_cols(d,
                 select_columns = cat_var,
                 remove_first_dummy = T,
                 remove_selected_columns = F)
colnames(df)

df <- df %>% select(id, choice_of_party,
                    sex_female:`class_Class 2`) %>%
  mutate(choice_of_party = as.numeric(choice_of_party) -1)

set.seed(666)
train <- df %>% sample_frac(.9)
test  <- anti_join(df, train, by = 'id')

x_train <- train %>% select(sex_female:`class_Class 2`)
x_test  <- test %>% select(sex_female:`class_Class 2`)

y_train <- train %>% select(choice_of_party)
y_test <- test %>% select(choice_of_party)

reticulate::use_condaenv("r-reticulate")

library(keras)
y_train <- to_categorical(y_train, 10)
y_test  <- to_categorical(y_test,  10)

x_train <- as.matrix(x_train)
y_train <- as.matrix(y_train)
x_test <- as.matrix(x_test)
y_test <- as.matrix(y_test)

build_model <- function() {
  model <- keras_model_sequential()
  model %>% 
    layer_dense(units = 66,
                input_shape = dim(x_train)[2],
                kernel_regularizer = regularizer_l2(l = 0.001)) %>%
    layer_activation_relu() %>%
    layer_dense(units = 120,
                activation = 'relu',
                kernel_regularizer = regularizer_l1_l2(l1 = 0.01, l2 = 0.01)) %>% 
    layer_dropout(0.4) %>%
    layer_dense(units = 240,
                activation = 'relu',
                kernel_regularizer = regularizer_l1_l2(l1 = 0.01, l2 = 0.01)) %>% 
    layer_dropout(0.5) %>%
    layer_dense(units = 120,
                activation = 'relu',
                kernel_regularizer = regularizer_l1_l2(l1 = 0.01, l2 = 0.01)) %>%
    layer_dropout(0.4) %>%
    layer_dense(units = 60,
                activation = 'relu',
                kernel_regularizer = regularizer_l1_l2(l1 = 0.01, l2 = 0.01)) %>% 
    layer_dropout(0.3) %>% 
    layer_dense(units = 30,
                activation = 'relu',
                kernel_regularizer = regularizer_l1_l2(l1 = 0.01, l2 = 0.01)) %>% 
    layer_dropout(0.2) %>% 
    layer_dense(units = 10, activation = "softmax")
  
  model %>% compile(
    loss = "categorical_crossentropy",
    optimizer = "adam",
    metrics = list("accuracy")
  )
  model
}

model <- build_model()
model %>% summary()

print_dot_callback <- callback_lambda(
  on_epoch_end = function(epoch, logs) {
    if (epoch %% 80 == 0) cat("\n")
    cat(".")
  }
)

early_stop <- callback_early_stopping(monitor = "val_loss", patience = 20)

epochs <- 100

model_history <- model %>% fit(
  x_train,
  y_train,
  epochs = epochs,
  validation_split = 0.2,
  verbose = 1,
  callbacks = list(early_stop, print_dot_callback)
)

score <- evaluate(model, x_test, y_test)

plot(model_history) + theme_bw() + xlab("") +
  labs(subtitle = "Model Evaluation | Accuracy -- MultiClass in the test set = 19 %")
