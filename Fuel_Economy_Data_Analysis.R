library(tidyverse) 
library(skimr)
library(inspectdf)
library(mice)
library(plotly)
library(h2o)
library(caret)

# 1. Add ggplot2::mpg dataset.
data <- ggplot2::mpg

# 2. Make data ready for analysis doing preprocessing techniques.
data %>% skim()

data %>% inspect_na()


dummy <- data %>% select(drv,fl,class,trans)
not_dummy <- data %>% select(!drv) %>% select(!fl) %>% select (!class) %>% select(!trans)

dummy <- dummyVars(" ~ .", data = dummy) %>% 
  predict(newdata = dummy) %>% 
  as.data.frame()

df <- cbind(dummy,not_dummy)

target <- df %>% select(cty)
features <- df %>% select(year,cyl,displ)


# 3. Fit Generalized Linear Model using H2O in R.
h2o.init()

h2o_data <- df %>% as.h2o()

h2o_data <- h2o_data %>% h2o.splitFrame(ratios = 0.8, seed = 123)
train <- h2o_data[[1]]
test <- h2o_data[[2]]


# 4. Run GLM using following modelling structure. cty ~ year + cyl + displ.
f <- as.formula(paste(target, paste(features, collapse = " + "), sep = " ~ "))
glm <- glm(f, data = df)  #glm=== linear regression

glm %>% summary()


# 5. Print coefficients table and give interpretation of results.
model <- h2o.glm(
  x = features %>% names(), y = target %>% names(),
  training_frame = train,
  validation_frame = test,
  nfolds = 10, seed = 123,
  lambda = 0, compute_p_values = T)

model@model$coefficients_table %>%
  as.data.frame() %>%
  dplyr::select(names,p_value) %>%
  mutate(p_value = round(p_value,3)) %>%
  .[-1,] %>%
  arrange(desc(p_value))

# Nəticə:cty column`a ən az təsir edən column Year column`dur 