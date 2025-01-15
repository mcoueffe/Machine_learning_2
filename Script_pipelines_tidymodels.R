# ------------------------ Pipelines de traitement et TidyModels

library(tidymodels)
library(arrow)

data <- read_parquet("readmission_avc.parquet")
head(data)

# Objectif : prédire si les personnes qui ont fait un AVC vont revenir à l'hopital

# 0. ---- Nettoyage des données

# Variables manquantes
colSums(is.na(data))

# Valeurs manquantes dans nbda (comorbidités) : vraies valeurs manquantes ou 0 ?
unique(data$nbda) # pas de 0

# Transformation des variables catégorielles 
dataset <- data %>% filter(!is.na(id_D)) %>% 
  mutate(target = as.factor(ifelse(id_D == "", 0, 1))) %>% 
  mutate_at(c("sexe", "modeEntree", "modeSortie"), as.character) %>% 
  mutate(nbda = ifelse(is.na(nbda), 0, 1)) %>% 
  filter(modeSortie != 9) %>% 
  select(-c(id, id_D))# on supprime les personnes décédées car ils ne vont pas revenir

# 1. ---- Data Split : Resample library
# On fait un échantillon, en précisant avec strata qu'on doit avoir la même proportion
# de target dans les deux échantillons
set.seed(42)
dataset_split <- initial_split(dataset, prop = 0.9, strata = target) 

train_val_set <- training(dataset_split)
test_set <- testing(dataset_split)

# Nouveau split train et validation
train_val_split <- initial_split(train_val_set, prop = 0.8, strata = target)
train_set <- training(train_val_split)
val_set <- testing(train_val_split)

cat(dim(train_set), dim(val_set), dim(test_set))

# 2. ----- Features engineering : Recipes library

# Recettes de base, variables sans transformation
rec_basic <- recipe(data = train_set, target ~ .) %>%
  step_impute_mode(sexe) %>% # on impute les NA avec la valeur la plus fréquente avec mode
  step_impute_mean(age) %>%  # on impute les NA avec la moyenne
  step_normalize(age) %>%  # on met à l'échelle
  step_dummy(modeEntree) %>% # on code les variables catégorielles
  step_other(dp, threshold = 0.03) %>% # on regroupe les modilités ayant une fréquence inférieure à 3 %
  step_dummy(dp)

prep(rec_basic) # on cuisine la recette
juice(prep(rec_basic)) # on presse le plat préparé
formula(prep(rec_basic))

# Version générale
rec_basic <- recipe(data = train_set, target ~ .) %>% 
  step_impute_mean(all_numeric_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_impute_mode(all_nominal_predictors()) %>% 
  step_other(dp, threshold = 0.03) %>% 
  step_other(ghm2, threshold = 0.01) %>%
  step_dummy(all_nominal_predictors())
juice(prep(rec_basic))

# 3. ---- Modélisation : Parsnip library

## 3.1 Logistical model and workflow

log_mod <- logistic_reg() %>% 
  set_engine("glm") %>% # fait appel à un package existant et harmonise
  set_mode("classification")

# pipeline = workflow
log_wf <- workflow() %>% 
  add_recipe(rec_basic) %>% 
  add_model(log_mod)

log_fitted <- log_wf %>% fit(train_set)

log_fitted %>% predict(val_set, type = "prob")
log_fitted %>% predict(val_set)

log_pred <- val_set %>% select(target) %>% 
  bind_cols(log_fitted %>% predict(val_set),
            log_fitted %>% predict(val_set, type = "prob"))
log_pred

accuracy(log_pred, truth = target, .pred_class)
roc_auc(log_pred, truth = target, .pred_0)
roc_auc(log_pred, truth = target, .pred_1, event_level = "second")

## 3.2 Random Forest workflow
library(ranger)

rf_mod <- rand_forest(
  trees = tune(),
  #mtry = tune(),
  min_n = tune()
) %>% 
  set_engine("ranger") %>% 
  set_mode("classification")

rf_wf <- workflow() %>% 
  add_recipe(rec_basic) %>% 
  add_model(rf_mod)

rf_grid <- expand.grid(
  trees = c(50, 100, 200),
  min_n = c(1, 2, 5)
)

set.seed(42)
cv_split <- vfold_cv(train_val_set, v= 5, strata = target)

rf_grid_fitted <- tune_grid(
  rf_wf,
  resamples = cv_split,
  grid = rf_grid,
  metrics = metric_set(accuracy, roc_auc, f_meas)
)
