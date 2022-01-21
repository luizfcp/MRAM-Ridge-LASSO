
# Pacotes -----------------------------------------------------------------

library(readr)
library(janitor)
library(magrittr)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidymodels)
library(patchwork)
library(lubridate)

# Data --------------------------------------------------------------------

data <- read.csv("data/Consumo_cerveja.csv") %>% clean_names() %>% as_tibble()
data %<>%
  select(data, consumo_de_cerveja_litros, temperatura_media_c) %>% 
  na.omit() %>% 
  mutate_at(2:3, ~ .x %>% str_replace_all(",", ".") %>% as.numeric()) %>% 
  mutate(data = ymd(data))

# EDA ---------------------------------------------------------------------

p1 <- data %>% 
  ggplot(aes(x = data, y = temperatura_media_c)) + 
  geom_line() +
  labs(x = "Data", y = "Temperatura Média (°C)") +
  theme_minimal(); p1
ggsave("img/consumo_cerveja_temp_media.png", plot = p1, dpi = "retina", width = 5, height = 3)

p2 <- data %>% 
  ggplot(aes(x = data, y = consumo_de_cerveja_litros)) +
  geom_line() +
  labs(x = "Data", y = "Consumo de Cerveja (litros)") +
  theme_minimal(); p2
ggsave("img/consumo_cerveja_litros.png", plot = p2, dpi = "retina", width = 5, height = 3)

p3 <- data %>% 
  ggplot(aes(x = consumo_de_cerveja_litros, y = temperatura_media_c)) + 
  geom_point(size=2) +
  labs(x="Temperatura Média (°C)", y="Consumo de Cerveja (litros)") +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5)); p3
ggsave("img/consumo_cerveja_pontos.png", plot = p3, dpi = "retina", width = 5, height = 3)

# Model -------------------------------------------------------------------

set.seed(1000)
fit <- lm(consumo_de_cerveja_litros~temperatura_media_c, data)
summary(fit)
predict(fit, data.frame(temperatura_media_c=30))

# Tabela 1 exemplo --------------------------------------------------------

data %>% 
  mutate(data = data %>% ymd() %>% format("%d/%m/%Y")) %>% 
  mutate_at(2:3, ~ .x %>% comma(accuracy = 0.001, decimal.mark = ",")) %>% 
  `colnames<-`(c("Data", "Consumo de Cerveja (litros)", "Temperatura Média Ambiente (°C)")) %>% 
  head(6)

############################## Ridge - LASSO ##############################

# Data --------------------------------------------------------------------

data <- read.csv("data/Consumo_cerveja.csv") %>% clean_names() %>% as_tibble()
data %<>% 
  select(data, consumo_de_cerveja_litros, temperatura_media_c, precipitacao_mm) %>% 
  na.omit() %>% 
  mutate_at(2:4, ~ .x %>% str_replace_all(",", ".") %>% as.numeric()) %>% 
  mutate(data = ymd(data))

# Tabela 2 exemplo --------------------------------------------------------

data %>% 
  mutate(data = data %>% ymd() %>% format("%d/%m/%Y")) %>% 
  mutate_at(2:4, ~ .x %>% comma(accuracy = 0.001, decimal.mark = ",")) %>% 
  `colnames<-`(c("Data", "Consumo de Cerveja (litros)", "Temperatura Média Ambiente (°C)", "Precipitação (mm)")) %>% 
  head(6)

# Data --------------------------------------------------------------------

data %<>% select(-data)

# Model -------------------------------------------------------------------

# Training/Teste
set.seed(2021)
data_split <- initial_split(data)
data_train <- training(data_split)
data_test <- testing(data_split)

# Data Prep
data_rec <- recipe(consumo_de_cerveja_litros ~ ., data = data_train) %>%
  step_normalize(all_numeric(), -all_outcomes())

juice(prep(data_rec)) # Visualizar a base de dados tratada

# Model - Ridge -----------------------------------------------------------

ridge_spec <- linear_reg(penalty = 0.1, mixture = 0) %>% set_engine("glmnet")
wf <- workflow() %>% add_recipe(data_rec)
ridge_fit <- wf %>% add_model(ridge_spec) %>% fit(data = data_train)

set.seed(2021)
ridge_resamples <- vfold_cv(data_train)
tune_spec <- linear_reg(penalty = tune(), mixture = 0) %>% set_engine("glmnet")
lambda_grid <- grid_regular(penalty(), levels = 50)

doParallel::registerDoParallel()
set.seed(2021)
ridge_grid <- tune_grid(
  wf %>% add_model(tune_spec),
  resamples = ridge_resamples,
  grid = lambda_grid
)

ridge_grid %>% collect_metrics()

p1_cc_ridge <- ridge_grid %>%
  collect_metrics() %>%
  mutate(.metric = ifelse(.metric=="rmse", "RMSE", "R2")) %>% 
  ggplot(aes(penalty, mean, color = .metric)) +
  geom_errorbar(aes(ymin = mean - std_err, ymax = mean + std_err), alpha = 0.5) +
  geom_line(size = 1.5) +
  facet_wrap(~.metric, scales = "free", nrow = 2) +
  scale_x_log10() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = expression(lambda), y = "Média", title = "Ridge"); p1_cc_ridge

lowest_rmse <- ridge_grid %>% select_best("rmse", maximize = FALSE)

final_ridge <- finalize_workflow(wf %>% add_model(tune_spec), lowest_rmse)

ridge <- last_fit(final_ridge, data_split) %>% collect_metrics()

ridge %>% 
  mutate(.metric = ifelse(.metric=="rmse", "RMSE", "R2")) %>% 
  select(1,3) %>% 
  `colnames<-`(c("Métrica", "Estimativa"))

# LASSO -------------------------------------------------------------------

lasso_spec <- linear_reg(penalty = 0.1, mixture = 1) %>% set_engine("glmnet")
wf <- workflow() %>% add_recipe(data_rec)
lasso_fit <- wf %>% add_model(lasso_spec) %>% fit(data = data_train)

set.seed(2021)
lasso_resamples <- vfold_cv(data_train)
tune_spec <- linear_reg(penalty = tune(), mixture = 1) %>% set_engine("glmnet")
lambda_grid <- grid_regular(penalty(), levels = 50)

doParallel::registerDoParallel()
set.seed(2021)
lasso_grid <- tune_grid(
  wf %>% add_model(tune_spec), 
  resamples = lasso_resamples, 
  grid = lambda_grid
)

p1_cc_lasso <- lasso_grid %>%
  collect_metrics() %>%
  mutate(.metric = ifelse(.metric=="rmse", "RMSE", "R2")) %>% 
  ggplot(aes(penalty, mean, color = .metric)) +
  geom_errorbar(aes(ymin = mean - std_err,ymax = mean + std_err), alpha = 0.5) +
  geom_line(size = 1.5) +
  facet_wrap(~.metric, scales = "free", nrow = 2) +
  scale_x_log10() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = expression(lambda), y = "Média", title = "LASSO"); p1_cc_lasso

lowest_rmse <- lasso_grid %>% select_best("rmse", maximize = FALSE)
final_lasso <- finalize_workflow(wf %>% add_model(tune_spec), lowest_rmse)

lasso <- last_fit(final_lasso, data_split) %>% collect_metrics()

lasso %>% 
  mutate(.metric = ifelse(.metric=="rmse", "RMSE", "R2")) %>% 
  select(1,3) %>% 
  `colnames<-`(c("Métrica", "Estimativa"))

# Imagem Ride - LASSO -----------------------------------------------------

{p1_cc_ridge + p1_cc_lasso}
{p1_cc_ridge + p1_cc_lasso} %>% ggsave(filename = "img/cc_r2_rmse_lasso_ridge.png", dpi = "retina", width = 7, height = 5)

