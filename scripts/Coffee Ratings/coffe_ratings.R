
# Pacotes -----------------------------------------------------------------

library(readr)
library(magrittr)
library(janitor)
library(dplyr)
library(stringr)
library(ggplot2)
library(patchwork)
library(tidymodels)
library(vip)
library(naniar)
library(forcats)
library(tidytuesdayR)

# Data --------------------------------------------------------------------

dat <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv') %>% 
  mutate(color = ifelse(color=="None", NA, color)) %>% 
  select(-c(owner, farm_name:altitude, producer:variety, expiration:altitude_high_meters, country_of_origin, region, cupper_points))

dat %>% vis_miss() # visualizar dados faltantes
dat %<>% na.omit() # excluir linhas com dados faltantes

# EDA ---------------------------------------------------------------------

y_name <- "Pontuação"
w <- 4
h <- 2.15

p1 <- dat %>% ggplot(aes(x = species, y =  total_cup_points)) + geom_boxplot() + theme_minimal() + labs(title = paste("Espécies vs", "Pontuação"), x = "Espécies", y = y_name)
p1 %>% ggsave(filename = "img/especies.png", dpi = "retina", width = w, height = h)

p3 <- dat %>% ggplot(aes(x = processing_method, y = total_cup_points)) + geom_boxplot() + theme_minimal() + labs(title = paste("Método de Processamento vs", "Pontuação"), x = "Método de processamento", y = y_name)
p3 %>% ggsave(filename = "img/metodo_processamento.png", dpi = "retina", width = 8, height = h)

p4 <- dat %>% ggplot(aes(x = color, y = total_cup_points)) + geom_boxplot() + theme_minimal() + labs(title = paste("Cor vs", "Pontuação"), x = "Cor", y = y_name)
p4 %>% ggsave(filename = "img/cor.png", dpi = "retina", width = w, height = h)

p5 <- dat %>% ggplot(aes(x = aroma, y = total_cup_points)) + geom_point() + theme_minimal() + labs(title = paste("Aroma vs", "Pontuação"), x = "Aroma", y = y_name, subtitle = paste("cor:", cor(dat %>% select(aroma, total_cup_points))[2,1] %>% round(2)))
p5 %>% ggsave(filename = "img/aroma.png", dpi = "retina", width = w, height = h)

p6 <- dat %>% ggplot(aes(x = flavor, y = total_cup_points)) + geom_point() + theme_minimal() + labs(title = paste("Sabor vs", "Pontuação"), x = "Sabor", y = y_name, subtitle = paste("cor:", cor(dat %>% select(flavor, total_cup_points))[2,1] %>% round(2)))
p6 %>% ggsave(filename = "img/sabor.png", dpi = "retina", width = w, height = h)

p7 <- dat %>% ggplot(aes(x = aftertaste, y = total_cup_points)) + geom_point() + theme_minimal() + labs(title = paste("Gosto Residual vs", "Pontuação"), x = "Gosto Residual", y = y_name, subtitle = paste("cor:", cor(dat %>% select(aftertaste, total_cup_points))[2,1] %>% round(2)))
p7 %>% ggsave(filename = "img/gosto_residual.png", dpi = "retina", width = w, height = h)

p8 <- dat %>% ggplot(aes(x = acidity, y = total_cup_points)) + geom_point() + theme_minimal() + labs(title = paste("Acidez vs", "Pontuação"), x = "Acidez", y = y_name, subtitle = paste("cor:", cor(dat %>% select(acidity, total_cup_points))[2,1] %>% round(2)))
p8 %>% ggsave(filename = "img/acidez.png", dpi = "retina", width = w, height = h)

p9 <- dat %>% ggplot(aes(x = body, y = total_cup_points)) + geom_point() + theme_minimal() + labs(title = paste("Corpo vs", "Pontuação"), x = "Corpo", y = y_name, subtitle = paste("cor:", cor(dat %>% select(body, total_cup_points))[2,1] %>% round(2)))
p9 %>% ggsave(filename = "img/corpo.png", dpi = "retina", width = w, height = h)

p10 <- dat %>% ggplot(aes(x = balance, y = total_cup_points)) + geom_point() + theme_minimal() + labs(title = paste("Balanço vs", "Pontuação"), x = "Balanço", y = y_name, subtitle = paste("cor:", cor(dat %>% select(balance, total_cup_points))[2,1] %>% round(2)))
p10 %>% ggsave(filename = "img/balanco.png", dpi = "retina", width = w, height = h)

p11 <- dat %>% ggplot(aes(x = uniformity, y = total_cup_points)) + geom_point() + theme_minimal() + labs(title = paste("Uniformidade vs", "Pontuação"), x = "Uniformidade", y = y_name, subtitle = paste("cor:", cor(dat %>% select(uniformity, total_cup_points))[2,1] %>% round(2)))
p11 %>% ggsave(filename = "img/uniformidade.png", dpi = "retina", width = w, height = h)

p12 <- dat %>% ggplot(aes(x = clean_cup, y = total_cup_points)) + geom_point() + theme_minimal() + labs(title = paste("Limpeza do Copo vs", "Pontuação"), x = "Limpeza do Copo", y = y_name, subtitle = paste("cor:", cor(dat %>% select(clean_cup, total_cup_points))[2,1] %>% round(2)))
p12 %>% ggsave(filename = "img/limpeza_copo.png", dpi = "retina", width = w, height = h)

p13 <- dat %>% ggplot(aes(x = sweetness, y = total_cup_points)) + geom_point() + theme_minimal() + labs(title = paste("Doçura vs", "Pontuação"), x = "Doçura", y = y_name, subtitle = paste("cor:", cor(dat %>% select(sweetness, total_cup_points))[2,1] %>% round(2)))
p13 %>% ggsave(filename = "img/docura.png", dpi = "retina", width = w, height = h)

p14 <- dat %>% ggplot(aes(x = moisture, y = total_cup_points)) + geom_point() + theme_minimal() + labs(title = paste("Umidade vs", "Pontuação"), x = "Umidade", y = y_name, subtitle = paste("cor:", cor(dat %>% select(moisture, total_cup_points))[2,1] %>% round(2)))
p14 %>% ggsave(filename = "img/umidade.png", dpi = "retina", width = w, height = h)

p15 <- dat %>% ggplot(aes(x = category_one_defects, y = total_cup_points)) + geom_point() + theme_minimal() + labs(title = paste("Categoria de Defeitos 1 vs", "Pontuação"), x = "Categoria de Defeitos 1", y = y_name, subtitle = paste("cor:", cor(dat %>% select(category_one_defects, total_cup_points))[2,1] %>% round(2)))
p15 %>% ggsave(filename = "img/catagoria_defeitos_1.png", dpi = "retina", width = w, height = h)

p16 <- dat %>% ggplot(aes(x = category_two_defects, y = total_cup_points)) + geom_point() + theme_minimal() + labs(title = paste("Categoria de Defeitos 2 vs", "Pontuação"), x = "Categoria de Defeitos 2", y = y_name, subtitle = paste("cor:", cor(dat %>% select(category_two_defects, total_cup_points))[2,1] %>% round(2)))
p16 %>% ggsave(filename = "img/catagoria_defeitos_2.png", dpi = "retina", width = w, height = h)

p17 <- dat %>% ggplot(aes(x = altitude_mean_meters, y = total_cup_points)) + geom_point() + theme_minimal() + labs(title = paste("Altitude Média (m) vs", "Pontuação"), x = "Altitude Média (m)", y = y_name, subtitle = paste("cor:", cor(dat %>% select(altitude_mean_meters, total_cup_points))[2,1] %>% round(2)))
p17 %>% ggsave(filename = "img/altitude_media.png", dpi = "retina", width = w, height = h)

p18 <- dat %>% ggplot(aes(x = quakers, y = total_cup_points)) + geom_point() + theme_minimal() + labs(title = paste("Quakers vs", "Pontuação"), x = "Quakers", y = y_name, subtitle = paste("cor:", cor(dat %>% select(quakers, total_cup_points))[2,1] %>% round(2)))
p18 %>% ggsave(filename = "img/quakers.png", dpi = "retina", width = w, height = h)

# Data --------------------------------------------------------------------

dat %<>% select(-c(altitude_mean_meters, category_one_defects, category_two_defects, quakers, moisture))
dat %>% vis_miss()
dat %<>% `colnames<-`(c("pontuacao", "especies", "metodo_processamento", "aroma", "sabor", "gosto_residual", "acidez", "corpo", "balanco", "uniformidade", "limpeza_copo", "docura", "cor")) %>% na.omit()

# Model -------------------------------------------------------------------

# Training/Teste
set.seed(2021)
dat_split <- initial_split(dat)
dat_train <- training(dat_split)
dat_test <- testing(dat_split)

# Data Prep
dat_rec <- recipe(pontuacao ~ ., data = dat_train) %>%
  step_normalize(all_numeric(), -all_outcomes()) %>% 
  step_dummy(all_nominal(), -all_outcomes())

# Model - Ridge -----------------------------------------------------------

ridge_spec <- linear_reg(penalty = 0.1, mixture = 0) %>% set_engine("glmnet")

wf <- workflow() %>% add_recipe(dat_rec)

ridge_fit <- wf %>% add_model(ridge_spec) %>% fit(data = dat_train)

ridge_fit %>% pull_workflow_fit() %>% tidy()

set.seed(2021)
ridge_resamples <- vfold_cv(dat_train)

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

p1_ridge <- ridge_grid %>%
  collect_metrics() %>%
  mutate(.metric = ifelse(.metric=="rmse", "RMSE", "R2")) %>% 
  ggplot(aes(penalty, mean, color = .metric)) +
  geom_errorbar(aes(
    ymin = mean - std_err,
    ymax = mean + std_err
  ),
  alpha = 0.5
  ) +
  geom_line(size = 1.5) +
  facet_wrap(~.metric, scales = "free", nrow = 2) +
  scale_x_log10() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Penalidade", y = "Média"); p1_ridge

p1_ridge %>% ggsave(filename = "img/r2_rmse_ridge.png", dpi = "retina", width = 7, height = 5)

lowest_rmse <- ridge_grid %>% select_best("rmse", maximize = FALSE)

final_ridge <- finalize_workflow(
  wf %>% add_model(tune_spec),
  lowest_rmse
)

p2_ridge <- final_ridge %>%
  fit(dat_train) %>%
  pull_workflow_fit() %>%
  vi(lambda = lowest_rmse$penalty) %>%
  mutate(
    Importance = abs(Importance),
    Variable = fct_reorder(Variable, Importance)
  ) %>%
  ggplot(aes(x = Importance, y = Variable, fill = Sign)) +
  geom_col() +
  scale_x_continuous(expand = c(0, 0)) +
  theme_minimal() +
  labs(y = NULL, x = "Importância", fill = "Sinal"); p2_ridge

p2_ridge %>% ggsave(filename = "img/importancia_ridge.png", dpi = "retina", width = 7, height = 3)

ridge <- last_fit(
  final_ridge,
  dat_split
) %>%
  collect_metrics()

ridge %>% 
  mutate(
    .metric = ifelse(.metric=="rmse", "RMSE", "R2")
  ) %>% 
  select(1,3) %>% 
  `colnames<-`(c("Métrica", "Estimativa"))

# Model - LASSO -----------------------------------------------------------

lasso_spec <- linear_reg(penalty = 0.1, mixture = 1) %>% set_engine("glmnet")

wf <- workflow() %>% add_recipe(dat_rec)

lasso_fit <- wf %>% add_model(lasso_spec) %>% fit(data = dat_train)

lasso_fit %>% pull_workflow_fit() %>% tidy()

set.seed(2021)
lasso_resamples <- vfold_cv(dat_train)

tune_spec <- linear_reg(penalty = tune(), mixture = 1) %>% set_engine("glmnet")

lambda_grid <- grid_regular(penalty(), levels = 50)

doParallel::registerDoParallel()

set.seed(2021)
lasso_grid <- tune_grid(
  wf %>% add_model(tune_spec),
  resamples = lasso_resamples,
  grid = lambda_grid
)

lasso_grid %>% collect_metrics()

p1_lasso <- lasso_grid %>%
  collect_metrics() %>%
  mutate(.metric = ifelse(.metric=="rmse", "RMSE", "R2")) %>% 
  ggplot(aes(penalty, mean, color = .metric)) +
  geom_errorbar(aes(
    ymin = mean - std_err,
    ymax = mean + std_err
  ),
  alpha = 0.5
  ) +
  geom_line(size = 1.5) +
  facet_wrap(~.metric, scales = "free", nrow = 2) +
  scale_x_log10() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Penalidade", y = "Média"); p1_lasso

p1_lasso %>% ggsave(filename = "img/r2_rmse_lasso.png", dpi = "retina", width = 7, height = 5)

lowest_rmse <- lasso_grid %>% select_best("rmse", maximize = FALSE)

final_lasso <- finalize_workflow(
  wf %>% add_model(tune_spec),
  lowest_rmse
)

p2_lasso <- final_lasso %>%
  fit(dat_train) %>%
  pull_workflow_fit() %>%
  vi(lambda = lowest_rmse$penalty) %>%
  mutate(
    Importance = abs(Importance),
    Variable = fct_reorder(Variable, Importance)
  ) %>%
  ggplot(aes(x = Importance, y = Variable, fill = Sign)) +
  geom_col() +
  scale_x_continuous(expand = c(0, 0)) +
  theme_minimal() +
  labs(y = NULL, x = "Importância", fill = "Sinal"); p2_lasso

p2_lasso %>% ggsave(filename = "img/importancia_lasso.png", dpi = "retina", width = 7, height = 3)

lasso <- last_fit(
  final_lasso,
  dat_split
) %>%
  collect_metrics()

lasso %>% 
  mutate(
    .metric = ifelse(.metric=="rmse", "RMSE", "R2")
  ) %>% 
  select(1,3) %>% 
  `colnames<-`(c("Métrica", "Estimativa"))

