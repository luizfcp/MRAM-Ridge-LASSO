
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

dat <- read_csv2("data/Behavior of the urban traffic of the city of Sao Paulo in Brazil.csv") %>% clean_names() %>% as_tibble()
dat %>% vis_miss()
dat %<>% na.omit()

# EDA ---------------------------------------------------------------------

y_name <- "Lentidão no Trânsito (%)"
w <- 4
h <- 2.15

p1 <- dat %>% ggplot(aes(x = hour_coded, y =  slowness_in_traffic_percent)) + geom_point() + theme_minimal() + labs(title = paste("ID da Hora vs\n", "Lentidão no Trânsito (%)"), x = "ID da Hora", y = y_name, subtitle = paste("cor:", cor(dat %>% select(hour_coded, slowness_in_traffic_percent))[2,1] %>% round(2)))
p1 %>% ggsave(filename = "img/hora.png", dpi = "retina", width = w, height = h)

p2 <- dat %>% mutate(immobilized_bus = immobilized_bus %>% as.character()) %>% ggplot(aes(x = immobilized_bus, y =  slowness_in_traffic_percent)) + geom_boxplot() + theme_minimal() + labs(title = paste("Ônibus imobilizado vs\n", "Lentidão no Trânsito (%)"), x = "Ônibus imobilizado", y = y_name)
p2 %>% ggsave(filename = "img/onibus.png", dpi = "retina", width = w, height = h)

p3 <- dat %>% mutate(broken_truck = broken_truck %>% as.character()) %>% ggplot(aes(x = broken_truck, y = slowness_in_traffic_percent)) + geom_boxplot() + theme_minimal() + labs(title = paste("Caminhão quebrado vs\n", "Lentidão no Trânsito (%)"), x = "Caminhão quebrado", y = y_name)
p3 %>% ggsave(filename = "img/caminhao_quebrado.png", dpi = "retina", width = w, height = h)

p4 <- dat %>% mutate(vehicle_excess = vehicle_excess %>% as.character()) %>% ggplot(aes(x = vehicle_excess, y = slowness_in_traffic_percent)) + geom_boxplot() + theme_minimal() + labs(title = paste("Excesso de Veículos vs\n", "Lentidão no Trânsito (%)"), x = "Excesso de Veículos", y = y_name)
p4 %>% ggsave(filename = "img/excesso_veiculos.png", dpi = "retina", width = w, height = h)

p5 <- dat %>% mutate(accident_victim = accident_victim %>% as.character()) %>% ggplot(aes(x = accident_victim, y = slowness_in_traffic_percent)) + geom_boxplot() + theme_minimal() + labs(title = paste("Vítimas no Acidente vs\n", "Lentidão no Trânsito (%)"), x = "Vítimas no Acidente", y = y_name)
p5 %>% ggsave(filename = "img/vitimas_acidente.png", dpi = "retina", width = w, height = h)

p6 <- dat %>% mutate(running_over = running_over %>% as.character()) %>% ggplot(aes(x = running_over, y = slowness_in_traffic_percent)) + geom_boxplot() + theme_minimal() + labs(title = paste("Atropelamento vs\n", "Lentidão no Trânsito (%)"), x = "Atropelamento", y = y_name)
p6 %>% ggsave(filename = "img/atropelamento.png", dpi = "retina", width = w, height = h)

p7 <- dat %>% mutate(fire_vehicles = fire_vehicles %>% as.character()) %>% ggplot(aes(x = fire_vehicles, y = slowness_in_traffic_percent)) + geom_boxplot() + theme_minimal() + labs(title = paste("Veículos Incendiados vs\n", "Lentidão no Trânsito (%)"), x = "Veículos Incendiados", y = y_name)
p7 %>% ggsave(filename = "img/veiculos_fogo.png", dpi = "retina", width = w, height = h)

p8 <- dat %>% mutate(occurrence_involving_freight = occurrence_involving_freight %>% as.character()) %>% ggplot(aes(x = occurrence_involving_freight, y = slowness_in_traffic_percent)) + geom_boxplot() + theme_minimal() + labs(title = paste("Ocorrência envolvendo carga vs\n", "Lentidão no Trânsito (%)"), x = "Ocorrência envolvendo carga", y = y_name)
p8 %>% ggsave(filename = "img/ocorrencia_carga.png", dpi = "retina", width = w, height = h)

p9 <- dat %>% mutate(incident_involving_dangerous_freight = incident_involving_dangerous_freight %>% as.character()) %>% ggplot(aes(x = incident_involving_dangerous_freight, y = slowness_in_traffic_percent)) + geom_boxplot() + theme_minimal() + labs(title = paste("Incidente envolvendo carga perigosa vs\n", "Lentidão no Trânsito (%)"), x = "Incidente envolvendo carga perigosa", y = y_name)
p9 %>% ggsave(filename = "img/carga_perigosa.png", dpi = "retina", width = w, height = h)

p10 <- dat %>% mutate(lack_of_electricity = lack_of_electricity %>% as.character()) %>% ggplot(aes(x = lack_of_electricity, y = slowness_in_traffic_percent)) + geom_boxplot() + theme_minimal() + labs(title = paste("Falta de eletricidade vs\n", "Lentidão no Trânsito (%)"), x = "Falta de eletricidade", y = y_name)
p10 %>% ggsave(filename = "img/falta_eletricidade.png", dpi = "retina", width = w, height = h)

p11 <- dat %>% mutate(fire = fire %>% as.character()) %>% ggplot(aes(x = fire, y = slowness_in_traffic_percent)) + geom_boxplot() + theme_minimal() + labs(title = paste("Incêndio vs\n", "Lentidão no Trânsito (%)"), x = "Incêndio", y = y_name)
p11 %>% ggsave(filename = "img/incendio.png", dpi = "retina", width = w, height = h)

p12 <- dat %>% mutate(point_of_flooding = point_of_flooding %>% as.character()) %>% ggplot(aes(x = point_of_flooding, y = slowness_in_traffic_percent)) + geom_boxplot() + theme_minimal() + labs(title = paste("Ponto de alagamento vs\n", "Lentidão no Trânsito (%)"), x = "Ponto de alagamento", y = y_name)
p12 %>% ggsave(filename = "img/ponto_alagamento.png", dpi = "retina", width = w, height = h)

p13 <- dat %>% mutate(manifestations = manifestations %>% as.character()) %>% ggplot(aes(x = manifestations, y = slowness_in_traffic_percent)) + geom_boxplot() + theme_minimal() + labs(title = paste("Manifestação vs\n", "Lentidão no Trânsito (%)"), x = "Manifestação", y = y_name)
p13 %>% ggsave(filename = "img/manifestacao.png", dpi = "retina", width = w, height = h)

p14 <- dat %>% mutate(defect_in_the_network_of_trolleybuses = defect_in_the_network_of_trolleybuses %>% as.character()) %>% ggplot(aes(x = defect_in_the_network_of_trolleybuses, y = slowness_in_traffic_percent)) + geom_boxplot() + theme_minimal() + labs(title = paste("Defeito na rede vs\n", "Lentidão no Trânsito (%)"), x = "Defeito na rede", y = y_name)
p14 %>% ggsave(filename = "img/defeito_rede.png", dpi = "retina", width = w, height = h)

p15 <- dat %>% mutate(tree_on_the_road = tree_on_the_road %>% as.character()) %>% ggplot(aes(x = tree_on_the_road, y = slowness_in_traffic_percent)) + geom_boxplot() + theme_minimal() + labs(title = paste("Árvore na estrada vs\n", "Lentidão no Trânsito (%)"), x = "Árvore na estrada", y = y_name)
p15 %>% ggsave(filename = "img/arvore.png", dpi = "retina", width = w, height = h)

p16 <- dat %>% mutate(semaphore_off = semaphore_off %>% as.character()) %>% ggplot(aes(x = semaphore_off, y = slowness_in_traffic_percent)) + geom_boxplot() + theme_minimal() + labs(title = paste("Semáforo desligado vs\n", "Lentidão no Trânsito (%)"), x = "Semáforo desligado", y = y_name)
p16 %>% ggsave(filename = "img/semaforo_off.png", dpi = "retina", width = w, height = h)

p17 <- dat %>% mutate(intermittent_semaphore = intermittent_semaphore %>% as.character()) %>% ggplot(aes(x = intermittent_semaphore, y = slowness_in_traffic_percent)) + geom_boxplot() + theme_minimal() + labs(title = paste("Semáforo intermitente vs\n", "Lentidão no Trânsito (%)"), x = "Semáforo intermitente", y = y_name)
p17 %>% ggsave(filename = "img/semaforo_intermitente.png", dpi = "retina", width = w, height = h)

# Data --------------------------------------------------------------------

dat %<>% 
  select(-c(fire_vehicles, occurrence_involving_freight, incident_involving_dangerous_freight, fire, defect_in_the_network_of_trolleybuses, intermittent_semaphore)) %>% 
  `colnames<-`(c("id_hora", "onibus_imobilizado", "caminhao_quebrado", "excesso_veiculo", "vitimas_acidente", "atropelamento", "falta_eletricidade", "ponto_alagamento", "manifestacoes", "arvore_estrada", "semaforo_desligado", "percentual_lentidao_trafego")) %>% na.omit()
dat %>% vis_miss()

# Model -------------------------------------------------------------------

# Training/Teste
set.seed(2021)
dat_split <- initial_split(dat)
dat_train <- training(dat_split)
dat_test <- testing(dat_split)

# Data Prep
dat_rec <- recipe(percentual_lentidao_trafego ~ ., data = dat_train) %>%
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

p1_ridge %>% ggsave(filename = "img/trafego_r2_rmse_ridge.png", dpi = "retina", width = 7, height = 5)

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

p2_ridge %>% ggsave(filename = "img/trafego_importancia_ridge.png", dpi = "retina", width = 7, height = 3)

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

p1_lasso %>% ggsave(filename = "img/trafego_r2_rmse_lasso.png", dpi = "retina", width = 7, height = 5)

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

p2_lasso %>% ggsave(filename = "img/trafego_importancia_lasso.png", dpi = "retina", width = 7, height = 3)

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
