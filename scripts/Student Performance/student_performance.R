
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

dat <- read_csv2("data/student-por.csv") %>% clean_names() %>% as_tibble()

dat %<>% select(school, sex, age, address, famsize, pstatus, medu, fedu, traveltime, studytime, failures, schoolsup, famsup, activities, higher, internet, freetime, health, absences, g1, g2, g3)
dat %>% vis_miss()
dat %<>% na.omit()

# EDA ---------------------------------------------------------------------

y_name <- "G3"
w <- 4
h <- 2

p1 <- dat %>% ggplot(aes(x = school, y =  g3)) + geom_boxplot() + theme_minimal() + labs(title = paste("Escola G3"), x = "Escola", y = y_name)
p1 %>% ggsave(filename = "img/escola.png", dpi = "retina", width = w, height = h)

p2 <- dat %>% ggplot(aes(x = sex, y =  g3)) + geom_boxplot() + theme_minimal() + labs(title = paste("Sexo vs", " G3"), x = "Sexo", y = y_name)
p2 %>% ggsave(filename = "img/sexo.png", dpi = "retina", width = w, height = h)

p3 <- dat %>% mutate(age = age %>% as.character()) %>% ggplot(aes(x = age, y = g3)) + geom_boxplot() + theme_minimal() + labs(title = paste("Idade vs", " G3"), x = "Idade", y = y_name)
p3 %>% ggsave(filename = "img/idade.png", dpi = "retina", width = w, height = h)

p4 <- dat %>% mutate(address = ifelse(address=="U", "Urbano", "Rural")) %>% ggplot(aes(x = address, y = g3)) + geom_boxplot() + theme_minimal() + labs(title = paste("Endereço vs", " G3"), x = "Endereço", y = y_name)
p4 %>% ggsave(filename = "img/endereco.png", dpi = "retina", width = w, height = h)

p5 <- dat %>% mutate(famsize = ifelse(famsize=="LE3", "< 3", "> 3")) %>% ggplot(aes(x = famsize, y = g3)) + geom_boxplot() + theme_minimal() + labs(title = paste("Tamanho da família vs", " G3"), x = "Tamanho da família", y = y_name)
p5 %>% ggsave(filename = "img/tamanho_familia.png", dpi = "retina", width = w, height = h)

p6 <- dat %>% mutate(pstatus = ifelse(pstatus=="T", "Morando junto", "À parte")) %>% ggplot(aes(x = pstatus, y = g3)) + geom_boxplot() + theme_minimal() + labs(title = paste("Coabitação dos pais vs", " G3"), x = "Coabitação dos pais vs", y = y_name)
p6 %>% ggsave(filename = "img/coab_pais.png", dpi = "retina", width = w, height = h)

p7 <- dat %>% mutate(medu = ifelse(medu=="1", "E. Fundamental", ifelse(medu==2, "E. Médio", ifelse(medu==3, "E. Superior", "Nenhum")))) %>% ggplot(aes(x = medu, y = g3)) + geom_boxplot() + theme_minimal() + labs(title = paste("Educação da Mãe vs", " G3"), x = "Educação da Mãe", y = y_name)
p7 %>% ggsave(filename = "img/educ_mae.png", dpi = "retina", width = w, height = h)

p8 <- dat %>% mutate(fedu = ifelse(fedu=="1", "E. Fundamental", ifelse(fedu==2, "E. Médio", ifelse(fedu==3, "E. Superior", "Nenhum")))) %>% ggplot(aes(x = fedu, y = g3)) + geom_boxplot() + theme_minimal() + labs(title = paste("Educação do Pai vs", " G3"), x = "Educação do Pai", y = y_name)
p8 %>% ggsave(filename = "img/educ_pai.png", dpi = "retina", width = w, height = h)

p9 <- dat %>% mutate(traveltime2 = ifelse(traveltime==1, "<15min", ifelse(traveltime==2, "15 a 30min", ifelse(traveltime==3, "30 a 60min", "> 60min")))) %>% ggplot(aes(x = fct_reorder(traveltime2, traveltime), y = g3)) + geom_boxplot() + theme_minimal() + labs(title = paste("Tempo de Viagem vs", " G3"), x = "Tempo de Viagem", y = y_name)
p9 %>% ggsave(filename = "img/tempo_viagem.png", dpi = "retina", width = w, height = h)

p10 <- dat %>% mutate(studytime2 = ifelse(studytime==1, "<2h", ifelse(studytime==2, "2 a 5h", ifelse(studytime==3, "5 a 10h", "> 10h")))) %>% ggplot(aes(x = fct_reorder(studytime2, studytime), y = g3)) + geom_boxplot() + theme_minimal() + labs(title = paste("Tempo de Estudo vs", " G3"), x = "Tempo de Estudo", y = y_name)
p10 %>% ggsave(filename = "img/tempo_estudo.png", dpi = "retina", width = w, height = h)

p11 <- dat %>% mutate(failures = failures %>% as.character()) %>% ggplot(aes(x = failures, y = g3)) + geom_boxplot() + theme_minimal() + labs(title = paste("Reprovações vs", " G3"), x = "Reprovações", y = y_name)
p11 %>% ggsave(filename = "img/reprovacoes.png", dpi = "retina", width = w, height = h)

p12 <- dat %>% mutate(schoolsup = ifelse(schoolsup=="yes", "Sim", "Não")) %>% ggplot(aes(x = schoolsup, y = g3)) + geom_boxplot() + theme_minimal() + labs(title = paste("Suporte Educacional Extra vs", " G3"), x = "Suporte Educacional Extra", y = y_name)
p12 %>% ggsave(filename = "img/sup_educ_extra.png", dpi = "retina", width = w, height = h)

p13 <- dat %>% mutate(famsup  = ifelse(famsup =="yes", "Sim", "Não")) %>% ggplot(aes(x = famsup , y = g3)) + geom_boxplot() + theme_minimal() + labs(title = paste("Suporte Educacional da Família vs", " G3"), x = "Suporte Educacional da Família", y = y_name)
p13 %>% ggsave(filename = "img/sup_educ_fam.png", dpi = "retina", width = w, height = h)

p14 <- dat %>% mutate(activities  = ifelse(activities =="yes", "Sim", "Não")) %>% ggplot(aes(x = activities , y = g3)) + geom_boxplot() + theme_minimal() + labs(title = paste("Atividades Extracurriculares vs", " G3"), x = "Atividades Extracurriculares", y = y_name)
p14 %>% ggsave(filename = "img/ativ_extra.png", dpi = "retina", width = w, height = h)

p15 <- dat %>% mutate(higher  = ifelse(higher =="yes", "Sim", "Não")) %>% ggplot(aes(x = higher , y = g3)) + geom_boxplot() + theme_minimal() + labs(title = paste("Deseja cursar o ensino superior vs", " G3"), x = "Deseja cursar o ensino superior", y = y_name)
p15 %>% ggsave(filename = "img/desej_superior.png", dpi = "retina", width = w, height = h)

p16 <- dat %>% mutate(internet  = ifelse(internet =="yes", "Sim", "Não")) %>% ggplot(aes(x = internet , y = g3)) + geom_boxplot() + theme_minimal() + labs(title = paste("Acesso a Internet em casa vs", " G3"), x = "Acesso a Internet em casa", y = y_name)
p16 %>% ggsave(filename = "img/acesso_internet.png", dpi = "retina", width = w, height = h)

p17 <- dat %>% mutate(freetime = freetime %>% as.character()) %>% ggplot(aes(x = freetime , y = g3)) + geom_boxplot() + theme_minimal() + labs(title = paste("Tempo Livre Pós Escola vs", " G3"), x = "Tempo Livre Pós Escola", y = y_name)
p17 %>% ggsave(filename = "img/tempo_livre.png", dpi = "retina", width = w, height = h)

p18 <- dat %>% mutate(health = health %>% as.character()) %>% ggplot(aes(x = health , y = g3)) + geom_boxplot() + theme_minimal() + labs(title = paste("Estado de saúde atual vs", " G3"), x = "Estado de saúde atual", y = y_name)
p18 %>% ggsave(filename = "img/estado_saude.png", dpi = "retina", width = w, height = h)

p19 <- dat %>% ggplot(aes(x = absences , y = g3)) + geom_point() + theme_minimal() + labs(title = paste("Número de faltas vs", " G3"), x = "Número de faltas", y = y_name, subtitle = paste("cor:", cor(dat %>% select(absences, g3))[2,1] %>% round(2)))
p19 %>% ggsave(filename = "img/faltas.png", dpi = "retina", width = w, height = h)

p20 <- dat %>% ggplot(aes(x = g1 , y = g3)) + geom_point() + theme_minimal() + labs(title = paste("Nota do Período 1 (G1) vs", " G3"), x = "G1", y = y_name, subtitle = paste("cor:", cor(dat %>% select(g1, g3))[2,1] %>% round(2)))
p20 %>% ggsave(filename = "img/nota_per1.png", dpi = "retina", width = w, height = h)

p21 <- dat %>% ggplot(aes(x = g2 , y = g3)) + geom_point() + theme_minimal() + labs(title = paste("Nota do Período 2 (G2) vs", " G3"), x = "G2", y = y_name, subtitle = paste("cor:", cor(dat %>% select(g2, g3))[2,1] %>% round(2)))
p21 %>% ggsave(filename = "img/nota_per2.png", dpi = "retina", width = w, height = h)

# Data --------------------------------------------------------------------

dat <- dat %>% select(-c(famsize, pstatus, activities, absences))
dat %>% vis_miss()

dat %<>% 
  mutate(
    address = ifelse(address=="U", "Urbano", "Rural"),
    medu = ifelse(medu==1, "E. Fundamental", ifelse(medu==2, "E. Médio", ifelse(medu==3, "E. Superior", "Nenhum"))),
    fedu = ifelse(fedu==1, "E. Fundamental", ifelse(fedu==2, "E. Médio", ifelse(fedu==3, "E. Superior", "Nenhum"))),
    traveltime = ifelse(traveltime==1, "menos de 15min", ifelse(traveltime==2, "15 a 30min", ifelse(traveltime==3, "30 a 60min", "mais de 60min"))),
    studytime = ifelse(studytime==1, "menos de 2h", ifelse(studytime==2, "2 a 5h", ifelse(studytime==3, "5 a 10h", "mais de 10h"))),
    freetime = freetime %>% as.character(),
    health = health %>% as.character(),
    schoolsup = ifelse(schoolsup=="yes", "Sim", "Não"),
    famsup = ifelse(famsup =="yes", "Sim", "Não"),
    higher = ifelse(higher =="yes", "Sim", "Não"),
    internet  = ifelse(internet =="yes", "Sim", "Não")
  ) %>% 
  `colnames<-`(c("escola", "sexo", "idade", "endereco", "educ_mae", "educ_pai", "tempo_viagem", "tempo_estudo", "reprovacoes", "sup_educ_extra", "sup_educ_fam", "dejesa_ensino_superior", "acesso_internet", "tempo_livre", "estado_saude", "g1", "g2", "g3")) %>% na.omit()

# Model -------------------------------------------------------------------

# Training/Teste
set.seed(2021)
dat_split <- initial_split(dat, strata = escola) #
dat_train <- training(dat_split)
dat_test <- testing(dat_split)

# Data Prep
dat_rec <- recipe(g3 ~ ., data = dat_train) %>%
  step_normalize(all_numeric(), -all_outcomes()) %>% 
  step_dummy(all_nominal(), -all_outcomes())

juice(prep(dat_rec)) # Visualizar a base de dados tratada

# Model - Ridge -----------------------------------------------------------

ridge_spec <- linear_reg(penalty = 0.1, mixture = 0) %>% set_engine("glmnet")

wf <- workflow() %>% add_recipe(dat_rec)

ridge_fit <- wf %>% add_model(ridge_spec) %>% fit(data = dat_train)

ridge_fit %>% pull_workflow_fit() %>% tidy()

set.seed(2021)
ridge_resamples <- vfold_cv(dat_train, strata = escola)

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
  labs(x = expression(lambda), y = "Média"); p1_ridge

p1_ridge %>% ggsave(filename = "img/portugues_r2_rmse_ridge.png", dpi = "retina", width = 7, height = 5)

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

p2_ridge %>% ggsave(filename = "img/portugues_importancia_ridge.png", dpi = "retina", width = 7, height = 4)

# Ajusta o melhor modelo final ao conjunto de treinamento e avalia o conjunto de teste
# https://tune.tidymodels.org/reference/last_fit.html
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
lasso_resamples <- vfold_cv(dat_train, strata = escola)

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
  labs(x = expression(lambda), y = "Média"); p1_lasso

p1_lasso %>% ggsave(filename = "img/portugues_r2_rmse_lasso.png", dpi = "retina", width = 7, height = 5)

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

p2_lasso %>% ggsave(filename = "img/portugues_importancia_lasso.png", dpi = "retina", width = 7, height = 4)

# Ajusta o melhor modelo final ao conjunto de treinamento e avalia o conjunto de teste
# https://tune.tidymodels.org/reference/last_fit.html
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
