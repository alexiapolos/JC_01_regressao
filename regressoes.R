# Instalar os pacotes necessários
install.packages("tidymodels")
install.packages("ggplot2")

# Carregar pacotes
library(tidymodels)
library(ggplot2)

# Divisão do dataset em treino e teste
set.seed(123)
data_split <- initial_split(subset, prop = 0.8)
train_data <- training(data_split)
test_data <- testing(data_split)


# Regressão Linear --------------------------------------------------------
linear_recipe <- recipe(idade ~ estadiamento, data = train_data)
linear_model <- linear_reg() %>% 
  set_engine("lm")
linear_workflow <- workflow() %>% 
  add_recipe(linear_recipe) %>% 
  add_model(linear_model)

linear_fit <- linear_workflow %>% 
  fit(data = train_data)

tidy(linear_fit)

ggplot(train_data, aes(x = estadiamento, y = idade)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Regressão Linear Simples", x = "Estadiamento", y = "Idade")

# Regressão múltipla ------------------------------------------------------
multiple_recipe <- recipe(idade ~ tabagismo + sexo + estadiamento, data = train_data)

multiple_workflow <- workflow() %>% 
  add_recipe(multiple_recipe) %>% 
  add_model(linear_model)

multiple_fit <- multiple_workflow %>% 
  fit(data = train_data)

tidy(multiple_fit)

residuals <- augment(multiple_fit, new_data = train_data)

ggplot(residuals, aes(.fitted, .resid)) +
  geom_point() +
  labs(title = "Resíduos da Regressão Múltipla", x = "Fitted Values", y = "Resíduos")

# Regressão não linear ----------------------------------------------------

nonlinear_recipe <- recipe(idade ~ faixa_etaria, data = train_data) %>%
  step_poly(faixa_etaria, degree = 2)

nonlinear_workflow <- workflow() %>% 
  add_recipe(nonlinear_recipe) %>% 
  add_model(linear_model)

tidy(nonlinear_fit)

ggplot(train_data, aes(x = faixa_etaria, y = idade)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), col = "red") +
  labs(title = "Regressão Não Linear", x = "Faixa Etária", y = "Idade")


# Avaliação no conjunto de teste para regressão linear
linear_preds <- predict(linear_fit, test_data) %>% 
  bind_cols(test_data)

metrics(data = linear_preds, truth = idade, estimate = .pred)

# Avaliação para regressão múltipla
multiple_preds <- predict(multiple_fit, test_data) %>% 
  bind_cols(test_data)

metrics(data = multiple_preds, truth = idade, estimate = .pred)

# Avaliação para regressão não linear
nonlinear_preds <- predict(nonlinear_fit, test_data) %>% 
  bind_cols(test_data)

metrics(data = nonlinear_preds, truth = idade, estimate = .pred)


