# Instalar pacotes 
# install.packages(c("ggplot2", "caret", "lmtest", "plotly", "car", "dplyr"))
library(ggplot2)
library(caret)
library(lmtest)
library(plotly)
library(car)
library(dplyr)

# Carregar o conjunto de dados
dados <- read.csv("Lung_Cancer_Dataset.csv")

# Transformando variáveis categóricas em fatores, caso necessário
dados$GENDER <- as.factor(dados$GENDER)
dados$LUNG_CANCER <- as.factor(dados$LUNG_CANCER)  # Presença ou ausência de câncer

dados <- dados %>%
  mutate(TUMOR_SIZE = ifelse(LUNG_CANCER == "YES", sample(1:8, size = n(), replace = TRUE), NA))

dados$AGE <- as.numeric(dados$AGE)
dados$TUMOR_SIZE <- as.numeric(dados$TUMOR_SIZE)

dados_cancer = dados %>%  
  dplyr:: filter (LUNG_CANCER == "YES")

# Para outras variáveis inteiras (sem AGE)
# dados <- dados %>%
#   mutate(across(where(is.integer) & !starts_with("AGE", "TUMOR_SIZE"), ~ factor(ifelse(. == 0, 1, 2), levels = c(1, 2), labels = c("Não", "Sim"))))

# Regressão Linear Simples (Modelando LUNG_CANCER ~ AGE)
modelo_lr <- lm(TUMOR_SIZE ~ SMOKING, data = dados_cancer)
summary(modelo_lr)

# Regressão Múltipla (Modelando LUNG_CANCER ~ AGE + CHRONIC.DISEASE + COUGHING + CHEST.PAIN)
# modelo_lr_mult <- lm(as.numeric(LUNG_CANCER) ~ AGE + CHRONIC.DISEASE + COUGHING + CHEST.PAIN, data = dados)
# summary(modelo_lr_mult)

# Regressão Não Linear: Número de diagnósticos por faixa etária (dividido a cada 10 anos)
# dados$faixa_etaria <- cut(dados$AGE, breaks = seq(0, 100, by = 10), right = FALSE, labels = FALSE)

# Contagem de diagnósticos por faixa etária
agg_data <- dados %>%
  group_by(faixa_etaria) %>%
  summarise(count = n())

print(agg_data)

# Regressão Não Linear com polinômio (ajuste de AGE com grau 2)
modelo_nl <- lm(as.numeric(LUNG_CANCER) ~ poly(AGE, 2), data = dados)
summary(modelo_nl)

# Avaliação de Acurácia (Previsões)
pred_lr <- predict(modelo_lr, dados)
pred_lr_mult <- predict(modelo_lr_mult, dados)
pred_nl <- predict(modelo_nl, dados)

# Plotando gráficos dos modelos

# Regressão Linear
ggplot(dados, aes(x = AGE, y = as.numeric(LUNG_CANCER))) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(title = "Regressão Linear: LUNG_CANCER ~ AGE")

# Regressão Múltipla
ggplot(dados, aes(x = AGE, y = as.numeric(LUNG_CANCER))) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ AGE + CHRONIC.DISEASE + COUGHING + CHEST.PAIN, se = FALSE) +
  labs(title = "Regressão Múltipla")

# Regressão Não Linear
ggplot(dados, aes(x = AGE, y = as.numeric(LUNG_CANCER))) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(AGE, 2), se = FALSE) +
  labs(title = "Regressão Não Linear (polinômio de grau 2)")

# Calculando o R² para cada modelo
r2_lr <- summary(modelo_lr)$r.squared
r2_lr_mult <- summary(modelo_lr_mult)$r.squared
r2_nl <- summary(modelo_nl)$r.squared

cat("R² para regressão linear:", r2_lr, "\n")
cat("R² para regressão múltipla:", r2_lr_mult, "\n")
cat("R² para regressão não linear:", r2_nl, "\n")

# Extra: Análise de Multicolinearidade (para Regressão Múltipla)
vif(modelo_lr_mult)

# Análise de resíduos (Regressão Linear Simples)
residuos_lr <- residuals(modelo_lr)

# Plotando os resíduos
ggplot(data.frame(residuos = residuos_lr), aes(x = residuos)) +
  geom_histogram(binwidth = 0.5) +
  labs(title = "Distribuição dos Resíduos - Regressão Linear Simples")

# Interpretação de coeficientes (Regressão Múltipla)
summary(modelo_lr_mult)

# Gráfico interativo (Plotly) para a Regressão Linear
plot_ly(data = dados, x = ~AGE, y = ~as.numeric(LUNG_CANCER), mode = 'markers', type = 'scatter', 
        marker = list(color = 'blue')) %>%
  add_lines(x = ~AGE, y = predict(modelo_lr, dados), line = list(color = 'red'))





