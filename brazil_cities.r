rm(list = ls())

library(tidyverse)
library(data.table)
library(xtable)
library(broom)
library(GGally)
library(psych)
library(lmtest)
library(FactoMineR)

source("./functions.r")

cities <- read.csv("BRAZIL_CITIES.csv", header = TRUE, sep = ";")

###### Data cleaning ######

cities <- cities %>%
  mutate(AREA = gsub(",", "", AREA) %>% as.numeric(AREA)) %>%
  mutate(
    POP_DENSITY = IBGE_RES_POP / AREA,
    TAUX_ACTIF = IBGE_15.59 / IBGE_RES_POP,
    TAX_CAPITA = TAXES / (IBGE_RES_POP / 1000),
    CARS_CAPITA = Cars / (IBGE_RES_POP / 1000),
    MOTO_CAPITA = Motorcycles / (IBGE_RES_POP / 1000),
    COMP_CAPITA = COMP_TOT / (IBGE_RES_POP / 1000),
    DIST_NEAREST_CAP = find_dist_to_cap(.),
    CROP = IBGE_CROP_PRODUCTION_. / IBGE_PLANTED_AREA
  ) %>%
  find_region() %>%
  mutate(IS_SOUTH = if_else(REGION %in% south, 1, 0)) %>%
  select(-REGION)

map <- cities %>%
  ggplot() +
    geom_point(
      aes(x = LONG, y = LAT),
      alpha = 0.5
    ) +
    geom_point(
      data = filter(cities, CAPITAL == 1),
      mapping = aes(x = LONG, y = LAT),
      colour = "red"
    ) +
    theme(
      axis.title = element_text(size = 14)
    )
ggsave(file = "./figures/map.pdf", width = 7, height = 7)

vars <- c(
  "CITY",
  "IDHM",
  "POP_DENSITY", "TAUX_ACTIF", "CARS_CAPITA", "MOTO_CAPITA",
  "GDP_CAPITA", "TAX_CAPITA", "COMP_CAPITA",
  "DIST_NEAREST_CAP", "CROP",
  "IS_SOUTH"
)
labs <- as_labeller(c(
  "CITY" = "Ville",
  "IDHM" = "IDH",
  "TAUX_ACTIF" = "Taux actif",
  "POP_DENSITY" = "Densité de population",
  "CARS_CAPITA" = "N. de voitures / 1k hab.",
  "MOTO_CAPITA" = "N. de motos / 1k hab.",
  "GDP_CAPITA" = "PIB par hab.",
  "DIST_NEAREST_CAP" = "Distance de la capitale la plus proche",
  "TAX_CAPITA" = "Revenu taxe par hab. [BRL]",
  "COMP_CAPITA" = "N. d'entreprises / 1k hab.",
  "CROP" = "Prod. agri. / s. de terres cultivées"
))

cities_var <- cities %>%
  select(all_of(vars))

cities_num <- cities_var %>%
  select(-c(CITY)) %>%
  drop_na()

cities_scaled <- cities_num %>%
  mutate_at(vars(!contains("IS_SOUTH")), ~ scale(.))

sink(file = "./outputs/summary_scaled.txt")
summary(cities_scaled) %>% print()
sink(file = NULL)

### Split into train and test dataset ###
# Raw data
set.seed(0)
cities_id <- cities_num %>%
  mutate(id = 1:nrow(.))
cities_train <- cities_id %>%
  sample_frac(0.7)
cities_test <- cities_id %>%
  anti_join(cities_train, by = "id")

# Standardised data
set.seed(0)
cities_scaled_id <- cities_scaled %>%
  mutate(id = 1:nrow(.))
cities_scaled_train <- cities_scaled_id %>%
  sample_frac(0.7)
cities_scaled_test <- cities_scaled_id %>%
  anti_join(cities_scaled_train, by = "id")

### Log conversion ###
cities_log <- cities_num %>%
  mutate(
    ln_POPDENS = log(POP_DENSITY),
    ln_GDP_CAPITA = log(GDP_CAPITA),
    ln_TAX_CAPITA = log(TAX_CAPITA)
  ) %>%
  select(-c(POP_DENSITY, GDP_CAPITA, TAX_CAPITA)) %>%
  drop_na()

cities_log_scaled <- cities_log %>%
  scale() %>%
  as.data.table()

set.seed(0)
cities_log_id <- cities_log_scaled %>%
  mutate(id = 1:nrow(.))
cities_scaled_train_log <- cities_log_id %>%
  sample_frac(0.7)
cities_scaled_test_log <- cities_log_id %>%
  anti_join(cities_scaled_train_log, by = "id")

###### Descriptive statistics ######

### Table ###

cities_num %>%
  rename(
    "IDH" = IDHM,
    "Densité de population" = POP_DENSITY,
    "Taux actif" = TAUX_ACTIF,
    "N. de voitures par 1k hab." = CARS_CAPITA,
    "N. de motos par 1k hab." = MOTO_CAPITA,
    "PIB par hab." = GDP_CAPITA,
    "Distance de la capitale la plus proche" = TAX_CAPITA,
    "Revenu taxe par habitant" = COMP_CAPITA,
    "N. d'entreprises par 1k hab." = DIST_NEAREST_CAP,
    "Prod. agri. par terre cultivée unitaire" = CROP,
    "Région (0 si nord, 1 si sud)" = IS_SOUTH
  ) %>%
  describe(skew = FALSE) %>%
  select(-c(vars, range)) %>%
  xtable(
    caption = "Tableau des statistiques descriptives",
    label = "tab:desc",
    digits = 2
  ) %>%
  print(file = "./tables/stats_desc.tex")

### Boxplot ###

box <- cities_var %>%
  select(-IS_SOUTH) %>%
  gather(var, val, -CITY) %>%
  drop_na() %>%
  ggplot() +
    geom_boxplot(aes(
      x = "",
      y = val
    )) +
    facet_wrap(~ var, scales = "free", labeller = labs) +
    xlab("Variable") +
    ylab("Value")
ggsave(file = "./figures/boxplot.pdf", width = 12, height = 8)

### Pairs plot ###

pdf("./figures/pairs.pdf", width = 12, height = 12)
pairs <- cities_num %>%
  select(-IS_SOUTH) %>%
  ggpairs() %>%
  print()
dev.off()

pdf("./figures/pairs_log.pdf", width = 12, height = 12)
pairs <- cities_log %>%
  select(-IS_SOUTH) %>%
  ggpairs() %>%
  print()
dev.off()

###### Multiple linear regression ######

lmObj_raw <- lm(IDHM ~ ., data = cities_train %>% select(-id))

sink(file = "./outputs/res_reg_raw.txt")
summary(lmObj_raw) %>% print()
step(lmObj_raw) %>% print()
sink(file = NULL)

### Cook distance ###
cook_dist_r <- influence.measures(lmObj_raw)$infmat %>%
  as.data.table() %>%
  select(cook.d)

n <- lmObj_raw$fitted.values %>%
  length()
p <- lmObj_raw$coef %>%
  length() - 1

threshold_cook_r <- 4 / (n - p)

pl_cook_dist_r <- cook_dist_r %>%
  mutate(obs = 1:nrow(.)) %>%
  ggplot() +
    geom_bar(aes(x = obs, y = cook.d),
    stat = "identity",
    fill = "red",
    width = 5
  ) +
  geom_hline(aes(yintercept = threshold_cook_r)) +
  xlab("Observations") +
  ylab("Distance de Cook")
ggsave(file = "./figures/cookd_r.pdf", width = 5, height = 7)

cities_train_cook <- cities_train %>%
  mutate(cook_dist = cook_dist_r$cook.d)

cities_train_atyp <- cities_train_cook %>%
  filter(cook_dist >= threshold_cook_r) %>%
  select(-cook_dist)
cities_train_typ <- cities_train_cook %>%
  filter(cook_dist < threshold_cook_r) %>%
  select(-cook_dist)

paste0(
  "Seuil : ", signif(threshold_cook_r, digit = 3), "\n",
  "Nombre de données exclues : ", nrow(cities_train_atyp)
) %>%
  write(file = "./outputs/cookd_r.txt")

### Fit model - 2 (raw data) ###
lmObj_typ_r <- lm(IDHM ~ ., data = cities_train_typ %>% select(-id))

sink(file = "./outputs/res_reg_raw_2.txt")
summary(lmObj_typ_r) %>% print()
step(lmObj_typ_r) %>% print()
sink(file = NULL)

### Normality of residuals ###
sink(file = "./outputs/swtest_raw.txt")
shapiro.test(lmObj_typ_r$residuals) %>% print()
sink(file = NULL)

pl_qq_typ_r <- ggplot() +
  geom_qq(aes(sample = lmObj_typ_r$residuals)) +
  geom_qq_line(
    mapping = aes(sample = lmObj_typ_r$residuals),
    colour = "red",
  ) +
  xlab("Théorique") +
  ylab("Échantillon")
ggsave(file = "./figures/qq_raw.pdf", width = 4, height = 4)

### Multi colinearity ###
cor_mat_r <- cities_train_typ %>%
  select(-id) %>%
  drop_na() %>%
  cor()
vif_r <- 1 / (1 - (cor_mat_r ^ 2))
xtable(vif_r, caption = "Matrice de VIF", label = "tab:vif_r") %>%
  print(file = "./tables/vif_r.tex")

### Homoscedasticity ###
pl_fitres_r <- ggplot() +
  geom_point(aes(
    x = lmObj_typ_r$fitted.values,
    y = lmObj_typ_r$residuals
  )) +
  xlab("Fitted values") +
  ylab("Residuals")
ggsave(file = "./figures/homoscedasticity_r.pdf", width = 6, height = 6)

sink(file = "./outputs/bptest_r.txt")
bptest(lmObj_typ_r) %>% print()
sink(file = NULL)

### Predictive performance ###
pred_r <- cities_test %>%
  predict(lmObj_typ_r, newdata = .)
cities_pred <- cities_test %>%
  mutate(pred = pred_r) %>%
  mutate(
    error = IDHM - pred,
    sq_error = (IDHM - pred) ^ 2,
    pct_error = (IDHM - pred) / IDHM
  )

rmse_r <- cities_pred$sq_error %>%
  mean() %>%
  sqrt()

nrmse_r <- cities_pred$IDHM %>% {
  rmse_r / (max(.) - min(.))
}

mae_r <- cities_pred$error %>%
  abs() %>%
  mean()

mape_r <- cities_pred$pct_error %>%
  abs() %>%
  mean()

paste0(
  "RMSE : ", rmse_r, "\n",
  "NRMSE : ", nrmse_r, "\n",
  "MAE : ", mae_r, "\n",
  "MAPE : ", mape_r, "\n"
) %>%
  write(file = "./outputs/rmse_r.txt")

pl_yy <- cities_pred %>%
  ggplot() +
    geom_point(aes(
      x = IDHM, y = pred
    )) +
    geom_abline(
      intercept = 0,
      slope = 1,
      colour = "red"
    ) +
    ylim(-4.5, 4.5) +
    xlim(-4.5, 4.5) +
    xlab("Observations") +
    ylab("Prédictions")
ggsave(file = "./figures/yy_r.pdf", width = 4, height = 4)


###### Multiple linear regression (scaled data) ######

### Fit model ###
lmObj <- lm(IDHM ~  ., data = cities_scaled_train %>% select(-id))

sink(file = "./outputs/res_reg1.txt")
summary(lmObj) %>% print()
sink(file = NULL)

tidy(lmObj) %>%
  xtable(
    caption = "Résultat de la régression linéaire multiple",
    label = "tab:res_reg1"
  ) %>%
  print(file = "./tables/res_reg1.tex")

### Model selection ###
sink(file = "./outputs/step1.txt")
step(lmObj) %>%
  summary() %>%
  print()
sink(file = NULL)

### Normality of residuals ###
sink(file = "./outputs/swtest1.txt")
shapiro.test(lmObj$residuals) %>% print()
sink(file = NULL)

pl_qq <- ggplot() +
  geom_qq(aes(sample = lmObj$residuals)) +
  geom_qq_line(
    mapping = aes(sample = lmObj$residuals),
    colour = "red",
  ) +
  xlab("Théorique") +
  ylab("Échantillon")
ggsave(file = "./figures/qq_1.pdf", width = 4, height = 4)

### Cook distance ###
cook_dist <- influence.measures(lmObj)$infmat %>%
  as.data.table() %>%
  select(cook.d)

n <- lmObj$fitted.values %>%
  length()
p <- lmObj$coef %>%
  length() - 1

threshold_cook <- 4 / (n - p)

pl_cook_dist <- cook_dist %>%
  mutate(obs = 1:nrow(.)) %>%
  ggplot() +
    geom_bar(aes(x = obs, y = cook.d),
    stat = "identity",
    fill = "red",
    width = 5
  ) +
  geom_hline(aes(yintercept = threshold_cook)) +
  xlab("Observations") +
  ylab("Distance de Cook")
ggsave(file = "./figures/cookd_1.pdf", width = 5, height = 7)

cities_scaled_train_cook <- cities_scaled_train %>%
  mutate(cook_dist = cook_dist$cook.d)

cities_scaled_train_atyp <- cities_scaled_train_cook %>%
  filter(cook_dist >= threshold_cook) %>%
  select(-cook_dist)
cities_scaled_train_typ <- cities_scaled_train_cook %>%
  filter(cook_dist < threshold_cook) %>%
  select(-cook_dist)

paste0(
  "Seuil : ", signif(threshold_cook, digit = 3), "\n",
  "Nombre de données exclues : ", nrow(cities_scaled_train_atyp)
) %>%
  write(file = "./outputs/cookd_1.txt")

### Fit model - 2 ###
lmObj_typ <- lm(IDHM ~ ., data = cities_scaled_train_typ %>% select(-id))

sink(file = "./outputs/res_reg2.txt")
summary(lmObj_typ) %>% print()
step(lmObj_typ) %>% print()
sink(file = NULL)

tidy(lmObj_typ) %>%
  xtable(
    caption = "Résultat de la régression linéaire multiple",
    label = "tab:res_reg2"
  ) %>%
  print(file = "./tables/res_reg2.tex")

### Normality of residuals ###
sink(file = "./outputs/swtest2.txt")
shapiro.test(lmObj_typ$residuals) %>% print()
sink(file = NULL)

pl_qq_typ <- ggplot() +
  geom_qq(aes(sample = lmObj_typ$residuals)) +
  geom_qq_line(
    mapping = aes(sample = lmObj_typ$residuals),
    colour = "red",
  ) +
  xlab("Théorique") +
  ylab("Échantillon")
ggsave(file = "./figures/qq_2.pdf", width = 4, height = 4)

### Multi colinearity ###
cor_mat <- cities_scaled_train_typ %>%
  select(-id) %>%
  drop_na() %>%
  cor()
vif <- 1 / (1 - (cor_mat ^ 2))
xtable(vif, caption = "Matrice de VIF", label = "tab:vif") %>%
  print(file = "./tables/vif.tex")

### Homoscedasticity ###
pl_fitres <- ggplot() +
  geom_point(aes(
    x = lmObj_typ$fitted.values,
    y = lmObj_typ$residuals
  )) +
  xlab("Fitted values") +
  ylab("Residuals")
ggsave(file = "./figures/homoscedasticity.pdf", width = 6, height = 6)

sink(file = "./outputs/bptest.txt")
bptest(lmObj_typ) %>% print()
sink(file = NULL)

### Predictive performance ###
pred <- cities_scaled_test %>%
  predict(lmObj_typ, newdata = .)
cities_scaled_pred <- cities_scaled_test %>%
  mutate(pred = pred) %>%
  mutate(
    error = IDHM - pred,
    sq_error = (IDHM - pred) ^ 2,
    pct_error = (IDHM - pred) / IDHM
  )

rmse <- cities_scaled_pred$sq_error %>%
  mean() %>%
  sqrt()

nrmse <- cities_scaled_pred$IDHM %>% {
  rmse / (max(.) - min(.))
}

mae <- cities_scaled_pred$error %>%
  abs() %>%
  mean()

mape <- cities_scaled_pred$pct_error %>%
  abs() %>%
  mean()

paste0(
  "RMSE : ", rmse, "\n",
  "NRMSE : ", nrmse, "\n",
  "MAE : ", mae, "\n",
  "MAPE : ", mape, "\n"
) %>%
  write(file = "./outputs/rmse.txt")

pl_yy <- cities_scaled_pred %>%
  ggplot() +
    geom_point(aes(
      x = IDHM, y = pred
    )) +
    geom_abline(
      intercept = 0,
      slope = 1,
      colour = "red"
    ) +
    ylim(-4.5, 4.5) +
    xlim(-4.5, 4.5) +
    xlab("Observations") +
    ylab("Prédictions")
ggsave(file = "./figures/yy_1.pdf", width = 4, height = 4)

### Visualisation of results ###
pl_coef <- lmObj_typ$coefficients %>%
  as.data.frame() %>%
  rownames_to_column(var = "var") %>%
  rename("coef" = ".") %>%
  mutate(var = factor(var, level = vars)) %>%
  filter(var != "(Intercept)") %>%
  ggplot() +
    geom_col(aes(x = var, y = coef)) +
    xlab("Variable") +
    ylab("Coefficient") +
    theme(
      axis.title = element_text(size = 16)
    )
ggsave(file = "./figures/coefficients.pdf", width = 12, height = 6)

###### Regression with converted data ######
lm_log <- lm(IDHM ~ . + 0, data = cities_scaled_train_log %>% select(-id))

sink(file = "./outputs/res_reg_log.txt")
summary(lm_log)
step(lm_log)
sink(file = NULL)

pred_log <- cities_scaled_test_log %>%
  predict(lm_log, newdata = .)
cities_pred_log <- cities_scaled_test_log %>%
  mutate(pred = pred_log) %>%
  mutate(
    error = IDHM - pred,
    sq_error = (IDHM - pred) ^ 2,
    pct_error = (IDHM - pred) / IDHM
  )

rmse_log <- cities_pred_log$sq_error %>%
  mean() %>%
  sqrt()

mae_log <- cities_pred_log$error %>%
  abs() %>%
  mean()

mape_log <- cities_pred_log$pct_error %>%
  abs() %>%
  mean()

paste0(
  "RMSE : ", rmse_log, "\n",
  "MAE : ", mae_log, "\n",
  "MAPE : ", mape_log, "\n"
) %>%
  write(file = "./outputs/rmse_log.txt")

pl_yy_log <- cities_pred_log %>%
  ggplot() +
    geom_point(aes(
      x = IDHM, y = pred
    )) +
    geom_abline(
      intercept = 0,
      slope = 1,
      colour = "red"
    ) +
    ylim(-4.5, 4.5) +
    xlim(-4.5, 4.5) +
    xlab("Observations") +
    ylab("Prédictions")
ggsave(file = "./figures/yy_log.pdf", width = 4, height = 4)

###### PCA ######

pca <- prcomp(~ ., scale = TRUE, data = cities_num)

sink(file = "./outputs/res_pca.txt")
summary(pca) %>% print()
sink(file = NULL)

var_explained <- (pca$sdev ^ 2) / sum((pca$sdev ^ 2))
pc_names <- paste0("PC", seq(1, ncol(pca$x), 1))
var_ex_df <- data.frame(
  PC = pc_names,
  var = var_explained,
  stringsAsFactors = FALSE
) %>%
  mutate(PC = factor(PC, levels = pc_names))

scree <- var_ex_df %>%
  ggplot() +
    geom_col(aes(
      x = PC,
      y = var
    )) +
    xlab("Composantes pricipales") +
    ylab("Variance expliquée")
ggsave(file = "./figures/pca_scree.pdf", width = 4, height = 4)

pca_score <- pca$x[, 1:2]

cities_pca <- cities_scaled %>%
  cbind(pca_score) %>%
  scale() %>%
  as.data.table()

set.seed(0)
cities_pca_id <- cities_pca %>%
  mutate(id = 1:nrow(.))
cities_pca_train <- cities_pca_id %>%
  sample_frac(0.7)
cities_pca_test <- cities_pca_id %>%
  anti_join(cities_scaled_train, by = "id")

lm_pca <- lm(IDHM ~ PC1 + PC2 + 0, data = cities_pca_train)

sink(file = "./outputs/res_reg_pca.txt")
summary(lm_pca) %>% print()
step(lm_pca) %>% print()
sink(file = NULL)

pred_pca <- cities_pca_test %>%
  predict(lm_pca, newdata = .)
cities_pred_pca <- cities_pca_test %>%
  mutate(pred = pred_pca) %>%
  mutate(
    error = IDHM - pred,
    sq_error = (IDHM - pred) ^ 2,
    pct_error = (IDHM - pred) / IDHM
  )

rmse_pca <- cities_pred_pca$sq_error %>%
  mean() %>%
  sqrt()

mae_pca <- cities_pred_pca$error %>%
  abs() %>%
  mean()

mape_pca <- cities_pred_pca$pct_error %>%
  abs() %>%
  mean()

paste0(
  "RMSE : ", rmse_pca, "\n",
  "MAE : ", mae_pca, "\n",
  "MAPE : ", mape_pca, "\n"
) %>%
  write(file = "./outputs/rmse_pca.txt")


###### ACP ######
#Q6
#How to put two qualitative variables ? 
out_acp <- PCA(cities_acp[2:12],scale.unit=TRUE,ncp=9, quali.sup=1)
summary(out_acp)
out_acp

val_prop <- out_acp$eig[,"eigenvalue"]
val_prop_cum <- cumsum(val_prop)/sum(val_prop)
cp <- 1:length(val_prop)
vp <- data.frame(cp=cp,val_prop=val_prop)
vp_cum <- data.frame(cp=cp,val_prop_cum=val_prop_cum)

ggplot(data=vp,aes(x=cp,y=val_prop))+
  geom_bar(stat="identity",fill="steelblue")+
  theme_minimal()+
  ggtitle("Eboulis des valeurs propres")+
  xlab("Nombre de composantes principales")+
  ylab("Valeurs propres")+
  scale_x_continuous(breaks=cp)

#Q7

ggplot(data=vp_cum,aes(x=cp,y=val_prop_cum))+
  geom_bar(stat="identity",fill="steelblue")+
  theme_minimal()+
  ggtitle("Part d'inertie expliquée en fonction du nombre de CP")+
  xlab("Nombre de composantes principales")+
  ylab("Part d'inertie expliquée")+
  scale_x_continuous(breaks=cp)


#Q9
plot.PCA(out_acp,shadow=TRUE,cex=0.8,axes=c(1,2),choix="ind",habillage=1,label="none",new.plot=TRUE,
         title="Projection des individus : en fonction de la REGION")
plot.PCA(out_acp,shadow=TRUE,cex=0.8,axes=c(1,2),choix="ind",habillage=1,label="none",new.plot=TRUE,
         title="Projection des individus : en fonction de la catégorie RURALE/URBAINE")

