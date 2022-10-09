################################################################
## Regressão logística ##
################################################################


## Estima-se o modelo utilizando as dummies
modelo_01_dummy <- glm(formula = Target ~ .,
                       data = df_treino_dummy,
                       family = "binomial")

## Aplica-se o procedimento stepwise
step_modelo_01_dummy <- step(object = modelo_01_dummy,
                             k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))

summary(step_modelo_01_dummy)

## Predição do modelo na base de teste
df_teste_dummy$Result <- predict(step_modelo_01_dummy, df_teste_dummy, type="response")


## Aplica-se o cutoff - 0.5
df_teste_dummy$Predicao <- df_teste_dummy$Result >= 0.5


## Matriz de confusão
df_teste_dummy <- mutate(df_teste_dummy,
                         Predicao = replace(Predicao, Predicao == TRUE, '1'),
                         Predicao = replace(Predicao, Predicao == FALSE, '0')
)

confusionMatrix(table(df_teste_dummy$Predicao == 1,df_teste_dummy$Target == 1)[2:1, 2:1])


## Calcula-se área sob curva ROC
df_teste_dummy$Target_ROC <- as.factor(ifelse(df_teste_dummy$Target == "0", "N", "Y"))
df_teste_dummy$Predictor = base::factor(ifelse(df_teste_dummy$Result>.5, "Y", "N"))

aval_dummy_teste <- data.frame(obs=df_teste_dummy$Target_ROC,
                               pred=df_teste_dummy$Predictor,
                               Y = df_teste_dummy$Result,
                               N = 1-df_teste_dummy$Result
)
ROC_log <- twoClassSummary(aval_dummy_teste, lev=levels(aval_dummy_teste$obs))
ROC_log

## Imprime-se a curva ROC
CurvaROC_log <- ggplot(aval_dummy_teste, aes(d = obs, m = Y, colour='a')) + 
  plotROC::geom_roc(n.cuts = 0) +
  scale_color_viridis_d(direction = -1, begin=0, end=.25) +
  theme(legend.position = "none") +
  ggtitle(sprintf("Curva ROC - Regressão logística e área sob a curva = %s", round(ROC_log[1],3)))

CurvaROC_log


## Manipula-se os dados para criar as métricas da matriz de custo
df_teste_dummy <- mutate(df_teste_dummy,
                         valorTratado =
                           case_when(
                             df_teste_dummy$Target_ROC == "N" & df_teste_dummy$Predictor == "Y" ~ ((df_teste_dummy$InterestRate)/100)*df_teste_dummy$CreditAmount,
                             df_teste_dummy$Target_ROC == "Y" & df_teste_dummy$Predictor == "N" ~ df_teste_dummy$CreditAmount*1.0,
                             TRUE ~ 0)
)


## Aplica-se a matriz de custo para obter-se o custo total
tabela_custo_log = aggregate(df_teste_dummy$valorTratado,
                             list(df_teste_dummy$Target_ROC, df_teste_dummy$Predictor),
                             sum)

nomesCusto <- c("Observação","Predição","valorCusto")
colnames(tabela_custo_log) <- nomesCusto
tabela_custo_log

custoFinal_log <- sum(tabela_custo_log$valorCusto)
# custoFinal
sprintf('O custo final para o modelo de regressão logística é: %s', custoFinal_log)





