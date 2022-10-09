################################################################
## Árvore de decisão ##
################################################################


## Modelo de árvore de decisão
arvore <- rpart::rpart(Target ~ .,
                       data=df_treino_arv,
                       method='class',
                       xval=20,
                       control = rpart.control(cp = 0, 
                                               minsplit = 1, 
                                               maxdepth = 30)
)


## Aplica-se o procedimento grid search
tab_cp <- rpart::printcp(arvore)
plotcp(arvore)


## Armazena-se o cp mínimo da árvore
cp_min <- tab_cp[which.min(tab_cp[,'xerror']),'CP']
cp_min

## Modelo de árvore de decisão - agora utilizando o cp mínimo obtido no grid search
arvore_cp <- rpart::rpart(Target ~ .,
                          data=df_treino_arv,
                          method='class',
                          xval=5,
                          control = rpart.control(cp = cp_min, 
                                                  minsplit = 1, 
                                                  maxdepth = 30)
)


## Plota-se a árvore de decisão
# Define-se uma paleta de cores amigável para daltônicos
paleta = scales::viridis_pal(begin=.75, end=1)(20)
# Plota-se a árvore
rpart.plot::rpart.plot(arvore_cp,box.palette = paleta,
                       cex=0.7, split.cex=0.8,
                       facsep=",\n",eq=" =\n")


## Realiza-se a predição na base teste e aplica-se o cutoff
p_teste = stats::predict(arvore_cp, df_teste_arv)
p_teste_aux = ifelse(p_teste>.5,1,0)
c_teste = base::factor(ifelse(p_teste[,2]>.5, 1, 0))


## Avalia-se a matriz de confusão
confusionMatrix(table(p_teste_aux[,2] == 1 ,df_teste_arv$Target == 1)[2:1, 2:1])


## Calcula-se área sob curva ROC
df_teste_arv$Target_ROC <- as.factor(ifelse(df_teste_arv$Target == "0", "N", "Y"))
df_teste_arv$Predictor = base::factor(ifelse(p_teste[,2]>.5, "Y", "N"))

aval_arv <- data.frame(obs=df_teste_arv$Target_ROC,
                       pred=df_teste_arv$Predictor,
                       Y = p_teste[,2],
                       N = 1-p_teste[,2]
)
ROC_arv <- twoClassSummary(aval_arv, lev=levels(aval_arv$obs))
ROC_arv


## Imprime-se a curva ROC
CurvaROC_arv <- ggplot(aval_arv, aes(d = obs, m = Y, colour='a')) + 
  plotROC::geom_roc(n.cuts = 0) +
  scale_color_viridis_d(direction = -1, begin=0, end=.25) +
  theme(legend.position = "none") +
  ggtitle(sprintf("Curva ROC - Árvore de decisão e área sob a curva = %s", round(ROC_arv[1],3)))

CurvaROC_arv


## Manipula-se os dados para criar as métricas da matriz de custo
df_teste_arv <- mutate(df_teste_arv,
                       valorTratado =
                         case_when(
                           df_teste_arv$Target_ROC == "N" & df_teste_arv$Predictor == "Y" ~ ((df_teste_arv$InterestRate)/100)*df_teste_arv$CreditAmount,
                           df_teste_arv$Target_ROC == "Y" & df_teste_arv$Predictor == "N" ~ df_teste_arv$CreditAmount*1.0,
                           TRUE ~ 0)
)


## Aplica-se a matriz de custo para obter-se o custo total
tabela_custo_arv = aggregate(df_teste_arv$valorTratado,
                             list(df_teste_arv$Target_ROC, df_teste_arv$Predictor),
                             sum)

nomesCusto <- c("Observação","Predição","valorCusto")
colnames(tabela_custo_arv) <- nomesCusto
tabela_custo_arv

custoFinal_arv <- sum(tabela_custo_arv$valorCusto)
# custoFinal
sprintf('O custo final para o modelo de árvore de decisão é: %s', custoFinal_arv)










