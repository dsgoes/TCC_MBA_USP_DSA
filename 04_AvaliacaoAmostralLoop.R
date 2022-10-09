###############################################################################
## Loop para testar vários seeds  ##
## Assim consegue-se mitigar o viés da amostra na avaliação do modelo ##
###############################################################################


## Gera-se o número de loops para comparar-se os modelos
qtd <- 50


## Tratamento para ter-se sempre os mesmos seeds para gerar as amostras de treino e teste
set.seed(100)
seeds_testes <- round(rnorm(qtd)*10000,0)


## Constrói-se a matriz para armazenar os resultados (área sob curva ROC e custo),
## a iteração e qual seed foi utilizada
M = matrix(data = 0, nrow = qtd, ncol = 6)
nomesMatriz <- c("Iteração", "Seed", "ROC_log", "ROC_arv", "Custo_log", "Custo_arv")
colnames(M) <- nomesMatriz
for (i in 1:qtd){
  M[i,1] = i
  M[i,2] = seeds_testes[i]
  
  set.seed(M[i,2])
  
  ############################################################################
  ## Bloco para separação das amostras
  ############################################################################
  
  ## Separa as amostras entre treino e teste
  ind_treino = createDataPartition(y=df00$Target, p=0.7, list=FALSE)
  
  # Separação para logística
  df_treino_dummy = df_dummy[ind_treino,]
  df_teste_dummy = df_dummy[-ind_treino,]
  
  # Separação para a árvore
  df_treino_arv = df00[ind_treino,]
  df_teste_arv = df00[-ind_treino,]
  ############################################################################
  
  
  ############################################################################
  ## Bloco para desenvolvimento da logística
  ############################################################################
  ## Predição do modelo na base de teste
  df_teste_dummy$Result <- predict(step_modelo_01_dummy, df_teste_dummy, type="response")
  
  ## Aplica-se o cutoff - 0.5
  df_teste_dummy$Predicao <- df_teste_dummy$Result >= 0.5
  
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
  
  ## Manipula-se os dados para criar as métricas da matriz de custo - logística
  df_teste_dummy <- mutate(df_teste_dummy,
                           valorTratado =
                             case_when(
                               df_teste_dummy$Target_ROC == "N" & df_teste_dummy$Predictor == "Y" ~ ((df_teste_dummy$InterestRate)/100)*df_teste_dummy$CreditAmount,
                               df_teste_dummy$Target_ROC == "Y" & df_teste_dummy$Predictor == "N" ~ df_teste_dummy$CreditAmount*1.0,
                               TRUE ~ 0)
  )
  
  
  ## Aplica-se a matriz de custo para obter-se o custo total - logística
  tabela_custo_log = aggregate(df_teste_dummy$valorTratado,
                               list(df_teste_dummy$Target_ROC, df_teste_dummy$Predictor),
                               sum)
  
  nomesCusto <- c("Observação","Predição","valorCusto")
  colnames(tabela_custo_log) <- nomesCusto
  custoFinal_log <- sum(tabela_custo_log$valorCusto)
  ############################################################################
  
  
  ############################################################################
  ## Bloco para desenvolvimento da logística
  ############################################################################
  ## Realiza-se a predição na base teste e aplica-se o cutoff
  p_teste = stats::predict(arvore_cp, df_teste_arv)
  p_teste_aux = ifelse(p_teste>.5,1,0)
  c_teste = base::factor(ifelse(p_teste[,2]>.5, 1, 0))
  
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
  
  colnames(tabela_custo_arv) <- nomesCusto
  custoFinal_arv <- sum(tabela_custo_arv$valorCusto)
  ############################################################################
  
  
  ############################################################################
  ## Salva resultados
  ############################################################################
  M[i,3] = ROC_log[1]
  M[i,5] = custoFinal_log
  M[i,4] = ROC_arv[1]
  M[i,6] = custoFinal_arv
  ############################################################################
}


## Contrói-se uma métrica para verificar a soma das diferenças entre os custos dos modelos
## Pela definição da métrica tem-se:
##   Se Indice_custoFinal_total < 0 então o modelo de árvore obteve melhor performance
##   Se Indice_custoFinal_total > 0 então o modelo logístico obteve melhor performance
##   Se Indice_custoFinal_total = 0 modelos possuem mesma performance

M2 <- as.data.frame(M)
M2$DiffCusto <- M2$Custo_arv/M2$Custo_log-1
Indice_custoFinal_total <- sum(M2$DiffCusto)
M2
Indice_custoFinal_total













