################################################################
## Instalação de pacotes ##
################################################################

pacotes <- c("tidyverse", "readxl", "dplyr", "caret", "fastDummies", "glmnet",
             "pROC", "plotly", "plotROC", "rpart", "scales", "rpart.plot",
             "base", "XML")

# plotROC é usado no algortimo da árvore de decisão -> deletar essa linha depois

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}
