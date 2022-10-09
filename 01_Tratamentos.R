################################################################
## Preparação da base ##
################################################################


## Carregamento dos pacotes
library("tidyverse")
library("readxl")
library("dplyr")
library("caret")
library("fastDummies")
library("glmnet")
library("pROC")
library("rpart")
library("scales")
library("rpart.plot")
library("XML")

## Seed para tornar os resultados replicáveis
set.seed(-100)

## Import dos dados diretamente da fonte (site UCI)
df_inicial <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data")

## Tratamento para incluir nome nas colunas
nomes <- c("CheckingAccount","Duration","CreditHistory","Purpose","CreditAmount",
           "SavingsAccount","PresentEmploySince","InterestRate","Sex","OthersDebtors",
           "PresentResidenceSince","Property","Age","OtherInstallmentPlans",
           "Housing","ExistingCreditsAtBannk","Job","PeopleProviderFor",
           "Telephone","ForeignWorker","Target")

colnames(df_inicial) <- nomes

## Manipulação das variáveis
df00 <- mutate(df_inicial,
               CheckingAccount = replace(CheckingAccount, CheckingAccount=="A11", "<0 DM"),
               CheckingAccount = replace(CheckingAccount, CheckingAccount=="A12", ">0 e <200 DM"),
               CheckingAccount = replace(CheckingAccount, CheckingAccount=="A13", ">= 200 DM"),
               CheckingAccount = replace(CheckingAccount, CheckingAccount=="A14", "Inexistente"),
               
               CreditHistory = replace(CreditHistory, CreditHistory=="A30", "Tomou e Pagou"),
               CreditHistory = replace(CreditHistory, CreditHistory=="A31", "Tomou e Pagou neste banco"),
               CreditHistory = replace(CreditHistory, CreditHistory=="A32", "Tomado em dia"),
               CreditHistory = replace(CreditHistory, CreditHistory=="A33", "Atraso no pagamento"),
               CreditHistory = replace(CreditHistory, CreditHistory=="A34", "Tomado em outros bancos"),
               
               Purpose = replace(Purpose, Purpose=="A40", "Carro"),
               Purpose = replace(Purpose, Purpose=="A41", "Carro"),
               Purpose = replace(Purpose, Purpose=="A42", "Eletrodomesticos"),
               Purpose = replace(Purpose, Purpose=="A43", "Eletrodomesticos"),
               Purpose = replace(Purpose, Purpose=="A44", "Eletrodomesticos"),
               Purpose = replace(Purpose, Purpose=="A45", "Reparos"),
               Purpose = replace(Purpose, Purpose=="A46", "Educacao"),
               Purpose = replace(Purpose, Purpose=="A47", "Ferias"),
               Purpose = replace(Purpose, Purpose=="A48", "Treinamentos"),
               Purpose = replace(Purpose, Purpose=="A49", "Negocios"),
               Purpose = replace(Purpose, Purpose=="A410", "Outros"),
               
               SavingsAccount = replace(SavingsAccount, SavingsAccount=="A61", "<100 DM"),
               SavingsAccount = replace(SavingsAccount, SavingsAccount=="A62", ">=100 e <500 DM"),
               SavingsAccount = replace(SavingsAccount, SavingsAccount=="A63", ">=500 e <1000 DM"),
               SavingsAccount = replace(SavingsAccount, SavingsAccount=="A64", ">=1000 DM"),
               SavingsAccount = replace(SavingsAccount, SavingsAccount=="A65", "Inexistente"),
               
               PresentEmploySince = replace(PresentEmploySince, PresentEmploySince=="A71", "Desempregado"),
               PresentEmploySince = replace(PresentEmploySince, PresentEmploySince=="A72", "<1 ano"),
               PresentEmploySince = replace(PresentEmploySince, PresentEmploySince=="A73", ">=1 a <4 anos"),
               PresentEmploySince = replace(PresentEmploySince, PresentEmploySince=="A74", ">=4 a <7 anos"),
               PresentEmploySince = replace(PresentEmploySince, PresentEmploySince=="A75", ">=7 anos"),
               
               Sex = replace(Sex, Sex=="A91", "H"),
               Sex = replace(Sex, Sex=="A92", "M"),
               Sex = replace(Sex, Sex=="A93", "H"),
               Sex = replace(Sex, Sex=="A94", "H"),
               Sex = replace(Sex, Sex=="A95", "M"),
               
               OthersDebtors = replace(OthersDebtors, OthersDebtors=="A101", "Nenhum"),
               OthersDebtors = replace(OthersDebtors, OthersDebtors=="A102", "Avalista parcial"),
               OthersDebtors = replace(OthersDebtors, OthersDebtors=="A103", "Avalista"),
               
               # A variável propriedade é restritiva, ou seja, não tendo um item o cliente se 
               # enquadra na categoria seguinte, dá ideia de classe
               Property = replace(Property, Property=="A121", "Imovel"),
               Property = replace(Property, Property=="A122", "Poupança imobiliaria/seguro de vida"),
               Property = replace(Property, Property=="A123", "Carro"),
               Property = replace(Property, Property=="A124", "Inexistente"),
               
               OtherInstallmentPlans = replace(OtherInstallmentPlans, OtherInstallmentPlans=="A141", "Bancos"),
               OtherInstallmentPlans = replace(OtherInstallmentPlans, OtherInstallmentPlans=="A142", "Lojas"),
               OtherInstallmentPlans = replace(OtherInstallmentPlans, OtherInstallmentPlans=="A143", "Inexistente"),
               
               Housing = replace(Housing, Housing=="A151", "Aluguel"),
               Housing = replace(Housing, Housing=="A152", "Propria"),
               Housing = replace(Housing, Housing=="A153", "Gratuita"),
               
               Job = replace(Job, Job=="A171", "Desempregado"),
               Job = replace(Job, Job=="A172", "Desempregado"),
               Job = replace(Job, Job=="A173", "Empregado"),
               Job = replace(Job, Job=="A174", "Empregado"),
               
               Telephone = replace(Telephone, Telephone=="A191", "0"),
               Telephone = replace(Telephone, Telephone=="A192", "1"),
               
               ForeignWorker = replace(ForeignWorker, ForeignWorker=="A201", "1"),
               ForeignWorker = replace(ForeignWorker, ForeignWorker=="A202", "0")
               
               ,
               Target = replace(Target, Target=="1", "0"),
               Target = replace(Target, Target=="2", "1")
)

## Transforma em Factor
cols <- c("CheckingAccount", "CreditHistory", "Purpose", "SavingsAccount",
          "PresentEmploySince", "Sex", "OthersDebtors", "Property", "OtherInstallmentPlans",
          "Housing", "ExistingCreditsAtBannk", "Job", "Telephone", "ForeignWorker"
          ,"Target"
)
df00[cols] <- lapply(df00[cols], factor)


## Exclui-se variáveis controversas
cols_excluir <- c("Sex", "ForeignWorker", "Telephone")
df00 <- df00[,!(names(df00)%in%cols_excluir)]


## Monta-se as dummies das variáveis qualitativas
df_dummy <- dummy_columns(.data = df00,
                          select_columns = c("CheckingAccount",
                                             "CreditHistory",
                                             "Purpose",
                                             "SavingsAccount",
                                             "PresentEmploySince",
                                             "OthersDebtors",
                                             "Property",
                                             "OtherInstallmentPlans",
                                             "Housing",
                                             "ExistingCreditsAtBannk",
                                             "Job"),
                          remove_selected_columns = T,
                          remove_first_dummy = T)


## Separa as amostras entre treino e teste
ind_treino = createDataPartition(y=df00$Target, p=0.7, list=FALSE)

# Separação para logística
df_treino_dummy = df_dummy[ind_treino,]
df_teste_dummy = df_dummy[-ind_treino,]

# Separação para a árvore
df_treino_arv = df00[ind_treino,]
df_teste_arv = df00[-ind_treino,]







