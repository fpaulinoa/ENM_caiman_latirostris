# Modelagem de nicho ecológico
# by Fernando Paulino e Carolina Loss 
# JAN - 2025

# Localiza o caminho para o arquivo maxent.jar, necessário para rodar MaxEnt via R
system.file("java", "maxent.jar", package = "dismo")

# Instala o pacote 'remotes', necessário para instalar pacotes do GitHub
install.packages("remotes")

# Instala o pacote ENMeval diretamente do GitHub
remotes::install_github("danlwarren/ENMeval")

# Carrega os pacotes necessários
library(ENMeval)
library(terra)
library(dismo)
library(rJava)

# Lista os arquivos de background (biasSWD) e ocorrência (xySWD) disponíveis
list.files("./output/biasSWD/")
list.files("./output/xySWD/")

# Verifica a versão instalada do Java
system("java -version")

# Instala e carrega o pacote rJava para interface com Java
install.packages("rJava")
library(rJava)
.jinit()
Sys.getenv("JAVA_HOME")  # Mostra o caminho do Java configurado no sistema

# Comentário sobre combinações de parâmetros que podem ser ajustadas no MaxEnt
# ('linear', 'quadratic', 'hinge', 'product', 'threshold', 'betamultiplier')

# Lista os nomes dos arquivos de background, retirando prefixos e extensões para trabalhar com nomes limpos
names <- list.files("./output/biasSWD/")
names <- gsub("biasSWD_", "", names)
names <- gsub(".csv", "", names)

# Loop para rodar o ENMevaluate para cada espécie ou grupo
for (i in 1:length(names)){
  
  # Lê o arquivo SWD de ocorrência
  pts.file <- paste0("./output/xySWD/xySWD_", names[i], ".csv")
  xySWD <- read.csv(pts.file)
  xySWD <- xySWD[,4:ncol(xySWD)]  # Remove as primeiras três colunas (geralmente ID, longitude, latitude)
  
  # Lê o arquivo SWD de background (pseudo-ausências)
  bg.file <- paste0("./output/biasSWD/biasSWD_", names[i], ".csv")
  bgSWD <- read.csv(bg.file)
  bgSWD <- bgSWD[,4:ncol(bgSWD)]  # Remove as primeiras três colunas
  
  # Se houver pelo menos 5 registros de ocorrência
  if (nrow(xySWD) >= 5){
    # Rodar a calibração de modelos usando ENMevaluate com:
    # - diferentes combinações de "feature classes" (L, LQ, LQH)
    # - valores de regulação (rm = 1, 2, 3)
    # - usando MaxEnt (maxent.jar)
    # - particionamento aleatório em folds ("randomkfold")
    eval <- ENMevaluate(occs = xySWD, bg = bgSWD, parallel = TRUE, numCores = 7,
                        tune.args = list(fc = c("L", "LQ", "LQH"), rm = 1:3),
                        algorithm = "maxent.jar", partitions = "randomkfold")
    
    # Comentários adicionais:
    # - fc = tipos de transformações de variáveis (linear, quadrático, hinge)
    # - rm = valores de regularização (quanto maior, mais suave o modelo)
    # - Parâmetros comuns e recomendados conforme Elith et al. 2011
    
    # Seleciona o melhor modelo baseado no menor AICc (critério de informação corrigido)
    bestmod <- which(eval@results$AICc == min(eval@results$AICc, na.rm = TRUE))
    f <- eval@results[bestmod,]
    f[1:2]  # Mostra as primeiras duas colunas do melhor modelo (geralmente FC e RM)
    
    # Salva os resultados:
    # - parâmetros do melhor modelo
    write.csv(f, paste0("./output/Fclass_best/ENMeval_best_", names[i], ".csv"))
    
    # - todos os resultados de avaliação
    write.csv(eval@results, paste0("./output/Fclass_results/ENMeval_evalResults_", names[i], ".csv"))
    
  } else {
    # Caso a espécie tenha menos de 5 registros, imprime uma mensagem
    print(paste0(names[i], " has fewer than 5 occurence records"))
  }
}
