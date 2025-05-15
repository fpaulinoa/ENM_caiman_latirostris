# Modelagem de nicho ecológico
# by Fernando Paulino e Carolina Loss 
# JAN - 2025

# Aumenta o limite de memória para o Java (necessário para o MaxEnt rodar grandes datasets)
options(java.parameters = "-Xmx8g")

# Carrega os pacotes necessários
library(dismo)
library(raster)

# Lê o arquivo CSV com as ocorrências de todas as espécies
spp.xy <- read.csv("./data/occour/Caimans.csv")
# Substitui espaços nos nomes das espécies por underlines (_)
spp.xy$sp <- gsub(" ", "_", spp.xy$sp)

# Cria uma lista com os nomes únicos das espécies
spp <- unique(spp.xy$sp)

# Define os nomes das pastas de saída para as 20 repetições de modelagem
run.dir <- paste0("run_", 1:20)

# Inicia um loop para cada espécie
for (i in 1:length(spp)) {
  
  # Inicia um loop para cada repetição (run_1 até run_20)
  for (j in 1:length(run.dir)){
    
    # Define o caminho para a pasta com os preditores cortados da espécie
    species_folder <- paste0("./output/predictors/predmod/predmodcrop/", spp[i], "/")
    
    # Carrega todos os arquivos de preditores ambientais (.asc) da espécie
    pred.files <- list.files(species_folder, pattern = '*.asc$', full.names = TRUE)
    pred.all <- stack(pred.files)
    
    # Cria a pasta onde o modelo MaxEnt e suas projeções serão salvos
    dir.name <- paste0("./output/models/", spp[i], "/" , run.dir[j], "/")
    dir.create(dir.name, recursive = TRUE)
    
    # Carrega o arquivo SWD de pontos de presença
    xy.path <- paste0("./output/xySWD/xySWD_", spp[i], ".csv")
    pts <- read.csv(xy.path)
    # Mantém apenas as colunas de variáveis ambientais (a partir da coluna 6)
    pts <- pts[, 6:ncol(pts)]
    
    # Carrega o arquivo SWD de pontos de pseudo-ausência ou background
    env.path <- paste0("./output/biasSWD/biasSWD_", spp[i], ".csv")
    env <- read.csv(env.path)
    # Mantém apenas as colunas de variáveis ambientais
    env <- env[, 6:ncol(env)]
    
    # Junta os dados de presença e pseudo-ausência em um único data frame
    swd <- rbind(pts, env)
    # Cria um vetor indicando presenças (1) e pseudo-ausências (0)
    swd.v <- c(rep(1, nrow(pts)), rep(0, nrow(env)))
    
    # Remove a palavra "current" dos nomes das colunas, para padronizar
    colnames(swd) <- gsub("current", "", colnames(swd))
    
    # Seleciona os preditores correspondentes às colunas do SWD
    p.names <- colnames(swd)
    pred <- subset(pred.all, p.names)
    
    # Carrega o arquivo CSV que contém os melhores parâmetros selecionados via ENMeval
    f.path <- paste0("./output/Fclass_best/ENMeval_best_", spp[i], ".csv")
    f <- read.csv(f.path)
    fc <- f$fc  # Tipo de feature class (ex: L, LQ, LQH)
    
    # Define os argumentos das feature classes para o MaxEnt conforme o resultado do ENMeval
    if (fc == 'L') {
      fclass <- c("linear=true", "quadratic=false", "hinge=false", "product=false", "threshold=false")
    } else if (fc == 'LQ') {
      fclass <- c("linear=true", "quadratic=true", "hinge=false", "product=false", "threshold=false")
    } else {
      fclass <- c("linear=true", "quadratic=true", "hinge=true", "product=false", "threshold=false")
    }
    
    # Define o valor do betamultiplier (regularização) baseado no ENMeval
    rm <- paste0("betamultiplier=", f$rm)
    
    # Define todos os argumentos para a modelagem no MaxEnt
    args <- c('randomseed=true',                    # Usa sementes aleatórias
              'outputformat=logistic',               # Output no formato logístico (entre 0 e 1)
              'replicatetype=crossvalidate',         # Usa validação cruzada
              'replicates=5',                        # 5 repetições de validação cruzada
              fclass,                                # Feature classes definidas acima
              rm,                                    # Regularização definida acima
              'writebackgroundpredictions=true')     # Salva as predições para os dados de fundo
    
    # Ajusta o modelo MaxEnt usando as presenças e pseudo-ausências
    model <- maxent(x = swd, p = swd.v, path = dir.name, args = args)
    
    # Realiza a projeção espacial do modelo treinado
    predict_filename <- paste0(dir.name, spp[i], "_prediction.tif")
    p <- predict(object = model, x = pred, filename = predict_filename,
                 na.rm = TRUE, format = 'GTiff', overwrite = TRUE, progress = 'text')
    
    # Mensagem de conclusão para cada espécie
    print(paste0("Modelagem concluída para a espécie: ", spp[i]))
  }
}

# Mensagem final indicando que todo o processamento foi concluído
print("Processamento completo para todas as espécies.")
