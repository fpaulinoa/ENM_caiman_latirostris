# Modelagem de nicho ecológico
# by Fernando Paulino e Carolina Loss 
# JAN - 2025

# Carrega o pacote terra, usado para manipulação de dados espaciais
library(terra)

# Lista os nomes dos diretórios dentro da pasta de preditores com VIF aplicado
names <- list.files("./output/vif/pred_vif/")

# Loop para processar cada conjunto de variáveis
for (i in 1:length(names)){
  
  # Define a pasta onde estão os preditores da espécie atual
  pred.folder <- paste0("./output/vif/pred_vif/", names[i])
  
  # Lista os arquivos .tiff dentro da pasta dos preditores
  pred.files <- list.files(pred.folder, patter='*.tiff$') # (atenção: "pattern" está escrito errado)
  
  # Carrega os rasters dos preditores
  pred <- rast(paste0(pred.folder,"/",pred.files))
  
  # Carrega o raster de viés (bias) correspondente
  bias <- rast(paste0("./output/bias/",names[i],"_bias.tiff"))
  
  # Extrai todas as coordenadas (latitude e longitude) dos pixels do raster de viés
  xy <- xyFromCell(bias, cells(bias)) 
  
  # Extrai o número das células (pixels)
  c <- cells(bias)
  
  # Extrai os valores dos pixels do raster de viés
  v <- values(bias, dataframe = T, na.rm = T)
  
  # Combina número da célula, coordenadas e valor de viés em um único data frame
  KDEpts <- cbind(c,xy,v)
  colnames(KDEpts) <- c("cell", "x", "y", "prob")
  
  ## Carrega os registros de ocorrência (já afinados, ou seja, "thinned")
  occ <- read.csv(paste0("./output/pts_thin/xy_", names[i], ".csv"))
  
  ## Extrai o número da célula e coordenadas correspondentes aos pontos de ocorrência
  o <- extract(bias, occ[,5:6], cells = T, xy = TRUE)
  
  ## Extrai os valores dos preditores para os pontos de ocorrência
  xySWD <- as.data.frame(extract(pred, o[,3]))
  
  ## Adiciona o nome da espécie aos dados
  species <- rep(names[i],nrow(o))
  
  ## Combina nome da espécie, células e valores dos preditores em um único data frame
  xySWD <- cbind(species, o[,3:5], xySWD)
  
  ## Salva o arquivo com as variáveis extraídas para os pontos de ocorrência
  write.csv(xySWD, paste0("./output/xySWD/xySWD_",names[i],".csv"))
  
  ## Remove os pontos de ocorrência do conjunto de pontos de fundo (background)
  KDEpts <- subset(KDEpts, !is.element(KDEpts$cell, o$cell))
  
  ## Extrai os valores dos preditores para todos os pontos de fundo
  biasKDE_all <- as.data.frame(extract(pred, KDEpts[,1]))
  
  ## Marca esses pontos como "background"
  bg <- rep("background", nrow(KDEpts))
  
  ## Combina tipo de ponto (background), coordenadas e preditores
  biasKDE_all <- cbind(bg, KDEpts, biasKDE_all)
  
  ## Remove a coluna de probabilidade para o conjunto final de ambiente
  envSWD <- biasKDE_all[,-5]
  
  ## Salva os dados ambientais dos pontos de fundo
  write.csv(envSWD, paste0("./output/envSWD/envSWD_",names[i],".csv"))
  
  # Se o número de pontos de fundo for maior que 10.000:
  if (nrow(biasKDE_all) > 10000){
    
    ## Amostra 10.000 pontos de fundo, ponderando pela probabilidade do bias
    biasSWD <- biasKDE_all[sample(seq(1:nrow(biasKDE_all)), 
                                  size=10000, 
                                  replace=F, 
                                  prob=biasKDE_all[,"prob"]),
                           c(1:4,6:ncol(biasKDE_all))] # remove a coluna de probabilidade "prob"
    
    ## Salva o conjunto amostrado de pontos de fundo
    write.csv(biasSWD, paste0("./output/biasSWD/biasSWD_",names[i],".csv"))
    
  } else { # Caso haja menos de 10.000 pontos de fundo disponíveis
    
    ## Imprime uma mensagem informando que há menos de 10.000 pontos
    print (paste0(names[i], " has fewer than 10000 background points"))
    
    ## Define o número de amostras como 80% dos pontos disponíveis
    n <- round(nrow(biasKDE_all)*.8, digits = 0)
    
    ## Amostra 80% dos pontos disponíveis, ponderando pela probabilidade do bias
    biasSWD <- biasKDE_all[sample(seq(1:nrow(biasKDE_all)), 
                                  size=n, 
                                  replace=F, 
                                  prob=biasKDE_all[,"prob"]),
                           c(1:4,6:ncol(biasKDE_all))]
    
    ## Salva o conjunto amostrado de pontos de fundo
    write.csv(biasSWD, paste0("./output/biasSWD/biasSWD_",names[i],".csv"))
    
  }
}
