# Modelagem de nicho ecológico
# by Fernando Paulino e Carolina Loss 
# JAN - 2025

# Cria uma máscara NA para os shapes das áreas recortadas

# Carrega a biblioteca 'terra' para manipulação de dados espaciais
library(terra)

# Carrega o arquivo CSV com os pontos de ocorrência das espécies
pts <- read.csv("./data/occour/Caimans.csv")

# Padroniza os nomes das espécies substituindo espaços por underscores
pts$sp <- gsub(" ", "_", pts$sp)

# Obtém a lista única de nomes de espécies
names <- unique(pts$sp)

# Loop para criar máscara de NA para cada espécie
for (i in 1:length(names)) {
  
  # Define o caminho da pasta com os preditores recortados para cada espécie
  pred_folder = paste0("./output/predictors/predmod/predmodcrop/", names[i], "/")
  
  # Lista todos os arquivos .asc na pasta da espécie atual
  pred_path <- list.files(path = pred_folder, pattern = '*.asc$')
  
  # Monta os caminhos completos para os arquivos
  pred_files <- paste(pred_folder, pred_path, sep = "")
  
  # Carrega todos os preditores como um único objeto raster
  pred0 <- rast(pred_files)
  
  # Processamento dos rasters:
  # 1. Substitui valores 0 por 100 para evitar problemas matemáticos
  pred <- subst(pred0, 0, 100)
  
  # 2. Normaliza os valores dividindo cada camada por ela mesma (resultando em 1s)
  pred <- pred / pred
  
  # 3. Multiplica todas as camadas para criar uma máscara única
  #    (onde qualquer NA em qualquer camada resulta em NA na máscara final)
  predNA <- prod(pred, na.rm = FALSE)
  
  # Plota a máscara de NA gerada com o nome da espécie como título
  plot(predNA, main = paste0("Máscara de NA ", names[i]))
  
  # Salva a máscara como arquivo TIFF na pasta de output
  writeRaster(predNA, paste0("./output/predNA/", names[i], "_predNA.tiff"))
}
  


