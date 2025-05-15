# Modelagem de nicho ecológico
# by Fernando Paulino e Carolina Loss 
# JAN - 2025

# Carrega a biblioteca 'terra' para manipulação de dados espaciais
library(terra)

## Processamento dos arquivos de preditores NA (criados anteriormente)

# Define o caminho da pasta com as máscaras NA
pred_folder = paste0("./output/predNA/")

# Lista todos os arquivos TIFF de máscaras NA
pred_path <- list.files(path = pred_folder, pattern='*.tiff$')

# Cria caminhos completos para os arquivos
pred_files <- paste(pred_folder, pred_path, sep="")

# Extrai os nomes das espécies a partir dos nomes dos arquivos:
# 1. Remove o caminho do arquivo
names <- gsub("./output/predNA/", "", pred_files)
# 2. Remove o sufixo '_predNA.tiff'
names <- gsub("_predNA.tiff", "", names)
# 3. (Opcional) Poderia converter underscores para espaços se necessário
# names <- gsub("_", " ", names)

## Carrega os dados de ocorrência das espécies
xy_all <- read.csv("./data/occour/Caimans.csv")
# Padroniza nomes científicos substituindo espaços por underscores
xy_all$sp <- gsub(" ", "_", xy_all$sp)

## Loop principal para processar cada espécie
for (i in 1:length(pred_files)){
  # Carrega a máscara NA para a espécie atual
  predNA <- rast(pred_files[i])
  
  # Filtra os pontos de ocorrência para a espécie atual
  xy <- xy_all[xy_all$sp == names[i],]
  
  # Converte pontos para objeto espacial (vetor) com CRS WGS84
  pts <- vect(xy, crs = "epsg:4326", geom = c("lon", "lat"))
  
  # Processamento espacial:
  # 1. Extrai valores da máscara NA para cada ponto
  pts$mask <- extract(predNA, pts)[,2]
  # 2. Identifica a célula do raster correspondente a cada ponto
  pts$cell <- cells(predNA, pts)[,2]
  # 3. Remove registros duplicados na mesma célula (thinning)
  pts <- pts[!duplicated(pts$cell),]
  
  # Identifica e salva pontos fora da área de preditores válidos (NA)
  pts_out <- pts[is.na(pts$mask), ] # Pontos com NA na máscara
  ptsNA <- as.data.frame(pts_out) # Converte para dataframe
  sp.name <- names[i]
  file_name <- paste0("./output/ptsNA/ptsNA_", sp.name, ".csv")
  write.csv(ptsNA, file_name, fileEncoding = "UTF-8")
  
  # Filtra apenas pontos com valores válidos de preditores
  pts <- pts[!is.na(pts$mask),]
  
  # Prepara dados finais para modelagem:
  # 1. Extrai coordenadas
  coords <- crds(pts)
  # 2. Converte para dataframe e junta com coordenadas
  pts_xy <- as.data.frame(pts)
  pts_xy <- cbind(pts_xy, coords)
  
  # Verifica número mínimo de registros para modelagem (>= 3)
  if (nrow(pts_xy) >= 3){
    file_name <- paste0("./output/pts_thin/xy_", sp.name, ".csv")
    write.csv(pts_xy, file_name, fileEncoding = "UTF-8") 
  } else {
    # Mensagem para espécies com poucos registros
    paste0(sp.name, " tem menos de 3 registros. Não será modelada.")
  }
}