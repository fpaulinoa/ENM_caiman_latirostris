# Modelagem de nicho ecológico
# by Fernando Paulino e Carolina Loss 
# JAN - 2025

# Carrega a biblioteca 'terra' para manipulação de dados espaciais
library(terra)

# Define os diretórios de trabalho:
path_rasters <- "./output/predictors/Vars/"  # Caminho dos arquivos raster originais
path_shapes <- "./data/shapes/"              # Caminho das pastas com shapes das bacias
path_output <- "./output/predictors/"        # Caminho para salvar os resultados

# Lista todas as pastas de shapefiles (cada pasta representa uma bacia)
bacias <- list.dirs(path_shapes, recursive = FALSE)

# Lista todos os arquivos raster no formato .tif
rasters <- list.files(path_rasters, pattern = "\\.tif$", full.names = TRUE)

# Loop principal para processar cada bacia
for (bacia in bacias) {
  # Extrai o nome da bacia a partir do caminho da pasta
  nome_bacia <- basename(bacia)
  
  # Cria uma pasta de saída específica para esta bacia
  output_bacia <- file.path(path_output, paste0(nome_bacia, "_rast"))
  dir.create(output_bacia, recursive = TRUE, showWarnings = FALSE)
  
  # Encontra e carrega o primeiro arquivo .shp encontrado na pasta da bacia
  shape <- vect(list.files(bacia, pattern = "\\.shp$", full.names = TRUE)[1])
  
  # Processa cada raster para a bacia atual
  for (raster in rasters) {
    # Mantém o nome original do arquivo raster
    nome_raster <- basename(raster)
    
    # Carrega o raster atual
    rast <- rast(raster)
    
    # Recorta o raster para a extensão da bacia
    rast_crop <- crop(rast, shape)
    
    # Aplica máscara usando o polígono da bacia (valores NA fora da área)
    rast_mask <- mask(rast_crop, shape)
    
    # Salva o raster processado mantendo o nome original
    writeRaster(rast_mask, file.path(output_bacia, nome_raster), overwrite = TRUE)
  }
}

# Mensagem de conclusão do processo
cat("Processo concluído! Rasters recortados e organizados por bacias.\n")