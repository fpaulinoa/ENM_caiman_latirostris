# Modelagem de nicho ecológico
# by Fernando Paulino e Carolina Loss 
# JAN - 2025

# Carrega a biblioteca 'terra' para manipulação de dados espaciais
library(terra)

# Define o caminho da pasta onde estão os arquivos de preditores bioclimáticos
bio_folder = "./output/predictors/predmod/"

# Lista todos os arquivos com extensão .asc na pasta de preditores
bio_path <- list.files(path = bio_folder, pattern='*.asc$')

# Cria os caminhos completos para cada arquivo .asc
bio_files <- paste(bio_folder, bio_path, sep="")

# Carrega todos os arquivos de preditores como um único objeto raster
bio <- rast(bio_files)

# Carrega um shapefile de áreas de calibração com sistema de referência WGS84
b <- vect("./data/shapes/calibrarea.shp", crs = "EPSG:4326")

# Plota o shapefile das áreas de calibração
plot(b)

# Loop para processar cada polígono do shapefile individualmente
for (i in 1:length(b)){
  # Seleciona um polígono por vez
  v <- b[i]
  
  # Recorta os rasters bioclimáticos para a extensão do polígono
  c <- crop(bio, v)
  
  # Aplica máscara usando o polígono (valores NA fora da área)
  m <- mask(c, v)
  
  # Obtém o identificador único (CLADO) do polígono
  id <- v$CLADO
  
  # Cria um diretório de saída com o nome do clado
  out.dir <- paste0("./output/predictors/predmod/predmodcrop/", id, "/")
  dir.create(out.dir)
  
  # Salva cada camada raster recortada como arquivo .asc
  writeRaster(m, paste0(out.dir, names(m), ".asc"))
}

# Carrega um raster recortado específico (Caiman latirostris, variável bio1) para visualização
caminho <- rast("./output/predictors/predmod/predmodcrop/Caiman_latirostris/bio1.asc")

# Plota o raster recortado
plot(caminho)