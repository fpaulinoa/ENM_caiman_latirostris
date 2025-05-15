# Modelagem de nicho ecológico
# by Fernando Paulino e Carolina Loss 
# JAN - 2025

library(terra)  # Carrega o pacote terra para trabalhar com rasters

# Lista de espécies a serem processadas
species_list <- c("Caimamn_gpi", "Caiman_flumi", "Caiman_gac", 
                  "Caiman_latirostris", "Caiman_nocaa", "Caiman_paran", 
                  "Caiman_saof", "Caiman_norma")

# Diretório principal onde estão os arquivos raster organizados por espécie
base_dir <- "/Users/nando/Documents/Mestrado/RFiles/output/models"

# Loop para processar cada espécie
for (species in species_list) {
  species_dir <- file.path(base_dir, species)  # Define o caminho da pasta da espécie
  
  # Lista todas as pastas "run_X" dentro da pasta da espécie
  run_dirs <- list.dirs(species_dir, recursive = FALSE)
  
  raster_list <- list()  # Inicializa uma lista vazia para armazenar os rasters de todas as runs
  
  # Loop para processar cada "run_X" da espécie
  for (run in run_dirs) {
    # Lista todos os arquivos .tif dentro da pasta da run
    raster_files <- list.files(run, pattern = "\\.tif$", full.names = TRUE)
    
    if (length(raster_files) > 0) {  # Verifica se existem rasters na pasta
      rasters <- rast(raster_files)  # Carrega os rasters encontrados
      raster_list <- c(raster_list, as.list(rasters))  # Adiciona os rasters na lista geral (corrigido usando as.list)
    }
  }
  
  # Após percorrer todas as runs
  if (length(raster_list) > 0) {  # Verifica se algum raster foi carregado
    all_rasters <- rast(raster_list)  # Empilha todos os rasters coletados
    mean_raster <- mean(all_rasters)  # Calcula a média pixel a pixel
    
    # Define o caminho para salvar o raster médio
    output_path <- file.path(base_dir, paste0(species, "_mean.tif"))
    writeRaster(mean_raster, output_path, overwrite = TRUE)  # Salva o raster médio no disco, sobrescrevendo se já existir
    
    print(paste("Mapa médio salvo para:", species))  # Mensagem informando que o mapa foi salvo
  } else {
    print(paste("Nenhum raster encontrado para:", species))  # Mensagem se não houver rasters
  }
}
