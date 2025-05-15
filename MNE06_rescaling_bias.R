# Modelagem de nicho ecológico
# by Fernando Paulino e Carolina Loss 
# JAN - 2025

# Carrega a biblioteca 'terra' para manipulação de dados espaciais
library(terra)

# Define função para reescalonar valores para um novo intervalo
rescale <- function(x, x.min = NULL, x.max = NULL, new.min = 0, new.max = 1) {
  if(is.null(x.min)) x.min = min(x)  # Se mínimo não for fornecido, usa o mínimo dos dados
  if(is.null(x.max)) x.max = max(x)  # Se máximo não for fornecido, usa o máximo dos dados
  new.min + (x - x.min) * ((new.max - new.min) / (x.max - x.min))  # Fórmula de reescalonamento
}

## Carrega arquivo de viés amostral gerado no QGIS
# O arquivo de viés indica áreas com maior probabilidade de amostragem
b <- rast("./data/bias.tif", "EPSG:4326")  # Carrega com sistema de referência WGS84

## Define caminho das máscaras NA para todas as espécies
na_folder = "./output/predNA/"
na_path <- list.files(path = na_folder, pattern = '*.tiff$')  # Lista arquivos TIFF
na_files <- paste(na_folder, na_path, sep = "")  # Cria caminhos completos

# Extrai nomes das espécies a partir dos nomes dos arquivos
names <- gsub("./output/predNA/", "", na_files)  # Remove caminho
names <- gsub("_predNA.tiff", "", names)  # Remove sufixo

## Loop para processar cada espécie
for (i in 1:length(names)){
  
  # Carrega máscara NA para a espécie atual
  maskNA <- rast(na_files[i])  # Máscara com 1 (dentro da área) e NA (fora)
  
  # Prepara arquivo de viés:
  # 1. Redimensiona o viés para coincidir com a resolução/extensão da máscara
  bNA <- resample(b, maskNA)
  # 2. Aplica máscara para restringir à área de estudo
  bNA <- bNA * maskNA
  
  # Cria máscara inversa (0 dentro da área, NA fora)
  mask0 <- maskNA - 1  # Transforma 1 em 0, mantém NA
  
  # Combina máscara inversa com viés:
  # - Onde tem viés, mantém o valor do viés
  # - Onde não tem viés (NA), usa 0 da máscara inversa
  bias <- sum(mask0, bNA, na.rm = TRUE)
  
  # Garante que o viés final esteja restrito à área de estudo
  bias <- bias * maskNA
  
  # Reescalona o viés para intervalo 1-100 (para facilitar interpretação)
  b.scale <- rescale(x = bias,
                     x.min = minmax(bias)[1],  # Mínimo original
                     x.max = minmax(bias)[2],  # Máximo original
                     new.min = 1,  # Novo mínimo
                     new.max = 100) # Novo máximo
  
  # Salva arquivo de viés processado
  writeRaster(b.scale, paste0("./output/bias/", names[i], "_bias.tiff"))
}

## Visualização dos resultados (exemplo para 3 espécies)
r <- rast("./output/bias/Caimamn_gpi_bias.tiff")  # Carrega viés da espécie 1
r2 <- rast("./output/bias/Caiman_flumi_bias.tiff") # Carrega viés da espécie 2
r3 <- rast("./output/bias/Caiman_latirostris_bias.tiff") # Carrega viés da espécie 3

# Plota os mapas de viés
plot(r)   # Mapa de viés para Caimamn_gpi
plot(r2)  # Mapa de viés para Caiman_flumi
plot(r3)  # Mapa de viés para Caiman_latirostris