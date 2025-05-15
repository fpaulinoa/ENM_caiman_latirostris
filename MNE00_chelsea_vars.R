# Modelagem de nicho ecológico
# by Fernando Paulino e Carolina Loss 
# JAN - 2025

# Instalar e carregar os pacotes necessários
library(dismo)  # Pacote para funções de modelagem ecológica (inclui a função biovars)
library(terra)  # Pacote para manipulação de dados raster

# Definir os caminhos dos arquivos TIF de temperatura mínima, máxima e precipitação
tmin_dir <- "D:/dadosR/Corte R/dados/cropfiles/tmin"
tmax_dir <- "D:/dadosR/Corte R/dados/cropfiles/tmax"
precip_dir <- "D:/dadosR/Corte R/dados/cropfiles/pr"

# Listar todos os arquivos .tif nas pastas especificadas
tmin_files <- list.files(tmin_dir, pattern = ".tif$", full.names = TRUE)
tmax_files <- list.files(tmax_dir, pattern = ".tif$", full.names = TRUE)
precip_files <- list.files(precip_dir, pattern = ".tif$", full.names = TRUE)

# Organizar os arquivos em ordem alfabética para manter a sequência correta
tmin_files <- sort(tmin_files)
tmax_files <- sort(tmax_files)
precip_files <- sort(precip_files)

# Criar RasterStacks a partir dos arquivos
tmin_stack <- stack(tmin_files)
tmax_stack <- stack(tmax_files)
precip_stack <- stack(precip_files)

# Verificar o número de camadas (layers) em cada RasterStack
print(nlayers(tmin_stack))
print(nlayers(tmax_stack))
print(nlayers(precip_stack))

# Se houver mais de 12 camadas (ou seja, dados de vários anos), calcular a média mensal
if (nlayers(tmin_stack) > 12) {
  tmin_monthly <- stackApply(tmin_stack, indices = rep(1:12, times = nlayers(tmin_stack) / 12), fun = mean)
  tmax_monthly <- stackApply(tmax_stack, indices = rep(1:12, times = nlayers(tmax_stack) / 12), fun = mean)
  precip_monthly <- stackApply(precip_stack, indices = rep(1:12, times = nlayers(precip_stack) / 12), fun = mean)
} else {
  # Se já tiver 12 camadas, apenas usar os RasterStacks como estão
  tmin_monthly <- tmin_stack
  tmax_monthly <- tmax_stack
  precip_monthly <- precip_stack
}

# Calcular as variáveis bioclimáticas (BIOCLIM) usando as médias mensais
bioclim_vars <- biovars(prec = precip_monthly, tmin = tmin_monthly, tmax = tmax_monthly)

# Salvar cada variável BIOCLIM separadamente como arquivos .tif
for (i in 1:19) {
  writeRaster(bioclim_vars[[i]], paste0("BIO", i, ".tif"), overwrite = TRUE)
}
