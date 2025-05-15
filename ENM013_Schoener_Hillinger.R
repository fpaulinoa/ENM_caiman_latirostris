# Modelagem de nicho ecológico
# by Fernando Paulino e Carolina Loss 
# MAI - 2025
library(terra)      # Para manipulação de dados espaciais
library(ecospat)    # Para análises de sobreposição de nicho
library(ade4)       # Para análises multivariadas (PCA)
library(factoextra) # Para visualização de resultados de PCA

# Carrega os dados de ocorrência para Caiman gpi (região 1)
ocorrencias_regiao1 <- read.csv("./data/Caiman_gpi.csv", sep = ";", header = TRUE)
# Padroniza nomes das colunas para longitude (x) e latitude (y)
colnames(ocorrencias_regiao1) <- c("x", "y")  

# Carrega os dados de ocorrência para Caiman gac (região 2)
ocorrencias_regiao2 <- read.csv("./data/Caiman_gac.csv", sep = ";", header = TRUE)
colnames(ocorrencias_regiao2) <- c("x", "y")  # Padroniza nomes das colunas

# Verifica a estrutura dos dados carregados
head(ocorrencias_regiao2)      # Mostra as primeiras linhas
colnames(ocorrencias_regiao2)  # Verifica os nomes das colunas

# Lista arquivos .asc (formato raster) para cada região
arquivos_regiao1 <- list.files("./output/predictors/predmod/predmodcrop/Caiman_gpi/", 
                               pattern = ".asc$", full.names = TRUE)
arquivos_regiao2 <- list.files("./output/predictors/predmod/predmodcrop/Caiman_gac/", 
                               pattern = ".asc$", full.names = TRUE)

# Combina os arquivos em objetos SpatRaster multicamadas
env_regiao1 <- rast(arquivos_regiao1)  # Carrega todas as camadas para região 1
# Seleciona camadas específicas
env_regiao1 <- c(env_regiao1[[c(1,5,6,7,8,14,15,16,17,18,21,22,24,25,26,27)]])  
env_regiao2 <- rast(arquivos_regiao2)  # Carrega todas as camadas para região 2
# Mesma seleção de camadas
env_regiao2 <- c(env_regiao2[[c(1,5,6,7,8,14,15,16,17,18,21,22,24,25,26,27)]])  

# Verificação dos dados carregados
print(env_regiao1)  # Mostra metadados das variáveis ambientais
names(env_regiao1)  # Mostra os nomes das camadas/variáveis

# Converte pontos de ocorrência para objetos espaciais (SpatVector)
pontos_regiao1 <- vect(ocorrencias_regiao1, 
                       geom = c("x", "y"),      # Especifica colunas de coordenadas
                       crs = crs(env_regiao1))  # Usa o mesmo sistema de referência das variáveis

pontos_regiao2 <- vect(ocorrencias_regiao2, 
                       geom = c("x", "y"), 
                       crs = crs(env_regiao2))

# Padroniza nomes das variáveis para facilitar análises
#names(env_regiao1) <- paste0("var", 1:nlyr(env_regiao1))  # var1, var2, etc.
#names(env_regiao2) <- paste0("var", 1:nlyr(env_regiao2))

# Extrai valores ambientais nos pontos de ocorrência
vals_sp1 <- as.data.frame(terra::extract(env_regiao1, pontos_regiao1, ID = FALSE))  # Para espécie 1
vals_sp2 <- as.data.frame(terra::extract(env_regiao2, pontos_regiao2, ID = FALSE))  # Para espécie 2

# Cria matriz de background combinando valores ambientais de ambas regiões
vals_bg1 <- as.data.frame(terra::values(env_regiao1, na.rm = TRUE))  # Background região 1
vals_bg2 <- as.data.frame(terra::values(env_regiao2, na.rm = TRUE))  # Background região 2
combined_env <- rbind(vals_bg1, vals_bg2)                            # Combina os backgrounds

# Remove valores faltantes e verifica estrutura
combined_env <- na.omit(combined_env)
str(combined_env)  # Verifica se todas as variáveis são numéricas

# Realiza PCA no ambiente combinado
pca.env <- dudi.pca(combined_env,
                    scannf = FALSE,  # Não mostra gráfico interativo
                    nf = 2)          # Mantém 2 componentes principais

################################################################################
# Gráfico de correlações entre variáveis e eixos PCA
ade4::s.corcircle(pca.env$co, 
                 lab = colnames(combined_env), # Rótulos das variáveis
                 clabel = 1,                   # Tamanho dos rótulos
                 full = FALSE)                 # Mostra meio círculo

# Versão mais elaborada com factoextra
#fviz_pca_var(pca.env, repel = TRUE) +  # Gráfico das variáveis
 # ggtitle("Gráfico de Variáveis - PCA")

#fviz_pca_ind(pca.env, col.ind = "contrib") +  # Gráfico dos indivíduos
  #ggtitle("Gráfico de Indivíduos - PCA")
################################################################################

# Garante que as colunas estão na mesma ordem
vals_sp1 <- vals_sp1[, colnames(combined_env)]
vals_sp2 <- vals_sp2[, colnames(combined_env)]

# Projeta os pontos no espaço PCA
scores_sp1 <- suprow(pca.env, vals_sp1)$li  # Scores para espécie 1
scores_sp2 <- suprow(pca.env, vals_sp2)$li  # Scores para espécie 2
scores_bg  <- pca.env$li                    # Scores do background

# Cria grids climáticos para cada espécie
z1 <- ecospat.grid.clim.dyn(glob = scores_bg,    # Ambiente completo
                            glob1 = scores_sp1,  # Background espécie 1
                            sp = scores_sp1,     # Ocorrências espécie 1
                            R = 1000)             # Resolução da grade

z2 <- ecospat.grid.clim.dyn(glob = scores_bg,    # Ambiente completo
                            glob1 = scores_sp2,  # Background espécie 2
                            sp = scores_sp2,     # Ocorrências espécie 2
                            R = 1000)             # Resolução da grade

# Calcula métricas de sobreposição
overlap <- ecospat.niche.overlap(z1, z2, cor = TRUE)  # Considera correlação
print(overlap)  # Mostra os índices D (Schoener) e I (Hellinger)

# 1. Plotar sobreposição
ecospat.plot.niche.dyn(
  z1, 
  z2, 
  quant = 0.95, 
  title = ""  # Remove o título padrão
)

# Adiciona um título personalizado com margem ajustada
mtext(
  "Sobreposição de Nicho", 
  side = 3,     # Topo do gráfico
  line = 2.3,   # Distância da borda (aumente para afastar mais)
  cex = 1.2,    # Tamanho do texto
  font = 2      # Negrito
)

# 2. Testes de significância
set.seed(123)  # Para reprodutibilidade
eq_test <- ecospat.niche.equivalency.test(z1, z2, rep = 100)
sim_test <- ecospat.niche.similarity.test(z1, z2, rep = 100)

# 3. Plotar resultados dos testes
par(mfrow = c(1, 2))
ecospat.plot.overlap.test(eq_test, "D", "Teste de Equivalência")
ecospat.plot.overlap.test(sim_test, "D", "Teste de Similaridade")
