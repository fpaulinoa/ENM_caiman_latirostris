# Modelagem de nicho ecológico
# by Fernando Paulino e Carolina Loss 
# JAN - 2025

# Carrega os pacotes necessários
library(terra)
library(xlsx)
library(enmSdmX)

### Define o caminho para os modelos gerados
output_path <- "./output/models/"
species_list <- list.files(output_path, full.names = FALSE)

# Cria o diretório para armazenar as avaliações, se não existir
evaluation_path <- "./output/evaluation"
if (!dir.exists(evaluation_path)) {
  dir.create(evaluation_path)
}

# Inicializa um data frame vazio para armazenar as métricas de avaliação
eval_df <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(eval_df) <- c("species", "Replica", "Bin.Prob", "AUC", "wAUC", "TSS", "CBI")

# Loop principal: percorre cada espécie
for (species in species_list) {
  cat("Processando espécie:", species, "\n")
  species_path <- paste0(output_path, species, "/")
  
  # Loop para percorrer cada rodada (run_1 a run_20)
  for (run_num in 1:20) {
    run_folder <- paste0(species_path, "run_", run_num)
    
    # Verifica se a pasta da rodada existe
    if (!dir.exists(run_folder)) {
      cat("Aviso: Subpasta", run_folder, "não encontrada. Pulando...\n")
      next
    }
    
    # Carrega o arquivo de resultados do Maxent
    maxent_results_file <- paste0(run_folder, "/maxentResults.csv")
    if (!file.exists(maxent_results_file)) {
      cat("Aviso: maxentResults.csv não encontrado em", run_folder, "pulando...\n")
      next
    }
    maxres <- read.csv(maxent_results_file)
    
    # Lista os arquivos de predição de presença para cada replicado
    replicates <- list.files(run_folder, pattern = "_samplePredictions.csv")
    for (replicate in replicates) {
      replicate_name <- sub("_samplePredictions.csv", "", replicate)
      
      # Carrega os dados de predição de presença e fundo
      presence <- read.csv(paste0(run_folder, "/", replicate_name, "_samplePredictions.csv"))
      background <- read.csv(paste0(run_folder, "/", replicate_name, "_backgroundPredictions.csv"))
      
      # Extrai as predições
      predPres <- presence$Logistic.prediction
      predBg <- background$Logistic
      
      # Extrai o valor de AUC e o limiar (threshold) dos resultados do Maxent
      auc <- maxres[maxres[, 1] == replicate_name, "Test.AUC"]
      threshold <- maxres[maxres[, 1] == replicate_name, "Maximum.test.sensitivity.plus.specificity.Logistic.threshold"]
      
      # Calcula métricas de avaliação: TSS, AUC ponderado e CBI
      tss <- enmSdmX::evalTSS(pres = predPres, contrast = predBg, thresholds = threshold)
      auc_eval <- enmSdmX::evalAUC(pres = predPres, contrast = predBg)
      cbi <- enmSdmX::evalContBoyce(pres = predPres, contrast = predBg)
      
      # Imprime as métricas individuais para conferência
      print(list(
        species = species,
        run = run_num,
        replicate_name = replicate_name,
        AUC = auc,
        Threshold = threshold,
        TSS = tss,
        wAUC = auc_eval,
        CBI = cbi
      ))
      
      # Cria uma linha com as métricas e adiciona ao data frame geral
      eval_row <- data.frame(species, replicate_name, threshold, auc, auc_eval, tss, cbi)
      colnames(eval_row) <- c("species", "Replica", "Bin.Prob", "AUC", "wAUC", "TSS", "CBI")
      eval_df <- rbind(eval_df, eval_row)
    }
  }
}

# Imprime todas as avaliações realizadas
print("Avaliações completas:")
print(eval_df)

# Filtra os replicados válidos de acordo com os critérios estabelecidos
eval_df$decision <- ifelse(eval_df$wAUC >= 0.5 & eval_df$TSS >= 0.0 & eval_df$CBI >= 0.4, "include", "remove")
valid_replicates <- subset(eval_df, decision == "include")

# Imprime os replicados considerados válidos
print("Replicados válidos:")
print(valid_replicates)

# Se houver replicados válidos, calcula as médias das métricas por espécie
if (nrow(valid_replicates) > 0) {
  metric_average <- aggregate(valid_replicates[, 3:7], by = list(valid_replicates$species), FUN = mean)
  colnames(metric_average) <- c("species", "Bin.Prob", "AUC", "wAUC", "TSS", "CBI")
  
  # Exporta as métricas médias para CSV e Excel
  write.csv(metric_average, file = paste0(evaluation_path, "/validation_average.csv"), row.names = FALSE)
  write.xlsx(metric_average, file = paste0(evaluation_path, "/validation_average.xlsx"), sheetName = "Sheet1", showNA = FALSE)
  
  # Imprime as métricas médias
  print("Métricas médias por espécie:")
  print(metric_average)
} else {
  message("Nenhum replicado válido encontrado. Verifique os critérios ou as métricas calculadas.")
}
