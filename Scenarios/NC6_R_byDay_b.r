start_time <- Sys.time()

# ---- Seed ----
# Carregar o estado guardado
load("estado_final_C5.RData") # De R.Data

# Restaurar
.Random.seed <- estado_final_C5

# ---- Data frame dos controlos - informação proveniente da bula dos controlos ----
controlos_info <- data.frame(
  nome = c("PC ISD 1", "PC ISD 2", "PC ISD 3"),
  Target = c(2.50, 9.20, 16.5),
  SD = c(0.25, 0.92, 1.65)
)

# ---- Nome e Valor alvo das amostras ----
ISD1_nome <- "PC ISD 1"
ISD2_nome <- "PC ISD 2"
ISD3_nome <- "PC ISD 3"
ISD1_Target <- 2.50
ISD2_Target <- 9.20
ISD3_Target <- 16.5

# ---- Precisão base - conforme ----
## Calcula alegações de CV do fabricante por interpolação
  cv_df_ISD1 <- cv_esperado(dados_cv, ISD1_Target)
  cv_df_ISD2 <- cv_esperado(dados_cv, ISD2_Target)
  cv_df_ISD3 <- cv_esperado(dados_cv, ISD3_Target)

## Estimativas de precisão **EM CONFORMIDADE com** alegações do fabricante em SD
ISD1_SD_Repetibilidade <- ISD1_Target*cv_df_ISD1$CVr_esperado # CVr 0.0373
ISD2_SD_Repetibilidade <- ISD2_Target*cv_df_ISD2$CVr_esperado # CVr 0.0300
ISD3_SD_Repetibilidade <- ISD3_Target*cv_df_ISD3$CVr_esperado # CVr 0.0239
ISD1_SD_Whithin_Laboratory <- ISD1_Target*cv_df_ISD1$CVwl_esperado # CVwl 0.0433
ISD2_SD_Whithin_Laboratory <- ISD2_Target*cv_df_ISD2$CVwl_esperado # CVwl 0.0361
ISD3_SD_Whithin_Laboratory <- ISD3_Target*cv_df_ISD3$CVwl_esperado # CVwl 0.0316

CVwl_max <- round(max(ISD1_SD_Whithin_Laboratory/ISD1_Target, ISD2_SD_Whithin_Laboratory/ISD2_Target, ISD3_SD_Whithin_Laboratory/ISD3_Target, na.rm = TRUE)*100)

## atributos_df base

### ---- Desenho de simulação base ----
dias <- 5
n_reps <- 5
n_QC <-1
correlacao = 0.71

max_Iwl = 18

# ---- 525 - 5 dias, 2 amostras - simulações ----
## ---- -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ----
print("-=-=-=-=-= 5x2x5 -=-=-=-=-=")
############################################################################
#################### **5 dias, 2 amostras - simulações** ###################
############################################################################

## ---- Amostras ----
# -- 2 amostras --
atributos_df <- criar_tabela_atributos()
atributos_df <- adicionar_atributos_amostra(atributos_df, ISD1_nome, ISD1_Target, ISD1_SD_Repetibilidade, ISD1_SD_Whithin_Laboratory)

atributos_df <- adicionar_atributos_amostra(atributos_df, ISD3_nome, ISD3_Target, ISD3_SD_Repetibilidade, ISD3_SD_Whithin_Laboratory)

## alterações de Justeza de medição
reset_variables

# ---- variáveis calculadas ----
nSample <- length(unique(atributos_df$Amostra))
vies <- max(viesr, viesa)
CVr <- max(atributos_df$SD_repetibilidade/atributos_df$Target, na.rm = TRUE)
CVwl <- max(atributos_df$SD_Within_Laboratory/atributos_df$Target, na.rm = TRUE)
CVr <- round(CVr * 100)
CVwl <- round(CVwl * 100)
vies <- round(vies * 100)
Iwl <- 16 # Imprecisão total


### Mostrar estatísticas
# Copiar o df original
df_temp <- atributos_df

# Calcular CVs em percentagem
df_temp$CVr_percent <- (df_temp$SD_repetibilidade / df_temp$Target) * 100
df_temp$CVwl_percent <- (df_temp$SD_Within_Laboratory / df_temp$Target) * 100

# Opcional: remover colunas SD e manter só os CV
df_temp <- df_temp[, c("Amostra", "Target", "CVr_percent", "CVwl_percent")]

# Arredondar para 2 casas decimais
df_temp$CVr_percent <- round(df_temp$CVr_percent, 2)
df_temp$CVwl_percent <- round(df_temp$CVwl_percent, 2)

## Simulações

# ---- Abre Excel ----
wb <- createWorkbook()
MSExcel_filename <- paste("resultados_nDias_Var_generation_test", nSim, Iwl, sep = "_")
MSExcel_filename <- paste0(MSExcel_filename, ".xlsx") 

# ---- Inicializa performance_df_masterC6525 ----
performance_df_masterC6525 <- inicializar_df_performance()
df_guia_master <- data.frame(Avaliacao = todas_categorias, stringsAsFactors = FALSE)

print("Looping")
cat("Iwl= ", Iwl)
# ---- Loop ----
for(dias in c(2:5,7,10,15,22,100, 1000)){
  
  SDr1_new <- ISD1_SD_Repetibilidade + (ISD1_Target * (Ir / 100))
  SDwl1_new <- ISD1_SD_Whithin_Laboratory + (ISD1_Target * (Iwl / 100))
  SDr3_new <- ISD3_SD_Repetibilidade + (ISD3_Target * (Ir / 100))
  SDwl3_new <- ISD3_SD_Whithin_Laboratory + (ISD3_Target * (Iwl / 100))

  atributos_df <- criar_tabela_atributos()
  atributos_df <- adicionar_atributos_amostra(atributos_df, ISD1_nome, ISD1_Target, SDr1_new, SDwl1_new)
  atributos_df <- adicionar_atributos_amostra(atributos_df, ISD3_nome, ISD3_Target, SDr3_new, SDwl3_new)
  
  # ---- Sheetname ----
  # CVwl <- max(atributos_df$SD_Within_Laboratory/atributos_df_alterado$Target, na.rm = TRUE)
  CVwl <- max(atributos_df$SD_Within_Laboratory/atributos_df$Target, na.rm = TRUE)
  CVwl <- round(CVwl * 100)
  
  #MSExcel_sheetname <- paste(CVr, CVwl, vies, sep = "_")
  MSExcel_sheetname <- paste("dias=", dias)
  
   # ---- Inicializa data frames ----
  resultados <- inicializar_resultados()
  amostras_df <- criar_df_amostras(atributos_df$Amostra)
  cover_df <- inicializar_cv_dataframe(amostras_df, nSim)
  performance <- inicializar_df_performance()
  
  # ---- Realiza a simulação ----
  for(i in 1:nSim){
    
   # Gera os dados para QC e Amostras
    targets_dia <- gerar_targets_diarios(atributos_df, dias, correlacao)
    dados_simulados <- gerar_replicados_correlacionados(targets_dia, atributos_df, n_reps, n_QC = 1)
  
        # Guarda as métricas para calculo do desempenho da simulação - cover_df
    cover_df <- atualizar_cv_dataframe(cover_df, i, dados_simulados, atributos_df, amostras_df$Nome)
    
  }
  
  # ---- Atualização de Performance master ----
  performance_tmp <- atualizar_performance_df(cover_df, amostras_df, atributos_df, inicializar_df_performance())

  # Adicionar coluna para identificar Iwl
  # performance_tmp$IWL <- Iwl
  performance_tmp$IWL <- dias

  # Acumular no master
  performance_df_masterC6525 <- rbind(performance_df_masterC6525, performance_tmp)
  
  
  
}
print("End of Loop")

## performance_df_masterC6525

  addWorksheet(wb, "performance_df_masterC6525")
  writeData(wb, sheet = "performance_df_masterC6525", x = performance_df_masterC6525)

### ---- Avaliacao do desempenho da simulação ----
performance_df_masterC6525$Estatistica <- stringi::stri_enc_toutf8(performance_df_masterC6525$Estatistica)
performance_df_masterC6525$Metrica <- stringi::stri_enc_toutf8(performance_df_masterC6525$Metrica)
performance_tf <- formatar_performance_kable_word(performance_df_masterC6525)

# ---- Tabelas CVwl, CVr e Media wide ----
# -- CVr (%) --
tabela_cvr_wide <- pivotar_juntar_amostras(performance_df_masterC6525, "CVr", atributos_df$Amostra)

# -- Adicionar nova sheet no workbook --
addWorksheet(wb, "CVr_T_Pivoted")

# -- Escrever tabela pivotada CVr --
writeData(wb, sheet = "CVr_T_Pivoted", x = tabela_cvr_wide, startRow = 1, startCol = 1, withFilter = TRUE)

# -- CVwl (%) --
tabela_cvwl_wide <- pivotar_juntar_amostras(performance_df_masterC6525, "CVwl", atributos_df$Amostra)

# -- Adicionar nova sheet no workbook --
addWorksheet(wb, "CVwl_T_Pivoted")

# -- Escrever tabela pivotada CVwl --
writeData(wb, sheet = "CVwl_T_Pivoted", x = tabela_cvwl_wide, startRow = 1, startCol = 1, withFilter = TRUE)

# -- Media (ng/ml) --
tabela_Media_wide <- pivotar_juntar_amostras(performance_df_masterC6525, "Media", atributos_df$Amostra)

# ---- Saves & Cleans ----
## -- Adicionar nova sheet no workbook --
addWorksheet(wb, "Media_T_Pivoted")

## -- Escrever tabela pivotada CVwl --
writeData(wb, sheet = "Media_T_Pivoted", x = tabela_Media_wide, startRow = 1, startCol = 1, withFilter = TRUE)

## -- Limpar performance_tf --
performance_tf <- performance_tf[!duplicated(performance_tf[, c("Amostra", "Estatística", "Métrica")]), ]

## -- Salvar workbook novamente --
saveWorkbook(wb, MSExcel_filename, overwrite = TRUE)

save(performance_df_masterC6525, file="performance_df_masterC6525")
## Clean

## ---- Guardar estados ----
# Guarda o estado atual do gerador de números aleatórios
estado_final_C6_b <- .Random.seed

# -->Guardar estado final como csv
save(estado_final_C6_b, file = "estado_final_C6_b.RData")

# Restaura o estado guardado
.Random.seed <- estado_final_C6_b


# ---- Salvar workbook novamente ----
saveWorkbook(wb, MSExcel_filename, overwrite = TRUE)

# ---- reset vars ----
atributos_df <- criar_tabela_atributos()
atributos_df <- adicionar_atributos_amostra(atributos_df, ISD1_nome, ISD1_Target*(1+viesr)+viesa, ISD1_SD_Repetibilidade, ISD1_SD_Whithin_Laboratory)
atributos_df <- adicionar_atributos_amostra(atributos_df, ISD2_nome, ISD2_Target*(1+viesr)+viesa, ISD2_SD_Repetibilidade, ISD2_SD_Whithin_Laboratory)
atributos_df <- adicionar_atributos_amostra(atributos_df, ISD3_nome, ISD3_Target*(1+viesr)+viesa, ISD3_SD_Repetibilidade, ISD3_SD_Whithin_Laboratory)
nSample <- length(unique(atributos_df$Amostra))
vies <- max(viesr, viesa)
CVr <- max(atributos_df$SD_repetibilidade/atributos_df$Target, na.rm = TRUE)
CVwl <- max(atributos_df$SD_Within_Laboratory/atributos_df$Target, na.rm = TRUE)
CVr <- round(CVr * 100)
CVwl <- round(CVwl * 100)
vies <- round(vies * 100)

# Calcular tempo final
end_time <- Sys.time()

# Calcular duração em segundos
duracao_segundos <- as.numeric(difftime(end_time, start_time, units = "secs"))

# Converter para minutos
duracao_minutos <- duracao_segundos / 60

# Imprimir em minutos com 2 casas decimais
print(cat("Tempo total de execução do R NC6:", round(duracao_minutos, 2), "minutos\n"))
