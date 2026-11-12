############################################################
# title: "NC5c_R"
# author: "Bruno"
# date: "2025-03-15"
############################################################
# ---- Start ----
start_time <- Sys.time()

print ("######################## NC5c ########################")
print ("############ Alteração de viés relativo, com Iwl #############")

## ---- Data frame dos controlos - informação proveniente da bula dos controlos ----
controlos_info <- data.frame(
  nome = c("PC ISD 1", "PC ISD 2", "PC ISD 3"),
  Target = c(2.50, 9.20, 16.5),
  SD = c(0.25, 0.92, 1.65) # 10%
)

## ---- Nome e Valor alvo das amostras, Precisão ----
ISD1_nome <- "PC ISD 1"
ISD2_nome <- "PC ISD 2"
ISD3_nome <- "PC ISD 3"
ISD1_Target <- 2.50
ISD2_Target <- 9.20
ISD3_Target <- 16.5

## -- Precisão base - conforme --
## -- Calcula alegações de CV do fabricante por interpolação --
cv_df_ISD1 <- cv_esperado(dados_cv, ISD1_Target)
cv_df_ISD2 <- cv_esperado(dados_cv, ISD2_Target)
cv_df_ISD3 <- cv_esperado(dados_cv, ISD3_Target)

## -- Estimativas de precisão **EM CONFORMIDADE com** alegações do fabricante em SD --
ISD1_SD_Repetibilidade <- ISD1_Target*cv_df_ISD1$CVr_esperado # CVr 0.0373
ISD2_SD_Repetibilidade <- ISD2_Target*cv_df_ISD2$CVr_esperado # CVr 0.0300
ISD3_SD_Repetibilidade <- ISD3_Target*cv_df_ISD3$CVr_esperado # CVr 0.0239
ISD1_SD_Whithin_Laboratory <- ISD1_Target*cv_df_ISD1$CVwl_esperado # CVwl 0.0433
ISD2_SD_Whithin_Laboratory <- ISD2_Target*cv_df_ISD2$CVwl_esperado # CVwl 0.0361
ISD3_SD_Whithin_Laboratory <- ISD3_Target*cv_df_ISD3$CVwl_esperado # CVwl 0.0316

# -- Calcula Sb base Não necessário - Commented --
# Sb1 <- sqrt(max(ISD1_SD_Whithin_Laboratory^2 - ISD1_SD_Repetibilidade^2, 0))
# Sb2 <- sqrt(max(ISD2_SD_Whithin_Laboratory^2 - ISD2_SD_Repetibilidade^2, 0))
# Sb3 <- sqrt(max(ISD3_SD_Whithin_Laboratory^2 - ISD3_SD_Repetibilidade^2, 0))

# -- Offset - Determina CVwl máximo de entre as alegações --
# CVwl_max <- round(max(ISD1_SD_Whithin_Laboratory/ISD1_Target, ISD2_SD_Whithin_Laboratory/ISD2_Target, ISD3_SD_Whithin_Laboratory/ISD3_Target, na.rm = TRUE)*100)
Media_min <- min(ISD1_Target, ISD2_Target, ISD3_Target)

## atributos_df base

## ---- Desenho de simulação base ----

dias <- 5
n_reps <- 5
n_QC <-1
correlacao = 0.71

# max_Iwl = 6
# max_vies <- 20

min_vies <- -10
max_vies <- 10

## -- nSim --
# definido em Run.R
# nSim <- 1600
# nSim <- 1600

## ---- Seed ----
# Carregar o estado guardado
# load("NC1_FinalState.RData") # De RMd
load("estado_final_C5b.RData") # De R.Data

# Restaurar
.Random.seed <- estado_final_C5b

## ---- -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ----
# ---- 525 - 5 dias, 2 amostras - simulações** ----
print("-=-=-=-=-= NC5c1 5x2x5 -=-=-=-=-=")

### ---- Variáveis mutantes ----
Ir <- 0 # Imprecisão de repetibilidade
Iwl <- 1 # Imprecisão total
### -- alterações de Justeza de medição --
viesr <- 0 # viés relativo
viesa <- 0 # viés absoluto

## ---- Amostras ----
## -- 2 amostras --
atributos_df <- criar_tabela_atributos()
atributos_df <- adicionar_atributos_amostra(atributos_df, ISD1_nome, ISD1_Target, ISD1_SD_Repetibilidade, ISD1_SD_Whithin_Laboratory)
# atributos_df <- adicionar_atributos_amostra(atributos_df, ISD2_nome, ISD2_Target, ISD2_SD_Repetibilidade, ISD2_SD_Whithin_Laboratory)
atributos_df <- adicionar_atributos_amostra(atributos_df, ISD3_nome, ISD3_Target, ISD3_SD_Repetibilidade, ISD3_SD_Whithin_Laboratory)

## ---- variáveis calculadas para filename ----
nSample <- length(unique(atributos_df$Amostra))
vies <- max(viesr, viesa)
CVr <- max(atributos_df$SD_repetibilidade/atributos_df$Target, na.rm = TRUE)
CVwl <- max(atributos_df$SD_Within_Laboratory/atributos_df$Target, na.rm = TRUE)
CVr <- round(CVr * 100)
CVwl <- round(CVwl * 100)
vies <- round(vies * 100)

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

## ---- Abre Excel ----
wb <- createWorkbook()
MSExcel_filename <- paste("resultados Cenário 5c", nSim, dias, nSample, n_reps, Iwl, sep = "_")
MSExcel_filename <- paste0(MSExcel_filename, ".xlsx") 

## ---- ########## Simulações 5x2x5 ########## ----
## ---- Inicializa performance_df_masterC5c525 ----
performance_df_masterC5c525 <- inicializar_df_performance()
df_guia_master <- data.frame(Avaliacao = todas_categorias, stringsAsFactors = FALSE)

## ---- Loop ----
print("Looping")
# cat("min_vies: ", min_vies, "\n")
# cat("max_vies: ", max_vies, "\n")

# for(viesa in seq(min_vies, max_vies, by = 2))
for(viesa in seq(min_vies, max_vies)){
  # cat("viesa: ", viesa, "\n")
  ## ---- Criar atributos_df - alteração da componente intersérie ----
  
  bias <- 1+viesa/100
  bias <- 1+viesa/100
  
  atributos_df <- criar_tabela_atributos()
  atributos_df <- adicionar_atributos_amostra(atributos_df, ISD1_nome, ISD1_Target*bias, ISD1_SD_Repetibilidade, ISD1_SD_Whithin_Laboratory+(Iwl/100)*ISD1_Target)
  # atributos_df <- adicionar_atributos_amostra(atributos_df, ISD2_nome, ISD2_Target*bias, ISD2_SD_Repetibilidade, ISD2_SD_Whithin_Laboratory)
  atributos_df <- adicionar_atributos_amostra(atributos_df, ISD3_nome, ISD3_Target*bias, ISD3_SD_Repetibilidade, ISD3_SD_Whithin_Laboratory+(Iwl/100)*ISD3_Target)
    # print("atributos_df definido")
    
  ### ---- Sheetname ----
  # CVwl <- max(atributos_df$SD_Within_Laboratory/atributos_df_alterado$Target, na.rm = TRUE)
  # CVwl <- max(atributos_df$SD_Within_Laboratory/atributos_df$Target, na.rm = TRUE)
  # CVwl <- round(CVwl * 100)
  
  #MSExcel_sheetname <- paste(CVr, CVwl, vies, sep = "_")
  MSExcel_sheetname <- paste("viesa=", viesa)
  
    ## ---- Inicializa data frames ----
  resultados <- inicializar_resultados()
  amostras_df <- criar_df_amostras(atributos_df$Amostra)
  cover_df <- inicializar_cv_dataframe(amostras_df, nSim)
  performance <- inicializar_df_performance()
  # print("data frames inicializados")
  
  ## ---- Realiza a simulação ----
  for(i in 1:nSim){
    # print("In ciclo for()")
    # Gera os dados para QC e Amostras
    targets_dia <- gerar_targets_diarios(atributos_df, dias, correlacao)
    # print("targets_dia atribuido")
    dados_simulados <- gerar_replicados_correlacionados(targets_dia, atributos_df, n_reps, n_QC = 1)
    # print("dados_simulados criado")
    
    # Calcula e guarda os resultados da simulação - df resultados
    resultados <- Faz_as_contas(i,dados_simulados, resultados)
    # print("resultados calculados")
    
    # Guarda as métricas para calculo do desempenho da simulação - cover_df
    cover_df <- atualizar_cv_dataframe(cover_df, i, dados_simulados, atributos_df, amostras_df$Nome)
    # print("cover_df criado")
  }
  # print("Out for(){}")
  
  ## ---- Atualização de Performance master ----
  performance_tmp <- atualizar_performance_df(cover_df, amostras_df, atributos_df, inicializar_df_performance())
  # print("performance_tmp atualizado")
  
  # Adicionar coluna para identificar Iwl
  # performance_tmp$viesa <- viesa
  # performance_tmp$Ir <- Ir
  performance_tmp$viesa <- viesa
  
  # Acumular no master
  performance_df_masterC5c525 <- rbind(performance_df_masterC5c525, performance_tmp)
  # print("performance_df_masterC5c525 atribuido")
  
  ## ---- Atualização de avaliação master ----
  df_avaliacao_tmp <- avaliar_guia(resultados)
  
  # -- linhas_faltantes --
  linhas_faltantes <- setdiff(todas_categorias, df_avaliacao_tmp$Avaliacao)
  
  if (length(linhas_faltantes) > 0) {
    df_faltantes <- data.frame(
      Avaliacao = linhas_faltantes,
      Frequencia = 0,
      Percentagem = 0,
      stringsAsFactors = FALSE
    )
    df_avaliacao_tmp <- rbind(df_avaliacao_tmp, df_faltantes)
  }
  
  # Adicionar coluna com valor de Iwl
  # df_avaliacao_tmp$Iwl <- paste0("IWL_", Iwl)
  df_avaliacao_tmp$viesa <- paste0("viesa_", viesa)
  
  # Ordenar
  df_avaliacao_tmp <- df_avaliacao_tmp[match(todas_categorias, df_avaliacao_tmp$Avaliacao), ]
  
  # -- Pivotar: queremos colunas por Iwl --
  df_avaliacao_tmp_wide <- df_avaliacao_tmp[, c("Avaliacao", "Percentagem", "viesa")]
  colnames(df_avaliacao_tmp_wide)[2] <- paste0(df_avaliacao_tmp_wide$viesa[1], "_Valor")
  
  df_avaliacao_tmp_wide$viesa <- NULL
  
  ## ---- Merge ----
  df_guia_master <- merge(df_guia_master, df_avaliacao_tmp_wide, by = "Avaliacao", all.x = TRUE)
  
  ## ---- Escrever Excel - Adicionar folhas e escrever dados no doc MS Excel ----
  # -- Avaliação do desempenho da simulação --
  addWorksheet(wb, MSExcel_sheetname)
  writeData(wb, sheet = MSExcel_sheetname, x = resultados)
  
}
print("End of Looping")

## ---- Acrescentar "(%)" ao campo Avaliacao antes de usar funções ----
df_guia_master$Avaliacao <- paste0(df_guia_master$Avaliacao, " (%)")

# adicionar todas
df_guia_master <- adicionar_sens_espec_ba(df_guia_master, opcoes = c("CVwl", "CQI", "CVr", "bias"))



# Guardar versão completa
#df_guia_master_C5c_5x2x5 <- df_guia_master
df_guia_master_C5c_5x2x5 <- arredondar_df_guia_master(df_guia_master)

## ---- avaliação da eficácia do guia ----
addWorksheet(wb, "Avaliação_Guia")
writeData(wb, sheet = "Avaliação_Guia", x = df_guia_master_C5c_5x2x5)


## performance_df_masterC5c525
addWorksheet(wb, "performance_df_masterC5c525")
writeData(wb, sheet = "performance_df_masterC5c525", x = performance_df_masterC5c525)

## ----valiação do desempenho da simulação ----
performance_tf <- formatar_performance_kable_word(performance_df_masterC5c525)

# -- CVr (%) --
tabela_cvr_wide <- pivotar_juntar_amostras_ba(performance_df_masterC5c525, "CVr", atributos_df$Amostra)
# Adicionar nova sheet no workbook
addWorksheet(wb, "CVr_T_Pivoted")
# Escrever tabela pivotada CVr
writeData(wb, sheet = "CVr_T_Pivoted", x = tabela_cvr_wide, startRow = 1, startCol = 1, withFilter = TRUE)

# -- CVwl (%) --
  tabela_cvwl_wide <- pivotar_juntar_amostras_ba(performance_df_masterC5c525, "CVwl", atributos_df$Amostra)
# Adicionar nova sheet no workbook
  addWorksheet(wb, "CVwl_T_Pivoted")
# Escrever tabela pivotada CVwl
  writeData(wb, sheet = "CVwl_T_Pivoted", x = tabela_cvwl_wide, startRow = 1, startCol = 1, withFilter = TRUE)

# -- Média (ng/ml) --
tabela_Media_wide <- pivotar_juntar_amostras_ba(performance_df_masterC5c525, "Media", atributos_df$Amostra)
# Adicionar nova sheet no workbook
addWorksheet(wb, "Media_T_Pivoted")

## ---- Escrever tabela pivotada CVwl ----
writeData(wb, sheet = "Media_T_Pivoted", x = tabela_Media_wide, startRow = 1, startCol = 1, withFilter = TRUE)

## ---- Limpar performance_tf ----
# Limpar duplicados antes de usar
performance_tf <- performance_tf[!duplicated(performance_tf[, c("Amostra", "Estatística", "Métrica")]), ]

## ---- Salvar workbook novamente ----
saveWorkbook(wb, MSExcel_filename, overwrite = TRUE)

## ---- Clean ----
rm(df_guia_master, cover_df, resultados, amostras_df, performance, tabela_Media_wide, tabela_cvwl_wide, tabela_cvr_wide, atributos_df)
save(performance_df_masterC5c525, file="performance_df_masterC5c525")

# Save WB
## ---- Salvar workbook novamente ----
saveWorkbook(wb, MSExcel_filename, overwrite = TRUE)

## ---- Guardar estados ----
# Guarda o estado atual do gerador de números aleatórios
estado_final_C5c1 <- .Random.seed

# -- Guardar estado final como csv
save(estado_final_C5c1, file = "estado_final_C5c1.RData")

# Restaura o estado guardado
.Random.seed <- estado_final_C5c1

# Calcular tempo final
end_time <- Sys.time()

# Calcular duração em segundos
duracao_segundos <- as.numeric(difftime(end_time, start_time, units = "secs"))

# Converter para minutos
duracao_minutos <- duracao_segundos / 60

# Imprimir em minutos com 2 casas decimais
print(cat("Tempo total de execução do R Markdown:", round(duracao_minutos, 2), "minutos\n"))