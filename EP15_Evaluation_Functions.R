# title: "Funcões"
# author: "Bruno"
# date: "2025-03-15"

# ---- libraries ----
library(ggplot2)
library(lme4)
library(nlme)
library(openxlsx)
library(knitr)
library(kableExtra)
library(tidyr)
library(dplyr)

# ---- formatar_performance_kable_word - Corrigida ----
formatar_performance_kable_word <- function(performance_df) {
  
  # Atualizar nomes das estatísticas
  performance_df$Estatistica <- ifelse(
    performance_df$Estatistica == "CVr",
    "CVr (%)",
    ifelse(performance_df$Estatistica == "CVwl", "CVwl (%)",
           ifelse(performance_df$Estatistica == "Media", "Media (ng/ml)",
                  performance_df$Estatistica))
  )
  
  # Atualizar nome das métricas
  performance_df$Metrica <- ifelse(
    performance_df$Metrica == "Viés relativo" & performance_df$Estatistica %in% c("CVr (%)", "CVwl (%)"),
    "Viés relativo (% rel. ao CV)",
    ifelse(performance_df$Metrica == "Viés relativo", "Viés relativo (%)", 
           ifelse(performance_df$Metrica == "Cobertura", "Cobertura (%)", performance_df$Metrica))
  )
  
  # Apagar MCSE do Comprimento IC
  performance_df$MCSE <- ifelse(performance_df$Metrica == "Comprimento IC", NA, performance_df$MCSE)
  
  # Formatar coluna Valor
  performance_df$Valor_fmt <- sprintf("%.2f", performance_df$Valor)
  
  # Formatar coluna MCSE
  performance_df$MCSE_fmt <- ifelse(
    is.na(performance_df$MCSE), 
    "",
    sprintf("%.2f", performance_df$MCSE)
  )
  
  # Selecionar colunas
  tabela_final <- performance_df[, c("Amostra", "Estatistica", "Metrica", "Valor_fmt", "MCSE_fmt")]
  colnames(tabela_final) <- c("Amostra", "Estatística", "Métrica", "Valor", "MCSE")
  
  return(tabela_final)
}

# ---- Funcao para inicializar o data frame 'resultados' ----
inicializar_resultados <- function() {
  data.frame(
    Simulacao = integer(),
    Amostra = character(),
    CQI_eval = character(),
    Outliers = numeric (),
    CVr_esperado = numeric(),
    CVwl_esperado = numeric(),
    CVr_estimado = numeric(),
    CVwl_estimado = numeric(),
    FabCVr_eval = character(),
    FabCVwl_eval = character(),
    FabCVrUVL_eval = character(),
    FabCVwlUVL_eval = character(),
    FabCV_EP15_Cenário = character(),
    IATDMSC6 = character(),
    IATDMSC10 = character(),
    EMA20 = character(),
    Valor_alvo = character(),
    Média = numeric(),
    Viés = character(),
    Lim_inf = numeric(),
    Lim_sup = numeric(),
    Bias_eval = character(),
    stringsAsFactors = FALSE
  )
}

# ---- atualizar_WR_resultados ----
atualizar_WR_resultados <- function(R_Westgard, dados_simulados, resultados, simulacao) {
  
  # Obter lista de amostras únicas no conjunto de dados
  # amostras <- unique(dados_simulados$Amostra[dados_simulados$Tipo == "QC"])
  amostras <- unique(dados_simulados$Amostra)
  
  # Criar vetor para armazenar avaliacao CQI por amostra
  CQI_status <- character(length(amostras))
  
  for (i in seq_along(amostras)) {
    amostra <- amostras[i]
    
    # Identificar os dias em que essa amostra tem medicões QC
    dias_amostra <- unique(dados_simulados$Dia[dados_simulados$Amostra == amostra & dados_simulados$Tipo == "QC"])
    
    # Avaliacao desses dias na tabela R_Westgard
    avaliacoes <- R_Westgard$Avaliacao[R_Westgard$Dia %in% dias_amostra]
    
    # Se algum dos dias foi rejeitado, marca como "Rejected"
    if (any(avaliacoes == "Rejected")) {
      CQI_status[i] <- "Rejected"
    } else {
      CQI_status[i] <- "Accepted"
    }
  }
  
  # Criar linha de resultados para esta simulacao
  resultados_simulacao <- data.frame(
    Simulacao = simulacao,
    Amostra = amostras,
    CQI_eval = CQI_status,
    Outliers = NA,
    CVr_esperado = NA,
    CVwl_esperado = NA,
    CVr_estimado = NA,
    CVwl_estimado = NA,
    FabCVr_eval = NA,
    FabCVwl_eval = NA,
    FabCVrUVL_eval = NA,
    FabCVwlUVL_eval = NA,
    FabCV_EP15_Cenário = NA,
    IATDMSC6 = NA,
    IATDMSC10 = NA,
    EMA20 = NA,
    Valor_alvo = NA,
    Média = NA,
    Viés = NA,
    Lim_inf = NA,
    Lim_sup = NA,
    Bias_eval = NA,
    stringsAsFactors = FALSE
  )
  
  # Juntar ao data frame de resultados
  resultados <- rbind(resultados, resultados_simulacao)
  rownames(resultados) <- NULL  # Limpar rownames
  
  return(resultados)
}



# ---- funcao atualizar Outliers no df resultados ----
atualizar_outliers <- function(df_g_resultado, resultados, i) {
  for (j in seq_len(nrow(df_g_resultado))) {
    amostra_atual <- df_g_resultado$Amostra[j]
    n_outliers <- df_g_resultado$Outliers[j]
    
    # Atualiza a linha correspondente à simulacao e à amostra
    linha_resultado <- which(resultados$Simulacao == i & resultados$Amostra == amostra_atual)
    
    if (length(linha_resultado) == 1) {
      resultados$Outliers[linha_resultado] <- n_outliers
    } else {
      cat("⚠️ Linha nao encontrada ou múltiplas correspondências para simulacao", i, "e amostra", amostra_atual, "\n")
    }
  }
  
  return(resultados)
}

# ---- Atualizar cv resultados ----
atualizar_cvs_resultados <- function(resultados, resultado_cv_amostras, simulacao) {
  for (i in seq_len(nrow(resultado_cv_amostras))) {
    amostra <- resultado_cv_amostras$Amostra[i]
    cv_r <- resultado_cv_amostras$CVr_esperado[i] #commented
    cv_wl <- resultado_cv_amostras$CVwl_esperado[i] #comemnted
    cv_r <- round(cv_r, digits = 3)
    cv_wl <- round(cv_wl, digits = 3)
    
    linha_idx <- which(resultados$Simulacao == simulacao & resultados$Amostra == amostra)
    
    if (length(linha_idx) == 1) {
      resultados$CVr_esperado[linha_idx] <- cv_r
      resultados$CVwl_esperado[linha_idx] <- cv_wl
    }
  }
  return(resultados)
}

# ---- Atualizar limites internacionais resultados ----
avaliar_limites_internacionais <- function(i, resultados, DFwl) {
  # Filtrar linhas da simulacao i
  idx_simulacao <- resultados$Simulacao == i
  
  for (j in which(idx_simulacao)) {
    amostra <- resultados$Amostra[j]
    CVwl_est <- resultados$CVwl_estimado[j]
    
    # Obter UVLwl da amostra
    UVL_wl <- DFwl$UVLwl[DFwl$Amostra == amostra]
    
    # Verificacao com base nos limites internacionais multiplicados pelo UVL
    limite_6 <- 0.06 * UVL_wl
    limite_10 <- 0.10 * UVL_wl
    limite_20 <- 0.20 * UVL_wl
    
    resultados$IATDMSC6[j] <- ifelse(CVwl_est > limite_6, "Fail", "Pass")
    resultados$IATDMSC10[j] <- ifelse(CVwl_est > limite_10, "Fail", "Pass")
    resultados$EMA20[j] <- ifelse(CVwl_est > limite_20, "Fail", "Pass")
  }
  
  return(resultados)
}

# ---- avaliar_resultados ----
avaliar_resultados <- function(resultados, avaliacao) {
  # Transformar valores
  valores <- as.character(resultados[[avaliacao]])
  
  # Reclassificar "Outlier" como "Nao avaliado"
  valores[valores == "Outlier"] <- "Nao avaliado"
  valores[is.na(valores)] <- "Nao avaliado"
  
  # Contar categorias
  freq <- table(valores)
  
  # Total
  total <- sum(freq)
  
  # Percentagens
  percentagem <- round((freq / total) * 100, 2)
  
  # Data frame
  df_resumo <- data.frame(
    Avaliacao = names(freq),
    Percentagem = as.numeric(percentagem)
  )
  
  # Garantir todas as categorias aparecem
  todas_categorias <- data.frame(Avaliacao = c("Pass", "Barely", "Fail", "Nao avaliado"))
  df_resumo <- merge(todas_categorias, df_resumo, by = "Avaliacao", all.x = TRUE)
  df_resumo$Percentagem[is.na(df_resumo$Percentagem)] <- 0
  
  return(df_resumo)
}

# ---- Vetor com nomes das colunas ----
colunas <- c("FabCVr_eval", "FabCVwl_eval", "FabCVrUVL_eval", "FabCVwlUVL_eval",
             "IATDMSC6", "IATDMSC10", "EMA20", "Bias_eval")

# ---- todas_categorias ----
todas_categorias <- c(
  "Bias_eval - Fail", "Bias_eval - Outlier", "Bias_eval - Pass",
  "CQI_eval - Accepted", "CQI_eval - Rejected",
  "EMA20 - Fail", "EMA20 - Pass",
  "FabCV_EP15_Cenário - C1A", "FabCV_EP15_Cenário - C1B", "FabCV_EP15_Cenário - C2", "FabCV_EP15_Cenário - C3",
  "FabCVr_eval - Fail", "FabCVr_eval - Pass",
  "FabCVrUVL_eval - Fail", "FabCVrUVL_eval - Barely", "FabCVrUVL_eval - Pass",
  "FabCVwl_eval - Fail", "FabCVwl_eval - Pass",
  "FabCVwlUVL_eval - Fail", "FabCVwlUVL_eval - Barely", "FabCVwlUVL_eval - Pass",
  "IATDMSC10 - Fail", "IATDMSC10 - Pass",
  "IATDMSC6 - Fail", "IATDMSC6 - Pass"
)

# ---- avaliar_guia ----
avaliar_guia <- function(resultados) {
  # Lista de colunas de avaliacao
  colunas_avaliacao <- c(
    "CQI_eval", "FabCVr_eval", "FabCVwl_eval", "FabCVrUVL_eval", "FabCVwlUVL_eval",
    "FabCV_EP15_Cenário", "IATDMSC6", "IATDMSC10", "EMA20", "Bias_eval"
  )
  
  lista_freqs <- list()
  
  for (col in colunas_avaliacao) {
    # Definir todas as categorias esperadas
    todas_categorias <- unique(resultados[[col]])
    
    # Adicionar manualmente categorias esperadas para casos fixos
    if (col == "CQI_eval") {
      todas_categorias <- c("Accepted", "Rejected")
    }
    if (col == "Bias_eval") {
      todas_categorias <- c("Pass", "Outlier", "Fail")
    }
    
    # Frequência
    freq <- table(factor(resultados[[col]], levels = todas_categorias))
    
    # Percentagem
    percentagem <- (freq / sum(freq)) * 100
    
    # Data frame
    df_tmp <- data.frame(
      Avaliacao = paste(col, "-", names(freq)),
      Frequencia = as.numeric(freq),
      Percentagem = as.numeric(percentagem),
      stringsAsFactors = FALSE
    )
    
    lista_freqs[[col]] <- df_tmp
  }
  
  # Concatenar
  df_resumo <- do.call(rbind, lista_freqs)
  return(df_resumo)
}

# ---- adicionar_sens_espec_CVwl_wl ----
adicionar_sens_espec_CVwl <- function(df_guia_master) {
  col_neg <- "IWL_0_Valor"
  #colunas_pos <- setdiff(names(df_guia_master)[startsWith(names(df_guia_master), "IWL_")], col_neg)
  
  # Inclui também a coluna negativa agora
  colunas_todas <- names(df_guia_master)[startsWith(names(df_guia_master), "IWL_")]
  colunas_pos <- colunas_todas
  
  limiares <- c("FabCVwl_eval", "FabCVwlUVL_eval", "IATDMSC6", "IATDMSC10", "EMA20")
  
  for (limiar in limiares) {
    TN <- df_guia_master[df_guia_master$Avaliacao %in% paste0(limiar, " - Pass (%)"), col_neg]
    TN <- c(TN, df_guia_master[df_guia_master$Avaliacao %in% paste0(limiar, " - Barely (%)"), col_neg])
    FP <- df_guia_master[df_guia_master$Avaliacao %in% paste0(limiar, " - Fail (%)"), col_neg]
    espec <- (sum(TN)) / (sum(TN) + sum(FP))
    
    sensibilidades <- numeric(length(colunas_pos))
    names(sensibilidades) <- colunas_pos
    for (i in seq_along(colunas_pos)) {
      col <- colunas_pos[i]
      TP <- df_guia_master[df_guia_master$Avaliacao %in% paste0(limiar, " - Fail (%)"), col]
      FN <- df_guia_master[df_guia_master$Avaliacao %in% c(paste0(limiar, " - Pass (%)"), paste0(limiar, " - Barely (%)")), col]
      sensibilidades[i] <- (sum(TP)) / (sum(TP) + sum(FN))
    }
    
    df_espec <- data.frame(
      Avaliacao = paste0("Especificidade (", limiar, ") (%)"),
      t(setNames(round(espec * 100, 2), col_neg)),
      stringsAsFactors = FALSE
    )
    df_sens <- data.frame(
      Avaliacao = paste0("Sensibilidade (", limiar, ") (%)"),
      t(round(sensibilidades * 100, 2)),
      stringsAsFactors = FALSE
    )
    
    faltam_cols <- setdiff(names(df_guia_master), names(df_sens))
    if (length(faltam_cols) > 0) for (col in faltam_cols) df_sens[[col]] <- NA
    faltam_cols <- setdiff(names(df_guia_master), names(df_espec))
    if (length(faltam_cols) > 0) for (col in faltam_cols) df_espec[[col]] <- NA
    
    df_sens <- df_sens[, names(df_guia_master)]
    df_espec <- df_espec[, names(df_guia_master)]
    
    df_guia_master <- rbind(df_guia_master, df_espec, df_sens)
  }
  
  return(df_guia_master)
}

# ---- adicionar_sens_espec_CQI_wl ----
adicionar_sens_espec_CQI <- function(df_guia_master) {
  col_neg <- "IWL_0_Valor"
  #colunas_pos <- setdiff(names(df_guia_master)[startsWith(names(df_guia_master), "IWL_")], col_neg)
  
  colunas_todas <- names(df_guia_master)[startsWith(names(df_guia_master), "IWL_")]
  colunas_pos <- colunas_todas
  
  TN <- df_guia_master[df_guia_master$Avaliacao == "CQI_eval - Accepted (%)", col_neg]
  FP <- df_guia_master[df_guia_master$Avaliacao == "CQI_eval - Rejected (%)", col_neg]
  espec_cqi <- (sum(TN)) / (sum(TN) + sum(FP))
  
  sens_cqi <- numeric(length(colunas_pos))
  names(sens_cqi) <- colunas_pos
  for (i in seq_along(colunas_pos)) {
    col <- colunas_pos[i]
    TP <- df_guia_master[df_guia_master$Avaliacao == "CQI_eval - Rejected (%)", col]
    FN <- df_guia_master[df_guia_master$Avaliacao == "CQI_eval - Accepted (%)", col]
    sens_cqi[i] <- ifelse((sum(TP) + sum(FN)) > 0, sum(TP) / (sum(TP) + sum(FN)), NA)
  }
  
  df_espec_cqi <- data.frame(
    Avaliacao = "Especificidade (CQI_eval) (%)",
    t(setNames(round(espec_cqi * 100, 2), col_neg)),
    stringsAsFactors = FALSE
  )
  df_sens_cqi <- data.frame(
    Avaliacao = "Sensibilidade (CQI_eval) (%)",
    t(round(sens_cqi * 100, 2)),
    stringsAsFactors = FALSE
  )
  
  faltam_cols <- setdiff(names(df_guia_master), names(df_sens_cqi))
  if (length(faltam_cols) > 0) for (col in faltam_cols) df_sens_cqi[[col]] <- NA
  faltam_cols <- setdiff(names(df_guia_master), names(df_espec_cqi))
  if (length(faltam_cols) > 0) for (col in faltam_cols) df_espec_cqi[[col]] <- NA
  
  df_sens_cqi <- df_sens_cqi[, names(df_guia_master)]
  df_espec_cqi <- df_espec_cqi[, names(df_guia_master)]
  
  df_guia_master <- rbind(df_guia_master, df_espec_cqi, df_sens_cqi)
  
  return(df_guia_master)
}

# ---- adicionar_sens_espec_CVr_wl ----
adicionar_sens_espec_CVr <- function(df_guia_master) {
  col_neg <- "IWL_0_Valor"
  colunas_pos <- setdiff(names(df_guia_master)[startsWith(names(df_guia_master), "IWL_")], col_neg)
  limiares <- c("FabCVr_eval", "FabCVrUVL_eval")
  
  for (limiar in limiares) {
    TN <- df_guia_master[df_guia_master$Avaliacao %in% paste0(limiar, " - Pass (%)"), col_neg]
    TN <- c(TN, df_guia_master[df_guia_master$Avaliacao %in% paste0(limiar, " - Barely (%)"), col_neg])
    FP <- df_guia_master[df_guia_master$Avaliacao %in% paste0(limiar, " - Fail (%)"), col_neg]
    espec <- (sum(TN)) / (sum(TN) + sum(FP))
    
    sensibilidades <- numeric(length(colunas_pos))
    names(sensibilidades) <- colunas_pos
    for (i in seq_along(colunas_pos)) {
      col <- colunas_pos[i]
      TP <- df_guia_master[df_guia_master$Avaliacao %in% paste0(limiar, " - Fail (%)"), col]
      FN <- df_guia_master[df_guia_master$Avaliacao %in% c(paste0(limiar, " - Pass (%)"), paste0(limiar, " - Barely (%)")), col]
      sensibilidades[i] <- (sum(TP)) / (sum(TP) + sum(FN))
    }
    
    df_espec <- data.frame(
      Avaliacao = paste0("Especificidade (", limiar, ") (%)"),
      t(setNames(round(espec * 100, 2), col_neg)),
      stringsAsFactors = FALSE
    )
    df_sens <- data.frame(
      Avaliacao = paste0("Sensibilidade (", limiar, ") (%)"),
      t(round(sensibilidades * 100, 2)),
      stringsAsFactors = FALSE
    )
    
    faltam_cols <- setdiff(names(df_guia_master), names(df_sens))
    if (length(faltam_cols) > 0) for (col in faltam_cols) df_sens[[col]] <- NA
    faltam_cols <- setdiff(names(df_guia_master), names(df_espec))
    if (length(faltam_cols) > 0) for (col in faltam_cols) df_espec[[col]] <- NA
    
    df_sens <- df_sens[, names(df_guia_master)]
    df_espec <- df_espec[, names(df_guia_master)]
    
    df_guia_master <- rbind(df_guia_master, df_espec, df_sens)
  }
  
  return(df_guia_master)
}

# ---- adicionar_sens_espec_bias_wl ----
adicionar_sens_espec_bias <- function(df_guia_master) {
  col_neg <- "IWL_0_Valor"
  colunas_pos <- setdiff(names(df_guia_master)[startsWith(names(df_guia_master), "IWL_")], col_neg)
  limiar <- "Bias_eval"
  
  TN <- df_guia_master[df_guia_master$Avaliacao %in% paste0(limiar, " - Pass (%)"), col_neg]
  FP <- df_guia_master[df_guia_master$Avaliacao %in% paste0(limiar, " - Fail (%)"), col_neg]
  espec <- (sum(TN)) / (sum(TN) + sum(FP))
  
  sensibilidades <- numeric(length(colunas_pos))
  names(sensibilidades) <- colunas_pos
  for (i in seq_along(colunas_pos)) {
    col <- colunas_pos[i]
    TP <- df_guia_master[df_guia_master$Avaliacao %in% paste0(limiar, " - Fail (%)"), col]
    FN <- df_guia_master[df_guia_master$Avaliacao %in% paste0(limiar, " - Pass (%)"), col]
    sensibilidades[i] <- (sum(TP)) / (sum(TP) + sum(FN))
  }
  
  df_espec <- data.frame(
    Avaliacao = paste0("Especificidade (", limiar, ") (%)"),
    t(setNames(round(espec * 100, 2), col_neg)),
    stringsAsFactors = FALSE
  )
  df_sens <- data.frame(
    Avaliacao = paste0("Sensibilidade (", limiar, ") (%)"),
    t(round(sensibilidades * 100, 2)),
    stringsAsFactors = FALSE
  )
  
  faltam_cols <- setdiff(names(df_guia_master), names(df_sens))
  if (length(faltam_cols) > 0) for (col in faltam_cols) df_sens[[col]] <- NA
  faltam_cols <- setdiff(names(df_guia_master), names(df_espec))
  if (length(faltam_cols) > 0) for (col in faltam_cols) df_espec[[col]] <- NA
  
  df_sens <- df_sens[, names(df_guia_master)]
  df_espec <- df_espec[, names(df_guia_master)]
  
  df_guia_master <- rbind(df_guia_master, df_espec, df_sens)
  
  return(df_guia_master)
}

# ---- adicionar_sens_espec_CVwl_r ----
adicionar_sens_espec_CVwl_r <- function(df_guia_master) {
  col_neg <- "Ir_0_Valor"
  #colunas_pos <- setdiff(names(df_guia_master)[startsWith(names(df_guia_master), "Ir_")], col_neg)
  
  # Inclui também a coluna negativa agora
  colunas_todas <- names(df_guia_master)[startsWith(names(df_guia_master), "Ir_")]
  colunas_pos <- colunas_todas
  
  limiares <- c("FabCVwl_eval", "FabCVwlUVL_eval", "IATDMSC6", "IATDMSC10", "EMA20")
  
  for (limiar in limiares) {
    TN <- df_guia_master[df_guia_master$Avaliacao %in% paste0(limiar, " - Pass (%)"), col_neg]
    TN <- c(TN, df_guia_master[df_guia_master$Avaliacao %in% paste0(limiar, " - Barely (%)"), col_neg])
    FP <- df_guia_master[df_guia_master$Avaliacao %in% paste0(limiar, " - Fail (%)"), col_neg]
    espec <- (sum(TN)) / (sum(TN) + sum(FP))
    
    sensibilidades <- numeric(length(colunas_pos))
    names(sensibilidades) <- colunas_pos
    for (i in seq_along(colunas_pos)) {
      col <- colunas_pos[i]
      TP <- df_guia_master[df_guia_master$Avaliacao %in% paste0(limiar, " - Fail (%)"), col]
      FN <- df_guia_master[df_guia_master$Avaliacao %in% c(paste0(limiar, " - Pass (%)"), paste0(limiar, " - Barely (%)")), col]
      sensibilidades[i] <- (sum(TP)) / (sum(TP) + sum(FN))
    }
    
    df_espec <- data.frame(
      Avaliacao = paste0("Especificidade (", limiar, ") (%)"),
      t(setNames(round(espec * 100, 2), col_neg)),
      stringsAsFactors = FALSE
    )
    df_sens <- data.frame(
      Avaliacao = paste0("Sensibilidade (", limiar, ") (%)"),
      t(round(sensibilidades * 100, 2)),
      stringsAsFactors = FALSE
    )
    
    faltam_cols <- setdiff(names(df_guia_master), names(df_sens))
    if (length(faltam_cols) > 0) for (col in faltam_cols) df_sens[[col]] <- NA
    faltam_cols <- setdiff(names(df_guia_master), names(df_espec))
    if (length(faltam_cols) > 0) for (col in faltam_cols) df_espec[[col]] <- NA
    
    df_sens <- df_sens[, names(df_guia_master)]
    df_espec <- df_espec[, names(df_guia_master)]
    
    df_guia_master <- rbind(df_guia_master, df_espec, df_sens)
  }
  
  return(df_guia_master)
}

# ---- adicionar_sens_espec_CQI_r ----
adicionar_sens_espec_CQI_r <- function(df_guia_master) {
  col_neg <- "Ir_0_Valor"
  #colunas_pos <- setdiff(names(df_guia_master)[startsWith(names(df_guia_master), "Ir_")], col_neg)
  
  colunas_todas <- names(df_guia_master)[startsWith(names(df_guia_master), "Ir_")]
  colunas_pos <- colunas_todas
  
  TN <- df_guia_master[df_guia_master$Avaliacao == "CQI_eval - Accepted (%)", col_neg]
  FP <- df_guia_master[df_guia_master$Avaliacao == "CQI_eval - Rejected (%)", col_neg]
  espec_cqi <- (sum(TN)) / (sum(TN) + sum(FP))
  
  sens_cqi <- numeric(length(colunas_pos))
  names(sens_cqi) <- colunas_pos
  for (i in seq_along(colunas_pos)) {
    col <- colunas_pos[i]
    TP <- df_guia_master[df_guia_master$Avaliacao == "CQI_eval - Rejected (%)", col]
    FN <- df_guia_master[df_guia_master$Avaliacao == "CQI_eval - Accepted (%)", col]
    sens_cqi[i] <- ifelse((sum(TP) + sum(FN)) > 0, sum(TP) / (sum(TP) + sum(FN)), NA)
  }
  
  df_espec_cqi <- data.frame(
    Avaliacao = "Especificidade (CQI_eval) (%)",
    t(setNames(round(espec_cqi * 100, 2), col_neg)),
    stringsAsFactors = FALSE
  )
  df_sens_cqi <- data.frame(
    Avaliacao = "Sensibilidade (CQI_eval) (%)",
    t(round(sens_cqi * 100, 2)),
    stringsAsFactors = FALSE
  )
  
  faltam_cols <- setdiff(names(df_guia_master), names(df_sens_cqi))
  if (length(faltam_cols) > 0) for (col in faltam_cols) df_sens_cqi[[col]] <- NA
  faltam_cols <- setdiff(names(df_guia_master), names(df_espec_cqi))
  if (length(faltam_cols) > 0) for (col in faltam_cols) df_espec_cqi[[col]] <- NA
  
  df_sens_cqi <- df_sens_cqi[, names(df_guia_master)]
  df_espec_cqi <- df_espec_cqi[, names(df_guia_master)]
  
  df_guia_master <- rbind(df_guia_master, df_espec_cqi, df_sens_cqi)
  
  return(df_guia_master)
}
# ---- adicionar_sens_espec_CVr_r ----
adicionar_sens_espec_CVr_r <- function(df_guia_master) {
  col_neg <- "Ir_0_Valor"
  # colunas_pos <- setdiff(names(df_guia_master)[startsWith(names(df_guia_master), "Ir_")], col_neg)
  colunas_todas <- names(df_guia_master)[startsWith(names(df_guia_master), "Ir_")]
  colunas_pos <- colunas_todas
  
  limiares <- c("FabCVr_eval", "FabCVrUVL_eval")
  
  for (limiar in limiares) {
    TN <- df_guia_master[df_guia_master$Avaliacao %in% paste0(limiar, " - Pass (%)"), col_neg]
    TN <- c(TN, df_guia_master[df_guia_master$Avaliacao %in% paste0(limiar, " - Barely (%)"), col_neg])
    FP <- df_guia_master[df_guia_master$Avaliacao %in% paste0(limiar, " - Fail (%)"), col_neg]
    espec <- (sum(TN)) / (sum(TN) + sum(FP))
    
    sensibilidades <- numeric(length(colunas_pos))
    names(sensibilidades) <- colunas_pos
    for (i in seq_along(colunas_pos)) {
      col <- colunas_pos[i]
      TP <- df_guia_master[df_guia_master$Avaliacao %in% paste0(limiar, " - Fail (%)"), col]
      FN <- df_guia_master[df_guia_master$Avaliacao %in% c(paste0(limiar, " - Pass (%)"), paste0(limiar, " - Barely (%)")), col]
      sensibilidades[i] <- (sum(TP)) / (sum(TP) + sum(FN))
    }
    
    df_espec <- data.frame(
      Avaliacao = paste0("Especificidade (", limiar, ") (%)"),
      t(setNames(round(espec * 100, 2), col_neg)),
      stringsAsFactors = FALSE
    )
    df_sens <- data.frame(
      Avaliacao = paste0("Sensibilidade (", limiar, ") (%)"),
      t(round(sensibilidades * 100, 2)),
      stringsAsFactors = FALSE
    )
    
    faltam_cols <- setdiff(names(df_guia_master), names(df_sens))
    if (length(faltam_cols) > 0) for (col in faltam_cols) df_sens[[col]] <- NA
    faltam_cols <- setdiff(names(df_guia_master), names(df_espec))
    if (length(faltam_cols) > 0) for (col in faltam_cols) df_espec[[col]] <- NA
    
    df_sens <- df_sens[, names(df_guia_master)]
    df_espec <- df_espec[, names(df_guia_master)]
    
    df_guia_master <- rbind(df_guia_master, df_espec, df_sens)
  }
  
  return(df_guia_master)
}
# ---- adicionar_sens_espec_bias_r ----
adicionar_sens_espec_bias_r <- function(df_guia_master) {
  col_neg <- "Ir_0_Valor"
  colunas_pos <- setdiff(names(df_guia_master)[startsWith(names(df_guia_master), "Ir_")], col_neg)
  limiar <- "Bias_eval"
  
  TN <- df_guia_master[df_guia_master$Avaliacao %in% paste0(limiar, " - Pass (%)"), col_neg]
  FP <- df_guia_master[df_guia_master$Avaliacao %in% paste0(limiar, " - Fail (%)"), col_neg]
  espec <- (sum(TN)) / (sum(TN) + sum(FP))
  
  sensibilidades <- numeric(length(colunas_pos))
  names(sensibilidades) <- colunas_pos
  for (i in seq_along(colunas_pos)) {
    col <- colunas_pos[i]
    TP <- df_guia_master[df_guia_master$Avaliacao %in% paste0(limiar, " - Fail (%)"), col]
    FN <- df_guia_master[df_guia_master$Avaliacao %in% paste0(limiar, " - Pass (%)"), col]
    sensibilidades[i] <- (sum(TP)) / (sum(TP) + sum(FN))
  }
  
  df_espec <- data.frame(
    Avaliacao = paste0("Especificidade (", limiar, ") (%)"),
    t(setNames(round(espec * 100, 2), col_neg)),
    stringsAsFactors = FALSE
  )
  df_sens <- data.frame(
    Avaliacao = paste0("Sensibilidade (", limiar, ") (%)"),
    t(round(sensibilidades * 100, 2)),
    stringsAsFactors = FALSE
  )
  
  faltam_cols <- setdiff(names(df_guia_master), names(df_sens))
  if (length(faltam_cols) > 0) for (col in faltam_cols) df_sens[[col]] <- NA
  faltam_cols <- setdiff(names(df_guia_master), names(df_espec))
  if (length(faltam_cols) > 0) for (col in faltam_cols) df_espec[[col]] <- NA
  
  df_sens <- df_sens[, names(df_guia_master)]
  df_espec <- df_espec[, names(df_guia_master)]
  
  df_guia_master <- rbind(df_guia_master, df_espec, df_sens)
  
  return(df_guia_master)
}

# ---- adicionar_sens_espec_CVwl_ba ----
adicionar_sens_espec_CVwl_ba <- function(df_guia_master) {
  col_neg <- "viesa_0_Valor"
  #colunas_pos <- setdiff(names(df_guia_master)[startsWith(names(df_guia_master), "viesa_")], col_neg)
  
  # Inclui também a coluna negativa agora
  colunas_todas <- names(df_guia_master)[startsWith(names(df_guia_master), "viesa_")]
  colunas_pos <- colunas_todas
  
  limiares <- c("FabCVwl_eval", "FabCVwlUVL_eval", "IATDMSC6", "IATDMSC10", "EMA20")
  
  for (limiar in limiares) {
    TN <- df_guia_master[df_guia_master$Avaliacao %in% paste0(limiar, " - Pass (%)"), col_neg]
    TN <- c(TN, df_guia_master[df_guia_master$Avaliacao %in% paste0(limiar, " - Barely (%)"), col_neg])
    FP <- df_guia_master[df_guia_master$Avaliacao %in% paste0(limiar, " - Fail (%)"), col_neg]
    espec <- (sum(TN)) / (sum(TN) + sum(FP))
    
    sensibilidades <- numeric(length(colunas_pos))
    names(sensibilidades) <- colunas_pos
    for (i in seq_along(colunas_pos)) {
      col <- colunas_pos[i]
      TP <- df_guia_master[df_guia_master$Avaliacao %in% paste0(limiar, " - Fail (%)"), col]
      FN <- df_guia_master[df_guia_master$Avaliacao %in% c(paste0(limiar, " - Pass (%)"), paste0(limiar, " - Barely (%)")), col]
      sensibilidades[i] <- (sum(TP)) / (sum(TP) + sum(FN))
    }
    
    df_espec <- data.frame(
      Avaliacao = paste0("Especificidade (", limiar, ") (%)"),
      t(setNames(round(espec * 100, 2), col_neg)),
      stringsAsFactors = FALSE
    )
    df_sens <- data.frame(
      Avaliacao = paste0("Sensibilidade (", limiar, ") (%)"),
      t(round(sensibilidades * 100, 2)),
      stringsAsFactors = FALSE
    )
    
    faltam_cols <- setdiff(names(df_guia_master), names(df_sens))
    if (length(faltam_cols) > 0) for (col in faltam_cols) df_sens[[col]] <- NA
    faltam_cols <- setdiff(names(df_guia_master), names(df_espec))
    if (length(faltam_cols) > 0) for (col in faltam_cols) df_espec[[col]] <- NA
    
    df_sens <- df_sens[, names(df_guia_master)]
    df_espec <- df_espec[, names(df_guia_master)]
    
    df_guia_master <- rbind(df_guia_master, df_espec, df_sens)
  }
  
  return(df_guia_master)
}

# ---- adicionar_sens_espec_CQI_ba ----
adicionar_sens_espec_CQI_ba <- function(df_guia_master) {
  col_neg <- "viesa_0_Valor"
  #colunas_pos <- setdiff(names(df_guia_master)[startsWith(names(df_guia_master), "viesa_")], col_neg)
  
  colunas_todas <- names(df_guia_master)[startsWith(names(df_guia_master), "viesa_")]
  colunas_pos <- colunas_todas
  
  TN <- df_guia_master[df_guia_master$Avaliacao == "CQI_eval - Accepted (%)", col_neg]
  FP <- df_guia_master[df_guia_master$Avaliacao == "CQI_eval - Rejected (%)", col_neg]
  espec_cqi <- (sum(TN)) / (sum(TN) + sum(FP))
  
  sens_cqi <- numeric(length(colunas_pos))
  names(sens_cqi) <- colunas_pos
  for (i in seq_along(colunas_pos)) {
    col <- colunas_pos[i]
    TP <- df_guia_master[df_guia_master$Avaliacao == "CQI_eval - Rejected (%)", col]
    FN <- df_guia_master[df_guia_master$Avaliacao == "CQI_eval - Accepted (%)", col]
    sens_cqi[i] <- ifelse((sum(TP) + sum(FN)) > 0, sum(TP) / (sum(TP) + sum(FN)), NA)
  }
  
  df_espec_cqi <- data.frame(
    Avaliacao = "Especificidade (CQI_eval) (%)",
    t(setNames(round(espec_cqi * 100, 2), col_neg)),
    stringsAsFactors = FALSE
  )
  df_sens_cqi <- data.frame(
    Avaliacao = "Sensibilidade (CQI_eval) (%)",
    t(round(sens_cqi * 100, 2)),
    stringsAsFactors = FALSE
  )
  
  faltam_cols <- setdiff(names(df_guia_master), names(df_sens_cqi))
  if (length(faltam_cols) > 0) for (col in faltam_cols) df_sens_cqi[[col]] <- NA
  faltam_cols <- setdiff(names(df_guia_master), names(df_espec_cqi))
  if (length(faltam_cols) > 0) for (col in faltam_cols) df_espec_cqi[[col]] <- NA
  
  df_sens_cqi <- df_sens_cqi[, names(df_guia_master)]
  df_espec_cqi <- df_espec_cqi[, names(df_guia_master)]
  
  df_guia_master <- rbind(df_guia_master, df_espec_cqi, df_sens_cqi)
  
  return(df_guia_master)
}
# ---- adicionar_sens_espec_CVr_ba ----

adicionar_sens_espec_CVr_ba <- function(df_guia_master) {
  col_neg <- "viesa_0_Valor"
  # colunas_pos <- setdiff(names(df_guia_master)[startsWith(names(df_guia_master), "viesa_")], col_neg)
  colunas_todas <- names(df_guia_master)[startsWith(names(df_guia_master), "viesa_")]
  colunas_pos <- colunas_todas
  
  limiares <- c("FabCVr_eval", "FabCVrUVL_eval")
  
  for (limiar in limiares) {
    TN <- df_guia_master[df_guia_master$Avaliacao %in% paste0(limiar, " - Pass (%)"), col_neg]
    TN <- c(TN, df_guia_master[df_guia_master$Avaliacao %in% paste0(limiar, " - Barely (%)"), col_neg])
    FP <- df_guia_master[df_guia_master$Avaliacao %in% paste0(limiar, " - Fail (%)"), col_neg]
    espec <- (sum(TN)) / (sum(TN) + sum(FP))
    
    sensibilidades <- numeric(length(colunas_pos))
    names(sensibilidades) <- colunas_pos
    for (i in seq_along(colunas_pos)) {
      col <- colunas_pos[i]
      TP <- df_guia_master[df_guia_master$Avaliacao %in% paste0(limiar, " - Fail (%)"), col]
      FN <- df_guia_master[df_guia_master$Avaliacao %in% c(paste0(limiar, " - Pass (%)"), paste0(limiar, " - Barely (%)")), col]
      sensibilidades[i] <- (sum(TP)) / (sum(TP) + sum(FN))
    }
    
    df_espec <- data.frame(
      Avaliacao = paste0("Especificidade (", limiar, ") (%)"),
      t(setNames(round(espec * 100, 2), col_neg)),
      stringsAsFactors = FALSE
    )
    df_sens <- data.frame(
      Avaliacao = paste0("Sensibilidade (", limiar, ") (%)"),
      t(round(sensibilidades * 100, 2)),
      stringsAsFactors = FALSE
    )
    
    faltam_cols <- setdiff(names(df_guia_master), names(df_sens))
    if (length(faltam_cols) > 0) for (col in faltam_cols) df_sens[[col]] <- NA
    faltam_cols <- setdiff(names(df_guia_master), names(df_espec))
    if (length(faltam_cols) > 0) for (col in faltam_cols) df_espec[[col]] <- NA
    
    df_sens <- df_sens[, names(df_guia_master)]
    df_espec <- df_espec[, names(df_guia_master)]
    
    df_guia_master <- rbind(df_guia_master, df_espec, df_sens)
  }
  
  return(df_guia_master)
}

adicionar_sens_espec_bias_ba <- function(df_guia_master) {
  col_neg <- "viesa_0_Valor"
  # colunas_pos <- setdiff(names(df_guia_master)[startsWith(names(df_guia_master), "viesa_")], col_neg)
  
  #
  # colunas_pos <- names(df_guia_master)[
  #   startsWith(names(df_guia_master), "viesa_") & names(df_guia_master) != col_neg
  # ]
  colunas_pos <- grep("^viesa_-?[0-9]+_Valor$", names(df_guia_master), value = TRUE)
  colunas_pos <- setdiff(colunas_pos, col_neg)
  
  #
  
  limiar <- "Bias_eval"
  
  TN <- df_guia_master[df_guia_master$Avaliacao %in% paste0(limiar, " - Pass (%)"), col_neg]
  FP <- df_guia_master[df_guia_master$Avaliacao %in% paste0(limiar, " - Fail (%)"), col_neg]
  espec <- (sum(TN)) / (sum(TN) + sum(FP))
  
  sensibilidades <- numeric(length(colunas_pos))
  names(sensibilidades) <- colunas_pos
  for (i in seq_along(colunas_pos)) {
    col <- colunas_pos[i]
    TP <- df_guia_master[df_guia_master$Avaliacao %in% paste0(limiar, " - Fail (%)"), col]
    FN <- df_guia_master[df_guia_master$Avaliacao %in% paste0(limiar, " - Pass (%)"), col]
    sensibilidades[i] <- (sum(TP)) / (sum(TP) + sum(FN))
  }
  
  df_espec <- as.data.frame(matrix(NA, nrow = 1, ncol = length(names(df_guia_master))))
  names(df_espec) <- names(df_guia_master)
  df_espec$Avaliacao <- paste0("Especificidade (", limiar, ") (%)")
  df_espec[[col_neg]] <- round(espec * 100, 2)
  
  # Cria data.frame com uma linha e todas as colunas do guia, preenchidas com NA
  df_sens <- as.data.frame(matrix(NA, nrow = 1, ncol = length(names(df_guia_master))))
  names(df_sens) <- names(df_guia_master)
  df_sens$Avaliacao <- paste0("Sensibilidade (", limiar, ") (%)")
  
  # Preencher colunas com os valores calculados
  for (col in names(sensibilidades)) {
    df_sens[[col]] <- round(sensibilidades[col] * 100, 2)
  }
  
  faltam_cols <- setdiff(names(df_guia_master), names(df_sens))
  if (length(faltam_cols) > 0) for (col in faltam_cols) df_sens[[col]] <- NA
  faltam_cols <- setdiff(names(df_guia_master), names(df_espec))
  if (length(faltam_cols) > 0) for (col in faltam_cols) df_espec[[col]] <- NA
  
  df_sens <- df_sens[, names(df_guia_master)]
  df_espec <- df_espec[, names(df_guia_master)]
  
  df_guia_master <- rbind(df_guia_master, df_espec, df_sens)
  
  return(df_guia_master)
}

# ---- adicionar_sens_espec - wrapper geral ----
adicionar_sens_espec <- function(df_guia_master, opcoes = c("CQI", "CVr", "CVwl", "bias")) {
  # Acrescenta "(%)" caso ainda nao tenha sido feito
  if (!any(grepl("%", df_guia_master$Avaliacao))) {
    df_guia_master$Avaliacao <- paste0(df_guia_master$Avaliacao, " (%)")
  }
  
  # Ordem correta
  if ("CQI" %in% opcoes) {
    df_guia_master <- adicionar_sens_espec_CQI(df_guia_master)
  }
  
  if ("CVr" %in% opcoes) {
    df_guia_master <- adicionar_sens_espec_CVr(df_guia_master)
  }
  
  if ("CVwl" %in% opcoes) {
    df_guia_master <- adicionar_sens_espec_CVwl(df_guia_master)
  }
  
  if ("bias" %in% opcoes) {
    df_guia_master <- adicionar_sens_espec_bias(df_guia_master)
  }
  
  return(df_guia_master)
}

# ---- adicionar_sens_espec_r - wrapper geral ----
adicionar_sens_espec_r <- function(df_guia_master, opcoes = c("CQI", "CVr", "CVwl", "bias")) {
  # Acrescenta "(%)" caso ainda nao tenha sido feito
  if (!any(grepl("%", df_guia_master$Avaliacao))) {
    df_guia_master$Avaliacao <- paste0(df_guia_master$Avaliacao, " (%)")
  }
  
  # Ordem correta
  if ("CQI" %in% opcoes) {
    df_guia_master <- adicionar_sens_espec_CQI_r(df_guia_master)
  }
  
  if ("CVr" %in% opcoes) {
    df_guia_master <- adicionar_sens_espec_CVr_r(df_guia_master)
  }
  
  if ("CVwl" %in% opcoes) {
    df_guia_master <- adicionar_sens_espec_CVwl_r(df_guia_master)
  }
  
  if ("bias" %in% opcoes) {
    df_guia_master <- adicionar_sens_espec_bias_r(df_guia_master)
  }
  
  return(df_guia_master)
}
# ---- adicionar_sens_espec_ba - wrapper geral ----
adicionar_sens_espec_ba <- function(df_guia_master, opcoes = c("CQI", "CVr", "CVwl", "bias")) {
  # Acrescenta "(%)" caso ainda nao tenha sido feito
  if (!any(grepl("%", df_guia_master$Avaliacao))) {
    df_guia_master$Avaliacao <- paste0(df_guia_master$Avaliacao, " (%)")
  }
  
  # Ordem correta
  if ("CQI" %in% opcoes) {
    df_guia_master <- adicionar_sens_espec_CQI_ba(df_guia_master)
  }
  
  if ("CVr" %in% opcoes) {
    df_guia_master <- adicionar_sens_espec_CVr_ba(df_guia_master)
  }
  
  if ("CVwl" %in% opcoes) {
    df_guia_master <- adicionar_sens_espec_CVwl_ba(df_guia_master)
  }
  
  if ("bias" %in% opcoes) {
    df_guia_master <- adicionar_sens_espec_bias_ba(df_guia_master)
  }
  
  return(df_guia_master)
}
# ---- pivotar_juntar_amostras ----
pivotar_juntar_amostras <- function(df, estatistica, amostras) {
  library(tidyr)
  library(dplyr)
  
  lista_pivot <- list()
  
  for (amostra in amostras) {
    df_sub <- subset(df, Amostra == amostra & Estatistica == estatistica)
    
    df_wide <- pivot_wider(
      df_sub,
      id_cols = Metrica,
      names_from = IWL,
      values_from = c(Valor, MCSE),
      names_glue = "IWL_{IWL}_{.value}"
    )
    
    lista_pivot[[amostra]] <- df_wide
  }
  
  # Usar Reduce para juntar todos
  df_final <- Reduce(function(x, y) merge(x, y, by = "Metrica", all = TRUE), lista_pivot)
  
  return(df_final)
}
# ---- pivotar_juntar_amostras_r ----
pivotar_juntar_amostras_r <- function(df, estatistica, amostras) {
  lista_pivot <- list()
  
  for (amostra in amostras) {
    df_sub <- subset(df, Amostra == amostra & Estatistica == estatistica)
    
    df_wide <- pivot_wider(
      df_sub,
      id_cols = Metrica,
      names_from = Ir,
      values_from = c(Valor, MCSE),
      names_glue = "Ir_{Ir}_{.value}"
    )
    
    lista_pivot[[amostra]] <- df_wide
  }
  
  # Usar Reduce para juntar todos
  df_final <- Reduce(function(x, y) merge(x, y, by = "Metrica", all = TRUE), lista_pivot)
  
  return(df_final)
}
# ---- pivotar_juntar_amostras_ba ----
pivotar_juntar_amostras_ba <- function(df, estatistica, amostras) {
  lista_pivot <- list()
  
  for (amostra in amostras) {
    df_sub <- subset(df, Amostra == amostra & Estatistica == estatistica)
    
    df_wide <- pivot_wider(
      df_sub,
      id_cols = Metrica,
      names_from = viesa,
      values_from = c(Valor, MCSE),
      names_glue = "viesa_{viesa}_{.value}"
    )
    
    lista_pivot[[amostra]] <- df_wide
  }
  
  # Usar Reduce para juntar todos
  df_final <- Reduce(function(x, y) merge(x, y, by = "Metrica", all = TRUE), lista_pivot)
  
  return(df_final)
}

# ---- arredondar_df_guia_master ----
arredondar_df_guia_master <- function(df, casas_decimais = 1) {
  # Identificar as colunas numéricas (exceto Avaliacao)
  cols_num <- setdiff(names(df), "Avaliacao")
  
  # Arredondar
  df[cols_num] <- lapply(df[cols_num], function(x) {
    if (is.numeric(x)) round(x, casas_decimais) else x
  })
  
  return(df)
}

# funcões para plot
criar_df_CVwl <- function(dias, performance) {
  
  # Filtrar apenas as linhas com Metrica == "CVwl (%)"
  perf_filtrado <- subset(performance, Metrica == "CVwl (%)")
  
  # Identificar as amostras únicas
  amostras <- unique(perf_filtrado$Amostra)
  
  # Inicializar lista para guardar resultados
  lista_resultados <- list()
  
  # Loop pelas amostras
  for (amostra in amostras) {
    
    # Filtrar dados dessa amostra
    dados_amostra <- subset(perf_filtrado, Amostra == amostra)
    
    # Obter valores
    media_cv <- dados_amostra$Valor[dados_amostra$Estatistica == "Média"]
    empSE_cv <- dados_amostra$Valor[dados_amostra$Estatistica == "EmpSE"]
    
    # Criar data frame para essa amostra
    df_tmp <- data.frame(
      nRun = dias,
      Amostra = amostra,
      Media_CVwl = media_cv,
      EmpSE_CVwl = empSE_cv
    )
    
    # Guardar
    lista_resultados[[amostra]] <- df_tmp
  }
  
  # Juntar todos
  df_final <- do.call(rbind, lista_resultados)
  
  return(df_final)
}

# Dados prévios
i <- 1
