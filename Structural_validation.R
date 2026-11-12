# ---- inicializar_cv_dataframe - Inicializar df para calculo da cobertura ----
inicializar_cv_dataframe <- function(amostras_df, nSim) {
  # Extrair os nomes das colunas a partir de amostras_df
  colunas <- c(
    rbind(
      amostras_df$CVr,
      amostras_df$CVwl,
      amostras_df$Media
    )
  )
  
  # Criar o data frame com NA
  df <- as.data.frame(matrix(NA, nrow = nSim, ncol = length(colunas)))
  names(df) <- colunas
  
  return(df)
}

# ---- atualizar_cv_dataframe dados para calculo da cobertura ----
atualizar_cv_dataframe <- function(cv_df, i, dados_simulados, atributos_df, amostras) {
  for (a in amostras) {
    nome_fmt <- gsub(" ", "_", a)  # <- Adiciona isto
    
    # res <- calcular_CVr_e_CVwl_direto(dados_simulados, atributos_df, amostra = a) #Fora de uso
    # res <- calcular_CVr_e_CVwl_componentes(dados_simulados, atributos_df, amostra = a)
    res <- calcular_CVr_e_CVwl_componentesV3(dados_simulados, atributos_df, targets_dia, amostra = a)
    
    nome_cv_r <- paste0("CVr_", nome_fmt)
    nome_cv_wl <- paste0("CVwl_", nome_fmt)
    
    cv_df[i, nome_cv_r] <- res["CVr"]
    cv_df[i, nome_cv_wl] <- res["CVwl"]
    
    # Média global (sem filtro do Tipo)
    media_global <- mean(dados_simulados$Valor[dados_simulados$Amostra == a], na.rm = TRUE)
    cv_df[i, paste0("Media_", nome_fmt)] <- media_global
  }
  
  return(cv_df)
}

# ---- Calcular componentes CVr e CVwl para avaliacao da Fidelidade das simulacões (V2) ----
# em relacao ao target, atribuído, e nao á média

# ----Fora de uso? ----
calcular_CVr_e_CVwl_componentes <- function(dados_simulados, atributos_df, amostra) {
  dados_a <- subset(dados_simulados, Amostra == amostra)
  target <- atributos_df$Target[atributos_df$Amostra == amostra]
  
  modelo <- aov(Valor ~ factor(Dia), data = dados_a)
  tab <- summary(modelo)[[1]]
  
  MS_entre  <- tab["factor(Dia)", "Mean Sq"]
  MS_dentro <- tab["Residuals", "Mean Sq"]
  
  ni <- as.numeric(table(dados_a$Dia))
  N  <- sum(ni)
  a  <- length(ni)
  
  n0 <- if (length(unique(ni)) == 1) {
    N / a
  } else {
    (N - sum(ni^2) / N) / (a - 1)
  }
  
  Vb  <- max(0, (MS_entre - MS_dentro) / n0)
  Vr  <- MS_dentro
  Vwl <- Vb + Vr
  
  c(
    CVr  = sqrt(Vr)  / target,
    CVwl = sqrt(Vwl) / target
  )
}

# ---- Calcular componentes CVr e CVwl para avaliacao da Fidelidade das simulacões (V3) ----
calcular_CVr_e_CVwl_componentesV3 <- function(dados_simulados, atributos_df, targets_dia, amostra) {
  # Filtrar dados da amostra
  dados_a <- subset(dados_simulados, Amostra == amostra)
  
  # Target global
  target <- atributos_df$Target[atributos_df$Amostra == amostra]
  
  if (length(target) == 0) {
    stop("A amostra indicada nao existe em atributos_df.")
  }
  
  if (!amostra %in% names(targets_dia)) {
    stop("A amostra indicada nao existe em targets_dia.")
  }
  
  # Verificar compatibilidade entre Dias e linhas de targets_dia
  if (!all(dados_a$Dia %in% seq_len(nrow(targets_dia)))) {
    stop("Os valores da coluna Dia nao sao compatíveis com as linhas de targets_dia.")
  }
  
  # Associar a cada observacao o respetivo target diário verdadeiro
  target_diario_obs <- targets_dia[dados_a$Dia, amostra]
  
  # 1) Componente intra-dia -> Vr
  # Variância residual em torno do target diário verdadeiro
  Vr <- mean((dados_a$Valor - target_diario_obs)^2, na.rm = TRUE)
  
  # 2) Componente interdiária -> Vb
  # Variabilidade dos targets diários em torno do target global
  dias_usados <- sort(unique(dados_a$Dia))
  targets_amostra <- targets_dia[dias_usados, amostra, drop = TRUE]
  
  Vb <- mean((targets_amostra - target)^2, na.rm = TRUE)
  
  # 3) Variância within-laboratory total
  Vwl <- Vr + Vb
  
  # 4) CVs
  c(
    CVr  = sqrt(Vr)  / target,
    CVwl = sqrt(Vwl) / target
  )
}

# ---- Fora de uso? ----
# ---- calcular_CVr_e_CVwl_direto ----
calcular_CVr_e_CVwl_direto <- function(dados_simulados, atributos_df, amostra) {
  dados_a <- subset(dados_simulados, Amostra == amostra)
  target <- atributos_df$Target[atributos_df$Amostra == amostra]
  
  # CV total
  #CVwl <- sd(dados_a$Valor, na.rm = TRUE) / target
  #CVwl <- sqrt(mean((dados_a$Valor - mean(dados_a$Valor))^2, na.rm = TRUE)) / target
  CVwl <- sd(dados_a$Valor, na.rm = TRUE) / target #Leva em conta Correcao de Bessel
  
  # CV intra diário
  sd_intra <- tapply(dados_a$Valor, dados_a$Dia, sd, na.rm = TRUE)
  CVr <- sqrt(mean(sd_intra^2, na.rm = TRUE)) / target
  # sd() tem em conta a correccao de Bessel
  
  return(c(CVr = CVr, CVwl = CVwl))
  #' Na tua estrutura de simulacao, CVr e CVwl sao definidos em funcao do Target.
  #' Logo, para validacao coerente das simulacões,
  #' o Target deve também ser o denominador das estimativas observadas.
}

# ---- inicializar_df_performance ----
inicializar_df_performance <- function() {
  data.frame(
    Amostra = character(),
    Estatistica = character(),
    Metrica = character(),
    Valor = numeric(),
    MCSE = numeric(),
    stringsAsFactors = FALSE
  )
}

# ---- atualizar_performance_df ----
atualizar_performance_df <- function(cover_df, amostras_df, atributos_df, performance_df) {
  # Iterar por cada amostra
  for (i in seq_len(nrow(amostras_df))) {
    nome_amostra <- amostras_df$Nome[i]
    
    # Para cada estatística
    estatisticas <- c("CVr", "CVwl", "Media")
    
    for (estat in estatisticas) {
      nome_coluna <- amostras_df[[estat]][i]
      
      # Extrair vetor de estimativas
      est <- cover_df[[nome_coluna]]
      
      # Chamar funcao auxiliar, passando atributos_df
      df_metricas <- calcular_metricas_coluna(est, nome_amostra, estat, atributos_df)
      
      # Atualizar performance_df
      performance_df <- rbind(performance_df, df_metricas)
    }
  }
  
  return(performance_df)
}


# ---- calcular_metricas_coluna ----
calcular_metricas_coluna <- function(est, nome_amostra, estatistica, atributos_df) {
  # Procurar linha correspondente
  idx <- which(atributos_df$Amostra == nome_amostra)
  
  # Obter valor verdadeiro
  if (estatistica == "Media") {
    valor_verdadeiro <- atributos_df$Target[idx]
  } else if (estatistica == "CVr") {
    valor_verdadeiro <- atributos_df$SD_repetibilidade[idx] / atributos_df$Target[idx]
    est <- est * 100
  } else if (estatistica == "CVwl") {
    valor_verdadeiro <- atributos_df$SD_Within_Laboratory[idx] / atributos_df$Target[idx]
    est <- est * 100
  } else {
    valor_verdadeiro <- NA
  }
  
  valor_verdadeiro_perc <- ifelse(estatistica == "Media", valor_verdadeiro, valor_verdadeiro * 100)
  
  # Média e mediana
  media_est <- mean(est, na.rm = TRUE)
  mediana_est <- median(est, na.rm = TRUE)
  quartis <- quantile(est, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  # Add-in
  q1 <- quartis[1]
  q3 <- quartis[3]
  min_est <- min(est)
  max_est <- max(est)
  iqr <- q3 - q1
  lim_inf <- q1 - 1.5 * iqr
  lim_sup <- q3 + 1.5 * iqr
  # Add end
  
  # Viés relativo
  vies_abs <- mean(est - valor_verdadeiro_perc, na.rm = TRUE)
  vies_rel <- vies_abs / valor_verdadeiro_perc
  MCSE_vies <- sd((est - valor_verdadeiro_perc) / valor_verdadeiro_perc, na.rm = TRUE) / sqrt(length(est))
  
  # EmpSE
  EmpSE <- sd(est, na.rm = TRUE)
  MCSE_EmpSE <- EmpSE / sqrt(2 * (length(est) - 1))
  
  # MSE
  MSE <- mean((est - valor_verdadeiro_perc)^2, na.rm = TRUE)
  MCSE_MSE <- sd((est - valor_verdadeiro_perc)^2, na.rm = TRUE) / sqrt(length(est))
  
  # Cobertura usando EmpSE
  z <- 1.96
  ICinf <- est - z * EmpSE
  ICsup <- est + z * EmpSE
  inclui <- (valor_verdadeiro_perc >= ICinf) & (valor_verdadeiro_perc <= ICsup)
  cobertura <- mean(inclui, na.rm = TRUE)
  MCSE_cobertura <- sqrt(cobertura * (1 - cobertura) / length(est))
  
  # Comprimento médio IC
  comprimento_IC <- mean(ICsup - ICinf, na.rm = TRUE)
  MCSE_comp <- sd(ICsup - ICinf, na.rm = TRUE) / sqrt(length(est))
  
  # Teste KS
  ks <- ks.test(est, "pnorm", mean = mean(est, na.rm = TRUE), sd = sd(est, na.rm = TRUE))
  ks_D <- as.numeric(ks$statistic)
  ks_p <- ks$p.value
  
  # Construir data frame
  df <- data.frame(
    # Amostra = rep(nome_amostra, 10),
    # Estatistica = rep(estatistica, 10),
    Amostra = rep(nome_amostra, 16),
    Estatistica = rep(estatistica, 16),
    # Metrica = c("Valor atribuido", "Média", "Mediana", "Viés relativo", "EmpSE", "MSE", "Cobertura", "Comprimento IC", "KS", "KS_pvalue"),
    Metrica = c("Valor atribuido", "Média", "Mediana","q1", "q3", "Mínimo", "Máximo", "Limite Inferior", "Limite Superior",  "Viés relativo", "EmpSE", "MSE", "Cobertura", "Comprimento IC", "KS", "KS_pvalue"),
    Valor = c(round(valor_verdadeiro_perc, 2),
              round(media_est, 2),
              round(mediana_est, 2),
              # Add-in
              round(q1, 2),
              round(q3, 2),
              round(min_est, 2),
              round(max_est, 2),
              round(lim_inf, 2),
              round(lim_sup, 2),
              # Add end
              round(vies_rel * 100, 2),
              round(EmpSE, 2),
              round(MSE, 2),
              round(cobertura * 100, 2),
              round(comprimento_IC, 2),
              round(ks_D, 3),
              round(ks_p, 3)),
    MCSE = c(NA,
             NA,
             NA,
             # Add-in
             NA,
             NA,
             NA,
             NA,
             NA,
             NA,
             # Add end
             round(MCSE_vies * 100, 2),
             round(MCSE_EmpSE, 2),
             round(MCSE_MSE, 2),
             round(MCSE_cobertura * 100, 2),
             round(MCSE_comp, 2),
             NA,
             NA),
    stringsAsFactors = FALSE
  )
  
  return(df)
}


