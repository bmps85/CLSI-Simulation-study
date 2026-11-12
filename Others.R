# ---- R_Westgard_NO_QC ----
R_Westgard_NO_QC <- function(dados_simulados){ 
  R_Westgard_NO_QC<- data.frame(
    Dia = sort(unique(dados_simulados$Dia)),
    Avaliacao = rep("Accepted", length(unique(dados_simulados$Dia)))
  )
  return(R_Westgard_NO_QC)
}

# ---- calcular_CVwl_multimetodos ----
calcular_CVwl_multimetodos <- function(dados_simulados, atributos_df, amostra, n_boot = 1000) {
  # Pacotes necessários
  if (!requireNamespace("lme4", quietly = TRUE)) stop("Pacote 'lme4' nao instalado.")
  if (!requireNamespace("nlme", quietly = TRUE)) stop("Pacote 'nlme' nao instalado.")
  
  # Obter target da amostra
  target <- atributos_df$Target[atributos_df$Amostra == amostra]
  
  # Filtrar os dados da amostra (exclui QC)
  dados_a <- subset(dados_simulados, Amostra == amostra & Tipo == "Amostra")
  
  # Garantir que Dia é fator
  dados_a$Dia <- as.factor(dados_a$Dia)
  
  ## --- Método 1: Modelo misto com lmer (lme4)
  modelo_lmer <- try(lmer(Valor ~ 1 + (1 | Dia), data = dados_a), silent = TRUE)
  if (inherits(modelo_lmer, "try-error")) {
    CVwl_lmer <- NA
  } else {
    var_comp <- as.data.frame(VarCorr(modelo_lmer))
    sd_total_lmer <- sqrt(sum(var_comp$vcov))
    CVwl_lmer <- sd_total_lmer / target
  }
  
  ## --- Método 2: GLS com lme (nlme)
  modelo_gls <- try(lme(Valor ~ 1, random = ~1 | Dia, data = dados_a), silent = TRUE)
  if (inherits(modelo_gls, "try-error")) {
    CVwl_gls <- NA
  } else {
    var_gls <- VarCorr(modelo_gls)[, "Variance"]
    sd_total_gls <- sqrt(sum(as.numeric(var_gls)))
    CVwl_gls <- sd_total_gls / target
  }
  
  ## --- Método 3: Bootstrap
  boot_cvs <- replicate(n_boot, {
    amostra_boot <- dados_a[sample(nrow(dados_a), replace = TRUE), ]
    sd(amostra_boot$Valor, na.rm = TRUE) / target
  })
  CVwl_bootstrap <- mean(boot_cvs, na.rm = TRUE)
  
  # Retornar resultados como lista nomeada
  return(list(
    CVwl_lmer = CVwl_lmer,
    CVwl_gls = CVwl_gls,
    CVwl_bootstrap = CVwl_bootstrap
  ))
}

# ---- avaliar_CQI ----
avaliar_CQI <- function(resultados) {
  # Contar número de amostras rejeitadas por simulacao
  resumo <- aggregate(CQI_eval ~ Simulacao, data = resultados, 
                      FUN = function(x) sum(x == "Rejected"))
  
  # Tabela de frequências
  freq <- table(resumo$CQI_eval)
  
  # Total de simulacões
  total <- sum(freq)
  
  # Percentagens
  percentagem <- (freq / total) * 100
  
  # Construir data frame final
  df_resumo <- data.frame(
    Num_rejeicões = as.numeric(names(freq)),
    Percentagem = as.numeric(percentagem)
  )
  
  # Garantir que todas as categorias (0 a 3) aparecem
  todas_categorias <- data.frame(Num_rejeicões = 0:3)
  df_resumo <- merge(todas_categorias, df_resumo, by = "Num_rejeicões", all.x = TRUE)
  df_resumo$Percentagem[is.na(df_resumo$Percentagem)] <- 0
  
  return(df_resumo)
}

# ---- Funcao para repor as variáveis mutantes ----

reset_variables <- function() {
  Ir    <<- 0  # Imprecisao de repetibilidade
  Iwl   <<- 0  # Imprecisao total
  viesr <<- 0  # Viés relativo
  viesa <<- 0  # Viés absoluto
}

# ---- Westgard_eval - Avaliacao CQI  ----
# Output:
# 1 – Retorna apenas os controlos com ZScore e regras aplicadas
# 2 – Retorna todos os dados simulados com coluna "eval" preenchida para os controlos
# 3 – Retorna avaliacao por dia: "ACEITE" ou "REJEITADA" com base nas regras exceto "1_2SD"

Westgard_eval <- function(dados_simulados, controlos_info, Output = 1) {
  # Filtrar apenas os controlos
  dados_controlos <- dados_simulados[dados_simulados$Tipo == "QC", ]
  
  # Juntar as informacões de Target e SD
  dados_controlos <- merge(dados_controlos, controlos_info,
                           by.x = "Amostra", by.y = "nome", all.x = TRUE)
  
  # Calcular ZScore arredondado
  dados_controlos$ZScore <- round((dados_controlos$Valor - dados_controlos$Target) / dados_controlos$SD, 2)
  
  # Inicializar coluna de avaliacao
  dados_controlos$eval <- ""
  
  # Verificar número de amostras controlo
  n_controlos <- length(unique(dados_controlos$Amostra))
  dias_unicos <- unique(dados_controlos$Dia)
  
  if (n_controlos == 2) {
    
    # 1_3SD
    idx_13 <- which(abs(dados_controlos$ZScore) >= 3)
    dados_controlos$eval[idx_13] <- paste(dados_controlos$eval[idx_13], "1_3SD", sep = ";")
    
    # 1_2SD
    idx_12 <- which(abs(dados_controlos$ZScore) >= 2 & abs(dados_controlos$ZScore) < 3)
    dados_controlos$eval[idx_12] <- paste(dados_controlos$eval[idx_12], "1_2SD", sep = ";")
    
    # 2_2SD — mesma corrida (dia)
    # for (dia in dias_unicos) {
    #   linhas_dia <- which(dados_controlos$Dia == dia)
    #   z_dia <- dados_controlos$ZScore[linhas_dia]
    #   if (sum(z_dia >= 2 & z_dia < 3) >= 2 || sum(z_dia <= -2 & z_dia > -3) >= 2) {
    #     dados_controlos$eval[linhas_dia] <- paste(dados_controlos$eval[linhas_dia], "2_2SD", sep = ";")
    #   }
    # }
    
    # 2_2SD — dias consecutivos, mesma amostra
    # amostras <- unique(dados_controlos$Amostra)
    # for (a in amostras) {
    #   sub <- dados_controlos[dados_controlos$Amostra == a, ]
    #   sub <- sub[order(sub$Dia), ]
    #   for (i in 2:nrow(sub)) {
    #     z1 <- sub$ZScore[i-1]
    #     z2 <- sub$ZScore[i]
    #     d1 <- sub$Dia[i-1]
    #     d2 <- sub$Dia[i]
    #     idx1 <- rownames(sub)[i-1]
    #     idx2 <- rownames(sub)[i]
    # 
    #     if (d2 - d1 == 1) {
    #       if ((z1 >= 2 & z1 < 3) & (z2 >= 2 & z2 < 3) ||
    #           (z1 <= -2 & z1 > -3) & (z2 <= -2 & z2 > -3)) {
    #         dados_controlos$eval[rownames(dados_controlos) %in% c(idx1, idx2)] <-
    #           paste(dados_controlos$eval[rownames(dados_controlos) %in% c(idx1, idx2)], "2_2SD", sep = ";")
    #       }
    #     }
    #   }
    # }
    
    # R_4S — dois controlos no mesmo dia com diferenca > 4 SD
    # for (dia in dias_unicos) {
    #   linhas_dia <- which(dados_controlos$Dia == dia)
    #   z_dia <- dados_controlos$ZScore[linhas_dia]
    #   if (any(z_dia >= 2) && any(z_dia <= -2)) {
    #     dados_controlos$eval[linhas_dia] <- paste(dados_controlos$eval[linhas_dia], "R_4S", sep = ";")
    #   }
    # }
    
    # 4_1SD — 4 pontos consecutivos em 2 dias (2 controlos por dia)
    # dias_ordenados <- sort(unique(dados_controlos$Dia))
    # for (i in 1:(length(dias_ordenados) - 1)) {
    #   dia1 <- dias_ordenados[i]
    #   dia2 <- dias_ordenados[i + 1]
    #   
    #   linhas_dia1 <- which(dados_controlos$Dia == dia1)
    #   linhas_dia2 <- which(dados_controlos$Dia == dia2)
    #   
    #   # Considerar apenas os 2 primeiros controlos (pressupõe consistência)
    #   z1 <- dados_controlos$ZScore[linhas_dia1]
    #   z2 <- dados_controlos$ZScore[linhas_dia2]
    #   
    #   # Só aplicar se houver exatamente 2 controlos em cada dia
    #   if (length(z1) == 2 && length(z2) == 2) {
    #     z_all <- c(z1, z2)
    #     linhas_all <- c(linhas_dia1, linhas_dia2)
    #     
    #     if (all(z_all > 1) || all(z_all < -1)) {
    #       dados_controlos$eval[linhas_all] <- paste(dados_controlos$eval[linhas_all], "4_1SD", sep = ";")
    #     }
    #   }
    # }
    
    # 10X — dez resultados consecutivos no mesmo lado da média
    # total_linhas <- nrow(dados_controlos)
    # if (total_linhas >= 10) {
    #   for (i in 1:(total_linhas - 9)) {
    #     z_seq <- dados_controlos$ZScore[i:(i+9)]
    #     linhas_seq <- i:(i+9)
    #     
    #     if (all(z_seq > 0) || all(z_seq < 0)) {
    #       dados_controlos$eval[linhas_seq] <- paste(dados_controlos$eval[linhas_seq], "10X", sep = ";")
    #     }
    #   }
    # }
    
    # 7T – sete resultados consecutivos da mesma amostra com tendência crescente ou decrescente
    # if (length(dias_unicos) == 7) {
    #   amostras <- unique(dados_controlos$Amostra)
    #   
    #   for (a in amostras) {
    #     sub <- dados_controlos[dados_controlos$Amostra == a, ]
    #     sub <- sub[order(sub$Dia), ]
    #     
    #     if (nrow(sub) == 7) {
    #       z_seq <- sub$ZScore
    #       is_increasing <- all(diff(z_seq) > 0)
    #       is_decreasing <- all(diff(z_seq) < 0)
    #       
    #       if (is_increasing || is_decreasing) {
    #         linhas <- which(dados_controlos$Amostra == a)
    #         linhas <- linhas[order(dados_controlos$Dia[linhas])]
    #         dados_controlos$eval[linhas] <- paste(dados_controlos$eval[linhas], "7T", sep = ";")
    #       }
    #     }
    #   }
    # }
  }
  
  if (n_controlos == 3) {
    # 1_3SD
    idx_13 <- which(abs(dados_controlos$ZScore) >= 3)
    dados_controlos$eval[idx_13] <- paste(dados_controlos$eval[idx_13], "1_3SD", sep = ";")
    
    # 1_2SD
    idx_12 <- which(abs(dados_controlos$ZScore) >= 2 & abs(dados_controlos$ZScore) < 3)
    dados_controlos$eval[idx_12] <- paste(dados_controlos$eval[idx_12], "1_2SD", sep = ";")
    
    # 2de3_2SD — dois de três controlos no mesmo dia fora de ±2SD (mesma direcao)
    # for (dia in unique(dados_controlos$Dia)) {
    #   linhas_dia <- which(dados_controlos$Dia == dia)
    #   if (length(linhas_dia) == 3) {
    #     z_dia <- dados_controlos$ZScore[linhas_dia]
    #     pos <- sum(z_dia >= 2 & z_dia < 3)
    #     neg <- sum(z_dia <= -2 & z_dia > -3)
    #     
    #     if (pos >= 2 || neg >= 2) {
    #       dados_controlos$eval[linhas_dia] <- paste(dados_controlos$eval[linhas_dia], "2de3_2SD", sep = ";")
    #     }
    #   }
    # }
    
    # 3_1SD — três controlos no mesmo dia acima de +1 ou abaixo de -1
    # for (dia in unique(dados_controlos$Dia)) {
    #   linhas_dia <- which(dados_controlos$Dia == dia)
    #   if (length(linhas_dia) == 3) {
    #     z_dia <- dados_controlos$ZScore[linhas_dia]
    #     if (all(z_dia > 1) || all(z_dia < -1)) {
    #       dados_controlos$eval[linhas_dia] <- paste(dados_controlos$eval[linhas_dia], "3_1SD", sep = ";")
    #     }
    #   }
    # }
    
    # 6X — 6 resultados consecutivos (2 dias) do mesmo lado da média
    # dias_ordenados <- sort(unique(dados_controlos$Dia))
    # for (i in 1:(length(dias_ordenados) - 1)) {
    #   dia1 <- dias_ordenados[i]
    #   dia2 <- dias_ordenados[i + 1]
    #   
    #   linhas_dia1 <- which(dados_controlos$Dia == dia1)
    #   linhas_dia2 <- which(dados_controlos$Dia == dia2)
    #   
    #   if (length(linhas_dia1) == 3 && length(linhas_dia2) == 3) {
    #     linhas_all <- c(linhas_dia1, linhas_dia2)
    #     z_all <- dados_controlos$ZScore[linhas_all]
    #     
    #     if (all(z_all > 0) || all(z_all < 0)) {
    #       dados_controlos$eval[linhas_all] <- paste(dados_controlos$eval[linhas_all], "6X", sep = ";")
    #     }
    #   }
    # }
    
    # 7T – sete resultados consecutivos da mesma amostra com tendência crescente ou decrescente
    # if (length(dias_unicos) == 7) {
    #   amostras <- unique(dados_controlos$Amostra)
    #   
    #   for (a in amostras) {
    #     sub <- dados_controlos[dados_controlos$Amostra == a, ]
    #     sub <- sub[order(sub$Dia), ]
    #     
    #     if (nrow(sub) == 7) {
    #       z_seq <- sub$ZScore
    #       is_increasing <- all(diff(z_seq) > 0)
    #       is_decreasing <- all(diff(z_seq) < 0)
    #       
    #       if (is_increasing || is_decreasing) {
    #         linhas <- which(dados_controlos$Amostra == a)
    #         linhas <- linhas[order(dados_controlos$Dia[linhas])]
    #         dados_controlos$eval[linhas] <- paste(dados_controlos$eval[linhas], "7T", sep = ";")
    #       }
    #     }
    #   }
    # } # End 7T
    
  }
  
  # Limpeza final da coluna eval
  dados_controlos$eval <- gsub("^;|;$", "", dados_controlos$eval)  # remove ; no início/fim
  dados_controlos$eval[dados_controlos$eval == ""] <- NA           # "" vira NA
  
  # Ordenar por dia
  dados_controlos <- dados_controlos[order(dados_controlos$Dia), ]
  
  
  if (Output == 1) {
    return(dados_controlos)
    
  } else if (Output == 2) {
    dados_simulados$eval <- NA
    idx_qc <- which(dados_simulados$Tipo == "QC")
    dados_simulados$eval[idx_qc] <- dados_controlos$eval
    return(dados_simulados)
    
  } else if (Output == 3) {
    dias <- sort(unique(dados_controlos$Dia))
    resultado <- data.frame(Dia = dias, Avaliacao = "Accepted")
    
    for (i in seq_along(dias)) {
      dia_i <- dias[i]
      linhas_dia <- which(dados_controlos$Dia == dia_i)
      regras_dia <- na.omit(dados_controlos$eval[linhas_dia])
      regras_individuais <- unlist(strsplit(regras_dia, ";"))
      
      if (any(regras_individuais != "1_2SD")) {
        resultado$Avaliacao[i] <- "Rejected"
      }
    }
    return(resultado)
    
  } else {
    stop("Argument 'Output' must be 1 (controls), 2 (simulated data with eval), or 3 (daily run evaluation).")
  }
}

