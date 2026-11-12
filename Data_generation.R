# ---- libraries ----
library(ggplot2)
library(lme4)
library(nlme)
library(openxlsx)
library(knitr)
library(kableExtra)
library(tidyr)
library(dplyr)

# ---- Dados_cv ----
dados_cv <- data.frame(
  Amostra = c("HSP1", "HSP2", "HSP3", "HSP4", "HSP5",
              "PC ISD 1", "PC ISD 2", "PC ISD 3"),
  Média = c(1.27, 3.68, 8.03, 29.3, 36.3, 1.8, 8.91, 16.2),
  DPr = c(0.056, 0.114, 0.251, 0.783, 0.869, 0.065, 0.193, 0.337),
  CVr = c("4,4%", "3,1%", "3,1%", "2,7%", "2,4%", "3,6%", "2,2%", "2,1%"),
  DPwl = c(0.069, 0.123, 0.303, 1.05, 1.12, 0.079, 0.254, 0.401),
  CVwl = c("5,4%", "3,3%", "3,8%", "3,6%", "3,1%", "4,4%", "2,9%", "2,5%"),
  stringsAsFactors = FALSE
)

# ConversC#o correta: retirar o sC-mbolo % e dividir por 100
dados_cv$CVr <- as.numeric(gsub(",", ".", gsub("%", "", dados_cv$CVr))) / 100
dados_cv$CVwl <- as.numeric(gsub(",", ".", gsub("%", "", dados_cv$CVwl))) / 100

# ---- Criar um data frame atributos vazio com as colunas necessárias ----
criar_tabela_atributos <- function() {
  data.frame(
    Amostra = character(),
    Target = numeric(),
    SD_repetibilidade = numeric(),
    SD_Within_Laboratory = numeric(),
    stringsAsFactors = FALSE
  )
}

# ---- Adicionar uma nova amostra ao data frame de atributos ----
adicionar_atributos_amostra <- function(df, nome, Target, SD_repetibilidade, SD_Within_Laboratory) {
  # Verificar se SD_repetibilidade é maior que SD_Within_Laboratory
  if (SD_repetibilidade > SD_Within_Laboratory) {
    stop("Erro: O valor de SD_repetibilidade deve ser inferior ou igual a SD_Within_Laboratory.")
  }
  
  nova_linha <- data.frame(
    Amostra = nome,
    Target = Target,
    SD_repetibilidade = SD_repetibilidade,
    SD_Within_Laboratory = SD_Within_Laboratory,
    stringsAsFactors = FALSE
  )
  rbind(df, nova_linha)
}

# ---- Calcular o desvio padrao da componente diária (SD_target) ----
calcular_SD_target <- function(atributos_df) {
  # Calcula os coeficientes de variacao e os desvios padrao da componente diária da variabilidade
  CVr <- atributos_df$SD_repetibilidade / atributos_df$Target
  CVwl <- atributos_df$SD_Within_Laboratory / atributos_df$Target
  CV_target <- sqrt(CVwl^2 - CVr^2) # componente interdiária (relativa)
  SD_target <- CV_target * atributos_df$Target # componente interdiária (absoluta)
  
  # Adiciona os coeficientes de variacao e os desvios padrao ao data frame
  atributos_df$CVr <- CVr
  atributos_df$CVwl <- CVwl
  atributos_df$CV_target_utilizado <- CV_target
  atributos_df$SD_target <- SD_target
  
  return(atributos_df)
}

# ---- Gerar valores alvo (targets) diários com correlacao entre amostras ----
gerar_targets_diarios <- function(atributos_df, dias = 5, correlacao = 0.6) {
  # 1. Cálculo da componente interdiária (SD_target)
  # Usa os SDs fornecidos e aplica a decomposicao da variância
  atributos_com_sd <- calcular_SD_target(atributos_df)
  
  # 2. Obtencao dos vetores de desvio padrao e médias alvo
  SD_Target <- atributos_com_sd$SD_target # SD interdiário por amostra
  media_targets <- atributos_com_sd$Target # Média de cada amostra
  amostras <- atributos_com_sd$Amostra # Nomes das amostras
  
  # 3. Construir matriz de covariância
  # Esta matriz incorpora a correlacao entre as amostras no mesmo dia
  cov_matrix <- outer(SD_Target, SD_Target, function(a, b) correlacao * a * b)
  diag(cov_matrix) <- SD_Target^2 # Variância de cada amostra na diagonal
  
  # 4. Simulacao dos valores alvo diários
  # Gera um vetor de médias por dia com distribuicao multivariada normal
  library(MASS)
  targets <- MASS::mvrnorm(n = dias, mu = media_targets, Sigma = cov_matrix)
  colnames(targets) <- amostras # Dar nomes às colunas com os identificadores das amostras
  
  # 5. Devolve o data frame com os targets simulados
  return(as.data.frame(targets))
}

# ---- Gerar os replicados correlacionados ----
gerar_replicados_correlacionados <- function(targets_diarios, atributos_df, n_reps, n_QC = 0) {
  # Obtém o número de dias e as amostras presentes nos targets diários
  dias <- nrow(targets_diarios)
  amostras <- colnames(targets_diarios)
  
  # Inicializa um data frame vazio para armazenar os resultados
  resultados <- data.frame()
  
  # Loop para gerar os replicados diários
  for (dia in 1:dias) {
    for (amostra in amostras) {
      # Obtém o valor alvo diário para a amostra no dia corrente
      target_dia <- targets_diarios[dia, amostra]
      
      # Obtém o desvio padrao da repetibilidade para a amostra atual
      sd_rep <- atributos_df$SD_repetibilidade[atributos_df$Amostra == amostra]
      
      # Gerar os valores para os controlos de qualidade (QC) se n_QC > 0 e para os replicados da amostra
      valores_qc <- rnorm(n_QC, mean = target_dia, sd = sd_rep)
      valores_rep <-rnorm(n_reps, mean = target_dia, sd = sd_rep)
      
      # Criar o data frame para os controlos de qualidade (QC)
      df_qc <- if (n_QC > 0) {
        data.frame(
          Dia = dia, # Dia correspondente
          Amostra = amostra, # Nome da amostra
          Tipo = "QC", # Tipo: QC (controle de qualidade)
          Replicado = 1:n_QC, # Identificador dos replicados de QC
          Valor = valores_qc # Valores gerados para os controlos de qualidade
        )
      } else {
        NULL # Se n_QC = 0, nao gera nenhum dado de QC
      }
      
      df_rep <- data.frame(
        Dia = dia, # Dia correspondente
        Amostra = amostra, # Nome da amostra
        Tipo = "Amostra", # Tipo: Amostra
        Replicado = 1:n_reps, # Identificador dos replicados da amostra
        Valor = valores_rep # Valores gerados para os replicados
      )
      
      # Adicionar os dados de QC e amostras ao data frame de resultados
      resultados <- rbind(resultados, df_qc, df_rep)
    }
  }
  return(resultados)
}

# ---- Plot CVs Alegacões do fabricante ----
plotar_cv_vs_media <- function(dados_cv) {
  # Estrutura longa
  dados_long <- data.frame(
    Amostra = rep(dados_cv$Amostra, 2),
    Media = rep(dados_cv$Média, 2),
    CV = c(dados_cv$CVr, dados_cv$CVwl),
    Tipo = rep(c("CVr", "CVwl"), each = nrow(dados_cv)),
    stringsAsFactors = FALSE
  )
  
  # Converter CV para numérico
  dados_long$CV <- as.numeric(gsub(",", ".", gsub("%", "", dados_long$CV))) / 100
  
  # Gráfico com linhas a ligar os pontos de cada tipo
  library(ggplot2)
  ggplot(dados_long, aes(x = Media, y = CV, color = Tipo, shape = Tipo)) +
    geom_line(aes(group = Tipo), linewidth = 1) +  # linha entre os pontos do mesmo tipo
    geom_point(size = 3) +
    labs(x = "Média", y = "Coeficiente de Variacao (CV)", color = "Tipo", shape = "Tipo") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme_minimal()
}

# ---- Funcao para criar os data frames, ### Fora de uso? ### ----
replicados_df <- function(vetor_origem, Dias, n_reps) {
  # Calcular o número total de linhas (dias * n_reps)
  total_linhas <- Dias * n_reps
  
  # Verificar se o vetor de origem tem o número exato de elementos necessários
  if (length(vetor_origem) != total_linhas) {
    stop(paste("Erro: O vetor de origem deve ter exatamente", total_linhas, "elementos."))
  }
  
  # Criar as colunas
  Indice <- 1:total_linhas
  Dia <- rep(1:Dias, each = n_reps)
  Replicado <- rep(1:n_reps, times = Dias)
  Tipo <- rep("Amostra", total_linhas)
  Amostra <- rep(deparse(substitute(vetor_origem)), total_linhas)
  
  # Criar o data frame
  df <- data.frame(Indice, Dia, Replicado, Valor = vetor_origem, Tipo, Amostra)
  
  return(df)
}

# ---- Funcao cv_esperado ----
cv_esperado <- function(df, concentracao) {
  df <- df[order(df$Média), ]
  
  # Verificacao do intervalo
  if (concentracao < min(df$Média) || concentracao > max(df$Média)) {
    return(data.frame(
      Concentracao = concentracao,
      CVr_esperado = NA,
      CVwl_esperado = NA
    ))
  }
  
  # Encontrar intervalo base [Média[i], Média[i+1]]
  for (i in 1:(nrow(df) - 1)) {
    if (df$Média[i] < concentracao && concentracao < df$Média[i + 1]) {
      break
    }
  }
  
  # Índices candidatos aos extremos
  idx_ext1 <- if (i - 1 >= 1) i - 1 else NA
  idx_ext2 <- if (i + 2 <= nrow(df)) i + 2 else NA
  
  dist1 <- if (!is.na(idx_ext1)) abs(df$Média[idx_ext1] - concentracao) else Inf
  dist2 <- if (!is.na(idx_ext2)) abs(df$Média[idx_ext2] - concentracao) else Inf
  
  # Escolher o intervalo baseado na distância
  if (dist1 < dist2) {
    pontos <- df[c(idx_ext1, i, i + 1), ]
  } else if (dist2 < dist1) {
    pontos <- df[c(i, i + 1, idx_ext2), ]
  } else {  # Empate
    pontos1 <- df[c(idx_ext1, i, i + 1), ]
    pontos2 <- df[c(i, i + 1, idx_ext2), ]
    
    # Interpolar CVr em cada intervalo
    interp1 <- approx(pontos1$Média[c(1, 3)], pontos1$CVr[c(1, 3)], xout = concentracao)$y
    interp2 <- approx(pontos2$Média[c(1, 3)], pontos2$CVr[c(1, 3)], xout = concentracao)$y
    
    if (interp1 >= interp2) {
      pontos <- pontos1
    } else {
      pontos <- pontos2
    }
  }
  
  pontos <- pontos[order(pontos$Média), ]
  x <- pontos$Média
  y_r <- pontos$CVr
  y_wl <- pontos$CVwl
  
  # Verificacao de curvatura (se o ponto intermédio está acima da linha entre extremos)
  interp_cv <- function(x, y, x0) {
    y_extremos <- approx(x = c(x[1], x[3]), y = c(y[1], y[3]), xout = x[2])$y
    if (y[2] > y_extremos) {
      if (x[2] < x0) {
        return(approx(x = c(x[2], x[3]), y = c(y[2], y[3]), xout = x0)$y)
      } else {
        return(approx(x = c(x[1], x[2]), y = c(y[1], y[2]), xout = x0)$y)
      }
    } else {
      return(approx(x = c(x[1], x[3]), y = c(y[1], y[3]), xout = x0)$y)
    }
  }
  
  cv_r_esp <- interp_cv(x, y_r, concentracao)
  cv_wl_esp <- interp_cv(x, y_wl, concentracao)
  
  data.frame(
    Concentracao = concentracao,
    CVr_esperado = cv_r_esp,
    CVwl_esperado = cv_wl_esp
  )
}

# ---- criar_df_amostras ----
criar_df_amostras <- function(amostras) {
  amostras_fmt <- gsub(" ", "_", amostras)
  
  df <- data.frame(
    Nome = amostras,
    CVr = paste0("CVr_", amostras_fmt),
    CVwl = paste0("CVwl_", amostras_fmt),
    Media = paste0("Media_", amostras_fmt),
    stringsAsFactors = FALSE
  )
  
  return(df)
}
