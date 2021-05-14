library(tidyverse)

#' @description Padroniza a sigla, removendo espaços, pontos e "º";
#' padroniza nome de medidas provisórias e deixa tudo em lowercase.
.padroniza_sigla <- function(df) {
  df %>%
    mutate(sigla_processada = gsub("º| |\\.", "", sigla) %>% tolower()) %>%
    mutate(sigla_processada = gsub("mpv", "mp", sigla_processada))
}

#' @description Padroniza a data para ser a segunda-feira da semana 
#' correspondente à data do tweet.
.padroniza_data <- function(df) {
  library(aweek)
  df %>%
    mutate(week = date2week(created_at)) %>%
    mutate(date = week2date(week, week_start = 1, floor_day = T)) %>%
    select(-c(week, created_at))
}

#' @description Faz o mapeamento de proposição para id sem considerar o ano.
.mapeia_citadas_sem_ano_para_id <-
  function(tweets_raw, proposicoes) {
    tweets_raw %>%
      filter(!str_detect(sigla_processada, "/")) %>%
      left_join(
        proposicoes %>%
          mutate(
            sigla_processada_sem_ano = str_remove(sigla_processada, "/.*")
          ),
        by = c("sigla_processada" = "sigla_processada_sem_ano")
      ) %>%
      group_by(sigla_processada) %>%
      mutate(siglas_diferentes = n_distinct(sigla)) %>%
      ungroup() %>%
      mutate(ambigua = siglas_diferentes > 1) %>%
      filter(!is.na(id_leggo), !ambigua) %>%
      select(id_leggo,
             sigla,
             username,
             created_at,
             text,
             interactions)
}

#' @description Faz o mapeamento de proposição para as proposições monitoradas
#' pelo Painel.
mapeia_citadas_para_id <- function(
  tweets_path = here::here("data/raw/tweets_parlamentares_e_influenciadores_v2.csv.zip"),
  props_path = here::here("data/raw/proposicoes.csv")) {
    
    tweets_raw <- read_csv(tweets_path) %>%
      rename(sigla = citadas) %>%
      .padroniza_sigla() %>%
      select(-sigla)
    
    proposicoes <- read_csv(props_path) %>%
      distinct(sigla, id_leggo) %>%
      .padroniza_sigla()
    
    tweets_proposicoes_com_ano <- tweets_raw %>%
      left_join(proposicoes, by = "sigla_processada") %>%
      filter(!is.na(id_leggo))
    
    tweets_proposicoes_sem_ano <-
      .mapeia_citadas_sem_ano_para_id(tweets_raw, proposicoes)
    
    tweets_proposicoes_df <- tweets_proposicoes_com_ano %>%
      bind_rows(tweets_proposicoes_sem_ano)
    
    tweets_proposicoes_df <- tweets_proposicoes_df %>%
      .padroniza_data() %>% 
      select(sigla,
             username,
             date,
             text,
             interactions) %>% 
      distinct()
    
    return(tweets_proposicoes_df)
  }

#' @description Calcula a pressão considerando duas formas:
#' A primeira normaliza tanto o número de usuários quanto o de engajamento, 
#' dividindo cada coluna pelo maior valor geral, deixando-as numa escala 
#' entre 0 e 1. Depois disso é feita uma média ponderada onde 
#' score = 0.5 * usuarios + 0.5 * engajamento.
#' A segunda considera o log10 do engajamento no cálculo, 
#' além da normalização tanto do número de usuários quanto o valor do 
#' log2 de engajamento, dividindo cada coluna pelo maior valor geral, 
#' deixando-as numa escala entre 0 e 1. Depois disso é feita uma média 
#' ponderada onde score = 0.5 * usuarios + 0.5 * log2(engajamento).
.calcula_pressao <- function(df) {
  df %>%
    mutate(log_interactions = if_else(sum_interactions > 0,
                                      log2(sum_interactions),
                                      0)) %>%
    mutate(
      norm_user_count = user_count / max(user_count),
      norm_sum_interactions = sum_interactions / max(sum_interactions),
      norm_log_sum_interactions = log_interactions / max(log_interactions)
    ) %>%
    mutate(
      metricas_normalizadas = 0.5 * norm_user_count + 0.5 * norm_sum_interactions,
      metricas_norm_log_interactions = 0.5 * norm_user_count + 0.5 * norm_log_sum_interactions
    ) %>%
    complete(date, sigla, fill = list(interactions = 0)) %>%
    mutate_at(vars(user_count:metricas_norm_log_interactions),
              list( ~ ifelse(is.na(.), 0, .)))
}

calcula_pressao_tweets <- function(tweets_proposicoes) {
  df <- tweets_proposicoes %>%
    mutate(interactions = replace_na(interactions, 0)) %>%
    group_by(sigla, date) %>%
    summarise(user_count = n_distinct(username),
              sum_interactions = sum(interactions)) %>%
    ungroup()
  
  
  df <- df %>% 
    .calcula_pressao()
  
  return(df)
}

tweets_proposicoes <- mapeia_citadas_para_id()
pressao <- calcula_pressao_tweets(tweets_proposicoes)

write_csv(
  pressao,
  here::here("data/ready/pressao.csv")
)
