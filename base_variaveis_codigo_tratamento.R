library(dplyr)
library(readxl)
library(openxlsx)

# Lendo bases de dados
causas_mal_definidas     <- read.csv("causas_mal_definidas.csv",     sep = ";", encoding = "UTF-8")
doencas_respiratorias    <- read.csv("doencas_respiratorias.csv",    sep = ";", encoding = "UTF-8")
obitos_fetais            <- read.csv("óbitos_fetais.csv",            sep = ";", encoding = "UTF-8")
obitos_infantis          <- read.csv("obitos_infantis.csv",          sep = ";", encoding = "UTF-8")
obitos_60_mais           <- read.csv("óbitos_60+.csv",               sep = ";", encoding = "UTF-8")
mortalidade_geral        <- read.csv("mortalidade_geral.csv",        sep = ";", encoding = "UTF-8")
nascidos_vivos_municipio <- read.csv("nascidos_vivos_municipio.csv", sep = ";", encoding = "UTF-8")
populacao_municipio      <- read_excel("populacao_por_municipio.xlsx")

renomear_anos <- function(df) {
  df %>% 
    rename_with(
      ~ gsub("^X(20\\d{2})$", "\\1", .x),  # X2018 -> 2018
      matches("^X20\\d{2}$")
    )
}

# Lista nomeada de dataframes
lista_dfs <- list(
  causas_mal_definidas     = causas_mal_definidas,
  doencas_respiratorias    = doencas_respiratorias,
  obitos_fetais            = obitos_fetais,
  obitos_infantis          = obitos_infantis,
  obitos_60_mais           = obitos_60_mais,
  mortalidade_geral        = mortalidade_geral,
  nascidos_vivos_municipio = nascidos_vivos_municipio,
  populacao_municipio      = populacao_municipio
)

# 1) renomeia X2017...X2022 -> 2017...2022
lista_dfs <- lapply(lista_dfs, renomear_anos)

# 2) padroniza tipos
padronizar_tipos <- function(df) {
  df %>% 
    mutate(
      codigo = as.character(codigo)
    ) %>%
    mutate(
      across(matches("^20\\d{2}$"), ~ suppressWarnings(as.numeric(.x)))
    )
}

lista_dfs <- lapply(lista_dfs, padronizar_tipos)

# 4) empilha tudo
df_unico <- bind_rows(lista_dfs)

df_unico <- df_unico %>% 
  mutate(
    antes_pandemia   = rowSums(across(`2017`:`2019`), na.rm = TRUE),
    durante_pandemia = rowSums(across(`2020`:`2022`), na.rm = TRUE)
  ) %>% 
  select(
    -`2017`, -`2018`, -`2019`,
    -`2020`, -`2021`, -`2022`
  )


# Transformando em excel
write.xlsx(df_unico, "base_variáveis.xlsx")
