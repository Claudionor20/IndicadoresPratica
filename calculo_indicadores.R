library(readxl)
library(dplyr)
library(tidyr)
library(janitor)

# 1. Ler base
df <- read_excel("base_variáveis.xlsx") %>% 
  clean_names()

# 2. Pivotar para formato wide
wide <- df %>%
  pivot_wider(
    names_from = tipo_variavel,
    values_from = c(antes_pandemia, durante_pandemia),
    names_sep = "_"
  )

# 3. Criar indicadores
indicadores <- wide %>%
  mutate(
    
    # --- Taxa Mortalidade 60+ ---
    tx_mortalidade_60_mais_17_19 = antes_pandemia_obitos_60_mais / antes_pandemia_populacao,
    tx_mortalidade_60_mais_20_22 = durante_pandemia_obitos_60_mais / durante_pandemia_populacao,
    
    # --- Variação Relativa Mortalidade 60+ ---
    tx_var_mort = (tx_mortalidade_60_mais_20_22 / tx_mortalidade_60_mais_17_19) - 1,
    
    # --- Óbitos Fetais ---
    nascimentos_totais_17_19 = antes_pandemia_nascidos_vivos + antes_pandemia_obitos_fetais,
    nascimentos_totais_20_22 = durante_pandemia_nascidos_vivos + durante_pandemia_obitos_fetais,
    
    tx_obitos_fetais_17_19 = antes_pandemia_obitos_fetais / nascimentos_totais_17_19,
    tx_obitos_fetais_20_22 = durante_pandemia_obitos_fetais / nascimentos_totais_20_22,
    
    # --- Doenças Respiratórias ---
    tx_resp_17_19 = antes_pandemia_doencas_respiratorias / antes_pandemia_populacao,
    tx_resp_20_22 = durante_pandemia_doencas_respiratorias / durante_pandemia_populacao,
    
    # --- Variação relativa respiratória ---
    tx_var_resp = (tx_resp_20_22 / tx_resp_17_19) - 1,
    
    # --- Causas mal definidas (CMD) ---
    tx_cmd_17_19 = antes_pandemia_causas_mal_definidas / antes_pandemia_mortalidade_geral,
    tx_cmd_20_22 = durante_pandemia_causas_mal_definidas / durante_pandemia_mortalidade_geral,
    
    # --- Indicador qualitativo impacto 60+ ---
    ind_impacto_mort_60_mais = case_when(
      tx_var_mort < quantile(tx_var_mort, 1/3, na.rm = TRUE) ~ "Baixo",
      tx_var_mort < quantile(tx_var_mort, 2/3, na.rm = TRUE) ~ "Moderado",
      TRUE ~ "Alto"
    ),
    
    # --- Qualidade CMD ---
    ind_qualidade_registro_cmd = case_when(
      tx_cmd_20_22 < 0.05 ~ "Boa",
      tx_cmd_20_22 <= 0.10 ~ "Regular",
      tx_cmd_20_22 > 0.10 ~ "Ruim",
      TRUE ~ NA_character_
    )
  )
