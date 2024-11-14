# br_pop1a.R

# This script downloads IBGE population projections for Brazil and creates Amazon Legal and related aggregates
# Then saves the data as an R dataframe in a working repository and in a Shiny App 


library(dplyr)
library(openxlsx)

# Load the data from the IBGE website: https://www.ibge.gov.br/estatisticas/sociais/populacao/9109-projecao-da-populacao.html; 
pop01_70a <- openxlsx::read.xlsx("C:/Users/wb164718/OneDrive - WBG/AmazEduData/rawdata/ibge/projecoes_2024_tab3_grupos_etarios_especificos.xlsx", 
                                 sheet = 1, startRow = 7, colNames = TRUE)

# Select relevant columns and rename
pop01_70b <- pop01_70a %>%
  select(1:4, POP_T, `0-14_T`, `15-17_T`, `18-21_T`, `15-59_T`, `60+_T`) %>%
  mutate(CODEFED = as.character(`CÓD.`)) %>%
  select(-`CÓD.`) %>%
  relocate(CODEFED, .after = "SIGLA")

# Define lists for aggregate regions
amazonia_legal <- c("Acre", "Amapá", "Amazonas", "Maranhão", "Mato Grosso",
                    "Pará", "Rondônia", "Roraima", "Tocantins")

nordeste_r <- c("Alagoas", "Bahia", "Ceará", "Paraíba", 
                "Pernambuco", "Piauí", "Rio Grande do Norte", "Sergipe")

centro_oeste_r <- c("Distrito Federal", "Goiás", "Mato Grosso do Sul")

variables_to_sum <- c("POP_T", "0-14_T", "15-17_T", "18-21_T", "15-59_T", "60+_T")


# I create hopefully intuitive SIGLA and  CODEFED for the new aggregates

# Aggregate regions with updated syntax
amazon_aggregate <- pop01_70a %>%
  filter(LOCAL %in% amazonia_legal) %>%
  group_by(ANO) %>%
  summarise(
    LOCAL = "Amazonia_Legal",
    CODEFED = "99",
    SIGLA = "AML",
    across(all_of(variables_to_sum), \(x) sum(x, na.rm = TRUE))
  )

nordeste_aggregate <- pop01_70a %>%
  filter(LOCAL %in% nordeste_r) %>%
  group_by(ANO) %>%
  summarise(
    LOCAL = "Nordeste_r",
    CODEFED = "2b",
    SIGLA = "ND_",
    across(all_of(variables_to_sum), \(x) sum(x, na.rm = TRUE))
  )

centro_oeste_aggregate <- pop01_70a %>%
  filter(LOCAL %in% centro_oeste_r) %>%
  group_by(ANO) %>%
  summarise(
    LOCAL = "Centro-Oeste_r",
    CODEFED = "5b",
    SIGLA = "CO_",
    across(all_of(variables_to_sum), \(x) sum(x, na.rm = TRUE))
  )

# Combine aggregates with the main dataframe
pop01_70b <- bind_rows(pop01_70b, amazon_aggregate, nordeste_aggregate, centro_oeste_aggregate)

# Calculate proportions
pop01_70b <- pop01_70b %>%
  mutate(
    P_0_14_T = `0-14_T` / POP_T,
    P_15_17_T = `15-17_T` / POP_T,
    P_18_21_T = `18-21_T` / POP_T,
    P_15_59_T = `15-59_T` / POP_T,
    P_60_plus_T = `60+_T` / POP_T
  )

# Determine the crossover year and add crossover values
pop01_70b <- pop01_70b %>%
  group_by(LOCAL) %>%
  mutate(
    Crossover_Flag = if_else(
      lag(`0-14_T`) > lag(`60+_T`) & `0-14_T` < `60+_T`,
      1,
      0
    ),
    Crossover_Value_Num = if_else(Crossover_Flag == 1, `0-14_T`, NA_real_),
    Crossover_Value_Prop = if_else(Crossover_Flag == 1, P_0_14_T, NA_real_)
  ) %>%
  ungroup()


# save the R dataframe in working folder
save(pop01_70b, file = "C:/Users/wb164718/OneDrive - WBG/AmazEduData/working/ibge/pop01_70b.rda")
# sabe the R datadrame in Shiny App
save(pop01_70b, file = "D:/Country/Brazil/AmazEdu/RShiny/AmazEduPopu1/pop01_70b.rda")


