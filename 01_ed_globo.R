## Setup
source("C:/Users/vdeolinfo/Documents/data_storyline/setup.R")

## Entrada de dados

base <- read_delim("ge_df_users_editorias_revisada.csv",
                   ";", 
                   escape_double = FALSE, 
                   locale = locale(asciify = TRUE), 
                   na = "NA", 
                   trim_ws = TRUE) %>% as.data.frame()

## Estr;utura dos dados
ExpData(data=base,type=1) %>% 
  kable() %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                  font_size = 12)

## Estrutura dos dados por variaveis
ExpData(data=base,type=2) %>% 
  kable() %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                  font_size = 12)

## Verificação de dados faltantes
missmap(base,main = "Mapa de valores faltantes")

## Resumo de dados categoricos
DT::datatable(ExpCTable(base,
                        Target=NULL,
                        margin=1,
                        clim=10,
                        nlim=NULL,
                        round=2,
                        bin=NULL,
                        per=T))

## Mapa de Correlação das variáveis numericas
M <- cor(select_if(base,is.numeric))

cor(select_if(base,is.numeric)) %>% 
  kable() %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                  font_size = 12)


corrplot(M,method = "number")

## Analise Exploratoria de dados
# Cartola Status
ggplot(base) +
  aes(x = visitas, y = pviews, colour = cartola_status) +
  geom_point(size = 3L) +
  scale_color_hue() +
  labs(title = "Análise Explorátoria de dados ", 
       subtitle = "Cartola Status") +
  coord_flip() +
  theme_minimal() +
  facet_wrap(vars(cartola_status), scales = "free")

# PC, Mobile e PC&Mobile
ggplot(base) +
  aes(x = cartola_status, weight = user) +
  geom_bar(fill = "#35b779") +
  labs(x = "Device", title = "Análise Explorátoria de dados ", subtitle = "PC, Mobile e PC&Mobile") +
  coord_flip() +
  theme_minimal()

# Sexo
ggplot(base) +
  aes(x = sexo, weight = user) +
  geom_bar(fill = "#35b779") +
  labs(x = "Sexo", title = "Análise Explorátoria de dados ", 
       subtitle = "Sexo") +
  theme_minimal()

# Estados
base <- base %>%
  filter(!is.na(uf))
ggplot(base) +
  aes(x = uf, weight = user) +
  geom_bar(fill = "#35b779") +
  labs(x = "UF", title = "Análise Explorátoria de dados ", subtitle = "UF") +
  coord_flip() +
  theme_minimal()

# Gráficos com referencia cruzado
ggplot(base) +
  aes(x = device, fill = device, weight = user) +
  geom_bar() +
  scale_fill_hue() +
  labs(title = "Análise Explorátoria de dados ", subtitle = "Device e Cartola Status") +
  theme_minimal() +
  theme(legend.position = "top") +
  facet_wrap(vars(cartola_status))





