library(basedosdados)
library(tidyverse)
library(gganimate)
library(geobr)
library(patchwork)
library(fpp3)


basedosdados::set_billing_id('magia-dos-bolinhos')

# Baixar o arquivo temporariamente
url <- "https://www.epe.gov.br/sites-pt/publicacoes-dados-abertos/dados-abertos/Documents/Dados_abertos_Consumo_Mensal.xlsx"
temp_file <- base::tempfile(fileext = ".xlsx")

# Download do arquivo
httr::GET(url, httr::write_disk(temp_file, overwrite = TRUE))

# Ler o arquivo Excel
epe <- readxl::read_excel(temp_file)

### Consumo Total ####

epe %>% 
  dplyr::summarise(Consumo = sum(Consumo),
                   .by = DataExcel) %>% 
  dplyr::mutate(DataExcel = tsibble::yearmonth(DataExcel)) %>%
  tsibble::as_tsibble(index = DataExcel) %>% 
  fabletools::model(STL(Consumo)) %>% 
  fabletools::components() %>%
  dplyr::mutate(DataExcel = as.Date(DataExcel)) %>% 
  ggplot() +
  geom_line(aes(x = DataExcel,
                y = Consumo,
                color = 'Original',
                linetype = 'Original'),
            linewidth = 1.45,
            alpha = 0.7) +
  geom_line(aes(x = DataExcel,
                y = trend,
                color = 'Tendência',
                linetype = 'Tendência'),
            linewidth = 1.45) +
  labs(x = element_blank(),
       y = element_blank(),
       linetype = 'Série',
       color = 'Série') +
  scale_x_date(date_breaks = '2 year',
               date_labels = '%y') +
  scale_y_continuous(labels = scales::number_format(suffix = ' TWh',
                                                    scale = 1/1e6)) +
  scale_linetype_manual(values = c('solid','longdash')) +
  scale_color_manual(values = c('black', 'grey')) +
  theme_minimal() +
  theme(text = element_text(size = 22),
        legend.position = 'top')


p <- consumo %>% 
  dplyr::summarise(Consumo = sum(Consumo),
                   .by = c(DataExcel, Classe, UF)) %>%
  tidyr::pivot_wider(names_from = Classe,
                     values_from = Consumo) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(Total = sum(Comercial, Industrial, Outros, Residencial, Rural),
                across(Comercial:Rural, ~ .x/Total),
                DataExcel = as.Date(DataExcel)) %>%
  dplyr::ungroup() %>% 
  tidyr::pivot_longer(cols = c(Comercial:Rural)) %>% 
  dplyr::right_join(estados,
                    by = c('UF' = 'abbrev_state')) %>% 
  ggplot() +
  geom_sf(aes(fill = value, geometry = geom)) +
  scale_fill_viridis_c(option = "viridis",
                       name = "Consumo (%)",
                       breaks = seq(0,0.88, 0.2),
                       limits = c(0, 0.8)) +
  labs(title = 'Data: {frame_time} / 2024-07-01',
       x = element_blank(),
       y = element_blank()) +
  facet_wrap(~name) +
  theme_void() +
  transition_time(DataExcel) +
  ease_aes('linear')

a <- Sys.time()
animate(p, 
        width = 1000, height = 800, 
        fps = 24, duration = 40, 
        renderer = gifski_renderer("consumo_residencial.gif"))
Sys.time() - a


#### Geração #####

geracao <- basedosdados::read_sql('
    SELECT
      dados.geracao_termica_verificada as geracao_termica_verificada,
      dados.geracao_fotovoltaica_verificada as geracao_fotovoltaica_verificada,
      dados.carga_verificada as carga_verificada,
      dados.geracao_hidraulica_verificada as geracao_hidraulica_verificada,
      dados.geracao_eolica_verificada as geracao_eolica_verificada,
      dados.ano as ano,
      dados.mes as mes
    FROM `basedosdados.br_ons_estimativa_custos.balanco_energia_subsistemas` AS dados') %>% 
  dplyr::group_by(ano, mes) %>% 
  dplyr::summarise(across(geracao_termica_verificada:geracao_eolica_verificada,
                          ~ sum(.x, na.rm = T)/1000000000000)) %>% 
  dplyr::mutate(data = as.Date(paste(ano, mes, '01', sep='-')))

estados <- geobr::read_state()

readxl::read_xlsx("https://www.epe.gov.br/sites-pt/publicacoes-dados-abertos/dados-abertos/Documents/Dados_abertos_Consumo_Mensal.xlsx")

#### Geração #####

geracao <- geracao %>% 
  tidyr::pivot_longer(cols = geracao_termica_verificada:geracao_eolica_verificada,
                      values_to = 'Carga',
                      names_to = 'Geração') %>% 
  dplyr::mutate(data = as.Date(data),
                Geração = factor(Geração,
                                 levels = c('carga_verificada',
                                            'geracao_hidraulica_verificada',
                                            'geracao_termica_verificada',
                                            'geracao_eolica_verificada',
                                            'geracao_fotovoltaica_verificada'),
                                 labels = c('Total',
                                            'Hidráulica',
                                            'Térmica',
                                            'Eólica',
                                            'Fotovoltaica')))

geracao_prop <- geracao %>% 
  dplyr::filter(Geração != 'Total') %>% 
  ggplot() +
  aes(x = data,
      y = Carga,
      fill = Geração) +
  geom_area(position = 'fill') +
  labs(x = element_blank(),
       y= element_blank()) +
  scale_x_date(date_labels = '%b/%y',
               date_breaks = '2 year') +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_linetype_manual(values = c('longdash','solid')) +
  scale_fill_manual(values=c('#0047BBFF','#C8102EFF',"#E4D5D3FF",'#FFB81CFF')) +
  theme_minimal() +
  theme(legend.position = 'top',
        text = element_text(size = 22))

  
geracao_total <- geracao %>% 
  dplyr::filter(Geração == 'Total') %>% 
  tsibble::as_tsibble(index = data, key = Geração) %>% 
  dplyr::mutate(data = tsibble::yearmonth(data)) %>% 
  fabletools::model(STL(Carga)) %>% 
  fabletools::components() %>%
  dplyr::mutate(data = as.Date(data)) %>% 
  ggplot() +
  geom_line(aes(x = data,
                y = Carga,
                linetype = "Original"),
            linewidth = 1.45,
            color = 'grey') +
  geom_line(aes(x = data,
                y = trend,
                linetype = 'Tendência'),
            linewidth = 1.45,
            alpha = 1) +
  labs(x = element_blank(),
       y= element_blank(),
       linetype = 'Série') +
  scale_x_date(date_labels = '%b/%y',
               date_breaks = '2 year') +
  scale_linetype_manual(values = c('longdash','solid')) +
  theme_minimal() +
  theme(legend.position = 'top',
        text = element_text(size = 22))

(geracao_total / geracao_prop)

