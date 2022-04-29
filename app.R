################## SETUP ##################
library(shiny)
library(tidyr)
library(dplyr)
library(shadowtext)
library(ggrepel)
library(ggplot2)
library(plotly)
library(stringr)
library(lubridate)
library(readr)
library(data.table)
library(smooth)
library(htmltools)
library(viridis)
library(BAMMtools)
library(leaflet)
library(sf)
library(highcharter)
library(scales)

options(rsconnect.error.trace = TRUE) 
options(scipen = 999)

# Carregar dados gerais (população e códigos das UFS e shape dos municípios brasileiros):
load("data.RData")

################# BASES DE DADOS E TRANSFORMAÇÕES #################

load("alldata.RData")
#load(url("https://github.com/cassianord/covid_shiny_app/blob/main/data/alldata.RData?raw=true"))

data_mun <- data_mun %>% mutate(date = as.Date(date, origin = "2020-02-25"))

paises_lista <- sort(unique(mortes_casos_100k %>% dplyr::select(country))$country) # lista de países para UI
paises_lista_100 <- sort(unique(mortes_casos_100k %>% dplyr::select(country))$country)
paises_lista_50 <- sort(unique(growth_newcases$country))
ufs_lista <- sort(unique(ufs_data$UF)) # lista de UFs para UI

last_date <- HTML(paste0("<b><font color=\"#696969\"><font size=\"2\">Última atualização: ", 
                format(as.Date(max(mortes_casos_100k$time)), "%d-%m-%Y"),
                "</font></font></b>"))


############## UI ##############
ui <- fluidPage(
    
    # Application title
    titlePanel(fluidRow(
        column(12, "COVID-19 - Estatísticas Nacionais e Internacionais"),
            column(12,  last_date))),
    
    #abas para as diversas páginas
    navbarPage( '', id = "selected_menu",
                
                ## . Menus para Países ----
                navbarMenu("Países",
                           
                           # ... Menu 1, Aba 1 ----
                           tabPanel('Casos',
                                    
                                    sidebarLayout(
                                        sidebarPanel(
                                            h4(HTML(paste0("<b>", "Casos confirmados:", "</b>"))), 
                                            h5(HTML(paste0("<b>", "No mundo:", "</b>"))), 
                                            htmlOutput("total_mundo1"),
                                            htmlOutput("total_mundo1_lastday"),
                                            h5(HTML(paste0("<b>", "No Brasil:", "</b>"))),
                                            htmlOutput("total_br1"),
                                            htmlOutput("total_br1_lastday"),
                                            hr(),
                                            selectInput("country", "Selecione os países:", paises_lista_100, 
                                                        selected = c("Brasil", "Estados Unidos", "Itália", "Japão", "Reino Unido",
                                                                     "Chile", "Alemanha", "Coreia do Sul"),
                                                        multiple = TRUE),
                                            
                                            radioButtons("eixoy1", "Escolha a variável do eixo vertical", 
                                                         choices = c("Casos", "Casos por 100 mil habitantes"), selected = "Casos"),
                                            radioButtons("escala1", "Escolha a escala do gráfico", 
                                                         choices = c("Logarítmica", "Linear"), selected = "Logarítmica"),
                                            
                                            # Mostrar painel se eixoy1 = Casos
                                            conditionalPanel(
                                                condition = "input.eixoy1 == 'Casos'",
                                                
                                                radioButtons("eixo_paises", "Escolha o eixo horizontal", 
                                                             choices = c("Data", "Dias desde o primeiro caso"), 
                                                             selected = "Data"),
                                                
                                            ), # end conditional panel
                                            
                                            # Mostrar painel alternativo se eixoy1 = Casos por 100 mil habitantes
                                            conditionalPanel(
                                                condition = "input.eixoy1 == 'Casos por 100 mil habitantes'",
                                                
                                                radioButtons("eixo_paises_alt", "Escolha o eixo horizontal", 
                                                             choices = c("Data", "Dias desde 0,1 casos por 100 mil hab."), 
                                                             selected = "Data"),
                                                
                                            ), # end conditional panel 2
                                            
                                            h5("Ao passar o mouse sobre a série de um país, ele ficará destacado. Para remover o destaque, dê dois cliques na área do gráfico."),
                                            hr(),
                                            h5(HTML("<b>Fonte dos dados:</b>")),
                                            h6("Johns Hopkins University, Center for Systems Science and Engineering (CSSE)."),
                                            width = 2
                                        ), # end sidebar panel
                                        mainPanel(
                                            plotlyOutput(outputId = 'serie1', height = 800),
                                            width = 10
                                        ) #end main panel
                                    ), # end sidebar layout
                           ), # end tab
                           
                           
                           # ... Menu 1, Aba 2 ----
                           tabPanel('Novos casos/óbitos diários e fatores de crescimento',
                                    sidebarLayout(
                                        sidebarPanel(
                                            selectInput("country3", "Selecione o país:", paises_lista_50,
                                                        selected = c("Brasil"),
                                                        multiple = FALSE),
                                            radioButtons("var4", "Escolha a variável",
                                                         choices = c("Casos confirmados", "Mortes"), selected = "Casos confirmados"),
                                            h5(HTML("<b>Notas:</b>")),
                                            h6(paste0("1: O Fator de Crescimento equivale à proporção de novos casos/óbitos em um dia em relação",
                                                      " aos novos casos/óbitos do dia anterior. Um Fator acima de 1 indica crescimento dos novos casos/óbitos,",
                                                      " ao passo que um Fator entre 0 e 1 indica decréscimo. Fatores consistentemente acima de 1",
                                                      " indicam a possibilidade de um crescimento exponencial.")),
                                            h5(HTML("<b>Fonte dos dados:</b>")),
                                            h6("Johns Hopkins University, Center for Systems Science and Engineering (CSSE)."),
                                            width = 2
                                        ), # end sidebar panel
                                        mainPanel(
                                            plotlyOutput(outputId = 'serie3_1', height = 400),
                                            plotlyOutput(outputId = 'serie3_2', height = 400),
                                            width = 10
                                        ) #end main panel
                                    ), # end sidebar layout
                           ), # end tab
                           
                           
                           # .... Menu 1, Aba 3 ----
                           tabPanel('Taxa de letalidade',
                                    
                                    sidebarLayout(
                                        sidebarPanel(
                                            h4(HTML(paste0("<b>", "Taxa de letalidade:", "</b>"))), 
                                            h5(HTML(paste0("<b>", "No mundo:", "</b>"))), 
                                            htmlOutput("cfr_mundo"),
                                            h5(HTML(paste0("<b>", "No Brasil:", "</b>"))),
                                            htmlOutput("cfr_brasil"),
                                            hr(),
                                            selectInput("country4", "Selecione os países:", c(as.character(paises_lista), "Mundo"),
                                                        selected = c("Brasil", "Mundo", "Estados Unidos", "Espanha", "Itália", "Japão", 
                                                                     "Coreia do Sul"),
                                                        multiple = TRUE),
                                            
                                            radioButtons("eixo4", "Escolha o eixo horizontal", 
                                                         choices = c("Data", "Dias desde o primeiro caso"), selected = "Data"),
                                            h5("Ao passar o mouse sobre a série de um país, ele ficará destacado. Para remover o destaque, dê dois cliques na área do gráfico."),
                                            hr(),
                                            h5(HTML("<b>Notas:</b>")), 
                                            h6(paste0("1: A Taxa de Letalidade (ou Case Fatality Rate, CFR, em inglês), consiste na proporção de mortes",
                                                      " em relação ao total de casos diagnosticados durante certo momento no tempo. A Taxa de Letalidade",
                                                      " só pode ser considerada final ou definitiva quando todos os casos foram concluídos (com morte ou",
                                                      " recuperação). Geralmente a taxa preliminar ao longo de um surto epidêmico com rápido aumento de",
                                                      " casos diagnosticados e com resolução relativamente lenta tende a ser menor que a taxa final.")),
                                            h5(HTML("<b>Fonte dos dados:</b>")),
                                            h6("Johns Hopkins University, Center for Systems Science and Engineering (CSSE)."),
                                            width = 2
                                        ), # end sidebar panel
                                        mainPanel(
                                            plotlyOutput(outputId = 'serie4', height = 800),
                                            width = 10
                                        ) #end main panel
                                    ), # end sidebar layout
                           ), # end tab
                           
                           
                           # ... Menu 1, Aba 4 ----
                           tabPanel('Mortes',
                                    
                                    sidebarLayout(
                                        sidebarPanel(
                                            h4(HTML(paste0("<b>", "Total de mortes:", "</b>"))), 
                                            h5(HTML(paste0("<b>", "No mundo:", "</b>"))), 
                                            htmlOutput("total_mortes_mundo"),
                                            htmlOutput("total_mortes_mundo_lastday"),
                                            h5(HTML(paste0("<b>", "No Brasil:", "</b>"))),
                                            htmlOutput("total_mortes_br"),
                                            htmlOutput("total_mortes_br_lastday"),
                                            hr(),
                                            selectInput("country5", "Selecione os países:", paises_lista,
                                                        selected = c("Brasil", "Estados Unidos", "Itália", "Japão", 
                                                                     "Alemanha", "Reino Unido"),
                                                        multiple = TRUE),
                                            
                                            radioButtons("var_mortes_paises", "Escolha a variável", 
                                                         choices = c("Mortes", "Mortes por 100 mil habitantes"), selected = "Mortes"),
                                            radioButtons("escala_mortes", "Escolha a escala do gráfico", 
                                                         choices = c("Logarítmica", "Linear"), selected = "Logarítmica"),
                                            
                                            # Mostrar painel se var_mortes_paises = Mortes
                                            conditionalPanel(
                                                condition = "input.var_mortes_paises == 'Mortes'",
                                                
                                                radioButtons("eixo_mortes", "Escolha o eixo horizontal", 
                                                             choices = c("Data", "Dias desde a primeira morte"), selected = "Data")
                                                
                                            ), # end conditional panel
                                            
                                            # Mostrar painel alternativo se var_mortes_paises = Mortes por 100 mil habitantes
                                            conditionalPanel(
                                                condition = "input.var_mortes_paises == 'Mortes por 100 mil habitantes'",
                                                
                                                radioButtons("eixo_mortes_alt", "Escolha o eixo horizontal", 
                                                             choices = c("Data", "Dias desde 0,1 mortes por 100 mil hab."), 
                                                             selected = "Data")
                                                
                                            ), # end conditional panel 2
                                            
                                            h5("Ao passar o mouse sobre a série de um país, ele ficará destacado. Para remover o destaque, dê dois cliques na área do gráfico."),
                                            hr(),
                                            h5(HTML("<b>Fonte dos dados:</b>")),
                                            h6("Johns Hopkins University, Center for Systems Science and Engineering (CSSE)."),
                                            width = 2
                                        ), # end sidebar panel
                                        mainPanel(
                                            plotlyOutput(outputId = 'serie_mortes', height = 800),
                                            width = 10
                                        ) #end main panel
                                    ), # end sidebar layout
                           ) # end tab
                           
                           
                ), # End Menu dos Países
                
                
                # . Menus para Estados ----
                navbarMenu("Estados do Brasil",
                           
                           # ... Menu 2, Aba 1 ----
                           tabPanel('Casos',
                                    
                                    sidebarLayout(
                                        sidebarPanel(
                                            h4(HTML(paste0("<b>", "Casos confirmados:", "</b>"))), 
                                            h5(HTML(paste0("<b>", "No mundo:", "</b>"))), 
                                            htmlOutput("total_mundo1_uf"),
                                            htmlOutput("total_mundo1_lastday_uf"),
                                            h5(HTML(paste0("<b>", "No Brasil:", "</b>"))),
                                            htmlOutput("total_br1_uf"),
                                            htmlOutput("total_br1_lastday_uf"),
                                            hr(),
                                            
                                            selectInput("ufs1", "Selecione os estados:", ufs_lista, 
                                                        selected = c("São Paulo", "Santa Catarina", "Rio de Janeiro", "Distrito Federal"),
                                                        multiple = TRUE),
                                            
                                            radioButtons("escala_casos21", "Escolha a variável do eixo vertical", 
                                                         choices = c("Casos", "Casos por 100 mil habitantes"), 
                                                         selected = "Casos"),
                                            radioButtons("escala5", "Escolha a escala do gráfico", 
                                                         choices = c("Logarítmica", "Linear"), selected = "Logarítmica"),
                                            
                                            
                                            # Mostrar painel se escala_casos21 = Casos
                                            conditionalPanel(
                                                condition = "input.escala_casos21 == 'Casos'",
                                                
                                                radioButtons("eixo5", "Escolha o eixo horizontal", 
                                                             choices = c("Data", "Dias desde o primeiro caso"), 
                                                             selected = "Data"),
                                                
                                            ), # end conditional panel
                                            
                                            # Mostrar painel alternativo se escala_casos21 = Casos por 100 mil habitantes
                                            conditionalPanel(
                                                condition = "input.escala_casos21 == 'Casos por 100 mil habitantes'",
                                                
                                                radioButtons("eixo5_alt", "Escolha o eixo horizontal", 
                                                             choices = c("Data", "Dias desde 0,1 casos por 100 mil hab."), 
                                                             selected = "Data"),
                                                
                                            ), # end conditional panel 2
                                            
                                            h5("Ao passar o mouse sobre a série de um estado, ele ficará destacado. Para remover o destaque, dê dois cliques na área do gráfico."),
                                            hr(),
                                            h5(HTML("<b>Fontes dos dados:</b>")),
                                            h6(HTML(paste0("<a href=\"https://brasil.io/home/\" target=\"_blank\">", "Brasil.io", "</a>"))),
                                            h6(HTML(paste0("<a href=\"https://github.com/wcota/covid19br/\" target=\"_blank\">", "Wesley Cota", "</a>"))),
                                            width = 2
                                        ), # end sidebar panel
                                        mainPanel(
                                            plotlyOutput(outputId = 'serie5', height = 800),
                                            width = 10
                                        ), #end main panel
                                    ) # end sidebar layout
                           ), # end tab
                           
                           
                           # ... Menu 2, Aba 2 ----
                           tabPanel('Proporção do total de casos e mortes',
                                    
                                    sidebarLayout(
                                        sidebarPanel(
                                            selectInput("ufs2", "Selecione os estados:", ufs_lista, 
                                                        selected = c("São Paulo", "Santa Catarina", "Rio de Janeiro", 
                                                                     "Distrito Federal"),
                                                        multiple = TRUE),
                                            
                                            selectInput("tipo_uf", "Escolha o gráfico a visualizar:", 
                                                        choices = c("Participação no total de mortes", "Participação no total de casos"),
                                                        selected = "Participação no total de casos"),
                                            h5("Ao passar o mouse sobre a série de um estado, ele ficará destacado. Para remover o destaque, dê dois cliques na área do gráfico."),
                                            hr(),
                                            h5(HTML("<b>Fontes dos dados:</b>")),
                                            h6(HTML(paste0("<a href=\"https://brasil.io/home/\" target=\"_blank\">", "Brasil.io", "</a>"))),
                                            h6(HTML(paste0("<a href=\"https://github.com/wcota/covid19br/\" target=\"_blank\">", "Wesley Cota", "</a>"))),
                                            width = 2
                                        ), # end sidebar panel
                                        mainPanel(
                                            plotlyOutput(outputId = 'serie6', height = 800),
                                            width = 10
                                        ), #end main panel
                                    ) # end sidebar layout
                           ), # end tab
                           
                           
                           # ... Menu 2, Aba 3 ----
                           tabPanel('Taxa de letalidade',
                                    
                                    sidebarLayout(
                                        sidebarPanel(
                                            h4(HTML(paste0("<b>", "Taxa de letalidade:", "</b>"))), 
                                            h5(HTML(paste0("<b>", "No mundo:", "</b>"))), 
                                            htmlOutput("cfr_mundo_uf"),
                                            h5(HTML(paste0("<b>", "No Brasil:", "</b>"))),
                                            htmlOutput("cfr_brasil_uf"),
                                            hr(),
                                            
                                            selectInput("ufs4", "Selecione os estados:", c(ufs_lista, "Brasil"),
                                                        selected = c("Santa Catarina", "São Paulo", "Rio de Janeiro", "Distrito Federal", 
                                                                     "Amazonas"),
                                                        multiple = TRUE),
                                            
                                            radioButtons("eixo_cfr_ufs", "Escolha o eixo horizontal", 
                                                         choices = c("Data", "Dias desde o primeiro caso"), selected = "Data"),
                                            h5(HTML("<b>Notas:</b>")), 
                                            h6(paste0("1: A Taxa de Letalidade (ou Case Fatality Rate, CFR, em inglês), consiste na proporção de mortes",
                                                      " em relação ao total de casos diagnosticados durante certo momento no tempo. A Taxa de Letalidade",
                                                      " só pode ser considerada final ou definitiva quando todos os casos foram concluídos (com morte ou",
                                                      " recuperação). Geralmente a taxa preliminar ao longo de um surto epidêmico com rápido aumento de",
                                                      " casos diagnosticados e com resolução relativamente lenta tende a ser menor que a taxa final.")),
                                            h5(HTML("<b>Fontes dos dados:</b>")),
                                            h6(HTML(paste0("<a href=\"https://brasil.io/home/\" target=\"_blank\">", "Brasil.io", "</a>"))),
                                            h6(HTML(paste0("<a href=\"https://github.com/wcota/covid19br/\" target=\"_blank\">", "Wesley Cota", "</a>"))),
                                            width = 2
                                        ), # end sidebar panel
                                        mainPanel(
                                            plotlyOutput(outputId = 'serie_cfr_ufs', height = 800),
                                            width = 10
                                        ), #end main panel
                                    ) # end sidebar layout
                           ), # end tab
                           
                           
                           # ... Menu 2, Aba 4 ----
                           tabPanel('Mortes',
                                    
                                    sidebarLayout(
                                        sidebarPanel(
                                            h4(HTML(paste0("<b>", "Total de mortes:", "</b>"))), 
                                            h5(HTML(paste0("<b>", "No mundo:", "</b>"))), 
                                            htmlOutput("total_mortes_mundo_uf"),
                                            htmlOutput("total_mortes_mundo_lastday_uf"),
                                            h5(HTML(paste0("<b>", "No Brasil:", "</b>"))),
                                            htmlOutput("total_mortes_br_uf"),
                                            htmlOutput("total_mortes_br_lastday_uf"),
                                            hr(),
                                            
                                            selectInput("ufs5", "Selecione os estados:", ufs_lista,
                                                        selected = c("Santa Catarina", "São Paulo", "Rio de Janeiro", "Amazonas",
                                                                     "Distrito Federal"),
                                                        multiple = TRUE),
                                            
                                            radioButtons("var_morte", "Escolha a variável", 
                                                         choices = c("Mortes", "Mortes por 100 mil habitantes"), selected = "Mortes"),
                                            
                                            radioButtons("escala_mortes_ufs", "Escolha a escala do gráfico", 
                                                         choices = c("Logarítmica", "Linear"), selected = "Logarítmica"),
                                            #),
                                            
                                            # Mostrar painel se var_morte = Mortes
                                            conditionalPanel(
                                                condition = "input.var_morte == 'Mortes'",
                                                
                                                radioButtons("eixo_mortes_ufs", "Escolha o eixo horizontal", 
                                                             choices = c("Data", "Dias desde a primeira morte"), 
                                                             selected = "Data"),
                                                
                                            ), # end conditional panel
                                            
                                            # Mostrar painel alternativo se var_morte  = Mortes por 100 mil habitantes
                                            conditionalPanel(
                                                condition = "input.var_morte == 'Mortes por 100 mil habitantes'",
                                                
                                                radioButtons("eixo_mortes_ufs_alt", "Escolha o eixo horizontal", 
                                                             choices = c("Data", "Dias desde 0,1 mortes por 100 mil hab."), 
                                                             selected = "Data"),
                                                
                                            ), # end conditional panel 2
                                            
                                            
                                            
                                            h5(HTML("<b>Fonte dos dados:</b>")),
                                            h6(HTML(paste0("<a href=\"https://brasil.io/home/\" target=\"_blank\">", "Brasil.io", "</a>"))),
                                            h6(HTML(paste0("<a href=\"https://github.com/wcota/covid19br/\" target=\"_blank\">", "Wesley Cota", "</a>"))),
                                            width = 2
                                        ), # end sidebar panel
                                        mainPanel(
                                            plotlyOutput(outputId = 'serie_mortes_ufs', height = 800),
                                            width = 10
                                        ), #end main panel
                                    ) # end sidebar layout
                           ), # end tab
                           
                           
                           # ... Menu 2, Aba 5 ----
                           tabPanel('Novos casos/óbitos diários e fatores de crescimento',
                                    sidebarLayout(
                                        sidebarPanel(
                                            selectInput("ufs6", "Selecione um Estado:", ufs_lista, 
                                                        selected = c("Santa Catarina"),
                                                        multiple = FALSE),
                                            radioButtons("var_uf6", "Escolha a variável", 
                                                         choices = c("Casos confirmados", "Mortes"), selected = "Casos confirmados"),
                                            h5(HTML("<b>Notas:</b>")), 
                                            h6(paste0("1: O Fator de Crescimento equivale à proporção de novos casos/óbitos em um dia em relação",
                                                      " aos novos casos/óbitos do dia anterior. Um Fator acima de 1 indica crescimento dos novos casos/óbitos,",
                                                      " ao passo que um Fator entre 0 e 1 indica decréscimo. Fatores consistentemente acima de 1",
                                                      " indicam a possibilidade de um crescimento exponencial.")),
                                            h5(HTML("<b>Fonte dos dados:</b>")),
                                            h6(HTML(paste0("<a href=\"https://brasil.io/home/\" target=\"_blank\">", "Brasil.io", "</a>"))),
                                            h6(HTML(paste0("<a href=\"https://github.com/wcota/covid19br/\" target=\"_blank\">", "Wesley Cota", "</a>"))),
                                            width = 2
                                        ), # end sidebar panel
                                        mainPanel(
                                            plotlyOutput(outputId = 'serie_uf6_1', height = 400),
                                            plotlyOutput(outputId = 'serie_uf6_2', height = 400),
                                            width = 10
                                        ), #end main panel
                                    ) # end sidebar layout
                           ), # end tab
                           
                           # ... Menu 2, Aba 6 ----
                           tabPanel('Tempo para duplicação de casos e mortes',
                                    sidebarLayout(
                                        sidebarPanel(
                                            selectInput("ufs7", "Selecione um Estado:", ufs_lista, 
                                                        selected = c("Santa Catarina"),
                                                        multiple = FALSE),
                                            h5(HTML("<b>Notas:</b>")), 
                                            h6(paste0("1: As barras verticais indicam quando houve duplicação das ocorrências",
                                                      " (casos confirmados ou mortes) em comparação com o valor da última duplicação.",
                                                      " Barras mais distantes entre si indicam menor velocidade no aumento das ocorrências,",
                                                      " ao passo que barras mais próximas significam que as ocorrências estão aumentando em ritmo mais acelerado.")),
                                            h5(HTML("<b>Fonte dos dados:</b>")),
                                            h6(HTML(paste0("<a href=\"https://brasil.io/home/\" target=\"_blank\">", "Brasil.io", "</a>"))),
                                            h6(HTML(paste0("<a href=\"https://github.com/wcota/covid19br/\" target=\"_blank\">", "Wesley Cota", "</a>"))),
                                            width = 2
                                        ), # end sidebar panel
                                        mainPanel(
                                            plotlyOutput(outputId = 'serie_uf7_1', height = 600),
                                            plotlyOutput(outputId = 'serie_uf7_2', height = 600),
                                            width = 10
                                        ), #end main panel
                                    ) # end sidebar layout
                           ) # end tab
                           
                ), # end Menu UFs
                
                
                
                # . Menu para Municípios ----
                navbarMenu("Municípios do Brasil",
                           
                           # ... Menu 3, Aba 1 ----
                           tabPanel("Mapas de casos e mortes nos municípios",
                                    sidebarLayout(
                                        sidebarPanel(
                                            selectInput("mun_var1", "Selecione uma variável:", c("Casos confirmados", "Mortes"), 
                                                        selected = c("Casos confirmados"),
                                                        multiple = FALSE),
                                            radioButtons("escala_mun1", "Selecione a escala da variável", 
                                                         choices = c("Valor absoluto", "Valor por 100 mil habitantes"), 
                                                         selected = "Valor absoluto"),
                                            dateInput("map_mun_date",
                                                      label = h5("Selecione a data:"),
                                                      min = as.Date(min(data_mun$date),"%Y-%m-%d"),
                                                      max = as.Date(max(data_mun$date),"%Y-%m-%d"),
                                                      value = as.Date(max(data_mun$date)),
                                                      format = "dd-mm-yyyy", 
                                                      language = "pt-BR"),
                                            h5(HTML("<b>Notas:</b>")), 
                                            h6(paste0("1: Os dados mais recentes nem sempre estão disponíveis para todos os municípios,",
                                                      " o que pode fazer com que municípios sem tais informações não apareçam no mapa",
                                                      " na data mais atual, mas sejam visualizados em datas anteriores.")),
                                            h5(HTML("<b>Fonte dos dados:</b>")),
                                            h6(HTML(paste0("<a href=\"https://brasil.io/home/\" target=\"_blank\">", "Brasil.io", "</a>"))),
                                            h6(HTML(paste0("<a href=\"https://github.com/wcota/covid19br/\" target=\"_blank\">", "Wesley Cota", "</a>"))),
                                            width = 2
                                        ), # end sidebar panel
                                        mainPanel(
                                            leafletOutput(outputId = 'mapa_mun1', height = 800),
                                            width = 10
                                        ), #end main panel
                                    ) # end sidebar layout
                           ) # end tab
                           
                ), # end Menu Municípios
                
                
                # . Menu "Sobre" ----
                tabPanel("Sobre",
                         fluidPage(
                             br(),
                             includeMarkdown("About.Rmd")
                    ) # end fluid page
                ) # end tab
                
            ) # end navbarPage
) # end fluidPage






############# SERVER ##################
server <- function(input, output, session) {

    # Menu 1, Aba 1: Países - Casos ----
    
    observeEvent(input$country,{
        
    })
    
    observe({
        
        selected_countries <-  mortes_casos_100k %>% filter(country %in% input$country) # recorte países
        
        output$serie1 <- renderPlotly({
            
            p <- highlight_key(selected_countries, key = ~country)
            
            plot1 <- highlight(plot_ly(p,
                            x = ~time, y = ~confirm, color = ~country,
                            type = 'scatter', mode = 'lines',
                            line = list(width = 2),
                            hoverinfo = 'text',
                            text = ~paste("<b>País: ", country, '</b></br></br>', 
                                          "Data: ", format(time, "%d-%b-%y"), '</br>',
                                          "Casos: ", format(confirm, big.mark = " "))),
                on = "plotly_hover", off = "plotly_doubleclick")  %>%
                layout(showlegend = FALSE) %>%
                layout(title = list(text = 'Países - Número de casos confirmados', font = list(family = "Open Sans", size = 18)),
                       xaxis = list(title = "Data", dtick = "M1", tickformat="%b<br>%Y"),
                       yaxis = list(title = 'Total de casos confirmados (escala logarítmica)', type = "log")) %>%
                config(locale = 'pt-BR')
            
            
            if (input$escala1 == 'Linear' && input$eixoy1 == "Casos" && input$eixo_paises == "Dias desde o primeiro caso") {
                
                plot1 <- highlight(plot_ly(p,
                              x = ~days_since_1, y = ~confirm, color = ~country,
                              type = 'scatter', mode = 'lines',
                              line = list(width = 2),
                              hoverinfo = 'text',
                              text = ~paste("<b>País:</b> ", country, '</br></br>', 
                                            "Dias desde 1º caso: ", days_since_1, '</br>',
                                            "Casos: ", format(confirm, big.mark = " "))),
                              on = "plotly_hover", off = "plotly_doubleclick")  %>%
                    layout(showlegend = FALSE) %>%
                    layout(title = list(text = 'Países - Número de casos confirmados', font = list(family = "Open Sans", size = 18)),
                           xaxis = list(title = "Dias desde o primeiro caso"),
                           yaxis = list(title = 'Total de casos confirmados')) %>%
                    config(locale = 'pt-BR')
                
            } # end if 
            
            else if (input$escala1 == 'Linear' && input$eixoy1 == "Casos por 100 mil habitantes" && input$eixo_paises_alt == "Dias desde 0,1 casos por 100 mil hab.") {
                
                plot1 <- highlight(plot_ly(p,
                                           x = ~days_since_pointone_cases100k, y = ~cases_per_100k, color = ~country,
                                           type = 'scatter', mode = 'lines',
                                           line = list(width = 2),
                                           hoverinfo = 'text',
                                           text = ~paste("<b>País: ", country, '</b></br></br>', 
                                                         "Dias desde 0,1 casos/100 mil: ", days_since_pointone_cases100k, '</br>',
                                                         "Casos/100 mil: ", format(cases_per_100k, big.mark = " "))),
                                   on = "plotly_hover", off = "plotly_doubleclick")  %>%
                    layout(showlegend = FALSE) %>%
                    layout(title = list(text = 'Países - Casos por 100 mil habitantes', font = list(family = "Open Sans", size = 18)),
                           xaxis = list(title = "Dias desde 0,1 casos por 100 mil habitantes"),
                           yaxis = list(title = 'Total de casos confirmados')) %>%
                    config(locale = 'pt-BR')
                
            } # end else if1
            
            else if (input$escala1 == 'Logarítmica' && input$eixoy1 == "Casos por 100 mil habitantes" && input$eixo_paises_alt == "Dias desde 0,1 casos por 100 mil hab.") {
                
                plot1 <- highlight(plot_ly(p,
                                           x = ~days_since_pointone_cases100k, y = ~cases_per_100k, color = ~country,
                                           type = 'scatter', mode = 'lines',
                                           line = list(width = 2),
                                           hoverinfo = 'text',
                                           text = ~paste("<b>País: ", country, '</b></br></br>', 
                                                         "Dias desde 0,1 casos/100 mil: ", days_since_pointone_cases100k, '</br>',
                                                         "Casos/100 mil: ", format(cases_per_100k, big.mark = " "))),
                                   on = "plotly_hover", off = "plotly_doubleclick")  %>%
                    layout(showlegend = FALSE) %>%
                    layout(title = list(text = 'Países - Casos por 100 mil habitantes', font = list(family = "Open Sans", size = 18)),
                           xaxis = list(title = "Dias desde 0,1 casos por 100 mil habitantes"),
                           yaxis = list(title = 'Total de casos confirmados (escala logarítmica)', type = "log")) %>%
                    config(locale = 'pt-BR')
                
            } # end else if2
        
            

            else if (input$escala1 == 'Logarítmica' && input$eixoy1 == "Casos" && input$eixo_paises == "Dias desde o primeiro caso") {
            plot1 <- highlight(plot_ly(p,
                                       x = ~days_since_1, y = ~confirm, color = ~country,
                                       type = 'scatter', mode = 'lines',
                                       line = list(width = 2),
                                       hoverinfo = 'text',
                                       text = ~paste("<b>País: ", country, '</b></br></br>', 
                                                     "Dias desde 1º caso: ", days_since_1, '</br>',
                                                     "Casos: ", format(confirm, big.mark = " "))),
                               on = "plotly_hover", off = "plotly_doubleclick")  %>%
                layout(showlegend = FALSE) %>%
                layout(title = list(text = 'Países - Número de casos confirmados', font = list(family = "Open Sans", size = 18)),
                       xaxis = list(title = "Dias desde o primeiro caso"),
                       yaxis = list(title = 'Total de casos confirmados (escala logarítmica)', type = "log")) %>%
                config(locale = 'pt-BR')
            
            } # end else if 3
            
            else if (input$escala1 == 'Linear' && input$eixoy1 == "Casos" && input$eixo_paises == "Data") {
                
                plot1 <- highlight(plot_ly(p,
                                      x = ~time, y = ~confirm, color = ~country,
                                      type = 'scatter', mode = 'lines',
                                      line = list(width = 2),
                                      hoverinfo = 'text',
                                      text = ~paste("<b>País: ", country, '</b></br></br>', 
                                                    "Data: ", format(time, "%d-%b-%y"), '</br>',
                                                    "Casos: ", format(confirm, big.mark = " "))),
                              on = "plotly_hover", off = "plotly_doubleclick")  %>%
                    layout(showlegend = FALSE) %>%
                    layout(title = list(text = 'Países - Número de casos confirmados', font = list(family = "Open Sans", size = 18)),
                           xaxis = list(title = "Data", dtick = "M1", tickformat="%b<br>%Y"),
                           yaxis = list(title = 'Total de casos confirmados')) %>%
                    config(locale = 'pt-BR')
                
            } # end else if 4
            
            else if (input$escala1 == 'Linear' && input$eixoy1 == "Casos por 100 mil habitantes" && input$eixo_paises_alt == "Data") {
                
                plot1 <- highlight(plot_ly(p,
                                           x = ~time, y = ~cases_per_100k, color = ~country,
                                           type = 'scatter', mode = 'lines',
                                           line = list(width = 2),
                                           hoverinfo = 'text',
                                           text = ~paste("<b>País: ", country, '</b></br></br>', 
                                                         "Data: ", format(time, "%d-%b-%y"), '</br>',
                                                         "Casos/100 mil: ", format(cases_per_100k, big.mark = " "))),
                                   on = "plotly_hover", off = "plotly_doubleclick")  %>%
                    layout(showlegend = FALSE) %>%
                    layout(title = list(text = 'Países - Casos confirmados por 100 mil habitantes', font = list(family = "Open Sans", size = 18)),
                           xaxis = list(title = "Data", dtick = "M1", tickformat="%b<br>%Y"),
                           yaxis = list(title = 'Casos confirmados por 100 mil habitantes')) %>%
                    config(locale = 'pt-BR')
                
            } # end else if 5
            
            else if (input$escala1 == 'Logarítmica' && input$eixoy1 == "Casos por 100 mil habitantes" && input$eixo_paises_alt == "Data") {
                
                plot1 <- highlight(plot_ly(p,
                                           x = ~time, y = ~cases_per_100k, color = ~country,
                                           type = 'scatter', mode = 'lines',
                                           line = list(width = 2),
                                           hoverinfo = 'text',
                                           text = ~paste("<b>País: ", country, '</b></br></br>', 
                                                         "Data: ", format(time, "%d-%b-%y"), '</br>',
                                                         "Casos/100 mil: ", format(cases_per_100k, big.mark = " "))),
                                   on = "plotly_hover", off = "plotly_doubleclick")  %>%
                    layout(showlegend = FALSE) %>%
                    layout(title = list(text = 'Países - Casos confirmados por 100 mil habitantes', font = list(family = "Open Sans", size = 18)),
                           xaxis = list(title = "Data", dtick = "M1", tickformat="%b<br>%Y"),
                           yaxis = list(title = 'Casos confirmados por 100 mil habitantes (escala logarítmica)', type = "log")) %>%
                    config(locale = 'pt-BR')
                
            } # end else if 6
            
            
            plot1
        })
        
        
        ### Casos totais, Brasil e mundo
        last_data <- growth_newcases %>% 
            group_by(country) %>%
            mutate(last_day_cases = confirm - lag(confirm)) %>%
            top_n(1, time)
        
        # Mundo total:
        output$total_mundo1 <- renderText({
            HTML(paste0("<b><font color=\"#FF0000\"><font size=\"5\">", 
                        formatC(as.numeric(sum(last_data$confirm)), 
                                big.mark = ".", decimal.mark = ",", format = "d"),
                        "</font></font></b>"))
        })
        
        # Mundo último dia:
        output$total_mundo1_lastday <- renderText({
            HTML(paste0("<b><font color=\"#FF0000\"><font size=\"3\">", "+ ",
                        formatC(as.numeric(sum(last_data$last_day_cases)), 
                                big.mark = ".", decimal.mark = ",", format = "d"), 
                        "</font></font></b>",
                        "<font color=\"#000000\"><font size=\"3\">", " nas últimas 24hs", "</font></font>"))
        })
        
        
        # Brasil total:
        output$total_br1 <-  renderText({
            HTML(paste0("<b><font color=\"#FF0000\"><font size=\"5\">", 
                        formatC(as.numeric(last_data[last_data$country == "Brasil", 3]$confirm), 
                                big.mark = ".", decimal.mark = ",", format = "d"), 
                        "</font></font></b>"))
        })
        
        # Brasil último dia:
        output$total_br1_lastday <-  renderText({
            HTML(paste0("<b><font color=\"#FF0000\"><font size=\"3\">", "+ ",
                        formatC(as.numeric(last_data[last_data$country == "Brasil", 8]$last_day_cases), 
                                big.mark = ".", decimal.mark = ",", format = "d"), 
                        "</font></font></b>",
                        "<font color=\"#000000\"><font size=\"3\">", " nas últimas 24hs", "</font></font>"))
        })
        
        
        
    })
    
    
    # Menu 1, Aba 2: Países - novos casos diários e fator de crescimento ----
    
    observe({
        
        selected_country_cases <-  growth_newcases %>% filter(country %in% input$country3)
        selected_country_deaths <-  mortes_casos_100k %>% filter(country %in% input$country3)
        
        ma_pais_casos <- sma(selected_country_cases$new_cases, order = 5) # Média móvel 5 dias
        ma_pais_deaths <- sma(selected_country_deaths$new_deaths, order = 5) # Média móvel 5 dias
        
        ma_pais_casos_7 <- sma(selected_country_cases$new_cases, order = 7) # Média móvel 7 dias
        ma_pais_deaths_7 <- sma(selected_country_deaths$new_deaths, order = 7) # Média móvel 7 dias
        
        selected_country_cases$MA5 <- ma_pais_casos$fitted
        selected_country_deaths$MA5 <- ma_pais_deaths$fitted
        
        selected_country_cases$MA7 <- ma_pais_casos_7$fitted
        selected_country_deaths$MA7 <- ma_pais_deaths_7$fitted
        
        pais_novcas <- highlight_key(selected_country_cases, key = ~country)
        pais_novmort <- highlight_key(selected_country_deaths, key = ~country)
        
        # Novos casos
        output$serie3_1 <- renderPlotly({
            
            plot_new_cd <- plot_ly(pais_novcas,
                                   x = ~time, y = ~new_cases, color = I("gray50"),
                                   type = 'bar',
                                   
                                   showlegend = F,
                                   hoverinfo = 'text',
                                   text = ~paste("<b>País: ", country, '</b></br></br>', 
                                                 "Data: ", format(time, "%d-%b-%y"), '</br>',
                                                 "Novos casos: ", format(new_cases, big.mark = " "), '</br>',
                                                 "Média móvel (5 dias): ", format(round(MA5, 1), big.mark = " ", decimal.mark = ","), '</br>',
                                                 "Média móvel (7 dias): ", format(round(MA7, 1), big.mark = " ", decimal.mark = ",")))  %>%
                layout(showlegend = T, legend = list(x = 0.1, y = 0.9)) %>%
                add_lines(x=~time, y=~MA5, name="Média Móvel - 5 dias", color = I("dodgerblue2"), showlegend = T) %>%
                add_lines(x=~time, y=~MA7, name="Média Móvel - 7 dias", color = I("darkorange2"), showlegend = T) %>%
                layout(title = list(text = paste0("Novos casos diários e médias móveis - ", input$country3), 
                                    font = list(family = "Open Sans", size = 18)),
                       xaxis = list(title = "Data", dtick = "M1", tickformat="%b<br>%Y"),
                       yaxis = list(title = 'Número de novos casos diários')) %>%
                config(locale = 'pt-BR')
            
            
            if (input$var4 == "Mortes") {
                
                plot_new_cd <- plot_ly(pais_novmort,
                                       x = ~time, y = ~new_deaths, color = I("gray50"),
                                       type = 'bar',
                                       
                                       showlegend = F,
                                       hoverinfo = 'text',
                                       text = ~paste("<b>País: ", country, '</b></br></br>', 
                                                     "Data: ", format(time, "%d-%b-%y"), '</br>',
                                                     "Novos casos: ", format(new_deaths, big.mark = " "), '</br>',
                                                     "Média móvel (5 dias): ", format(round(MA5, 1), big.mark = " ", decimal.mark = ","), '</br>',
                                                     "Média móvel (7 dias): ", format(round(MA7, 1), big.mark = " ", decimal.mark = ",")))  %>%
                    layout(showlegend = T, legend = list(x = 0.1, y = 0.9)) %>%
                    add_lines(x=~time, y=~MA5, name="Média Móvel - 5 dias", color = I("dodgerblue2"), showlegend = T) %>%
                    add_lines(x=~time, y=~MA7, name="Média Móvel - 7 dias", color = I("darkorange2"), showlegend = T) %>%
                    layout(title = list(text = paste0("Novos óbitos diários e médias móveis - ", input$country3), 
                                        font = list(family = "Open Sans", size = 18)),
                           xaxis = list(title = "Data", dtick = "M1", tickformat="%b<br>%Y"),
                           yaxis = list(title = 'Número de óbitos casos diários')) %>%
                    config(locale = 'pt-BR') 
                
            }
            
            plot_new_cd
            
        }) # end renderPlotly 3_1
        
        # Fator de crescimento   
        output$serie3_2 <- renderPlotly({
            
            # Função para linha horizontal
            hline <- function(y = 0, color = "orangered") {
                list(
                    type = "line",
                    x0 = 0,
                    x1 = 1,
                    xref = "paper",
                    y0 = y,
                    y1 = y,
                    line = list(color = color, dash = "dot",width = 3)
                )
            }
            
            plot_factor <-  highlight(plot_ly(pais_novcas,
                                              x = ~time, y = ~growth, color = I("black"),
                                              type = 'scatter', mode = 'lines',
                                              line = list(width = 1),
                                              hoverinfo = 'text',
                                              text = ~paste("<b>País: ", country, '</b></br></br>',
                                                            "Data: ", format(time, "%d-%b-%y"), '</br>',
                                                            "Fator: ", growth)),
                                      on = "plotly_hover", off = "plotly_doubleclick")  %>%
                layout(showlegend = FALSE) %>%
                layout(title = list(text = paste0("Fator de crescimento dos casos - ", input$country3),
                                    font = list(family = "Open Sans", size = 18)),
                       xaxis = list(title = "Data", dtick = "M1", tickformat="%b<br>%Y"),
                       yaxis = list(title = 'Fator de crescimento', zeroline = FALSE,
                                    range = c(-1, 10), tickvals = list(-50, -40, -30, -20, -10, 0, 1, 5, 10, 15, 20, 30, 40, 50, 60)),
                       shapes = hline(1)) %>%
                config(locale = 'pt-BR')
            
            if (input$var4 == "Mortes") {
                
                plot_factor <-  highlight(plot_ly(pais_novmort,
                                                  x = ~time, y = ~growth_deaths, color = I("black"),
                                                  type = 'scatter', mode = 'lines',
                                                  line = list(width = 1),
                                                  hoverinfo = 'text',
                                                  text = ~paste("<b>País: ", country, '</b></br></br>',
                                                                "Data: ", format(time, "%d-%b-%y"), '</br>',
                                                                "Fator: ", growth_deaths)),
                                          on = "plotly_hover", off = "plotly_doubleclick")  %>%
                    layout(showlegend = FALSE) %>%
                    layout(title = list(text = paste0("Fator de crescimento dos óbitos - ", input$country3),
                                        font = list(family = "Open Sans", size = 18)),
                           xaxis = list(title = "Data", dtick = "M1", tickformat="%b<br>%Y"),
                           yaxis = list(title = 'Fator de crescimento', zeroline = FALSE,
                                        range = c(-1, 8), tickvals = list(-50, -40, -30, -20, -10, 0, 1, 5, 10, 15, 20, 30, 40, 50, 60)),
                           shapes = hline(1)) %>%
                    config(locale = 'pt-BR')
                
            }
            
            plot_factor
            
            
        }) # end renderPlotly 3_2
        
    }) #end observe
    
    
    # Menu 1, Aba 3: Países - Taxas de letalitade ----
    observeEvent(input$country4,{
        
    })
    
    observe({ 
        
        selected_country4 <-  mortes_casos_100k %>% filter(country %in% input$country4)    
        
        output$serie4 <- renderPlotly({ 
           
            p_cfr <- highlight_key(selected_country4, key = ~country)
            
            cfr_plot <- highlight(plot_ly(p_cfr,
                                          x = ~time, y = ~cfr, color = ~country,
                                          type = 'scatter', mode = 'lines',
                                          line = list(width = 2),
                                          hoverinfo = 'text',
                                          text = ~paste("<b>País: ", country, '</b></br></br>', 
                                                        "Data: ", format(time, "%d-%b-%y"), '</br>',
                                                        "Letalidade (CFR): ", cfr)),
                                  on = "plotly_hover", off = "plotly_doubleclick")  %>%
                layout(showlegend = FALSE) %>%
                layout(title = list(text = 'Países - Taxas de letalidade', font = list(family = "Open Sans", size = 18)),
                       xaxis = list(title = "Data", dtick = "M1", tickformat="%b<br>%Y"),
                       yaxis = list(title = 'Taxa de letalidade (CFR) - %')) %>%
                config(locale = 'pt-BR')
                
                
            if (input$eixo4 == 'Dias desde o primeiro caso') {
                
                cfr_plot <- highlight(plot_ly(p_cfr,
                                              x = ~days_since_1, y = ~cfr, color = ~country,
                                              type = 'scatter', mode = 'lines',
                                              line = list(width = 2),
                                              hoverinfo = 'text',
                                              text = ~paste("<b>País: ", country, '</b></br></br>', 
                                                            "Dias desde 1º caso: ", days_since_1, '</br>',
                                                            "Letalidade (CFR): ", cfr)),
                                      on = "plotly_hover", off = "plotly_doubleclick")  %>%
                    layout(showlegend = FALSE) %>%
                    layout(title = list(text = 'Países - Taxas de letalidade', font = list(family = "Open Sans", size = 18)),
                           xaxis = list(title = "Dias desde o primeiro caso"),
                           yaxis = list(title = 'Taxa de letalidade (CFR) - %')) %>%
                    config(locale = 'pt-BR')
                
            } # end if
            
            cfr_plot
        }) # end renderPlotly
        
        ### CFR no Brasil e no mundo
        last_cfr <- mortes_casos_100k %>% 
            filter(!is.na(cfr)) %>%
            group_by(country) %>%
            top_n(1, time)
        
                # CFR Mundo:
                output$cfr_mundo <- renderText({
                    HTML(paste0("<b><font color=\"#FF0000\"><font size=\"6\">", 
                                formatC(as.numeric(last_cfr[last_cfr$country == "Mundo", ]$cfr), 
                                        big.mark = ".", decimal.mark = ",", format = "f", digits = 2),
                                "</font></font></b>"))
                })
                
                # CFR Brasil:
                output$cfr_brasil <-  renderText({
                    HTML(paste0("<b><font color=\"#FF0000\"><font size=\"6\">", 
                                formatC(as.numeric(last_cfr[last_cfr$country == "Brasil", ]$cfr), 
                                        big.mark = ".", decimal.mark = ",", format = "f", digits = 2), 
                                "</font></font></b>"))
                })
                
        
        
    }) # end observe
    
    
    # Menu 1, Aba 4: Países - Mortes ----
    
    observeEvent(input$country5,{
        
    })
    
    observe({ 
        
        selected_country5 <-  mortes_casos_100k %>% filter(country %in% input$country5)    
        
        output$serie_mortes <- renderPlotly({ 
            
            p_d <- highlight_key(selected_country5, key = ~country)
            
            # Mortes, escala log e x = data
            deaths_plot <- highlight(plot_ly(p_d,
                                             x = ~time, y = ~deaths, color = ~country,
                                             type = 'scatter', mode = 'lines',
                                             line = list(width = 2),
                                             hoverinfo = 'text',
                                             text = ~paste("<b>País: ", country, '</b></br></br>', 
                                                           "Data: ", format(time, "%d-%b-%y"), '</br>',
                                                           "Mortes: ", format(deaths, big.mark = " "))),
                                     on = "plotly_hover", off = "plotly_doubleclick")  %>%
                layout(showlegend = FALSE) %>%
                layout(title = list(text = 'Países - Número de mortes', font = list(family = "Open Sans", size = 18)),
                       xaxis = list(title = "Data", dtick = "M1", tickformat="%b<br>%Y"),
                       yaxis = list(title = 'Número de mortes (escala logarítmica)', type = "log")) %>%
                config(locale = 'pt-BR')
                
                
            # Mortes, escala linear e x = dias desde 1ª morte
            if (input$var_mortes_paises == 'Mortes' &&
                input$escala_mortes == 'Linear' && input$eixo_mortes == 'Dias desde a primeira morte') {
                
                deaths_plot <- highlight(plot_ly(p_d,
                                                 x = ~days_since_1, y = ~deaths, color = ~country,
                                                 type = 'scatter', mode = 'lines',
                                                 line = list(width = 2),
                                                 hoverinfo = 'text',
                                                 text = ~paste("<b>País: ", country, '</b></br></br>', 
                                                               "Dias desde 1ª morte: ", days_since_1, '</br>',
                                                               "Mortes: ", format(deaths, big.mark = " "))),
                                         on = "plotly_hover", off = "plotly_doubleclick")  %>%
                    layout(showlegend = FALSE) %>%
                    layout(title = list(text = 'Países - Número de mortes', font = list(family = "Open Sans", size = 18)),
                           xaxis = list(title = "Dias desde a primeira morte"),
                           yaxis = list(title = 'Número de mortes')) %>%
                    config(locale = 'pt-BR')
                
            } # end if 1
            
            # Mortes, escala log e x = dias desde 1ª morte
            else if (input$var_mortes_paises == 'Mortes' &&
                     input$escala_mortes == 'Logarítmica' && input$eixo_mortes == 'Dias desde a primeira morte') {
                
                deaths_plot <- highlight(plot_ly(p_d,
                                             x = ~days_since_1, y = ~deaths, color = ~country,
                                             type = 'scatter', mode = 'lines',
                                             line = list(width = 2),
                                             hoverinfo = 'text',
                                             text = ~paste("<b>País: ", country, '</b></br></br>', 
                                                           "Dias desde 1ª morte: ", days_since_1, '</br>',
                                                           "Mortes: ", format(deaths, big.mark = " "))),
                                     on = "plotly_hover", off = "plotly_doubleclick")  %>%
                    layout(showlegend = FALSE) %>%
                    layout(title = list(text = 'Países - Número de mortes', font = list(family = "Open Sans", size = 18)),
                           xaxis = list(title = "Dias desde a primeira morte"),
                           yaxis = list(title = 'Número de mortes (escala logarítmica)',  type = "log")) %>%
                    config(locale = 'pt-BR')
                
            } # end else if 1
            
            
            # Mortes, escala linear e x = data
            else if (input$var_mortes_paises == 'Mortes' &&
                     input$escala_mortes == 'Linear' && input$eixo_mortes == 'Data') {
                
                deaths_plot <- highlight(plot_ly(p_d,
                                                 x = ~time, y = ~deaths, color = ~country,
                                                 type = 'scatter', mode = 'lines',
                                                 line = list(width = 2),
                                                 hoverinfo = 'text',
                                                 text = ~paste("<b>País: ", country, '</b></br></br>', 
                                                               "Data: ", format(time, "%d-%b-%y"), '</br>',
                                                               "Mortes: ", format(deaths, big.mark = " "))),
                                         on = "plotly_hover", off = "plotly_doubleclick")  %>%
                    layout(showlegend = FALSE) %>%
                    layout(title = list(text = 'Países - Número de mortes', font = list(family = "Open Sans", size = 18)),
                           xaxis = list(title = "Data", dtick = "M1", tickformat="%b<br>%Y"),
                           yaxis = list(title = 'Número de mortes')) %>%
                    config(locale = 'pt-BR') 
                
            } # end else if 2
            
            
            
            # Mortes por 100k, escala linear e x = dias desde 0,1 casos
            else if (input$var_mortes_paises == 'Mortes por 100 mil habitantes' &&
                     input$escala_mortes == 'Linear' && input$eixo_mortes_alt == 'Dias desde 0,1 mortes por 100 mil hab.') {
                
                deaths_plot <- highlight(plot_ly(p_d,
                                                 x = ~days_since_pointone_deaths100k, y = ~deaths_per_100k, color = ~country,
                                                 type = 'scatter', mode = 'lines',
                                                 line = list(width = 2),
                                                 hoverinfo = 'text',
                                                 text = ~paste("<b>País: ", country, '</b></br></br>', 
                                                               "Dias desde 0,1 mortes/100 mil: ", days_since_pointone_deaths100k, '</br>',
                                                               "Mortes/100 mil: ", format(deaths_per_100k, big.mark = " "))),
                                         on = "plotly_hover", off = "plotly_doubleclick")  %>%
                    layout(showlegend = FALSE) %>%
                    layout(title = list(text = 'Países - Número de mortes por 100 mil habitantes', font = list(family = "Open Sans", size = 18)),
                           xaxis = list(title = "Dias desde 0,1 mortes/100 mil habitantes"),
                           yaxis = list(title = 'Mortes por 100 mil habitantes')) %>%
                    config(locale = 'pt-BR')
                
            } # end else if 3
            
            
            # Mortes 100k, escala log e x = data
            else if (input$var_mortes_paises == 'Mortes por 100 mil habitantes' &&
                     input$escala_mortes == 'Logarítmica' && input$eixo_mortes_alt == 'Data') {
                
                deaths_plot <- highlight(plot_ly(p_d,
                                                 x = ~time, y = ~deaths_per_100k, color = ~country,
                                                 type = 'scatter', mode = 'lines',
                                                 line = list(width = 2),
                                                 hoverinfo = 'text',
                                                 text = ~paste("<b>País: ", country, '</b></br></br>', 
                                                               "Data: ", format(time, "%d-%b-%y"), '</br>',
                                                               "Mortes/100 mil: ", format(deaths_per_100k, big.mark = " "))),
                                         on = "plotly_hover", off = "plotly_doubleclick")  %>%
                    layout(showlegend = FALSE) %>%
                    layout(title = list(text = 'Países - Número de mortes por 100 mil habitantes', font = list(family = "Open Sans", size = 18)),
                           xaxis = list(title = "Data"),
                           yaxis = list(title = 'Mortes por 100 mil habitantes (escala logarítmica)', type = "log")) %>%
                    config(locale = 'pt-BR')
                
            } # end else if 4
            
            # Mortes 100k, escala linear e x = data
            else if (input$var_mortes_paises == 'Mortes por 100 mil habitantes' &&
                     input$escala_mortes == 'Linear' && input$eixo_mortes_alt == 'Data') {
                
                deaths_plot <- highlight(plot_ly(p_d,
                                                 x = ~time, y = ~deaths_per_100k, color = ~country,
                                                 type = 'scatter', mode = 'lines',
                                                 line = list(width = 2),
                                                 hoverinfo = 'text',
                                                 text = ~paste("<b>País: ", country, '</b></br></br>', 
                                                               "Data: ", format(time, "%d-%b-%y"), '</br>',
                                                               "Mortes/100 mil: ", format(deaths_per_100k, big.mark = " "))),
                                         on = "plotly_hover", off = "plotly_doubleclick")  %>%
                    layout(showlegend = FALSE) %>%
                    layout(title = list(text = 'Países - Número de mortes por 100 mil habitantes', font = list(family = "Open Sans", size = 18)),
                           xaxis = list(title = "Data"),
                           yaxis = list(title = 'Mortes por 100 mil habitantes')) %>%
                    config(locale = 'pt-BR')
                
            } # end else if 5
            
             
            # Mortes 100k, escala log e x = dias desde 0,1 casos
            else if (input$var_mortes_paises == 'Mortes por 100 mil habitantes' &&
                     input$escala_mortes == 'Logarítmica' && input$eixo_mortes_alt == 'Dias desde 0,1 mortes por 100 mil hab.') {
               
                 deaths_plot <- highlight(plot_ly(p_d,
                                                  x = ~days_since_pointone_deaths100k, y = ~deaths_per_100k, color = ~country,
                                                  type = 'scatter', mode = 'lines',
                                                  line = list(width = 2),
                                                  hoverinfo = 'text',
                                                  text = ~paste("<b>País: ", country, '</b></br></br>', 
                                                                "Dias desde 0,1 mortes/100 mil: ", days_since_pointone_deaths100k, '</br>',
                                                                "Mortes/100 mil: ", format(deaths_per_100k, big.mark = " "))),
                                          on = "plotly_hover", off = "plotly_doubleclick")  %>%
                     layout(showlegend = FALSE) %>%
                     layout(title = list(text = 'Países - Número de mortes por 100 mil habitantes', font = list(family = "Open Sans", size = 18)),
                            xaxis = list(title = "Dias desde 0,1 mortes por 100 mil habitantes"),
                            yaxis = list(title = 'Mortes por 100 mil habitantes (escala logarítmica)', type = "log")) %>%
                     config(locale = 'pt-BR')
                
            } # end else if 6
            
            deaths_plot
            
        }) # end plotly
        
        ### Totais de mortes, no Brasil e no mundo
        last_data_mortes <- mortes_casos_100k %>% 
            group_by(country) %>%
            mutate(last_day_deaths = deaths - lag(deaths)) %>%
            top_n(1, time)
        
                # Mundo:
                output$total_mortes_mundo <- renderText({
                    HTML(paste0("<b><font color=\"#FF0000\"><font size=\"5\">", 
                                formatC(as.numeric(last_data_mortes[last_data_mortes$country == "Mundo", ]$deaths), 
                                        big.mark = ".", decimal.mark = ",", format = "d"),
                                "</font></font></b>"))
                })
                
                # Mundo último dia:
                output$total_mortes_mundo_lastday <- renderText({
                    HTML(paste0("<b><font color=\"#FF0000\"><font size=\"3\">", "+ ", 
                                formatC(as.numeric(last_data_mortes[last_data_mortes$country == "Mundo", ]$last_day_deaths), 
                                        big.mark = ".", decimal.mark = ",", format = "d"),
                                "</font></font></b>",
                                "<font color=\"#000000\"><font size=\"3\">", " nas últimas 24hs", "</font></font>"))
                })
                
                
                
                # Brasil:
                output$total_mortes_br <-  renderText({
                    HTML(paste0("<b><font color=\"#FF0000\"><font size=\"5\">", 
                                formatC(as.numeric(last_data_mortes[last_data_mortes$country == "Brasil", ]$deaths), 
                                        big.mark = ".", decimal.mark = ",", format = "d"), 
                                "</font></font></b>"))
                })
                
                # Brasil último dia
                output$total_mortes_br_lastday <-  renderText({
                    HTML(paste0("<b><font color=\"#FF0000\"><font size=\"3\">", "+ ",
                                formatC(as.numeric(last_data_mortes[last_data_mortes$country == "Brasil", 17]$last_day_deaths), 
                                        big.mark = ".", decimal.mark = ",", format = "d"), 
                                "</font></font></b>",
                                "<font color=\"#000000\"><font size=\"3\">", " nas últimas 24hs", "</font></font>"))
                })            
        
        
    }) # end observe
    
    
    # Menu 2, Aba 1: Casos - UFs brasileiras ----
    
    observeEvent(input$ufs1,{
        
    })
    
    
    observe({
        
        selected_ufs <-  ufs_data  %>% filter(UF %in% input$ufs1)    
        
        output$serie5 <- renderPlotly({
        
            ufs_c <- highlight_key(selected_ufs, key = ~UF)
        
            # escala log e x = data
            plot_uf <- highlight(plot_ly(ufs_c,
                                         x = ~date, y = ~confirm, color = ~UF,
                                         type = 'scatter', mode = 'lines',
                                         line = list(width = 2),
                                         hoverinfo = 'text',
                                         text = ~paste("<b>Estado: ", UF, '</b></br></br>', 
                                                       "Data: ", format(date, "%d-%b-%y"), '</br>',
                                                       "Casos: ", format(confirm, big.mark = " "))),
                                 on = "plotly_hover", off = "plotly_doubleclick")  %>%
                layout(showlegend = FALSE) %>%
                layout(title = list(text = 'Estados - Total de casos', font = list(family = "Open Sans", size = 18)),
                       xaxis = list(title = "Data", dtick = "M1", tickformat="%b<br>%Y"),
                       yaxis = list(title = 'Total de casos confirmados (escala logarítmica)', type = "log")) %>%
                config(locale = 'pt-BR')
            

            # escala linear, x = dias desde caso 1, y = casos
            if (input$escala5 == 'Linear' && input$eixo5 == 'Dias desde o primeiro caso' && input$escala_casos21 == "Casos") {
                
                plot_uf <- highlight(plot_ly(ufs_c,
                                             x = ~days_since_1, y = ~confirm, color = ~UF,
                                             type = 'scatter', mode = 'lines',
                                             line = list(width = 2),
                                             hoverinfo = 'text',
                                             text = ~paste("<b>Estado: ", UF, '</b></br></br>', 
                                                           "Dias desde 1º caso: ", days_since_1, '</br>',
                                                           "Casos: ", format(confirm, big.mark = " "))),
                                     on = "plotly_hover", off = "plotly_doubleclick")  %>%
                    layout(showlegend = FALSE) %>%
                    layout(title = list(text = 'Estados - Total de casos', font = list(family = "Open Sans", size = 18)),
                           xaxis = list(title = "Dias desde o primeiro caso"),
                           yaxis = list(title = 'Total de casos confirmados')) %>%
                    config(locale = 'pt-BR')
                
            } #end if1
            
            # escala log, x = dias desde caso 1, y = casos
            else if (input$escala5 == 'Logarítmica' && input$eixo5 == 'Dias desde o primeiro caso' && input$escala_casos21 == "Casos") {
                
                plot_uf <- highlight(plot_ly(ufs_c,
                                             x = ~days_since_1, y = ~confirm, color = ~UF,
                                             type = 'scatter', mode = 'lines',
                                             line = list(width = 2),
                                             hoverinfo = 'text',
                                             text = ~paste("<b>Estado: ", UF, '</b></br></br>', 
                                                           "Dias desde 1º caso: ", days_since_1, '</br>',
                                                           "Casos: ", format(confirm, big.mark = " "))),
                                     on = "plotly_hover", off = "plotly_doubleclick")  %>%
                    layout(showlegend = FALSE) %>%
                    layout(title = list(text = 'Estados - Total de casos', font = list(family = "Open Sans", size = 18)),
                           xaxis = list(title = "Dias desde o primeiro caso"),
                           yaxis = list(title = 'Total de casos confirmados (escala logarítmica)', type = "log")) %>%
                    config(locale = 'pt-BR')
                
            } # end else if 1
            
            # escala linear, x = data, y = casos
            else if (input$escala5 == 'Linear' && input$eixo5 == 'Data' && input$escala_casos21 == "Casos") {
                
                plot_uf <- highlight(plot_ly(ufs_c,
                                             x = ~date, y = ~confirm, color = ~UF,
                                             type = 'scatter', mode = 'lines',
                                             line = list(width = 2),
                                             hoverinfo = 'text',
                                             text = ~paste("<b>Estado: ", UF, '</b></br></br>', 
                                                           "Data: ", format(date, "%d-%b-%y"), '</br>',
                                                           "Casos: ", format(confirm, big.mark = " "))),
                                     on = "plotly_hover", off = "plotly_doubleclick")  %>%
                    layout(showlegend = FALSE) %>%
                    layout(title = list(text = 'Estados - Total de casos', font = list(family = "Open Sans", size = 18)),
                           xaxis = list(title = "Data", dtick = "M1", tickformat="%b<br>%Y"),
                           yaxis = list(title = 'Total de casos confirmados')) %>%
                    config(locale = 'pt-BR')
                
            } # end else if 2
            
            # escala linear, x = dias desde 0,1 casos, y = casos por 100 mil
            else if (input$escala5 == 'Linear' && input$eixo5_alt == 'Dias desde 0,1 casos por 100 mil hab.' && 
                     input$escala_casos21 == "Casos por 100 mil habitantes") {
                
                plot_uf <- highlight(plot_ly(ufs_c,
                                             x = ~days_since_pointone_cases100k, y = ~cases_per_100k, color = ~UF,
                                             type = 'scatter', mode = 'lines',
                                             line = list(width = 2),
                                             hoverinfo = 'text',
                                             text = ~paste("<b>Estado: ", UF, '</b></br></br>', 
                                                           "Dias desde 0,1 casos/100 mil: ", days_since_pointone_cases100k, '</br>',
                                                           "Casos/100 mil: ", format(cases_per_100k, big.mark = " "))),
                                     on = "plotly_hover", off = "plotly_doubleclick")  %>%
                    layout(showlegend = FALSE) %>%
                    layout(title = list(text = 'Estados - Casos por 100 mil habitantes', font = list(family = "Open Sans", size = 18)),
                           xaxis = list(title = "Dias desde 0,1 casos por 100 mil habitantes"),
                           yaxis = list(title = 'Casos por 100 mil habitantes')) %>%
                    config(locale = 'pt-BR')
                
            } #end else if 3
            
            # escala log, x = data, y = casos por 100 mil
            else if (input$escala5 == 'Logarítmica' && input$eixo5_alt == 'Data' 
                     && input$escala_casos21 == "Casos por 100 mil habitantes") {
                
                plot_uf <- highlight(plot_ly(ufs_c,
                                             x = ~date, y = ~cases_per_100k, color = ~UF,
                                             type = 'scatter', mode = 'lines',
                                             line = list(width = 2),
                                             hoverinfo = 'text',
                                             text = ~paste("<b>Estado: ", UF, '</b></br></br>', 
                                                           "Data: ", format(date, "%d-%b-%y"), '</br>',
                                                           "Casos/100 mil: ", format(cases_per_100k, big.mark = " "))),
                                     on = "plotly_hover", off = "plotly_doubleclick")  %>%
                    layout(showlegend = FALSE) %>%
                    layout(title = list(text = 'Estados - Casos por 100 mil habitantes', font = list(family = "Open Sans", size = 18)),
                           xaxis = list(title = "Data", dtick = "M1", tickformat="%b<br>%Y"),
                           yaxis = list(title = 'Casos por 100 mil habitantes (escala logarítmica)', type = "log")) %>%
                    config(locale = 'pt-BR')
                
            } # end else if 4
            
            # escala linear, x = data, y = casos por 100 mil
            else if (input$escala5 == 'Linear' && input$eixo5_alt == 'Data' && input$escala_casos21 == "Casos por 100 mil habitantes") {
                
                plot_uf <- highlight(plot_ly(ufs_c,
                                             x = ~date, y = ~cases_per_100k, color = ~UF,
                                             type = 'scatter', mode = 'lines',
                                             line = list(width = 2),
                                             hoverinfo = 'text',
                                             text = ~paste("<b>Estado: ", UF, '</b></br></br>', 
                                                           "Data: ", format(date, "%d-%b-%y"), '</br>',
                                                           "Casos/100 mil: ", format(cases_per_100k, big.mark = " "))),
                                     on = "plotly_hover", off = "plotly_doubleclick")  %>%
                    layout(showlegend = FALSE) %>%
                    layout(title = list(text = 'Estados - Casos por 100 mil habitantes', font = list(family = "Open Sans", size = 18)),
                           xaxis = list(title = "Data", dtick = "M1", tickformat="%b<br>%Y"),
                           yaxis = list(title = 'Casos por 100 mil habitantes')) %>%
                    config(locale = 'pt-BR')
                
            } # end else if 5
            
            
            # escala logarítmica, x = dias desde 0,1 casos, y = casos por 100 mil
            else if (input$escala5 == 'Logarítmica' && input$eixo5_alt == 'Dias desde 0,1 casos por 100 mil hab.' && input$escala_casos21 == "Casos por 100 mil habitantes") {
                
                plot_uf <- highlight(plot_ly(ufs_c,
                                             x = ~days_since_pointone_cases100k, y = ~cases_per_100k, color = ~UF,
                                             type = 'scatter', mode = 'lines',
                                             line = list(width = 2),
                                             hoverinfo = 'text',
                                             text = ~paste("<b>Estado: ", UF, '</b></br></br>', 
                                                           "Dias desde 0,1 casos/100 mil: ", days_since_pointone_cases100k, '</br>',
                                                           "Casos/100 mil: ", format(cases_per_100k, big.mark = " "))),
                                     on = "plotly_hover", off = "plotly_doubleclick")  %>%
                    layout(showlegend = FALSE) %>%
                    layout(title = list(text = 'Estados - Casos por 100 mil habitantes', font = list(family = "Open Sans", size = 18)),
                           xaxis = list(title = "Dias desde 0,1 casos por 100 mil habitantes"),
                           yaxis = list(title = 'Casos por 100 mil habitantes (escala logarítmica)', type = "log")) %>%
                    config(locale = 'pt-BR')
                
            } #end else if 6
            
            plot_uf
        })
        
        
        ### Casos totais, Brasil e mundo
        last_data <- growth_newcases %>% 
            group_by(country) %>%
            mutate(last_day_cases = confirm - lag(confirm)) %>%
            top_n(1, time)
        
                # Mundo total:
                output$total_mundo1_uf <- renderText({
                    HTML(paste0("<b><font color=\"#FF0000\"><font size=\"5\">", 
                                formatC(as.numeric(sum(last_data$confirm)), 
                                        big.mark = ".", decimal.mark = ",", format = "d"),
                                "</font></font></b>"))
                })
                
                # Mundo último dia:
                output$total_mundo1_lastday_uf <- renderText({
                    HTML(paste0("<b><font color=\"#FF0000\"><font size=\"3\">", "+ ",
                                formatC(as.numeric(sum(last_data$last_day_cases)), 
                                        big.mark = ".", decimal.mark = ",", format = "d"), 
                                "</font></font></b>",
                                "<font color=\"#000000\"><font size=\"3\">", " nas últimas 24hs", "</font></font>"))
                })
                
                
        # Brasil total:
        output$total_br1_uf <-  renderText({
            HTML(paste0("<b><font color=\"#FF0000\"><font size=\"5\">", 
                        formatC(as.numeric(last_data[last_data$country == "Brasil", 3]$confirm), 
                                big.mark = ".", decimal.mark = ",", format = "d"), 
                        "</font></font></b>"))
        })
        
        # Brasil último dia:
        output$total_br1_lastday_uf <-  renderText({
            HTML(paste0("<b><font color=\"#FF0000\"><font size=\"3\">", "+ ",
                        formatC(as.numeric(last_data[last_data$country == "Brasil", 7]$last_day_cases), 
                                big.mark = ".", decimal.mark = ",", format = "d"), 
                        "</font></font></b>",
                        "<font color=\"#000000\"><font size=\"3\">", " nas últimas 24hs", "</font></font>"))
        })
        
        
    })
    
    
    ## Menu 2, Aba 2: UFs - Fator de crescimento e participação no total ----
    observeEvent(input$ufs2,{
        updateSelectInput(session,'highlight_uf2',
                          choices=sort(unique(input$ufs2)),
                          selected = c("Santa Catarina"),)
    })
    
    observe({
        
        selected_ufs2 <-  ufs_data %>% filter(UF %in% input$ufs2)     
        
        output$serie6 <- renderPlotly({
            
            ufs_f <- highlight_key(selected_ufs2, key = ~UF)
            
            # Participação no total de mortes
            plot_share_growth <- highlight(plot_ly(ufs_f,
                                                       x = ~date, y = ~share_deaths, color = ~UF,
                                                       type = 'scatter', mode = 'lines',
                                                       line = list(width = 2),
                                                       hoverinfo = 'text',
                                                       text = ~paste("<b>Estado: ", UF, '</b></br></br>', 
                                                                     "Data: ", format(date, "%d-%b-%y"), '</br>',
                                                                     "Participação (%): ", share_deaths)),
                                               on = "plotly_hover", off = "plotly_doubleclick")  %>%
                layout(showlegend = FALSE) %>%
                layout(title = list(text = 'Estados - Proporção (%) no total de mortes', font = list(family = "Open Sans", size = 18)),
                       xaxis = list(title = "Data", dtick = "M1", tickformat="%b<br>%Y"),
                       yaxis = list(title = 'Participação no total de mortes (%)')) %>%
                config(locale = 'pt-BR')
                
            
            # Participação no total de casos
            if (input$tipo_uf == 'Participação no total de casos') {
                
                plot_share_growth <- highlight(plot_ly(ufs_f,
                                          x = ~date, y = ~share, color = ~UF,
                                          type = 'scatter', mode = 'lines',
                                          line = list(width = 2),
                                          hoverinfo = 'text',
                                          text = ~paste("<b>Estado: ", UF, '</b></br></br>', 
                                                        "Data: ", format(date, "%d-%b-%y"), '</br>',
                                                        "Participação (%): ", share)),
                                  on = "plotly_hover", off = "plotly_doubleclick")  %>%
                    layout(showlegend = FALSE) %>%
                    layout(title = list(text = 'Estados - Proporção (%) no total de casos', font = list(family = "Open Sans", size = 18)),
                           xaxis = list(title = "Data", dtick = "M1", tickformat="%b<br>%Y"),
                           yaxis = list(title = 'Participação no total de casos (%)')) %>%
                    config(locale = 'pt-BR')
            } # end if
            plot_share_growth
        })
    })
    
    
    ## Menu 2, Aba 3: UFs - Taxas de letalidade ----
    observeEvent(input$ufs4,{
        updateSelectInput(session,'highlight_uf4',
                          choices=sort(unique(input$ufs4)),
                          selected = c("Santa Catarina"),)
    })
    
    observe({
        
        selected_ufs4 <-  ufs_data %>% filter(UF %in% input$ufs4)
        
        output$serie_cfr_ufs <- renderPlotly({ 
            
            ufs_l <- highlight_key(selected_ufs4, key = ~UF)
            
            # desde o dia 1 
            cfr_ufs <- highlight(plot_ly(ufs_l,
                                         x = ~days_since_1, y = ~cfr, color = ~UF,
                                         type = 'scatter', mode = 'lines',
                                         line = list(width = 2),
                                         hoverinfo = 'text',
                                         text = ~paste("<b>Estado: ", UF, '</b></br></br>', 
                                                       "Dias desde 1º caso: ", days_since_1, '</br>',
                                                       "Letalidade (CFR): ", cfr)),
                                 on = "plotly_hover", off = "plotly_doubleclick")  %>%
                layout(showlegend = FALSE) %>%
                layout(title = list(text = 'Estados - Taxas de letalidade', font = list(family = "Open Sans", size = 18)),
                       xaxis = list(title = "Dias desde o primeiro caso"),
                       yaxis = list(title = 'Taxa de letalidade (%)')) %>%
                config(locale = 'pt-BR')
            
            # datas
            if (input$eixo_cfr_ufs == 'Data') {
                cfr_ufs <- highlight(plot_ly(ufs_l,
                                             x = ~date, y = ~cfr, color = ~UF,
                                             type = 'scatter', mode = 'lines',
                                             line = list(width = 2),
                                             hoverinfo = 'text',
                                             text = ~paste("<b>Estado: ", UF, '</b></br></br>', 
                                                           "Data: ", format(date, "%d-%b-%y"), '</br>',
                                                           "Letalidade (CFR): ", cfr)),
                                     on = "plotly_hover", off = "plotly_doubleclick")  %>%
                    layout(showlegend = FALSE) %>%
                    layout(title = list(text = 'Estados - Taxas de letalidade', font = list(family = "Open Sans", size = 18)),
                           xaxis = list(title = "Data", dtick = "M1", tickformat="%b<br>%Y"),
                           yaxis = list(title = 'Taxa de letalidade (%)')) %>%
                    config(locale = 'pt-BR')
            } # end if
            
            cfr_ufs
            
        }) # end renderPlotly
        
        
        ### CFR no Brasil e no mundo
        last_cfr <- mortes_casos_100k %>% 
            filter(!is.na(cfr)) %>%
            group_by(country) %>%
            top_n(1, time)
        
                # CFR Mundo:
                output$cfr_mundo_uf <- renderText({
                    HTML(paste0("<b><font color=\"#FF0000\"><font size=\"6\">", 
                                formatC(as.numeric(last_cfr[last_cfr$country == "Mundo", ]$cfr), 
                                        big.mark = ".", decimal.mark = ",", format = "f", digits = 2),
                                "</font></font></b>"))
                })
                
                # CFR Brasil:
                output$cfr_brasil_uf <-  renderText({
                    HTML(paste0("<b><font color=\"#FF0000\"><font size=\"6\">", 
                                formatC(as.numeric(last_cfr[last_cfr$country == "Brasil", ]$cfr), 
                                        big.mark = ".", decimal.mark = ",", format = "f", digits = 2), 
                                "</font></font></b>"))
                })
                
    }) # end observe
    
    
    
    ## Menu 2, Aba 4: UFs - Mortes ----
    observeEvent(input$ufs5,{
        updateSelectInput(session,'highlight_uf5',
                          choices=sort(unique(input$ufs5)),
                          selected = c("Santa Catarina"),)
    })
    
    observe({
        
        selected_ufs5 <-  ufs_data %>% filter(UF %in% input$ufs5)     
        
        output$serie_mortes_ufs <- renderPlotly({ 
            
            ufs_m <- highlight_key(selected_ufs5, key = ~UF)
            
            # escala linear e x = data
            mortes_ufs <- highlight(plot_ly(ufs_m,
                                            x = ~date, y = ~deaths, color = ~UF,
                                            type = 'scatter', mode = 'lines',
                                            line = list(width = 2),
                                            hoverinfo = 'text',
                                            text = ~paste("<b>Estado: ", UF, '</b></br></br>', 
                                                          "Data: ", format(date, "%d-%b-%y"), '</br>',
                                                          "Mortes: ", format(deaths, big.mark = " "))),
                                    on = "plotly_hover", off = "plotly_doubleclick")  %>%
                layout(showlegend = FALSE) %>%
                layout(title = list(text = 'Estados - Número de mortes', font = list(family = "Open Sans", size = 18)),
                       xaxis = list(title = "Data", dtick = "M1", tickformat="%b<br>%Y"),
                       yaxis = list(title = 'Número de mortes')) %>%
                config(locale = 'pt-BR')
            
            
            
            # escala linear e x = dias desde morte 1
            if (input$var_morte == "Mortes" &&
                input$escala_mortes_ufs == 'Linear' && input$eixo_mortes_ufs == 'Dias desde a primeira morte') {
                
                mortes_ufs <- highlight(plot_ly(ufs_m,
                                                x = ~days_since_1, y = ~deaths, color = ~UF,
                                                type = 'scatter', mode = 'lines',
                                                line = list(width = 2),
                                                hoverinfo = 'text',
                                                text = ~paste("<b>Estado: ", UF, '</b></br></br>', 
                                                              "Dias desde 1ª morte: ", days_since_1, '</br>',
                                                              "Mortes: ", format(deaths, big.mark = " "))),
                                        on = "plotly_hover", off = "plotly_doubleclick")  %>%
                    layout(showlegend = FALSE) %>%
                    layout(title = list(text = 'Estados - Número de mortes', font = list(family = "Open Sans", size = 18)),
                           xaxis = list(title = "Dias desde a primeira morte"),
                           yaxis = list(title = 'Número de mortes')) %>%
                    config(locale = 'pt-BR')
                
            } # end if 1
            
            # escala log e x = data
            else if (input$var_morte == "Mortes" &&
                     input$escala_mortes_ufs == 'Logarítmica' && input$eixo_mortes_ufs == 'Data') {
                mortes_ufs <- highlight(plot_ly(ufs_m,
                                                x = ~date, y = ~deaths, color = ~UF,
                                                type = 'scatter', mode = 'lines',
                                                line = list(width = 2),
                                                hoverinfo = 'text',
                                                text = ~paste("<b>Estado: ", UF, '</b></br></br>', 
                                                              "Data: ", format(date, "%d-%b-%y"), '</br>',
                                                              "Mortes: ", format(deaths, big.mark = " "))),
                                        on = "plotly_hover", off = "plotly_doubleclick")  %>%
                    layout(showlegend = FALSE) %>%
                    layout(title = list(text = 'Estados - Número de mortes', font = list(family = "Open Sans", size = 18)),
                           xaxis = list(title = "Data", dtick = "M1", tickformat="%b<br>%Y"),
                           yaxis = list(title = 'Número de mortes (escala logarítmica)', type = "log")) %>%
                    config(locale = 'pt-BR')
                
            } # end else if 1
            
            # escala log e x = dias desde morte 1
            else if (input$var_morte == "Mortes" &&
                     input$escala_mortes_ufs == 'Logarítmica' && input$eixo_mortes_ufs == 'Dias desde a primeira morte') {
                
                mortes_ufs <- highlight(plot_ly(ufs_m,
                                                x = ~days_since_1, y = ~deaths, color = ~UF,
                                                type = 'scatter', mode = 'lines',
                                                line = list(width = 2),
                                                hoverinfo = 'text',
                                                text = ~paste("<b>Estado: ", UF, '</b></br></br>', 
                                                              "Dias desde 1ª morte: ", days_since_1, '</br>',
                                                              "Mortes: ", format(deaths, big.mark = " "))),
                                        on = "plotly_hover", off = "plotly_doubleclick")  %>%
                    layout(showlegend = FALSE) %>%
                    layout(title = list(text = 'Estados - Número de mortes', font = list(family = "Open Sans", size = 18)),
                           xaxis = list(title = "Dias desde a primeira morte"),
                           yaxis = list(title = 'Número de mortes (escala logarítmica)', type = "log")) %>%
                    config(locale = 'pt-BR') 
                
            } # end else if 2
            
            # Escala linear, x = data, y = mortes 100k
            else if (input$var_morte == "Mortes por 100 mil habitantes" &&
                     input$escala_mortes_ufs == 'Linear' &&
                     input$eixo_mortes_ufs_alt == 'Data') {
                
                mortes_ufs <- highlight(plot_ly(ufs_m,
                                                x = ~date, y = ~mortes_100k, color = ~UF,
                                                type = 'scatter', mode = 'lines',
                                                line = list(width = 2),
                                                hoverinfo = 'text',
                                                text = ~paste("<b>Estado: ", UF, '</b></br></br>', 
                                                              "Data: ", format(date, "%d-%b-%y"), '</br>',
                                                              "Mortes: ", format(mortes_100k, big.mark = " "))),
                                        on = "plotly_hover", off = "plotly_doubleclick")  %>%
                    layout(showlegend = FALSE) %>%
                    layout(title = list(text = 'Estados - Número de mortes', font = list(family = "Open Sans", size = 18)),
                           xaxis = list(title = "Data"),
                           yaxis = list(title = 'Número de mortes')) %>%
                    config(locale = 'pt-BR')
                
            } # end else if 3
            
            # Escala linear, x = dias, y = mortes 100k
            else if (input$var_morte == "Mortes por 100 mil habitantes" &&
                     input$escala_mortes_ufs == 'Linear' &&
                     input$eixo_mortes_ufs_alt == 'Dias desde 0,1 mortes por 100 mil hab.') {
                
                mortes_ufs <- highlight(plot_ly(ufs_m,
                                                x = ~days_since_pointone_deaths100k, y = ~mortes_100k, color = ~UF,
                                                type = 'scatter', mode = 'lines',
                                                line = list(width = 2),
                                                hoverinfo = 'text',
                                                text = ~paste("<b>Estado: ", UF, '</b></br></br>', 
                                                              "Dias desde 0,1 mortes/100 mil: ", days_since_pointone_deaths100k, '</br>',
                                                              "Mortes/100 mil: ", format(mortes_100k, big.mark = " "))),
                                        on = "plotly_hover", off = "plotly_doubleclick")  %>%
                    layout(showlegend = FALSE) %>%
                    layout(title = list(text = 'Estados - Mortes por 100 mil habitantes', font = list(family = "Open Sans", size = 18)),
                           xaxis = list(title = "Dias desde 0,1 mortes por 100 mil habitantes"),
                           yaxis = list(title = 'Número de mortes por 100 mil habitantes')) %>%
                    config(locale = 'pt-BR')
                
            } # end else if 4
            
            
            
            # Escala log, x = data, y = mortes 100k
            else if (input$var_morte == "Mortes por 100 mil habitantes" &&
                     input$escala_mortes_ufs == 'Logarítmica' &&
                     input$eixo_mortes_ufs_alt == 'Data') {
                
                mortes_ufs <- highlight(plot_ly(ufs_m,
                                                x = ~date, y = ~mortes_100k, color = ~UF,
                                                type = 'scatter', mode = 'lines',
                                                line = list(width = 2),
                                                hoverinfo = 'text',
                                                text = ~paste("<b>Estado: ", UF, '</b></br></br>', 
                                                              "Data: ", format(date, "%d-%b-%y"), '</br>',
                                                              "Mortes/100 mil: ", format(mortes_100k, big.mark = " "))),
                                        on = "plotly_hover", off = "plotly_doubleclick")  %>%
                    layout(showlegend = FALSE) %>%
                    layout(title = list(text = 'Estados - Mortes por 100 mil habitantes', font = list(family = "Open Sans", size = 18)),
                           xaxis = list(title = "Data", dtick = "M1", tickformat="%b<br>%Y"),
                           yaxis = list(title = 'Número de mortes por 100 mil habitantes (escala logarítmica)', type = "log")) %>%
                    config(locale = 'pt-BR')
                
            } # end else if 5
            
            # Escala log, x = dias, y = mortes 100k
            else if (input$var_morte == "Mortes por 100 mil habitantes" &&
                     input$escala_mortes_ufs == 'Logarítmica' &&
                     input$eixo_mortes_ufs_alt == 'Dias desde 0,1 mortes por 100 mil hab.') {
                
                mortes_ufs <- highlight(plot_ly(ufs_m,
                                                x = ~days_since_pointone_deaths100k, y = ~mortes_100k, color = ~UF,
                                                type = 'scatter', mode = 'lines',
                                                line = list(width = 2),
                                                hoverinfo = 'text',
                                                text = ~paste("<b>Estado: ", UF, '</b></br></br>', 
                                                              "Dias desde 0,1 mortes/100 mil: ", days_since_pointone_deaths100k, '</br>',
                                                              "Mortes/100 mil: ", format(mortes_100k, big.mark = " "))),
                                        on = "plotly_hover", off = "plotly_doubleclick")  %>%
                    layout(showlegend = FALSE) %>%
                    layout(title = list(text = 'Estados - Mortes por 100 mil habitantes', font = list(family = "Open Sans", size = 18)),
                           xaxis = list(title = "Dias desde 0,1 mortes por 100 mil habitantes"),
                           yaxis = list(title = 'Número de mortes por 100 mil habitantes (escala logarítmica)', type = "log")) %>%
                    config(locale = 'pt-BR')
                
            } # end else if 6
            
            
            
            mortes_ufs
            
        }) # end renderPlotly
        
        
        
        ### Totais de mortes, no Brasil e no mundo
        last_data_mortes <- mortes_casos_100k %>% 
            group_by(country) %>%
            mutate(last_day_deaths = deaths - lag(deaths)) %>%
            top_n(1, time)
        
                # Mundo:
                output$total_mortes_mundo_uf <- renderText({
                    HTML(paste0("<b><font color=\"#FF0000\"><font size=\"5\">", 
                                formatC(as.numeric(sum(last_data_mortes$deaths)), 
                                        big.mark = ".", decimal.mark = ",", format = "d"),
                                "</font></font></b>"))
                })
                
                # Mundo último dia:
                output$total_mortes_mundo_lastday_uf <- renderText({
                    HTML(paste0("<b><font color=\"#FF0000\"><font size=\"3\">", "+ ", 
                                formatC(as.numeric(sum(last_data_mortes$last_day_deaths)), 
                                        big.mark = ".", decimal.mark = ",", format = "d"),
                                "</font></font></b>",
                                "<font color=\"#000000\"><font size=\"3\">", " nas últimas 24hs", "</font></font>"))
                })
                
                
                # Brasil:
                output$total_mortes_br_uf <-  renderText({
                    HTML(paste0("<b><font color=\"#FF0000\"><font size=\"5\">", 
                                formatC(as.numeric(last_data_mortes[last_data_mortes$country == "Brasil", ]$deaths), 
                                        big.mark = ".", decimal.mark = ",", format = "d"), 
                                "</font></font></b>"))
                })
                
                # Brasil último dia
                output$total_mortes_br_lastday_uf <-  renderText({
                    HTML(paste0("<b><font color=\"#FF0000\"><font size=\"3\">", "+ ",
                                formatC(as.numeric(last_data_mortes[last_data_mortes$country == "Brasil", ]$last_day_deaths), 
                                        big.mark = ".", decimal.mark = ",", format = "d"), 
                                "</font></font></b>",
                                "<font color=\"#000000\"><font size=\"3\">", " nas últimas 24hs", "</font></font>"))
                })            
               
        
    }) # end observe
    
    
    # Menu 2, Aba 5: UFs - novos casos diários e fator de crescimento (por UF) ----
    
    observe({
        
        # Seleção para novos casos
        selected_uf6 <- ufs_data %>% filter(UF %in% input$ufs6)
        
        ma_uf_casos <- sma(selected_uf6$new_cases, order = 5) # Média móvel 5 dias
        ma_uf_deaths <- sma(selected_uf6$new_deaths, order = 5) # Média móvel 5 dias
        
        ma_uf_casos_7 <- sma(selected_uf6$new_cases, order = 7) # Média móvel 7 dias
        ma_uf_deaths_7 <- sma(selected_uf6$new_deaths, order = 7) # Média móvel 7 dias
        
        selected_uf6$MA5_casos <- ma_uf_casos$fitted
        selected_uf6$MA5_deaths <- ma_uf_deaths$fitted
        
        selected_uf6$MA7_casos <- ma_uf_casos_7$fitted
        selected_uf6$MA7_deaths <- ma_uf_deaths_7$fitted
        
        uf_nov <- highlight_key(selected_uf6, key = ~UF)
        
        # Seleção para fator de crescimento
        selected_uf6_2 <- ufs_data %>% filter(UF %in% input$ufs6)
        
        uf_fat <- highlight_key(selected_uf6_2, key = ~UF)
        
        # Novos casos
        output$serie_uf6_1 <- renderPlotly({
            
            plot_ufs_deaths <- plot_ly(uf_nov,
                                       x = ~date, y = ~new_cases, color = I("gray50"),
                                       type = 'bar',
                                       
                                       showlegend = F,
                                       hoverinfo = 'text',
                                       text = ~paste("<b>Estado: ", UF, '</b></br></br>', 
                                                     "Data: ", format(date, "%d-%b-%y"), '</br>',
                                                     "Novos casos: ", format(new_cases, big.mark = " "), '</br>',
                                                     "Média móvel (5 dias): ", format(round(MA5_casos, 1), big.mark = " ", decimal.mark = ","), '</br>',
                                                     "Média móvel (7 dias): ", format(round(MA7_casos, 1), big.mark = " ", decimal.mark = ",")))  %>%
                layout(showlegend = T, legend = list(x = 0.1, y = 0.9)) %>%
                add_lines(x=~date, y=~MA5_casos, name="Média Móvel - 5 dias", color = I("dodgerblue2"), showlegend = T) %>%
                add_lines(x=~date, y=~MA7_casos, name="Média Móvel - 7 dias", color = I("darkorange2"), showlegend = T) %>%
                layout(title = list(text = paste0("Novos casos diários e médias móveis - ", input$ufs6), 
                                    font = list(family = "Open Sans", size = 18)),
                       xaxis = list(title = "Data"),
                       yaxis = list(title = 'Número de novos casos diários')) %>%
                config(locale = 'pt-BR')
            
            if (input$var_uf6 == "Mortes") {
                
                plot_ufs_deaths <- plot_ly(uf_nov,
                                           x = ~date, y = ~new_deaths, color = I("gray50"),
                                           type = 'bar',
                                           
                                           showlegend = F,
                                           hoverinfo = 'text',
                                           text = ~paste("<b>Estado: ", UF, '</b></br></br>', 
                                                         "Data: ", format(date, "%d-%b-%y"), '</br>',
                                                         "Novos óbitos: ", format(new_deaths, big.mark = " "), '</br>',
                                                         "Média móvel (5 dias): ", format(round(MA5_casos, 1), big.mark = " ", decimal.mark = ","), '</br>',
                                                         "Média móvel (7 dias): ", format(round(MA7_casos, 1), big.mark = " ", decimal.mark = ",")))  %>%
                    layout(showlegend = T, legend = list(x = 0.1, y = 0.9)) %>%
                    add_lines(x=~date, y=~MA5_deaths, name="Média Móvel - 5 dias", color = I("dodgerblue2"), showlegend = T) %>%
                    add_lines(x=~date, y=~MA7_deaths, name="Média Móvel - 7 dias", color = I("darkorange2"), showlegend = T) %>%
                    layout(title = list(text = paste0("Novos óbitos diários e médias móveis - ", input$ufs6), 
                                        font = list(family = "Open Sans", size = 18)),
                           xaxis = list(title = "Data"),
                           yaxis = list(title = 'Número de novos óbitos diários')) %>%
                    config(locale = 'pt-BR')
                
            }
            
            plot_ufs_deaths
            
        }) # end renderPlotly 6_1
        
        # Fator de crescimento   
        output$serie_uf6_2 <- renderPlotly({
            
            hline <- function(y = 0, color = "orangered") {
                list(
                    type = "line",
                    x0 = 0,
                    x1 = 1,
                    xref = "paper",
                    y0 = y,
                    y1 = y,
                    line = list(color = color, dash = "dot",width = 3)
                )
            }
            
            plot_ufs_growth <-  highlight(plot_ly(uf_fat,
                                                  x = ~date, y = ~growth, color = I("black"),
                                                  type = 'scatter', mode = 'lines',
                                                  line = list(width = 1),
                                                  hoverinfo = 'text',
                                                  text = ~paste("<b>Estado: ", UF, '</b></br></br>',
                                                                "Data: ", format(date, "%d-%b-%y"), '</br>',
                                                                "Fator: ", growth)),
                                          on = "plotly_hover", off = "plotly_doubleclick")  %>%
                layout(showlegend = FALSE) %>%
                layout(title = list(text = paste0("Fator de crescimento dos casos - ", input$ufs6),
                                    font = list(family = "Open Sans", size = 18)),
                       xaxis = list(title = "Data", dtick = "M1", tickformat="%b<br>%Y"),
                       yaxis = list(title = 'Fator de crescimento', zeroline = FALSE,
                                    range = c(-2, 20), tickvals = list(-50, -40, -30, -20, -10, 0, 1, 5, 10, 15, 20, 30, 40, 50, 60)),
                       shapes = hline(1)) %>%
                config(locale = 'pt-BR')
            
            if (input$var_uf6 == "Mortes") {
                
                plot_ufs_growth <-  highlight(plot_ly(uf_fat,
                                                      x = ~date, y = ~growth_deaths, color = I("black"),
                                                      type = 'scatter', mode = 'lines',
                                                      line = list(width = 1),
                                                      hoverinfo = 'text',
                                                      text = ~paste("<b>Estado: ", UF, '</b></br></br>',
                                                                    "Data: ", format(date, "%d-%b-%y"), '</br>',
                                                                    "Fator: ", growth_deaths)),
                                              on = "plotly_hover", off = "plotly_doubleclick")  %>%
                    layout(showlegend = FALSE) %>%
                    layout(title = list(text = paste0("Fator de crescimento dos óbitos - ", input$ufs6),
                                        font = list(family = "Open Sans", size = 18)),
                           xaxis = list(title = "Data", dtick = "M1", tickformat="%b<br>%Y"),
                           yaxis = list(title = 'Fator de crescimento', zeroline = FALSE,
                                        range = c(-2, 15), tickvals = list(-50, -40, -30, -20, -10, 0, 1, 5, 10, 15, 20, 30, 40, 50, 60)),
                           shapes = hline(1)) %>%
                    config(locale = 'pt-BR')
                
            }
    
            plot_ufs_growth
            
        }) # end renderPlotly 3_2
        
    }) #end observe
    
    
    # Menu 2, Aba 6: UFs - Tempo para duplicação de casos e mortes ----
    
    function_dobra <- function(x_var, date_var, new_var) { # função para encontrar quando valores dobram (usar com dplyr)
        for(i in (x_var)[1]) 
            for(j in seq_along(x_var))
                if(x_var[j]/x_var[i] >= 2){
                    new_var[j] <- as.character(date_var[j])
                    break() 
                }
        
        for(k in seq_along(new_var))
            if(is.na(new_var[k]) == FALSE)
                for(j in seq_along(x_var))
                    if(x_var[j]/x_var[k] >= 2){
                        new_var[j] <- as.character(date_var[j])
                        break() 
                    }  
        
        new_var
    }
    
    
    ufs_dobra_casos <- ufs_data %>%
        filter(confirm > 0) %>%
        group_by(UF) %>%
        mutate(dobra_casos = NA) %>%
        mutate(dobra_casos = function_dobra(confirm, date, dobra_casos)) %>%
        mutate(dobra_casos = as.Date(dobra_casos, tryFormats = c("%Y-%m-%d"))) %>%
        ungroup
    
    
    ufs_dobra_mortes <- ufs_data %>%
        filter(deaths > 0) %>%
        group_by(UF) %>%
        mutate(dobra_mortes = NA) %>%
        mutate(dobra_mortes = function_dobra(deaths, date, dobra_mortes)) %>%
        mutate(dobra_mortes = as.Date(dobra_mortes, tryFormats = c("%Y-%m-%d"))) %>%
        ungroup
    
    x2 <- "Duplicação dos casos" 
    
    observe({
        
        selected_uf7_1 <-  ufs_dobra_casos %>% filter(UF %in% input$ufs7)
        dobra_dates <- selected_uf7_1$dobra_casos[!is.na(selected_uf7_1$dobra_casos)] # datas das dobras
        valor_dobras_c <- selected_uf7_1 %>% filter(date %in% dobra_dates) %>% select(., "confirm") # casos nas datas de dobra
        casos_nas_dobras <- unname(unlist(valor_dobras_c)) 
        
        selected_uf7_2 <-  ufs_dobra_mortes %>% filter(UF %in% input$ufs7)
        dobra_mor <-selected_uf7_2$dobra_mortes[!is.na(selected_uf7_2$dobra_mortes)] # datas das dobras
        valor_dobras_m <- selected_uf7_2 %>% filter(date %in% dobra_mor) %>% select(., "deaths") # mortes nas datas de dobra
        mortes_nas_dobras <- unname(unlist(valor_dobras_m)) 
        
        
        uf_dobca <- highlight_key(selected_uf7_1, key = ~UF)
        uf_dobmo <- highlight_key(selected_uf7_2, key = ~UF)
        
        
        # Duplicação de casos
        output$serie_uf7_1 <- renderPlotly({ 
            
            p <- highlight(plot_ly(uf_dobca,
                                   x = ~date, y = ~confirm, color = I("dodgerblue4"),
                                   type = 'scatter', mode = 'lines',
                                   line = list(width = 2),
                                   hoverinfo = 'text',
                                   text = ~paste("<b>Estado: ", UF, '</b></br></br>',
                                                 "Data: ", format(date, "%d-%b-%y"), '</br>',
                                                 "Casos: ", format(confirm, big.mark = " "))),
                           on = "plotly_hover", off = "plotly_doubleclick")  %>%
                layout(showlegend = FALSE) %>%
                layout(title = list(text = paste0("Intervalos entre duplicação de casos confirmados - ", input$ufs7),
                                    font = list(family = "Open Sans", size = 18)),
                       xaxis = list(title = "Data", dtick = "M1", tickformat="%b<br>%Y"),
                       yaxis = list(title = 'Casos confirmados (escala logarítmica)', type = "log")) %>%
                config(locale = 'pt-BR')
            
            for(i in 1:length(dobra_dates))
            {
                text1_c <- paste0("Dobra: ", format(dobra_dates[i], "%d-%b-%Y"), "</br></br>",
                                  "Casos: ", format(casos_nas_dobras[i], big.mark = " "))
                p <- p %>%
                    add_trace(x = dobra_dates[i], type = 'scatter', mode = 'lines',
                              line = list(color = "red", width = 1), hoverinfo = 'text',
                              text = text1_c)
            }
            
            p
            
         })
        
        
        # Duplicação de mortes
        output$serie_uf7_2 <- renderPlotly({ 
            
            
            p2 <- highlight(plot_ly(uf_dobmo,
                                   x = ~date, y = ~deaths, color = I("dodgerblue4"),
                                   type = 'scatter', mode = 'lines',
                                   line = list(width = 2),
                                   hoverinfo = 'text',
                                   text = ~paste("<b>Estado: ", UF, '</b></br></br>',
                                                 "Data: ", format(date, "%d-%b-%y"), '</br>',
                                                 "Mortes: ", format(deaths, big.mark = " "))),
                           on = "plotly_hover", off = "plotly_doubleclick")  %>%
                layout(showlegend = FALSE) %>%
                layout(title = list(text = paste0("Intervalos entre duplicação de mortes confirmadas - ", input$ufs7),
                                    font = list(family = "Open Sans", size = 18)),
                       xaxis = list(title = "Data", dtick = "M1", tickformat="%b<br>%Y"),
                       yaxis = list(title = 'Mortes confirmadas (escala logarítmica)', type = "log")) %>%
                config(locale = 'pt-BR')
            
            for(i in 1:length(dobra_mor))
            {
                text1_m <- paste0("Dobra: ", format(dobra_mor[i], "%d-%b-%Y"), "</br></br>",
                                "Mortes: ", format(mortes_nas_dobras[i], big.mark = " "))
                p2 <- p2 %>%
                    add_trace(x = dobra_mor[i], type = 'scatter', mode = 'lines',
                              line = list(color = "red", width = 1), 
                              text = text1_m)
            }
            
            p2
            
        })
        
    }) # end observe
    
    
    
    # Menu 3, Aba 1: Municípios - Mapas de casos e mortes ----
    
    ## Unir dados da data selecionada ao shape de municípios
    data_shp_mun <- reactive({
        
        mun_br_dados <- shp_mun_br %>%
            left_join(., data_mun %>% group_by(CD_GEOCMU) %>% filter(date %in% input$map_mun_date), 
                      by = "CD_GEOCMU", keep = F)
        mun_br_dados
    })
    
    ## labels
    label_municipios <- reactive({
        dados_municipios <- data_shp_mun()
        paste0("<strong>", dados_municipios$NM_MUNICIP, "</strong>",
               "<br />Casos confirmados até ", format(as.Date(dados_municipios$date), "%d/%m/%y"), ": ", dados_municipios$confirmed,
               "<br />Mortes confirmadas até ", format(as.Date(dados_municipios$date), "%d/%m/%y"), ": ", dados_municipios$deaths)
    })
    
    
    label_municipios_100k <- reactive({
        dados_municipios <- data_shp_mun()
        paste0("<strong>", dados_municipios$NM_MUNICIP, "</strong>",
               "<br />Casos por 100 mil habitantes <br /> confirmados até ", format(as.Date(dados_municipios$date), "%d/%m/%y"), ": ", round(dados_municipios$confirmed_per_100k_inhabitants, 2),
               "<br />Mortes por 100 mil habitantes <br /> confirmadas até ", format(as.Date(dados_municipios$date), "%d/%m/%y"), ": ", round(dados_municipios$deaths_per_100k, 2))
    })
    
    
    
    ## Renderizar mapa
    output$mapa_mun1 <- renderLeaflet({ 
        mun_br_dados <- data_shp_mun()
        leaflet(options = leafletOptions(zoomSnap = 0.5,
                                         zoomDelta = 0.5, 
                                         wheelPxPerZoomLevel = 150)) %>%  
            setView(lat = -15, lng = -50, zoom = 4.5) %>%
            addLayersControl(baseGroups = c("Mapa de pontos", "Mapa coroplético"),
                             options = layersControlOptions(collapsed = F)) %>%
            addProviderTiles("OpenStreetMap.Mapnik", group = 'OSM', 
                             options = providerTileOptions(opacity = 0.7))
        
    })
    
    
    # Atualizar mapa
    # Casos - valores absolutos
    observe({
        mun_br_dados <- data_shp_mun()
        bins_mapa <- unique(BAMMtools::getJenksBreaks(mun_br_dados$confirmed, 10))
        n_bins <- length(unique(BAMMtools::getJenksBreaks(mun_br_dados$confirmed, 10)))
        
        if (input$mun_var1 == 'Casos confirmados' && input$escala_mun1 == "Valor absoluto") {
            req(input$selected_menu == "Mapas de casos e mortes nos municípios") # Only display if tab is 
            
            leafletProxy('mapa_mun1') %>% 
                clearMarkers() %>% clearMarkerClusters() %>% clearShapes() %>% clearControls() %>%
                addCircleMarkers(data = mun_br_dados, lat = ~Ycoord, lng = ~ Xcoord, group = "Mapa de pontos",
                                 weight = 1, radius = ~(confirmed)^(1/5), fillOpacity = 0.7,
                                 color = ~colorBin(palette = "viridis", domain = confirmed, 
                                                   bins = bins_mapa, 
                                                   reverse = FALSE, pretty = FALSE,
                                                   na.color = "#FFFAFA")(confirmed),
                                 label= lapply(label_municipios(), HTML)) %>%
                addPolygons(data = mun_br_dados, group = "Mapa coroplético",
                            stroke = TRUE, color = "#666", weight = 1, fillOpacity = 0.5, opacity = 0.6,
                            fillColor = ~colorBin(palette = "viridis", domain = confirmed, 
                                                  bins = bins_mapa, 
                                                  reverse = FALSE, pretty = FALSE,
                                                  na.color = "#FFFAFA")(confirmed),
                            label= lapply(label_municipios(), HTML), 
                            highlight = highlightOptions(
                                weight = 5,
                                color = "#666",
                                fillOpacity = 0
                            )) %>%
                addLegend(position = "bottomleft", 
                          colors = viridis(n_bins), 
                          labels = as.character(bins_mapa), 
                          opacity = 0.6,
                          title = "Casos confirmados")
        }
    })
    
    # Mortes - valores absolutos
    observe({
        mun_br_dados <- data_shp_mun()
        bins_mapa <- unique(BAMMtools::getJenksBreaks(mun_br_dados$deaths, 10))
        n_bins <- length(unique(BAMMtools::getJenksBreaks(mun_br_dados$deaths, 10)))
        
        if (input$mun_var1 == 'Mortes' && input$escala_mun1 == "Valor absoluto") {
            
            leafletProxy('mapa_mun1') %>% 
                clearMarkers() %>% clearMarkerClusters() %>% clearShapes() %>% clearControls() %>%
                addCircleMarkers(data = mun_br_dados, lat = ~Ycoord, lng = ~ Xcoord, group = "Mapa de pontos",
                                 weight = 0.5, radius = ~(deaths)^(1/3), fillOpacity = 1, opacity = 1,
                                 color = ~colorBin(palette = "viridis", domain = deaths, 
                                                   bins = bins_mapa, 
                                                   reverse = FALSE, pretty = FALSE,
                                                   na.color = "#FFFAFA")(deaths),
                                 label= lapply(label_municipios(), HTML)) %>%
                addPolygons(data = mun_br_dados, group = "Mapa coroplético",
                            stroke = TRUE,  color = "#666", weight = 1, fillOpacity = 0.5, opacity = 0.6,
                            fillColor = ~colorBin(palette = "viridis", domain = deaths, 
                                                  bins = bins_mapa, 
                                                  reverse = FALSE, pretty = FALSE,
                                                  na.color = "#FFFAFA")(deaths),
                            label= lapply(label_municipios(), HTML), 
                            highlight = highlightOptions(
                                weight = 5,
                                color = "#666",
                                fillOpacity = 0
                            )) %>%
                addLegend(position = "bottomleft", 
                          colors = viridis(n_bins), 
                          labels = as.character(bins_mapa),
                          opacity = 0.6,
                          title = "Mortes")
            
        }
    })
    
    
    # Casos - valores por 100k
    observe({
        mun_br_dados <- data_shp_mun()
        bins_mapa <- unique(BAMMtools::getJenksBreaks(mun_br_dados$confirmed_per_100k_inhabitants, 10))
        n_bins <- length(unique(BAMMtools::getJenksBreaks(mun_br_dados$confirmed_per_100k_inhabitants, 10)))
        
        if (input$mun_var1 == 'Casos confirmados' && input$escala_mun1 == "Valor por 100 mil habitantes") {
            req(input$selected_menu == "Mapas de casos e mortes nos municípios") # Only display if tab is 
            
            leafletProxy('mapa_mun1') %>% 
                clearMarkers() %>% clearMarkerClusters() %>% clearShapes() %>% clearControls() %>%
                addCircleMarkers(data = mun_br_dados, lat = ~Ycoord, lng = ~ Xcoord, group = "Mapa de pontos",
                                 weight = 1, radius = ~(confirmed_per_100k_inhabitants)^(1/5), fillOpacity = 0.7,
                                 color = ~colorBin(palette = "viridis", domain = confirmed_per_100k_inhabitants, 
                                                   bins = bins_mapa, 
                                                   reverse = FALSE, pretty = FALSE,
                                                   na.color = "#FFFAFA")(confirmed_per_100k_inhabitants),
                                 label= lapply(label_municipios_100k(), HTML)) %>%
                addPolygons(data = mun_br_dados, group = "Mapa coroplético",
                            stroke = TRUE,  color = "#666", weight = 1, fillOpacity = 0.5, opacity = 0.6,
                            fillColor = ~colorBin(palette = "viridis", domain = confirmed_per_100k_inhabitants, 
                                                  bins = bins_mapa, 
                                                  reverse = FALSE, pretty = FALSE,
                                                  na.color = "#FFFAFA")(confirmed_per_100k_inhabitants),
                            label= lapply(label_municipios_100k(), HTML), 
                            highlight = highlightOptions(
                                weight = 5,
                                color = "#666",
                                fillOpacity = 0
                            )) %>%
                addLegend(position = "bottomleft", 
                          colors = viridis(n_bins), 
                          labels = as.character(bins_mapa), 
                          opacity = 0.6,
                          title = "Casos confirmados por 100 mil hab.")
        }
    })
    
    
    # Mortes - valores por 100k
    observe({
        mun_br_dados <- data_shp_mun()
        bins_mapa <- unique(BAMMtools::getJenksBreaks(mun_br_dados$deaths_per_100k, 10))
        n_bins <- length(unique(BAMMtools::getJenksBreaks(mun_br_dados$deaths_per_100k, 10)))
        
        if (input$mun_var1 == 'Mortes' && input$escala_mun1 == "Valor por 100 mil habitantes") {
            
            leafletProxy('mapa_mun1') %>% 
                clearMarkers() %>% clearMarkerClusters() %>% clearShapes() %>% clearControls() %>%
                addCircleMarkers(data = mun_br_dados, lat = ~Ycoord, lng = ~ Xcoord, group = "Mapa de pontos",
                                 weight = 0.5, radius = ~(deaths_per_100k)^(1/3), fillOpacity = 1, opacity = 1,
                                 color = ~colorBin(palette = "viridis", domain = deaths_per_100k, 
                                                   bins = bins_mapa, 
                                                   reverse = FALSE, pretty = FALSE,
                                                   na.color = "#FFFAFA")(deaths_per_100k),
                                 label= lapply(label_municipios_100k(), HTML)) %>%
                addPolygons(data = mun_br_dados, group = "Mapa coroplético",
                            stroke = TRUE, color = "#666", weight = 1, fillOpacity = 0.5, opacity = 0.6,
                            fillColor = ~colorBin(palette = "viridis", domain = deaths_per_100k, 
                                                  bins = bins_mapa, 
                                                  reverse = FALSE, pretty = FALSE,
                                                  na.color = "#FFFAFA")(deaths_per_100k),
                            label= lapply(label_municipios_100k(), HTML), 
                            highlight = highlightOptions(
                                weight = 5,
                                color = "#666",
                                fillOpacity = 0
                            )) %>%
                addLegend(position = "bottomleft", 
                          colors = viridis(n_bins), 
                          labels = as.character(bins_mapa),
                          opacity = 0.6,
                          title = "Mortes por 100 mil hab.")
            
        }
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)