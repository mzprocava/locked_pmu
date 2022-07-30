library(bs4Dash)
library(bslib)
library(fresh)
library(data.table)
library(shinyvalidate)
library(shinyalert)
library(shinyWidgets)
library(aws.s3)
library(openxlsx)
library(pool, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(tidyverse, quietly = TRUE)
library(DBI, quietly = TRUE)
library(RPostgres, quietly = TRUE)
useShinyalert(force = TRUE)
# library(fresh)
tags$style("@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);")
dashboardPage(skin = "light",
              
  dashboardHeader(skin = "light", title = title), 
  dashboardSidebar(collapsed = TRUE, tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
                   tags$style(HTML(
                       ".error {background-color: red; color: white;}
                       .success {background-color: green;color: white;}")),
    sidebarMenu(
      menuItem("ACTIVIDADES", startExpanded = FALSE, icon = icon("line-chart"),
               menuSubItem("PAAO", tabName = "awpb_updating"),
               menuSubItem("Acções Acordadas", tabName = "accoes_acordadas"),
               menuSubItem("Execução", tabName = "awpb_status")
               ),
      
      menuItem("DESPESAS", startExpanded = FALSE, icon = icon("money"),
               menuSubItem("Transações", tabName = "expense_summary"),
               menuSubItem("Relatórios", tabName = "financial_reports")
      )
      
     
      )),
  
  dashboardBody(tags$style("@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);"),
                tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
                useShinyFeedback(feedback = FALSE),
    tabItems(
      tabItem("awpb_updating", fluidPage(
        shinyjs::useShinyjs(),
        shinyjs::inlineCSS(appCSS),
        div(style = "height:10px"),
        fluidRow(width="100%",

                 valueBoxOutput("latencias", width=3),
                 valueBoxOutput("completed_activities",  width=3),
                 valueBoxOutput("started_activities", width=3),
                 valueBoxOutput("advanced_activities", width=3),
                 
                prettySwitch(inputId = "apenas_niassa", label = "UPGPN",  status = "success", value = FALSE, fill = TRUE),
                prettySwitch(inputId = "apenas_gaza", label = "URGPS",  status = "success", value = FALSE, fill = TRUE),

                DT::dataTableOutput("responses_table"),
                tags$script("$(document).on('click', '#responses_table button', function () {
                    Shiny.onInputChange('lastClickId',this.id);
                    Shiny.onInputChange('lastClick', Math.random())});")
        )
      )),
      
      tabItem("accoes_acordadas", fluidPage(
        shinyjs::useShinyjs(),
        shinyjs::inlineCSS(appCSS),
        div(style = "height:10px"),
        fluidRow(width="100%",
                 

                   valueBoxOutput("total_accoes", width=3),
                   valueBoxOutput("accoes_cumpridas",  width=3),
                   valueBoxOutput("accoes_em_curso", width=3),
                   valueBoxOutput("accoes_pendentes", width=3),
                   
                 column(width = 4, selectizeInput('responsible_tecnical', 'Técnico Responsável',  choices = c("Todos", responsible_tech), width = '100%')),
                 column(width = 4, selectizeInput('responsible_institution', 'Entidade Responsável', choices = c("Todas", responsible_institut), width = '100%')),
                 column(width = 4, selectizeInput('mission_name', 'Designação da Missão',  choices = c("Todas", mission_names), width = '100%')),
                 
                 DT::dataTableOutput("aggreed_actions"),
                 tags$script("$(document).on('click', '#aggreed_actions button', function () {
                    Shiny.onInputChange('lastClickId',this.id);
                    Shiny.onInputChange('lastClick', Math.random())});")
        )
      )),
      
      tabItem("awpb_status", fluidRow(
          valueBoxOutput("actividades_iniciadas", width=3),
          valueBoxOutput("actividades_latentes",  width=3),
          valueBoxOutput("actividades_concluidas", width=3),
          valueBoxOutput("taxa_conclusoes", width=3),

        box(title = "ACTIVIDADES PLANIFICADAS vs INICIADAS", closable = TRUE, width = 6, height = "600px", collapsible = TRUE,
             collapsed = FALSE, maximizable = TRUE, plotOutput("responsaveis_estado", height = "100%", width = "100%")),
        box(title = "ACTIVIDADES INICIADAS vs CONCLUÍDAS", closable = TRUE, width = 6, height = "600px", collapsible = TRUE,
            collapsed = FALSE, maximizable = TRUE, plotOutput("por_iniciar", height = "100%", width = "100%")),
        
        box(title = "EXECUÇÃO FÍSICA E FINANCEIRA GLOBAL", closable = TRUE, width = 12, height = "600px", collapsible = TRUE,
            collapsed = FALSE, maximizable = TRUE, echarts4rOutput("physical_execution", height = "100%", width = "100%")),
        
        box(title = "ESTÁGIOS DAS ACTIVIDADES vs COMPONENTES", closable = TRUE, width = 12, height = "550px", collapsible = TRUE,
            collapsed = TRUE, maximizable = TRUE, DTOutput("resumo_actividades", height = "100%", width = "100%")),
        box(title = "IMPACTO ORÇAMENTAL DAS ACTIVIDADES (US$)", closable = TRUE, width = 12, height = "550px", collapsible = TRUE,
            collapsed = TRUE, maximizable = TRUE, DTOutput("impacto_orcamental", height = "100%", width = "100%")),
      )),
      
      
      
      tabItem("expense_summary", fluidPage(
        shinyjs::useShinyjs(),
        shinyjs::inlineCSS(appCSS),
        div(style = "height:10px"),
        fluidRow(width="100%",
                 
                 valueBoxOutput("values_disbursed_usd",  width=3),
                 valueBoxOutput("values_paid_usd", width=3),
                 valueBoxOutput("government_contribution_usd", width=3),
                 valueBoxOutput("beneficiary_contribution_usd", width=3),
                 
                 tags$style("@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);"),
                 tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
                 
                 tags$style(HTML(".error {background-color: red;color: white;} .success {background-color: green;color: white;}")),
                 
                 column(width = 2, selectInput(inputId = "proponents", label= "Proponentes", choices = c("Todos", unique(approval_requests$applicant)),  width = '100%')),
                 column(width = 1, selectInput(inputId = "urgencia_processual", label= "Nível de Urgência", choices = c("Todos", unique(approval_requests$expense_urgencia_processo)),  width = '100%')),
                 column(width = 3, selectInput(inputId = "proponents_process_typologies", label= "Tipologia de Processos", choices = c("Todos", unique(approval_requests$process_type)),  width = '100%')),
                 column(width = 1, selectInput(inputId = "pmu_costcentre", label= "UGB", choices = c("Todas", "UNGP", "URGPS", "UPGPN", "URGPS", "URGPN"), selected = "TODAS", width = '100%')),
                 column(width = 3, selectInput(inputId = "rubricas_ced_pedidas", label= "Rubricas (CED)", choices = c("Todas", unique(approval_requests$e_sistafe_pt)), selected = "TODAS", width = '100%')),
                 column(width = 2, dateRangeInput("periodo_de_despesas", "Período de Submissão:", start = Sys.Date()-365, end = Sys.Date(), width = '100%')),
                 
                 p("Clique no botão na ÚLTIMA COLUNA caso caso tenha PARECER POSITIVO sobre cada despesa"),
                 # DT::dataTableOutput('checkbox_table'),
                 DT::dataTableOutput("finance_clearance"),
                 tags$script("$(document).on('click', '#finance_clearance button', function () {Shiny.onInputChange('lastClickId',this.id);
                  Shiny.onInputChange('lastClick', Math.random())});"),
                 br(),
                 column(width = 2, numericInput(inputId = "forex_exchange_rate", label= "Câmbio Forex para US$", value = forex_e_rate,  width = '100%')),
                 column(width = 4, textInput(inputId = "approval_comments", label= "Comentários e Observações", width = '100%')),
                 column(width = 5, textInput(inputId = "approval_suggestions_finance", label= "Recomendações e Acções de Seguimento", width = '100%')),
                 column(width = 1, dateInput("data_aprovacao_financas", "Pagamento (Data):", value = Sys.Date(), format = "dd/mm/yyyy", width = '100%')),
                 br(),
                 actionButton("submit_finance_approval", "Gravar alterações", class = "success", icon=icon("play")),
        )
      )),
      
      tabItem("financial_reports", fluidPage(
        shinyjs::useShinyjs(),
        shinyjs::inlineCSS(appCSS),
        div(style = "height:10px"),
        fluidRow(width="100%",
                 
                 
                 valueBoxOutput("usd_disbursed", width=3),
                 valueBoxOutput("usd_government",  width=3),
                 valueBoxOutput("usd_beneficiaries", width=3),
                 valueBoxOutput("rpsf_funds_paid", width=3),
                 
                 # column(width = 2, selectInput(inputId = "applicado", label= "Proponentes", choices = c("Todos", unique(approval_requests$applicant)),  width = '100%')),
                 # column(width = 1, selectInput(inputId = "urgency_level", label= "Nível de Urgência", choices = c("Todos", unique(approval_requests$expense_urgencia_processo)),  width = '100%')),
                 # column(width = 3, selectInput(inputId = "tipo_processual", label= "Tipologia de Processos", choices = c("Todos", unique(approval_requests$process_type)),  width = '100%')),
                 # column(width = 1, selectInput(inputId = "centros_cost", label= "UGB", choices = c("Todas", "UNGP", "URGPS", "UPGPN", "URGPS", "URGPN"), selected = "TODAS", width = '100%')),
                 # column(width = 3, selectInput(inputId = "ced_rubrics", label= "Rubricas (CED)", choices = c("Todas", unique(approval_requests$e_sistafe_pt)), selected = "TODAS", width = '100%')),
                 # column(width = 2, dateRangeInput("periodo_despesa", "Período de Submissão:", start = Sys.Date()-365, end = Sys.Date(), width = '100%')),
                 # 
                 box(title = "ANÁLISE GRÁFICA DAS DESPESAS", closable = TRUE, width = 6, height = "680px", collapsible = TRUE,
                     collapsed = FALSE, maximizable = TRUE, status = 'info', solidHeader = TRUE,echarts4rOutput("financial_charts", height = "100%", width = "100%")),
                 
                 box(title = "DESPESAS REALIZADAS POR DIMENSÃO", closable = TRUE, width = 6, height = "680px", collapsible = TRUE,
                     collapsed = FALSE, maximizable = TRUE, status = 'info', solidHeader = TRUE, DTOutput("financial_tables", height = "100%", width = "100%")),
                 
                 box(title = "DETALHES DAS DESPESAS REALIZADAS", closable = TRUE, width = 12, height = "600px", collapsible = TRUE,
                     collapsed = TRUE, maximizable = TRUE, status = 'info', solidHeader = TRUE, DTOutput("detailed_transactions", height = "100%", width = "100%")),
                 
                 # box(title = "ESTÁGIOS DAS ACTIVIDADES vs COMPONENTES", closable = TRUE, width = 12, height = "550px", collapsible = TRUE,
                 #     collapsed = TRUE, maximizable = TRUE, DTOutput("resumo_actividades", height = "100%", width = "100%")),
                 # box(title = "IMPACTO ORÇAMENTAL DAS ACTIVIDADES (US$)", closable = TRUE, width = 12, height = "550px", collapsible = TRUE,
                 #     collapsed = TRUE, maximizable = TRUE, DTOutput("impacto_orcamental", height = "100%", width = "100%")),
                 
                 # DT::dataTableOutput("aggreed_actions"),
                 # tags$script("$(document).on('click', '#aggreed_actions button', function () {
                 #    Shiny.onInputChange('lastClickId',this.id);
                 #    Shiny.onInputChange('lastClick', Math.random())});")
        )
      )),

      # tabItem("expenditure_level", fluidRow(
      #   valueBoxOutput("montante_desembolsado", width=3),
      #   valueBoxOutput("cumulativo_executado",  width=3),
      #   valueBoxOutput("executado_rpsf", width=3),
      #   valueBoxOutput("comparticipacao_governo", width=3),
      #   
      #   box(title = "HISTÓRICO DE DESPESAS POR FINANCIADOR ('000 USD)", closable = TRUE, width = 6, height = "550px", collapsible = TRUE,
      #       collapsed = FALSE, maximizable = TRUE,  echarts4rOutput("expense_timeline", height = "100%", width = "100%")),
      #   
      #   box(title = "DESPESAS POR TRIMESTRE (MZN)", closable = TRUE, width = 6, height = "550px", collapsible = TRUE,
      #       collapsed = FALSE, maximizable = TRUE, echarts4rOutput("despesas_trimestrais", height = "100%", width = "100%")),
      #   
      #   box(title = "EXECUÇÃO POR TRIMESTRE E COMPONENTE", closable = TRUE, width = 12, height = "100%", collapsible = TRUE,
      #       collapsed = TRUE, maximizable = TRUE, DTOutput("components_quarters")),
      #   
      #   box(title = "EXECUÇÃO POR TRIMESTRE E CATEGORIA", closable = TRUE, width = 12, height = "100%", collapsible = TRUE,
      #       collapsed = TRUE, maximizable = TRUE, DTOutput("category_quarters")),
      #   
      #   box(title = "EXECUÇÃO POR TRIMESTRE E FINANCIADOR", closable = TRUE, width = 12, height = "100%", collapsible = TRUE,
      #       collapsed = TRUE, maximizable = TRUE, DTOutput("financiers_quarters")),
      #   
      #   box(title = "EXECUÇÃO POR TRIMESTRE E RUBRICA (CED)", closable = TRUE, width = 12, height = "100%", collapsible = TRUE,
      #       collapsed = TRUE, maximizable = TRUE, DTOutput("execucao_ced"))
      # )),
      
      # tabItem("current_payments", fluidRow(
      #   box(title = "PAGAMENTO DE DESPESAS CORRENTES", closable = TRUE, width = 12, height = "100%", collapsible = TRUE,
      #       collapsed = FALSE, maximizable = TRUE,
      #       fileInput('info_proposta', '', accept = ".xlsx"),
      #       actionButton("submeter_informacao_proposta", "SUBMETER", icon("save")),
      #       DT::dataTableOutput("informacao_proposta")))),
      # 
      # tabItem("imprimir_ip", fluidRow(
      #   box(title = "IMPRESSÃO DE INFORMAÇÃO PROPOSTA", closable = TRUE, width = 12, height = "100%", collapsible = TRUE,
      #       collapsed = FALSE, maximizable = TRUE,
      #       fluidRow(
      #       column(width = 7, selectInput("descricao_processo", "Selecione o processo", choices = c("Todos", detailed), selected = "Todos", multiple = FALSE)),
      #       column(width = 3, selectInput("despesa_a_pagar", "Tipo de informação proposta", choices = c("Todas", tipos_de_despesas), selected = "Todas", multiple = FALSE)),
      #       column(width = 2, dateInput("data_formulacao", "Data de formulação"))),
      # 
      #       tags$style(type="text/css", "#downloadData {background-color:blue;color: white;font-family: Courier New}"),
      #       downloadButton('downloadData', label = "Baixar IP"),
      #       br(),
      #       div(style = "height:10px"),
      #       DT::dataTableOutput("dados_processo")))
      #      ),
      # 
      # 
      # tabItem("expense_approvals", fluidRow(
      #   tags$head(tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/jquery-contextmenu/2.8.0/jquery.contextMenu.min.css"),
      #             tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jquery-contextmenu/2.8.0/jquery.contextMenu.min.js")),
      #   
      #   box(title = "APROVAÇÃO DE PAGAMENTOS", closable = TRUE, width = 12, height = "100%", collapsible = TRUE,
      #       actionButton("saveBtn","SALVAR"),
      #       div(style = "height:10px"),
      #       DT::dataTableOutput("payment_approvals"))
      # )),
      
      # tabItem("procurement_status", fluidRow(
      #   valueBoxOutput("dossiers_iniciados", width=3),
      #   valueBoxOutput("dossiers_contratados",  width=3),
      #   valueBoxOutput("usd_em_procurement", width=3),
      #   valueBoxOutput("usd_latentes", width=3),
      #   
      #   box(title = "SITUAÇÃO POR RESPONSÁVEL ('000 USD)", closable = TRUE, width = 6, height = "550px", collapsible = TRUE, collapsed = FALSE, maximizable = TRUE,
      #       plotOutput("situacao_pp_responsaveis", height = "100%", width = "100%")),
      #   
      #   box(title = "DOSSIERS LATENTES POR RESPONSÁVEL ('000 USD)", closable = TRUE, width = 6, height = "550px", collapsible = TRUE, collapsed = FALSE, maximizable = TRUE,
      #       plotOutput("dossiers_latentes_responsavel", height = "100%", width = "100%")),
      #   
      #   box(title = "DOSSIERS POR COMPONENTE E SUBCOMPONENTE ('000 USD)", closable = TRUE, width = 12, height = "550px", collapsible = TRUE, collapsed = TRUE, maximizable = TRUE,
      #       DT::dataTableOutput("dossiers_components_usd")),
      #   
      #   box(title = "DOSSIERS POR COMPONENTE E SUBCOMPONENTE", closable = TRUE, width = 12, height = "550px", collapsible = TRUE, collapsed = TRUE, maximizable = TRUE,
      #       DT::dataTableOutput("dossiers_components")),
      #   
      #   box(title = "DOSSIERS POR CATEGORIA E MÉTODO ('000 USD)", closable = TRUE, width = 12, height = "550px", collapsible = TRUE, collapsed = TRUE, maximizable = TRUE,
      #       DT::dataTableOutput("dossiers_methods_usd")),
      #   
      #   box(title = "DOSSIERS POR CATEGORIA E MÉTODO", closable = TRUE, width = 12, height = "550px", collapsible = TRUE, collapsed = TRUE, maximizable = TRUE,
      #       DT::dataTableOutput("dossiers_methods")),
      #   
      # )),
      
      #   tabItem("contract_performance", fluidRow(
      #   valueBoxOutput("contratos_celebrados", width=3),
      #   valueBoxOutput("usd_contratados",  width=3),
      #   valueBoxOutput("usd_pagos", width=3),
      #   valueBoxOutput("contratos_fechados", width=3),
      #   
      #   box(title = "# DE CONTRATOS CELEBRADOS VS CONCLUÍDOS", closable = TRUE, width = 6, height = "550px", collapsible = TRUE, collapsed = FALSE, maximizable = TRUE,
      #       plotOutput("signed_contracts", height = "100%", width = "100%")),
      #   
      #   box(title = "CONTRATOS ASSINADOS VS CONCLUÍDOS ('000 USD)", closable = TRUE, width = 6, height = "550px", collapsible = TRUE, collapsed = FALSE, maximizable = TRUE,
      #       plotOutput("ammount_contracted", height = "100%", width = "100%")),
      #   
      #   box(title = "SITUAÇÃO DOS CONTRATOS POR COMPONENTE ('000 USD)", closable = TRUE, width = 12, height = "550px", collapsible = TRUE, collapsed = TRUE, maximizable = TRUE,
      #       DT::dataTableOutput("component_contracts")),
      #   
      #   box(title = "CATEGORIAS DOS CONTRATOS ('000 USD)", closable = TRUE, width = 12, height = "550px", collapsible = TRUE, collapsed = TRUE, maximizable = TRUE,
      #       fluidRow(
      #       column(width= 4,selectInput("linhas_contratos", "Variável das linhas", choices = variaveis_contratos, selected = "contract_status")),
      #       column(width=4,selectInput("colunas_contratos", "Variável das colunas", choices = variaveis_contratos, selected = "procurement_type")),
      #       column(width=4,selectInput("numeric_contratos", "Valor", choices = valores_contratos, selected = "revised_ammount")),
      #       br(),
      #       DT::dataTableOutput("contract_categories")))
      #   
      
      # tabItem("procurement_updates", fluidPage(
      #   
      #   shinyjs::useShinyjs(),
      #   shinyjs::inlineCSS(appCSS),
      #   fluidRow(actionButton("edit_dossier", "ACTUALIZAR", icon("edit"))),
      #   div(style = "height:10px"),
      #   fluidRow(width="100%",
      #            column(width= 4,selectInput("dossier_responsibles", "Responsável", multiple = TRUE, choices = c("Todos", pp_responsibles), selected = "Todos")),
      #            column(width= 4,selectInput("procurement_sheets", "Categoria de Procurement", multiple = TRUE, choices = c("Todos", "Consultoria" = "CONSULTANCY", "Bens" = "GOODS","Construções" = "WORKS"), selected = "Todos")),
      #            column(width= 4,selectInput("procurement_methods", "Métodos", multiple = TRUE, choices = c("Todos", unique(procurement_view$method_name_pt), selected = "Todos"))),
      #            
      #            div(style = "height:10px"),
      #            DT::dataTableOutput("ppdossiers")
      #   )
      # )),
      
      #######################  ESTA ABA DEVE FICAR ESCONDIDA  ###########################
      # tabItem("contracts_updating", fluidPage(
      #   shinyjs::useShinyjs(),
      #   shinyjs::inlineCSS(appCSS),
      #   fluidRow(actionButton("edit_aggreements", "ACTUALIZAR", icon("edit"))),
      #   br(),
      #   fluidRow(width="100%", selectInput("gestores_contractuais", "Gestor do Contrato", multiple = FALSE, choices = c("Todos", staff_choices), selected = "Todos"),
      #            br(),
      #            DT::dataTableOutput("contractdossiers")
      #   )
      # )),
      
      tabItem("contract_ballance", fluidPage(
        shinyjs::useShinyjs(),
        shinyjs::inlineCSS(appCSS),
        # fluidRow(actionButton("edit_aggreements", "ACTUALIZAR", icon("edit"))),
        # br(),
        fluidRow(width="100%",
                 column(width = 4, selectizeInput("gestor_do_contrato", "Gestor do Contrato", multiple = FALSE, choices = c("Todos", staff_choices), selected = "Todos", width="350px")),
                 column(width = 6, selectizeInput("numerador_contratual", "Número do Contrato", multiple = FALSE, choices = c("Todos", contract_numbers), selected = "Todos", width="600px")),
                 column(width = 2, selectizeInput("situacao_contratual", "Situação do Contrato", multiple = FALSE, choices = c("Todos", contract_status), selected = "Todos")),
                 # contract_status
                 div(style = "height:10px"),
                 DT::dataTableOutput("contract_execution_status")
        )
      ))
    )
  ),
  
  footer = bs4DashFooter(left = a(href = "pmu.mozprocava.org", target = "_blank", "FAR, FP - PROCAVA"), right = paste0("Updated: ", format(Sys.Date(), "%B"), " ", format(Sys.Date(), "%Y"))),
  
  dashboardControlbar( dateRangeInput("periodo_referencia", "Período:", start = "2022-01-01", end = "2021-12-31", min = "2020-05-22", max = "2030-12-31"),
                       # sliderInput("anos_referencia", "Anos", min = 2020, max = 2025, value = 2022, sep = ""),
                       selectInput("responsible_technician", "Responsáveis Internos", choices = c("Todos", responsaveis), selected = "Todos"),
                       selectInput("unidades_gestoras", "Unidades Gestoras", choices = c("Todas", management_units), selected = "Todas"),
                       
                       # selectizeInput("responsaveis", "Responsáveis", multiple = FALSE, choices = c("Todos", responsaveis), selected = "Todos"),
                       # column(width= 2,selectizeInput("quadro_logico", "Quadro Lógico", choices = c("Todas", "Contribuição directa" = "NO", "Contribuição indirecta"= "YES"), selected = "Todas")),
                       selectizeInput("mader_relavant", "Prioridade MADER", choices = c("Todas", "Baixa Prioridade" = "NO", "Alta Prioridade"= "YES"), selected = "Todas"),
                       selectizeInput("mader_institutions", "Entidades", multiple = TRUE, choices = c("Todas", instituicoes), selected = "Todas"),
                       selectizeInput("relevance_level", "Nível da Actividade", choices = c("Todas", "Simples" = "Activity", "Compostas" = "Macro-activity", "Lotes", "Sub-actividades" = "Sub-activity"), selected = c("Todas"), multiple = FALSE),
                       selectizeInput("critical_activity", "Importância", choices = c("Todas", "Não Crítica" = "Not Critical", "Crítica" = "Critical"), selected = "Todas"),
                       
                       # selectInput("expense_tipologies", "Tipologia de Despesa", choices = c("Todas", full_pay_proposals$tipos_de_despesas), selected = "Todas"),
                       # selectInput("process_designation", "Designação do Processo", choices = c("Todas", full_pay_proposals$expense_description), selected = "Todas"),
                       
                       # downloadButton('baixar_informacao_proposta', label = "Baixar IP"),
                       # actionButton("baixar_informacao_proposta", "Baixar Informação Proposta", width="87.5%")
                       actionButton("recalcular_realizacoes", "Recalcular Dados!", width="87.5%"))
  
)
