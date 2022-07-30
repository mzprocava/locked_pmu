library(bs4Dash, warn.conflicts = FALSE)
library(DT, warn.conflicts = FALSE)
library(pool, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(tidyverse, warn.conflicts = FALSE)
library(DBI, warn.conflicts = FALSE)
library(RPostgres, warn.conflicts = FALSE)
library(ggcharts, warn.conflicts = FALSE)

library(echarts4r, warn.conflicts = FALSE)
library(janitor, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(feather, warn.conflicts = FALSE)
library(fst, warn.conflicts = FALSE)
# library(shinysurveys, warn.conflicts = FALSE)
library(shinyvalidate, warn.conflicts = FALSE)

library(shinyjs, warn.conflicts = FALSE)
library(sodium, warn.conflicts = FALSE)
library(httr, warn.conflicts = FALSE)

library(splitstackshape, warn.conflicts = FALSE)
library(zoo, warn.conflicts = FALSE)
library(data.table, warn.conflicts = FALSE)
library(flextable, warn.conflicts = FALSEe)
library(glue, warn.conflicts = FALSE)
library(shinyFiles, warn.conflicts = FALSE)
library(readr, warn.conflicts = FALSE)
library(readxl, warn.conflicts = FALSE)
library(officer, warn.conflicts = FALSE)
library(officedown, warn.conflicts = FALSE)
library(shinyFeedback, warn.conflicts = FALSE)
library(shinyalert, warn.conflicts = FALSE)

library(uuid, warn.conflicts = FALSE)
library(collapse, warn.conflicts = FALSE)
library(shinyWidgets)

bs4dash_font(size_base = "1.5rem", weight_bold = 900)
thematic::thematic_shiny(font = "auto")
options(scipen = 9999)
options(digits=15)
options(warn = 0)
e_rate = 64.46

function(input, output, session) {
############################################### REACTIVE DATASETS #############################
  filtered_awpb <- reactive({
    awpb_for_summary <- as.data.frame(dbGetQuery(pool, "SELECT * FROM procava.awpb_for_summary WHERE relevance NOT IN ('Lotes')"))
    # if(input$responsible_technician !="Todos"){awpb_for_summary <- awpb_for_summary[awpb_for_summary$internal_responsible %in% input$responsible_technician,]}
    
    if(input$responsible_technician != "Todos"){awpb_for_summary <- fsubset(awpb_for_summary, internal_responsible %in% input$responsible_technician)}
    if(input$mader_relavant != "Todas"){awpb_for_summary <- fsubset(awpb_for_summary, mader %in% input$mader_relavant)}
    if(input$relevance_level != "Todas"){awpb_for_summary <- fsubset(awpb_for_summary, relevance %in% input$relevance_level)}
    if(input$critical_activity != "Todas"){awpb_for_summary <- fsubset(awpb_for_summary, critical_path == input$critical_activity)}
    # if(input$critical_activity !="Todos"){awpb_for_summary <- awpb_for_summary[awpb_for_summary$critical_path %in% input$critical_activity,]}
    if(input$mader_institutions != "Todas"){awpb_for_summary <- fsubset(awpb_for_summary,institution %in% input$mader_institutions)}
    if(input$apenas_niassa == TRUE){awpb_for_summary <- awpb_for_summary %>% fsubset(niassa > 0, awpb_id, descricao_da_actividade, unidades, niassa_usd, niassa, area, distrital_niassa, achieved_niassa, niassa_period_achievement, niassa_year_achievement, ammount_spent, financial_execution, chart_status, comments_niassa_pt, q1_niassa, q2_niassa, q3_niassa, q4_niassa, internal_responsible, institution, relevance, critical_path, mader, pt_status, situacao, target, q1, q2, q3, q4, q1_south, q2_south, q3_south, q4_south, simplified_status, en_status, componentnum_pt, components_pt, province_locations, distrital_sul, achieved_sul, distrital_locations, month_achieved, achieved_national, srpmu_usd, current_year_budget_us, sul_year_achievement, sul_period_achievement, period_achievement, year_achievement, target_sul, jan_rev_upgpn, feb_rev_upgpn, mar_rev_upgpn, apr_rev_upgpn, may_rev_upgpn, jun_rev_upgpn, jul_rev_upgpn, ago_rev_upgpn, sep_rev_upgpn, oct_rev_upgpn, nov_rev_upgpn, dec_rev_upgpn, jan_rev_urgps, feb_rev_urgps, mar_rev_urgps, apr_rev_urgps, may_rev_urgps, jun_rev_urgps, jul_rev_urgps, ago_rev_urgps, sep_rev_urgps, oct_rev_urgps, nov_rev_urgps, dec_rev_urgps, orcamento_revisto_urgps,  revision_comments_urgps, paao_ungp, paao_upgpn, paao_urgps, financing_category, activity_financiers)}
    if(input$apenas_gaza == TRUE){awpb_for_summary <- awpb_for_summary %>% fsubset(target_sul > 0, awpb_id, descricao_da_actividade, unidades, srpmu_usd, target_sul,  area, distrital_sul, achieved_sul, sul_period_achievement, sul_year_achievement, ammount_spent, financial_execution, chart_status, comments_gaza_pt, q1_south, q2_south, q3_south, q4_south, internal_responsible, institution, relevance, critical_path, mader, pt_status, situacao, target, q1, q2, q3, q4, q1_niassa, q2_niassa, q3_niassa, q4_niassa, simplified_status, en_status, componentnum_pt, components_pt, province_locations, distrital_niassa, achieved_niassa, distrital_locations, month_achieved, achieved_national, current_year_budget_us, niassa_period_achievement, niassa_year_achievement, period_achievement, year_achievement, niassa_usd, niassa, jan_rev_upgpn, feb_rev_upgpn, mar_rev_upgpn, apr_rev_upgpn, may_rev_upgpn, jun_rev_upgpn, jul_rev_upgpn, ago_rev_upgpn, sep_rev_upgpn, oct_rev_upgpn, nov_rev_upgpn, dec_rev_upgpn, jan_rev_urgps, feb_rev_urgps, mar_rev_urgps, apr_rev_urgps, may_rev_urgps, jun_rev_urgps, jul_rev_urgps, ago_rev_urgps, sep_rev_urgps, oct_rev_urgps, nov_rev_urgps, dec_rev_urgps, orcamento_revisto_upgpn,  revision_comments_upgpn, paao_ungp, paao_upgpn, paao_urgps, financing_category, activity_financiers)}
    awpb_for_summary
  })

  # full_payment_proposals <- reactive({
  #   full_payment_proposals <- dbGetQuery(pool, "SELECT * FROM fiduciary.full_payment_proposals")
  #   if(input$expense_tipologies !="Todas"){full_payment_proposals <- full_payment_proposals[full_payment_proposals$tipos_de_despesa %in% input$expense_tipologies,]}
  #   if(input$unidades_gestoras !="Todas"){full_payment_proposals <- full_payment_proposals[full_payment_proposals$cost_centers %in% input$unidades_gestoras,]}
  #   if(input$process_designation !="Todas"){full_payment_proposals <- full_payment_proposals[full_payment_proposals$expense_description %in% input$process_designation,]}
  #   full_payment_proposals
  # })
  accoes_acordadas_procava <-reactive({
    accoes_acordadas_procava <- as.data.frame(dbGetQuery(pool, "SELECT * FROM procava.full_aggreed_actions"))
    if(input$responsible_tecnical !="Todos"){accoes_acordadas_procava <- accoes_acordadas_procava[accoes_acordadas_procava$action_technician %in% input$responsible_tecnical,]}
    if(input$responsible_institution !="Todas"){accoes_acordadas_procava <- accoes_acordadas_procava[accoes_acordadas_procava$action_responsible %in% input$responsible_institution,]}
    if(input$mission_name !="Todas"){accoes_acordadas_procava <- accoes_acordadas_procava[accoes_acordadas_procava$mission_name %in% input$mission_name,]}
    accoes_acordadas_procava
  })
  
  
  # approval_requests <-reactive({
  #   approval_requests <- DBI::dbGetQuery(pool, "SELECT * FROM fiduciary.approval_requests")
  #   # if(input$responsible_technician !="Todos"){paao_granulado <- paao_granulado[paao_granulado$responsibility %in% input$responsible_technician,]}
  #   approval_requests
  # })
  
  
  
  
  paao_granulado <-reactive({
    paao_granulado <- paao_granulado
    if(input$responsible_technician !="Todos"){paao_granulado <- paao_granulado[paao_granulado$responsibility %in% input$responsible_technician,]}
    paao_granulado
  })
  
  granular_payments <-reactive({
    approval_requests <- DBI::dbGetQuery(pool, "SELECT * FROM fiduciary.approval_requests") %>% pivot_longer(cols = c('ifadloan_pct', 'ifadgrant_pct', 'ifadrpsf1_pct', 'ifadrpsf2_pct',
                                                                                                                      'beneficiaryinkind_pct', 'beneficiarymonetary_pct',
                                                                                                                      'privateinkind_pct', 'privatemoney_pct', 'governmentinkind_pct',
                                                                                                                      'governmentmoney_pct'), names_to = "financier", values_to = "percent_financed") %>% fsubset(percent_financed > 0) %>% fsubset(usd_paid>0)
    approval_requests$usd_paid <- approval_requests$usd_paid*approval_requests$percent_financed
    
    approval_requests <- approval_requests %>% mutate(
      financiers = case_when(
        financier == "ifadloan_pct" ~ "IFAD Loan",
        financier == "ifadgrant_pct" ~ "IFAD Grant",
        financier == "ifadrpsf1_pct" ~ "RPSF 1st Allocation",
        financier == "ifadrpsf2_pct" ~ "RPSF 2nd Allocation",
        financier == "beneficiaryinkind_pct" ~ "Beneficiaries (in kind)",
        financier == "beneficiarymonetary_pct" ~ "Beneficiaries (Monetary)",
        financier == "privateinkind_pct" ~ "Private (In kind)",
        financier == "privatemoney_pct" ~ "(Private (Monetary))",
        financier == "governmentinkind_pct" ~ "Government (in kind)",
        financier == "governmentmoney_pct" ~ "Government (Monetary)",
        TRUE                      ~  "others"
      )
    )
    
    approval_requests <- approval_requests %>% mutate(
      financiadores = case_when(
        financier == "ifadloan_pct" ~ "Crédito 154-FIDA",
        financier == "ifadgrant_pct" ~ "Donativo 134-FIDA",
        financier == "ifadrpsf1_pct" ~ "RPSF 1.ª Alocação",
        financier == "ifadrpsf2_pct" ~ "RPSF 2.ª Alocação",
        financier == "beneficiaryinkind_pct" ~ "Beneficiários (Espécie)",
        financier == "beneficiarymonetary_pct" ~ "Beneficiários (Monetária)",
        financier == "privateinkind_pct" ~ "Privados (Espécie)",
        financier == "privatemoney_pct" ~ "Privados (Monetária)",
        financier == "governmentinkind_pct" ~ "Governo (Espécie)",
        financier == "governmentmoney_pct" ~ "Governo (Monetária)",
        TRUE                      ~  "Outros"
      )
    )
    approval_requests
  })
  
  
  # full_approved_payments <-reactive({
  #   full_approved_payments <- dbGetQuery(pool, "SELECT * from fiduciary.full_approved_payments")
  #   if(input$unidades_gestoras !="Todas"){full_approved_payments <- full_approved_payments[full_approved_payments$cost_centre %in% input$unidades_gestoras,]}
  #   full_approved_payments
  # })

  
  proposta_de_pagamento <-reactive({
    full_approved_payments <- dbGetQuery(pool, "SELECT * from fiduciary.full_payments_dataset")
    if(input$despesa_a_pagar !="Todas"){full_approved_payments <- full_approved_payments[full_approved_payments$tipos_de_despesas %in% input$despesa_a_pagar,]}
    if(input$descricao_processo !="Todos"){full_approved_payments <- full_approved_payments[full_approved_payments$detailed %in% input$descricao_processo,]}
    full_approved_payments
  })

  # ##################### AWPB VALUE BOXES  ######################
  output$actividades_iniciadas  <- renderValueBox({
    started <-  filtered_awpb() %>% select(situacao, awpb_id) %>% fsubset(situacao %in% c("Iniciada (Execução < 50%)", "Estado avançado (50% < Execução < 75%)",
                                                                                            "Quase concluída (75% < Execução < 100%)", "Concluída  (Execução >= 100%)"))
    
    valueBox(tags$b(prettyNum(n_distinct(started$awpb_id)), big.mark=",",
                    style = "font-size: 400%;"),"Actividades iniciadas",icon = ionicon(name ="walk"), color = "indigo")
  }) 
  
  output$actividades_latentes   <- renderValueBox({
    started <-  filtered_awpb() %>% select(situacao, awpb_id) %>% fsubset(situacao %in% c("Não iniciada (atrasada)", "Não iniciada (dentro do prazo)"))
    
    valueBox(tags$b(prettyNum(n_distinct(started$awpb_id)), big.mark=",",
                    style = "font-size: 400%;"),"Actividades não iniciadas",icon = ionicon(name ="warning"), color = "danger")
  }) 
  
  output$actividades_concluidas   <- renderValueBox({
    completed <- filtered_awpb() %>% select(situacao, awpb_id) %>% fsubset(situacao == "Concluída  (Execução >= 100%)")
    
    valueBox(tags$b(prettyNum(n_distinct(completed$awpb_id)), big.mark=",",
                    style = "font-size: 400%;"),"Actividades Concluídas",icon = icon("calendar-check"), color = "success")
  }) 
 
  output$taxa_conclusoes   <- renderValueBox({
    completed <-  filtered_awpb() %>% select(situacao, awpb_id) %>% fsubset(situacao %in% c("Concluída  (Execução >= 100%)"))
    valor <- paste(round(n_distinct(completed$awpb_id)/n_distinct(filtered_awpb()$awpb_id)*100, 2),"%")
    valueBox(tags$b(valor, style = "font-size: 400%;"),"Nível de Conclusão das Actividades",icon = icon("percent"), color = "info")
  }) 
  
  output$total_accoes   <- renderValueBox({
    valueBox(tags$b(nrow(accoes_acordadas_procava()), style = "font-size: 400%;"),"Acções Acordadas", icon = icon("handshake-o"), color = "primary")
  })
  
  output$accoes_cumpridas   <- renderValueBox({
    accoes_acordadas_procava <- accoes_acordadas_procava() %>% filter(action_implementation=="Cumprida")
    accoes_cumprimedas <- round(nrow(accoes_acordadas_procava)/nrow(accoes_acordadas_procava())*100,2)
    valueBox(tags$b(nrow(accoes_acordadas_procava), style = "font-size: 400%;"), paste0("Acções Cumpridas (",  accoes_cumprimedas, "%)"),icon = icon("trophy"), color = "success")
  })
  
  output$accoes_em_curso  <- renderValueBox({
    accoes_acordadas_procava <- accoes_acordadas_procava() %>% filter(action_implementation=="Em curso")
    percent_em_curso <- round(nrow(accoes_acordadas_procava)/nrow(accoes_acordadas_procava())*100,2)
    
    valueBox(tags$b(nrow(accoes_acordadas_procava), style = "font-size: 400%;"), paste0("Acções em Curso (", percent_em_curso, "%)"), icon = icon("hourglass-half"), color = "info")
  })
  
  output$accoes_pendentes  <- renderValueBox({
    accoes_acordadas_procava <- accoes_acordadas_procava() %>% filter(action_implementation %in% c("Não iniciada", "Propõe-se cancelar", ""))
    
    percentcurso <- round(nrow(accoes_acordadas_procava)/nrow(accoes_acordadas_procava())*100,2)
    valueBox(tags$b(nrow(accoes_acordadas_procava), style = "font-size: 400%;"), paste0("Acções Pendentes ou Latentes (", percentcurso, "%)"), icon = icon("exclamation-circle"), color = "danger")
  }) 
  
  
  ################   AWPB value Boxes
  output$latencias  <- renderValueBox({
    accoes_acordadas_procava <- filtered_awpb() %>% filter(simplified_status %in% c("Latente"))
    valueBox(tags$b(nrow(accoes_acordadas_procava), style = "font-size: 400%;"),"Actividades Latentes (por Iniciar)",icon = icon("exclamation-circle"), color = "danger")
  })
  
  output$completed_activities  <- renderValueBox({
    accoes_acordadas_procava <- filtered_awpb() %>% filter(simplified_status %in% c("Finalizada"))
    valueBox(tags$b(nrow(accoes_acordadas_procava), style = "font-size: 400%;"),"Actividades Concluídas",icon = icon("trophy"), color = "success")
  })
  
  output$started_activities  <- renderValueBox({
    accoes_acordadas_procava <- filtered_awpb() %>% filter(simplified_status %in% c("Em curso"))
    valueBox(tags$b(nrow(accoes_acordadas_procava), style = "font-size: 400%;"),"Actividades em Curso",icon = icon("cogs"), color = "warning")
  })
  
  output$advanced_activities  <- renderValueBox({
    accoes_acordadas_procava <- filtered_awpb() %>% filter(en_status %in% c("Almost completed (75% < Execution < 100%)", "Advanced stage (50% < Execution < 75%)"))
    valueBox(tags$b(nrow(accoes_acordadas_procava), style = "font-size: 400%;"),"Actividades em Estado Avançado",icon = icon("line-chart"), color = "primary")
  })

  
  #############   PAYMENT VALUE BOXES
  
  # valueBoxOutput("values_disbursed_usd",  width=3),
  # valueBoxOutput("values_paid_usd", width=3),
  # valueBoxOutput("government_contribution_usd", width=3),
  # valueBoxOutput("beneficiary_contribution_usd", width=3),
  output$values_disbursed_usd  <- renderValueBox({
    disbursed_ammount <- DBI::dbGetQuery(pool, "SELECT * FROM fiduciary.disbursed_ammount")
    valueBox(tags$b(round((disbursed_ammount[1,1])/1000000,2), style = "font-size: 400%;"),paste0("Milhões de US$ desembolsados (", round((disbursed_ammount[1,1])/42000000*100,2), "%)"),icon = icon("gift"), color = "success")
  })
  
  output$values_paid_usd  <- renderValueBox({
    payment_data <- approval_requests %>% filter(full_clearance == TRUE, usd_paid>0)
    
    valueBox(tags$b(round(sum(payment_data$usd_paid)/1000, 2), style = "font-size: 400%;"),paste0("Mil US$ pagos"),icon = icon("credit-card"), color = "primary")
  })
  
  output$government_contribution_usd  <- renderValueBox({
    payment_data <- granular_payments() %>% filter(financiers %in% c("Government (In kind)", "Government (Monetary)"))
    
    valueBox(tags$b(round(sum(payment_data$usd_paid)/1000, 2), style = "font-size: 400%;"),paste0("Mil US$ de Contribuição do Governo"),icon = icon("flag-checkered"), color = "maroon")
  })
  
  output$beneficiary_contribution_usd  <- renderValueBox({
    payment_data <- granular_payments() %>% filter(financiers %in% c("Beneficiaries (In kind)", "Benefiiaries (Monetary)"))
    
    valueBox(tags$b(round(sum(payment_data$usd_paid)/1000, 2), style = "font-size: 400%;"),paste0("Mil US$ de Contribuição dos Beneficiários"),icon = icon("users"), color = "olive")
  })
  
  ######  FINANCIAL SUMMARIES VALUE BOXES
  # valueBoxOutput("usd_disbursed", width=3),
  # valueBoxOutput("usd_government",  width=3),
  # valueBoxOutput("usd_beneficiaries", width=3),
  # valueBoxOutput("ammount_paid", width=3),
  
  
  output$usd_disbursed <- renderValueBox({
    disbursed_ammount <- DBI::dbGetQuery(pool, "SELECT * FROM fiduciary.disbursed_ammount")
    valueBox(tags$b(round((disbursed_ammount[1,1])/1000000,2), style = "font-size: 400%;"),paste0("Milhões de US$ desembolsados (", round((disbursed_ammount[1,1])/42000000*100,2), "%)"),icon = icon("gift"), color = "success")
  })
  
  output$rpsf_funds_paid  <- renderValueBox({
    payment_data <- granular_payments() %>% filter(financiers %in% c("RPSF 1st Allocation", "RPSF 2nd Allocation"))
    
    valueBox(tags$b(round(sum(payment_data$usd_paid)/1000, 2), style = "font-size: 400%;"),paste0("Mil US$ pagos"),icon = icon("credit-card"), color = "primary")
  })
  
  output$usd_government  <- renderValueBox({
    payment_data <- granular_payments() %>% filter(financiers %in% c("Government (In kind)", "Government (Monetary)"))
    
    valueBox(tags$b(round(sum(payment_data$usd_paid)/1000, 2), style = "font-size: 400%;"),paste0("Mil US$ de Contribuição do Governo"),icon = icon("flag-checkered"), color = "maroon")
  })
  
  output$usd_beneficiaries  <- renderValueBox({
    payment_data <- granular_payments() %>% filter(financiers %in% c("Beneficiaries (In kind)", "Benefiiaries (Monetary)"))
    
    valueBox(tags$b(round(sum(payment_data$usd_paid)/1000, 2), style = "font-size: 400%;"),paste0("Mil US$ de Contribuição dos Beneficiários"),icon = icon("users"), color = "olive")
  })
  
  #################  AWPB CHARTS ##################
  
  output$responsaveis_estado <- renderPlot({
    finalizadas <- filtered_awpb() %>% group_by(internal_responsible, simplified_status) %>% summarize(n_distinct(awpb_id)) %>% spread(simplified_status, -internal_responsible) %>% adorn_totals("col")
    finalizadas$Iniciadas <- finalizadas$Total - finalizadas$`Latente`
    finalizadas[is.na(finalizadas)] <- 0
    finalizadas$internal_responsible <- fct_reorder(finalizadas$internal_responsible, finalizadas$Total, min)
    ggplot(finalizadas, aes(x= internal_responsible, y = Total)) + geom_bar(stat= "identity", col = "#c4fb83",fill="#c4fb83") +
      geom_bar(aes(x= internal_responsible, y = Iniciadas), stat= "identity", width = 0.5,col = "#10b005",fill="#10b005") + theme_void() +
      labs(x = "", y = "# de actividades", caption = "Fonte: Actualizações do PAAO @ FAR,FP - PROCAVA")+
      theme(axis.text.y = element_text(size = 10))+
      geom_text(aes(label=round(Total), hjust = 1.5), colour="#1f5502", size=3, position=position_dodge(width=0.1))+
      coord_flip()
  }) 
  
  output$por_iniciar <- renderPlot({
    finalizadas <- filtered_awpb() %>% group_by(internal_responsible, simplified_status) %>% summarize(n_distinct(awpb_id)) %>% spread(simplified_status, -internal_responsible) %>% adorn_totals("col")
    finalizadas[is.na(finalizadas)] <- 0
    finalizadas$Iniciadas <- finalizadas$Total - finalizadas$Latente
    finalizadas$internal_responsible <- fct_reorder(finalizadas$internal_responsible, finalizadas$Iniciadas, min)
    
    ggplot(finalizadas, aes(x = internal_responsible, y = Iniciadas)) + geom_bar(stat= "identity", col = "#c4fb83",fill="#c4fb83") +
      geom_bar(aes(x= internal_responsible, y = Finalizada), stat= "identity", width = 0.5,col = "#10b005",fill="#10b005") + theme_void() +
      labs(x = "", y = "# de actividades", caption = "Fonte: Actualizações do PAAO @ FAR,FP - PROCAVA")+
      theme(axis.text.y = element_text(size = 10))+
      geom_text(aes(label=round(Iniciadas), hjust = 1.5), colour="#1f5502", size=3, position=position_dodge(width=0.1))+
      coord_flip()
  }) 
  
  output$resumo_actividades <- DT::renderDataTable({
    totais <- filtered_awpb() %>% group_by(chart_status) %>% summarize(actividades = n_distinct(awpb_id))%>%
      pivot_wider(names_from = "chart_status", values_from = "actividades", values_fill = 0)
    tt_pct <- totais
    tt_pct <- tt_pct[,1:ncol(tt_pct)]/n_distinct(filtered_awpb()$awpb_id)*100
    tt_pct$components_pt <- "(%)"
    
    completions <- filtered_awpb() %>% group_by(components_pt, componentnum_pt, chart_status) %>% summarize(actividades = n_distinct(awpb_id)) %>%
      pivot_wider(names_from = "chart_status", values_from = "actividades", values_fill = 0)
    
    C1 <- completions %>% fsubset(componentnum_pt == "Componente 1") %>% adorn_totals("row") %>% mutate(components_pt = replace(components_pt, components_pt == "Total", "Componente 1")) %>% arrange(components_pt)
    C2 <- completions %>% fsubset(componentnum_pt == "Componente 2") %>% adorn_totals("row") %>% mutate(components_pt = replace(components_pt, components_pt == "Total", "Componente 2"))%>% arrange(components_pt)
    C3 <- completions %>% fsubset(componentnum_pt == "Componente 3") %>% adorn_totals("row") %>% mutate(components_pt = replace(components_pt, components_pt == "Total", "Componente 3"))%>% arrange(components_pt)
    C4 <- completions %>% fsubset(componentnum_pt == "Componente 4") %>% adorn_totals("row") %>% mutate(components_pt = replace(components_pt, components_pt == "Total", "Componente 4"))%>% arrange(components_pt)
    
    completions <- dplyr::bind_rows(C1, C2, C3, C4, totais, tt_pct)
    completions <- completions %>% fselect(-2) %>% adorn_totals("col")
    completions$components_pt[is.na(completions$components_pt)] <- "Total"
    completions$col_share <- round(completions$Total/n_distinct(filtered_awpb()$awpb_id)*100,2)
    completions <- completions %>% filter(col_share>0)
    datatable(completions, rownames= FALSE, extensions = 'Buttons', options = list(paging = TRUE, searching = TRUE, fixedColumns = TRUE, autoWidth = TRUE, ordering = FALSE, dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel')), class = "display")
    
  }) 

  output$physical_execution <- renderEcharts4r({
    e_charts() |> 
      e_gauge(5.55, "Física (%)") |> 
      e_title("")
  })
  
  output$financial_execution <- renderEcharts4r({
    e_charts() |> 
      e_gauge(5.28, "Financeira (%)") |> 
      e_title("")
  })
  
  output$impacto_orcamental <- DT::renderDataTable({
    totais <- completions <- filtered_awpb() %>% group_by(chart_status) %>% summarize(actividades = sum(current_year_budget_us))%>%
      pivot_wider(names_from = "chart_status", values_from = "actividades", values_fill = 0)
    tt_pct <- totais
    tt_pct <- tt_pct[,1:ncol(tt_pct)]/sum(filtered_awpb()$current_year_budget_us)*100
    tt_pct$components_pt <- "(%)"
    
    completions <- filtered_awpb()%>% group_by(components_pt, componentnum_pt, chart_status) %>% summarize(actividades = sum(current_year_budget_us)) %>%
      pivot_wider(names_from = "chart_status", values_from = "actividades", values_fill = 0)
    
    C1 <- completions %>% fsubset(componentnum_pt == "Componente 1") %>% adorn_totals("row") %>% mutate(components_pt = replace(components_pt, components_pt == "Total", "Componente 1")) %>% arrange(components_pt)
    C2 <- completions %>% fsubset(componentnum_pt == "Componente 2") %>% adorn_totals("row") %>% mutate(components_pt = replace(components_pt, components_pt == "Total", "Componente 2"))%>% arrange(components_pt)
    C3 <- completions %>% fsubset(componentnum_pt == "Componente 3") %>% adorn_totals("row") %>% mutate(components_pt = replace(components_pt, components_pt == "Total", "Componente 3"))%>% arrange(components_pt)
    C4 <- completions %>% fsubset(componentnum_pt == "Componente 4") %>% adorn_totals("row") %>% mutate(components_pt = replace(components_pt, components_pt == "Total", "Componente 4"))%>% arrange(components_pt)
    
    completions <- dplyr::bind_rows(C1, C2, C3, C4, totais, tt_pct)
    completions <- completions %>% fselect(-2) %>% adorn_totals("col")
    completions$components_pt[is.na(completions$components_pt)] <- "Total"
    completions$col_share <- round(completions$Total/sum(filtered_awpb()$current_year_budget_us)*100,2)
    
    datatable(completions, rownames= FALSE, extensions = 'Buttons', options = list(paging = TRUE, searching = TRUE, fixedColumns = TRUE, autoWidth = TRUE, ordering = FALSE, dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel')), class = "display")
  }) 

  output$responses_table <- DT::renderDataTable({
    DT  <- as.data.frame(filtered_awpb())
    DT$period_achievement[DT$period_achievement == 999] <- NA
    DT$year_achievement[DT$year_achievement == 999] <- NA
    DT$niassa_period_achievement[DT$niassa_period_achievement == 999] <- NA
    DT$sul_period_achievement[DT$sul_period_achievement == 999] <- NA
    DT$niassa_year_achievement[DT$niassa_year_achievement == 999] <- NA
    DT$sul_year_achievement[DT$sul_year_achievement == 999] <- NA
    
    # DT[["Select"]]<-paste0('<input type="checkbox" name="row_selected" value="Row',1:nrow(DT),'"><br>')
    DT[["Actions"]]<-
      paste0('
             <div class="btn-group" role="group" aria-label="Basic example">
                <button type="button" class="fa fa-edit modify" style="color:blue" id=modify_',1:nrow(DT),'></button>
             </div>
             ')
    DT[["IP"]]<-
      paste0('
             <div class="btn-group" role="group" aria-label="Proposta">
                <button type="button" class="fa fa-coins informacaoproposta" style="color:blue" id=informacaoproposta_',1:nrow(DT),'></button>
             </div>
             ')
    
    DT <- DT %>% select(Actions, IP, everything())

    names(DT) <- c('','IP', 'Ref.ª', 'Actividade', 'Unidade', 'Orçamento (US$)', 'Meta', 'Abrangência', 'Distritos Real', 'Real', 'Período (%)', 'Ano (%)', 'Despesas (MZN)', '(%)', 'Situação', 'Comentários', 'Q1', 'Q2', 'Q3', 'Q4', 'internal_responsible', 'institution', 'relevance', 'critical_path', 'mader', 'pt_status', 'situacao', 'target', 'q1', 'q2', 'q3', 'q4', 'q1_niassa', 'q2_niassa', 'q3_niassa', 'q4_niassa', 'simplified_status', 'en_status', 'componentnum_pt', 'components_pt', 'province_locations', 'distrital_sul', 'achieved_sul', 'distrital_locations', 'month_achieved', 'achieved_national', 'current_year_budget_us', 'sul_year_achievement', 'sul_period_achievement', 'period_achievement', 'year_achievement', 'niassa_usd', 'niassa')
    
    datatable(DT, escape = FALSE,  rownames=F, extensions = c('Buttons'), options = list(dom = 'Bfrtip', buttons = c('pageLength', 'copy', 'csv', 'excel', 'pdf', 'print'),  pagelength = 10, lengthMenu = list(c(10, 25, 100, -1), c('10', '25', '100','All')),
                                                                                         columnDefs = list(list(targets = c(16:(ncol(DT)-1)), visible = FALSE))))%>%
      
      formatStyle(columns = c('Situação'), backgroundColor = styleEqual(c("Atrasada", "Latente", "Iniciada",
                                                                          "Quase a terminar", "Terminada",
                                                                          "Fase final"), c("#f15a4e", "#f9ad6e","#fced03", "#8cf827", "#05b205","#00FF00"))) %>%
      
      formatRound(c('Orçamento (US$)','Meta', 'Q1', 'Q2','Q3','Q4'), digits = 0) %>% 
      formatRound(c('Real', 'Período (%)', 'Ano (%)'), digits = 2) %>% 
      formatStyle(columns = c('Q1', 'Q2', 'Q3', 'Q4'), backgroundColor = styleInterval(cuts =c(0,1), values = c("#f5f7f7", "none","#00f0ff")), fontWeight = styleInterval(cuts = 1,values = c("normal","bold"))) %>% 
      # formatStyle(columns = c(12), backgroundColor = styleInterval(cuts =c(0,25,50,75,100,998), values = c("#f15a4e", "#f9ad6e","#fced03", "#8cf827","#05b205","#d0d5d0")), fontWeight = styleInterval(cuts = 6,values = c("normal","bold", "normal","bold", "normal","bold"))) %>% 
      formatStyle(columns = c('Período (%)', 'Ano (%)'), backgroundColor = styleInterval(cuts =c(0,25,50,75,100,998), values = c("#f15a4e", "#f9ad6e","#fced03", "#8cf827", "#05b205","#05b205","#d0d5d0")), fontWeight = styleInterval(cuts = 1,values = c("bold","normal"))) %>% 
      
      formatCurrency(c('Orçamento (US$)', "Despesas (MZN)", "(%)"), '') %>% 
      formatStyle(c('Situação', 'Abrangência', 'Unidade'), `text-align` = 'center')
  })

  fieldsMandatory <- c("situacao", "comentarios")

   
  
  output$aggreed_actions <- DT::renderDataTable({
    DT  <- as.data.frame(accoes_acordadas_procava())
    
    
    DT$action_deadline  <- strftime(DT$action_deadline, format = '%d-%b-%Y')
    DT[["Actions"]]<-
      paste0('<div class="btn-group" role="group" aria-label="Basic example">
                <button type="button" class="fa fa-edit changeactions" style="color:blue" id=changeactions_',1:nrow(DT),'></button>
             </div>')
    DT <- DT %>% select(Actions, everything())
    
    names(DT) <- c('','Acção acordada', 'Entidade Responsável', 'Prazo', 'Situação', 'Data da Situação', 'Delivery timing', 'Comentários e Observações')
    
    datatable(DT, escape = FALSE,  rownames=F, extensions = c('Buttons'), options = list(dom = 'Bfrtip', buttons = c('pageLength', 'copy', 'csv', 'excel', 'pdf', 'print'),  pagelength = 10, lengthMenu = list(c(10, 25, 100, -1), c('10', '25', '100','All')),
                                                                                         columnDefs = list(list(targets = c(8:(ncol(DT)-1)), visible = FALSE))))%>%

      formatStyle(columns = c('Situação'), backgroundColor = styleEqual(c("Não iniciada","Em curso", "Cumprida", "Propõe-se cancelar", "Cancelada", "Não cumprida"), c("#F5FB00", "#FFC300","#0CF717", "#B7AFAE", "#8AB67B", "#FF0F00")))
  })
  
  
  
  # output$transaction_list <- DT::renderDataTable({
  #   DT  <- as.data.frame(proposta_de_pagamento())
  #   
  #   
  #   # DT$action_deadline  <- strftime(DT$action_deadline, format = '%d-%b-%Y')
  #   # DT[["Actions"]]<-
  #   #   paste0('<div class="btn-group" role="group" aria-label="Basic example">
  #   #             <button type="button" class="fa fa-edit changeactions" style="color:blue" id=changeactions_',1:nrow(DT),'></button>
  #   #          </div>')
  #   # DT <- DT %>% select(Actions, everything())
  #   
  #   names(DT) <- c('','Acção acordada', 'Entidade Responsável', 'Prazo', 'Situação', 'Data da Situação', 'Delivery timing', 'Comentários e Observações')
  #   
  #   datatable(DT, escape = FALSE,  rownames=F, extensions = c('Buttons'), options = list(dom = 'Bfrtip', buttons = c('pageLength', 'copy', 'csv', 'excel', 'pdf', 'print'),  pagelength = 10, lengthMenu = list(c(10, 25, 100, -1), c('10', '25', '100','All')),
  #                                                                                        columnDefs = list(list(targets = c(8:(ncol(DT)-1)), visible = FALSE))))%>%
  #     
  #     formatStyle(columns = c('Situação'), backgroundColor = styleEqual(c("Não iniciada","Em curso", "Cumprida", "Propõe-se cancelar", "Cancelada", "Não cumprida"), c("#F5FB00", "#FFC300","#0CF717", "#B7AFAE", "#8AB67B", "#FF0F00")))
  # })
  # 
  
  
  #  output$informacao_proposta <- DT::renderDataTable({
  #    req(input$info_proposta)
  #    inFile <- input$info_proposta
  #    ficheiro <- read_excel(inFile$datapath, sheet = "DATASET")
  #    ficheiro$paid_ammount <-  as.numeric(ficheiro$paid_ammount)
  #    payments_dataset <- ficheiro %>% fsubset(!is.na(paid_ammount))
  #    ficheiro_visivel <- payments_dataset
  #    ficheiro_visivel <- ficheiro_visivel %>% fselect(beneficiary, awpb_id, processo_numero, detailed, payment_details, paid_ammount, contract,
  #                                                           e_sistafe_w_code, funding, applicant_name, everything())
  #    datatable(ficheiro_visivel, options=list(columnDefs = list(list(visible=FALSE, targets=c(13:ncol(ficheiro_visivel)))), rowsGroup = list(2)))
  #    
  #  })
  #  
  #  
  #  observeEvent(input$salvar_xls_ip, priority = 20, {
  #    req(input$payments_xls_fida)
  #    inFile <- input$payments_xls_fida
  #    ficheiro <- read_excel(inFile$datapath, sheet = "DATASET", col_types = c('text', 'text', 'text', 'text', 'numeric', 'text', 'text', 'text', 'text', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'text', 'text', 'text', 'date', 'date', 'numeric', 'text', 'text', 'text', 'text', 'text', 'text', 'text', 'text', 'text', 'text', 'text', 'numeric', 'text', 'text', 'text', 'text', 'numeric', 'numeric', 'text', 'date', 'text', 'text', 'numeric', 'text', 'numeric', 'numeric', 'text', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'text', 'text', 'text', 'text', 'text', 'text', 'text', 'text', 'text', 'text', 'text', 'text', 'text', 'numeric', 'text', 'text', 'text', 'date', 'text', 'text', 'text', 'text', 'text', 'text', 'text', 'text', 'numeric', 'text', 'text', 'text', 'text', 'numeric'))
  #    
  #    payments_dataset <- ficheiro %>% fsubset(!is.na(paid_ammount))
  #    
  #    payments_dataset$ced <- parse_number(payments_dataset$e_sistafe_w_code)
  #    
  #    payments_dataset <- payments_dataset %>% fselect(expense_description, detailed_en, e_sistafe_w_code, payment_beneficiary, 
  #                                                     nuit_creditado, justificativo_tipo, document_number, awpb_ids, expense_detailed_pt,
  #                                                     unidades_medida, quantity10, quantity30, quantity100, unit_prices, is_contract, 
  #                                                     relevance, activity_started, activity_ended, product_location, process_type, 
  #                                                     submission_date, contract_vs_invoice, contract_vs_owner,
  #                                                     contract_levels, governmentinkind_pct, governmentmoney_pct, financier_paying,
  #                                                     ifadgrant_pct, ifadloan_pct, beneficiaryinkind_pct, beneficiarymonetary_pct,
  #                                                     ifadrpsf1_pct, ifadrpsf2_pct, privateinkind_pct, privatemoney_pct, process_count,
  #                                                     planning_clearance, procurement_clearance, finance_clearance, full_clearance,
  #                                                     approval_comments, applicant_nuit, expense_urgencia_processo, expense_comments,
  #                                                     expense_currency, contribuinte_name, contribuinte_surname, contribuicao_tipo,
  #                                                     localidade_contribuinte, comunidade_contribuinte, genero_contribuinte,
  #                                                     idade_contribuinte, vulnerabilidade_contribuinte, funcao_social_contribuinte,
  #                                                     categoria_prof_contribuinte, cargo_contribuinte, salario_contribuinte)
  #    
  #    dbWriteTable(pool, SQL("fiduciary.payment_proposals"), payments_dataset, overwrite = FALSE, append = TRUE)
  #    # showModal(modalDialog("Pedido submetido com sucesso", easyClose = TRUE, footer = NULL, class = "success")) 
  #    showToast("success", "Comunicaremos sobre o andamento", "DESPESAS CARREGADAS!", .options = myToastOptions)
  #    # showModal(modalDialog(title=paste0("Pedido submetido"), br(), div(tags$b(paste0("SUCESSO!"), style = "color: green;"))))
  #  })
  # 
  # ################ INICIO DO PROBLEMA  ####################
  # 
  #  iv<- InputValidator$new()
  #  iv$add_rule("nuit_creditado", sv_between(100000000, 599999999, message_fmt = ""))
  #  iv$add_rule("ifadgrant_pct", sv_between(1, 100, message_fmt = ""))
  #  iv$add_rule("ifadloan_pct", sv_between(1, 100, message_fmt = ""))
  #  iv$add_rule("ifadrpsf1_pct", sv_between(1, 100, message_fmt = ""))
  #  iv$add_rule("ifadrpsf2_pct", sv_between(1, 100, message_fmt = ""))
  #  iv$add_rule("governmentinkind_pct", sv_between(1, 100, message_fmt = "d"))
  #  iv$add_rule("governmentmonetary_pct", sv_between(1, 100, message_fmt = ""))
  #  iv$add_rule("beneficiaryinkind_pct", sv_between(1, 100, message_fmt = ""))
  #  iv$add_rule("beneficiarymoney_pct", sv_between(1, 100, message_fmt = ""))
  #  iv$add_rule("privatemoney_pct", sv_between(1, 100, message_fmt = ""))
  #  iv$add_rule("privateinkind_pct", sv_between(1, 100, message_fmt = ""))
  #  
  #  iv$add_rule("document_number", sv_between(0, 999, message_fmt = ""))
  #  iv$add_rule("value", sv_between(0, 100000000, message_fmt = ""))
  #  iv$add_rule("contract_level", sv_between(0, 100, message_fmt = ""))
  #  iv$add_rule("contract", sv_required(message = "É obrigatório indicar o contrato"))
  #  iv$enable()
  # 
  #  output$baixar_informacao_proposta <- downloadHandler(
  #    filename <- function(){
  #     
  #      payments_data <- full_payment_proposals()
  #      # tipo_despesa <- "Pagamento de contratos"
  #      file_name <- ifelse(input$expense_tipologies == "Ajudas de custos", "Modelo_IP_RMarkdown_Final_ADC.docx",
  #                          ifelse(input$expense_tipologies =="Salários e remunerações", "Modelo_IP_RMarkdown_Final_SALARIOS.docx",
  #                                 ifelse(input$expense_tipologies == "Pagamento de contratos", "Modelo_IP_RMarkdown_Final_CONTRATOS.docx",
  #                                        "Modelo_IP_RMarkdown_Final_OUTROS.docx")))
  #      sample_doc <- read_docx(file_name)
  #      
  #      keywords <- ifelse(input$expense_tipologies == "Ajudas de custos", "acordo com a tabela a seguir.",
  #                         ifelse(input$expense_tipologies =="Salários e remunerações", "ANEXO. DETALHES DO PAGAMENTO", "ANEXO. DETALHES DO PAGAMENTO"))
  #      
  #      date_proposal <- as.Date(Sys.Date(), "%m/%d/%y")
  #      month_number <- month(date_proposal)
  #      
  #      year_proposal <- year(date_proposal)
  #      
  #      formated_date <- paste0(day(date_proposal),"/", ifelse(month(date_proposal)<10,paste0("0",month(date_proposal)), month(date_proposal)), "/", year(date_proposal))
  #      month_salaries_paid <- ifelse(month_number== 1, "Janeiro", ifelse(month_number== 2, "Fevereiro", ifelse(month_number== 3, "Março", ifelse(month_number== 4, "Abril",
  #                                                                                                                                                ifelse(month_number== 5, "Maio", ifelse(month_number== 6, "Junho", ifelse(month_number== 7, "Julho", ifelse(month_number== 8, "Agosto",
  #                                                                                                                                                                                                                                                            ifelse(month_number== 9, "Setembro", ifelse(month_number== 10, "Outubro",
  #                                                                                                                                                                                                                                                                                                        ifelse(month_number== 11, "Novembro", ifelse(month_number== 12, "Dezembro"))))))))))))
  #      payments_data <- payments_data %>% mutate_if(is.numeric, ~replace_na(., 0))
  #      
  #      ################   SUMMARY FOR OTHERS
  #      payments_summary <- payments_data %>% group_by(ced, e_sistafe_pt, payment_beneficiary, nuit_creditado) %>% summarize(qty = sum(quantity30)*0.3 + sum(quantity10)*0.1 + sum(quantity100),
  #                                                                                                                           valor = sum(paid_ammount))
  #      
  #      payments_summary$qty <- ifelse(payments_summary$qty == 0, 1, payments_summary$qty)
  #      payments_summary$preco <- payments_summary$valor/payments_summary$qty
  #      
  #      payments_summary <-  modify_if(payments_summary, ~is.numeric(.), ~round(., 2))
  #      payments_summary$e_sistafe_pt[is.na(payments_summary$e_sistafe_pt)] <- ""
  #      payments_summary$nuit_creditado[is.na(payments_summary$nuit_creditado)] <- 0
  #      payments_summary[is.na(payments_summary)] <- 0
  #      
  #      payments_summary <- payments_summary %>% adorn_totals("row", fill = "", col_var_name = c("valor"))
  #      payments_summary <- payments_summary %>% select(ced, e_sistafe_pt, qty, preco, everything())
  #      payments_summary$ced <- as.character(payments_summary$ced)
  #      setnames(payments_summary, c("ced", "e_sistafe_pt", "payment_beneficiary", "nuit_creditado", "qty", "preco", "valor"), c("CED", "Designação do CED", "Beneficiário", "NUIT", "Qty", "Preço" , "Valor (MT)"))
  #      
  #      ############ SUMMARY AJUDAS DE CUSTOS
  #      payments_summary2 <- payments_data %>% group_by(ced, e_sistafe_pt, payment_beneficiary, nuit_creditado) %>% summarize(qty = sum(quantity30)*0.3 + sum(quantity10)*0.1 + sum(quantity100),
  #                                                                                                                            valor = sum(paid_ammount))
  #      payments_summary2$qty <- ifelse(payments_summary2$qty == 0, 1, payments_summary2$qty)
  #      payments_summary2$preco <- payments_summary2$valor/payments_summary2$qty
  #      payments_summary2 <-  modify_if(payments_summary2, ~is.numeric(.), ~round(., 2))
  #      payments_summary2$e_sistafe_pt[is.na(payments_summary2$e_sistafe_pt)] <- ""
  #      payments_summary2$nuit_creditado[is.na(payments_summary2$nuit_creditado)] <- 0
  #      payments_summary2[is.na(payments_summary2)] <- 0
  #      payments_summary2 <- payments_summary2 %>% adorn_totals("row", fill = "", col_var_name = c("valor"))
  #      payments_summary2 <- payments_summary2 %>% select(ced, e_sistafe_pt, payment_beneficiary, nuit_creditado,qty, preco, everything())
  #      payments_summary2$ced <- as.character(payments_summary2$ced) 
  #      setnames(payments_summary2, c("ced", "e_sistafe_pt", "payment_beneficiary", "nuit_creditado", "qty", "preco", "valor"), c("CED", "Designação do CED", "Beneficiário", "NUIT", "Qty", "Preço" , "Valor (MT)"))
  #      
  #      # payments_summary <- ifelse(tipo_despesa == "Ajudas de custos", payments_summary2, payments_summary)
  #      
  #      set_flextable_defaults(font.size = 9, font.color = "black", table.layout = "fixed", digits = 1)
  #      ft <- flextable(payments_summary)
  #      ft <- theme_booktabs(ft)
  #      ft <- autofit(ft)
  #      ft <- colformat_double(x = ft, big.mark=",", digits = 2, na_str = "N/A")
  #      ft <- align_nottext_col(ft, align = "right")
  #      ft <- line_spacing(ft, space = 1.0, part = "all")
  #      ft <- bold(ft, bold = TRUE, part = "header")
  #      ft <- bold(ft, i = nrow(payments_summary), bold = TRUE)
  #      ft <- padding(ft, padding = 1)
  #      ft <- compose(ft, i = nrow(payments_summary), j = 3, as_paragraph(as_chunk('-')))
  #      ft <- compose(ft, i = nrow(payments_summary), j = 4, as_paragraph(as_chunk('-')))
  #      ft <- compose(ft, i = nrow(payments_summary), j = 5, as_paragraph(as_chunk('-')))
  #      
  #      # ft
  #      set_flextable_defaults(font.size = 9, font.color = "black", table.layout = "fixed", digits = 1)
  #      ft2 <- flextable(payments_summary2)
  #      ft2 <- theme_booktabs(ft2)
  #      ft2 <- autofit(ft2)
  #      ft2 <- colformat_double(x = ft2, big.mark=",", digits = 2, na_str = "N/A")
  #      ft2 <- align_nottext_col(ft2, align = "right")
  #      ft2 <- line_spacing(ft2, space = 1.0, part = "all")
  #      ft2 <- bold(ft2, bold = TRUE, part = "header")
  #      ft2 <- bold(ft2, i = nrow(payments_summary), bold = TRUE)
  #      ft2 <- padding(ft2, padding = 1)
  #      ft2 <- compose(ft2, i = nrow(payments_summary), j = 3, as_paragraph(as_chunk('-')))
  #      ft2 <- compose(ft2, i = nrow(payments_summary), j = 4, as_paragraph(as_chunk('-')))
  #      ft2 <- compose(ft2, i = nrow(payments_summary), j = 5, as_paragraph(as_chunk('-')))
  #      ft2_ajudas <- compose(ft2, i = nrow(payments_summary), j = 6, as_paragraph(as_chunk('-')))
  #      
  #      # ft2_ajudas
  #      sample_doc <- cursor_reach(sample_doc, keyword = keywords)
  #      sample_doc <- body_add_flextable(sample_doc, value = ft, pos = "after")
  #      
  #      document <- glue_collapse(payments_data$document, ", ", last = " e ")
  #      
  #      financiadores <- payments_data %>% pivot_longer(cols = c(ifadloan_pct:governmentmoney_pct), names_to = "contributors", values_to = "percent_financed")
  #      financiadores$paid_ammount <- financiadores$paid_ammount* financiadores$percent_financed
  #      financiadores <- financiadores %>% group_by(contributors) %>% summarize(paid_ammount = sum(paid_ammount)) %>% adorn_totals("col") %>% dplyr::filter(Total >0)
  #      
  #      financiadores <- merge(financiadores, pdr_financiers, by.x = "contributors", by.y = "percentage_column", all.x = TRUE)
  #      
  #      # financiadores$pdr_financiers
  #      financiadores$percent <-  financiadores$Total/sum(payments_data$paid_ammount)*100
  #      financiadores$contribs <- paste0(financiadores$details_contributors, " (", round(financiadores$percent,2), "%)")
  #      funding <- glue_collapse(financiadores$contribs, ", ", last = " e ")
  # 
  #      sample_doc <- body_replace_all_text(sample_doc, old_value = "FUNDO DE FOMENTO AGRÁRIO E EXTENSÃO RURAL, FUNDO PÚBLICO (FAR, FP)", new_value = toupper(paste0(payments_data$instituicao[1])), only_at_cursor = FALSE, fixed = TRUE)
  #      sample_doc <- body_replace_all_text(sample_doc, old_value = "MINISTÉRIO DA AGRICULTURA E DESENVOLVIMENTO RURAL", new_value = toupper(paste0(payments_data$entidade_governo[1])), only_at_cursor = FALSE, fixed = TRUE)
  #      sample_doc <- body_replace_all_text(sample_doc, old_value = "UNIDADE_GESTORA", new_value = toupper(paste0(payments_data$unidade_gestora[1])), only_at_cursor = FALSE, fixed = TRUE)
  #      sample_doc <- body_replace_all_text(sample_doc, old_value = "supplier_name", new_value = paste0(payments_data$payment_beneficiary[1]), only_at_cursor = FALSE, fixed = TRUE)
  #      sample_doc <- body_replace_all_text(sample_doc, old_value = "submission_date", new_value = paste0(as.character(payments_data$submission_date[1], format = "%d/%m/%Y")), only_at_cursor = FALSE, fixed = TRUE)
  #      sample_doc <- body_replace_all_text(sample_doc, old_value = "subcomponente_pt", new_value = paste0(payments_data$componentnames_pt[1]), only_at_cursor = FALSE, fixed = TRUE)
  #      sample_doc <- body_replace_all_text(sample_doc, old_value = "subcompo_desc", new_value = paste0(payments_data$components_pt[1]), only_at_cursor = FALSE, fixed = TRUE)
  #      sample_doc <- body_replace_all_text(sample_doc, old_value = "sectores", new_value = paste0(payments_data$sectores[1]), only_at_cursor = FALSE, fixed = TRUE)
  #      sample_doc <- body_replace_all_text(sample_doc, old_value = "relevance", new_value = paste0(payments_data$relevance[1]), only_at_cursor = FALSE, fixed = TRUE)
  #      sample_doc <- body_replace_all_text(sample_doc, old_value = "process_type", new_value = paste0(payments_data$process_type[1]), only_at_cursor = FALSE, fixed = TRUE)
  #      
  #      sample_doc <- body_replace_all_text(sample_doc, old_value = "date_proposal_printed", new_value = formated_date, only_at_cursor = FALSE, fixed = TRUE)
  #      sample_doc <- body_replace_all_text(sample_doc, old_value = "month_salaries_paid", new_value = month_salaries_paid, only_at_cursor = FALSE, fixed = TRUE)
  #      sample_doc <- body_replace_all_text(sample_doc, old_value = "year_proposal", new_value = paste0(year_proposal), only_at_cursor = FALSE, fixed = TRUE)
  #      
  #      sample_doc <- body_replace_all_text(sample_doc, old_value = "processos_numeros", new_value = paste0(payments_data$process_number[1]), only_at_cursor = FALSE, fixed = TRUE)
  #      
  #      sample_doc <- body_replace_all_text(sample_doc, old_value = "physical_compl", new_value = paste0(payments_data$physical_compl[1]), only_at_cursor = FALSE, fixed = TRUE)
  #      sample_doc <- body_replace_all_text(sample_doc, old_value = "paid_ammount", new_value = paste0(format(round(sum(payments_data$paid_ammount), 2), big.mark=",", nsmall = 2, scientific=FALSE)), only_at_cursor = FALSE, fixed = TRUE)
  #      sample_doc <- body_replace_all_text(sample_doc, old_value = "funding", new_value = funding, only_at_cursor = FALSE, fixed = TRUE)
  #      sample_doc <- body_replace_all_text(sample_doc, old_value = "new_salute", new_value = paste0(payments_data$new_salute[1]), only_at_cursor = FALSE, fixed = TRUE)
  #      sample_doc <- body_replace_all_text(sample_doc, old_value = "new_approver", new_value = paste0(payments_data$new_approver[1]), only_at_cursor = FALSE, fixed = TRUE)
  #      
  #      sample_doc <- body_replace_all_text(sample_doc, old_value = "name_applicant", new_value = paste0(payments_data$mid_names[1]), only_at_cursor = FALSE, fixed = TRUE)
  #      
  #      sample_doc <- body_replace_all_text(sample_doc, old_value = "financial_compl", new_value = paste0(payments_data$financial_compl[1]), only_at_cursor = FALSE, fixed = TRUE)
  #      sample_doc <- body_replace_all_text(sample_doc, old_value = "e_sistafe_w_code", new_value = paste0(payments_data$e_sistafe_pt[1]), only_at_cursor = FALSE, fixed = TRUE)
  #      sample_doc <- body_replace_all_text(sample_doc, old_value = "documents_numbers", new_value = document, only_at_cursor = FALSE, fixed = TRUE)
  # 
  #      sample_doc <- body_replace_all_text(sample_doc, old_value = "detailed", new_value = paste0(payments_data$expense_description[1]), only_at_cursor = FALSE, fixed = TRUE)
  #      sample_doc <- body_replace_all_text(sample_doc, old_value = "awpb_description", new_value = paste0(payments_data$descricao_da_actividade[1]), only_at_cursor = FALSE, fixed = TRUE)
  #      sample_doc <- body_replace_all_text(sample_doc, old_value = "cost_centers", new_value = paste0(payments_data$cost_centers[1]), only_at_cursor = FALSE, fixed = TRUE)
  #      sample_doc <- body_replace_all_text(sample_doc, old_value = "contractdescription_pt", new_value = paste0(payments_data$contractdescription_pt[1]), only_at_cursor = FALSE, fixed = TRUE)
  #      sample_doc <- body_replace_all_text(sample_doc, old_value = "CONTRACT_NUMBER", new_value = paste0(payments_data$contract_number[1]), only_at_cursor = FALSE, fixed = TRUE)
  #      sample_doc <- body_replace_all_text(sample_doc, old_value = "contract", new_value = paste0(payments_data$contract_number[1]), only_at_cursor = FALSE, fixed = TRUE)
  #      sample_doc <- body_replace_all_text(sample_doc, old_value = "categoria_pdr", new_value = paste0(payments_data$categoria_pdr[1]), only_at_cursor = FALSE, fixed = TRUE)
  #      sample_doc <- body_replace_all_text(sample_doc, old_value = "carreira_proponente", new_value = paste0(payments_data$categoria[1]), only_at_cursor = FALSE, fixed = TRUE)
  #      sample_doc <- body_replace_all_text(sample_doc, old_value = "awpb_id", new_value = paste0(payments_data$awpb_id[1]), only_at_cursor = FALSE, fixed = TRUE)
  #      sample_doc <- footers_replace_all_text(sample_doc, old_value = "encedereco_alvo", new_value = paste0(payments_data$address[1]), only_at_cursor = FALSE, fixed = TRUE)
  #      sample_doc <- footers_replace_all_text(sample_doc, old_value = "cidade_alvo", new_value = paste0(payments_data$city[1]), only_at_cursor = FALSE, fixed = TRUE)
  #      sample_doc <- body_replace_all_text(sample_doc, old_value = "role", new_value = paste0(payments_data$short_vaccancy[1]), only_at_cursor = FALSE, fixed = TRUE)
  #      
  #      
  #      sample_docx <- print(sample_doc, target = file.path(Sys.getenv("Home"), paste0("IP_", payments_data$paao_code[1], "_", input$expense_tipologies, ".docx")))
  #      
  #      return (sample_docx)
  #    },
  #    content <- function(file) {file.copy(filename(), file)},
  #    
  #    contentType = "application/docx"
  #  )

    observeEvent(input$submeter_dossier, priority = 1, {

      qry = paste0("UPDATE fiduciary.procurement_dossiers SET  comments_update = '", paste(input$comments_update),"', ",
                   "last_updated =  '", paste(as.character(format(Sys.Date()))),"', ",
                   "procurement_stage = '", paste(input$procurement_stage),"', ",
                   "proc_methods = '", paste(input$proc_methods),"', ",
                   "eo_i_submission    =  '", lubridate::ymd(if_else(is.na(input$eo_i_submission), ymd(NA),input$eo_i_submission)),"', ",
                   "no_eo_i    =  '", lubridate::ymd(if_else(is.na(input$no_eo_i), ymd(NA),input$no_eo_i)),"', ",
                   "inv_r_eo_i   =  '", lubridate::ymd(if_else(is.na(input$inv_r_eo_i ), ymd(NA),input$inv_r_eo_i )),"', ",
                   "close_r_eo_i  =  '", lubridate::ymd(if_else(is.na(input$close_r_eo_i), ymd(NA),input$close_r_eo_i)),"', ",
                   "report_eo_i   =  '", lubridate::ymd(if_else(is.na(input$report_eo_i ), ymd(NA),input$report_eo_i )),"', ",
                   "no_eo_i_report   =  '", lubridate::ymd(if_else(is.na(input$no_eo_i_report ), ymd(NA),input$no_eo_i_report )),"', ",
                   "rfp_rno   =  '", lubridate::ymd(if_else(is.na(input$rfp_rno ), ymd(NA),input$rfp_rno )),"', ",
                   "rfp_no  =  '", lubridate::ymd(if_else(is.na(input$rfp_no), ymd(NA),input$rfp_no)),"', ",
                   "invitation   =  '", lubridate::ymd(if_else(is.na(input$invitation ), ymd(NA),input$invitation )),"', ",
                   "closing   =  '", lubridate::ymd(if_else(is.na(input$closing ), ymd(NA),input$closing )),"', ",
                   "rno_eva_r   =  '", lubridate::ymd(if_else(is.na(input$rno_eva_r ), ymd(NA),input$rno_eva_r )),"', ",
                   "no_eva_r   =  '", lubridate::ymd(if_else(is.na(input$no_eva_r ), ymd(NA),input$no_eva_r )),"', ",
                   "full_eva_rno  =  '", lubridate::ymd(if_else(is.na(input$full_eva_rno), ymd(NA),input$full_eva_rno)),"', ",
                   "full_eva_no  =  '", lubridate::ymd(if_else(is.na(input$full_eva_no), ymd(NA),input$full_eva_no)),"', ",
                   "noita   =  '", lubridate::ymd(if_else(is.na(input$noita ), ymd(NA),input$noita )),"', ",

                   "updated_by = '", paste(input$updated_by),"'",
                   " WHERE idpp = '", paste(input$idpp),"'")

      dbExecute(pool, qry)
    })

    observeEvent(input$submeter_dossier, priority = 1, {

      qry = paste0("UPDATE fiduciary.procurement_dossiers SET  comments_update = '", paste(input$comments_update),"', ",
                   "contract_usd = ", as.numeric(ifelse(is.na(input$contract_usd),0,input$contract_usd)),", ",

                   "contract_awards   =  '", lubridate::ymd(if_else(is.na(input$contract_awards ), ymd(NA),input$contract_awards )),"', ",
                   "negotiations   =  '", lubridate::ymd(if_else(is.na(input$negotiations ), ymd(NA),input$negotiations )),"', ",
                   "rno_contract   =  '", lubridate::ymd(if_else(is.na(input$rno_contract ), ymd(NA),input$rno_contract )),"', ",
                   "no_contract   =  '", lubridate::ymd(if_else(is.na(input$no_contract ), ymd(NA),input$no_contract )),"', ",
                   "contract_n  =  '", paste(as.character(format(input$contract_n ))),"' ",
                   "WHERE lotes = '", paste(input$awpb_id),"'")

      dbExecute(pool, qry)
      # removeModal()
      showToast("success", "Dossier Actualizado!", .options = myToastOptions)
      # showModal(modalDialog(title=paste0("Parabéns. Dossier actualizado!"), br(), div(tags$b(paste0("SUCESSO!"), style = "color: green;"))))
    })


    observeEvent(input$submeter_actividade, priority = 10, {
      
      SQL_df <- filtered_awpb()
      row_selection <- SQL_df[input$responses_table_rows_selected, "awpb_id"]
      qry = paste0("UPDATE procava.awpb_updates SET  comentarios = '", paste(input$comentarios),"', ", 
                   "national_update_date =  '", paste(as.character(format(Sys.Date(), format="%Y-%m-%d"))),"', ",
                   "situacao_ungp = '", paste(input$situacao_ungp),"', ",
                   "national_notified = '", FALSE,"', ",
                   "national_deadline = '", input$national_deadline,"', ",
                   "national_follower = '", paste0(input$national_follower, collapse = "; "),"', ",
                   "comments_national_pt = '", paste(input$comments_national_pt),"', ",
                   "comments_national_en = '", paste(input$comments_national_en),"', ",
                   "actions_national_pt = '", paste(input$actions_national_pt),"', ",
                   "actions_national_en = '", paste(input$actions_national_en),"' ",
                   " WHERE awpb_id = '", paste(row_selection),"'")
      
      dbGetQuery(pool, qry)
      showToast("success", "Actividade actualizada com sucesso!", .options = myToastOptions)
    })

    
    observeEvent(input$submeter_actividade_sul, priority = 10, {
      SQL_df <- filtered_awpb()
      row_selection <- SQL_df[input$responses_table_rows_selected, "awpb_id"]
      qry = paste0("UPDATE procava.awpb_updates SET  
                   south_update_date =  '", paste(as.character(format(Sys.Date(), format="%Y-%m-%d"))),"', ",
                   "situacao_urgps = '", paste(input$situacao_urgps),"', ",
                   "south_follower = '", paste0(input$south_follower, collapse = "; "),"', ",
                   "south_notified = '", FALSE,"', ",
                   "south_deadline = '", input$south_deadline,"', ",
                   "comments_gaza_pt = '", paste(input$comments_gaza_pt),"', ",
                   "comments_gaza_en = '", paste(input$comments_gaza_en),"', ",
                   "actions_gaza_en = '", paste(input$actions_gaza_en),"', ",
                   "actions_gaza_pt = '", paste(input$actions_gaza_pt),"' ",
                   " WHERE awpb_id = '", paste(row_selection),"'")
      
      dbGetQuery(pool, qry)
      showToast("success", "Actividade actualizada com sucesso!", .options = myToastOptions)
    })
    
      observeEvent(input$submeter_actividade_niassa, priority = 10, {
      SQL_df <- filtered_awpb()
      row_selection <- SQL_df[input$responses_table_rows_selected, "awpb_id"]
      qry = paste0("UPDATE procava.awpb_updates SET  
                   niassa_update_date =  '", paste(as.character(format(Sys.Date(), format="%Y-%m-%d"))),"', ",
                   "situacao_upgpn = '", paste(input$situacao_upgpn),"', ",
                   "niassa_notified = '", FALSE,"', ",
                   "comments_niassa_pt = '", paste(input$comments_niassa_pt),"', ",
                   "niassa_deadline = '", input$niassa_deadline,"', ",
                   "comments_niassa_en = '", paste(input$comments_niassa_en),"', ",
                   "actions_niassa_en = '", paste(input$actions_niassa_en),"', ",
                   "niassa_follower = '", paste0(input$niassa_follower, collapse = "; "),"', ",
                   "actions_niassa_pt = '", paste(input$actions_niassa_pt),"' ",
                   " WHERE awpb_id = '", paste(row_selection),"'")
      
      dbGetQuery(pool, qry)
      showToast("success", "Actividade actualizada com sucesso!", .options = myToastOptions)
    })
    
    
    
    observeEvent(input$recalcular_realizacoes, priority = 10, {
      location_achievements <- DBI::dbGetQuery(pool, "SELECT awpb_id, distrito_beneficiario, value_achieved, province_odk FROM procava.achieved_targets_regions WHERE value_achieved >0")
      location_achievements$value_achieved <- as.numeric(location_achievements$value_achieved)
      
      ###################  PAAO DISTRITAL
      paao_distrital <- location_achievements %>% group_by(awpb_id, distrito_beneficiario) %>% summarize(achieved = sum(value_achieved)) %>% pivot_wider(names_from = distrito_beneficiario, values_from = achieved)
      paao_distrital <- paao_distrital %>% adorn_totals("col", name = "achieved_national") %>% mutate_all(as.character) %>% select(awpb_id, achieved_national, everything())
      for(i in colnames(paao_distrital)) {
        for(j in 1:nrow(paao_distrital)) {
          paao_distrital[j,i] <-  ifelse(is.na(paao_distrital[j,i]), NA, paste0(i, " (", paao_distrital[j,i], ")"))
        }
      }
      
      paao_distrital <- paao_distrital  %>% unite("distrital_locations", 3:ncol(paao_distrital), sep = ", ", na.rm = TRUE, remove = FALSE)
      paao_distrital$achieved_national  <- as.numeric(gsub("[^[:digit:]]+", "", paao_distrital$achieved_national))
      paao_distrital$awpb_id <- gsub("[awpb_id (]", "", paao_distrital$awpb_id)
      paao_distrital$awpb_id <- gsub("[)]", "", paao_distrital$awpb_id)
      paao_distrital <- paao_distrital[, c("awpb_id", "distrital_locations", "achieved_national")]
      
      ###################  PAAO NIASSA
      paao_niassa <- location_achievements %>% fsubset(province_odk %in% c("NIASSA", "UPGPN")) %>% group_by(awpb_id, distrito_beneficiario) %>% summarize(achieved = sum(value_achieved)) %>% pivot_wider(names_from = distrito_beneficiario, values_from = achieved)
      paao_niassa <- paao_niassa %>% adorn_totals("col", name = "achieved_niassa") %>% mutate_all(as.character) %>% select(awpb_id, achieved_niassa, everything())
      
      for(i in colnames(paao_niassa)) {
        for(j in 1:nrow(paao_niassa)) {
          paao_niassa[j,i] <-  ifelse(is.na(paao_niassa[j,i]), NA, paste0(i, " (", paao_niassa[j,i], ")"))
        }
      }
      
      paao_niassa <- paao_niassa  %>% unite("distrital_niassa", 3:ncol(paao_niassa), sep = ", ", na.rm = TRUE, remove = FALSE)
      paao_niassa$achieved_niassa  <- as.numeric(gsub("[^[:digit:]]+", "", paao_niassa$achieved_niassa))
      
      paao_niassa$awpb_id <- gsub("[awpb_id (]", "", paao_niassa$awpb_id)
      paao_niassa$awpb_id <- gsub("[)]", "", paao_niassa$awpb_id)
      paao_niassa <- paao_niassa[, c("awpb_id", "distrital_niassa", "achieved_niassa")]

      ###################  PAAO  ZONA SUL
      paao_sul <- location_achievements %>% fsubset(province_odk %in% c("MAPUTO CIDADE", "URGPS", "GAZA", "MAPUTO", "INHAMBANE")) %>% group_by(awpb_id, distrito_beneficiario) %>% summarize(achieved = sum(value_achieved)) %>% pivot_wider(names_from = distrito_beneficiario, values_from = achieved)
      paao_sul <- paao_sul %>% adorn_totals("col", name = "achieved_sul") %>% mutate_all(as.character) %>% select(awpb_id, achieved_sul, everything())
      
      for(i in colnames(paao_sul)) {
        for(j in 1:nrow(paao_sul)) {
          paao_sul[j,i] <-  ifelse(is.na(paao_sul[j,i]), NA, paste0(i, " (", paao_sul[j,i], ")"))
        }
      }
      
      paao_sul <- paao_sul  %>% unite("distrital_sul", 3:ncol(paao_sul), sep = ", ", na.rm = TRUE, remove = FALSE)
      paao_sul$achieved_sul <- as.numeric(gsub("[^[:digit:]]+", "", paao_sul$achieved_sul))
      
      paao_sul$awpb_id <- gsub("[awpb_id (]", "", paao_sul$awpb_id)
      paao_sul$awpb_id <- gsub("[)]", "", paao_sul$awpb_id)
      paao_sul <- paao_sul[, c("awpb_id", "distrital_sul", "achieved_sul")]
      
      ###################  PROVINCIAL
      provincial <- location_achievements %>% group_by(awpb_id, province_odk) %>% summarize(achieved = sum(value_achieved)) %>% pivot_wider(names_from = province_odk, values_from = achieved)
      provincial <-provincial %>% mutate_all(as.character)
      
      for(i in colnames(provincial)) {
        for(j in 1:nrow(provincial)) {
          provincial[j,i] <-  ifelse(is.na(provincial[j,i]), NA, paste0(i, " (", provincial[j,i], ")"))
        }
      }
      
      provincial <- provincial  %>% unite("province_locations", 3:ncol(provincial), sep = ", ", na.rm = TRUE, remove = FALSE)
      provincial$awpb_id <- gsub("[awpb_id (]", "", provincial$awpb_id)
      provincial$awpb_id <- gsub("[)]", "", provincial$awpb_id)
      provincial <- provincial[, c("awpb_id", "province_locations")]
      paao_distrital <- merge(provincial, paao_distrital, by = "awpb_id")
      paao_distrital <- merge(paao_niassa, paao_distrital, by = "awpb_id")
      paao_distrital <- merge(paao_sul, paao_distrital, by = "awpb_id")
      
      paao_distrital$awpb_id <- gsub(" ", "", paao_distrital$awpb_id)
      
      dbGetQuery(pool, "DELETE FROM procava.district_achievements")
      dbWriteTable(pool, SQL("procava.district_achievements"), value =  paao_distrital, append = TRUE, overwrite = FALSE, row.names = FALSE)

    })

    observeEvent(input$adicionar_realizacao, priority = 10, {
      
      SQL_df <- filtered_awpb()
      row_selection <- SQL_df[input$responses_table_rows_selected, "awpb_id"]

      qry = paste0("INSERT INTO procava.district_achieved (distrito_beneficiario, value_achieved, in_progress, update_date, awpb_id, updated_day, district_awpb)
      VALUES ('", input$distrito_beneficiario, "', ", input$value_achieved, ", ",
                    input$in_progress, ", '", Sys.Date(), "', '", input$awpb_id, "', '",
                    input$updated_day, "', '", toupper(paste0(input$awpb_id, "--", input$distrito_beneficiario)), "')
      
       ON CONFLICT (district_awpb) DO UPDATE SET 
       distrito_beneficiario = EXCLUDED.distrito_beneficiario,
       value_achieved = EXCLUDED.value_achieved,
       in_progress = EXCLUDED.in_progress,
       update_date = EXCLUDED.update_date,
       awpb_id = EXCLUDED.awpb_id,
       updated_day = EXCLUDED.updated_day,
       district_awpb = EXCLUDED.district_awpb;")

      
      dbGetQuery(pool, qry)

      updateSelectizeInput(session, "distrito_beneficiario", selected = "", "Distrito")
      updateSelectizeInput(session, "district_status", selected = "", "Situação no Distrito")
      updateSelectizeInput(session, "target_units", selected = "", "Unidades de Medida")
      updateNumericInput(session, "value_achieved", value = 0, "Realização")
      updateNumericInput(session, "in_progress", value = 0, "Em curso")
      
      showToast("success", "Já pode adicionar outro distrito", "REGISTO COPIADO!", .options = myToastOptions)
    })

    
    
    
    
    observeEvent(input$salvar_pedido, priority = 10, {

      qry = paste0("INSERT INTO fiduciary.payment_proposals(awpb_ids, cost_tabs_fk, relevance, expense_urgencia_processo, is_contract , expense_currency, expense_comments, contract_vs_owner, contract_levels,  expense_description, e_sistafe_w_code, payment_beneficiary,
      nuit_creditado, justificativo_tipo, document_number, unidades_medida, unit_prices, quantity100, quantity30, quantity10, financier_paying, activity_started, activity_ended, applicant_nuit, process_id, process_type, submission_date, product_location, finance_clearance, full_clearance, procurement_clearance, planning_clearance, forex_exchange_rate, expense_detailed_pt, detailed_en,
      ifadloan_pct, ifadgrant_pct, ifadrpsf1_pct, ifadrpsf2_pct, beneficiaryinkind_pct, beneficiarymonetary_pct, privateinkind_pct, privatemoney_pct, governmentinkind_pct, governmentmoney_pct, contract_vs_invoice)
      VALUES ('", input$awpb_ids, "', ",
                   input$cost_tabs_fk, ", '",
                   as.character(input$relevance_justification), "', '",

                   as.character(input$expense_urgencia_processo), "', '",
                   as.character(input$is_contract), "', '",
                   as.character(input$expense_currency), "', '",
                   as.character(input$expense_comments), "', '",

                   input$contract_vs_owner, "', ",
                   as.numeric(input$contract_levels), ", '",
                   input$expense_description, "', ",
                   as.numeric(input$e_sistafe_w_code), ", '",
                   input$payment_beneficiary, "', ",
                   as.numeric(input$nuit_creditado), ", '",
                   as.character(input$justificativo_tipo), "', '",
                   as.character(input$document_number), "', '",
                   as.character(input$unidades_medida), "', ",
                   as.numeric(input$unit_prices), ", ",
                   as.numeric(input$quantity100), ", ",
                   as.numeric(input$quantity30), ", ",
                   as.numeric(input$quantity10), ", '",
                   input$financier_paying, "', '",

                   input$activity_started, "', '",
                   input$activity_ended, "', ",
                   as.numeric(input$applicant_nuit), ", ",
                   input$process_count, ", '",
                   input$process_type, "', '",
                   input$submission_date, "', '",
                   paste0(input$product_location, collapse = "; "), "', '",

                   "FALSE', '",
                   "FALSE', '",
                   "FALSE', '",
                   "FALSE', ",
                   forex_e_rate, ", '",
                   input$expense_detailed_pt, "', '",
                   input$detailed_en, "', ",
                   input$ifadloan_pct, ", ",
                   100 - (input$ifadloan_pct + input$ifadrpsf1_pct + input$ifadrpsf2_pct + input$beneficiaryinkind_pct + input$beneficiarymonetary_pct + input$privateinkind_pct + input$privatemoney_pct + input$governmentinkind_pct + input$governmentmoney_pct),  ", ",
                   input$ifadrpsf1_pct, ", ",
                   input$ifadrpsf2_pct, ", ",
                   input$beneficiaryinkind_pct, ", ",
                   input$beneficiarymonetary_pct, ", ",
                   input$privateinkind_pct, ", ",
                   input$privatemoney_pct, ", ",
                   input$governmentinkind_pct, ", ",
                   input$governmentmoney_pct, ", '",

                   toupper(ifelse(input$is_contract == "SIM", paste0(unlist(strsplit(input$contract_vs_owner, " : .*")), "--", input$justificativo_tipo, "--", input$document_number), UUIDgenerate(use.time = TRUE))),  "')")

      dbGetQuery(pool, qry)

      updateSelectizeInput(session, "contract_vs_owner", selected = "", "Número do Contrato")
      updateNumericInput(session, "contract_levels", value = 0, "Execução física")

      updateTextInput(session, "expense_description", value = '', "Detalhe da Despesa")

      updateSelectizeInput(session, "e_sistafe_w_code", selected = "", "Rubrica CED")

      updateTextInput(session, "payment_beneficiary", value = '', "Beneficiário")

      updateNumericInput(session, "nuit_creditado", value = 0, "NUIT a creditar")
      updateSelectizeInput(session, "justificativo_tipo", selected = "", "Documento de Suporte")

      updateTextInput(session, "document_number", value = "", "Número do Documento")

      updateSelectizeInput(session, "unidades_medida", selected = "", "Unidade")
      updateNumericInput(session, "unit_prices", value = 0, "Preço")
      updateNumericInput(session, "quantity100", value = 0, "Qty@100%")
      updateNumericInput(session, "quantity30", value = 0, "Qty@30%")
      updateNumericInput(session, "quantity10", value = 0, "Qty@10%")

      updateRadioButtons(session, "financier_paying", selected = 'FIDA', "Financiador")

      updateNumericInput(session, "ifadloan_pct", value = 0, "Crédito FIDA (%)")
      updateNumericInput(session, "ifadgrant_pct", value = 0, "Donativo FIDA (%)")
      updateNumericInput(session, "ifadrpsf1_pct", value = 0, "RPSF 1 COVID (%)")
      updateNumericInput(session, "ifadrpsf2_pct", value = 0, "RPSF 2 COVID (%)")
      updateNumericInput(session, "beneficiaryinkind_pct", value = 0, "Beneficiários em Espécie (%)")
      updateNumericInput(session, "beneficiarymonetary_pct", value = 0, "Beneficiários em Dinheiro (%)")
      updateNumericInput(session, "privateinkind_pct", value = 0, "Privados em Espécie (%)")
      updateNumericInput(session, "privatemoney_pct", value = 0, "Privados em Dinheiro (%)")
      updateNumericInput(session, "governmentinkind_pct", value = 0, "Governo em Espécie (%)")
      updateNumericInput(session, "governmentmoney_pct", value = 0, "Governo em Dinheiro (%)")
      # showModal(modalDialog("Despesas registadas com sucesso", easyClose = TRUE, footer = NULL, class = "success"))
      showToast("success", "Já pode adicionar outra despesa!", "DESPESA REGISTADA!", .options = myToastOptions)
    })

    payments_form = modalDialog(title = "PROPOSTA DE PAGAMENTOS", footer = modalButton("Fechar"),
                              div(
                                fluidPage(
                                  fluidRow(

                                    radioGroupButtons("via_de_entrada", "Via de Entrada", choices = c('Excel' = 'Ficheiro Excel', 'e-Form' = "Electronic form"), status = "primary", width = "120px"),
                                    div(style = "display: inline-block;", radioGroupButtons("expense_urgencia_processo", "Urgência do processo", choices = c('NORMAL', 'URGENTE', 'MUITO URGENTE'), status = "primary", checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove",lib = "glyphicon")), width = "350px")),

                                    div(style = "display: inline-block;",
                                        selectizeInput('awpb_ids','Código PAAO', choices = codes, selected = 0, width = "110px")),

                                    div(style = "display: inline-block;",
                                        selectizeInput('cost_tabs_fk','COSTTABS', choices = costab_code, selected = 0, width = "90px")),

                                    div(style = "display: inline-block;", selectizeInput('expense_currency','Moeda', choices = c('MZN', 'USD', 'EUR', 'ZAR', 'GBP'), selected = 'Todos', multiple = FALSE, width = "90px")),

                                    div(style = "display: inline-block;",
                                        dateInput('activity_started', 'Início', width = "90px")),

                                    div(style = "display: inline-block;",
                                        dateInput('activity_ended', 'Término', width = "90px")),

                                    div(style = "display: inline-block;",
                                        selectizeInput('applicant_nuit','Proponente', choices = c('', 'Amâncio António Nhantumbo' = '106803609', 'Andércio Vitane' = '111175047', 'Augusto Oreste' = '118185331', 'Luísa Ângela Josselina Calima' = '128504222', 'Daniel Lourenço Chitupila' = '103847915', 'Dionísia Castelo Machuza Cuna' = '123200871', 'Eduardo Marcos Cuamba' = '107958584', 'Ana Crimilda Fernando Silva' = '111730814', 'Baptista Ruben Ngine Zunguze' = '103149061', 'Ernesto Abrantes Dulamo Wane' = '104922521', 'Daniel Ozias Mate' = '103659124', 'Egídio Artur Alfredo Mutimba' = '103260116', 'Esperança David Muchanga' = '109197580', 'Eugénio Nhone' = '102542762', 'Gil Estevão Nhantumbo' = '100894343', 'Jerónimo Joaquim Francisco' = '110589123', 'Joaquim Daniel Macaringue' = '300249248', 'José Sancho Cumbi' = '100870630', 'Júlio Aguiar Bila' = '106864233', 'Júlio Marcelino Macaco' = '103160987', 'Lucas Albino Chiau' = '119429897', 'Lucília Santos' = '100864665', 'Manuel Tinga Mangueze' = '100868113', 'Neide Custódio Daniel' = '102032330', 'Neila Lúcia da Conceição Manjate' = '110757891', 'Rachida Jafar Abdul' = '115695673', 'Ibraimo Assuade Assane' = '104468241', 'Tiago Tiago' = '112841822', 'Joaquim Daniel Macaringue' = '108596120', 'Ámina Amade Mussá Faquirá' = '101202429', 'Nilza Racide Abdul Adolfo' = '118151771', 'Osvaldo Lázaro Banze' = '122094545', 'Rosário Aide' = '103846501'), selected = '', multiple = FALSE, width = "350px")),

                                    div(style = "display: inline-block;",
                                        numericInput('process_count', 'Processo N.º', value = 0, width = "120px")),

                                    div(style = "display: inline-block;",
                                        selectizeInput('process_type','Tipo de processo', choices = c('', process_types), selected = '', multiple = FALSE, width = "350px")),

                                    div(style = "display: inline-block;",
                                        dateInput('submission_date', 'Submissão', width = "120px")),

                                    div(style = "display: inline-block;",
                                        selectizeInput('product_location','Localização do Produto', choices = distritos_procava, selected = '', multiple = TRUE, width = "470")),

                                    textInput('expense_detailed_pt', 'Actividade Específica (Português)', width = "100%"),
                                    textInput('detailed_en', 'Actividade Específica (Inglês)', width = "100%"),
                                    textAreaInput('relevance_justification', 'Relevância da actividade', width = "100%"),

                                    br(),
                                    div(style = "height:30px"),

                                    textAreaInput('expense_comments', 'Comentários', width = "100%"),

                                    conditionalPanel(condition = "input.via_de_entrada == 'Electronic form'",
                                                     actionButton("optimizations","LISTAGEM DE DESPESAS ASSOCIADAS", class = "run-button", style="color: #fff; background-color: #D2691E; border-color: #D2691E; height: 30px; width:470px;")),

                                    conditionalPanel(condition = "input.via_de_entrada == 'Electronic form'",

                                                     radioGroupButtons(inputId = "is_contract", label = "É Contrato?", choices = c('SIM', 'NÃO'), selected = "NÃO", status = "primary", checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove",lib = "glyphicon")), width = "100%")),


                                    # div(style = "height:30px"),
                                    div(
                                      style = "display: inline-block;", conditionalPanel(condition = "input.is_contract == 'SIM' && input.via_de_entrada == 'Electronic form'",
                                                                                         selectizeInput('contract_vs_owner','Número do Contrato', choices = c("", contract_numbers), selected = '', multiple = FALSE, width = "470px"))),

                                    div(
                                      style = "display: inline-block;", conditionalPanel(condition = "input.is_contract == 'SIM' && input.via_de_entrada == 'Electronic form'",
                                                                                         shiny::numericInput(inputId = "contract_levels", label = "Execução física", value =  0, width = "470px"))),

                                    div(style = "display: inline-block;", conditionalPanel(condition = "input.via_de_entrada == 'Electronic form'",
                                                                                           shiny::textInput(inputId = "expense_description", label = "Detalhe da Despesa", width = "470px"))),

                                    div(style = "display: inline-block;", conditionalPanel(condition = "input.via_de_entrada == 'Electronic form'",
                                                                                           shiny::selectizeInput(inputId = "e_sistafe_w_code", 'Rubrica CED', choices = ced_coding, selected = '', multiple = FALSE, width = "470px"))),


                                    div(style = "display: inline-block;", conditionalPanel(condition = "input.via_de_entrada == 'Electronic form'",
                                                                                           shiny::textInput(inputId = "payment_beneficiary", label = "Beneficiário", width = "320px"))),

                                    div(style = "display: inline-block;", conditionalPanel(condition = "input.via_de_entrada == 'Electronic form'",
                                                                                           shiny::numericInput(inputId = "nuit_creditado", label = "NUIT a creditar", value =  0, width = "150px"))),

                                    div(style = "display: inline-block;", conditionalPanel(condition = "input.via_de_entrada == 'Electronic form'",
                                                                                           shiny::selectizeInput(inputId = "justificativo_tipo", 'Documento de Suporte', choices = c('', 'Declaração', 'Factura de Cobrança',
                                                                                                                                                                                     'Factura Proforma', 'Folha de Salários',
                                                                                                                                                                                     'Guia de Marcha', 'Guia de Entrega',
                                                                                                                                                                                     'Livro de Bordo', 'Relatório', 'Termos de Referência', 'Auto de Medições',
                                                                                                                                                                                     'Venda à Dinheiro', 'Outro'), selected = '', multiple = FALSE, width = "320px"))),


                                    div(style = "display: inline-block;", conditionalPanel(condition = "input.via_de_entrada == 'Electronic form'",
                                                                                           shiny::textInput(inputId = "document_number", label = "Número do Documento", width = "150px"))),

                                    div(style = "display: inline-block;", conditionalPanel(condition = "input.via_de_entrada == 'Electronic form'",
                                                                                           shiny::selectizeInput(inputId = "unidades_medida", 'Unidade', choices = c('Dias', 'Lum Sum', procava_measurement_units), selected = 'Todos', multiple = FALSE, width = "110px"))),

                                    div(style = "display: inline-block;", conditionalPanel(condition = "input.via_de_entrada == 'Electronic form'",
                                                                                           autonumericInput("unit_prices","Preço", value = 0, align = "left", currencySymbol = '', currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", formulaMode = TRUE, width = "115px"))),
                                                                                           # numericInput('unit_prices', 'Preço', value =  0, width = "100px"))),
                                    div(style = "display: inline-block;", conditionalPanel(condition = "input.via_de_entrada == 'Electronic form'",
                                                                                           # numericInput('quantity100', 'Qty@100%', value =  0, width = "80px"),
                                                                                           autonumericInput("quantity100","Qty@100%", value = 0, align = "left", currencySymbol = '', currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", formulaMode = TRUE, width = "75px"))),
                                    div(style = "display: inline-block;",  conditionalPanel(condition = "input.via_de_entrada == 'Electronic form'",
                                                                                            autonumericInput("quantity30","Qty@30%", value = 0, align = "left", currencySymbol = '', currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", formulaMode = TRUE, width = "75px"))),
                                    div(style = "display: inline-block;", conditionalPanel(condition = "input.via_de_entrada == 'Electronic form'",
                                                                                           autonumericInput("quantity10","Qty@10%", value = 0, align = "left", currencySymbol = '', currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", formulaMode = TRUE, width = "75px"))),

                                    conditionalPanel(condition = "input.via_de_entrada == 'Electronic form'",
                                                     radioGroupButtons(inputId = "financier_paying", label = "Financiador", choices = c('FIDA', 'COVID', 'Beneficiários', 'Governo', 'Privados'), status = "primary", checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove",lib = "glyphicon")), width = "100%")),
                                                     # radioButtons('financier_paying', 'Financiador', choices = c('FIDA', 'COVID', 'Beneficiários', 'Governo', 'Privados'),inline = TRUE, width = "100%")),

                                    div(style = "display: inline-block;", conditionalPanel(condition = "input.financier_paying == 'FIDA' && input.via_de_entrada == 'Electronic form'",
                                                                                           shiny::numericInput(inputId = "ifadloan_pct", label = "Crédito FIDA (%)", value = 0, width = "240px"))),

                                    div(style = "display: inline-block;", conditionalPanel(condition = "input.financier_paying == 'FIDA' && input.via_de_entrada == 'Electronic form'",
                                                                                           shiny::numericInput(inputId = "ifadgrant_pct", label = "Donativo FIDA (%)", value = 0, width = "230px"))),

                                    div(style = "display: inline-block;", conditionalPanel(condition = "input.financier_paying == 'COVID' && input.via_de_entrada == 'Electronic form'",
                                                                                           shiny::numericInput(inputId = "ifadrpsf1_pct", label = "RPSF 1 COVID (%)", value = 0, width = "240px"))),

                                    div(style = "display: inline-block;", conditionalPanel(condition = "input.financier_paying == 'COVID' && input.via_de_entrada == 'Electronic form'",
                                                                                           shiny::numericInput(inputId = "ifadrpsf2_pct", label = "RPSF 2 COVID (%)", value = 0, width = "230px"))),

                                    div(style = "display: inline-block;", conditionalPanel(condition = "input.financier_paying == 'Beneficiários' && input.via_de_entrada == 'Electronic form'",
                                                                                           shiny::numericInput(inputId = "beneficiaryinkind_pct", label = "Beneficiários em Espécie (%)", value = 0, width = "240px"))),

                                    div(style = "display: inline-block;",conditionalPanel(condition = "input.financier_paying == 'Beneficiários' && input.via_de_entrada == 'Electronic form'",
                                                                                          shiny::numericInput(inputId = "beneficiarymonetary_pct", label = "Beneficiários em Dinheiro (%)", value = 0, width = "230px"))),

                                    div(style = "display: inline-block;", conditionalPanel(condition = "input.financier_paying == 'Privados' && input.via_de_entrada == 'Electronic form'",
                                                                                           shiny::numericInput(inputId = "privateinkind_pct", label = "Privados em Espécie (%)", value = 0, width = "240px"))),

                                    div(style = "display: inline-block;",conditionalPanel(condition = "input.financier_paying == 'Privados' && input.via_de_entrada == 'Electronic form'",
                                                                                          shiny::numericInput(inputId = "privatemoney_pct", label = "Privados em Dinheiro (%)", value = 0, width = "230px"))),

                                    div(style = "display: inline-block;", conditionalPanel(condition = "input.financier_paying == 'Governo' && input.via_de_entrada == 'Electronic form'",
                                                                                           shiny::numericInput(inputId = "governmentinkind_pct", label = "Governo em Espécie (%)", value = 0, width = "240px"))),

                                    div(style = "display: inline-block;",conditionalPanel(condition = "input.financier_paying == 'Governo' && input.via_de_entrada == 'Electronic form'",
                                                                                          shiny::numericInput(inputId = "governmentmoney_pct", label = "Governo em Dinheiro (%)", value = 0, width = "230px"))),

                                    # splitLayout(cellWidths = c("300px"),
                                    conditionalPanel(condition = "input.via_de_entrada == 'Electronic form'",
                                                     # actionButton("salvar_pedido", "COMETER DESPESAS e-FORM", style = "color: white;  background-color: blue; ", icon=icon("plus"))
                                                     actionButton("salvar_pedido",  "SALVAR", style = "color: white;  background-color: green; ", icon=icon("save"))),



                                    # conditionalPanel(condition = "input.via_de_entrada == 'Electronic form'",
                                    #                  actionButton("optimize","", class = "run-button", style="color: #fff; background-color: #D2691E; border-color: #D2691E; height: 10px; width:470px;")),


                                    ####################  PAGAMENTOS NORMAIS

                                    div(style = "display: inline-block;", conditionalPanel(condition = "input.via_de_entrada == 'Ficheiro Excel' && (input.financier_paying == 'FIDA' || input.financier_paying == 'COVID')",
                                                                                           fileInput('payments_xls_fida', '', accept = c(".xlsx", ".xls"), width = "470px"))),

                                    div(style = "display: inline-block;", conditionalPanel(condition = "input.via_de_entrada == 'Ficheiro Excel' && (input.financier_paying == 'FIDA' || input.financier_paying == 'COVID')",
                                                                                           DT::dataTableOutput("visualizar_xlsx_fida"))),

                                    div(style = "display: inline-block;", conditionalPanel(condition = "input.via_de_entrada == 'Ficheiro Excel' && (input.financier_paying == 'FIDA' || input.financier_paying == 'COVID')",
                                                                                           # actionButton("salvar_xls_ip", "COMETER DESPESA", style = "color: white;  background-color: blue; ", icon=icon("plus"))
                                                                                           actionButton("salvar_xls_ip",  "SALVAR", style = "color: white;  background-color: green; ", icon=icon("save")))),



                                    ################################### COMPARTICIPACOES

                                    div(style = "display: inline-block;", conditionalPanel(condition = "input.via_de_entrada == 'Ficheiro Excel' && input.financier_paying != 'FIDA' && input.financier_paying != 'COVID'",
                                                                                           fileInput('payments_xls_comparticipacao', '', accept = c(".xlsx", ".xls"), width = "470px"))),

                                    div(style = "display: inline-block;", conditionalPanel(condition = "input.via_de_entrada == 'Ficheiro Excel' && input.financier_paying != 'FIDA' && input.financier_paying != 'COVID'",
                                                                                           DT::dataTableOutput("visualizar_xlsx_comparticipacao"))),

                                    div(style = "display: inline-block;", conditionalPanel(condition = "input.via_de_entrada == 'Ficheiro Excel' && input.financier_paying != 'FIDA' && input.financier_paying != 'COVID'",
                                                                                           # actionButton("salvar_xls_comparticipacao", "COMETER DESPESA", style = "color: white;  background-color: blue; ", icon=icon("plus"))
                                                                                           actionButton("salvar_xls_comparticipacao",  "SALVAR", style = "color: white;  background-color: green; ", icon=icon("save"))))


                                  ),

                                  size = "l",

                                  footer = modalButton("Fechar"),
                                  easyClose = TRUE
                                )
                              ))

new_awpb_update_form = modalDialog(title = "ACTUALIZAÇÃO DO PAAO", footer = modalButton("Fechar"),
                                   div(id=("new_awpb_update_form"),
                                       tags$head(tags$style(".modal-dialog{ width:500px}")),
                                       tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
                                       fluidPage(
                                         fluidRow(
                                           div(style = "display: inline-block;",
                                               radioGroupButtons("idioma_update", "", choices = c("Português" = "PT", "English" = "EN"), status = "warning", checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")))
                                               ),

                                           div(style = "display: inline-block;",
                                               radioGroupButtons("unidade_de_management", "", choices = c("UNGP", "URGPS", "UPGPN"), status = "primary", checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")))
                                               ),
                                           br(),

                                           radioGroupButtons(inputId = "form_choice",label = "", choices = c("Actualizar PAAO", "Realizado", "Actualizar PP", "Programar"), 
                                                             individual = TRUE, checkIcon = list(yes = tags$i(class = "fa fa-circle", style = "color: red"), no = tags$i(class = "fa fa-circle-o",  style = "color: steelblue"))),

                                           conditionalPanel(condition = "input.form_choice == 'Actualizar PAAO'", 
                                                            div(style = "display: inline-block;", selectInput("awpb_id", labelMandatory("Código"), multiple = FALSE, choices = as.character(unique(awpb_updated$awpb_id)), width = "120px"))),
                                           
                                           div(style = "display: inline-block;", conditionalPanel(condition = "input.unidade_de_management == 'UPGPN' && input.form_choice == 'Actualizar PAAO'",
                                                                                                  selectInput("situacao_upgpn", labelMandatory("Situação"), multiple = FALSE, choices = awpb_situacoes, width = "350px"))),

                                           div(style = "display: inline-block;", conditionalPanel(condition = "input.unidade_de_management == 'UNGP' && input.form_choice == 'Actualizar PAAO'", 
                                                                                                  selectInput("situacao_ungp", labelMandatory("Situação"), multiple = FALSE, choices = awpb_situacoes, width = "350px"))),
       
                                           div(style = "display: inline-block;", conditionalPanel(condition = "input.unidade_de_management == 'URGPS' && input.form_choice == 'Actualizar PAAO'", 
                                                                                                  selectInput("situacao_urgps", labelMandatory("Situação"), multiple = FALSE, choices = awpb_situacoes, width = "350px"))),
                                           
                                           conditionalPanel(condition = "input.form_choice == 'Actualizar PAAO'", 
                                           div(style = "display: inline-block;", selectInput("financing_category", "Categoria", multiple = FALSE, choices = as.character(unique(awpb_updated$categories)), width = "470px"))),
                                           conditionalPanel(condition = "input.form_choice == 'Actualizar PAAO'", 
                                           div(style = "display: inline-block;", selectInput("activity_financiers", "Fontes de Recursos", multiple = FALSE, choices = as.character(unique(awpb_updated$financiers)), width = "470px"))),
                                           
                                           conditionalPanel(condition = "input.idioma_update == 'PT' && input.unidade_de_management == 'UPGPN'  && input.form_choice == 'Actualizar PAAO'", 
                                           textAreaInput("comments_niassa_pt", "Descrição do Ponto de Situação", placeholder = "", height = 100, width = "470px")),
                                           conditionalPanel(condition = "input.idioma_update == 'EN' && input.unidade_de_management == 'UPGPN'  && input.form_choice == 'Actualizar PAAO'", 
                                           textAreaInput("comments_niassa_en", "Detailed Status Description", placeholder = "", height = 100, width = "470px")),
                                           
                                           conditionalPanel(condition = "input.idioma_update == 'PT' && input.unidade_de_management == 'URGPS'  && input.form_choice == 'Actualizar PAAO'", 
                                           textAreaInput("comments_gaza_pt", "Descrição do Ponto de Situação", placeholder = "", height = 100, width = "470px")),
                                           conditionalPanel(condition = "input.idioma_update == 'EN' && input.unidade_de_management == 'URGPS'  && input.form_choice == 'Actualizar PAAO'", 
                                           textAreaInput("comments_gaza_en", "Detailed Status Description", placeholder = "", height = 100, width = "470px")),
                                           
                                           conditionalPanel(condition = "input.idioma_update == 'PT' && input.unidade_de_management == 'UNGP'  && input.form_choice == 'Actualizar PAAO'", 
                                           textAreaInput("comments_national_pt", "Descrição do Ponto de Situação", placeholder = "", height = 100, width = "470px")),
                                           conditionalPanel(condition = "input.idioma_update == 'EN' && input.unidade_de_management == 'UNGP'  && input.form_choice == 'Actualizar PAAO'", 
                                           textAreaInput("comments_national_en", "Detailed Status Description", placeholder = "", height = 100, width = "470px")),
                                           
                                           conditionalPanel(condition = "input.idioma_update == 'PT' && input.unidade_de_management == 'UPGPN'  && input.form_choice == 'Actualizar PAAO'", 
                                           textAreaInput("actions_niassa_pt", "Recomendações e Acções de Seguimento", placeholder = "", height = 100, width = "470px")),
                                           conditionalPanel(condition = "input.idioma_update == 'EN' && input.unidade_de_management == 'UPGPN'  && input.form_choice == 'Actualizar PAAO'", 
                                           textAreaInput("actions_niassa_en", "Recommendations and Actions", placeholder = "", height = 100, width = "470px")),
                                           
                                           conditionalPanel(condition = "input.idioma_update == 'PT' && input.unidade_de_management == 'UNGP'  && input.form_choice == 'Actualizar PAAO'", 
                                           textAreaInput("actions_national_pt", "Recomendações e Acções de Seguimento", placeholder = "", height = 100, width = "470px")),
                                           conditionalPanel(condition = "input.idioma_update == 'EN' && input.unidade_de_management == 'UNGP'  && input.form_choice == 'Actualizar PAAO'", 
                                           textAreaInput("actions_national_en", "Recommendations and Actions", placeholder = "", height = 100, width = "470px")),
                                           
                                           conditionalPanel(condition = "input.idioma_update == 'EN' && input.unidade_de_management == 'URGPS'  && input.form_choice == 'Actualizar PAAO'", 
                                           textAreaInput("actions_gaza_en", "Recommendations and Actions", placeholder = "", height = 100, width = "470px")),

                                           conditionalPanel(condition = "input.idioma_update == 'PT' && input.unidade_de_management == 'URGPS'  && input.form_choice == 'Actualizar PAAO'", 
                                           textAreaInput("actions_gaza_pt", "Recomendações e Acções de Seguimento", placeholder = "", height = 100, width = "470px")),
                                           
                                           conditionalPanel(condition = "input.unidade_de_management == 'UNGP'  && input.form_choice == 'Actualizar PAAO'", 
                                                            div(style = "display: inline-block;", selectizeInput("national_follower", "Responsável pelo seguimento (Follow up responsible)",  choices = c("", staff_nuit), multiple = TRUE, width = "350px"))),
                                           conditionalPanel(condition = "input.unidade_de_management == 'UNGP'  && input.form_choice == 'Actualizar PAAO'",
                                                            div(style = "display: inline-block;", dateInput("national_deadline", "Prazo/Deadline", value = Sys.Date()+7, width = "100px"))),
                                           
                                           conditionalPanel(condition = "input.unidade_de_management == 'URGPS'  && input.form_choice == 'Actualizar PAAO'", 
                                                            div(style = "display: inline-block;", selectizeInput("south_follower", "Responsável do seguimento (Follow up responsible)",  choices = c("", staff_nuit), multiple = TRUE, width = "350px"))),
                                           conditionalPanel(condition = "input.unidade_de_management == 'URGPS'  && input.form_choice == 'Actualizar PAAO'",                
                                                            div(style = "display: inline-block;", dateInput("south_deadline", "Prazo/Deadline", value = Sys.Date()+7, width = "100px"))),
                                           
                                           conditionalPanel(condition = "input.unidade_de_management == 'UPGPN'   && input.form_choice == 'Actualizar PAAO'", 
                                                            div(style = "display: inline-block;", selectizeInput("niassa_follower", "Responsável do seguimento (Follow up responsible)",  choices = c("", staff_nuit), multiple = TRUE, width = "350px"))),
                                           conditionalPanel(condition = "input.unidade_de_management == 'UPGPN'   && input.form_choice == 'Actualizar PAAO'", 
                                                            div(style = "display: inline-block;", dateInput("niassa_deadline", "Prazo/Deadline", value = Sys.Date()+7, width = "100px"))),

                                           conditionalPanel(condition = "input.form_choice == 'Realizado'", div(style = "display: inline-block;", selectInput("awpb_idt", labelMandatory("Código"), multiple = FALSE, choices = as.character(unique(awpb_updated$awpb_id)), width = "120px"))),
                                                            
                                           conditionalPanel(condition = "input.form_choice == 'Realizado'", div(style = "display: inline-block;", selectInput("target_units", "Unidades de Medida", multiple = FALSE, choices = c('', as.character(unique(awpb_updated$unidades))), width = "220px"))), 
                                           conditionalPanel(condition = "input.form_choice == 'Realizado'", div(style = "display: inline-block;", dateInput('updated_day',label='Realização', width = "130px"))),
           
                                           ############ Realizado no distrito
                                           conditionalPanel(condition = "input.form_choice == 'Realizado'", div(style = "display: inline-block;", selectizeInput("distrito_beneficiario", labelMandatory("Distrito ou Unidade"), multiple = FALSE, choices = c("", distritos_procava), selected = "", width = "170px"))),
                                           conditionalPanel(condition = "input.form_choice == 'Realizado'", div(style = "display: inline-block;", autonumericInput("in_progress","Em curso", value = 0, align = "left", currencySymbol = '', currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", formulaMode = TRUE, width = "150px"))),
                                           conditionalPanel(condition = "input.form_choice == 'Realizado'", div(style = "display: inline-block;", autonumericInput("value_achieved","Concluídos", value = 0, align = "left", currencySymbol = '', currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", formulaMode = TRUE, width = "150px"))),

                                           br(),
                                           conditionalPanel(condition = "input.form_choice == 'Realizado'", actionButton("adicionar_realizacao", "SALVAR", style = "color: white;  background-color: green; ", icon=icon("save"))),
                                           br(),
                                           div(style = "height:30px"),
              
                                                           conditionalPanel(condition = "input.form_choice == 'Programar' && input.unidade_de_management == 'UNGP'",
                                                                            div(style = "display: inline-block;", autonumericInput("orcamento_revisto_ungp","Orçamento Revisto (US$)", value = 0, align = "right", currencySymbol = '', currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", formulaMode = TRUE, width = "160px"))),
                                                            
                                                           conditionalPanel(condition = "input.form_choice == 'Programar' && input.unidade_de_management == 'UNGP'",
                                                                            div(style = "display: inline-block;", autonumericInput("paao_ungp","Meta", value = 0, align = "right", currencySymbol = '', currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", formulaMode = TRUE, width = "135px"))),
                                                           
                                                           conditionalPanel(condition = "input.form_choice == 'Programar' && input.unidade_de_management == 'UNGP'",
                                                                            div(style = "display: inline-block;", shiny::selectizeInput(inputId = "unidades_medida_ungp", 'Unidade', choices = units_indicators, multiple = FALSE, width = "160px"))),
                                                            
                                          conditionalPanel(condition = "input.form_choice == 'Programar' && input.unidade_de_management == 'UNGP'",
                                                                                                                        
                                                            div(style = "display: inline-block;", autonumericInput("jan_rev_ungp","JANEIRO", value = 0, align = "right", currencySymbol = '', currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", formulaMode = TRUE, width = "115px")),
                                                            div(style = "display: inline-block;", autonumericInput("feb_rev_ungp","FEVEREIRO", value = 0, align = "right", currencySymbol = '', currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", formulaMode = TRUE, width = "115px")),
                                                            div(style = "display: inline-block;", autonumericInput("mar_rev_ungp","MARÇO", value = 0, align = "right", currencySymbol = '', currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", formulaMode = TRUE, width = "115px")),
                                                            div(style = "display: inline-block;", autonumericInput("apr_rev_ungp","ABRIL", value = 0, align = "right", currencySymbol = '', currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", formulaMode = TRUE, width = "115px")),
                                                            div(style = "display: inline-block;", autonumericInput("may_rev_ungp","MAIO", value = 0, align = "right", currencySymbol = '', currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", formulaMode = TRUE, width = "115px")),
                                                            div(style = "display: inline-block;", autonumericInput("jun_rev_ungp","JUNHO", value = 0, align = "right", currencySymbol = '', currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", formulaMode = TRUE, width = "115px")),

                                                            div(style = "display: inline-block;", autonumericInput("jul_rev_ungp","JULHO", value = 0, align = "right", currencySymbol = '', currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", formulaMode = TRUE, width = "115px")),
                                                            div(style = "display: inline-block;", autonumericInput("ago_rev_ungp","AGOSTO", value = 0, align = "right", currencySymbol = '', currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", formulaMode = TRUE, width = "115px")),
                                                            div(style = "display: inline-block;", autonumericInput("sep_rev_ungp","SETEMBRO", value = 0, align = "right", currencySymbol = '', currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", formulaMode = TRUE, width = "115px")),
                                                            div(style = "display: inline-block;", autonumericInput("oct_rev_ungp","OUTUBRO", value = 0, align = "right", currencySymbol = '', currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", formulaMode = TRUE, width = "115px")),
                                                            div(style = "display: inline-block;", autonumericInput("nov_rev_ungp","NOVEMBRO", value = 0, align = "right", currencySymbol = '', currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", formulaMode = TRUE, width = "115px")),
                                                            div(style = "display: inline-block;", autonumericInput("dec_rev_ungp","DEZEMBRO", value = 0, align = "right", currencySymbol = '', currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", formulaMode = TRUE, width = "115px")),
                                                            # div(style = "display: inline-block;", autonumericInput("paao_ungp","TOTAL", value = 0, align = "right", currencySymbol = '', currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", formulaMode = TRUE, width = "470px")), # div(style = "display: inline-block;", dateInput('updated_day',label='Realização', width = "120px")),
                                                            # div(style = "display: inline-block;", autonumericInput("orcamento_revisto_ungp","Proposta de Revisão Orçamental (US$)", value = 0, align = "right", currencySymbol = '', currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", formulaMode = TRUE, width = "370px")),
                                                            textAreaInput("revision_comments_ungp", "Comentários da Revisão", placeholder = "Justifique a necessidade de alteração", height = 100, width = "470px"),
                                                            br(),
                                                            
                                                            actionButton("salvar_metas_alteradas_ungp",  "SALVAR", style = "color: white;  background-color: green; ", icon=icon("save")),
                                                            br()
                                                           ),
                                           
                                           br(),
                                           div(style = "height:30px"),
                                          conditionalPanel(condition = "input.form_choice == 'Programar' && input.unidade_de_management == 'URGPS'",div(style = "display: inline-block;", autonumericInput("orcamento_revisto_urgps","Orçamento Revisto (US$)", value = 0, align = "right", currencySymbol = '', currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", formulaMode = TRUE, width = "160px"))),
                                          conditionalPanel(condition = "input.form_choice == 'Programar' && input.unidade_de_management == 'URGPS'",div(style = "display: inline-block;", autonumericInput("paao_urgps","Meta", value = 0, align = "right", currencySymbol = '', currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", formulaMode = TRUE, width = "135px"))),
                                          conditionalPanel(condition = "input.form_choice == 'Programar' && input.unidade_de_management == 'URGPS'",div(style = "display: inline-block;", shiny::selectizeInput(inputId = "unidades_medida_urgps", 'Unidade', choices = units_indicators, multiple = FALSE, width = "160px"))),
                                          conditionalPanel(condition = "input.form_choice == 'Programar'  && input.unidade_de_management == 'URGPS'",                  
                                                            div(style = "display: inline-block;", autonumericInput("jan_rev_urgps","JANEIRO", value = 0, align = "right", currencySymbol = '', currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", formulaMode = TRUE, width = "115px")),
                                                            div(style = "display: inline-block;", autonumericInput("feb_rev_urgps","FEVEREIRO", value = 0, align = "right", currencySymbol = '', currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", formulaMode = TRUE, width = "115px")),
                                                            div(style = "display: inline-block;", autonumericInput("mar_rev_urgps","MARÇO", value = 0, align = "right", currencySymbol = '', currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", formulaMode = TRUE, width = "115px")),
                                                            div(style = "display: inline-block;", autonumericInput("apr_rev_urgps","ABRIL", value = 0, align = "right", currencySymbol = '', currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", formulaMode = TRUE, width = "115px")),
                                                            div(style = "display: inline-block;", autonumericInput("may_rev_urgps","MAIO", value = 0, align = "right", currencySymbol = '', currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", formulaMode = TRUE, width = "115px")),
                                                            div(style = "display: inline-block;", autonumericInput("jun_rev_urgps","JUNHO", value =0, align = "right", currencySymbol = '', currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", formulaMode = TRUE, width = "115px")),
                                                            div(style = "display: inline-block;", autonumericInput("jul_rev_urgps","JULHO", value = 0, align = "right", currencySymbol = '', currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", formulaMode = TRUE, width = "115px")),
                                                            div(style = "display: inline-block;", autonumericInput("ago_rev_urgps","AGOSTO", value = 0, align = "right", currencySymbol = '', currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", formulaMode = TRUE, width = "115px")),
                                                            div(style = "display: inline-block;", autonumericInput("sep_rev_urgps","SETEMBRO", value = 0, align = "right", currencySymbol = '', currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", formulaMode = TRUE, width = "115px")),
                                                            div(style = "display: inline-block;", autonumericInput("oct_rev_urgps","OUTUBRO", value = 0, align = "right", currencySymbol = '', currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", formulaMode = TRUE, width = "115px")),
                                                            div(style = "display: inline-block;", autonumericInput("nov_rev_urgps","NOVEMBRO", value = 0, align = "right", currencySymbol = '', currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", formulaMode = TRUE, width = "115px")),
                                                            div(style = "display: inline-block;", autonumericInput("dec_rev_urgps","DEZEMBRO", value = 0, align = "right", currencySymbol = '', currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", formulaMode = TRUE, width = "115px")),                                                            # div(style = "display: inline-block;", dateInput('updated_day',label='Realização', width = "120px")),
                                                            # div(style = "display: inline-block;", autonumericInput("paao_urgps","TOTAL", value = 0, align = "right", currencySymbol = '', currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", formulaMode = TRUE, width = "470px")),
                                                            textAreaInput("revision_comments_urgps", "Comentários da Revisão", placeholder = "Justifique a necessidade de alteração", height = 100, width = "470px"),
                                                            
                                                            br(),
                                                            
                                                            actionButton("salvar_metas_alteradas_urgps",  "SALVAR", style = "color: white;  background-color: green; ", icon=icon("save")),
                                                            br()
                                                           ),
                                          
                                          conditionalPanel(condition = "input.form_choice == 'Programar'  && input.unidade_de_management == 'UPGPN'",div(style = "display: inline-block;", autonumericInput("orcamento_revisto_upgpn","Orçamento Revisto (US$)", value = 0, align = "right", currencySymbol = '', currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", formulaMode = TRUE, width = "160px"))),
                                          conditionalPanel(condition = "input.form_choice == 'Programar' && input.unidade_de_management == 'UPGPN'",div(style = "display: inline-block;", autonumericInput("paao_upgpn","Meta", value = 0, align = "right", currencySymbol = '', currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", formulaMode = TRUE, width = "135px"))),
                                          conditionalPanel(condition = "input.form_choice == 'Programar'  && input.unidade_de_management == 'UPGPN'",div(style = "display: inline-block;", shiny::selectizeInput(inputId = "unidades_medida_upgpn", 'Unidade', choices = units_indicators, multiple = FALSE, width = "160px"))),
                                                            
                                                            conditionalPanel(condition = "input.form_choice == 'Programar'  && input.unidade_de_management == 'UPGPN'",
                                                            # autonumericInput("orcamento_revisto_upgpn","Proposta de Revisão Orçamental (US$)", value = 0, align = "right", currencySymbol = '', currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", formulaMode = TRUE, width = "160px")),
                                                            div(style = "display: inline-block;", autonumericInput("jan_rev_upgpn","JANEIRO", value = 0, align = "right", currencySymbol = '', currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", formulaMode = TRUE, width = "115px")),
                                                            div(style = "display: inline-block;", autonumericInput("feb_rev_upgpn","FEVEREIRO", value = 0, align = "right", currencySymbol = '', currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", formulaMode = TRUE, width = "115px")),
                                                            div(style = "display: inline-block;", autonumericInput("mar_rev_upgpn","MARÇO", value = 0, align = "right", currencySymbol = '', currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", formulaMode = TRUE, width = "115px")),
                                                            div(style = "display: inline-block;", autonumericInput("apr_rev_upgpn","ABRIL", value = 0, align = "right", currencySymbol = '', currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", formulaMode = TRUE, width = "115px")),
                                                            div(style = "display: inline-block;", autonumericInput("may_rev_upgpn","MAIO", value = 0, align = "right", currencySymbol = '', currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", formulaMode = TRUE, width = "115px")),
                                                            div(style = "display: inline-block;", autonumericInput("jun_rev_upgpn","JUNHO", value = 0, align = "right", currencySymbol = '', currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", formulaMode = TRUE, width = "115px")),
                                                            div(style = "display: inline-block;", autonumericInput("jul_rev_upgpn","JULHO", value = 0, align = "right", currencySymbol = '', currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", formulaMode = TRUE, width = "115px")),
                                                            div(style = "display: inline-block;", autonumericInput("ago_rev_upgpn","AGOSTO", value = 0, align = "right", currencySymbol = '', currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", formulaMode = TRUE, width = "115px")),
                                                            div(style = "display: inline-block;", autonumericInput("sep_rev_upgpn","SETEMBRO", value = 0, align = "right", currencySymbol = '', currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", formulaMode = TRUE, width = "115px")),
                                                            div(style = "display: inline-block;", autonumericInput("oct_rev_upgpn","OUTUBRO", value = 0, align = "right", currencySymbol = '', currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", formulaMode = TRUE, width = "115px")),
                                                            div(style = "display: inline-block;", autonumericInput("nov_rev_upgpn","NOVEMBRO", value = 0, align = "right", currencySymbol = '', currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", formulaMode = TRUE, width = "115px")),
                                                            div(style = "display: inline-block;", autonumericInput("dec_rev_upgpn","DEZEMBRO", value = 0, align = "right", currencySymbol = '', currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", formulaMode = TRUE, width = "115px")),
                                                            
                                                            textAreaInput("revision_comments_upgpn", "Comentários da Revisão", placeholder = "Justifique a necessidade de alteração", height = 100, width = "470px"),                                                            # div(style = "display:# div(style = "display: inline-block;", dateInput('updated_day',label='Realização', width = "120px")),
                                                            br(),
                                                            
                                                            actionButton("salvar_metas_alteradas_upgpn",  "SALVAR", style = "color: white;  background-color: green; ", icon=icon("save")),
                                                            br(),
                                                            # actionButton("optimize_3","", class = "run-button", style="color: #fff; background-color: #C24641; border-color: #C24641; height: 10px; width:465px;")
                                                            
                                                            ),

                                          ###############   PROCUREMENT PLAN UPDATES
                                          br(),
                                          div(style = "height:30px"),
                                          conditionalPanel(condition = "input.form_choice == 'Actualizar PP'", div(style = "display: inline-block;", selectInput("awpb_idpp", labelMandatory("Código"), multiple = FALSE, choices = as.character(unique(awpb_updated$awpb_id)), width = "150px"))),
                                          conditionalPanel(condition = "input.form_choice == 'Actualizar PP'", div(style = "display: inline-block;", selectInput("idpp", "Linha no PP", multiple = FALSE, choices = pp_codings, width = "150px"))),
                                          conditionalPanel(condition = "input.form_choice == 'Actualizar PP'", div(style = "display: inline-block;", autonumericInput("contract_usd","Valor estimado (US$)", value = 0, align = "right", currencySymbol = '', currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", formulaMode = TRUE, width = "170px"))),
                                          conditionalPanel(condition = "input.form_choice == 'Actualizar PP'", div(style = "display: inline-block;", selectInput("procurement_stage", "Fase da Contratação", multiple = FALSE, choices = c("", pp_stages), width = "470px"))),
                                          conditionalPanel(condition = "input.form_choice == 'Actualizar PP'", div(style = "display: inline-block;", selectInput("method_name_pt", "Método de Contratação", multiple = FALSE, choices = c("", pp_method_names_pt), width = "470px"))),
                                          
                                            
                                          
                                          conditionalPanel(condition = "input.procurement_stage == 'Contrato Assinado' || input.procurement_stage == 'Contrato visado' || input.procurement_stage == 'Contrato em Implementação' ", div(style = "display: inline-block;", selectInput("contract_n", "Contrato n.º", multiple = FALSE, choices = c("", contract_numbers), width = "470px"))),
                                          conditionalPanel(condition = "input.form_choice == 'Actualizar PP'", div(style = "display: inline-block;", selectInput("updated_by", "Actualiado por", multiple = FALSE, choices = c("", staff_nuit), width = "470px"))),
                                          conditionalPanel(condition = "input.form_choice == 'Actualizar PP'", div(style = "display: inline-block;", textAreaInput("comments_update", "Comentários (Justifique quaisquer desvios no processo)", placeholder = "", height = 100, width = "470px"))),
                                            
                                          conditionalPanel(condition = "input.form_choice == 'Actualizar PP'", 
                                                           actionButton("toggleAdvanced","ACTUALIZAR DATAS DO PP", class = "run-button", icon=icon("calendar"), style="color: #fff; background-color: #C24641; border-color: #C24641; height: 30px; width:465px;")),
           
                                          br(),
                                          hr(),
                                          div(style = "height:30px"),
                                          shinyjs::hidden(
                                            div(id = "advanced",
                                                
                                                splitLayout(cellWidths = c("33.3333%", "33.3333%","33.3333%"), cellArgs = list(style = "vertical-align: top"),
                                                            conditionalPanel(condition = "input.form_choice == 'Actualizar PP'",  dateInput("eo_i_submission", "RNO MDI", value = Sys.Date(), width = "155px")),
                                                            conditionalPanel(condition = "input.form_choice == 'Actualizar PP'",  dateInput("inv_r_eo_i", "Anúncio MDIs", value = Sys.Date(), width = "155px")),
                                                            conditionalPanel(condition = "input.form_choice == 'Actualizar PP'",  dateInput("close_r_eo_i", "Submissão de MDIs", value = Sys.Date(), width = "155px"))),
                                                
                                                splitLayout(cellWidths = c("33.3333%", "33.3333%","33.3333%"), cellArgs = list(style = "vertical-align: top"),
                                                            conditionalPanel(condition = "input.form_choice == 'Actualizar PP'", dateInput("report_eo_i", "RNO Lista Curta", value = Sys.Date(), width = "155px")),
                                                            conditionalPanel(condition = "input.form_choice == 'Actualizar PP'", dateInput("no_eo_i_report", "NO Lista Curta", value = Sys.Date(), width = "155px")),
                                                            conditionalPanel(condition = "input.form_choice == 'Actualizar PP'", dateInput("rfp_rno", "RNO RFP", value = Sys.Date(), width = "155px"))),
                                                
                                                splitLayout(cellWidths = c("33.3333%", "33.3333%","33.3333%"), cellArgs = list(style = "vertical-align: top"),
                                                            conditionalPanel(condition = "input.form_choice == 'Actualizar PP'",  dateInput("rfp_no", "NO RFP", value = Sys.Date(), width = "155px")),
                                                            conditionalPanel(condition = "input.form_choice == 'Actualizar PP'", dateInput("invitation", "Anúncio do Concurso", value = Sys.Date(), width = "155px")),
                                                            conditionalPanel(condition = "input.form_choice == 'Actualizar PP'", dateInput("closing", "Propostas Submetidas", value = Sys.Date(), width = "155px"))),
                                                
                                                splitLayout(cellWidths = c("33.3333%", "33.3333%","33.3333%"), cellArgs = list(style = "vertical-align: top"),
                                                            conditionalPanel(condition = "input.form_choice == 'Actualizar PP'",   dateInput("rno_eva_r", "RNO Relatório Técnico", value = Sys.Date(), width = "155px")),
                                                            conditionalPanel(condition = "input.form_choice == 'Actualizar PP'", dateInput("no_eva_r", "NO Relatório Técnico", value = Sys.Date(), width = "155px")),
                                                            conditionalPanel(condition = "input.form_choice == 'Actualizar PP'", dateInput("full_eva_rno", "RNO Relatório Comb.", value = Sys.Date(), width = "155px"))),
                                                
                                                splitLayout(cellWidths = c("33.3333%", "33.3333%","33.3333%"), cellArgs = list(style = "vertical-align: top"),
                                                            conditionalPanel(condition = "input.form_choice == 'Actualizar PP'",  dateInput("full_eva_no", "NO Relatório Comb.", value = Sys.Date(), width = "155px")),
                                                            conditionalPanel(condition = "input.form_choice == 'Actualizar PP'", dateInput("noita", "NOITA", value = Sys.Date(), width = "155px")),
                                                            conditionalPanel(condition = "input.form_choice == 'Actualizar PP'",  dateInput("contract_awards", "Adjudicação", value = Sys.Date(), width = "155px"))),
                                                
                                                splitLayout(cellWidths = c("33.3333%", "33.3333%","33.3333%"), cellArgs = list(style = "vertical-align: top"),
                                                            conditionalPanel(condition = "input.form_choice == 'Actualizar PP'",  dateInput("negotiations", "Negociações", value = Sys.Date(), width = "155px")),
                                                            conditionalPanel(condition = "input.form_choice == 'Actualizar PP'", dateInput("rno_contract", "RNO Contrato", value = Sys.Date(), width = "155px")),
                                                            conditionalPanel(condition = "input.form_choice == 'Actualizar PP'", dateInput("no_contract", "NO Contrato", value = Sys.Date(), width = "155px"))))),
                                          
                                          br(),
                                          div(style = "height:30px"),
                                          
                                          
                                          conditionalPanel(condition = "input.unidade_de_management == 'UNGP' && input.form_choice == 'Actualizar PAAO'", 
                                                           actionButton("submeter_actividade", "SALVAR", style = "color: white;  background-color: green; ", icon=icon("save"))),
                                          
                                          conditionalPanel(condition = "input.unidade_de_management == 'URGPS' && input.form_choice == 'Actualizar PAAO'", 
                                                           actionButton("submeter_actividade_sul", "SALVAR", style = "color: white;  background-color: green; ", icon=icon("save"))),
                                          
                                          conditionalPanel(condition = "input.unidade_de_management == 'UPGPN' && input.form_choice == 'Actualizar PAAO'", 
                                                           actionButton("submeter_actividade_niassa", "SALVAR", style = "color: white;  background-color: green; ", icon=icon("save"))),
                                          
                                          conditionalPanel(condition = "input.form_choice == 'Actualizar PP'", 
                                                           actionButton("submeter_dossier", "SALVAR", style = "color: white;  background-color: green; ", icon=icon("save")))
                                          

                                         ),
                                         
                                         footer = modalButton("Fechar"),
                                         easyClose = TRUE
                                       )
                                   )
    )
  






shinyjs::onclick("toggleAdvanced",
                 shinyjs::toggle(id = "advanced", anim = TRUE)) 
  
  observeEvent(input$lastClick, {if (input$lastClickId%like%"modify") {
    
    SQL_df <- filtered_awpb()
    
    updateTextAreaInput(session, "comments_niassa_pt", value = SQL_df[input$responses_table_rows_selected, "comments_niassa_pt"])
    updateTextAreaInput(session, "comments_niassa_en", value = SQL_df[input$responses_table_rows_selected, "comments_niassa_en"])
    
    updateTextAreaInput(session, "actions_niassa_pt", value = SQL_df[input$responses_table_rows_selected, "actions_niassa_pt"])
    updateTextAreaInput(session, "actions_niassa_en", value = SQL_df[input$responses_table_rows_selected, "actions_niassa_en"])
    
    updateTextAreaInput(session, "comments_gaza_pt", value = SQL_df[input$responses_table_rows_selected, "comments_gaza_pt"])
    updateTextAreaInput(session, "comments_gaza_en", value = SQL_df[input$responses_table_rows_selected, "comments_gaza_en"])
    
    updateTextAreaInput(session, "actions_gaza_pt", value = SQL_df[input$responses_table_rows_selected, "actions_gaza_pt"])
    updateTextAreaInput(session, "actions_gaza_en", value = SQL_df[input$responses_table_rows_selected, "actions_gaza_en"])
    
    updateTextAreaInput(session, "comments_national_pt", value = SQL_df[input$responses_table_rows_selected, "comments_national_pt"])
    updateTextAreaInput(session, "comments_national_en", value = SQL_df[input$responses_table_rows_selected, "comments_national_en"])
    
    updateTextAreaInput(session, "actions_national_pt", value = SQL_df[input$responses_table_rows_selected, "actions_national_pt"])
    updateTextAreaInput(session, "actions_national_en", value = SQL_df[input$responses_table_rows_selected, "actions_national_en"])
    
    updateSelectInput(session, "niassa_follower", selected = SQL_df[input$responses_table_rows_selected, "niassa_follower"])
    updateSelectInput(session, "national_follower", selected = SQL_df[input$responses_table_rows_selected, "national_follower"])
    updateSelectInput(session, "south_follower", selected = SQL_df[input$responses_table_rows_selected, "south_follower"])
    
    updateSelectInput(session, "situacao_upgpn", selected = SQL_df[input$responses_table_rows_selected, "situacao_upgpn"])
    updateSelectInput(session, "situacao_ungp", selected = SQL_df[input$responses_table_rows_selected, "situacao_ungp"])
    updateSelectInput(session, "situacao_urgps", selected = SQL_df[input$responses_table_rows_selected, "situacao_urgps"])
    
    updateSelectInput(session, "target_units", selected = SQL_df[input$responses_table_rows_selected, "unidades"])
    updateSelectInput(session, "awpb_idt", selected = SQL_df[input$responses_table_rows_selected, "awpb_id"])

    updateSelectInput(session, "awpb_id", selected = SQL_df[input$responses_table_rows_selected, "awpb_id"])
    updateSelectInput(session, "awpb_ids", selected = SQL_df[input$responses_table_rows_selected, "awpb_id"])
    updateSelectInput(session, "cost_tabs_fk", selected = SQL_df[input$responses_table_rows_selected, "costab_code"])
    
    updateNumericInput(session, "jan_rev_urgps", value = SQL_df[input$responses_table_rows_selected, "jan_rev_urgps"])
    updateNumericInput(session, "feb_rev_urgps", value = SQL_df[input$responses_table_rows_selected, "feb_rev_urgps"])
    updateNumericInput(session, "mar_rev_urgps", value = SQL_df[input$responses_table_rows_selected, "mar_rev_urgps"])
    updateNumericInput(session, "apr_rev_urgps", value = SQL_df[input$responses_table_rows_selected, "apr_rev_urgps"])
    updateNumericInput(session, "may_rev_urgps", value = SQL_df[input$responses_table_rows_selected, "may_rev_urgps"])
    updateNumericInput(session, "jun_rev_urgps", value = SQL_df[input$responses_table_rows_selected, "jun_rev_urgps"])
    updateNumericInput(session, "jul_rev_urgps", value = SQL_df[input$responses_table_rows_selected, "jul_rev_urgps"])
    updateNumericInput(session, "ago_rev_urgps", value = SQL_df[input$responses_table_rows_selected, "ago_rev_urgps"])
    updateNumericInput(session, "sep_rev_urgps", value = SQL_df[input$responses_table_rows_selected, "sep_rev_urgps"])
    updateNumericInput(session, "oct_rev_urgps", value = SQL_df[input$responses_table_rows_selected, "oct_rev_urgps"])
    updateNumericInput(session, "nov_rev_urgps", value = SQL_df[input$responses_table_rows_selected, "nov_rev_urgps"])
    updateNumericInput(session, "dec_rev_urgps", value = SQL_df[input$responses_table_rows_selected, "dec_rev_urgps"])
    
    updateNumericInput(session, "jan_rev_ungp", value = SQL_df[input$responses_table_rows_selected, "jan_rev_ungp"])
    updateNumericInput(session, "feb_rev_ungp", value = SQL_df[input$responses_table_rows_selected, "feb_rev_ungp"])
    updateNumericInput(session, "mar_rev_ungp", value = SQL_df[input$responses_table_rows_selected, "mar_rev_ungp"])
    updateNumericInput(session, "apr_rev_ungp", value = SQL_df[input$responses_table_rows_selected, "apr_rev_ungp"])
    updateNumericInput(session, "may_rev_ungp", value = SQL_df[input$responses_table_rows_selected, "may_rev_ungp"])
    updateNumericInput(session, "jun_rev_ungp", value = SQL_df[input$responses_table_rows_selected, "jun_rev_ungp"])
    updateNumericInput(session, "jul_rev_ungp", value = SQL_df[input$responses_table_rows_selected, "jul_rev_ungp"])
    updateNumericInput(session, "ago_rev_ungp", value = SQL_df[input$responses_table_rows_selected, "ago_rev_ungp"])
    updateNumericInput(session, "sep_rev_ungp", value = SQL_df[input$responses_table_rows_selected, "sep_rev_ungp"])
    updateNumericInput(session, "oct_rev_ungp", value = SQL_df[input$responses_table_rows_selected, "oct_rev_ungp"])
    updateNumericInput(session, "nov_rev_ungp", value = SQL_df[input$responses_table_rows_selected, "nov_rev_ungp"])
    updateNumericInput(session, "dec_rev_ungp", value = SQL_df[input$responses_table_rows_selected, "dec_rev_ungp"])
    
    updateNumericInput(session, "paao_ungp", value = SQL_df[input$responses_table_rows_selected, "paao_ungp"])
    updateNumericInput(session, "paao_upgpn", value = SQL_df[input$responses_table_rows_selected, "paao_upgpn"])
    updateNumericInput(session, "paao_urgps", value = SQL_df[input$responses_table_rows_selected, "paao_urgps"])
    
    updateNumericInput(session, "orcamento_revisto_ungp", value = SQL_df[input$responses_table_rows_selected, "orcamento_revisto_ungp"])
    updateTextAreaInput(session, "revision_comments_ungp", value = SQL_df[input$responses_table_rows_selected, "revision_comments_ungp"])
    updateSelectizeInput(session, "unidades_medida_ungp", selected = SQL_df[input$responses_table_rows_selected, "unidades"])
    
    updateSelectizeInput(session, "financing_category", selected = SQL_df[input$responses_table_rows_selected, "financing_category"])
    updateSelectizeInput(session, "activity_financiers", selected = SQL_df[input$responses_table_rows_selected, "activity_financiers"])
    
    updateNumericInput(session, "orcamento_revisto_urgps", value = SQL_df[input$responses_table_rows_selected, "orcamento_revisto_urgps"])
    updateTextAreaInput(session, "revision_comments_urgps", value = SQL_df[input$responses_table_rows_selected, "revision_comments_urgps"])
    updateSelectizeInput(session, "unidades_medida_urgps", selected = SQL_df[input$responses_table_rows_selected, "unidades"])
    
    updateNumericInput(session, "orcamento_revisto_upgpn", value = SQL_df[input$responses_table_rows_selected, "orcamento_revisto_upgpn"])
    updateTextAreaInput(session, "revision_comments_upgpn", value = SQL_df[input$responses_table_rows_selected, "revision_comments_upgpn"])
    updateSelectizeInput(session, "unidades_medida_upgpn", selected = SQL_df[input$responses_table_rows_selected, "unidades"])    
    
    updateNumericInput(session, "jan_rev_upgpn", value = SQL_df[input$responses_table_rows_selected, "jan_rev_upgpn"])
    updateNumericInput(session, "feb_rev_upgpn", value = SQL_df[input$responses_table_rows_selected, "feb_rev_upgpn"])
    updateNumericInput(session, "mar_rev_upgpn", value = SQL_df[input$responses_table_rows_selected, "mar_rev_upgpn"])
    updateNumericInput(session, "apr_rev_upgpn", value = SQL_df[input$responses_table_rows_selected, "apr_rev_upgpn"])
    updateNumericInput(session, "may_rev_upgpn", value = SQL_df[input$responses_table_rows_selected, "may_rev_upgpn"])
    updateNumericInput(session, "jun_rev_upgpn", value = SQL_df[input$responses_table_rows_selected, "jun_rev_upgpn"])
    updateNumericInput(session, "jul_rev_upgpn", value = SQL_df[input$responses_table_rows_selected, "jul_rev_upgpn"])
    updateNumericInput(session, "ago_rev_upgpn", value = SQL_df[input$responses_table_rows_selected, "ago_rev_upgpn"])
    updateNumericInput(session, "sep_rev_upgpn", value = SQL_df[input$responses_table_rows_selected, "sep_rev_upgpn"])
    updateNumericInput(session, "oct_rev_upgpn", value = SQL_df[input$responses_table_rows_selected, "oct_rev_upgpn"])
    updateNumericInput(session, "nov_rev_upgpn", value = SQL_df[input$responses_table_rows_selected, "nov_rev_upgpn"])
    updateNumericInput(session, "dec_rev_upgpn", value = SQL_df[input$responses_table_rows_selected, "dec_rev_upgpn"])

    updateSelectInput(session, "idpp", selected = SQL_df[input$responses_table_rows_selected, "idpp"])
    
    updateSelectInput(session, "awpb_idpp", selected = SQL_df[input$responses_table_rows_selected, "awpb_id"])
    updateSelectInput(session, "procurement_stage", selected = SQL_df[input$responses_table_rows_selected, "procurement_stage"])
    updateSelectInput(session, "method_name_pt", selected = SQL_df[input$responses_table_rows_selected, "method_name_pt"])
    updateSelectInput(session, "contract_n", selected = SQL_df[input$responses_table_rows_selected, "contract_n"])
    updateSelectInput(session, "updated_by", selected = SQL_df[input$responses_table_rows_selected, "updated_by"])
    
    updateNumericInput(session, "contract_usd", value = SQL_df[input$responses_table_rows_selected, "contract_usd"])
    
    updateDateInput(session, "no_eo_i", value = SQL_df[input$responses_table_rows_selected, "no_eo_i"])
    updateDateInput(session, "eo_i_submission", value = SQL_df[input$responses_table_rows_selected, "eo_i_submission"])
    updateDateInput(session, "no_eo_i", value = SQL_df[input$input$responses_table_rows_selected, "no_eo_i"])
    updateDateInput(session, "inv_r_eo_i", value = SQL_df[input$responses_table_rows_selected, "inv_r_eo_i"])
    updateDateInput(session, "close_r_eo_i", value = SQL_df[input$responses_table_rows_selected, "close_r_eo_i"])
    updateDateInput(session, "report_eo_i", value = SQL_df[input$responses_table_rows_selected, "report_eo_i"])
    updateDateInput(session, "no_eo_i_report", value = SQL_df[input$responses_table_rows_selected, "no_eo_i_report"])
    updateDateInput(session, "rfp_rno", value = SQL_df[input$responses_table_rows_selected, "rfp_rno"])
    updateDateInput(session, "rfp_no", value = SQL_df[input$responses_table_rows_selected, "rfp_no"])
    updateDateInput(session, "invitation", value = SQL_df[input$responses_table_rows_selected, "invitation"])
    updateDateInput(session, "closing", value = SQL_df[input$responses_table_rows_selected, "closing"])
    updateDateInput(session, "rno_eva_r", value = SQL_df[input$responses_table_rows_selected, "rno_eva_r"])
    updateDateInput(session, "no_eva_r", value = SQL_df[input$responses_table_rows_selected, "no_eva_r"])
    updateDateInput(session, "full_eva_rno", value = SQL_df[input$responses_table_rows_selected, "full_eva_rno"])
    updateDateInput(session, "full_eva_no", value = SQL_df[input$responses_table_rows_selected, "full_eva_no"])
    updateDateInput(session, "noita", value = SQL_df[input$responses_table_rows_selected, "noita"])
    updateDateInput(session, "contract_awards", value = SQL_df[input$responses_table_rows_selected, "contract_awards"])
    updateDateInput(session, "negotiations", value = SQL_df[input$responses_table_rows_selected, "negotiations"])
    updateDateInput(session, "rno_contract", value = SQL_df[input$responses_table_rows_selected, "rno_contract"])
    updateDateInput(session, "no_contract", value = SQL_df[input$responses_table_rows_selected, "no_contract"])
    updateDateInput(session, "signature", value = SQL_df[input$responses_table_rows_selected, "signature"])
    updateDateInput(session, "contract_completion", value = SQL_df[input$responses_table_rows_selected, "contract_completion"])
    
    updateTextAreaInput(session, "comments_update", value = SQL_df[input$responses_table_rows_selected, "comments_update"])
    
    
    showModal(new_awpb_update_form)
    
    }
    else if (input$lastClickId%like%"informacaoproposta") {
      
      ##### activar estas 3
      SQL_df <- filtered_awpb()
      updateSelectInput(session, "awpb_ids", selected = SQL_df[input$responses_table_rows_selected, "awpb_id"])
      updateSelectInput(session, "cost_tabs_fk", selected = SQL_df[input$responses_table_rows_selected, "costab_code"])


      showModal(payments_form)
      
      }
    
    })
  
  
  
  
  
  
  
  
  
  
  observeEvent(input$salvar_metas_alteradas_ungp, priority = 10, {
    SQL_df <- filtered_awpb()
    row_selection <- SQL_df[input$responses_table_rows_selected, "awpb_id"]
    qry = paste0("UPDATE procava.updated_targets SET ungp_update_date =  '", paste(as.character(format(Sys.Date(), format="%Y-%m-%d"))),"', ",
                 "jan_rev_ungp = ", ifelse(is.na(input$jan_rev_ungp),0,input$jan_rev_ungp),", ",
                 "feb_rev_ungp = ", ifelse(is.na(input$feb_rev_ungp),0,input$feb_rev_ungp),", ",
                 "mar_rev_ungp = ", ifelse(is.na(input$mar_rev_ungp),0,input$mar_rev_ungp),", ",
                 "apr_rev_ungp = ", ifelse(is.na(input$apr_rev_ungp),0,input$apr_rev_ungp),", ",
                 "may_rev_ungp = ", ifelse(is.na(input$may_rev_ungp),0,input$may_rev_ungp),", ",
                 "jun_rev_ungp = ", ifelse(is.na(input$jun_rev_ungp),0,input$jun_rev_ungp),", ",
                 "jul_rev_ungp = ", ifelse(is.na(input$jul_rev_ungp),0,input$jul_rev_ungp),", ",
                 "ago_rev_ungp = ", ifelse(is.na(input$ago_rev_ungp),0,input$ago_rev_ungp),", ",
                 "sep_rev_ungp = ", ifelse(is.na(input$sep_rev_ungp),0,input$sep_rev_ungp),", ",
                 "oct_rev_ungp = ", ifelse(is.na(input$oct_rev_ungp),0,input$oct_rev_ungp),", ",
                 "nov_rev_ungp = ", ifelse(is.na(input$nov_rev_ungp),0,input$nov_rev_ungp),", ",
                 "dec_rev_ungp = ", input$paao_ungp - (input$jan_rev_ungp+input$feb_rev_ungp+input$mar_rev_ungp+input$apr_rev_ungp+input$may_rev_ungp+input$jun_rev_ungp+input$jul_rev_ungp+input$ago_rev_ungp+input$sep_rev_ungp+input$oct_rev_ungp+input$nov_rev_ungp),", ",
                 "orcamento_revisto_ungp = ", ifelse(is.na(input$orcamento_revisto_ungp),0,input$orcamento_revisto_ungp),", ",
                 
                 "revision_comments_ungp = '", input$revision_comments_ungp,"' ",
                 " WHERE awpb_code = '", paste(row_selection),"'")
    
    dbGetQuery(pool, qry)
    showToast("success", "Proposta de alterações registada!", .options = myToastOptions)
  })
  
  observeEvent(input$salvar_metas_alteradas_urgps, priority = 10, {
    SQL_df <- filtered_awpb()
    row_selection <- SQL_df[input$responses_table_rows_selected, "awpb_id"]
    qry = paste0("UPDATE procava.updated_targets SET urgps_update_date =  '", paste(as.character(format(Sys.Date(), format="%Y-%m-%d"))),"', ",
                 "jan_rev_urgps = ", ifelse(is.na(input$jan_rev_urgps),0,input$jan_rev_urgps),", ",
                 "feb_rev_urgps = ", ifelse(is.na(input$feb_rev_urgps),0,input$feb_rev_urgps),", ",
                 "mar_rev_urgps = ", ifelse(is.na(input$mar_rev_urgps),0,input$mar_rev_urgps),", ",
                 "apr_rev_urgps = ", ifelse(is.na(input$apr_rev_urgps),0,input$apr_rev_urgps),", ",
                 "may_rev_urgps = ", ifelse(is.na(input$may_rev_urgps),0,input$may_rev_urgps),", ",
                 "jun_rev_urgps = ", ifelse(is.na(input$jun_rev_urgps),0,input$jun_rev_urgps),", ",
                 "jul_rev_urgps = ", ifelse(is.na(input$jul_rev_urgps),0,input$jul_rev_urgps),", ",
                 "ago_rev_urgps = ", ifelse(is.na(input$ago_rev_urgps),0,input$ago_rev_urgps),", ",
                 "sep_rev_urgps = ", ifelse(is.na(input$sep_rev_urgps),0,input$sep_rev_urgps),", ",
                 "oct_rev_urgps = ", ifelse(is.na(input$oct_rev_urgps),0,input$oct_rev_urgps),", ",
                 "nov_rev_urgps = ", ifelse(is.na(input$nov_rev_urgps),0,input$nov_rev_urgps),", ",
                 "dec_rev_urgps = ", input$paao_urgps - (input$jan_rev_urgps+input$feb_rev_urgps+input$mar_rev_urgps+input$apr_rev_urgps+input$may_rev_urgps+input$jun_rev_urgps+input$jul_rev_urgps+input$ago_rev_urgps+input$sep_rev_urgps+input$oct_rev_urgps+input$nov_rev_urgps),", ",
                 "orcamento_revisto_urgps = ", ifelse(is.na(input$orcamento_revisto_urgps),0,input$orcamento_revisto_urgps),", ",
                 
                 "revision_comments_urgps = '", input$revision_comments_urgps,"' ",
                 " WHERE awpb_code = '", paste(row_selection),"'")
    
    dbGetQuery(pool, qry)
    showToast("success", "Proposta de alterações registada!", .options = myToastOptions)
  })
  
  
  observeEvent(input$salvar_metas_alteradas_upgpn, priority = 10, {
    SQL_df <- filtered_awpb()
    row_selection <- SQL_df[input$responses_table_rows_selected, "awpb_id"]
    qry = paste0("UPDATE procava.updated_targets SET upgpn_update_date =  '", paste(as.character(format(Sys.Date(), format="%Y-%m-%d"))),"', ",
                 "jan_rev_upgpn = ", ifelse(is.na(input$jan_rev_upgpn),0, input$jan_rev_upgpn),", ",
                 "feb_rev_upgpn = ", ifelse(is.na(input$feb_rev_upgpn),0, input$feb_rev_upgpn),", ",
                 "mar_rev_upgpn = ", ifelse(is.na(input$mar_rev_upgpn),0, input$mar_rev_upgpn),", ",
                 "apr_rev_upgpn = ", ifelse(is.na(input$apr_rev_upgpn),0, input$apr_rev_upgpn),", ",
                 "may_rev_upgpn = ", ifelse(is.na(input$may_rev_upgpn),0, input$may_rev_upgpn),", ",
                 "jun_rev_upgpn = ", ifelse(is.na(input$jun_rev_upgpn),0, input$jun_rev_upgpn),", ",
                 "jul_rev_upgpn = ", ifelse(is.na(input$jul_rev_upgpn),0, input$jul_rev_upgpn),", ",
                 "ago_rev_upgpn = ", ifelse(is.na(input$ago_rev_upgpn),0, input$ago_rev_upgpn),", ",
                 "sep_rev_upgpn = ", ifelse(is.na(input$sep_rev_upgpn),0, input$sep_rev_upgpn),", ",
                 "oct_rev_upgpn = ", ifelse(is.na(input$oct_rev_upgpn),0, input$oct_rev_upgpn),", ",
                 "nov_rev_upgpn = ", ifelse(is.na(input$nov_rev_upgpn),0, input$nov_rev_upgpn),", ",
                 "dec_rev_upgpn = ", input$paao_upgpn - (input$jan_rev_upgpn+input$feb_rev_upgpn+input$mar_rev_upgpn+input$apr_rev_upgpn+input$may_rev_upgpn+input$jun_rev_upgpn+input$jul_rev_upgpn+input$ago_rev_upgpn+input$sep_rev_upgpn+input$oct_rev_upgpn+input$nov_rev_upgpn),", ",
                 
                 "orcamento_revisto_upgpn = ", ifelse(is.na(input$orcamento_revisto_upgpn),0,input$orcamento_revisto_upgpn),", ",
                 
                 "revision_comments_upgpn = '", input$revision_comments_upgpn,"' ",
                 
                 " WHERE awpb_code = '", paste(row_selection),"'")
    
    dbGetQuery(pool, qry)
    showToast("success", "Proposta de alterações registada!", .options = myToastOptions)
  })
  
  
  
  aggreed_actions_form = modalDialog(title = "ACÇÕES ACORDADAS", footer = modalButton("Fechar"),
                                     div(id=("aggreed_actions_form"),
                                         tags$head(tags$style(".modal-dialog{ width:500px}")),
                                         tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
                                         fluidPage(
                                           fluidRow(

                                             # radioGroupButtons("actions_idioma_update", "", choices = c("Português" = "PT", "English" = "EN"), status = "warning", checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))),
                                             # br(),
                                             # conditionalPanel(condition = "input.actions_idioma_update == 'EN'", textAreaInput('action_details_en', 'Descrição da acção (Inglês)', placeholder = 'Inserir detalhes', height = 100, width = "470px")),
                                             textAreaInput('action_details_pt', 'Descrição da acção (Português)', placeholder = 'Inserir detalhes', height = 100, width = "470px"),
                                             # div(style = "display: inline-block;", selectizeInput('action_responsible_institution', 'Entidade', choices = c('FAR, FP', 'UNGP', 'FIDA', 'PROCAVA', 'URGPS', 'UPGPN', 'MADER', 'DNDEL', 'INAM', 'MISAU', 'DPOPHRH', 'AQUA', 'DINAB', 'MITA', 'DCM', 'DNAAF', 'DNDAF', 'DNDP', 'DNSAB', 'DPAP', 'DPTA', 'FAEF', 'FAR, FP', 'FDA', 'FIDA', 'GCF', 'GdM', 'IIAM', 'INNOQ', 'MADER', 'MEF', 'MOPHRH', 'MTA', 'NPMU', 'PROCAVA', 'REFP', 'RPMU', 'SDAE', 'SDPI', 'SPAE', 'UEM', 'UNGP', 'MDN', 'MINT', 'MTC', 'MEDH', 'MICTUR', 'MITESS', 'MIGCAS', 'MAEF', 'MIMAIP', 'MIREME', 'MIC', 'MCT'), multiple = TRUE, width = '140px')),
                                             div(style = "display: inline-block;", selectizeInput('action_technician', 'Responsável Interno', choices = staff_nuit, width = '250px')),
                                             # dateInput('action_deadline', 'Prazo'),
                                             div(style = "display: inline-block;", selectizeInput('implementation_status_pt', 'Ponto de situação', choices = c("Não iniciada","Em curso", "Cumprida", "Propõe-se cancelar", "Cancelada"), width = '220px')),
                                             # conditionalPanel(condition = "input.actions_idioma_update == 'EN'", selectizeInput('implementation_status_en', 'Ponto de situação (Português)', choices = staff_nuit)),
                                             splitLayout(cellWidths = c("30%", "40%","30%"), cellArgs = list(style = "vertical-align: top"),
                                             dateInput('action_completed', 'Data de conclusão', width = '140px', value = as.Date('01-01-2031')),
                                             selectizeInput('indicator_aggreed', 'Unidade', choices = procava_measurement_units, width = '225px'),
                                             autonumericInput("achieved_aggreed","Realizado", value = 0, align = "right", currencySymbol = '', currencySymbolPlacement = "p", decimalCharacter = ",", digitGroupSeparator = ".", formulaMode = TRUE, width = "120px")),
                                             textAreaInput('action_status_comments', 'Descrição do estado (Português)', placeholder = 'Inserir detalhes', height = 100, width = "470px"),
                                             textAreaInput('action_status_comments_en', 'Status description (English)', placeholder = 'Insert details', height = 100, width = "470px"),
                                             # conditionalPanel(condition = "input.actions_idioma_update == 'EN'", textAreaInput('action_status_comments_en', 'Comentários do estado (Inglês)', placeholder = 'Inserir detalhes', width = '100%')),
                                             actionButton("submeter_aggred_actions", "SALVAR", style = "color: white;  background-color: green; ", icon=icon("save")))
                                           ),
                                           
                                           footer = modalButton("Fechar"),
                                           easyClose = TRUE
                                         )
                                     )
  
  
  shinyjs::onclick("toggleAdvanced",
                   shinyjs::toggle(id = "advanced_form", anim = TRUE)) 
  
  observeEvent(input$lastClick, {if (input$lastClickId%like%"changeactions") {
    SQL_df <- accoes_acordadas_procava()
    updateTextAreaInput(session, "action_details_en", value = SQL_df[input$aggreed_actions_rows_selected, "action_details_en"])
    updateTextAreaInput(session, "action_details_pt", value = SQL_df[input$aggreed_actions_rows_selected, "action_details_pt"])
    updateNumericInput(session, "achieved_aggreed", value = SQL_df[input$aggreed_actions_rows_selected, "achieved_aggreed"])
    updateTextAreaInput(session, "action_status_comments", value = SQL_df[input$aggreed_actions_rows_selected, "action_comments_pt"])
    updateTextAreaInput(session, "action_status_comments_en", value = SQL_df[input$aggreed_actions_rows_selected, "action_comment_en"])
    updateSelectInput(session, "action_technician", selected = SQL_df[input$aggreed_actions_rows_selected, "action_responsible"])
    updateSelectInput(session, "implementation_status_pt", selected = SQL_df[input$aggreed_actions_rows_selected, "action_implementation"])
    updateDateInput(session, "action_completed", value = SQL_df[input$aggreed_actions_rows_selected, "action_completed"])
    showModal(aggreed_actions_form)
    
    }
    else if (input$lastClickId%like%"execute") {
      SQL_df <- accoes_acordadas_procava()
      updateSelectInput(session, "implementation_status_pt", selected = SQL_df[input$aggreed_actions_rows_selected, "action_implementation"])
      showModal(aggreed_actions_form)
      
    }
    
  })
  
  
  observeEvent(input$submeter_aggred_actions, priority = 10, {
    SQL_df <- accoes_acordadas_procava()
    row_selection <- SQL_df[input$aggreed_actions_rows_selected, "action_id"]
    
    qry = paste0("UPDATE procava.aggreed_actions SET date_updated =  '", paste(as.character(format(Sys.Date(), format="%Y-%m-%d"))),"', ",
                 "achieved_aggreed = ", ifelse(is.na(input$achieved_aggreed),0, input$achieved_aggreed),", ",
                 "status_comments = '", ifelse(is.na(input$action_status_comments),0, input$action_status_comments),"', ",
                 "status_comments_en = '", ifelse(is.na(input$action_status_comments_en),NULL, input$action_status_comments_en),"', ",
                 "implementation_status = '", ifelse(is.na(input$implementation_status_pt),NULL, input$implementation_status_pt),"', ",
                 "date_complied =  '",  paste(as.character(format(input$action_completed, format="%Y-%m-%d"))),"' ",
                 
                 " WHERE action_id = '", paste(row_selection),"'")
    
    dbGetQuery(pool, qry)
    showToast("success", "Proposta de alterações registada!", .options = myToastOptions)
  })
  
  
  shinyInput <- function(FUN, ids, ...) {
    inputs <- NULL
    inputs <- sapply(ids, function(x) {inputs[x] <- as.character(FUN(inputId = x, label = NULL, ...))})
    inputs}
  
  filtered_payments_requests <- reactive({
    payment_requests <- DBI::dbGetQuery(pool, "SELECT * FROM fiduciary.approval_requests")
    if(input$proponents != "Todos"){payment_requests <- payment_requests[payment_requests$applicant %in% input$proponents,]}
    if(input$proponents_process_typologies != "Todos"){payment_requests <- payment_requests[payment_requests$process_type %in% input$proponents_process_typologies,]}
    if(input$pmu_costcentre != "Todas"){payment_requests <- payment_requests[payment_requests$cost_centers %in% input$pmu_costcentre,]}
    if(input$urgencia_processual != "Todos"){payment_requests <- payment_requests[payment_requests$expense_urgencia_processo %in% input$urgencia_processual,]}
    if(input$rubricas_ced_pedidas != "Todas"){payment_requests <- payment_requests[payment_requests$e_sistafe_pt %in% input$rubricas_ced_pedidas,]}
    
    payment_requests <- payment_requests[payment_requests$submission_date >= input$periodo_de_despesas[1] & payment_requests$submission_date <= input$periodo_de_despesas[2],]
    
    payment_requests
  })
  
  finance_clearance_forms = modalDialog(title = "PARECER DO SECTOR FINANCEIRO",
                                        fluidPage(
                                          fluidRow(
                                            selectizeInput("contract_vs_invoice", "Código da Despesa", choices = unique(approval_requests$contract_vs_invoice), width = '570px'),
                                            prettyCheckbox(inputId = "aprovacao_financeiros",label = "Despesa aprovada?", outline = FALSE, plain = TRUE, icon = icon("thumbs-up"), animation = "tada", fill = TRUE, bigger = TRUE, shape = "curve"),
                                            prettyCheckbox(inputId = "shared_feedback_finance",label = "Partilhar Feedback?", outline = TRUE, plain = TRUE, icon = icon("thumbs-up"), animation = "tada", fill = TRUE, bigger = TRUE, shape = "curve"),
                                            div(style = "display: inline-block;",numericInput(inputId = "forex_exchange_rates", label= "Câmbio Forex para US$", value = forex_e_rate,  width = '280px')),
                                            div(style = "display: inline-block;",dateInput("date_financeman_approved", "Pagamento (Data):", value = Sys.Date(), format = "dd/mm/yyyy", width = '280px')),
                                            div(style = "display: inline-block;",textAreaInput(inputId = "expense_comments", label= "Comentários e Observações", width = '570px')),
                                            div(style = "display: inline-block;",textAreaInput(inputId = "finance_suggestions", label= "Recomendações e Acções de Seguimento", width = '570px')),
                                            actionButton("salvar_parecer_financeiro", "Gravar alterações", class = "btn-success", icon=icon("play")),
                                            size = "l",
                                            footer = modalButton("Fechar"),
                                            easyClose = TRUE)))
  
  observeEvent(input$lastClick, {if (input$lastClickId%like%"modifyfinance") {showModal(finance_clearance_forms)}
    else if (input$lastClickId%like%"financedeleted") {
      
      row_selection <- df[input$finance_clearance_rows_selected, "contract_vs_invoice"]
      
      qry = paste0("DELETE FROM fiduciary.payment_proposals WHERE contract_vs_invoice = '", row_selection,"'")
      
      dbGetQuery(pool, qry)
      showModal(modalDialog("Proposta de Despesa Apagada do Sistema", easyClose = TRUE, footer = NULL, class = "error")) 
    }})
  
  df <-  approval_requests %>% mutate(Activity = answer_options,  finance_clearance = shinyInput(checkboxInput, answer_options))
  
  df[["comments_finance"]] <- paste0('
             <div class="btn-group" role="group" aria-label="Example">
                <button type="button" class="fa fa-edit modifyfinance" style="color:green" id=modifyfinance_',1:nrow(df),'></button>
             </div>
             ')
  
  df[["finance_Deletions"]] <- paste0('
            <div class="btn-group" role="group" aria-label="Basic">
               <button type="button" class="fa fa-trash financedeleted" style="color:red" id=financedeleted_',1:nrow(df),'></button>
            </div>
            ')
  
  df <- df %>% colorder(comments_finance, finance_Deletions, awpb_ids, expense_description, expense_detailed_pt, payment_beneficiary, submission_date, applicant, e_sistafe_pt, total_quantity, unidades_medida, total_cost, expense_currency, expense_urgencia_processo, expense_comments, finance_clearance, contract_vs_invoice, pos = "front")
  
  output$finance_clearance <- DT::renderDataTable(
    df, 
    
    colnames = c('A'  = 1, 'B'  = 2, 'PAAO'  = 3, 'Actividade específica'  = 4, 'Despesa'  = 5, 'Beneficiário'  = 6, 'Submissão'  = 7, 'Proponente'  = 8, 'CED'  = 9, 'Quantidade'  = 10, 'Unidades'  = 11, 'Custo'  = 12, 'Moeda'  = 13, 'Urgência'  = 14, 'Comentários'  = 15, 'Aprovar?'  = 16),
    server = FALSE, escape = FALSE, rownames = FALSE,
    options = list(columnDefs = list(list(targets = c(16:(ncol(df)-1)), visible = FALSE)),
                   dom = 'Bfrtip', paging = FALSE, ordering = FALSE,
                   preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                   drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
    )
  )
  
  observeEvent(input$submit_finance_approval, {
    responses <- data.frame(approval_comments = input$approval_comments, forex_exchange_rate = input$forex_exchange_rate, contract_vs_invoice = answer_options, finance_clearance = sapply(answer_options, function(i) input[[i]], USE.NAMES = FALSE))
    short_response <- responses %>% dplyr::filter(finance_clearance == TRUE)
    write.table(short_response, "responses.csv", col.names = TRUE, row.names = FALSE, append = FALSE, sep = ",")
    
    for(i in 1:nrow(short_response)) {
      qry = paste0("UPDATE fiduciary.payment_proposals SET finance_clearance = 'TRUE', 
                         forex_exchange_rate = ", input$forex_exchange_rate, ", 
                         fin_approve_date = '", input$data_aprovacao_financas, "', 
                         approval_actions = '", input$approval_suggestions_finance, "', 
                         approval_comments = '", short_response$approval_comments[i], "' 
                         WHERE contract_vs_invoice = '", short_response$contract_vs_invoice[i], "'")
      
      dbGetQuery(pool, qry)}
    
    showModal(modalDialog("Successfully submitted", easyClose = TRUE, footer = NULL, class = "success")) 
    
    sapply(answer_options, function(x) updateCheckboxInput(session, x, value = FALSE))
    updateTextInput(session, "username", value = "")
    
  })
  
  observeEvent(input$lastClick, priority = 20,{
    SQL_df <- df
    updateSelectInput(session, "contract_vs_invoice", selected = SQL_df[input$finance_clearance_rows_selected, "contract_vs_invoice"])
  })
  
  observeEvent(input$salvar_parecer_financeiro, priority = 10, {
    SQL_df <- df
    row_selection <- SQL_df[input$finance_clearance_rows_selected, "contract_vs_invoice"]
    # print(row_selection)
    qry = paste0("UPDATE fiduciary.payment_proposals SET  approval_comments = '", paste(input$expense_comments),"', ",
                 
                 "fin_approve_date =  '", input$data_aprovacao_financas,"', ",
                 "finance_clearance =  '", paste(input$aprovacao_financeiros),"', ",
                 "share_feedback =  '", paste(input$shared_feedback_finance),"', ",
                 "forex_exchange_rate =  '", paste(input$forex_exchange_rates),"', ",
                 "approval_actions = '", paste(input$finance_suggestions),"' ",
                 " WHERE contract_vs_invoice = '", input$contract_vs_invoice,"'")
    
    dbGetQuery(pool, qry)
    showModal(modalDialog("Parecer Registado com Sucesso", easyClose = TRUE, footer = NULL, class = "success")) 
  })
  

  
}
