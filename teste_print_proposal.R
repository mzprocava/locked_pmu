library(shinyFiles)
library(tidyverse)
library(flextable)
library(DBI)
library(RPostgres)
library(pool)
library(officedown)
library(officer)
library(lubridate)
library(janitor)
library(data.table)
library(glue)
db <- 'mzprocava'  
host_db <- "mzprocava.c3n7ltv3naza.eu-west-1.rds.amazonaws.com"
db_port <- '5432'  
db_user <- "mzprocava"
db_password <- "GoMPROCAVA;2030"
# conn <- dbConnect(RPostgres::Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_password)
pool <- dbPool(RPostgres::Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_password)

payment_proposals <-  DBI::dbGetQuery(pool, "SELECT * FROM fiduciary.full_payment_proposals")
pdr_financiers <-  DBI::dbGetQuery(pool, "SELECT details_contributors, percentage_column FROM fiduciary.pdr_financiers")

# setwd("C:/Users/Administrator/Dropbox/PROCAVA_PMU/pmu_production")
# poolClose(pool)

payments_data <- payment_proposals
tipo_despesa <- "Pagamento de contratos"
file_name <- ifelse(tipo_despesa == "Ajudas de custos", "Modelo_IP_RMarkdown_Final_ADC.docx",
                    ifelse(tipo_despesa =="Salários e remunerações", "Modelo_IP_RMarkdown_Final_SALARIOS.docx",
                           ifelse(tipo_despesa == "Pagamento de contratos", "Modelo_IP_RMarkdown_Final_CONTRATOS.docx",
                                  "Modelo_IP_RMarkdown_Final_OUTROS.docx")))
sample_doc <- read_docx(file_name)

keywords <- ifelse(tipo_despesa == "Ajudas de custos", "acordo com a tabela a seguir.",
                   ifelse(tipo_despesa =="Salários e remunerações", "ANEXO. DETALHES DO PAGAMENTO", "ANEXO. DETALHES DO PAGAMENTO"))

date_proposal <- as.Date(Sys.Date(), "%m/%d/%y")
month_number <- month(date_proposal)
year_proposal <- year(date_proposal)
formated_date <- paste0(day(date_proposal),"/", ifelse(month(date_proposal)<10,paste0("0",month(date_proposal)), month(date_proposal)), "/", year(date_proposal))
month_salaries_paid <- ifelse(month_number== 1, "Janeiro", ifelse(month_number== 2, "Fevereiro", ifelse(month_number== 3, "Março", ifelse(month_number== 4, "Abril",
                                                                                                                                          ifelse(month_number== 5, "Maio", ifelse(month_number== 6, "Junho", ifelse(month_number== 7, "Julho", ifelse(month_number== 8, "Agosto",
                                                                                                                                                                                                                                                      ifelse(month_number== 9, "Setembro", ifelse(month_number== 10, "Outubro",
                                                                                                                                                                                                                                                                                                  ifelse(month_number== 11, "Novembro", ifelse(month_number== 12, "Dezembro"))))))))))))

payments_data <- payments_data %>% mutate_if(is.numeric, ~replace_na(., 0))

################   SUMMARY FOR OTHERS
payments_summary <- payments_data %>% group_by(ced, e_sistafe_pt, payment_beneficiary, nuit_creditado) %>% summarize(qty = sum(quantity30)*0.3 + sum(quantity10)*0.1 + sum(quantity100),
                                                                                                   valor = sum(paid_ammount))

payments_summary$qty <- ifelse(payments_summary$qty == 0, 1, payments_summary$qty)
payments_summary$preco <- payments_summary$valor/payments_summary$qty

payments_summary <-  modify_if(payments_summary, ~is.numeric(.), ~round(., 2))
payments_summary$e_sistafe_pt[is.na(payments_summary$e_sistafe_pt)] <- ""
payments_summary$nuit_creditado[is.na(payments_summary$nuit_creditado)] <- 0
payments_summary[is.na(payments_summary)] <- 0

payments_summary <- payments_summary %>% adorn_totals("row", fill = "", col_var_name = c("valor"))
payments_summary <- payments_summary %>% select(ced, e_sistafe_pt, qty, preco, everything())
payments_summary$ced <- as.character(payments_summary$ced)
setnames(payments_summary, c("ced", "e_sistafe_pt", "payment_beneficiary", "nuit_creditado", "qty", "preco", "valor"), c("CED", "Designação do CED", "Beneficiário", "NUIT", "Qty", "Preço" , "Valor (MT)"))

############ SUMMARY AJUDAS DE CUSTOS
payments_summary2 <- payments_data %>% group_by(ced, e_sistafe_pt, payment_beneficiary, nuit_creditado) %>% summarize(qty = sum(quantity30)*0.3 + sum(quantity10)*0.1 + sum(quantity100),
                                                                                                    valor = sum(paid_ammount))
payments_summary2$qty <- ifelse(payments_summary2$qty == 0, 1, payments_summary2$qty)
payments_summary2$preco <- payments_summary2$valor/payments_summary2$qty
payments_summary2 <-  modify_if(payments_summary2, ~is.numeric(.), ~round(., 2))
payments_summary2$e_sistafe_pt[is.na(payments_summary2$e_sistafe_pt)] <- ""
payments_summary2$nuit_creditado[is.na(payments_summary2$nuit_creditado)] <- 0
payments_summary2[is.na(payments_summary2)] <- 0
payments_summary2 <- payments_summary2 %>% adorn_totals("row", fill = "", col_var_name = c("valor"))
payments_summary2 <- payments_summary2 %>% select(ced, e_sistafe_pt, payment_beneficiary, nuit_creditado,qty, preco, everything())
payments_summary2$ced <- as.character(payments_summary2$ced) 
setnames(payments_summary2, c("ced", "e_sistafe_pt", "payment_beneficiary", "nuit_creditado", "qty", "preco", "valor"), c("CED", "Designação do CED", "Beneficiário", "NUIT", "Qty", "Preço" , "Valor (MT)"))

# payments_summary <- ifelse(tipo_despesa == "Ajudas de custos", payments_summary2, payments_summary)

set_flextable_defaults(font.size = 9, font.color = "black", table.layout = "fixed", digits = 1)
ft <- flextable(payments_summary)
ft <- theme_booktabs(ft)
ft <- autofit(ft)
ft <- colformat_double(x = ft, big.mark=",", digits = 2, na_str = "N/A")
ft <- align_nottext_col(ft, align = "right")
ft <- line_spacing(ft, space = 1.0, part = "all")
ft <- bold(ft, bold = TRUE, part = "header")
ft <- bold(ft, i = nrow(payments_summary), bold = TRUE)
ft <- padding(ft, padding = 1)
ft <- compose(ft, i = nrow(payments_summary), j = 3, as_paragraph(as_chunk('-')))
ft <- compose(ft, i = nrow(payments_summary), j = 4, as_paragraph(as_chunk('-')))
ft <- compose(ft, i = nrow(payments_summary), j = 5, as_paragraph(as_chunk('-')))

# ft
set_flextable_defaults(font.size = 9, font.color = "black", table.layout = "fixed", digits = 1)
ft2 <- flextable(payments_summary2)
ft2 <- theme_booktabs(ft2)
ft2 <- autofit(ft2)
ft2 <- colformat_double(x = ft2, big.mark=",", digits = 2, na_str = "N/A")
ft2 <- align_nottext_col(ft2, align = "right")
ft2 <- line_spacing(ft2, space = 1.0, part = "all")
ft2 <- bold(ft2, bold = TRUE, part = "header")
ft2 <- bold(ft2, i = nrow(payments_summary), bold = TRUE)
ft2 <- padding(ft2, padding = 1)
ft2 <- compose(ft2, i = nrow(payments_summary), j = 3, as_paragraph(as_chunk('-')))
ft2 <- compose(ft2, i = nrow(payments_summary), j = 4, as_paragraph(as_chunk('-')))
ft2 <- compose(ft2, i = nrow(payments_summary), j = 5, as_paragraph(as_chunk('-')))
ft2_ajudas <- compose(ft2, i = nrow(payments_summary), j = 6, as_paragraph(as_chunk('-')))

# ft2_ajudas
sample_doc <- cursor_reach(sample_doc, keyword = keywords)
sample_doc <- body_add_flextable(sample_doc, value = ft, pos = "after")

document <- glue_collapse(payments_data$document, ", ", last = " e ")

financiadores <- payments_data %>% pivot_longer(cols = c(ifadloan_pct:governmentmoney_pct), names_to = "contributors", values_to = "percent_financed")
financiadores$paid_ammount <- financiadores$paid_ammount* financiadores$percent_financed
financiadores <- financiadores %>% group_by(contributors) %>% summarize(paid_ammount = sum(paid_ammount)) %>% adorn_totals("col") %>% dplyr::filter(Total >0)

financiadores <- merge(financiadores, pdr_financiers, by.x = "contributors", by.y = "percentage_column", all.x = TRUE)

# financiadores$pdr_financiers
financiadores$percent <-  financiadores$Total/sum(payments_data$paid_ammount)*100
financiadores$contribs <- paste0(financiadores$details_contributors, " (", round(financiadores$percent,2), "%)")
funding <- glue_collapse(financiadores$contribs, ", ", last = " e ")

# print(funding)

sample_doc <- body_replace_all_text(sample_doc, old_value = "FUNDO DE FOMENTO AGRÁRIO E EXTENSÃO RURAL, FUNDO PÚBLICO (FAR, FP)", new_value = toupper(paste0(payments_data$instituicao[1])), only_at_cursor = FALSE, fixed = TRUE)
sample_doc <- body_replace_all_text(sample_doc, old_value = "MINISTÉRIO DA AGRICULTURA E DESENVOLVIMENTO RURAL", new_value = toupper(paste0(payments_data$entidade_governo[1])), only_at_cursor = FALSE, fixed = TRUE)
sample_doc <- body_replace_all_text(sample_doc, old_value = "UNIDADE_GESTORA", new_value = toupper(paste0(payments_data$unidade_gestora[1])), only_at_cursor = FALSE, fixed = TRUE)
sample_doc <- body_replace_all_text(sample_doc, old_value = "supplier_name", new_value = paste0(payments_data$payment_beneficiary[1]), only_at_cursor = FALSE, fixed = TRUE)
sample_doc <- body_replace_all_text(sample_doc, old_value = "submission_date", new_value = paste0(as.character(payments_data$submission_date[1], format = "%d/%m/%Y")), only_at_cursor = FALSE, fixed = TRUE)
sample_doc <- body_replace_all_text(sample_doc, old_value = "subcomponente_pt", new_value = paste0(payments_data$componentnames_pt[1]), only_at_cursor = FALSE, fixed = TRUE)
sample_doc <- body_replace_all_text(sample_doc, old_value = "subcompo_desc", new_value = paste0(payments_data$components_pt[1]), only_at_cursor = FALSE, fixed = TRUE)
sample_doc <- body_replace_all_text(sample_doc, old_value = "sectores", new_value = paste0(payments_data$sectores[1]), only_at_cursor = FALSE, fixed = TRUE)
sample_doc <- body_replace_all_text(sample_doc, old_value = "relevance", new_value = paste0(payments_data$relevance[1]), only_at_cursor = FALSE, fixed = TRUE)
sample_doc <- body_replace_all_text(sample_doc, old_value = "process_type", new_value = paste0(payments_data$process_type[1]), only_at_cursor = FALSE, fixed = TRUE)

sample_doc <- body_replace_all_text(sample_doc, old_value = "date_proposal_printed", new_value = formated_date, only_at_cursor = FALSE, fixed = TRUE)
sample_doc <- body_replace_all_text(sample_doc, old_value = "month_salaries_paid", new_value = month_salaries_paid, only_at_cursor = FALSE, fixed = TRUE)
sample_doc <- body_replace_all_text(sample_doc, old_value = "year_proposal", new_value = paste0(year_proposal), only_at_cursor = FALSE, fixed = TRUE)

sample_doc <- body_replace_all_text(sample_doc, old_value = "processos_numeros", new_value = paste0(payments_data$process_number[1]), only_at_cursor = FALSE, fixed = TRUE)

sample_doc <- body_replace_all_text(sample_doc, old_value = "physical_compl", new_value = paste0(payments_data$physical_compl[1]), only_at_cursor = FALSE, fixed = TRUE)
sample_doc <- body_replace_all_text(sample_doc, old_value = "paid_ammount", new_value = paste0(format(round(sum(payments_data$paid_ammount), 2), big.mark=",", nsmall = 2, scientific=FALSE)), only_at_cursor = FALSE, fixed = TRUE)
sample_doc <- body_replace_all_text(sample_doc, old_value = "funding", new_value = funding, only_at_cursor = FALSE, fixed = TRUE)
sample_doc <- body_replace_all_text(sample_doc, old_value = "new_salute", new_value = paste0(payments_data$new_salute[1]), only_at_cursor = FALSE, fixed = TRUE)
sample_doc <- body_replace_all_text(sample_doc, old_value = "new_approver", new_value = paste0(payments_data$new_approver[1]), only_at_cursor = FALSE, fixed = TRUE)

sample_doc <- body_replace_all_text(sample_doc, old_value = "name_applicant", new_value = paste0(payments_data$mid_names[1]), only_at_cursor = FALSE, fixed = TRUE)

sample_doc <- body_replace_all_text(sample_doc, old_value = "financial_compl", new_value = paste0(payments_data$financial_compl[1]), only_at_cursor = FALSE, fixed = TRUE)
sample_doc <- body_replace_all_text(sample_doc, old_value = "e_sistafe_w_code", new_value = paste0(payments_data$e_sistafe_pt[1]), only_at_cursor = FALSE, fixed = TRUE)
sample_doc <- body_replace_all_text(sample_doc, old_value = "documents_numbers", new_value = document, only_at_cursor = FALSE, fixed = TRUE)


sample_doc <- body_replace_all_text(sample_doc, old_value = "detailed", new_value = paste0(payments_data$expense_description[1]), only_at_cursor = FALSE, fixed = TRUE)
sample_doc <- body_replace_all_text(sample_doc, old_value = "awpb_description", new_value = paste0(payments_data$descricao_da_actividade[1]), only_at_cursor = FALSE, fixed = TRUE)
sample_doc <- body_replace_all_text(sample_doc, old_value = "cost_centers", new_value = paste0(payments_data$cost_centers[1]), only_at_cursor = FALSE, fixed = TRUE)
sample_doc <- body_replace_all_text(sample_doc, old_value = "contractdescription_pt", new_value = paste0(payments_data$contractdescription_pt[1]), only_at_cursor = FALSE, fixed = TRUE)
sample_doc <- body_replace_all_text(sample_doc, old_value = "CONTRACT_NUMBER", new_value = paste0(payments_data$contract_number[1]), only_at_cursor = FALSE, fixed = TRUE)
sample_doc <- body_replace_all_text(sample_doc, old_value = "contract", new_value = paste0(payments_data$contract_number[1]), only_at_cursor = FALSE, fixed = TRUE)
sample_doc <- body_replace_all_text(sample_doc, old_value = "categoria_pdr", new_value = paste0(payments_data$categoria_pdr[1]), only_at_cursor = FALSE, fixed = TRUE)
sample_doc <- body_replace_all_text(sample_doc, old_value = "carreira_proponente", new_value = paste0(payments_data$categoria[1]), only_at_cursor = FALSE, fixed = TRUE)
sample_doc <- body_replace_all_text(sample_doc, old_value = "awpb_id", new_value = paste0(payments_data$awpb_id[1]), only_at_cursor = FALSE, fixed = TRUE)
sample_doc <- footers_replace_all_text(sample_doc, old_value = "encedereco_alvo", new_value = paste0(payments_data$address[1]), only_at_cursor = FALSE, fixed = TRUE)
sample_doc <- footers_replace_all_text(sample_doc, old_value = "cidade_alvo", new_value = paste0(payments_data$city[1]), only_at_cursor = FALSE, fixed = TRUE)
sample_doc <- body_replace_all_text(sample_doc, old_value = "role", new_value = paste0(payments_data$short_vaccancy[1]), only_at_cursor = FALSE, fixed = TRUE)
sample_docx <- print(sample_doc, target = file.path(Sys.getenv("Home"), paste0("IP_", payments_data$paao_code[1], "_", "Salários e remunerações", ".docx")))



