library(bs4Dash)
library(DT)
library(pool)
library(dplyr)
library(tidyverse)
library(DBI)
library(RPostgres)
library(ggcharts)
library(ggplot2)
library(echarts4r)
library(janitor)
library(lubridate)
library(feather)
library(fst)
library(shinyvalidate)
library(shinyjs)
library(sodium)
library(httr)
library(bslib)
library(fresh)
library(splitstackshape)
library(zoo)
library(data.table)
library(flextable)
library(glue)
library(shinyFiles)
library(collapse)
library(shinyFeedback)
library(memoise)
library(readr)
library(readxl)
library(shinyalert)
library(lares)
library(uuid)
library(fontawesome)
library(shinyBS)
library(shinyWidgets)

forex_e_rate = 63.20

shinyOptions(cache = cachem::cache_disk(file.path(dirname(tempdir()), "myapp-cache")))

myToastOptions <- list(
  positionClass = "toast-top-right",
  progressBar = FALSE,
  timeOut = 2000,
  closeButton = TRUE,
  # same as defaults
  newestOnTop = TRUE,
  preventDuplicates = FALSE,
  showDuration = 300,
  hideDuration = 1000,
  extendedTimeOut = 1000,
  showEasing = "linear",
  hideEasing = "linear",
  showMethod = "fadeIn",
  hideMethod = "fadeOut"
)


bs4dash_font(size_base = "1.5rem", weight_bold = 900)
thematic::thematic_shiny(font = "auto")
options(scipen = 9999)
options(digits=15)
options(warn = 0)
e_rate = 64.46
forex_e_rate = 63.36


title <- tags$a(href='https://www.google.com',
                tags$img(src="PROCAVA_LOGO.png", height = '92.5', width = '220'),
                '', target="_blank")

db <- 'mozprocava'  
host_db <- "mozprocava.ckzqmdzjvnlv.us-east-1.rds.amazonaws.com"
db_port <- '5432'  
db_user <- "postgres"
db_password <- "MZ;PROCAVA"
Sys.setenv("AWS_ACCESS_KEY_ID" = "AKIATXOOGZ5WE6EIQP7B", "AWS_SECRET_ACCESS_KEY" = "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", "AWS_DEFAULT_REGION" = "us-east-1")
pool <- dbPool(RPostgres::Postgres(), dbname = db, host=host_db, port=db_port, user=db_user, password=db_password)

onStop(function() {poolClose(pool)})


approval_requests <- DBI::dbGetQuery(pool, "SELECT * FROM fiduciary.approval_requests")
answer_options <- unique(approval_requests$contract_vs_invoice)
df <- DBI::dbGetQuery(pool, "SELECT * FROM fiduciary.approval_requests")

#############  GET GLOBAL DATASETS
# payment_proposals <-  DBI::dbGetQuery(pool, "SELECT * FROM fiduciary.full_payment_proposals")
# pdr_financiers <-  DBI::dbGetQuery(pool, "SELECT details_contributors, percentage_column FROM fiduciary.pdr_financiers")
guarantee_issuers <-  data.frame(dbGetQuery(pool, SQL("SELECT issuer_name from fiduciary.guarantee_issuers")))
staff_choices <-  data.frame(dbGetQuery(pool, SQL("SELECT staff_name_codes from fiduciary.procava_staff")))
# payment_requests_choices <-  data.frame(dbGetQuery(pool, SQL("SELECT tipos_de_despesas, detailed, cost_centers FROM fiduciary.full_payments_dataset")))
countries <-  data.frame(dbGetQuery(pool, SQL("SELECT country from fiduciary.countries")))
pp_stages_pt <-  data.frame(dbGetQuery(pool, SQL("SELECT detailedstage_pt from fiduciary.pp_stages")))
pp_dataset <-  DBI::dbGetQuery(pool, "SELECT * from fiduciary.procurement_dossiers")
disbursed <- dbGetQuery(pool, "SELECT contravalor_mzn, disbursed_usd from fiduciary.withdrawal_applications")
procurement_view <- dbGetQuery(pool, "SELECT * FROM fiduciary.procurement_view")
components_design <- read_feather("component_years_project.feather")  %>% select(subcomponent = components, total)
awpb_updated <-  dbGetQuery(pool, "SELECT internal_responsible, awpb_id, costab_code, unidades, financiers, categories FROM procava.awpb_updates")
# pagamentos_aprovados <- dbGetQuery(pool, "SELECT * from fiduciary.full_approved_payments")
paao_granulado <-  read_feather('granular_awpb_2022.feather')
paao_unit <-  DBI::dbGetQuery(pool, "SELECT unidades from procava.awpb_updates")
paao_unit <-  c('relat??rios', 'manuais', 'ferramentas', unique(paao_unit$unidades))
ced_codes <- read_feather("e_sistafe.feather") %>% select(ced = esistafe_key, e_sistafe_w_code)  
procava_staff <-  dbGetQuery(pool, "SELECT awpb_role from fiduciary.procava_staff")
cost_tab <- read_feather('cost_tab.feather')
contractrecords_df <- read_feather('contractrecords_df.feather')
costtabs <- read_feather('costtabs.feather')
components_design <- read_feather("component_years_project.feather") %>% select(subcomponent = components, PDR=total)
cost_tabs_pdr <- read_fst("summarycosttables.fst")
contract_numero <-  DBI::dbGetQuery(pool, "SELECT contract_number FROM fiduciary.external_contract_ballance")
procava_measurement_units <- read_fst("procava_units.fst")
pp_methods <- read_feather('procurement_deadlines.feather') %>% select(method_name_pt)

# full_pay_proposals <- dbGetQuery(pool, "SELECT tipos_de_despesas, process_type, expense_description FROM fiduciary.full_payment_proposals")


risk_level <- c('Muito baixo'  =  'Very Low', 'Baixo'  =  'Low', 'M??dio'  =  'Medium', 'Alto'  =  'High', 'Muito alto'  =  'Very High')
garantias <- c('Garantia banc??ria'  =  'Bank Guarantee', 'Seguro garantia'  =  'Insurance Guarantee', 'Numer??rio'  =  'Cash', 'Cheque visado'  =  'Certified Cheque')
contract_status <- c('Negocia????o'  =  'Negotiation', 'Assinado'  =  'Signed', 'VTA ou anota????o'  =  'Clearance', 'Implementa????o'  =  'Under implementation', 'Conclu??do'  =  'Completed', 'Conflito'  =  'Pending, conflict', 'Cancelado'  =  'Cancelled', 'Fechado'  =  'Closed')
moeda <- c('USD', 'ZAR', 'GBP', 'EUR', 'MZN')

instituicoes <- c("DCM", "DINAT", "DNDAF", "DNDEL", "DNDP", "DPP", "FAR,FP", "GSSA", "IIAM", "INAM", "INIR", "SETSAN")
countries <-  as.character(unique(countries$country))
staff_choices <- as.character(unique(staff_choices$staff_name_codes))
guarantee_issuers <- as.character(unique(guarantee_issuers$issuer_name))

# tipos_de_despesas <- unique(payment_requests_choices$tipos_de_despesas)
# detailed <- unique(payment_requests_choices$detailed)
# cost_centers <- unique(payment_requests_choices$cost_centers)

pp_stages <- c('Solicita????o de Manifesta????es de Interesse Submetido ?? NO', 'Solicita????o de Manifesta????es de Interesses Aprovada', 'Solicita????o de Manifesta????es de Interesses Publicada', 'Manifesta????es de Interesses Submetidas', 'Relat??rio de Manifesta????es de Interesse Submetido ?? NO', 'Relat??rio de Manifesta????es de Interesse Aprovado', 'Convite para Apresenta????o de Propostas Submetido ?? NO', 'Convite para Apresenta????o de Propostas Aprovado', 'Convite para Apresenta????o de Propostas Publicado', 'Propostas Submetidas', 'Relat??rio de Propostas T??cnicas Submetido ?? NO', 'Relat??rio de Propostas T??cnicas Aprovado', 'Propostas Financeiras Abertas', 'Relat??rio Combinado Submetido ?? NO', 'Relat??rio Combinado Aprovado', 'Notifica????o da Inten????o de Adjudica????o', 'Adjudica????o do Contrato', 'Negocia????o do Contrato', 'Proposta do Contrato Submetido ?? N??o Objec????o', 'Proposta do Contrato Aprovado', 'Contrato Assinado', 'Contrato visado', 'Contrato em Implementa????o', 'Contrato encerrado', 'N??o iniciado', 'Dossier em lit??gio')
pp_stages_colours <- c('#ffa100', '#fbad7a', '#fbee7a', '#d0c229', '#d0a129', '#d0b529', '#e4f808', '#e0f808', '#b6f808', '#08ebf8', '#08f8d7', '#08f8be', '#9df808', '#8cf808', '#5ef808', '#69ff71', '#4dfc03', '#49e507', '#2ce507', '#0cd337', '#35aa07', '#0faa07', '#07aa10', '#058a0c', '#ff0f00', '#fb7a87')

pp_method_names_pt <- as.list(unique(pp_methods$method_name_pt))
codes <- as.list(unique(pp_stages_pt$detailedstage_pt))
pp_responsibles <- as.list(unique(pp_dataset$responsible))
pp_codings <- as.list(unique(pp_dataset$idpp))

labelMandatory <- function(label) {tagList(label,span("*", class = "mandatory_star"))}
appCSS <- ".mandatory_star { color: red; }"
# units_indicators  <- unique(paao_unit$unidades)
procava_cost_tabs <-  cost_tab
# paid_set <- pagamentos_aprovados
e_sistafe  <-  ced_codes %>% fselect(esistafe_key=ced, description_e_sistafe=e_sistafe_w_code)
# paid_set$quarters <- quarters(as.Date(paid_set$payment_date))

granular_awpb_2022  <- paao_granulado

PDR_categories <- cost_tabs_pdr %>% dplyr::filter(class == "categories") %>% select(pdr_category = Expenditure, PDR = total_cost)
PDR_categories$pdr_category[PDR_categories$pdr_category == "Credit, Guarantee Funds"] <- "Credit Guarantee Funds"
PDR_categories$pdr_category[PDR_categories$pdr_category == "Training"] <- "Trainings"
PDR_categories$pdr_category[PDR_categories$pdr_category == "Workshop"] <- "Workshops"
PDR_categories$pdr_category[PDR_categories$pdr_category == "Works"] <- "Civil Works"

detailed_pp_stages <- as.list(unique(pp_stages_pt$detailedstage_pt))
pp_responsibles <- as.list(unique(pp_dataset$responsible)) 

codes <- as.list(unique(awpb_updated$awpb_id))
costab_code <- as.list(unique(awpb_updated$costab_code))
responsaveis <- as.list(unique(awpb_updated$internal_responsible))

management_units <- sort(c("URGPS", "UNGP", "URGPC", "URGN", "UPGPN"))

labelMandatory <- function(label) {tagList(label,span("*", class = "mandatory_star"))}

appCSS <- ".mandatory_star { color: red; ,}"
awpb_situacoes <- c("N??o iniciada (atrasada)", "N??o iniciada (dentro do prazo)", "Iniciada (Execu????o < 50%)", "Estado avan??ado (50% < Execu????o < 75%)", "Quase conclu??da (75% < Execu????o < 100%)", "Conclu??da  (Execu????o >= 100%)", "Cancelada")

callback <- c(
  "var id = $(table.table().node()).closest('.datatables').attr('id');",
  "$.contextMenu({",
  "  selector: '#' + id + ' td.factor input[type=text]',",
  "  trigger: 'hover',",
  "  build: function($trigger, e){",
  "    var levels = $trigger.parent().data('levels');",
  "    if(levels === undefined){",
  "      var colindex = table.cell($trigger.parent()[0]).index().column;",
  "      levels = table.column(colindex).data().unique();",
  "    }",
  "    var options = levels.reduce(function(result, item, index, array){",
  "      result[index] = item;",
  "      return result;",
  "    }, {});",
  "    return {",
  "      autoHide: true,",
  "      items: {",
  "        dropdown: {",
  "          name: 'Edit',",
  "          type: 'select',",
  "          options: options,",
  "          selected: 0",
  "        }",
  "      },",
  "      events: {",
  "        show: function(opts){",
  "          opts.$trigger.off('blur');",
  "        },",
  "        hide: function(opts){",
  "          var $this = this;",
  "          var data = $.contextMenu.getInputValues(opts, $this.data());",
  "          var $input = opts.$trigger;",
  "          $input.val(options[data.dropdown]);",
  "          $input.trigger('change');",
  "        }",
  "      }",
  "    };",
  "  }",
  "});"
)

createdCell <- function(levels){
  if(missing(levels)){
    return("function(td, cellData, rowData, rowIndex, colIndex){}")
  }
  quotedLevels <- toString(sprintf("\"%s\"", levels))
  c(
    "function(td, cellData, rowData, rowIndex, colIndex){",
    sprintf("  $(td).attr('data-levels', '[%s]');", quotedLevels),
    "}"
  )
}


procava_measurement_units <- c('relat??rios', 'manuais', 'ferramentas', unique(as.character(procava_measurement_units$unit_pt)))

units_indicators <- procava_measurement_units

process_types <- c('Reembolso de fundos dos t??cnicos',
                   'Pagamento de sal??rios e remunera????es',
                   'Pagamento de despesas de log??stica de viagens',
                   'Pagamento de fundos cobrados por fornecedores', 
                   'Adiantamento de fundos aos t??cnicos', 
                   'Adiantamento de fundos aos fornecedores',
                   'Pagamento de despesas de realiza????o de evento',
                   'Capta????o da comparticipa????o dos benefici??rios' = 'Contribuicao_dos_Beneficiarios',
                   'Capta????o da Comparticipa????o do Governo' = 'Contribuicao_do_Governo',
                   'Capta????o da Comparticipa????o do Sector Privado' = 'Contribuicao_dos_Privados')


programme_positions <- c('Cassava Value Chain Specialist', 
                         'Safeguards and Climate Adaptation Specialist', 
                         'Livestock Value Chains Specialist', 
                         'Livestock Value Chain Specialist', 
                         'National Procurement Officer', 
                         'Horticulture Value Chain Specialist ', 
                         'Horticulture Value Chain Specialist', 
                         'Livestock Value Chain Officer', 
                         'Monitoring and Evaluation Officer (National)', 
                         'Infrastructure and Irrigation Specialist', 
                         'Nutrition and Product Development Specialist', 
                         'Financial Services and Agribusiness Officer', 
                         'Gender, Youth and Targeting Specialist', 
                         'GIS and Natural Resources Officer', 
                         'Knowledge Management and Communication Officer', 
                         'National Finance Manager', 
                         'National Programme Coordinator', 
                         'National Infrastructure Officer', 
                         'Monitoring and Evaluation Officer (Regional / Provincial)', 
                         'Finance Manager (National)')

ced_coding <- c('', 'Sal??rios e remunera????es (111100)' = '111100', 
                'Ajudas de custo no Pa??s (112101)' = '112101', 
                'Ajudas de custo fora do Pa??s (112102)' = '112102', 
                'Bens (121000)' = '121000', 
                'Combust??veis e lubrificantes (121001)' = '121001', 
                'Material de consumo para escrit??rio (121005)' = '121005',
                'G??neros aliment??cios (121010)' = '121010', 
                'Servi??os (122000)' = '122000', 
                'Comunica????es em geral (122001)' = '122001', 
                'Passagens a??reas dentro do Pa??s (122003)' = '122002', 
                'Passagens fora do Pa??s (122003)' = '122003', 
                'Transfer??ncias  correntes  a administra????es privadas (142099)' = '142099', 
                'Bolsas de estudo no pa??s (143401)' = '143401', 
                'Constru????es (211000)' = '211000',
                'Maquinaria, equipamento e mobili??rio (212000)' = '212000', 
                'Meios de transporte (213000)' = '213000', 
                'Animais (214101)' = '214101', 
                'Transfer??ncias de capital a administra????es privadas (222099)' = '222099', 
                'Demais despesas de capital (224000)' = '224000')

# full_payments <- dbGetQuery(pool, "SELECT * FROM fiduciary.full_approved_payments")

# procava_payments <- full_payments

# procava_payments$quarters_paid <- as.yearqtr(as.Date(procava_payments$submission_date, "%m/%d/%Y"))
# procava_payments <- concat.split(data = procava_payments, split.col = which(colnames(procava_payments) == "quarters_paid"), sep = " ", drop = FALSE)
# setnames(procava_payments, c("quarters_paid_1", "quarters_paid_2"), c("year_paid", "quarter_paid"))
aggreed_action <- as.data.frame(dbGetQuery(pool, "SELECT mission_name, action_responsible, action_technician FROM procava.full_aggreed_actions"))
mission_names <- unique(aggreed_action$mission_name)
responsible_institut <- unique(aggreed_action$action_responsible)
responsible_tech <- unique(aggreed_action$action_technician)

variaveis_contratos <- c('M??todo de procurement' = "proc_method",
                         'Unidade Gestora (UGB)' = "cost_center",
                         'Pa??s da contratada' = "supplier_country",
                         'Gestor do Contrato' = "contract_manager",
                         'Infraestrutura' = "infrastructure",
                         'Categoria de procurement' = "procurement_type",
                         'Revis??o' = "review",
                         'Situa????o do contrato' = "contract_status",
                         'N??vel de risco' = "risk_flag")

valores_contratos <- c("Valor revisto" = "revised_ammount", "Valor pago" = "ammount_paid")

valores_contratos <- c("Valor revisto" = "revised_ammount", "Valor pago" = "value_paid")

contract_numbers <-  unique(contract_numero$contract_number)

distritos_procava <- c("UNGP", "URGPS", "URGPC", "URGPN", "UPGPN", 'Alto Mol??cu??' = 'Alto_Molocue', 'Ancuabe' = 'Ancuabe', 'Angoche' = 'Angoche', 'Ang??nia' = 'Angonia', 'Balama' = 'Balama', 'B??ru??' = 'Barue', 'Beira' = 'Beira', 'Bilene' = 'Bilene', 'Boane' = 'Boane', 'B??zi' = 'Buzi', 'Cahora Bassa' = 'Cahora_Bassa', 'Caia' = 'Caia', 'Changara' = 'Changara', 'Chemba' = 'Chemba', 'Cheringoma' = 'Cheringoma', 'Chibabava' = 'Chibabava', 'Chibuto' = 'Chibuto', 'Chicualacuala' = 'Chicualacuala', 'Chifunde' = 'Chifunde', 'Chigubo' = 'Chigubo', 'Chimbonila' = 'Chimbonila', 'Chimoio' = 'Chimoio', 'Chinde' = 'Chinde', 'Chi??re' = 'Chiure', 'Chiuta' = 'Chiuta', 'Ch??kw??' = 'Chokwe', 'Chongoene' = 'Chongoene', 'Cuamba' = 'Cuamba', 'Derre' = 'Derre', 'D??a' = 'Doa', 'Dondo' = 'Dondo', 'Er??ti' = 'Erati', 'Estrangeiro' = 'Estrangeiro', 'Funhalouro' = 'Funhalouro', 'Gil??' = 'Gile', 'Gondola' = 'Gondola', 'Gorongoza' = 'Gorongoza', 'Govuro' = 'Govuro', 'Guij??' = 'Guija', 'Guro' = 'Guro', 'Guru??' = 'Gurue', 'Homo??ne' = 'Homoine', 'Ibo' = 'Ibo', 'Ile' = 'Ile', 'Ilha de Mo??ambique' = 'Ilha_de_Mocambique', 'Inhambane' = 'Inhambane', 'Inharrime' = 'Inharrime', 'Inhassoro' = 'Inhassoro', 'Inhassunge' = 'Inhassunge', 'Jangamo' = 'Jangamo', 'Kamavota' = 'Kamavota', 'KaMaxakeni' = 'KaMaxakeni', 'KaMphumu' = 'KaMphumu', 'KaMubukwana' = 'KaMubukwana', 'KaNyaka' = 'KaNyaka', 'Katembe' = 'Katembe', 'Lago' = 'Lago', 'Lalaua' = 'Lalaua', 'Larde' = 'Larde', 'Lichinga' = 'Lichinga', 'Limpopo' = 'Limpopo', 'Li??po' = 'Liupo', 'Luabo' = 'Luabo', 'Lugela' = 'Lugela', 'Mabalane' = 'Mabalane', 'Mabote' = 'Mabote', 'Macanga' = 'Macanga', 'Macate' = 'Macate', 'Machanga' = 'Machanga', 'Machaze' = 'Machaze', 'Macomia' = 'Macomia', 'Macossa' = 'Macossa', 'Maganja da Costa' = 'Maganja_da_Costa', 'M??go??' = 'Magoe', 'Magude' = 'Magude', 'Majune' = 'Majune', 'Malema' = 'Malema', 'Mandimba' = 'Mandimba', 'Mandlakazi' = 'Mandlakazi', 'Manhi??a' = 'Manhica', 'Manica' = 'Manica', 'Mapai' = 'Mapai', 'Marara' = 'Marara', 'Mar??via' = 'Maravia', 'Mar??ngu??' = 'Maringue', 'Marracuene' = 'Marracuene', 'Marromeu' = 'Marromeu', 'Marrupa' = 'Marrupa', 'Massangena' = 'Massangena', 'Massinga' = 'Massinga', 'Massingir' = 'Massingir', 'Matola' = 'Matola', 'Matutu??ne' = 'Matutuine', 'Ma??a' = 'Maua', 'Mavago' = 'Mavago', 'Maxixe' = 'Maxixe', 'Mecanhelas' = 'Mecanhelas', 'Meconta' = 'Meconta', 'Mecub??ri' = 'Mecuburi', 'Mec??fi' = 'Mecufi', 'Mecula' = 'Mecula', 'Meluco' = 'Meluco', 'Memba' = 'Memba', 'Metarica' = 'Metarica', 'Metuge' = 'Metuge', 'Milange' = 'Milange', 'Moamba' = 'Moamba', 'Moatize' = 'Moatize', 'Moc??mboa da Praia' = 'Mocimboa_da_Praia', 'Mocuba' = 'Mocuba', 'Mocubela' = 'Mocubela', 'Mogincual' = 'Mogincual', 'Mogovolas' = 'Mogovolas', 'Molumbo' = 'Molumbo', 'Moma' = 'Moma', 'Monapo' = 'Monapo', 'Montepuez' = 'Montepuez', 'Mopeia' = 'Mopeia', 'Morrumbala' = 'Morrumbala', 'Morrumbene' = 'Morrumbene', 'Morrupula' = 'Morrupula', 'Mossuril' = 'Mossuril', 'Mossurize' = 'Mossurize', 'Muanza' = 'Muanza', 'Muecate' = 'Muecate', 'Mueda' = 'Mueda', 'Muembe' = 'Muembe', 'Mulevala' = 'Mulevala', 'Mutarara' = 'Mutarara', 'Nacala a Velha' = 'Nacala_a_Velha', 'Nacar??a' = 'Nacaroa', 'Namaacha' = 'Namaacha', 'Namacurra' = 'Namacurra', 'Namarroi' = 'Namarroi', 'Nampula' = 'Nampula', 'Namuno' = 'Namuno', 'Nangade' = 'Nangade', 'Ngauma' = 'Ngauma', 'Nhamatanda' = 'Nhamatanda', 'Nicoadala' = 'Nicoadala', 'Nipepe' = 'Nipepe', 'Nlhamankulu' = 'Nlhamankulu', 'Palma' = 'Palma', 'Panda' = 'Panda', 'Pebane' = 'Pebane', 'Pemba' = 'Pemba', 'Quelimane' = 'Quelimane', 'Quissanga' = 'Quissanga', 'Rapale' = 'Rapale', 'Rib??u??' = 'Ribaue', 'Sanga' = 'Sanga', 'Sussundenga' = 'Sussundenga', 'Tambara' = 'Tambara', 'Tete' = 'Tete', 'Tsangano' = 'Tsangano', 'Vanduzi' = 'Vanduzi', 'Vilankulo' = 'Vilankulo', 'Xai Xai' = 'Xai_Xai', 'Zavala' = 'Zavala', 'Zumbo' = 'Zumbo')
staff_nuit <- c('Am??ncio Ant??nio Nhantumbo' = '106803609', 'And??rcio Vitane' = '111175047', 'Augusto Oreste' = '118185331', 'Lu??sa ??ngela Josselina Calima' = '128504222', 'Daniel Louren??o Chitupila' = '103847915', 'Dion??sia Castelo Machuza Cuna' = '123200871', 'Eduardo Marcos Cuamba' = '107958584', 'Ana Crimilda Fernando Silva' = '111730814', 'Baptista Ruben Ngine Zunguze' = '103149061', 'Ernesto Abrantes Dulamo Wane' = '104922521', 'Daniel Ozias Mate' = '103659124', 'Eg??dio Artur Alfredo Mutimba' = '103260116', 'Esperan??a David Muchanga' = '109197580', 'Eug??nio Nhone' = '102542762', 'Gil Estev??o Nhantumbo' = '100894343', 'Jer??nimo Joaquim Francisco' = '110589123', 'Joaquim Daniel Macaringue' = '300249248', 'Jos?? Sancho Cumbi' = '100870630', 'J??lio Aguiar Bila' = '106864233', 'J??lio Marcelino Macaco' = '103160987', 'Lucas Albino Chiau' = '119429897', 'Luc??lia Santos' = '100864665', 'Manuel Tinga Mangueze' = '100868113', 'Neide Cust??dio Daniel' = '102032330', 'Neila L??cia da Concei????o Manjate' = '110757891', 'Rachida Jafar Abdul' = '115695673', 'Ibraimo Assuade Assane' = '104468241', 'Tiago Tiago' = '112841822', 'Joaquim Daniel Macaringue' = '108596120', '??mina Amade Muss?? Faquir??' = '101202429', 'Nilza Racide Abdul Adolfo' = '118151771', 'Osvaldo L??zaro Banze' = '122094545', 'Ros??rio Aide' = '103846501')



