library(feather)
library(tidyverse)
library(openxlsx)
metas_granulares <- read_feather("C:/Users/Administrator/Dropbox/web_apps/pim/procava_granular_targets.feather")

unique(metas_granulares$admin_prov)

producers <- metas_granulares %>% dplyr::filter(indicadores_simplex == "producers")
producers <- producers %>% dplyr::filter(crops %in% c("beans", "cassava", "potato")) %>% dplyr::filter(admin_prov %in% c("NIASSA", "MAPUTO CIDADE", "GAZA", "INHAMBANE", "MAPUTO"))

write.csv(producers, "C:\\Users\\Administrator\\Desktop\\Intensiva.csv")

paao_granular <- read_feather("C:\\Users\\Administrator\\OneDrive - ACQUABUILD\\SCRIPTS\\PROCAVA_PMU\\pmu_production\\granular_awpb_2022.feather")
rpsf <- paao_granular %>% dplyr::filter(financiers == "RPSF 1st Allocation" | financiers == "RPSF 2nd Allocation")
rpsf <- rpsf %>% group_by(ced) %>% summarise(orcamento = sum(granularmzn))

write.csv(metas_granulares, "C:\\Users\\Administrator\\Desktop\\metas_granulares.csv")
metas_granulares$ano_fiscal <- paste("year_", metas_granulares$fiscal_year)


metas <- metas_granulares %>% group_by(admin_distrito, xlsform_locality, culturas, descricao_da_actividade, ano_fiscal
) %>% summarize(meta = sum(target)) %>% pivot_wider(names_from = "ano_fiscal", values_from = "meta")

# write.csv(colnames(metas_granulares), "C:\\Users\\Administrator\\Desktop\\colunas_metas.csv")
metas_rever <- pivot_wider(metas_granulares, names_from = "ano_fiscal", values_from = "target")

write.xlsx(metas, file = "C:\\Users\\Administrator\\Desktop\\metasrevistas_2022.xlsx", colNames = TRUE, borders = "columns")
