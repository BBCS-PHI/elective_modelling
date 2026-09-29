library(readxl)
library(tidyverse)
source("_batch_diagnostics.R")

# Testing full in light of Tineke's advice
#in_dt <- read_excel("data/extract_20260814.xlsx", .name_repair = "universal_quiet")
library(readr)
in_tmp <- read_csv("data/waiting_list_data_diagnostics_20230903.csv")

#
# in_tmp$WL_start <- lag(in_tmp$TotalWL)
# in_tmp$Referrals <- in_tmp$TotalWL + in_tmp$WaitingListActivityExcludingPlanned + in_tmp$PlannedActivity - in_tmp$WL_start
# in_tmp$WLActivity <- in_tmp$WaitingListActivityExcludingPlanned + .in_tmp$PlannedActivity




in_dt <-
    in_tmp |>
    filter(!DiagnosticProcedure %in% c("BARIUM_ENEMA", "ELECTROPHYSIOLOGY")) |>
    mutate(end_date = as.Date(PeriodEndingDate, "%d/%m/%Y")) |>
    mutate(start_date = floor_date(end_date, unit = "month")) |>
    arrange(CCGCode, DiagnosticProcedure, start_date) |>
    group_by(CCGCode, DiagnosticProcedure) |>
    mutate(WL_start = lag(TotalWL)) |>
    ungroup()


in_dt <-
    in_dt |>
    mutate(Referrals = TotalWL + WaitingListActivityExcludingPlanned + PlannedActivity - WL_start ,
           Removals = as.numeric(NA),
           Org_Name = ifelse(CCGCode == '15E', 'Birmingham and Solihull ICB', 'Black Country ICB')) |>
    select(Org_Code = CCGCode,
           Org_Name,
           DiagnosticProcedure,
           start_date,
           end_date,
           Referrals,
           Removals,
           Waiting.list.size = TotalWL)







# # Split into feeder df for each org. Ideally needs a wrapper to apply to each, but will manually run for now.
bc_total <- filter(in_dt, Org_Code == "D2P2L") |> select(-Org_Code, -Org_Name)  |> mutate(adjustment = 1)
bsol_total <- filter(in_dt, Org_Code == "15E") |> select(-Org_Code, -Org_Name) |> mutate(adjustment = 1)

bc_total |>  group_by(DiagnosticProcedure) |> count()
bc_total |>  group_by(DiagnosticProcedure) |> count()

model_groups_list <-
    list(
        bc_total,
        bsol_total
        )

nms <- c("bc_total", "bsol_total")



names(model_groups_list) <-  nms

#purrr::map(model_groups_list,  ~wl_batch_func(.x, names(model_groups_list)))
#purrr::map(model_groups_list,  ~wl_batch_func(.x, names(model_groups_list)))
start <- Sys.time()
for (i in 1:length(model_groups_list)){
    wl_batch_func(model_groups_list[[i]], nms[i], "output/diagnostics3")
}
end <- Sys.time()

end - start
