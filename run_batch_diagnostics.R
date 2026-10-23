library(readxl)
library(tidyverse)
source("_batch_diagnostics.R")

# Testing full in light of Tineke's advice
#in_dt <- read_excel("data/extract_20260814.xlsx", .name_repair = "universal_quiet")
library(readr)
## total
in_tmp <- read_csv("data/waiting_list_data_diagnostics_20230903.csv")
in_tmp <- read_excel("data/Sandwell_Performance_Data_CM_20261005.xlsx",
                   sheet = "Data")

# Only take first quarter of 2026/27
in_tmp <-
    in_tmp |>
    filter(PeriodEndingDate < as.Date("2026-07-01", "%Y-%m-%d"))


#
# in_tmp$WL_start <- lag(in_tmp$TotalWL)
# in_tmp$Referrals <- in_tmp$TotalWL + in_tmp$WaitingListActivityExcludingPlanned + in_tmp$PlannedActivity - in_tmp$WL_start
# in_tmp$WLActivity <- in_tmp$WaitingListActivityExcludingPlanned + .in_tmp$PlannedActivity

in_tmp |>
    group_by(CCGCode) |>
    count()


in_dt <-
    in_tmp |>
    filter(!DiagnosticProcedure %in% c("BARIUM_ENEMA", "ELECTROPHYSIOLOGY")) |>
    mutate(end_date = as.Date(PeriodEndingDate)) |>
    mutate(start_date = floor_date(end_date, unit = "month")) |>
#    arrange(CCGCode, DiagnosticProcedure, start_date) |>
#    group_by(CCGCode, DiagnosticProcedure) |>
    select(start_date, end_date, DiagnosticProcedure, ProviderCode, ProviderName,
           TotalWL, WaitingListActivityExcludingPlanned, PlannedActivity, TotalActivity) |>
    arrange(ProviderCode, ProviderName, DiagnosticProcedure, start_date, end_date) |>
    group_by(ProviderCode, ProviderName, DiagnosticProcedure, start_date, end_date) |>
    summarise(TotalWL = sum(TotalWL),
              WaitingListActivityExcludingPlanned = sum(WaitingListActivityExcludingPlanned),
              PlannedActivity = sum(PlannedActivity),
              TotalActivity = sum(TotalActivity)
    ) |> ungroup() |>
    group_by(ProviderCode, ProviderName, DiagnosticProcedure) |>
    mutate(WL_start = lag(TotalWL, n = 1)) |>
    ungroup() #|>
    #filter(start_date > as.Date("2025-03-31", "%Y-%m-%d"))

#write.csv(in_dt, "sandwell_check.csv")

in_dt |>
    filter(DiagnosticProcedure %in% c("CT")) |>
    arrange(start_date)

in_dt <-
    in_dt |>
    mutate(Referrals = ifelse((TotalWL + WaitingListActivityExcludingPlanned + PlannedActivity - WL_start) < 0, 0,
           (TotalWL + WaitingListActivityExcludingPlanned + PlannedActivity - WL_start)),
           Removals = TotalActivity,
           #Removals = as.numeric(NA),
           Org_Name = ProviderName,
           #Org_Name = ifelse(CCGCode == '15E', 'Birmingham and Solihull ICB', 'Black Country ICB')
           ) |>

    select(Org_Code = ProviderCode, # CCGCode,
           Org_Name,
           DiagnosticProcedure,
           start_date,
           end_date,
           Referrals,
           Removals,
           Waiting.list.size = TotalWL)







# # Split into feeder df for each org. Ideally needs a wrapper to apply to each, but will manually run for now.
#bc_total <- filter(in_dt, Org_Code == "D2P2L") |> select(-Org_Code, -Org_Name)  |> mutate(adjustment = 1)
#bsol_total <- filter(in_dt, Org_Code == "15E") |> select(-Org_Code, -Org_Name) |> mutate(adjustment = 1)

rxk_total <- filter(in_dt, Org_Code == "RXK") |> select(-Org_Code, -Org_Name) |> mutate(adjustment = 1)


#bc_total |>  group_by(DiagnosticProcedure) |> count()
#bc_total |>  group_by(DiagnosticProcedure) |> count()

rxk_total |>  group_by(DiagnosticProcedure) |> count()

model_groups_list <-
    list(
        rxk_total
        #bc_total,
        #bsol_total
        )

nms <- c("rxk_total"
    #"bc_total", "bsol_total"
    )



names(model_groups_list) <-  nms

#purrr::map(model_groups_list,  ~wl_batch_func(.x, names(model_groups_list)))
#purrr::map(model_groups_list,  ~wl_batch_func(.x, names(model_groups_list)))
start <- Sys.time()
for (i in 1:length(model_groups_list)){
    wl_batch_func(model_groups_list[[i]], nms[i], "output/sandwell3")
}
end <- Sys.time()

end - start
