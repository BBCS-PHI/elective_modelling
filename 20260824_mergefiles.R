
library(data.table)
library(dplyr)



nms <- c("bc_total", #"bc_rbk", "bc_rl4",
         #"bc_rna", "bc_rxk", "bc_other",
         "bsol_total"#,
         # "bsol_rrk",
         # "bsol_rq3"
         # , "bsol_rrj", "bsol_ryw", "bsol_other"
)


base_dir <- "output/diagnostics3"
#base_dir <- "output/base_model2"

csv_files <- paste0(base_dir,"/", nms, "_long.csv")
#csv_files <- csv_files[2:7]

# Read all as a list
dt_list <- lapply(csv_files, fread)

# Bind and add id column for the list index
combined_dt <- rbindlist(dt_list, use.names = TRUE, fill = TRUE, idcol = "file_id")

# Build a lookup of file metadata
file_meta <- data.table(
    file_id       = seq_along(csv_files),
    #source_folder = basename(dirname(csv_files))
    provider_code = sub("^[^_]*_", "", nms)
)

# Add metadata to combined data
combined_dt <- file_meta[combined_dt, on = "file_id"]


future_dt <-
    combined_dt |>
    filter(as.Date(end_date) > as.Date("31/03/2026", "%d/%m/%Y"))


future_dt <-
    future_dt |>
    mutate(weeks = as.numeric(difftime(end_date, start_date, units = "days"))/7)

#
# sub <-
#     future_dt |>
#     filter(provider_code == "total" & Specialty == '999')
# View(sub)
#
#
# sub2 <-
#     out |>
#     filter(Provider == "total" & Specialty == '999')
# View(sub2)

out <-
    future_dt |>
    mutate(capacity_relief = ceiling(calc_capacity * weeks),
           capacity_do_nothing = ceiling(Removals * weeks),

           predicted_demand = round(Referrals * weeks) ,
           ICB = ifelse(file_id == 1, "BC", "BSOL"),  # Added for diagnostics
           #ICB = ifelse(Commissioner_Code == "D2P2L", "BC", "BSOL"),
           Provider = provider_code ) |>
    mutate(capacity_difference = capacity_relief - capacity_do_nothing,
           start_date = as.Date(start_date),
           end_date = as.Date(end_date)) |>

    select(ICB, Provider, DiagnosticProcedure, start_date, end_date,
           `Demand (predicted)` = predicted_demand,
           `Capacity (do nothing)` = capacity_do_nothing,
           `Waiting list size (do nothing)` = Waiting.list.size,
           `Waiting list compliance (do nothing)` = wl_performance_cur,
           `Capacity (relief)` = capacity_relief,
           `Waiting list size (relief)` = Waiting.list.size_relief,
           `Target (sustainable) WL size` = target_wl,
           `Target compliance with 18 weeks` = target,
           `Waiting list compliance (relief)` = wl_performance_rel,
           calc_capacity = calc_capacity,
           weeks,
           `Capacity difference` = capacity_difference)



fwrite(out, "output/diagnostics3/model_summary2.csv")
#fwrite(out, "output/base_model2/model_output_20260826.csv")

#
# out |>
#     filter(Specialty == '999' & Provider == "total" & ICB == "BSOL")
