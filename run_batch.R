library(readxl)
library(tidyverse)
source("_batch.R")

# Testing full in light of Tineke's advice
in_dt <- read_excel("data/extract_20260814.xlsx", .name_repair = "universal_quiet")

#Manual corrections
#September, bsol other 170
tmp <- rbind(
    data.frame(com_cd = '15E', org_cd = 'Other', org_nm = 'Other', sp = '170', st = as.Date("2025-09-01"), ed = as.Date("2025-09-30"), refs = 16, rem = 20, wl = 48),
    data.frame(com_cd = '15E', org_cd = 'Other', org_nm = 'Other', sp = '300', st = as.Date("2024-05-01"), ed = as.Date("2025-05-31"), refs = 8, rem = 4, wl = 12),
    data.frame(com_cd = 'D2P2L', org_cd = 'RL4', org_nm = 'THE ROYAL WOLVERHAMPTON NHS TRUST', sp = 'X06', st = as.Date("2025-12-01"), ed = as.Date("2025-12-31"), refs = 21, rem = 20, wl = 146),
    data.frame(com_cd = '15E', org_cd = 'RQ3', org_nm = "BIRMINGHAM WOMEN'S AND CHILDREN'S NHS FOUNDATION TRUST", sp = 'X02', st = as.Date("2025-04-01"), ed = as.Date("2025-04-30"), refs = 420, rem = 380, wl = 1800)

)

names(tmp) <- names(in_dt)
in_dt <- rbind(in_dt, tmp)


## Rename and remove WL column so don't have to update later code
in_dt$Waiting.list.size <- as.integer(in_dt$Waiting_List_Size)
in_dt$Waiting_List_Size <- NULL

in_dt <- arrange(in_dt, Commissioner_Code, Org_Code, Specialty, start_date)

# Exclusions for partial periods or other reasons
# # 160 is small numbers and a bit odd
in_dt <-
    in_dt |>
    filter(!(Specialty == '160' & Org_Code == "RBK"))

# in_dt <-
#     in_dt |>
#     filter(!(Specialty == '430' & Org_Code == "RBK"))


in_dt <-
    in_dt |>
    filter(!(Specialty == '300' & Org_Code == "RL4"))


in_dt <-
    in_dt |>
    filter(!(Specialty == '300' & Org_Code == "RNA"))

in_dt <-
    in_dt |>
    filter(!(Specialty == '170' & Org_Code == "RXK"))


in_dt <-
    in_dt |>
    filter(!(Specialty == 'X03' & Commissioner_Code == "D2P2L"))

# Both ICBs
in_dt <-
    in_dt |>
    filter(!(Specialty == '430' & Commissioner_Code == "D2P2L" & Org_Code == "Other"))


in_dt <-
    in_dt |>
    filter(!(Specialty == 'X03'& Org_Code == "RYW"))



in_dt <-
    in_dt |>
    filter(!(Specialty == 'X05'& Org_Code == "RQ3"))

in_dt <-
    in_dt |>
    filter(!(Specialty == '430'& Org_Code == "Other" & Commissioner_Code == "15E"))

in_dt <-
    in_dt |>
    filter(!(Specialty == '300'& Org_Code == "Other" & Commissioner_Code == "15E"))

in_dt <-
    in_dt |>
    filter(!(Specialty == '170'& Org_Code == "Other" & Commissioner_Code == "15E"))


in_dt <-
    in_dt |>
    filter(!(Specialty == 'X03'& Org_Code == "Other"))





# Need to add a year of x02 at rq3 prior to submission, or cut a year off
#This was 13 months needed.


# # Split into feeder df for each org. Ideally needs a wrapper to apply to each, but will manually run for now.
bc_total <- filter(in_dt, Org_Code == "D2P2L") |> select(-Org_Code, -Org_Name)  |> mutate(adjustment = 1)
bc_rbk <- filter(in_dt, Org_Code == "RBK") |> select(-Org_Code, -Org_Name) |> mutate(adjustment = 1)
bc_rl4 <- filter(in_dt, Org_Code == "RL4") |> select(-Org_Code, -Org_Name) |> mutate(adjustment = 1)
bc_rna <- filter(in_dt, Org_Code == "RNA") |> select(-Org_Code, -Org_Name) |> mutate(adjustment = 1)
bc_rxk <- filter(in_dt, Org_Code == "RXK") |> select(-Org_Code, -Org_Name) |> mutate(adjustment = 1)
bc_other <- filter(in_dt, Org_Code == "Other" & Commissioner_Code == "D2P2L") |> select(-Org_Code, -Org_Name) |> mutate(adjustment = 1)
#

#
bsol_total <- filter(in_dt, Org_Code == "15E") |> select(-Org_Code, -Org_Name) |> mutate(adjustment = 1)
bsol_rrk <- filter(in_dt, Org_Code == "RRK") |> select(-Org_Code, -Org_Name)  |> mutate(adjustment = 0.5)
bsol_rq3 <- filter(in_dt, Org_Code == "RQ3") |> select(-Org_Code, -Org_Name) |> mutate(adjustment = 0.07)
bsol_rrj <- filter(in_dt, Org_Code == "RRJ") |> select(-Org_Code, -Org_Name) |> mutate(adjustment = 0.04)
bsol_ryw <- filter(in_dt, Org_Code == "RYW") |> select(-Org_Code, -Org_Name) |> mutate(adjustment = 0.1)
bsol_other <- filter(in_dt, Org_Code == "Other" & Commissioner_Code == "15E") |> select(-Org_Code, -Org_Name) |> mutate(adjustment = 0.29)

#bsol_other

# in_bsol <-
#     in_bsol |>
#     filter(Specialty != 'X03')

# in_bc <-
#     in_bc |>
#     filter(Specialty != 'X03')

# Remove several DQ issues:
# single 300 in RBK:

# in_bc <-
#    in_bc |>
#    filter(!(Specialty == '300' & Org_code == "RBK"))
#
# # 170 at rxk.  0 referrals and removal smost weeks
#
# in_bc <-
#    in_bc |>
#    filter(!(Specialty == '170' & Org_code == "RXK"))
#
# # 160 is small numbers and a bit odd
# in_bc <-
#    in_bc |>
#    filter(!(Specialty == '160' & Org_code == "RXK"))

# # Rq3 -
# # Drop 100, 130, X05 due to very small numbers
# in_bsol <-
#    in_bsol |>
#    filter(!(Specialty == '100' & Org_Code == "RQ3"))
#
# in_bsol <-
#     in_bsol |>
#     filter(!(Specialty == '130' & Org_Code == "RQ3"))
#
# in_bsol <-
#     in_bsol |>
#     filter(!(Specialty == 'X05' & Org_Code == "RQ3"))


# in_bsol <-
#     in_bsol |>
#     filter(!(Specialty == '150' & Org_Code == "RRJ"))
#
#
# in_bsol <-
#     in_bsol |>
#     filter(!(Specialty == 'X04' & Org_Code == "RYW"))



model_groups_list <-
    list(
        bc_total,
        bc_rbk,
        bc_rl4,
        bc_rna,
        bc_rxk,
        bc_other,
        bsol_total,
        bsol_rrk,
        bsol_rq3 ,
        bsol_rrj,
        bsol_ryw,
        bsol_other
        )

nms <- c("bc_total", "bc_rbk", "bc_rl4",
         "bc_rna", "bc_rxk", "bc_other", "bsol_total",
         "bsol_rrk",
         "bsol_rq3"
         , "bsol_rrj", "bsol_ryw",
"bsol_other"
         )


names(model_groups_list) <-  nms

#purrr::map(model_groups_list,  ~wl_batch_func(.x, names(model_groups_list)))
#purrr::map(model_groups_list,  ~wl_batch_func(.x, names(model_groups_list)))
start <- Sys.time()
for (i in 1:length(model_groups_list)){
    wl_batch_func(model_groups_list[[i]], nms[i])
}
end <- Sys.time()

end - start
