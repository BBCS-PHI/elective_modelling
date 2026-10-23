library(readxl)
library(tidyverse)
library(tsibble)
library(lubridate)
library(feasts)
library(fable)

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


# -------------------------------------------------------------------------
# Create monthly organisation-level time series
# -------------------------------------------------------------------------

org_ts <- in_dt %>%
    mutate(
        month = yearmonth(start_date)
    ) %>%
    group_by(Org_Code, month) %>%
    summarise(
        Referrals         = sum(Referrals, na.rm = TRUE),
        Removals          = sum(Removals, na.rm = TRUE),
        Waiting.list.size = sum(Waiting.list.size, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    as_tsibble(
        key = Org_Code,
        index = month
    )

#------------------------------------
# Gaps
# ----------------------------------

org_ts %>%
    has_gaps()

org_ts <-
    org_ts |>
    filter(!Org_Code == 'Other')
# -----------------------------------------------------------------------------
# STL decomposition
# -----------------------------------------------------------------------------
# -------------------------------------------------------------------------
# STL decomposition
# -------------------------------------------------------------------------

stl_refs <- org_ts %>%
    model(
        STL(Referrals ~ trend() + season(period = "1 year"), robust = TRUE)
    )


stl_refs %>%
    components() %>%
    autoplot()


stl_rem <- org_ts %>%
    model(
        STL(Removals ~ trend() + season(period = "1 year"), robust = TRUE)
    )

stl_rem %>%
    components() %>%
    autoplot()

stl_wl <- org_ts %>%
    model(
        STL(Waiting.list.size ~ trend() + season(period = "1 year"), robust = TRUE)
    )

stl_wl %>%
    components() %>%
    autoplot()

refs_components <- stl_refs %>%
    components() %>%
    mutate(measure = "Referrals")

rem_components <- stl_rem %>%
    components() %>%
    mutate(measure = "Removals")

wl_components <- stl_wl %>%
    components() %>%
    mutate(measure = "Waiting.list.size")


calculate_growth <- function(components, measure_name) {

    components %>%
        as_tibble() %>%
        select(Org_Code, month, trend) %>%
        filter(
            !is.na(trend),
            is.finite(trend),
            trend > 0
        ) %>%
        group_by(Org_Code) %>%
        arrange(month, .by_group = TRUE) %>%
        mutate(t = row_number()) %>%
        group_modify(~ {

            dat <- .x

            if (nrow(dat) < 2 || length(unique(dat$trend)) < 2) {

                tibble(
                    n_months = nrow(dat),
                    monthly_growth = NA_real_,
                    annual_growth = NA_real_,
                    annual_growth_lower = NA_real_,
                    annual_growth_upper = NA_real_
                )

            } else {

                fit <- lm(log(trend) ~ t, data = dat)

                beta <- unname(coef(fit)["t"])

                beta_ci <- confint(fit, "t", level = 0.95)

                tibble(
                    n_months = nrow(dat),

                    monthly_growth =
                        exp(beta) - 1,

                    annual_growth =
                        exp(12 * beta) - 1,

                    annual_growth_lower =
                        exp(12 * beta_ci[1]) - 1,

                    annual_growth_upper =
                        exp(12 * beta_ci[2]) - 1
                )
            }
        }) %>%
        ungroup() %>%
        mutate(
            measure = measure_name,

            monthly_growth_pct =
                monthly_growth * 100,

            annual_growth_pct =
                annual_growth * 100,

            annual_growth_lower_pct =
                annual_growth_lower * 100,

            annual_growth_upper_pct =
                annual_growth_upper * 100
        )
}

growth_estimates <- bind_rows(
    calculate_growth(refs_components, "Referrals"),
    calculate_growth(rem_components, "Removals"),
    calculate_growth(wl_components, "Waiting.list.size")
)





growth_by_org <- growth_estimates %>%
    select(
        Org_Code,
        measure,
        n_months,
        monthly_growth_pct,
        annual_growth_pct
    ) %>%
    pivot_wider(
        names_from = measure,
        values_from = c(
            n_months,
            monthly_growth_pct,
            annual_growth_pct
        ),
        names_glue = "{measure}_{.value}"
    )


growth_by_org
