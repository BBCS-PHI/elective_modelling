

wl_batch_func <- function(model_group, group_name, out_path){

    require(tidyverse)
    require(fuzzyjoin)
    require(NHSRwaitinglist)
    source("utils.R")
    require(purrr)
    require(furrr)
    require(future.apply)
    require(parallel)
    require(BSOLwaitinglist)
    require(writexl)
    library(scales)
    library(glue)


######################## Targets & population growth ##############

last_data_date <- as.Date('01/06/2026', '%d/%m/%Y')

    target_dts <-
        data.frame(
            startdate = as.Date(c('01/04/2026', '01/04/2027'
                                  , '01/04/2028','01/04/2029','01/04/2030'), '%d/%m/%Y'),
            enddate = as.Date(c('31/03/2027', '31/03/2028'
                                , '31/03/2029', '31/03/2030', '31/03/2031'), '%d/%m/%Y'),
            value = c(0.86, 0.93, 0.99, 0.99, 0.99),
            descr = c("diag86%", "diag93%", "diag99%", "diag99%", "diag99%")
        )

# No Non-demo
# if(substr(group_name,1,2) == "bc"){
#     # #bc
#     population_growth <-
#         tibble::tribble(
#               ~start_date,    ~end_date, ~ratio_increase, ~population, ~adjustment_factor,
#              #"01/09/2025", "31/03/2026",               1,     1254329,     1.013762439,
#              "01/07/2026", "31/03/2027",     1.005597424,     1261350,     1.014112691,
#              "01/04/2027", "31/03/2028",     1.011238882,     1268426,     1.014032883,
#              "01/04/2028", "31/03/2029",     1.016872582,     1275493,     1.013953973,
#              "01/04/2029", "31/03/2030",      1.02287851,     1283026,     1.014080133,
#              "01/04/2030", "31/03/2031",     1.028903111,     1290583,     1.013893177
#           )
# } else {
#     # Bsol
#     population_growth <-
#         tibble::tribble(
#             ~start_date,    ~end_date, ~ratio_increase, ~population, ~adjustment_factor,
#             #"01/09/2025", "31/03/2026",               1,     1590793,    0.991027558,
#             #"01/03/2026", "31/03/2026",               1,     1590793,    0.991027558,
#             "01/07/2026", "31/03/2027",        1.003309, 1596056.425,    0.991161751,
#             "01/04/2027", "31/03/2028",        1.006733, 1601503.998,    0.991296965,
#             "01/04/2028", "31/03/2029",        1.010701, 1607815.611,    0.991226216,
#             "01/04/2029", "31/03/2030",        1.015318, 1615161.207,    0.991370238,
#             "01/04/2030", "31/03/2031",        1.020006, 1622704.257,    0.991201425
#         )
# }

# #+ 1% Non-demographic growth
if (substr(group_name,1,2) == "bc") {
    # #bc
    population_growth <-
        tibble::tribble(
            ~start_date,    ~end_date, ~ratio_increase, ~population, ~adjustment_factor,
            #"01/09/2025", "31/03/2026",               1,     1254329,     1.013762439,
            "01/07/2026", "31/03/2027",     1.015597424,     1261350,     1.014112691,
            "01/04/2027", "31/03/2028",     1.021238882,     1268426,     1.014032883,
            "01/04/2028", "31/03/2029",     1.026872582,     1275493,     1.013953973,
            "01/04/2029", "31/03/2030",      1.03287851,     1283026,     1.014080133,
            "01/04/2030", "31/03/2031",     1.038903111,     1290583,     1.013893177
        )
} else {
    # Bsol
    population_growth <-
        tibble::tribble(
            ~start_date,    ~end_date, ~ratio_increase, ~population, ~adjustment_factor,
            #"01/09/2025", "31/03/2026",               1,     1590793,    0.991027558,
            #"01/03/2026", "31/03/2026",               1,     1590793,    0.991027558,
            "01/07/2026", "31/03/2027",        1.013309, 1596056.425,    0.991161751,
            "01/04/2027", "31/03/2028",        1.016733, 1601503.998,    0.991296965,
            "01/04/2028", "31/03/2029",        1.020701, 1607815.611,    0.991226216,
            "01/04/2029", "31/03/2030",        1.025318, 1615161.207,    0.991370238,
            "01/04/2030", "31/03/2031",        1.030006, 1622704.257,    0.991201425
        )
}



# Convert Year to Date
population_growth$start_date <- as.Date(population_growth$start_date, '%d/%m/%Y')
population_growth$end_date <- as.Date(population_growth$end_date, '%d/%m/%Y')




##################Load data#############################



ICB <- TRUE



##################Prepare data###########################

############### Convert to list of data.frames per DiagnosticProcedure#####################
# Split data frame by DiagnosticProcedure, into a list.
# Each slot in the list is a data.frame for a DiagnosticProcedure
df_list <- model_group  |>  #test_input %>%
    mutate(ratio_increase = as.numeric(NA),
           #adjustment_factor = as.numeric(NA)
    ) |>
    group_by(DiagnosticProcedure) %>%
    group_split()

## TF 300 in BSOL
#df_list <- df_list[10]

# Check lists are equal length
#lapply(df_list, nrow)
#View(df_list[[1]])


#.x <- df_list[[1]]

#print(df_list[[1]][18,8], n = 30)

# Correct removals to balance waiting list
df_list <- map(df_list, function(.x) {
    sub <- .x[1,, drop = FALSE]

    .x$Removals  <- data.table::shift(.x$Waiting.list.size, 1, type = "lag") + .x$Referrals - .x$Waiting.list.size

    .x <- rbind(sub, .x[2:nrow(.x),, drop = FALSE])

    .x$Referrals <- as.integer(.x$Referrals / 4.33) # divide by 4.33 to turn monthly to weekly, needs to be done here after balance
    .x$Removals <- as.integer(.x$Removals / 4.33) # divide by 4.33 to turn monthly to weekly

    .x

})

# Now make same month-to-week adjustment and formatting outside look, for later plotting
model_group$Referrals <- as.integer(model_group$Referrals/ 4.33) # divide by 4.33 to turn monthly to weekly
model_group$Removals <- as.integer(model_group$Removals / 4.33) # divide by 4.33 to turn monthly to weekly


# # Chop September as it's wonky due to PAS implementation
# df_list <- map(df_list, function(.x) {
#     .x <- .x[.x$end_date < as.Date("01/09/2025", "%d/%m/%Y"),]
#     .x
# })


# Last row based on data for iteration elements. Another taken after extending later for full data + simualtion period.
last_data_row <- nrow(df_list[[1]])
#last_data_row <- map(df_list, nrow)
#View(df_list[[1]])

# Calculate coefficients of variation (how each list behaves)
# cv_demand and cv_capacity for each DiagnosticProcedure
tf_summary <- map_dfr(df_list, function(.x) {
    .x <- filter(.x, start_date < as.Date("2025-10-01"))
    cv_demand  <- sd(.x$Referrals, na.rm = TRUE) / mean(.x$Referrals, na.rm = TRUE)
    cv_capacity <- sd(.x$Removals, na.rm = TRUE) / mean(.x$Removals, na.rm = TRUE)

    tibble(
        DiagnosticProcedure = first(.x$DiagnosticProcedure),
        cv_demand = cv_demand,
        cv_capacity = cv_capacity
    )
})



# Append rows for population growth periods
df_append <- data.frame(#Commissioner_Code = NA,
                        DiagnosticProcedure = NA,
                        start_date = as.Date(population_growth$start_date, '%d/%m/%Y'),
                        end_date = as.Date(population_growth$end_date, '%d/%m/%Y'),
                        Referrals = NA, Removals = NA, Waiting.list.size = NA,
                        ratio_increase = population_growth$ratio_increase,
                        adjustment = df_list[[1]]$adjustment[1]
                        #adjustment_factor = population_growth$adjustment_factor
)


# Apply to data.frames in list.
df_list <- map(df_list, ~ rbind(.x, df_append))

#print(df_list[[1]], n = 50)

last_row <- map(df_list, nrow)

#### Populate values logically ####
######## Populate values ####

# fill in values, DiagnosticProcedure
df_list <- map(df_list, function(.x) {
    spec <- first(.x$DiagnosticProcedure)
    .x$DiagnosticProcedure[is.na(.x$DiagnosticProcedure)] <- spec
    .x
})
#
# df_list <- map(df_list, function(.x) {
#     com <- first(.x$Commissioner_Code)
#     .x$Commissioner_Code[is.na(.x$Commissioner_Code)] <- com
#     .x
# })

# median capacity over last 12 months. Longer seems a bit extreme
#j <- 1
df_list <- map(df_list, function(.x) {

    med <- as.integer(median(.x[(last_data_row - 11):last_data_row,]$Removals, na.rm = TRUE))

    .x$Removals[is.na(.x$Removals)] <- med
    #
    #j <<- j + 1

    .x
})

#
# # Fill in referrals based on last known value and growth ratios
df_list <- map(df_list, function(.x) {

    .x %>%
        mutate(
            Referrals = {
                last_val <- as.integer(median(.x[(last_data_row - 11):last_data_row,]$Referrals, na.rm = TRUE))
                new_vals <- .x$Referrals
                for (i in seq_along(new_vals)) {
                    if (is.na(new_vals[i])) {
                        if (i == 1 || !is.na(new_vals[i - 1])) {
                            if(ICB == TRUE) {
                                new_vals[i] <- last_val * ratio_increase[i]
                            } else {
                                new_vals[i] <- last_val * (((ratio_increase[i] - 1) * adjustment[i]) + 1)
                            }
                        } else {
                            if(ICB == TRUE) {
                                new_vals[i] <- new_vals[i - 1] * ratio_increase[i]
                            } else {
                                new_vals[i] <- new_vals[i - 1] * (((ratio_increase[i] - 1) * adjustment[i]) + 1)
                            }
                        }
                    }
                }
                as.integer(ceiling(new_vals))
            }
        )

})

#View(df_list[[3]])



#print(df_list[[1]], n = 50)
#print(df_list[[2]], n = 50)

#.x <- df_list[[1]]

# Add targets into data.frames in list
df_list <- map(df_list, function(.x) {

    .x <- fuzzy_left_join(.x, target_dts, by = c("start_date" = "startdate", "end_date" = "enddate")
                          , match_fun = list(`>=`, `<=`)) |>
        mutate(time_to_target =  floor(as.numeric(difftime(enddate, start_date, units = "weeks")))) |>
        select(-startdate,-enddate,-descr) |>
        rename(target = value)
    .x
})

#.x<-df_list[[1]]
# Initialize columns for later
#j <- 1

df_list <- map(df_list, function(.x) {

    .x$target_wl <- NA
    .x$target_capacity <- NA
    .x$relief_capacity_cur <- NA
    .x$relief_capacity_rel <- NA
    .x$Waiting.list.size_relief <- NA
    .x$wl_performance_cur <- NA
    .x$wl_performance_rel <- NA

    # Copy over right waiting list size as easier than using 2 columns in sim function.
    .x$Waiting.list.size_relief[last_data_row] <- .x$Waiting.list.size[last_data_row]

    #j <<- j + 1
    .x
})

#Fill in waiting list performance at last point
df_list <- map(df_list, function(.x) {
    .x$wl_performance_cur[last_data_row] <- est_wait_performance(.x$Referrals[last_data_row], .x$Waiting.list.size[last_data_row], 6)
    .x$wl_performance_rel[last_data_row]  <- est_wait_performance(.x$Referrals[last_data_row], .x$Waiting.list.size[last_data_row], 6)

    .x
})

#View(df_list[[1]])
#print(df_list[[16]], n = 92)
#print(df_list[[2]], n = 92)

#####

# Need to loop this now to build WL, then calculate targets, and sim next
#i <- 86L
#df <-
#df_list[[5]]
#View(df_list[[1]])

#i = 20

#.x <- df
plan(sequential)
gc()

# Create workers and pre-load Rcpp compilation on each
cl <- future::makeClusterPSOCK(workers = 6)

clusterEvalQ(cl, {
    library(BSOLwaitinglist)

})


#Optional sanity check: should be TRUE on each worker
parallel::clusterEvalQ(cl, exists("bsol_montecarlo_WL3", mode = "function"))


# Use the cluster in your plan
plan(cluster, workers = cl)

#print(df_list[[1]], n = 50)

#df_list <- df_list[1]
#.x <- df_list[[1]]
#i <- 28
for (i in (last_data_row + 1):nrow(df_list[[1]])) {

    # run NHSR waiting list functions over each data.frame in list (DiagnosticProcedure)

    #.x <- df_list[[1]]

    df_list <- map(df_list, function(.x) {


        .x$target_wl[i] <- floor(calc_target_queue_size(
            demand = .x$Referrals[i],
            target_wait = 6,
            factor = qexp(.x$target[i])  # Need to amend for target 0.65 or +0.07
        ))



        dscr <- first(.x$DiagnosticProcedure)

        cv_demand <- tf_summary %>% filter(DiagnosticProcedure == dscr) %>% pull(cv_demand)
        cv_capacity <- tf_summary %>% filter(DiagnosticProcedure == dscr) %>% pull(cv_capacity)

        # Assuming we meet target list size in each period
        #.x$Waiting.list.size <- coalesce(.x$Waiting.list.size, .x$target_wl)


        .x$target_capacity[i] <-
            ceiling(calc_target_capacity(
                demand = .x$Referrals[i],
                target_wait = 6,
                factor = qexp(.x$target[i]),
                cv_demand = 1,
                cv_capacity = 1))


        .x$relief_capacity_cur[i] <-
            ceiling(calc_relief_capacity(
                demand = .x$Referrals[i],
                queue_size = .x$Waiting.list.size[i - 1],
                target_queue_size = .x$target_wl[i],
                time_to_target = .x$time_to_target[i],
                cv_demand = 1
            ))


        .x$relief_capacity_rel[i] <-
            ceiling(calc_relief_capacity(
                demand = .x$Referrals[i],
                queue_size = .x$Waiting.list.size_relief[i - 1],
                target_queue_size = .x$target_wl[i],
                time_to_target = .x$time_to_target[i],
                cv_demand = 1

            ))


        # manual correction for 65% target year, if capacity already higher, dont' reduce.
        # .x$relief_capacity_rel[i] <- ifelse(
        #     .x$start_date[i] >= target_dts$startdate[1] &
        #     .x$end_date[i] <= target_dts$enddate[1] &
        #     .x$relief_capacity_rel[i] < .x$Removals[i],
        #     .x$Removals[i],
        #     .x$relief_capacity_rel[i]
        #
        #
        #     )


        .x
    })

    # Sim with current capacity projected forward
    sim_func_cur <- function(df) {
        current_wl <- data.frame(
            Referral = rep(as.Date(df$start_date[i] - 1)
                           , df$Waiting.list.size[i - 1]),
            Removal = rep(as.Date(NA), df$Waiting.list.size[i - 1])
        )

        sim <- wl_simulator_cpp(
            start_date = as.Date(df[i,]$start_date),
            end_date = as.Date(df[i,]$end_date),
            demand = df[i,]$Referrals,
            capacity = df[i,]$Removals, # project last point forward
            waiting_list = current_wl
        )

        data.frame(DiagnosticProcedure = df$DiagnosticProcedure[1], queue = tail(wl_queue_size(sim)[, 2],1),
                   mean_wait = wl_stats(sim)$mean_wait)
    }

    # Apply to each DiagnosticProcedure in df_list
    results_cur <- map(df_list, function(df) {
        # Run 50 simulations for this DiagnosticProcedure
        sims <- future_replicate(50, sim_func_cur(df), simplify = FALSE)
        # Combine into one data frame
        bind_rows(sims)
    })

    #plan(sequential)

    #saveRDs(results, "./data/results_didsinput2025.rds")

    # Combine all specialties into one data frame
    all_results_cur <- bind_rows(results_cur)

    # Summarize mean and median queue per DiagnosticProcedure
    summary_results_cur <- all_results_cur %>%
        group_by(DiagnosticProcedure) %>%
        summarise(
            mean_queue = mean(queue, na.rm = TRUE),
            median_queue = median(queue, na.rm = TRUE),
            .groups = "drop"
        )

    #summary_results

    #print(df_list[[1]], n = 92) # 15458 110
    #print(df_list[[2]], n = 92) # 163286 99999


    # Update df_list last row plus 1 to get to end of 2025/26row 19 with median_queue
    df_list <- map(df_list, function(df) {
        dscr <- df$DiagnosticProcedure[1]
        mean_val <- round(summary_results_cur$mean_queue[summary_results_cur$DiagnosticProcedure == dscr])
        df$Waiting.list.size[i] <- mean_val

        df$wl_performance_cur[i] <- est_wait_performance(df$Referrals[i], df$Waiting.list.size[i], 6)

        df
    })


    # Now using relief capacity instead
    sim_func_rel <- function(df) {
        current_wl <- data.frame(
            Referral = rep(as.Date(df$start_date[i] - 1)
                           , df$Waiting.list.size_relief[i - 1]),
            Removal = rep(as.Date(NA), df$Waiting.list.size_relief[i - 1])
        )

        sim_rel <- wl_simulator_cpp(
            start_date = as.Date(df[i,]$start_date),
            end_date = as.Date(df[i,]$end_date),
            demand = df[i,]$Referrals,
            capacity = coalesce(df[i,]$relief_capacity_rel, df[i,]$Removals), # Coalesce added here to counter against NA's is targets dont' start at beginning of projection period.
            waiting_list = current_wl
        )

        data.frame(DiagnosticProcedure = df$DiagnosticProcedure[1], queue = tail(wl_queue_size(sim_rel)[, 2],1),
                   mean_wait = wl_stats(sim_rel)$mean_wait)
    }

    # Apply to each DiagnosticProcedure in df_list
    results_rel <- map(df_list, function(df) {
        # Run 50 simulations for this DiagnosticProcedure
        sims_rel <- future_replicate(50, sim_func_rel(df), simplify = FALSE)
        # Combine into one data frame
        bind_rows(sims_rel)
    })

    #plan(sequential)

    #saveRDs(results, "./data/results_didsinput2025.rds")

    # Combine all specialties into one data frame
    all_results_rel <- bind_rows(results_rel)

    # Summarize mean and median queue per DiagnosticProcedure
    summary_results_rel <- all_results_rel %>%
        group_by(DiagnosticProcedure) %>%
        summarise(
            mean_queue = mean(queue, na.rm = TRUE),
            median_queue = median(queue, na.rm = TRUE),
            .groups = "drop"
        )

    #summary_results

    #print(df_list[[1]], n = 92) # 15458 110
    #print(df_list[[2]], n = 92) # 163286 99999

    #j <<- j + 1

    # Update df_list last row plus 1 to get to end of 2025/26row 19 with median_queue
    df_list <- map(df_list, function(df) {
        dscr <- df$DiagnosticProcedure[1]
        mean_val <- round(summary_results_rel$mean_queue[summary_results_rel$DiagnosticProcedure == dscr])
        df$Waiting.list.size_relief[i] <- mean_val

        df$wl_performance_rel[i] <- est_wait_performance(df$Referrals[i], df$Waiting.list.size_relief[i], 6)

        df
    })


}

parallel::stopCluster(cl)

gc()

plan(sequential)

#502, x02,

#print(df_list[[16]], n = 90)
#View(df_list[[2]])

#.x <- df_list[[1]]
#View(df_list[[1]])
######### Now add new capacity column #########################

df_list <- map(df_list, function(.x) {

        .x$calc_capacity <- ceiling(ifelse(.x$start_date <= .x[last_data_row,]$start_date, #target_dts[1,]$startdate,
                                           .x$Removals, # Actual capacity
                                           ifelse(.x$start_date > target_dts[3,]$enddate,
                                                  .x$target_capacity, # peg at target capacity for
                                                  .x$relief_capacity_rel)) # Calcualted relief capacity
        )


    .x
})


#print(df_list[[5]], n = 90)
#View(df_list[[13]])

#a <- df_list[[1]]

################################################################

#library(furrr)
#library(future.mirai)




# Create workers and pre-load Rcpp compilation on each
cl <- future::makeClusterPSOCK(workers = 6)
clusterEvalQ(cl, {
    library(BSOLwaitinglist)

})


# Does the Rcpp wrapper exist on workers?
parallel::clusterEvalQ(cl, exists("bsol_montecarlo_WL3", mode = "function"))


# Use the cluster in your plan
plan(cluster, workers = cl)

#plan(sequential)


#plansequential()#plan(mirai_multisession, workers = 6)
start_time <- Sys.time()
#df_list2 <- df_list[1]
#df <- df_list[[1]]
sim_results_rel <- map(df_list, function(df) {


    #Rcpp::sourceCpp("wl_simulator.cpp")
    # Extract starting_wl from first row
    start_wl <- df[last_data_row, "Waiting.list.size_relief", drop = TRUE]
    if (is.na(start_wl)) start_wl <- 0

    #df <- df[df$start_date >= target_dts$startdate[1],]
    df <- df[df$start_date >= last_data_date,]

    # Inner parallel map (optional)
    future_map(1:50, function(i) {
        #Rcpp::sourceCpp("wl_simulator.cpp")
        bsol_montecarlo_WL3(
            .data = df,
            run_id = i,
            start_date_name = "start_date",
            end_date_name = "end_date",
            adds_name = "Referrals",
            removes_name = "calc_capacity",
            starting_wl = start_wl
        )
    }, .options = furrr_options(seed = NULL, globals = TRUE
                                , packages = c("Rcpp", "NHSRwaitinglist", "BSOLwaitinglist")
    )
    )
})


end_time <- Sys.time()

#saveRDS(sim_results, "./data/bsol_sims.rds")

end_time - start_time
#inner_sequential <- end_time - start_time

# end parallel sessions
#plan(sequential)


#tail(sim_results[[1]][[2]])
# Bind each run together within first level of list, per speciality (each list slot is DiagnosticProcedure)
mc_bind_rel <- map(sim_results_rel,  function(.x) {do.call("rbind", .x)})

# Aggregate each function within list slot (each list slot is DiagnosticProcedure)
mc_agg_rel <- map(mc_bind_rel,  function(.x) {
    aggregate(
        queue_size ~ dates
        , data = .x
        , FUN = \(x) {
            c(mean_q = mean(x),
              median_q = median(x),
              lower_95CI = mean(x) -  (1.96 * (sd(x) / sqrt(length(x)))),
              upper_95CI = mean(x) +  (1.96 * (sd(x) / sqrt(length(x)))),
              q_25 = quantile(x, .025, names = FALSE),
              q_75 = quantile(x, .975, names = FALSE))
        }
    )
})

#mc_agg_rel[[1]]

# Rename funky column names form nest list in aggregate step
mc_agg_rel <- map(mc_agg_rel, ~ data.frame(
    dates = as.Date(.x$dates),
    unlist(.x$queue_size)
)
)


#plan(mirai_multisession, workers = 6)
start_time <- Sys.time()
#df_list2 <- df_list[1]
#df <- df_list2[[1]]
sim_results_cur <- map(df_list, function(df) {


    #Rcpp::sourceCpp("wl_simulator.cpp")
    # Extract starting_wl from first row
    start_wl <- df[last_data_row, "Waiting.list.size", drop = TRUE]
    if (is.na(start_wl)) start_wl <- 0

    #df <- df[df$start_date >= target_dts$startdate[1],]
    df <- df[df$start_date >= last_data_date,]

    # Inner parallel map (optional)
    future_map(1:50, function(i) {
        #Rcpp::sourceCpp("wl_simulator.cpp")
        bsol_montecarlo_WL3(
            .data = df,
            run_id = i,
            start_date_name = "start_date",
            end_date_name = "end_date",
            adds_name = "Referrals",
            removes_name = "Removals",
            starting_wl = start_wl
        )
    }, .options = furrr_options(seed = NULL, globals = TRUE))
}
)


# ggplot(b, aes(y=queue_size, x=dates, col = run_id))+
#     geom_line()

end_time <- Sys.time()

#saveRDS(sim_results, "./data/bsol_sims.rds")

end_time - start_time
#inner_sequential <- end_time - start_time

# end parallel sessions
plan(sequential)
stopCluster(cl)


# Bind each run together within first level of list, per speciality (each list slot is DiagnosticProcedure)
mc_bind_cur <-  map(sim_results_cur,  function(.x) {do.call("rbind", .x)})

# Aggregate each function within list slot (each list slot is DiagnosticProcedure)
mc_agg_cur <- map(mc_bind_cur,  function(.x) {
    aggregate(
        queue_size ~ dates
        , data = .x
        , FUN = \(x) {
            c(mean_q = mean(x),
              median_q = median(x),
              lower_95CI = mean(x) -  (1.96 * (sd(x) / sqrt(length(x)))),
              upper_95CI = mean(x) +  (1.96 * (sd(x) / sqrt(length(x)))),
              q_25 = quantile(x, .025, names = FALSE),
              q_75 = quantile(x, .975, names = FALSE))
        }
    )
})

#mc_agg_cur[[1]]

# Rename funk column names form nest list in aggregate step
mc_agg_cur <- map(mc_agg_cur, ~ data.frame(
    dates = as.Date(.x$dates),
    unlist(.x$queue_size)
)
)

gc()


# install.packages("writexl")

# Ensure list elements are named (used as sheet names)
names(df_list) <- model_group |> distinct(DiagnosticProcedure) |> pull()

#create directory
dir.create(paste0("./", out_path))

write_xlsx(df_list, path = paste0("./", out_path,"/", group_name, ".xlsx"))


out_long <- bind_rows(df_list, .id = "source")

write_csv(out_long, paste0("./", out_path,"/", group_name, "_long.csv"))



# ggplot elements.


# --- Sanity checks: all lists are the same length ---
n <- length(df_list)
stopifnot(
    length(mc_bind_cur) == n,
    length(mc_agg_cur)  == n,
    length(mc_bind_rel) == n,
    length(mc_agg_rel)  == n
)

# --- Output folder ---
out_dir <- out_path
dir.create(out_dir, showWarnings = FALSE)

# Optional: annotation text per plot (or just keep single string)
ann_labels <- rep("Target waiting list for 99%\nat 6 weeks by end of 2028/29", n)

# --- Helpers for rounded y-scale ---
round_up <- function(x, to) ceiling(x / to) * to

choose_step <- function(ymax) {
    # Choose a “nice” step in 100s or 1000s based on magnitude
    if (is.na(ymax) || ymax <= 0) return(100)
    if (ymax <= 1000)      return(100)
    else if (ymax <= 2000) return(250)
    else if (ymax <= 6000) return(500)
    else if (ymax <= 12000) return(1000)
    else if (ymax <= 30000) return(2000)
    else if (ymax <= 100000) return(5000)
    else return(10000)
}

# --- Plotting function ---

make_plot <- function(
        i,
        target_row = NULL,
        target_date = NULL,
        target_date_fmt = "%d/%m/%Y",        # set to NULL if target_date is already Date
        ann_label = ann_labels[i]
) {
    df        <- df_list[[i]]
    cur_bind  <- mc_bind_cur[[i]]
    cur_agg   <- mc_agg_cur[[i]]
    rel_bind  <- mc_bind_rel[[i]]
    rel_agg   <- mc_agg_rel[[i]]

    # --- Coerce x variables to Date (robust to character/POSIXct) ---
    coerce_date <- function(x, fmt = NULL) {
        if (inherits(x, "Date")) return(x)
        if (inherits(x, "POSIXt")) return(as.Date(x))
        if (is.character(x)) {
            if (!is.null(fmt)) return(as.Date(x, fmt))
            # Fallback: try ISO then UK
            out <- suppressWarnings(as.Date(x))                    # ISO
            if (any(is.na(out))) out <- suppressWarnings(as.Date(x, "%d/%m/%Y")) # UK
            return(out)
        }
        # As last resort
        suppressWarnings(as.Date(x))
    }

    df$start_date <- coerce_date(df$start_date)
    df$end_date   <- coerce_date(df$end_date)

    cur_bind$dates <- coerce_date(cur_bind$dates)
    cur_agg$dates  <- coerce_date(cur_agg$dates)
    rel_bind$dates <- coerce_date(rel_bind$dates)
    rel_agg$dates  <- coerce_date(rel_agg$dates)

    # Ensure aggregated frames are sorted (ribbon likes ordered x)
    cur_agg <- dplyr::arrange(cur_agg, dates)
    rel_agg <- dplyr::arrange(rel_agg, dates)

    # --- Target row/date selection ---
    idx <- NA_integer_
    if (!is.null(target_row)) {
        idx <- target_row
    } else if (!is.null(target_date)) {
        td <- if (inherits(target_date, "Date")) target_date
        else coerce_date(target_date, fmt = target_date_fmt)
        idx <- match(as.Date(td), as.Date(df$start_date))
    }
    if (is.na(idx) || idx < 1 || idx > nrow(df)) {
        warning(glue::glue("Plot {i}: target row/date not found; defaulting to row 30."))
        idx <- min(30L, nrow(df))
    }

    hline <- suppressWarnings(as.numeric(df$target_wl[idx]))
    cutoff_date <- as.Date('2026-07-01', "%Y-%m-%d")
    #if (!is.null(target_date)) coerce_date(target_date, fmt = target_date_fmt)
    #else df$start_date[idx]
    ann_x <- if (!is.null(target_date)) coerce_date(target_date, fmt = target_date_fmt)
    else as.Date("2025-01-01")

    # --- Dynamic y-axis ---
    round_up <- function(x, to) ceiling(x / to) * to
    choose_step <- function(ymax) {
        if (is.na(ymax) || ymax <= 0) return(100)
        if (ymax <= 100)       20 else
            if (ymax <= 500)       50 else
                if (ymax <= 1500)       100 else
                    if (ymax <= 6000)       500 else
                        if (ymax <= 12000)       1000 else
                            if (ymax <= 30000)     2000 else
                                if (ymax <= 100000)    5000 else 10000
    }

    ymax_raw <- suppressWarnings(max(
        df$Waiting.list.size,
        as.numeric(cur_bind$queue_size),
        as.numeric(rel_bind$queue_size),
        as.numeric(cur_agg$upper_95CI),
        as.numeric(rel_agg$upper_95CI),
        hline,
        na.rm = TRUE
    ))
    step  <- choose_step(ymax_raw)
    y_top <- round_up(ymax_raw, step)
    ann_y <- pmin(hline * 0.6, y_top * 0.9)

    # Title with DiagnosticProcedure
    spec <- dplyr::coalesce(
        df$DiagnosticProcedure[1] %||% NA
        #df$DiagnosticProcedure[1] %||% NA
    )

    if(substr(group_name,1,2) == "bc"){
        title_base <- "Simulated BC waiting list"
    } else {
        title_base <- "Simulated BSOL waiting list"
    }
    title_txt  <- if (!is.na(spec)) paste0(title_base, " for ", spec) else title_base

    ggplot() +
        geom_line(
            aes(x = end_date, y = Waiting.list.size),
            col = "black",
            data = dplyr::filter(df, start_date < cutoff_date)
        ) +
        geom_line(
            aes(x = dates, y = queue_size, group = run_id),
            alpha = 0.4, col = "#A6CEE3", data = as.data.frame(cur_bind)
        ) +
        geom_ribbon(
            aes(x = dates, y = mean_q, ymin = lower_95CI, ymax = upper_95CI),
            alpha = 0.5, data = cur_agg, fill = "#1F78B4"
        ) +
        geom_line(aes(x = dates, y = mean_q), data = cur_agg, col = "#1F78B4") +
        geom_line(
            aes(x = dates, y = queue_size, group = run_id),
            alpha = 0.3, col = "#B2DF8A", data = as.data.frame(rel_bind)
        ) +
        geom_ribbon(
            aes(x = dates, y = mean_q, ymin = lower_95CI, ymax = upper_95CI),
            alpha = 0.5, data = rel_agg, fill = "#33A02C"
        ) +
        geom_line(aes(x = dates, y = mean_q), data = rel_agg, col = "#33A02C") +
        geom_hline(yintercept = hline, col = "#FF7F00") +
        annotate("text", x = ann_x, y = ann_y, label = ann_label,
                 col = "#FF7F00", hjust = 0.1, vjust = 0.1) +
        scale_y_continuous(
            labels = scales::comma,
            breaks = seq(0, y_top, by = step),
            limits = c(0, y_top),
            expand = c(0, 0)
        ) +
        # Keep your overall limits; all x's are now Date
        scale_x_date(
            date_breaks = "6 months", date_labels = "%b-%y",
            date_minor_breaks = "3 months",
            limits = as.Date(c("2025-04-01", "2031-04-01")),
            expand = c(0, 0)
        ) +
        guides(x = guide_axis(check.overlap = TRUE, n.dodge = 2)) +
        labs(
            y = "Queue Size", x = "Date",
            title = title_txt,
            subtitle = "Average WL over 50 runs, with 95% point-wise confidence interval"
        ) +
        theme(
            axis.text.x = element_text(angle = 0),
            axis.line = element_line(color = "grey"),
            axis.ticks = element_line(color = "grey"),
            plot.margin = unit(c(2, 5, 2, 2), "mm")
        )
}

# --- Choose how to drive the target per plot ---

## Option A: provide an index per plot
target_rows <- rep(30, n)  # one row index per dataset

## Option B: provide a date per plot (e.g., from existing target_dts$startdate)
# target_dates <- target_dts$startdate[seq_len(n)]

# Build plots:
# A) If using row indices:
p_list <- map2(seq_len(n), target_rows, ~ make_plot(.x, target_row = .y))

# B) If using dates:
# p_list <- map2(seq_len(n), target_dates, ~ make_plot(.x, target_date = .y))

# C) If neither supplied, defaults to row 22:
p_list <- map(seq_len(n), make_plot)

# --- File naming helper ---
build_name <- function(i) {
    nm <- NULL
    if ("DiagnosticProcedure" %in% names(df_list[[i]])) nm <- df_list[[i]]$DiagnosticProcedure[1]
    else if ("DiagnosticProcedure" %in% names(df_list[[i]])) nm <- df_list[[i]]$DiagnosticProcedure[1]
    else if ("Org_code" %in% names(df_list[[i]])) nm <- df_list[[i]]$Org_Code[1]

    org <- group_name

    safe <- if (!is.null(nm)) gsub("[^A-Za-z0-9_\\-]+", "_", nm) else sprintf("plot_%02d", i)
    file.path(out_dir, paste0(org, "_", safe, ".png"))
}

# --- Save plots ---
walk2(p_list, seq_along(p_list), ~ ggsave(
    filename = build_name(.y), plot = .x,
    width = 9, height = 6, dpi = 300, bg = "white"
))


}
