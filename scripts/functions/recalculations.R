#library(tidyverse)

growthRate_growthFactor <- function(ds, time_var, firstyear, groupby_vars, shock_yearly, tot_yearly){
  
  ds |> group_by(pick(all_of(groupby_vars))) |> 
    mutate(yearly_shock = ifelse({{time_var}} == firstyear, 0, -{{shock_yearly}} / lag({{tot_yearly}}) *100)) |>  
    mutate(perc_damages = - yearly_shock) |>  
    mutate(growth_factor = 1 + (yearly_shock/100)) |> 
    ungroup()

}

cum_shocks_5y <- function(ds, time_var, firstyear, groupby_vars, growthfactor_var){
  
  first_block_5y <- firstyear
  if (first_block_5y %% 5 != 0) {
    while(first_block_5y %% 5 != 0) {
      first_block_5y = first_block_5y + 1
    }
  }

  ds |> 
    mutate(
      time_block = case_when(
        {{time_var}} >= firstyear & {{time_var}} <= first_block_5y ~ first_block_5y,                  
        {{time_var}} >= (first_block_5y+1) ~ (first_block_5y+5) + 5 * (({{time_var}} - (first_block_5y+1)) %/% 5)
      )
    ) |> 
    filter(!is.na(time_block)) |>   
    group_by(pick(all_of(groupby_vars)), time_block) |> 
    summarise(
      time = unique(time_block),
      cumulated_growth = prod({{growthfactor_var}}, na.rm = TRUE),
      .groups = "drop"
    ) |> 
    mutate(cumulated_shock = (cumulated_growth - 1) * 100) |> 
    arrange(across({{groupby_vars}}), time) |> select(-time_block)

}

cum_shock_manyyears <- function(ds, firstyear, lastyear, time_var, groupby_vars, growthfactor_var){
  
  ds |> filter({{time_var}} >= firstyear & {{time_var}} <= lastyear) |> 
    group_by(pick(all_of(groupby_vars))) |> 
    summarise(
      cumulated_growth = prod({{growthfactor_var}}, na.rm = TRUE),
      .groups = "drop"
    ) |> 
    mutate(cumulated_shock = (cumulated_growth - 1) * 100) |>
    arrange(across({{groupby_vars}}))

}
