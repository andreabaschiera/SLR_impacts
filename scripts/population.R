packages <- c(
  "tidyverse", "GDPuc", "countrycode", "readxl", "writexl",
  "rnaturalearth", "rnaturalearthdata", "sf", "shiny", "scales")

# installed <- packages %in% installed.packages()
# if (any(!installed)) install.packages(packages[!installed])

invisible(lapply(packages, library, character.only = TRUE)) # load packages

# Functions
source("scripts/functions/conversionGDPuc.R")
source("scripts/functions/interpolation.R")
source("scripts/functions/recalculations.R")
source("scripts/functions/saving_disaggr.R")
source("scripts/functions/crossjoin_tobind.R")
source("scripts/functions/plots.R")

options(scipen = 999)



# DATA IMPORT -------------------------------------------------------------

diva_or <- read_csv("inputs/ACCREU_COUNTRY.csv")

#read mapping ICES MH
agg_ICES_MH <- read_xlsx("inputs/ICES_MH_agg.xlsx", sheet = "ICES_MH_reg")
agg_ICES_MH <- agg_ICES_MH |> mutate(GTAP11_code = tolower(GTAP11_code))

#read mapping ICES REG
agg_ICES_REG <- read_xlsx("inputs/ICES_REG_agg.xlsx", sheet = "mapping_ICES_reg")
agg_ICES_REG <- agg_ICES_REG |> mutate(ICES_ACCREU_REG = tolower(ICES_ACCREU_REG))




# INTERPOLATION -----------------------------------------------------------

diva_pop <- diva_or |> filter(migration == "FALSE") |> 
  select(locationid:adaptation, time, expected_people_flooded, population_below_h100, totalpop) |>
  arrange(rcp,quantile,adaptation,time,locationid)

allyears <- seq(2015, 2100, by = 1)
time <- unique(diva_pop$time)
groupby_vars <- c("locationid", "rcp", "quantile", "adaptation")
interp_vars <- c("expected_people_flooded", "totalpop")

# linear interpolation 
pop_interpolated <- lin_interp_ongroups(diva_pop, time, allyears, groupby_vars, interp_vars)
pop_interpolated <- pop_interpolated |> rename(ppl_flooded_yearly = expected_people_flooded_yearly)



# RECALCULATIONS ISO ------------------------------------------------------

# annual pop growth rate and growth factor
pop_shock_growth_iso <- growthRate_growthFactor(pop_interpolated,
                                              time, 2015,
                                              groupby_vars,
                                              ppl_flooded_yearly,
                                              totalpop_yearly)

# 5 years cumulative shocks
pop_shock_5y_iso <- cum_shocks_5y(pop_shock_growth_iso, time, 2018, groupby_vars, growth_factor)

pop_shock_5y_iso_wide <- pop_shock_5y_iso |> 
  select(-cumulated_growth) |> 
  pivot_wider(names_from = time, values_from = cumulated_shock)

# cumulated shock over 50 years (2020-2070)
pop_shock_cum_iso <- cum_shock_manyyears(pop_shock_growth_iso, 2020, 2070, time, groupby_vars, growth_factor)


saveRDS(pop_shock_5y_iso_wide, "outputs/population/pop_shock_SLR_5y_ISO.rds")
write_xlsx(pop_shock_5y_iso_wide, "outputs/population/pop_shock_SLR_5y_ISO.xlsx")

saveRDS(pop_shock_cum_iso, "outputs/population/pop_shock_SLR_cum_ISO.rds")
write_xlsx(pop_shock_cum_iso, "outputs/population/pop_shock_SLR_cum_ISO.xlsx")



# RECALCULATIONS ACCREU ICES MH -------------------------------------------

# merging with mapping
pop_interpolated_ICES_MH <- agg_ICES_MH |> left_join(pop_interpolated, join_by(ISO == locationid))

pop_ICES_MH <- pop_interpolated_ICES_MH |>
  group_by(across(starts_with("ICES_ACCREU")), rcp, quantile, adaptation, time) |>
  summarise(across(c(ppl_flooded_yearly, totalpop_yearly), ~ sum(.x, na.rm = TRUE)),
            .groups = "drop")

# annual pop growth
groupby_varsMH <- c(groupby_vars[2:4], "ICES_ACCREU", "ICES_ACCREU_long", "ICES_ACCREU_n")
pop_shock_growth_ICES_MH <- growthRate_growthFactor(pop_ICES_MH,
                                                  time, 2015,
                                                  groupby_varsMH,
                                                  ppl_flooded_yearly,
                                                  totalpop_yearly)
# 5 years cumulative shocks 
pop_shock_5y_ICES_MH <- cum_shocks_5y(pop_shock_growth_ICES_MH, time, 2018, groupby_varsMH, growth_factor)

pop_shock_5y_ICES_MH_wide <- pop_shock_5y_ICES_MH |> 
  select(-cumulated_growth) |> 
  pivot_wider(names_from = time, values_from = cumulated_shock)

# cumulated 50-years shock
pop_shock_cum_ICES_MH <- cum_shock_manyyears(pop_shock_growth_ICES_MH, 2020, 2070, time, groupby_varsMH, growth_factor)


# Saving outputs (also, CSVs disaggregated by scenario and adaptation option)

saveRDS(pop_shock_5y_ICES_MH_wide, "outputs/population/pop_shock_5y_ICES_MH.rds")
write_xlsx(pop_shock_5y_ICES_MH_wide, "outputs/population/pop_shock_5y_ICES_MH.xlsx")

rcp_uniques <- unique(pop_shock_5y_ICES_MH_wide$rcp)
adapt_uniques <- unique(pop_shock_5y_ICES_MH_wide$adaptation)                          
saving_disagg(pop_shock_5y_ICES_MH_wide, "pop_shock_5y_ICES_MH_",
              rcp_uniques, adapt_uniques, quantile = 0.5,
              additional_cols_todrop = c("ICES_ACCREU_long", "ICES_ACCREU_n"),
              path = "outputs/population")

write_xlsx(pop_shock_cum_ICES_MH, "outputs/population/pop_shock_cum_ICES_MH.xlsx")




# RECALCULATIONS ACCREU ICES REG ------------------------------------------

agg_ICES_REG_nonNUTS <- agg_ICES_REG |> filter(Filter_NUTS == 1)

# merging with mapping and building whole ds
interp_ICES_REG <-  agg_ICES_REG_nonNUTS |> left_join(pop_interpolated, join_by(ISO == locationid))
landlock_countries <- anti_join(agg_ICES_REG_nonNUTS, pop_interpolated, join_by(ISO == locationid))

landlock_complete <- crossjoin_tobind(landlock_countries, interp_ICES_REG, 
                                      cols_structure = c("rcp", "quantile", "adaptation", "time"),
                                      cols_values = c("ppl_flooded_yearly", "totalpop_yearly"))
interp_ICES_REG_all <- bind_rows(interp_ICES_REG, landlock_complete)

POP_ICES_REG <- interp_ICES_REG_all |>
  group_by(across(starts_with("ICES_ACCREU")), rcp, quantile, adaptation, time) |>
  summarise(across(c(ppl_flooded_yearly, totalpop_yearly), ~ sum(.x, na.rm = TRUE)), .groups = "drop")

# annual pop growth
groupby_varsREG <- c(groupby_vars[2:4],
                     "ICES_ACCREU_REG","ICES_ACCREU_REG_long","ICES_ACCREU_REG_n","ICES_ACCREU_AGG")
pop_shock_growth_ICES_REG <- growthRate_growthFactor(POP_ICES_REG,
                                                   time, 2015,
                                                   groupby_varsREG,
                                                   ppl_flooded_yearly,
                                                   totalpop_yearly)
# 5 years cumulative shocks (for GTAP)
pop_shock_5y_ICES_REG <- cum_shocks_5y(pop_shock_growth_ICES_REG, time, 2018, groupby_varsREG, growth_factor)

pop_shock_5y_ICES_REG_wide <- pop_shock_5y_ICES_REG |> 
  select(-cumulated_growth) |> 
  pivot_wider(names_from = time, values_from = cumulated_shock)

# cumulated 50-years shock
pop_shock_cum_ICES_REG <- cum_shock_manyyears(pop_shock_growth_ICES_REG, 2020, 2070, time, groupby_varsREG, growth_factor)


# Saving outputs 

saveRDS(pop_shock_5y_ICES_REG_wide, "outputs/population/pop_shock_5y_ICES_REG.rds")
write_xlsx(pop_shock_5y_ICES_REG_wide, "outputs/population/pop_shock_5y_ICES_REG.xlsx")

write_xlsx(pop_shock_cum_ICES_REG, "outputs/population/pop_shock_cum_ICES_REG.xlsx")






# MAPS AND DASHBOARD ---------------------------------------------------------

# geometry data
world <- tibble(ne_countries(scale = "medium", returnclass = "sf")) |> rename(locationid = iso_a3) |> 
  mutate(locationid = if_else(iso_a3_eh %in% c("FRA", "NOR"), iso_a3_eh, locationid)) |> 
  select(locationid, geometry) |> filter(locationid != -99)

# "epp": 5y cumulated shock (expected people flooded)
epp <- left_join(pop_shock_5y_iso, world, join_by(locationid))
# "epp_cum": cumulated shock 2018-2070
epp_cum <- left_join(pop_shock_cum_iso, world, join_by(locationid))

epp_missing <- anti_join(world, pop_shock_5y_iso, join_by(locationid))

structure <- colnames(epp |> select(rcp:time))
structure2 <- colnames(epp_cum |> select(rcp:adaptation))
cols_values <- tail(names(pop_shock_5y_iso), 2)

epp_world <- bind_rows(epp, crossjoin_tobind(epp_missing, epp, structure, cols_values, NA)) |> 
  mutate(cumulated_shock = if_else(locationid=="NLD", NA, cumulated_shock))

epp_cum_world <- bind_rows(epp_cum, crossjoin_tobind(epp_missing, epp_cum, structure2, cols_values, NA)) |> 
  mutate(cumulated_shock = if_else(locationid=="NLD", NA, cumulated_shock))

#data for the Netherlands are wrong (for now), so I rescale them for visualisation purposes



# maps of the cumulated shocks (2018-2070)
#### customizable parameters and choice to save output ###
plot_world_cum("population", epp_cum_world, scen = 370, quant = 0.95, adapt = "Constant dike height")



#Selection chart
ui <- fluidPage(
  titlePanel("World map of sea level rise impacts"),
  tags$h4("5-years cumulated % of expected people flooded"),
  
  fluidRow(
    column(
      width = 3,  # Sidebar column
      wellPanel(
        selectInput("rcp", "RCP:", choices = unique(epp_world$rcp)),
        selectInput("quantile", "Quantile:", choices = unique(epp_world$quantile)),
        selectInput("adaptation", "Adaptation:", choices = unique(epp_world$adaptation)),
        selectInput("time", "time:", choices = seq(2020,2100, 5))
      )),
    column(
      width = 9,  # Main plot column
      plotOutput("plot", height = "700px")  # You can adjust height if needed
    )))

# Server
server <- function(input, output) {
  filtered_data <- reactive({
    epp_world |> filter(
      rcp == input$rcp,
      quantile == input$quantile,
      adaptation == input$adaptation,
      time == input$time)
  })
  
  output$plot <- renderPlot({
    ggplot(st_as_sf(filtered_data())) +
      geom_sf(aes(fill = cumulated_shock)) +
      scale_fill_gradientn(
        colours = c("lightyellow", "khaki1", "gold", "orange", "orangered2", "red3","purple4"),
        values = rescale(-c(0, 2, 4, 8, 16, 32, 99)),
        na.value = "lightgrey"
      ) +
      theme_minimal() +
      labs(title = paste("RCP:", input$rcp,
                         "| Quantile:", input$quantile,
                         "| Adaptation:", input$adaptation,
                         "| time:", input$time),
           fill="1 - %cumgrowth",
           caption = "Data relaborated from the DIVA model")
  })
}

# run shinyApp to go to the dashboard
shinyApp(ui = ui, server = server)
