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

#data for GDP and POP to calculate gdpc (per capita)
GDP_ssp <- tibble(readRDS("inputs/GDP_MER_SSP3.1_ISO.rds"))
POP_ssp <- tibble(readRDS("inputs/POP_all_lipo_SSP3.1_ISO.rds"))




# CALCULATING GDP PER CAPITA FROM SSPs ------------------------------------

gdp <- GDP_ssp |> filter(Model == "OECD ENV-Growth 2023" & Scenario == "SSP2") |> 
  select(- starts_with("GTAP"), - Model, - Scenario, - Variable, - Unit, - num_range("", 1980:2014)) |> 
  pivot_longer(cols = starts_with("2"), names_to = "time", values_to = "gdp")

pop <- POP_ssp |> filter(Scenario == "SSP2") |> 
  select(- starts_with("GTAP"), -starts_with("ADB"), - Model, - Scenario, - Variable, - Unit, - num_range("", 1980:2014)) |> 
  pivot_longer(cols = starts_with("2"), names_to = "time", values_to = "pop")

# length(unique(gdp$ISO))
# length(unique(pop$ISO))

gdpc <- left_join(gdp, pop, join_by(ISO, Countries, time)) |> 
  mutate(computed_gdpc = gdp/(pop*1000)) # in DIVA assets and damages are in Mln$, that's why I divide by 1000
gdpc[["time"]] <- as.double(gdpc[["time"]])

saveRDS(gdpc, "inputs/GDPpercapita_fromSSPs.rds")


# CONVERSIONS (GDPuc) ------------------------------------------------

diva <- diva_or |> filter(migration == "FALSE") |> 
  select(locationid, rcp:adaptation, time, expected_annual_damages,
         sea_dike_cost_investment, sea_dike_cost_maintenance,totalpop) |> 
  arrange(rcp,quantile,adaptation,time,locationid)
diva <- diva |> filter(locationid != "PRI") #Porto Rico have missing data for totalpop

variables_toconvert <- c("expected_annual_damages", "sea_dike_cost_investment", "sea_dike_cost_maintenance")
abbreviations <- c("ead", "inv", "mant")

MER_values <- conversion_gdpuc(
  diva,
  variables_toconvert,
  from = "constant 2011 Int$PPP",
  to = "constant 2017 US$MER",
  abbreviations,
  loc = "locationid",
  time = "time"
)

# merging with gdp per capita values from SSPs
MER_values <- MER_values |> 
  left_join(gdpc |> select(ISO, time, computed_gdpc) |> rename(gdpc_MER2017 = computed_gdpc),
            join_by(locationid == ISO, time))

MER_values <- MER_values |> relocate(totalpop, .after = gdpc_MER2017) |> 
  rename(ead_PPP2011=expected_annual_damages,dike_inv_PPP2011=sea_dike_cost_investment,mant_PPP2011=sea_dike_cost_maintenance) |> 
  arrange(rcp, quantile, adaptation, time, locationid) |> 
  mutate(total_assets = gdpc_MER2017 * totalpop * 2.8) #computing total assets


saveRDS(MER_values,"outputs/capital/Ead_dikeinv_dike_maint_gdpc_MER2017_ISO.rds")
write_xlsx(MER_values, "outputs/capital/Ead_dikeinv_dike_maint_gdpc_MER2017_ISO.xlsx")






# INTERPOLATION -----------------------------------------------------------

sel <- MER_values |> select(locationid:time,ead_MER2017,total_assets) |> 
  arrange(rcp,quantile,adaptation,locationid,time)

alltimes <- seq(2015, 2100, by = 1)
time <- unique(sel$time)
groupby_vars <- c("locationid", "rcp", "quantile", "adaptation")
interp_vars <- c("ead_MER2017", "total_assets")


# linear interpolation 
k_interpolated <- lin_interp_ongroups(sel, time, alltimes, groupby_vars, interp_vars)





# RECALCULATIONS ISO ------------------------------------------------------

# annual capital growth rate and growth factor
k_shock_growth_ISO <- growthRate_growthFactor(k_interpolated,
                                              time, 2015,
                                              groupby_vars,
                                              ead_MER2017_yearly,
                                              total_assets_yearly)

# 5 times cumulative shocks for locationid
k_shock_5y_ISO <- cum_shocks_5y(k_shock_growth_ISO, time, 2018, groupby_vars, growth_factor)

k_shock_5y_ISO_wide <- k_shock_5y_ISO |> 
  select(-cumulated_growth) |> 
  pivot_wider(names_from = time, values_from = cumulated_shock)

# cumulated shock over 50 times (2020-2070)
k_shock_cum_ISO <- cum_shock_manyyears(k_shock_growth_ISO, 2018, 2070, time, groupby_vars, growth_factor)


saveRDS(k_shock_5y_ISO_wide, "outputs/capital/k_shock_SLR_5y_ISO.rds")
write_xlsx(k_shock_5y_ISO_wide, "outputs/capital/k_shock_SLR_5y_ISO.xlsx")

saveRDS(k_shock_cum_ISO, "outputs/capital/k_shock_cum_ISO.rds")
write_xlsx(k_shock_cum_ISO, "outputs/capital/k_shock_SLR_cum_ISO.xlsx")




# RECALCULATIONS ACCREU ICES MH -------------------------------------------

# merging with mapping
interp_ICES_MH <- agg_ICES_MH |> left_join(k_interpolated, join_by(ISO == locationid))

EAD_ASSET_ICES_MH <- interp_ICES_MH |>
  group_by(across(starts_with("ICES_ACCREU")), rcp, quantile, adaptation, time) |>
  summarise(across(c(ead_MER2017_yearly, total_assets_yearly), ~ sum(.x, na.rm = TRUE)),
            .groups = "drop")

# annual capital growth
groupby_varsMH <- c(groupby_vars[2:4], "ICES_ACCREU", "ICES_ACCREU_long", "ICES_ACCREU_n")
k_shock_growth_ICES_MH <- growthRate_growthFactor(EAD_ASSET_ICES_MH,
                                                  time, 2015,
                                                  groupby_varsMH,
                                                  ead_MER2017_yearly,
                                                  total_assets_yearly)
# 5 times cumulative shocks 
k_shock_5y_ICES_MH <- cum_shocks_5y(k_shock_growth_ICES_MH, time, 2018, groupby_varsMH, growth_factor)

k_shock_5y_ICES_MH_wide <- k_shock_5y_ICES_MH |> 
  select(-cumulated_growth) |> 
  pivot_wider(names_from = time, values_from = cumulated_shock)

# cumulated 50-times shock
k_shock_cum_ICES_MH <- cum_shock_manyyears(k_shock_growth_ICES_MH, 2018, 2070, time, groupby_varsMH, growth_factor)


# Saving outputs (also, CSVs disaggregated by scenario and adaptation option)
saveRDS(k_shock_5y_ICES_MH_wide, "outputs/capital/k_shock_5y_ICES_MH.rds")
write_xlsx(k_shock_5y_ICES_MH_wide, "outputs/capital/k_shock_5y_ICES_MH.xlsx")

rcp_uniques <- unique(k_shock_5y_ICES_MH_wide$rcp)
adapt_uniques <- unique(k_shock_5y_ICES_MH_wide$adaptation)
quantile_uniques <- unique(k_shock_5y_ICES_MH_wide$quantile)
saving_disagg(k_shock_5y_ICES_MH_wide, "k_shock_5y_ICES_MH_",
              rcp_uniques, adapt_uniques, quantile_uniques,
              additional_cols_todrop = c("ICES_ACCREU_long", "ICES_ACCREU_n"),
              path = "outputs/capital")

write_xlsx(k_shock_cum_ICES_MH, "outputs/capital/k_shock_cum_ICES_MH.xlsx")




# RECALCULATIONS ACCREU ICES REG ------------------------------------------

agg_ICES_REG_nonNUTS <- agg_ICES_REG |> filter(Filter_NUTS == 1)

# merging with mapping and building whole ds
interp_ICES_REG <-  agg_ICES_REG_nonNUTS |> left_join(k_interpolated, join_by(ISO == locationid))
landlock_countries <- anti_join(agg_ICES_REG_nonNUTS, k_interpolated, join_by(ISO == locationid))

landlock_complete <- crossjoin_tobind(landlock_countries, interp_ICES_REG, 
                 cols_structure = c("rcp", "quantile", "adaptation", "time"),
                 cols_values = c("ead_MER2017_yearly", "total_assets_yearly"))
interp_ICES_REG_all <- bind_rows(interp_ICES_REG, landlock_complete)

EAD_ASSET_ICES_REG <- interp_ICES_REG_all |>
  group_by(across(starts_with("ICES_ACCREU")), rcp, quantile, adaptation, time) |>
  summarise(across(c(ead_MER2017_yearly, total_assets_yearly), ~ sum(.x, na.rm = TRUE)), .groups = "drop")

# annual capital growth
groupby_varsREG <- c(groupby_vars[2:4],
                     "ICES_ACCREU_REG","ICES_ACCREU_REG_long","ICES_ACCREU_REG_n","ICES_ACCREU_AGG")
k_shock_growth_ICES_REG <- growthRate_growthFactor(EAD_ASSET_ICES_REG,
                                                  time, 2015,
                                                  groupby_varsREG,
                                                  ead_MER2017_yearly,
                                                  total_assets_yearly)
# 5 times cumulative shocks (for GTAP)
k_shock_5y_ICES_REG <- cum_shocks_5y(k_shock_growth_ICES_REG, time, 2018, groupby_varsREG, growth_factor)

k_shock_5y_ICES_REG_wide <- k_shock_5y_ICES_REG |> 
  select(-cumulated_growth) |> 
  pivot_wider(names_from = time, values_from = cumulated_shock)

# cumulated 50-times shock
k_shock_cum_ICES_REG <- cum_shock_manyyears(k_shock_growth_ICES_REG, 2018, 2070, time, groupby_varsREG, growth_factor)


# Saving outputs
saveRDS(k_shock_5y_ICES_REG_wide, "outputs/capital/k_shock_5y_ICES_REG.rds")
write_xlsx(k_shock_5y_ICES_REG_wide, "outputs/capital/k_shock_5y_ICES_REG.xlsx")

write_xlsx(k_shock_cum_ICES_REG, "outputs/capital/k_shock_cum_ICES_REG.xlsx")







# MAPS AND DASHBOARD ---------------------------------------------------------

# geometry data
world <- tibble(ne_countries(scale = "medium", returnclass = "sf")) |> rename(locationid = iso_a3) |> 
  mutate(locationid = if_else(iso_a3_eh %in% c("FRA", "NOR"), iso_a3_eh, locationid)) |> 
  select(locationid, geometry) |> filter(locationid != -99)

# "ead": 5y cum values
ead <- left_join(k_shock_5y_ISO, world, join_by(locationid))
# "ead_cum": cum values 2018-2070
ead_cum <- left_join(k_shock_cum_ISO, world, join_by(locationid))

ead_missing <- anti_join(world, k_shock_5y_ISO, join_by(locationid))

structure <- colnames(ead |> select(rcp:time))
cols_values <- tail(names(k_shock_cum_ISO), 2)
ead_world <- bind_rows(ead, crossjoin_tobind(ead_missing, ead, structure, cols_values, NA)) |> 
  mutate(cumulated_shock = if_else(locationid=="NLD", NA, cumulated_shock))

structure2 <- colnames(ead_cum |> select(rcp:adaptation))
ead_cum_world <- bind_rows(ead_cum, crossjoin_tobind(ead_missing, ead_cum, structure2, cols_values2, NA)) |> 
  mutate(cumulated_shock = if_else(locationid=="NLD", NA, cumulated_shock))

#data for the Netherlands are wrong (for now), so I rescale them for visualisation purposes



# maps of the cumulated shocks (2018-2070)
#### customizable parameters and choice to save output ###
plot_world_cum("capital",ead_cum_world_revisited, scen = 126, quant = 0.05, adapt = "Constant dike height")



#Selection chart
ui <- fluidPage(
  titlePanel("World map of sea level rise impacts"),
  tags$h4("5-years cumulated % of expected annual damages"),
  
  fluidRow(
    column(
      width = 3,  # Sidebar column
      wellPanel(
        selectInput("rcp", "RCP:", choices = unique(ead_world$rcp)),
        selectInput("quantile", "Quantile:", choices = unique(ead_world$quantile)),
        selectInput("adaptation", "Adaptation:", choices = unique(ead_world$adaptation)),
        selectInput("time", "time:", choices = seq(2020,2100, 5))
      )),
    column(
      width = 9,  # Main plot column
      plotOutput("plot", height = "700px")  # You can adjust height if needed
    )))

# Server
server <- function(input, output) {
  filtered_data <- reactive({
    ead_world |> filter(
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
