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

#read land areas (from WDI)
land <- read_csv("inputs/TotalLandArea_wdi.csv", skip=4)






# ATTACHING LAND AREAS ----------------------------------------------------

# counting NAs per year
years <- as.character(seq(2015,2023))
for (i in years) {
  land |>
    select(all_of(c("Country Name", i))) |>
    count(is.na(.data[[i]])) |>
    print()
}

#selection: year 2021
land_sel <- land |> select(locationid = `Country Code`, total_land = "2021")
#drop NAs
missing <- land_sel |> filter(is.na(total_land)) |> select(locationid)
# land |> filter(`Country Code` %in% missing[[1]]) 
land_sel <- land_sel |> filter(!locationid %in% missing[[1]]) #Kosovo and "Not Classified" (can be dropped)

#filtering (only Migration = FALSE) and rounding
diva_land <- diva_or |> filter(migration == "FALSE") |> 
  select(locationid:adaptation, time, land_loss) |>
  arrange(rcp,quantile,adaptation,locationid,time) |> select(!ssp) |> 
  relocate(locationid, .after = time)

diva_land_merged <- left_join(diva_land, land_sel, join_by(locationid)) |> 
  arrange(rcp,quantile,adaptation,time,locationid)

countrycode(setdiff(unique(diva_land$locationid),unique(land_sel$locationid)),
            origin = "iso3c", destination="country.name") 
# countries' lost (only islands and Taiwan - not present in tot land WDI ds)




# INTERPOLATION -----------------------------------------------------------

allyears <- seq(2015, 2100, by = 1)
time <- unique(diva_land$time)
groupby_vars <- c("locationid", "rcp", "quantile", "adaptation")
interp_vars <- c("land_loss")

# linear interpolation 
land_interpolated <- lin_interp_ongroups(diva_land_merged, time, allyears, groupby_vars, interp_vars)
land_interpolated <- land_interpolated |> left_join(land_sel, join_by(locationid)) |> 
  group_by(across(all_of(groupby_vars))) |> mutate(land_remaining = total_land[time == 2015][1] - cumsum(land_loss_yearly)) |> ungroup() |> 
  mutate(land_remaining = if_else(land_remaining < 0, 0, land_remaining))



# RECALCULATIONS ISO ------------------------------------------------------

# annual land growth (-)
land_shock_ISO_y <- land_interpolated  |> 
  group_by(across(all_of(groupby_vars))) |> 
  mutate(land_shock_y = if_else(land_remaining == 0, -100, (land_remaining / lag(land_remaining) - 1) * 100)) |> ungroup()

# 5 years cumulative shocks
land_shock_5y_iso <- land_interpolated |> 
  arrange(across(all_of(groupby_vars)), time) |> group_by(across(all_of(groupby_vars))) |> 
  mutate(land_shock_5y = if_else(land_remaining == 0, -100, (land_remaining / lag(land_remaining, 5) - 1) * 100)) |> ungroup() |> 
  filter(time %% 5 == 0 & time != 2015)

land_shock_5y_iso_wide <- land_shock_5y_iso |> 
  select(- c("land_loss_yearly", -"total_land", -"land_remaining")) |> 
  pivot_wider(names_from = time, values_from = land_shock_5y)

# cumulated shock over 50 years (2020-2070)
land_shock_cum_iso <- land_interpolated |> 
  arrange(across(all_of(groupby_vars)), time) |> group_by(across(all_of(groupby_vars))) |>
  mutate(land_shock_cum = if_else(land_remaining == 0, -100, (land_remaining / land_remaining[time == 2020][1] - 1) * 100)) |>
  ungroup() |> filter(time == 2070) |> select(-time)

# saving output
saveRDS(land_shock_5y_iso_wide, "outputs/land/land_shock_SLR_5y_ISO.rds")
write_xlsx(land_shock_5y_iso_wide, "outputs/land/land_shock_SLR_5y_ISO.xlsx")

saveRDS(land_shock_cum_iso, "outputs/land/land_shock_SLR_cum_ISO.rds")
write_xlsx(land_shock_cum_iso, "outputs/land/land_shock_SLR_cum_ISO.xlsx")



# RECALCULATIONS ACCREU ICES MH -------------------------------------------

# merging with mapping
land_interpolated_ICES_MH <- agg_ICES_MH |> left_join(land_interpolated, join_by(ISO == locationid))

groupby_varsMH <- c(groupby_vars[2:4], "ICES_ACCREU", "ICES_ACCREU_long", "ICES_ACCREU_n")
LAND_ICES_MH <- land_interpolated_ICES_MH |>
  group_by(across(starts_with("ICES_ACCREU")), rcp, quantile, adaptation, time) |>
  summarise(across(c(land_loss_yearly, total_land), ~ sum(.x, na.rm = TRUE)), .groups="drop") |>
  group_by(across(all_of(groupby_varsMH))) |> 
  mutate(land_remaining = total_land[time == 2015][1] - cumsum(land_loss_yearly)) |> ungroup() |> 
  mutate(land_remaining = if_else(land_remaining < 0, 0, land_remaining))

# annual capital growth
land_shock_growth_ICES_MH <- LAND_ICES_MH |> 
  group_by(across(all_of(groupby_varsMH))) |> 
  mutate(land_shock_y = if_else(land_remaining == 0, -100, (land_remaining / lag(land_remaining) - 1) * 100)) |> ungroup()

# 5 years cumulative shocks 
land_shock_5y_ICES_MH <- LAND_ICES_MH |> 
  arrange(across(all_of(groupby_varsMH)), time) |> group_by(across(all_of(groupby_varsMH))) |> 
  mutate(land_shock_5y = if_else(land_remaining == 0, -100, (land_remaining / lag(land_remaining, 5) - 1) * 100)) |> ungroup() |> 
  filter(time %% 5 == 0 & time != 2015)

land_shock_5y_ICES_MH_wide <- land_shock_5y_ICES_MH |> 
  select(- c("land_loss_yearly", -"total_land", -"land_remaining")) |> 
  pivot_wider(names_from = time, values_from = land_shock_5y)

# cumulated 50-years shock
land_shock_cum_ICES_MH <- LAND_ICES_MH |>
  arrange(across(all_of(groupby_varsMH)), time) |> group_by(across(all_of(groupby_varsMH))) |>
  mutate(land_shock_cum = if_else(land_remaining == 0, -100, (land_remaining / land_remaining[time == 2020][1] - 1) * 100)) |>
  ungroup() |> filter(time == 2070) |> select(-time)


# Saving outputs (also, CSVs disaggregated by scenario and adaptation option)

saveRDS(land_shock_5y_ICES_MH_wide, "outputs/land/land_shock_5y_ICES_MH.rds")
write_xlsx(land_shock_5y_ICES_MH_wide, "outputs/land/land_shock_5y_ICES_MH.xlsx")

rcp_uniques <- unique(land_shock_5y_ICES_MH_wide$rcp)
adapt_uniques <- unique(land_shock_5y_ICES_MH_wide$adaptation)                          
saving_disagg(land_shock_5y_ICES_MH_wide, "land_shock_5y_ICES_MH_",
              rcp_uniques, adapt_uniques, quantile = 0.5,
              additional_cols_todrop = c("ICES_ACCREU_long", "ICES_ACCREU_n"),
              path = "outputs/land/")

write_xlsx(land_shock_cum_ICES_MH, "outputs/land/land_shock_cum_ICES_MH.xlsx")




# RECALCULATIONS ACCREU ICES REG ------------------------------------------

agg_ICES_REG_nonNUTS <- agg_ICES_REG |> filter(Filter_NUTS == 1)

# merging with mapping and building whole ds
interp_ICES_REG <-  agg_ICES_REG_nonNUTS |> left_join(land_interpolated, join_by(ISO == locationid))
landlock_countries <- anti_join(agg_ICES_REG_nonNUTS, land_interpolated, join_by(ISO == locationid))

landlock_complete <- crossjoin_tobind(landlock_countries, interp_ICES_REG, 
                                      cols_structure = c("rcp", "quantile", "adaptation", "time"),
                                      cols_values = c("land_loss_yearly")) |>
  left_join(land_sel, join_by(ISO == locationid)) # adding total land (which is never = 0)
interp_ICES_REG_all <- bind_rows(interp_ICES_REG, landlock_complete)

groupby_varsREG <- c(groupby_vars[2:4],
                     "ICES_ACCREU_REG","ICES_ACCREU_REG_long","ICES_ACCREU_REG_n","ICES_ACCREU_AGG")
LAND_ICES_REG <- interp_ICES_REG_all |>
  group_by(across(starts_with("ICES_ACCREU")), rcp, quantile, adaptation, time) |>
  summarise(across(c(land_loss_yearly, total_land), ~ sum(.x, na.rm = TRUE)), .groups = "drop") |> 
  group_by(across(all_of(groupby_varsREG))) |> 
  mutate(land_remaining = total_land[time == 2015][1] - cumsum(land_loss_yearly)) |> ungroup()

# annual capital growth
land_shock_growth_ICES_REG <- LAND_ICES_REG |> 
  group_by(across(all_of(groupby_varsREG))) |> 
  mutate(land_shock_y = if_else(land_remaining == 0, -100, (land_remaining / lag(land_remaining) - 1) * 100)) |> ungroup()

# 5 years cumulative shocks 
land_shock_5y_ICES_REG <- LAND_ICES_REG |> 
  arrange(across(all_of(groupby_varsREG)), time) |> group_by(across(all_of(groupby_varsREG))) |> 
  mutate(land_shock_5y = if_else(land_remaining == 0, -100, (land_remaining / lag(land_remaining, 5) - 1) * 100)) |> ungroup() |> 
  filter(time %% 5 == 0 & time != 2015)

land_shock_5y_ICES_REG_wide <- land_shock_5y_ICES_REG |> 
  select(- c("land_loss_yearly", -"total_land", -"land_remaining")) |> 
  pivot_wider(names_from = time, values_from = land_shock_5y)

# cumulated 50-years shock
land_shock_cum_ICES_REG <- LAND_ICES_REG |>
  arrange(across(all_of(groupby_varsREG)), time) |> group_by(across(all_of(groupby_varsREG))) |>
  mutate(land_shock_cum = if_else(land_remaining == 0, -100, (land_remaining / land_remaining[time == 2020][1] - 1) * 100)) |>
  ungroup() |> filter(time == 2070) |> select(-time)

# Saving outputs 
saveRDS(land_shock_5y_ICES_REG_wide, "outputs/land/land_shock_5y_ICES_REG.rds")
write_xlsx(land_shock_5y_ICES_REG_wide, "outputs/land/land_shock_5y_ICES_REG.xlsx")

write_xlsx(land_shock_cum_ICES_REG, "outputs/land/land_shock_cum_ICES_REG.xlsx")



# MAPS AND DASHBOARD ---------------------------------------------------------

# geometry data
world <- tibble(ne_countries(scale = "medium", returnclass = "sf")) |> rename(locationid = iso_a3) |> 
  mutate(locationid = if_else(iso_a3_eh %in% c("FRA", "NOR"), iso_a3_eh, locationid)) |> 
  select(locationid, geometry) |> filter(locationid != -99)

# "land": 5y cumulated (total) land loss
land <- left_join(land_shock_5y_iso, world, join_by(locationid))
# "land_cum": 2018-2070 cumulated (total) land loss
land_cum <- left_join(land_shock_cum_iso, world, join_by(locationid))

land_missing <- anti_join(world, land_shock_5y_iso, join_by(locationid))

structure <- colnames(land |> select(rcp:time))
structure2 <- colnames(land_cum |> select(rcp:adaptation))
cols_values <- tail(names(land_shock_5y_iso), 4)
cols_values2 <- tail(names(land_shock_cum_iso), 4)

land_world <- bind_rows(land, crossjoin_tobind(land_missing, land, structure, cols_values, NA)) |> 
  filter(!locationid %in% c("NRU", "TUV")) # Here I eliminate Nauru and Tuvalu just for plotting purposes (they are outliers)

land_cum_world <- bind_rows(land_cum, crossjoin_tobind(land_missing, land_cum, structure2, cols_values2, NA)) |> 
  filter(!locationid %in% c("NRU", "TUV")) # Here I eliminate Nauru and Tuvalu just for plotting purposes (they are outliers)



# maps of the cumulated shocks (2018-2070)
#### customizable parameters and choice to save output ###
plot_world_cum("land", land_cum_world, scen = 370, quant = 0.95, adapt = "Constant dike height", land_shock_cum)



#Selection chart
ui <- fluidPage(
  titlePanel("World map of sea level rise impacts"),
  tags$h4("5-years cumulated % of (total) land loss"),
  
  fluidRow(
    column(
      width = 3,  # Sidebar column
      wellPanel(
        selectInput("rcp", "RCP:", choices = unique(land_world$rcp)),
        selectInput("quantile", "Quantile:", choices = unique(land_world$quantile)),
        selectInput("adaptation", "Adaptation:", choices = unique(land_world$adaptation)),
        selectInput("time", "time:", choices = seq(2020,2100, 5))
      )),
    column(
      width = 9,  # Main plot column
      plotOutput("plot", height = "700px")  # You can adjust height if needed
    )))

# Server
server <- function(input, output) {
  filtered_data <- reactive({
    land_world |> filter(
      rcp == input$rcp,
      quantile == input$quantile,
      adaptation == input$adaptation,
      time == input$time)
  })
  
  output$plot <- renderPlot({
    ggplot(st_as_sf(filtered_data())) +
      geom_sf(aes(fill = land_shock_5y)) +
      scale_fill_gradientn(
        colours = c("lightyellow", "khaki1", "gold", "orange", "orangered2", "red3","purple4"),
        values = rescale(-c(0, 0.8, 1.8, 3.5, 7.5, 15, 100)),
        na.value = "lightgrey",
        limits = c(0,-15)
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
