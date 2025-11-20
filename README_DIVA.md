# DIVA

The dataset is structured into three levels: **local "GADM1"**, **national "COUNTRY"**, and **global "GLOBAL"**. Each level provides data on **physical damages, coastal flooding projections, population and asset exposure and sea-level rise (SLR) impacts**.
The dataset includes information on **areas, populations and assets at risk** based on specific elevation thresholds, along with **adaptation costs and other variables related to coastal protection and adaptation strategies**.  

All cost and damage variables in **DIVA** represent **annual values** and must be multiplied by the number of years in the respective time step period to calculate total cumulative expenses or damages.
**DIVA uses 10-year time steps, except for 2015-2020**.
Thus, to correctly sum up the total **SLR and flood cost** over the century the cost in 2020 has to be multiplied by 5 (2015-2020 time span). From 2030 to 2100 you have to multiply by 10 (years).
Monetary values of Migration (Retreat) and EAD are expressed in **million US$ PPP in 2011 constant prices**.
Monetary values of Dike costs (investment and maintainance) are expressed in **million US$ PPP in 2011 constant prices**.
The protection levels are measured in **return period**.

## **Variables**  

### Area 

- **area_below_01p0** : area below 1 meter water level (km²)  
- **area_below_02p0** : area below 2 meter water level (km²)  
- **area_below_10p0** : area below 10 meter water level (km²)  
- **area_below_h100** : area below the 1-in-100-year floodplain (km²)  

### Assets

- **assets_below_01p0** : assets below 1 meter water level (Mln US$)  
- **assets_below_02p0** : assets below 2 meter water level (Mln US$)  
- **assets_below_10p0** : assets below 10 meter water level (Mln US$)    
- **assets_below_h100** : assets below the 1-in-100-year floodplain (Mln US$)  

### Population  

- **population_below_01p0** : population below 1 meter water level (number of people)  
- **population_below_02p0** : population below 2 meter water level (number of people)  
- **population_below_10p0** : population below 10 meter water level (number of people)  
- **population_below_20p0** : population below 20 meter water level (number of people)  
- **population_below_h100** : population below the 1-in-100-year floodplain (number of people)  
- **totalpop** : Total population (number of people), only at the country level

### Damages

- **expected_annual_damages** : expected annual damages from coastal flooding events (Mln US$/year)  
- **expected_people_flooded** : expected annual people flooded (number of people/year)  

- **land_loss** : land lost due to SLR, land is lost if it lies below the 1-in-1 year flood return level (km²/year)  

### Adaptation and Costs

- **population_migration** : population migrated from the floodplain (number of people/year). This is **reactive migration**. As soon as people lie below the 1-in-1-year floodplain, the area becomes uninhabitable, and they are forced to relocate as they experience at least one flood event per year.

- **migration_cost** : cost of migration, calculated as **number of migrants** * **GDP per capita** * **factor of 3** which inlcudes the relocation of assets and decostruction costs (Mln US$/year)  

- **length_protected** : coast protected by seadykes (km)  
- **protection_level** : protected against the event (1-in-x-year event). The event return period against which the floodplain is protected. For GLOBAL, COUNTRY and GADM1/NUTS2 this represents an average of the corresponding floodplains.

- **sea_dike_height** : current height of seadykes (m). For GLOBAL, COUNTRY and GADM1/NUTS2 this represents an average of the corresponding floodplains. 
- **sea_dike_cost_investment** : investment costs of building new seadykes or raising existing seadyke height (Mln US$/year)  
- **sea_dike_cost_maintenance** : maintenance costs for keeping seadykes functioning (Mln US$/year)  
 

### SLR and Coastline 

- **coast_length** : length of the coastline (km)  
- **rslr** : relative sea level rise (m). For GLOBAL, COUNTRY and GADM1/NUTS2 this represents an average of the corresponding floodplains.  


## Model runs

### Climate Scenarios (AR6)

**RCPs** : 
- RCP 2.6 Very low greenhouse gas emissions
- RCP 4.5 Intermediate emissions
- RCP 7.0 Higher emissions
- RCP 8.5 Very high emissions

### Socio-economic scenarios (IIASA)

**SSPs** : 
- SSP1 (Taking the Green Road): A world shifting towards sustainability, emphasizing inclusive development and respect for environmental boundaries
- SSP2 (Middle of the Road): A world following historical trends with uneven development and moderate environmental degradation
- SSP3 Regional Rivalry (A Rocky Road): A world with regional conflicts, national security focus, and rising inequality
- SSP4 Inequality (A Road Divided): A world with significant inequalities and varying levels of development
- SSP5 Fossil-fueled Development (Taking the Highway): A world with rapid economic growth based on fossil fuels and high energy consumption

### Confidence levels and quantiles

**Confidence** : Medium
**Quantiles** : 0.05, 0.5, 0.95

### Time steps

The **model initialization** is in 2015. The value in 2015 should not be used or interpreted as "model output". Rather this value represents the initial condition of the model.
The period 2015-2020 is represented by the 2020 value. 
**10y time steps from 2020** : 2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100

## Adaptation scenarios in DIVA

- **Constant dike height** : the seadyke height is maintained at the current days height (**no adaptation**)
- **Constant flood protection standards** : the seadyke height is updated to keep the current days protection level (**BAU adaptation**) 
- **Optimal protection** : Cost-benefit optimal protection, minimizing  (**high-level adaptation**)

## If any other clarifications are needed

Please refer to the ACCREU deliverables 2.1 and 2.4 in which we detail which datasets we use as inputs, how we process these inputs and how DIVA model works in general.

**If this does not reply to your specific query** feel free to contact the DIVA team:

**Jonas Haas** jonas.haas@globalclimateforum.org
**Sebastiano Bacca** sebastiano.bacca@globalclimateforum.org



