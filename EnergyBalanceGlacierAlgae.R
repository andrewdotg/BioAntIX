library(dplyr)
library(ggplot2)

# Constants
C_p <- 1005  # Specific heat capacity of air (J/(kg*K))
rho_a <- 1.2  # Air density (kg/m^3)
L_f <- 334000  # Latent heat of fusion for ice (J/kg)
k_base <- 0.001  # Base empirical heat transfer coefficient 
T_snow <- 0  # Snow temperature 
albedo_snow <- 0.85  # Albedo for snow
albedo_weathering_crust <- 0.6  # Albedo for weathering crust

density_snow <- 0.47  # Density of snow (g/cm^3)
density_weathering_crust <- 0.5  # Density of weathering crust (g/cm^3)

# Average cell count and standard deviation
avg_cell_count <- 2.5e5
cell_count_sd <- 2.7e5

weatherSt <- read.csv("figures/23021021432090_robert_1.csv")

# Rename columns 
weatherReadable <- weatherSt %>%
  rename(
    air_temp = `Temp...C..LGR.S.N..21432090..SEN.S.N..21420856.`,
    PAR = `PAR..µmol.m..s..LGR.S.N..21432090..SEN.S.N..21420546.`,
    humidity = `RH.....LGR.S.N..21432090..SEN.S.N..21420856.`,
    wind_speed = `Wind.Speed..m.s..LGR.S.N..21432090..SEN.S.N..21435605.`,
    DOY = `DOY`
  )

# Filter data for DOY 32 and DOY 37
doy32 <- weatherReadable %>% filter(DOY == 32)
doy37 <- weatherReadable %>% filter(DOY == 37)

# Calculate melt parameters for different surface types
day_melt_calculation <- function(filtered_data, albedo, density, avg_cell_count, cell_count_sd) {
  # Convert PAR from µmol/m²/s to W/m² (approximate conversion factor: 0.219)
  filtered_data <- filtered_data %>%
    mutate(PAR_W_m2 = PAR * 0.219)
  
  # Adjust empirical heat transfer coefficient (k) based on relative humidity
  filtered_data <- filtered_data %>%
    mutate(k = k_base * (1 + (humidity / 100) * 0.5))  # Assuming k increases by 50% when humidity is 100%
  
  # Calculate sensible heat flux (Q_H) for each 5-minute interval
  filtered_data <- filtered_data %>%
    mutate(Q_H = C_p * rho_a * k * (air_temp - T_snow) * wind_speed)
  
  # Calculate total energy due to mean air temperature and wind speed (in J/m²) for each 5-minute interval
  filtered_data <- filtered_data %>%
    mutate(E_temp = Q_H * 300)  # 300 seconds in 5 minutes
  
  # Calculate energy contribution from solar radiation (PAR) with albedo effect (in J/m²) for each 5-minute interval
  filtered_data <- filtered_data %>%
    mutate(E_solar = PAR_W_m2 * (1 - albedo) * 300)  # 300 seconds in 5 minutes
  
  # Calculate melt due to air temperature, wind speed, and solar radiation (in kg/m²) for each 5-minute interval
  # Adjust latent heat of fusion for density
  L_f_adjusted <- L_f * (density / 0.9)  # Assume 0.9 g/cm^3 ice density
  filtered_data <- filtered_data %>%
    mutate(M_temp_solar = (E_temp + E_solar) / L_f_adjusted)
  
  # Sum all 5-minute intervals to get total melt due to temperature, wind, and solar radiation
  M_temp_solar_total <- sum(filtered_data$M_temp_solar, na.rm = TRUE)
  
  cell_densities <- c(avg_cell_count, avg_cell_count - cell_count_sd, avg_cell_count + cell_count_sd)
  results <- data.frame(cell_density = cell_densities, total_melt_cm = numeric(length(cell_densities)), algal_melt_cm = numeric(length(cell_densities)), non_algal_total_melt_cm = numeric(length(cell_densities)), algal_melt_sd = numeric(length(cell_densities)))
  
  # Loop through each cell density to calculate melt
  for (i in seq_along(cell_densities)) {
    cell_density <- cell_densities[i]
    IRF <- if (cell_density <= 0) 0 else 5.52 * log(cell_density) - 24.03  # IRF in W/m²
    
    # Calculate energy due to algae for each 5-minute interval
    filtered_data <- filtered_data %>%
      mutate(E_IRF = PAR_W_m2 * (1 - albedo) * IRF * 300 / 1000)  # Adjust IRF based on PAR and albedo, convert to J
    
    # Calculate melt due to algae for each 5-minute interval (in kg/m²)
    filtered_data <- filtered_data %>%
      mutate(M_IRF = E_IRF / L_f_adjusted)
    
    # Sum all 5-minute intervals to get total melt due to algae
    M_IRF_total <- sum(filtered_data$M_IRF, na.rm = TRUE)
    
    # Calculate total melt for the scenario (in kg/m²)
    M_total <- M_temp_solar_total + M_IRF_total
    
    # Calculate non-algal total melt (in kg/m²)
    M_non_algal_total <- M_temp_solar_total
    
    # Convert melt from kg/m² to cm meltwater equivalent (1 kg/m² = 0.1 cm)
    results$total_melt_cm[i] <- M_total * 0.1
    results$algal_melt_cm[i] <- M_IRF_total * 0.1
    results$non_algal_total_melt_cm[i] <- M_non_algal_total * 0.1
  }
  
  # Calculate standard deviation of algal melt based on the IRF-cell density relationship (R^2 = 0.41)
  R2_IRF <- 0.41
  results$algal_melt_sd <- sqrt(1 - R2_IRF) * results$algal_melt_cm
  
  return(results)
}

# Calculate melt for DOY 32 and DOY 37 for snow
results_32_snow <- day_melt_calculation(doy32, albedo_snow, density_snow, avg_cell_count, cell_count_sd)
results_37_snow <- day_melt_calculation(doy37, albedo_snow, density_snow, avg_cell_count, cell_count_sd)

# Calculate melt for DOY 32 and DOY 37 for weathering crust
results_32_weathering <- day_melt_calculation(doy32, albedo_weathering_crust, density_weathering_crust, avg_cell_count, cell_count_sd)
results_37_weathering <- day_melt_calculation(doy37, albedo_weathering_crust, density_weathering_crust, avg_cell_count, cell_count_sd)

results_32_snow$DOY <- 32
results_37_snow$DOY <- 37
results_32_weathering$DOY <- 32
results_37_weathering$DOY <- 37
results_32_snow$surface <- "Snow"
results_37_snow$surface <- "Snow"
results_32_weathering$surface <- "Weathering Crust"
results_37_weathering$surface <- "Weathering Crust"

# Combine for plotting
combined_results <- rbind(results_32_snow, results_37_snow, results_32_weathering, results_37_weathering)

# Plot algal forced melt for both DOY 32 and DOY 37 for snow and weathering crust
plot_algal_melt <- ggplot() +
  geom_line(data = combined_results %>% filter(surface == "Snow"), aes(x = cell_density, y = algal_melt_cm, color = paste("Snow - DOY", DOY))) +
  geom_line(data = combined_results %>% filter(surface == "Weathering Crust"), aes(x = cell_density, y = algal_melt_cm, linetype = paste("Weathering Crust - DOY", DOY))) +
  geom_ribbon(data = combined_results %>% filter(surface == "Snow"), aes(x = cell_density, ymin = algal_melt_cm - algal_melt_sd, ymax = algal_melt_cm + algal_melt_sd, fill = paste("Snow - DOY", DOY)), alpha = 0.2) +
  geom_ribbon(data = combined_results %>% filter(surface == "Weathering Crust"), aes(x = cell_density, ymin = algal_melt_cm - algal_melt_sd, ymax = algal_melt_cm + algal_melt_sd, fill = paste("Weathering Crust - DOY", DOY)), alpha = 0.2) +
  labs(title = "Algal Forced Melt for DOY 32 and DOY 37 on Snow and Weathering Crust with Standard Deviation",
       x = "Cell Density (cells per ml)",
       y = "Algal Forced Melt (cm)",
       color = "Surface Type and Day of Year",
       linetype = "Surface Type and Day of Year",
       fill = "Standard Deviation") +
  theme_minimal()

print(plot_algal_melt)

# Calculate average percentage contribution of algal melt to total melt for both days and both surface types
avg_percent_contribution <- combined_results %>%
  group_by(DOY, surface) %>%
  summarise(avg_percent_contribution = mean((algal_melt_cm / total_melt_cm) * 100, na.rm = TRUE))

print(avg_percent_contribution)

# Calculate daily total melt for each scenario
combined_results_daily <- combined_results %>%
  group_by(DOY, surface) %>%
  summarise(
    daily_total_melt_cm = mean(total_melt_cm, na.rm = TRUE),
    algal_melt_sd = mean(algal_melt_sd, na.rm = TRUE),
    daily_algal_melt_cm = mean(algal_melt_cm, na.rm = TRUE)
  )

print(combined_results_daily)

# Calculate volume of melt for each day if the algae covers 13.8 km²
area_km2 <- 13.8
area_m2 <- area_km2 * 1e6
combined_results_daily <- combined_results_daily %>%
  mutate(
    daily_total_melt_volume_m3 = (daily_total_melt_cm / 100) * area_m2,
    daily_algal_melt_volume_m3 = (daily_algal_melt_cm / 100) * area_m2,
    algal_melt_volume_sd_m3 = (algal_melt_sd / 100) * area_m2
  )

print(combined_results_daily)

# Calculate average temperature and PAR for DOY 32 and DOY 37
avg_temp_par <- weatherReadable %>%
  filter(DOY %in% c(32, 37)) %>%
  group_by(DOY) %>%
  summarise(avg_temperature = mean(air_temp, na.rm = TRUE), avg_PAR = mean(PAR, na.rm = TRUE))

print(avg_temp_par)

