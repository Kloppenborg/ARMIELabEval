library(here)
# Define date . start time, end time, RH, temp and the source used to generate particles.
date <- "20250522"
start_time <- "11.00"
end_time <- "15:30"
chamber_RH <- 40
chamber_temp <- 20
source <- "clean_air"  #Specify the source used to generate particles
aps_seperator <- ";"

# Define paths using the specified date
folder_path <- paste0(here("lab_test", "data", date))
reference_dusttrak <- paste0(folder_path, "/reference/", date, " - ", source, "_dusttrak.csv")
reference_OPS <- paste0(folder_path, "/reference/", date, " - ", source, "_OPS.csv")
reference_APS_mass <- paste0(folder_path, "/reference/", date, " - ", source, "_mass_concentration_APS.TXT")
reference_SMPS_mass <- paste0(folder_path, "/reference/", date, " - ", source, "_mass_concentration_SMPS.TXT")
reference_APS_number <- paste0(folder_path, "/reference/", date, " - ", source, "_number_concentration_APS.TXT")
reference_SMPS_number <- paste0(folder_path, "/reference/", date, " - ", source, "_number_concentration_SMPS.TXT")

output_folder <- paste0(here("clean_data", date))

# Ensure the output folder exists
dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)

#source the functions from R-script
source(here("scripts", "functions_read_data.R"))

###################
###Armie nodes ####
#
# armie_data <- data
#
# # Save ARMIE data
# assign(paste0("armie_", date), armie_data)
# save(list = paste0("armie_", date), file = paste0(output_folder, "/armie_", date, ".rda"))

#################
### Dusttrak ####

DustTrak_data <- read_dusttrak_data(reference_dusttrak)
assign(paste0("dusttrak_", date, source), DustTrak_data)
save(list = paste0("dusttrak_", date, source), file = paste0(output_folder, "/dusttrak_", date, source, ".rda"))

############
### OPS ####

OPS_data <- read_ops_data(reference_OPS)
assign(paste0("OPS_", date, source), OPS_data)
save(list = paste0("OPS_", date, source), file = paste0(output_folder, "/OPS_", date, source, ".rda"))

##############
#### SMPS #### (Mass)

SMPS_data <- read_smps_data(reference_SMPS_mass)
assign(paste0("SMPS_mass_", date, source), SMPS_data)
save(list = paste0("SMPS_mass_", date, source), file = paste0(output_folder, "/SMPS_mass_", date, source, ".rda"))

#############
### APS ##### (Mass)

APS_data <- read_aps_data(reference_APS_mass)
assign(paste0("APS_mass_", date, source), APS_data)
save(list = paste0("APS_mass_", date, source), file = paste0(output_folder, "/APS_mass_", date, source, ".rda"))


################################
##### Number concentrations ####
################################

##############
### SMPS ##### (Number)

SMPS_number_data <- read_smps_number_data(reference_SMPS_number)
assign(paste0("SMPS_number_concentration_", date, source), SMPS_number_data)
save(list = paste0("SMPS_number_concentration_", date, source), file = paste0(output_folder, "/SMPS_number_concentration_", date, source, ".rda"))

#############
#### APS ##### (Number)

APS_number_data <- read_aps_number_data(reference_APS_number)
assign(paste0("APS_number_concentration_", date, source), APS_number_data)
save(list = paste0("APS_number_concentration_", date, source), file = paste0(output_folder, "/APS_number_concentration_", date, source, ".rda"))


#########################################
### Align SMPS & APS: PN (number) #######
#########################################

merge_and_save_all_data(SMPS_data, APS_number_data, SMPS_number_data, DustTrak_data)
