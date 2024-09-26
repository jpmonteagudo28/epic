#-----------------------------------------------#
# Flopatitis Procrastinosa Synthetic Data
#-----------------------------------------------#
# Set seed for reproducibility
set.seed(123)

# Total number of employees
total_employees <- 400

# Total number of employees who developed the disease
total_cases <- 130  # Includes both new and previously existing cases

# New cases during the year
new_cases <- 85
new_cases_exposed <- 63  # Among exposed employees
new_cases_unexposed <- 22  # Among unexposed employees

# Exposure status
exposed_employees <- 250  # Exposed employees (Deadline_Pressure == "Yes")
unexposed_employees <- 150  # Unexposed employees (Deadline_Pressure == "No")

# Generate random disease onset times for new cases between 1 and 12 months
# Existing cases already had the disease before the follow-up, marked as onset = 0
disease_onset_new_exposed <- sample(1:12, new_cases_exposed, replace = TRUE)
disease_onset_new_unexposed <- sample(1:12, new_cases_unexposed, replace = TRUE)

# Generate exposure status
exposure_status <- c(rep("Yes", exposed_employees), rep("No", unexposed_employees))

# Create the disease onset vector for all employees
# Set onset to 0 for non-cases and the previously existing cases (before follow-up)
disease_onset <- rep(NA, total_employees)
disease_onset[1:new_cases_exposed] <- disease_onset_new_exposed
disease_onset[(exposed_employees + 1):(exposed_employees + new_cases_unexposed)] <- disease_onset_new_unexposed

# Generate random disease onset times for the 45 previously existing cases (before follow-up)
# Assuming these cases are distributed equally between exposed and unexposed
existing_cases_exposed <- abs(45 - new_cases_unexposed)
existing_cases_unexposed <- abs(45 - new_cases_exposed)

# Add previous cases as onset = 0 (already sick before the year began)
disease_onset[1:existing_cases_exposed] <- 0
disease_onset[(exposed_employees + 1):(exposed_employees + existing_cases_unexposed)] <- 0

# Employee IDs
employee_id <- 1:total_employees

# Combine all into a data frame
flopatitis <- data.frame(ID = employee_id,
                              Disease_Onset = disease_onset,
                              Deadline_Pressure = exposure_status)

save(flopatitis,file = "data/flopatitis.rda")
