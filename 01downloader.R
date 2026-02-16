# ==============================================================================
# HMD DATA DOWNLOADER & PIVOT TOOL
# Target: Life expectancy at birth (e0) for all countries (inc. DE/UK regions)
# Method: Using 'data.table' for efficiency
# ==============================================================================

# 1. SETUP & LIBRARIES
# ------------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
 data.table,   # High-performance data manipulation
 HMDHFDplus,   # Interface to Human Mortality Database
 rstudioapi    # Secure password entry
)

# 2. AUTHENTICATION
# ------------------------------------------------------------------------------
# NOTE: Never hardcode your password in scripts committed to GitHub.
# Define your email here or input it interactively.
user_email <- rstudioapi::askForPassword("Please enter your mortality.org user email:") 

# Request password via a secure popup window
user_pass  <- rstudioapi::askForPassword("Please enter your mortality.org password:")

# 3. DEFINE COUNTRY LIST
# ------------------------------------------------------------------------------
# Fetch the list of all available countries/regions from HMD.
# This automatically includes:
# - DEUTNP (Germany Total), DEUTE (East), DEUTW (West)
# - GBR_NP (UK Total), GBRTENW (Eng/Wales), GBR_SCO (Scotland), GBR_NIR (N. Ireland)
all_codes <- getHMDcountries()

# 4. DOWNLOAD FUNCTION
# ------------------------------------------------------------------------------
# Helper function to download e0 data for a single country
fetch_life_table_metric <- function(code) {
 cat("Processing data:", code, "...\n")
 
 tryCatch({
  # A) Downloading FEMALES (fltper = Female Life Table)
  dt_f <- setDT(readHMDweb(code, "fltper_1x1", user_email, user_pass))
  val_f <- dt_f[, .(Year, Age, ex, qx, mx)] # Here you can choose: 'ex', 'qx', 'mx'
  setnames(val_f, "ex", "F")  # Rename simply to "F"
  setnames(val_f, "qx", "Fq") # Rename simply to "Fq"
  setnames(val_f, "mx", "Fm") # Rename simply to "Fm"
  
  # B) Downloading MALES (mltper = Male Life Table)
  dt_m <- setDT(readHMDweb(code, "mltper_1x1", user_email, user_pass))
  val_m <- dt_m[, .(Year, Age, ex, qx, mx)]
  setnames(val_m, "ex", "M")  # Rename simply to "M"
  setnames(val_m, "qx", "Mq") # Rename simply to "Mq"
  setnames(val_m, "mx", "Mm") # Rename simply to "Mm"
  
  # C) COMBINING (MERGE)
  # Merge the two based on Year (and Age)
  merged <- merge(val_f, val_m, by = c("Year","Age"), all = TRUE)
  merged[, Country := code]
  
  return(merged)
  
 }, error = function(e) {
  warning(paste("Error or missing data:", code))
  return(NULL)
 })
}

# 5. BATCH DOWNLOAD
# ------------------------------------------------------------------------------
# Loop through all codes and fetch data.
# Note: This might take 1-2 minutes depending on connection speed.
data_list <- lapply(all_codes$CNTRY, fetch_life_table_metric)

# # Manual correction
 # missing_countries <- c("KOR")
 # data_list <- c(data_list,lapply(missing_countries, fetch_life_table_metric))
#data_list[[which(all_codes$CNTRY == "KOR")]] <- fetch_life_table_metric("KOR")

# Combine all list elements into one single data.table
full_dt <- rbindlist(data_list, use.names = TRUE, fill = TRUE)

# # Check
# setdiff(full_dt[,Country], all_codes$CNTRY)
# setdiff(all_codes$CNTRY,(full_dt[,Country]))
# fwrite(full_dt,"C:/C/Mortality/mortality_database.tsv",sep="\t",dec=",")
# full_dt<-fread("C:/C/Mortality/mortality_database.tsv",sep="\t",dec=",")
# 6. DATA TRANSFORMATION (PIVOT)
# ------------------------------------------------------------------------------

# Step A: Melt to Long Format (combine Male/Female columns into one 'Sex' column)
long_dt <- melt(full_dt, 
                id.vars = c("Year", "Age", "Country"), 
                measure.vars = c("F", "M"), 
                variable.name = "Sex", 
                value.name = "e0")

y70 <- long_dt[Age==0&70<=e0&e0<71]#259
y75 <- long_dt[Age==0&75<=e0&e0<76]#366
y80 <- long_dt[Age==0&80<=e0&e0<81]#303
