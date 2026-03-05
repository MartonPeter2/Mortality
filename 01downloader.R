# ==============================================================================
# HMD DATA DOWNLOADER & PIVOT TOOL
# Target: Life expectancy at birth (e0) for all countries (inc. DE/UK regions)
# Method: Using 'data.table' for efficiency
# ==============================================================================
# 0. Configuration
reload_from_sketch=F
filenamedb="C:/C/Mortality/mortality_database.tsv"    #irrelevant if reloaded
filename70="C:/C/Mortality/mortality_database_y70.tsv"#irrelevant if reloaded
filename75="C:/C/Mortality/mortality_database_y75.tsv"#irrelevant if reloaded
filename80="C:/C/Mortality/mortality_database_y80.tsv"#irrelevant if reloaded

# 1. SETUP & LIBRARIES
# ------------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
 data.table,   # High-performance data manipulation
 HMDHFDplus,   # Interface to Human Mortality Database
 rstudioapi    # Secure password entry
)

if(reload_from_sketch){

# 2. AUTHENTICATION
# ------------------------------------------------------------------------------
# NOTE: Never hardcode your password in scripts committed to GitHub.
# Define your email here or input it interactively.
user_email <- readline(prompt = "Your email address to mortality.org account: ")

# Request password via a secure popup window
user_pass  <- rstudioapi::askForPassword("Password for mortality.org account:")

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
 cat("Adatok feldolgozása:", code, "...\n")
 
 tryCatch({
  # A) downloading Woman (fltper = Female Life Table)
  dt_f <- setDT(readHMDweb(code, "fltper_1x1", user_email, user_pass))
  val_f <- dt_f[, .(Year, Age, ex, qx, mx)] # Itt választhatsz: 'ex', 'qx', 'mx'
  setnames(val_f, "ex", "F")  # simply rename to F (this is to be used)
  setnames(val_f, "qx", "Fq") # simply rename to Fq(not usedÖ
  setnames(val_f, "mx", "Fm") # simply rename to Fm(not used)
  
  # B) Downloading Man (mltper = Male Life Table)
  dt_m <- setDT(readHMDweb(code, "mltper_1x1", user_email, user_pass))
  val_m <- dt_m[, .(Year, Age, ex, qx, mx)]
  setnames(val_m, "ex", "M")  # simply rename to M (this is to be used)
  setnames(val_m, "qx", "Mq") # simply rename to Mq(not usedÖ
  setnames(val_m, "mx", "Mm")## simply rename to Mm(not used)
  
  # C) MERGE
  # Mearging by year and age
  merged <- merge(val_f, val_m, by = c("Year","Age"), all = TRUE)
  merged[, Country := code]
  
  return(merged)
  
 }, error = function(e) {
  warning(paste("Hiba vagy hiányzó adat:", code))
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
# 
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
# fwrite(y70,"C:/C/Mortality/mortality_database_y70.tsv",sep="\t",dec=",")
# fwrite(y75,"C:/C/Mortality/mortality_database_y75.tsv",sep="\t",dec=",")
# fwrite(y80,"C:/C/Mortality/mortality_database_y80.tsv",sep="\t",dec=",")
# fwrite(long_dt[Age == 0 & ((70 <= e0 & e0 < 71) | (75 <= e0 & e0 < 76) | (80 <= e0 & e0 < 81)), .N, by = Country][order(Country)],"C:/C/Mortality/orszagok.tsv",sep="\t",dec=",")

}else{
full_dt<-fread(filenamedb,sep="\t",dec=",")
    y70<-fread(filename70,sep="\t",dec=",")
    y75<-fread(filename75,sep="\t",dec=",")
    y80<-fread(filename80,sep="\t",dec=",")
}

#Question: what's about child mortality? (relevant if gerontology is studied)

death_probs_70 <- as.data.frame(y70[,c(1,3,4)])

size70 <- length(death_probs_70[,1])

death_probs_70[paste0("q", 0:110)] <- NA

# --- Helper Function to Process Each Bucket (70, 75, 80) ---
process_mortality_cluster <- function(y_subset, full_data, label) {
 
 # 1. Initialize data frame with metadata
 death_probs <- as.data.frame(y_subset[, c("Year", "Country", "Sex")])
 
 # Add columns for ages 0 to 110
 death_probs[paste0("q", 0:110)] <- NA
 
 # 2. Fill the matrix from full_dt
 for (i in 1:nrow(death_probs)) {
  curr_year <- as.numeric(death_probs$Year[i])
  curr_cntr <- as.character(death_probs$Country[i])
  sex_label <- as.character(death_probs$Sex[i]) # Extract as "F" or "M"
  
  # Locate indices in full_dt
  indices <- which(full_data$Year == curr_year & full_data$Country == curr_cntr)
  
  if (length(indices) > 0) {
   # Check the actual labels created in your 'long_dt' melt step
   if (sex_label == "F") { 
    # Female probability of death (Column 4 in your original full_dt)
    death_probs[i, 4:114] <- as.numeric(full_data[indices, 4][[1]])
   } else if (sex_label == "M") { 
    # Male probability of death (Column 7 in your original full_dt)
    death_probs[i, 4:114] <- as.numeric(full_data[indices, 7][[1]])
   }
  }
 }
 
 # 3. Prepare Input for Clustering
 valid_rows <- complete.cases(death_probs[, 4:114])
 clean_df <- death_probs[valid_rows, ]
 
 if(nrow(clean_df) < 5) stop(paste("Not enough data points in bucket", label))
 
 # Ages 12 to 110
 input_data <- as.matrix(clean_df[, 16:114]) 
 
 # --- DATA SANITIZATION (Critical Fix) ---
 # K-means and Scale fail if there are zeros or NAs
 input_data[input_data <= 0] <- 0.0000001 # Replace 0 with a tiny value
 input_data[is.na(input_data)] <- 0.0000001 # Replace any missed NAs
 
 # --- CLUSTERING ---
 set.seed(123)
 
 # Use log-transformed data for clustering? 
 # Demographic shapes are much clearer on a log scale
 log_input <- log(input_data)
 
 # Standardize the log-data
 std_input <- scale(log_input)
 
 # Final check for non-finite values created by scale()
 std_input[!is.finite(std_input)] <- 0 
 
 km_std <- kmeans(std_input, centers = 2, nstart = 100)
 
 # --- PLOTTING ---
 c1_mean <- colMeans(input_data[km_std$cluster == 1, , drop=FALSE])
 c2_mean <- colMeans(input_data[km_std$cluster == 2, , drop=FALSE])
 
 plot(log(c1_mean), type="l", col="blue", ylim=c(-10, 0), lwd=2,
      main=paste("Mortality Profile (Log Scale):", label), 
      xlab="Age (starting at 12)", ylab="log(qx)")
 lines(log(c2_mean), col="red", lwd=2)
 grid()
 
 return(list(data = clean_df, km = km_std))
}

# --- EXECUTION ---

# Process 70s bucket
res70 <- process_mortality_cluster(y70, full_dt, "70-71")

# Process 75s bucket
res75 <- process_mortality_cluster(y75, full_dt, "75-76")

# Process 80s bucket
res80 <- process_mortality_cluster(y80, full_dt, "80-81")

# Example: Look at countries in Cluster 1 for the 80s bucket
print(res80$data[res80$km$cluster == 1, 1:3])
