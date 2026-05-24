library("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# I load the packages I will use in the analysis.

################################################### 
library(meta)
library(readxl)


############################# Meta-analysis Anthropomorphism_Control (Non-anthropomorphic interface) #############################

data <- read_excel("Anthro_Control.xlsx", sheet = "Sheet1")

# I perform the meta-analysis 
meta_analysis <- metacont(
  n.e = data$N_A,           # Sample size of the anthropomorphic group
  mean.e = data$M_A,        # Mean of the anthropomorphic group
  sd.e = data$SD_A,         # Standard deviation of the anthropomorphic group
  n.c = data$N_C,           # Sample size of the control group
  mean.c = data$M_C,        # Mean of the control group
  sd.c = data$SD_C,         # Standard deviation of the control group
  studlab = data$Study_ID, # Study labels (Study ID)
  sm = "SMD",                # Summary measure: Standardized Mean Difference (Hedges' g)
  data = data,              # Data frame containing the data
  common = FALSE,        # Do not use fixed effect model
  random = TRUE        # Use random effects model
)

# Print the results
print(meta_analysis)


# Plot forest plot
tiff("Anthro_Control.tiff", width = 3500, height = 2500, units = "px", res = 300)
forest(meta_analysis, byvar = TRUE)
dev.off()

##################################### Funnel plot (publication bias) #########################

# Defining output path for the TIFF file
tiff("funnel_plot.tiff",
     width = 2000, height = 2000, res = 300)

# Set font and scaling
par(font.lab = 2,   
    font.axis = 2,  
    font.main = 2,  
    cex = 1.5)      

# Basic funnel plot
funnel(meta_analysis,
       main = "Funnel Plot",
       xlab = "Standardized Mean Difference (SMD)",
       ylab = "Standard Error",
       pch = 19,
       col = "blue",
       col.study = "blue",
       contour = NULL,
       backtransf = FALSE,
       comb.random = FALSE)

# Add horizontal reference line at 0
abline(h = 0, col = "grey40", lwd = 3, lty = 2)

# Add custom funnel contour lines (95% CI)
se_max <- max(meta_analysis$seTE)
se_seq <- seq(0, se_max, length.out = 100)

upper_limit <- meta_analysis$TE.random + qnorm(0.975) * se_seq
lower_limit <- meta_analysis$TE.random - qnorm(0.975) * se_seq

lines(upper_limit, se_seq, col = "black", lwd = 4, lty = 1)
lines(lower_limit, se_seq, col = "black", lwd = 4, lty = 1)

dev.off()


######################### Egger’s Test (for Funnel Plot Asymmetry) ###########################################

egger_test <- metabias(meta_analysis, method.bias = "linreg")  
print(egger_test)



################################## Perform leave-one-out sensitivity analysis ###############################
leave_one_out <- metainf(meta_analysis)

# Print the summary of the leave-one-out analysis
print(leave_one_out)

# Plot the influence of each study
tiff("leave_one_out_plot_A_C.tiff", width = 2700, height = 3000, res = 300)

# Set global font and scaling
par(font.lab = 2,     
    font.axis = 2,    
    font.main = 2,    
    cex = 1.5)        

# Plot the results of leave-one-out analysis
forest(leave_one_out,
       main = "Leave-One-Out Sensitivity Analysis",
       col.diamond = "red",
       col.diamond.lines = "black",
       col.bg = "blue",
       col.border = "black")

dev.off()




################################################################################################
###################### Subgroup meta-analyses ###################################################

################### Anthropomorphism Modality #############################

data <- read_excel("Anthro_Control.xlsx", sheet = "Sheet1")

# Perform the subgroup meta-analysis based on 'Anthropomorphism_Modality'
meta_analysis <- metacont(
  n.e = data$N_A,           
  mean.e = data$M_A,        
  sd.e = data$SD_A,         
  n.c = data$N_C,           
  mean.c = data$M_C,        
  sd.c = data$SD_C,         
  studlab = data$Study_ID, 
  sm = "SMD",                
  data = data,              
  common = FALSE,        
  random = TRUE,        
  subgroup = data$Anthropomorphism_Modality # Subgroup analysis based on Anthropomorphism_Modality
)

# Print the results
print(meta_analysis)

# Plot forest plot with subgroups
tiff("Anthropomorphism_Modality.tiff", width = 3500, height = 3500, units = "px", res = 300)
forest(meta_analysis, byvar = TRUE)
dev.off()

################### Cultural differences (suggestion for future studies) #############################
# Read the Excel file
data <- read_excel("Anthro_Control.xlsx", sheet = "Sheet1")

data <- data[!is.na(data$Cultural_Differences) & data$Cultural_Differences != "", ]
data <- data[data$Participant_Role == "Driver", ]
# Perform the subgroup meta-analysis based on 'Cultural-differences'
meta_analysis <- metacont(
  n.e = data$N_A,           
  mean.e = data$M_A,        
  sd.e = data$SD_A,         
  n.c = data$N_C,           
  mean.c = data$M_C,        
  sd.c = data$SD_C,         
  studlab = data$Study_ID, 
  sm = "SMD",                
  data = data,              
  common = FALSE,        
  random = TRUE,        
  subgroup = data$Cultural_Differences # Subgroup analysis based on Cultural_Differences
)

# Print the results
print(meta_analysis)

# Specify the path for saving the plot


# Plot forest plot with subgroups
tiff("Cultural_Differences.tiff", file_path, width = 3500, height = 3500, units = "px", res = 300)
forest(meta_analysis, byvar = TRUE)
dev.off()


################### Automation Levels #############################

data <- read_excel("Anthro_Control.xlsx", sheet = "Sheet1")

# ------------------Remove NA from the data ------------------#
na_count <- sum(is.na(data$Automation_Level))
data <- data[!is.na(data$Automation_Level), ]

# Sort data by Automation_Level in ascending order
data <- data[order(data$Automation_Level), ]

# Perform the subgroup meta-analysis based on 'Automation_Level'
meta_analysis <- metacont(
  n.e = data$N_A,           
  mean.e = data$M_A,        
  sd.e = data$SD_A,         
  n.c = data$N_C,           
  mean.c = data$M_C,        
  sd.c = data$SD_C,         
  studlab = data$Study_ID, 
  sm = "SMD",                
  data = data,              
  common = FALSE,        
  random = TRUE,        
  subgroup = data$Automation_Level # Subgroup analysis based on Automation_Level
)

# Print the results
print(meta_analysis)


# Plot forest plot with subgroups
tiff("Automation_Levels.tiff", width = 3500, height = 3500, units = "px", res = 300)
forest(meta_analysis, byvar = TRUE)
dev.off()


################### JBI Quality #############################
data <- read_excel("Anthro_Control.xlsx", sheet = "Sheet1")

# Perform the subgroup meta-analysis based on 'JBI_Quality'
meta_analysis <- metacont(
  n.e = data$N_A,           
  mean.e = data$M_A,        
  sd.e = data$SD_A,         
  n.c = data$N_C,           
  mean.c = data$M_C,        
  sd.c = data$SD_C,         
  studlab = data$Study_ID, 
  sm = "SMD",                
  data = data,              
  common = FALSE,        
  random = TRUE,        
  subgroup = data$JBI_Quality # Subgroup analysis based on JBI_Quality
)

# Print the results
print(meta_analysis)


# Plot forest plot with subgroups
tiff("JBI_Quality.tiff", width = 3500, height = 3500, units = "px", res = 300)
forest(meta_analysis, byvar = TRUE)
dev.off()

################### NDRT #############################

data <- read_excel("Anthro_Control.xlsx", sheet = "Sheet1")

# ------------------Remove NA from the data ------------------#
na_count <- sum(is.na(data$NDRT))
data <- data[!is.na(data$NDRT), ]


# Perform the subgroup meta-analysis based on 'NDRT'
meta_analysis <- metacont(
  n.e = data$N_A,           
  mean.e = data$M_A,        
  sd.e = data$SD_A,         
  n.c = data$N_C,           
  mean.c = data$M_C,        
  sd.c = data$SD_C,         
  studlab = data$Study_ID, 
  sm = "SMD",                
  data = data,              
  common = FALSE,        
  random = TRUE,        
  subgroup = data$NDRT # Subgroup analysis based on NDRT
)

# Print the results
print(meta_analysis)

# Plot forest plot with subgroups
tiff("NDRT.tiff", width = 3500, height = 3500, units = "px", res = 300)
forest(meta_analysis, byvar = TRUE)
dev.off()



################### Role of Participants  #############################
data <- read_excel("Anthro_Control.xlsx", sheet = "Sheet1")

# Sort data by Participant_Role in ascending order
data <- data[order(data$Participant_Role), ]

# Perform the subgroup meta-analysis based on 'NDRT'
meta_analysis <- metacont(
  n.e = data$N_A,           
  mean.e = data$M_A,        
  sd.e = data$SD_A,         
  n.c = data$N_C,           
  mean.c = data$M_C,        
  sd.c = data$SD_C,         
  studlab = data$Study_ID, 
  sm = "SMD",                
  data = data,              
  common = FALSE,        
  random = TRUE,
  subgroup = data$Participant_Role # Subgroup analysis based on Participant_Role
)

# Print the results
print(meta_analysis)

# Specify the path for saving the plot
file_path_tiff <- "Participant_Role.tiff"

# Plot forest plot with subgroups
tiff(file_path_tiff, width = 3500, height = 3500, units = "px", res = 300)
forest(meta_analysis, byvar = TRUE)
dev.off()


################### Driving simulation #############################
# Read the Excel file
data <- read_excel("Anthro_Control.xlsx", sheet = "Sheet1")


# Perform the subgroup meta-analysis based on 'Driving_Simulation'
meta_analysis <- metacont(
  n.e = data$N_A,           
  mean.e = data$M_A,        
  sd.e = data$SD_A,         
  n.c = data$N_C,           
  mean.c = data$M_C,        
  sd.c = data$SD_C,         
  studlab = data$Study_ID, 
  sm = "SMD",                
  data = data,              
  common = FALSE,        
  random = TRUE,
  subgroup = data$Driving_Simulation # Subgroup analysis based on Driving_Simulation
)

# Print the results
print(meta_analysis)


# Plot forest plot with subgroups
tiff("Driving_Simulation.tiff", width = 3500, height = 3500, units = "px", res = 300)
forest(meta_analysis, byvar = TRUE)
dev.off()



