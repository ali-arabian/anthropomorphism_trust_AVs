library("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# I load the packages I will use in the analysis.

################################################### 
library(meta)
library(readxl)


############################# Meta-analysis Anthropomorphism vs Baseline (i.e., the lack of interface) #############################
# I read the Excel file
data <- read_excel("Anthro_Base.xlsx", sheet = "Sheet1")

# I perform the meta-analysis 
meta_analysis <- metacont(
  n.e = data$N_A,           # Sample size of the Anthropomorphic group
  mean.e = data$M_A,        # Mean of the Anthropomorphic group
  sd.e = data$SD_A,         # Standard deviation of the Anthropomorphic group
  n.c = data$N_B,           # Sample size of the Baseline group
  mean.c = data$M_B,        # Mean of the Baseline group
  sd.c = data$SD_B,         # Standard deviation of the Baseline group
  studlab = data$Study_ID, # Study labels (Study ID)
  sm = "SMD",                # Summary measure: Standardized Mean Difference (Hedges' g)
  data = data,              # Data frame containing the data
  common = FALSE,        # Do not use fixed effect model
  random = TRUE        # Use random effects model
)

# Print the results
print(meta_analysis)


# Plot forest plot 
tiff("Anthro_Base.tiff", width = 3500, height = 2500, units = "px", res = 300)
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
tiff("leave_one_out_plot.tiff", width = 2700, height = 3500, res = 300)

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

