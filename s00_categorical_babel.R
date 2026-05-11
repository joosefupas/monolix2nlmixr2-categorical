
# load libraries
library(monolix2rx)
library(rxode2)
library(babelmixr2)
library(ggplot2)
library(MASS)

# source
source("cat_function_1.R")
# do not run source twice ------------------------------

mlxtranFile <- file.path("cat_00.mlxtran")

# ══════════════════════════════════════════════════════════════
# SECTION 9: RUN EVERYTHING
# ══════════════════════════════════════════════════════════════

message("\n\u2550\u2550\u2550 Step 1: Translating Monolix model \u2550\u2550\u2550")
rx <- monolix2rx_categorical(
  mlxtranFile,
  modelTxtDir = getwd()
)

message("\n\u2550\u2550\u2550 Step 2: Importing as nlmixr2 object \u2550\u2550\u2550")
result <- as.nlmixr2.categorical(rx)
result
str(result, max.level = 1)

message("\n\u2550\u2550\u2550 Step 3: Generating categorical VPC plot \u2550\u2550\u2550")
p <- plot_categorical_vpc(
  result,
  nBins = 10,
  nSim = 50,
  ci = 0.95,
  xScale = 24,
  xlab = "Time (days)",
  ylab = "P(Y = 1)",
  xTickBy = 1,
  xTickStart = 0,
  rotateXLabels = TRUE,
  title = "Categorical VPC - Y"
)

print(p)

# dir.create("plots", showWarnings = FALSE, recursive = TRUE)
# 
# # Save in high quality
# endpoint_label <- "LLDAS5"
# timestamp <- format(Sys.time(), "%d-%m-%y_%H-%M-%S")
# 
# out_file <- file.path(
#   "plots",
#   paste0("categorical_vpc_", endpoint_label, "_", timestamp, ".png")
# )
# 
# ggsave(
#   filename = out_file,
#   plot     = p,
#   width    = 8,
#   height   = 6,
#   units    = "in",
#   dpi      = 300
# )
