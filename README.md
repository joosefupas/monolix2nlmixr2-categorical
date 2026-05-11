# Categorical Monolix to nlmixr2 Bridge

Utilities for translating **binary categorical Monolix models** into an `rxode2` / `nlmixr2`-compatible workflow using `monolix2rx`, `babelmixr2`, and custom helper functions.

This repository provides a patched categorical import path for Monolix `.mlxtran` models so that `babelmixr2` / `nlmixr2` workflows can also handle binary categorical endpoint models from Monolix. The goal is to support quick prediction, simulation, diagnostic, and categorical VPC-style exploration scenarios.

## Suggested Repository Name

Recommended:

```text
monolix2nlmixr2-categorical
```

Alternative options:

```text
monolix-categorical-to-nlmixr2
babelmixr2-categorical-monolix
monolix2rx-categorical-bridge
categorical-monolix-nlmixr2
nlmixr2-monolix-categorical
```

Suggested GitHub description:

```text
Import Monolix binary categorical endpoint models into babelmixr2/nlmixr2 workflows for quick simulation, prediction, diagnostics, and categorical VPC exploration.
```

## Overview

The main workflow:

1. Translate a Monolix `.mlxtran` model using a categorical-aware wrapper.
2. Import the translated model into an `nlmixr2`-style result object.
3. Compute prediction outputs for binary categorical endpoints.
4. Generate a categorical VPC plot for \(P(Y = 1)\).

## Repository Files

```text
.
├── cat_00/
├── cat_00.mlxproperties
├── cat_00.mlxtran
├── cat_00_patched_modelpath.mlxproperties
├── cat_00_patched_modelpath.mlxtran
├── cat_function_1.R
├── log_emax_hill_dose_reg_2.txt
├── mad_bin.csv
└── s00_categorical_babel.R
```

### `s00_categorical_babel.R`

Main driver script for the example workflow.

It:

- Loads the required R packages
- Sources `cat_function_1.R`
- Translates the Monolix model using `monolix2rx_categorical()`
- Converts the translated object using `as.nlmixr2.categorical()`
- Generates a categorical VPC plot

### `cat_function_1.R`

Core helper file containing the categorical import, patching, conversion, and plotting utilities.

It includes:

- Safer Monolix data import helpers
- Header mismatch diagnostics
- Categorical endpoint parsing
- Binary categorical endpoint translation
- Categorical covariate code fixes
- Data recoding helpers
- Monolix model `.txt` path patching
- Monolix data attachment helpers
- Custom `as.nlmixr2.categorical()` support
- Categorical VPC plotting

### `cat_00.mlxtran`

Original Monolix project model file.

This file contains the Monolix project definition and references the external model text file and data file.

### `cat_00.mlxproperties`

Original Monolix project properties file associated with `cat_00.mlxtran`.

### `cat_00/`

Monolix project output directory.

This may contain Monolix-generated outputs, project metadata, estimation results, and other run artifacts used by the import workflow.

### `log_emax_hill_dose_reg_2.txt`

Monolix model text file referenced by the `.mlxtran` model.

This file is required because the `.mlxtran` project points to an external structural or statistical model definition.

### `mad_bin.csv`

Monolix data file used by the example categorical endpoint model.

This file is required for importing the model, preparing observed data, computing predictions, and generating categorical VPC simulations.

### `cat_00_patched_modelpath.mlxtran`

Automatically generated patched `.mlxtran` file.

This is created by `monolix2rx_categorical()` when the model `.txt` path in the original `.mlxtran` file cannot be resolved from its stored location. The patched version rewrites the model text path to a valid local path.

### `cat_00_patched_modelpath.mlxproperties`

Copied properties file associated with `cat_00_patched_modelpath.mlxtran`.

It is created so that the patched `.mlxtran` file has a matching `.mlxproperties` file available beside it.

## Required Monolix Files

The example requires the full Monolix project context, not only the R scripts.

At minimum, the following files are needed:

- `cat_00.mlxtran`
- `cat_00.mlxproperties`
- `log_emax_hill_dose_reg_2.txt`
- `mad_bin.csv`

The model text file and data file are necessary because the Monolix `.mlxtran` file references them externally. If these paths differ on another machine, `monolix2rx_categorical()` can create a patched `.mlxtran` file with corrected local paths.

## Requirements

The scripts require R and the following packages:

```r
install.packages(c(
  "ggplot2",
  "MASS"
))
```

Additional packages are required:

```r
# Install using the appropriate source for your environment
# depending on package availability
monolix2rx
rxode2
babelmixr2
```

Required libraries:

```r
library(monolix2rx)
library(rxode2)
library(babelmixr2)
library(ggplot2)
library(MASS)
```

## Quick Start

Place your Monolix project files in the repository directory, including:

- A `.mlxtran` file
- The matching `.mlxproperties` file
- The referenced Monolix model `.txt` file
- The referenced data file

For this example, the default model file is:

```r
mlxtranFile <- file.path("cat_00.mlxtran")
```

Run:

```r
source("s00_categorical_babel.R")
```

## Example Workflow

```r
# Load libraries
library(monolix2rx)
library(rxode2)
library(babelmixr2)
library(ggplot2)
library(MASS)

# Source helper functions
source("cat_function_1.R")

# Monolix model file
mlxtranFile <- file.path("cat_00.mlxtran")

# Step 1: Translate Monolix model
rx <- monolix2rx_categorical(
  mlxtranFile,
  modelTxtDir = getwd()
)

# Step 2: Import as nlmixr2-style object
result <- as.nlmixr2.categorical(rx)

# Inspect result
result
str(result, max.level = 1)

# Step 3: Generate categorical VPC plot
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
```

## Saving the VPC Plot

The plotting section includes an optional commented block for saving the VPC as a PNG file.

Example:

```r
dir.create("plots", showWarnings = FALSE, recursive = TRUE)

endpoint_label <- "LLDAS5"
timestamp <- format(Sys.time(), "%d-%m-%y_%H-%M-%S")

out_file <- file.path(
  "plots",
  paste0("categorical_vpc_", endpoint_label, "_", timestamp, ".png")
)

ggsave(
  filename = out_file,
  plot     = p,
  width    = 8,
  height   = 6,
  units    = "in",
  dpi      = 300
)
```

## Main Functions

### `monolix2rx_categorical()`

Categorical-aware wrapper around `monolix2rx::monolix2rx()`.

```r
rx <- monolix2rx_categorical(
  mlxtranFile,
  modelTxtDir = getwd()
)
```

Key features:

- Supports binary categorical endpoints
- Patches missing Monolix model `.txt` paths
- Adds safer Monolix data import diagnostics
- Applies categorical covariate code fixes
- Skips default validation for categorical endpoints when needed

### `as.nlmixr2.categorical()`

Converts the translated object into a categorical `nlmixr2`-style result object.

```r
result <- as.nlmixr2.categorical(rx)
```

The returned object includes:

```r
list(
  ui,
  predIpredData,
  etaObf,
  omega,
  theta,
  fullTheta,
  objf,
  method,
  observedColumn
)
```

### `plot_categorical_vpc()`

Generates a categorical VPC plot for binary outcomes.

```r
p <- plot_categorical_vpc(
  result,
  nBins = 10,
  nSim = 500,
  ci = 0.90
)
```

Important arguments:

| Argument | Description |
|---|---|
| `result` | Output from `as.nlmixr2.categorical()` |
| `nBins` | Number of time bins |
| `nSim` | Number of simulation replicates |
| `ci` | Prediction interval confidence level |
| `xScale` | Scale factor for time axis |
| `xOffset` | Offset applied before time scaling |
| `xlab` | X-axis label |
| `ylab` | Y-axis label |
| `title` | Plot title |
| `xTickBy` | Tick interval for x-axis |
| `rotateXLabels` | Rotate x-axis labels |

## Supported Models

Currently supported:

- Binary categorical endpoints
- Logit, probit, and log links for binary categorical models
- Monolix models that can be translated by `monolix2rx`
- Categorical covariate handling in selected translated expressions

Currently not supported:

- Categorical endpoints with more than two categories
- Count endpoints
- Event endpoints
- Generalized categorical models beyond the implemented binary case

## Notes and Limitations

This repository uses internal functions from `monolix2rx`, including namespace patching with `assignInNamespace()`.

Because of this:

- The workflow may depend on specific versions of `monolix2rx`
- Future package updates may break compatibility
- The code should be treated as experimental
- Results should be checked carefully against the original Monolix project

The helper functions include additional diagnostics for:

- Header mismatches between `.mlxtran` declarations and data files
- Missing model `.txt` references
- Non-numeric values introduced during import
- String-valued categorical covariate recoding

## Troubleshooting

### Missing model `.txt` file

If the `.mlxtran` file references a model `.txt` file that no longer exists at the original path, provide the folder containing the model text file:

```r
rx <- monolix2rx_categorical(
  mlxtranFile,
  modelTxtDir = getwd()
)
```

Or provide the exact file:

```r
rx <- monolix2rx_categorical(
  mlxtranFile,
  modelTxtFile = "log_emax_hill_dose_reg_2.txt"
)
```

### Missing data file

If the Monolix data file cannot be found, ensure that the data file referenced by the `.mlxtran` file is available in the project directory or update the data path in the Monolix project.

For this example, the required data file is:

```text
mad_bin.csv
```

### Header mismatch

If the data file header does not match the `.mlxtran` header declaration, the helper functions attempt limited automatic reconciliation.

Common causes include:

- Extra unnamed first column from CSV row names
- Missing `EMPTY` column expected by Monolix
- Column order mismatch
- Column names differing between data and `.mlxtran`

### Non-numeric categorical values

Some translated models expect numeric coding. If a column contains text categories, the helper functions attempt to create indicator variables where possible.

Check messages printed during import for recoding details.

## Reproducibility

The categorical VPC uses a random seed.

Default:

```r
seed = 12345
```

You can change it:

```r
p <- plot_categorical_vpc(
  result,
  seed = 2025
)
```

## Suggested Citation

If you use this repository in scientific work, cite the relevant R packages used in the workflow:

- `monolix2rx`
- `rxode2`
- `babelmixr2`
- `ggplot2`
- `MASS`

Also cite Monolix if the original model was developed using Monolix.

## License

This project is licensed under the MIT License.
