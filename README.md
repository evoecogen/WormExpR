# WormExpR: WormExp Database Query Cleaning and Visualization

## Description

Provides functions for cleaning and visualizing [WormExp](https://wormexp.zoologie.uni-kiel.de/wormexp/) query results, internal functions for database maintenance.

## Installation

The package is best installed using the Bioconductor installer:

```         
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("evoecogen/WormExpR")
```

## Usage

### Exported Functions

1.  Clean WormExp query results:

```         
clean_results_table(
  input_file = "WormExp/results/ResultsTable.txt",
  output_path = "WormExp/results"
)
```

2.  Compare cleaned query results:

```         
compare_results(
  input_path = "WormExp/results",
  output_path = "WormExp/figures"
)
```

### Internal Functions

Internal functions are used for WormExp database maintenance and are not intended for public use. If necessary, they can be accessed using the triple colon operator `:::`, e.g:

```
WormExpR:::clean_reference(
  path = "WormExp/GitHub/05_QualityManagement/WormSource_v2.0/"
)
```
