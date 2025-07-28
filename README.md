---
editor_options: 
  markdown: 
    wrap: 72
---

# Global Embodied Emissions of Digital Technologies

**Authors:** Janna Axenbeck, Stefanie Kunkel, Joris Blain, Francis
Charpentier

## 1. Project Overview

This repository contains the R code for the research paper titled
"Global Embodied Emissions of Digital Technologies: The Hidden 42%."

The analysis leverages the FIGARO (Full International and Global
Accounts for Research in Input-Output) database to perform an
Environmentally Extended Input-Output (EEIO) analysis.

The code is designed to be reproducible and flexible, supporting two
primary modes of operation.

------------------------------------------------------------------------

## 2. Setup and Execution

This project supports two modes for data sourcing:

1.  **Local Mode:** Reproduce the original paper's results using the
    **2023 FIGARO dataset**.
2.  **Online Mode:** Run the analysis on the **latest available data**
    fetched directly from Eurostat.

Follow the instructions for your desired mode.

### **Workflow A: Reproducing the Paper (Local Mode)**

Use this mode to get the exact results published in the paper. The
specific 2023 edition dataset is available upon request from Eurostat.

1.  **Request the Dataset from Eurostat:** To obtain the specific
    dataset used for this paper, you must send an email request directly
    to Eurostat (`ESTAT-IGA@ec.europa.eu`).

2.  **Organize the Data:** Once you receive the data files from Eurostat
    (likely in a compressed format), you must create a main folder on
    your computer (e.g., `FIGARO_ed23_data`).

3.  **Configure `master.R`:** Open the `master.R` script located in the
    project's root directory. Find the user configuration section at the
    top and **uncomment** the `data_directory` line. Replace the
    placeholder path with the **full, absolute path** to the folder you
    created in the previous step.

    ``` r
    # Example configuration:
    data_directory <- "C:/Users/YourName/Documents/FIGARO_ed23_data"
    ```

4.  **Run the Analysis:** Execute the `master.R` script. It will detect
    your local path and run the analysis using the 2023 data.

### **Workflow B: Using the Latest Data (Online Mode)**

Use this mode to run the same analytical model on the most recent public
data. The script will automatically fetch: FIGARO Input-Output Tables
(**2025 Edition**) GHG Emissions Footprints (**2024 Estimates**)

1.  **Configure `master.R`:** Open the `master.R` script. Ensure that
    the `data_directory` line at the top is **commented out** or
    deleted.

    ``` r
    # Correct configuration for online mode:
    # data_directory <- "C:/Users/YourName/Documents/FIGARO_ed23_data"
    ```

2.  **Run the Analysis:** Execute the `master.R` script. It will detect
    that no local path is set, create a temporary cache folder for
    storing downloaded files, and run the analysis on the latest online
    data.

## 3. Scripts Description

The analysis is modularized into several R scripts located in the
project's root directory. The `master.R` script executes them in the
correct sequence.

-   **`master.R` (Main Control Script):** The central entry point. It
    configures the analysis mode (local or online) and executes all
    other scripts in sequence.
-   **`01_aggregate_FIGARO_files.R` (Data Aggregation):** Aggregates raw
    FIGARO data. If running in online mode, this script handles the
    downloading of data from the Eurostat website.
-   **`02_deflation_procedure.R` (Price Deflation):** Constructs a price
    index table to convert economic data to constant prices.
-   **`03_fpt_computations.R` (Footprint Calculation):** The core of the
    analysis, performing the EEIO calculations.
-   **`04_main_results.R` (Visualization & Tables):** Generates the
    primary figures and tables for the paper.
-   **`05_decomposition_time_trends.R` (Decomposition Analysis):**
    Decomposes changes in emissions over time into their constituent
    drivers.
-   **`06_sankey.R` (Flow Visualization):** Creates Sankey diagrams to
    visualize the flow of embodied emissions.
-   **`07_uncertainty_analysis.R` (Sensitivity Analysis):** Assesses the
    robustness of the results using a Monte Carlo simulation.
-   **`utils.R` (Utility Functions):** A collection of helper functions
    used across multiple scripts.

## 4. License

This project is licensed under the GNU General Public License v3.0.

The GPL is a strong "copyleft" license, which means that any derivative works (e.g., modified or extended versions of this code that are distributed) must also be licensed under the same or a compatible license. We chose the GPL to ensure that any improvements or extensions to this research code remain open and accessible to the entire community.

A full copy of the license is available in the LICENSE file included in this repository. You can also read the full text online here:
https://www.gnu.org/licenses/gpl-3.0.en.html.

## Supplementary Analyses (Computationally Intensive)

The `master.R` script is configured to run the main analysis pipeline by
default. Two supplementary scripts are included but are **commented
out** due to their significant resource requirements. To run them, you
must manually uncomment the corresponding `source()` lines in
`master.R`.

-   **`06_sankey.R`:** This script generates the Sankey flow diagrams.
    Please be aware that it involves large matrix operations and has
    **high memory (RAM) requirements**. It may not run successfully on
    machines with limited hardware configurations.

-   **`07_uncertainty_analysis.R`:** This script performs the
    Monte-Carlo sensitivity analysis. As it involves inverting large
    matrices repeatedly for each draw, it is **extremely
    time-consuming**. Replicating a sufficient number of draws takes
    several hours to complete.

### Note on the `price_data` subfolder

The provided data package includes a price_data folder containing all
necessary price index extracts (from UN, NBS, and BLS sources) to run
the deflation procedure.

No manual download is required for this step. The comments within the
`02_deflation_procedure.R` script serve as documentation of the original
data sources and provide guidance for users who may wish to update these
files in the future. This folder is required for both Local and Online
workflows.

### Note on Code Maintenance and Future Data Releases

As of July 2025, the official FIGARO dataset has changed its perimeter,
modeling 4 additional countries, and its files are now hosted on the
CIRCABC platform. It is likely that corresponding emissions estimates
provided by Eurostat will also evolve.

This may cause the automated download functions (in "Online Mode") to
fail in the future. A user encountering such a situation is invited
either to contact the authors or to open a related issue on this GitHub
repository.
