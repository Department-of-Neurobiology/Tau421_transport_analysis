# Tau421 (TauC3) Transport Analysis 

## Fast Axonal Transport Analysis Tool

![Analysis Description](analysis_description.png?raw=true)
*Figure: Schematic representation of the analysis process and potential insights gained from the Tau421 Transport Analysis tool.*

## Overview
This is a post-processing tool for analyzing axonal transport in model neurons. It focuses on APP vesicle tracking using an output from the autoregressive motion algorithm provided by IMARIS (versions 9.2 and 9.5 tested). This tool processes output from IMARIS, specifically Excel files, to derive meaningful statistics such as speed and processivity of vesicle transport within neurons. It also examines vesicle stops and directional changes in transport, distinguishing between retrograde and anterograde movements.

This tool was initially developed and used in the research conducted by Christian Conze et al., as cited below.

## Citation
Conze C, Rierola M, Trushina NI, Peters M, Janning D, Holzer M, Heinisch JJ, Arendt T, Bakota L, Brandt R. Caspase-cleaved tau is senescence-associated and induces a toxic gain of function by putting a brake on axonal transport. Mol Psychiatry. 2022 Jul;27(7):3010-3023. doi: 10.1038/s41380-022-01538-2. Epub 2022 Apr 7. PMID: 35393558; PMCID: PMC9205779.

## Authors
[Nataliya Trushina] - *Python and R scripts and concept design* - [https://github.com/zari-ross]

[Christian Conze] - *IMARIS analysis and concept design*

[Roland Brandt] - *Concept proof*

## Features
- **Merging Tables for Collection Analysis:** Script (01a_merge_multiple_tables_track_analysis.py) merges tables to facilitate collection analysis.
- **Data Processing for Mean and Median Statistics:** Script (01b_axonal_transport_cell_means.R) processes data to obtain mean statistics per cell and pivots tables into the correct format.
- **Individual Cell Track Analysis:** Script (02a_2D_trajectory_analysis.R) loops through individual cell data, analyzing track information and changes. It can generate kymographs for each cell if the option is enabled.
- **Data Visualization:** Script (02b_postanalysis_plot_numbers_of_state_changes.R) processes the analyzed data to create plots for state changes.

## Requirements
Ensure you have read and write permissions for the Excel files. The solution to rewrite the files into the new files with enabled editing is inside the 02a_2D_trajectory_analysis script.

01. **Collection analysis:** Condition folders containing files with different parameters for all cells selected in IMARIS:

02. **Single-cell trajectory analysis:** Condition folders containing individual files for cells:
   - Excel tables with all parameters exported together.
   Ensure you have read and write permissions for these files.

## Usage
1. **Prepare Data Files:** Ensure your files are in the required format and placed in condition folders.
2. **Run Scripts:** Execute the scripts in the following order:
   - `01a_merge_multiple_tables_track_analysis.py` for merging tables.
   - `01b_axonal_transport_cell_means.R` for processing means and medians.
   - `02a_2D_trajectory_analysis.R` for individual cell track analysis.
   - `02b_postanalysis_plot_numbers_of_state_changes.R` for generating plots.

## Output
- **Statistics:** The tool outputs statistics like speed and processivity of vesicle transport.
- **Visualizations:** Generates kymographs, mean, and distribution plots, providing insights into vesicle stops, turns, and overall transport dynamics in neurons.

## Contributing
Contributions to the Tau421 Transport Analysis project are welcome. If you want to contribute, please fork the repository and submit a pull request with your proposed changes.

## Contact
Please open an issue in the GitHub repository for questions or feedback regarding this tool. 


