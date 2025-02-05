# One More Constrained Than The Other

**The data come from the European Social Survey**:
[Wave 4: 2008](https://doi.org/10.21338/ess4e04_5). 
[Wave 8: 2016](https://doi.org/10.21338/ess8e02_2). The versions I worked with are uploaded to the data folder via git LFS.
These two waves include an additional module on welfare attitudes, which is essential for the analysis.

**The Chapel Hill Expert Survey**:
[Chapel Hill Expert Survey Trend File (EVS 1999-2019)](https://doi.org/10.1016/j.electstud.2021.102420). The version I worked with is uploaded to the data folder via git LFS.


**The Manifesto Project**:
[Manifesto Project](https://doi.org/10.25522/manifesto.mpds.2016b). The version I worked with is uploaded to the data folder via git LFS.

For more information on the measurements used, please refer to the main paper.

**Note**: Before running the code to analyze the data and generate plots, you may run the **data transformation file**. However, the transformed data required for the main analysis is already updated in the data folder. Therefore, you may start with the main analysis file.

## Producing Figures from the Main Paper

To generate the figures for the main paper, refer to the following files:

- *Main_Analysis.R*: Contains code for **Figures 1, 2, 4, 5, 6** and most tables
- *Belief Networks.R*: Contains code for **Figure 3** and necessary information for **Table 3**
- *Diagrams.R*: Contains code for **Figure 7**

The transformation and analysis were performed using **R version 4.3.2 (2023-10-31 ucrt)**.

## Troubleshooting

If you encounter issues when attempting to reproduce the analysis, ensure that:

1. The working directory is set correctly.
2. All required data files are located in the appropriate folders (e.g., the `data` folder).