# Green Space Configuration and Pluvial Flooding

## Comparing Delft and Xi’an using FWEI derived flood indicators and landscape metrics

CC BY 4.0

The report of this project is available online:

https://applied-spatial-analytics.github.io/create-your-report-groupc/

## Context

Urban green space is often discussed as a way to reduce pluvial flood risk. However, the relationship between green space and flooding does not only depend on the total amount of green area. The spatial configuration of green space, such as patch size, compactness, connectivity, and fragmentation, may also influence how surface water appears across an urban landscape.

This project investigates this relationship by comparing Delft, the Netherlands, and Xi’an, China. The two cities represent different urban and environmental contexts. Xi’an was analysed around a pluvial flood event in August 2023, while Delft was used as a non-event comparison case. Because directly comparable pluvial flood-depth maps were not available for both cities, the project used the Flood/Water Extraction Index (FWEI) as a shared proxy for detected surface-water change.

Both cities were analysed within a 10 km by 10 km study area using a shared grid-based workflow. Green-space metrics, FWEI-derived flood indicators, and Digital Elevation Model (DEM) variables were calculated per 100 m grid cell. These metrics were then used for correlation analysis, metric selection, and combined typology construction.

## Research Questions

**Main question:**
How does the spatial configuration of green spaces influence pluvial flooding in urban areas such as Delft and Xi’an?

**Sub-questions:**

1. How can green-space configuration and flood-related surface-water patterns be measured and compared between Delft and Xi’an?

2. What spatial patterns can be identified when green-space, flood-related, and topographic indicators are combined?

3. How do the identified patterns differ between Delft and Xi’an, and what do they suggest for urban flood-related planning?

## Analysis Method

The analysis used a grid-based spatial workflow. Each city was represented by a 10 km by 10 km study area divided into 100 m by 100 m grid cells.

The main steps were:

1. Prepare and clip the input datasets for Delft and Xi’an.
2. Convert green-space layers into binary green rasters.
3. Calculate FWEI before-and-after rasters from Sentinel-2 imagery.
4. Derive continuous FWEI change and binary flood masks.
5. Calculate green-space, flood-pattern, and DEM variables per grid cell.
6. Check metric redundancy and select a final metric set.
7. Run correlation analysis between green-space metrics and flood indicators.
8. Construct a shared green+flood+DEM typology using k-means clustering.
9. Render the final report with Quarto.

## Authors

| Name           | Student number | Institution             | Email                                                                 |
| -------------- | -------------: | ----------------------- | --------------------------------------------------------------------- |
| Hassan Osman   |        5169550 | TU Delft, MSc Geomatics | [H.Osman@student.tudelft.nl](mailto:H.Osman@student.tudelft.nl)       |
| Akhil Veeranki |        6305105 | TU Delft, MSc Geomatics | [A.veeranki@student.tudelft.nl](mailto:A.veeranki@student.tudelft.nl) |
| Daniel Marx    |        4624475 | TU Delft, MSc Geomatics | [d.marx@student.tudelft.nl](mailto:d.marx@student.tudelft.nl)         |

## Repository Structure

```text
.
├── .github/                         # GitHub Classroom and workflow files
├── data/                            # Data folders used by the scripts
│   ├── context/                     # Context layers used for mapping
│   ├── processed/                   # Processed input files needed for the R scripts
│   └── results/                     # Output tables, metric files, and typology layers
├── docs/                            # Rendered Quarto website
├── figures/                         # Figures used in the report
│   ├── discussion/
│   ├── preliminary/
│   └── results/
├── sections/                        # Quarto report chapters
├── src/                             # Main R scripts
│   ├── 01_green_metrics_delft.R
│   ├── 02_green_metrics_xian.R
│   ├── 03_make_maps_and_plots.R
│   ├── 04_metric_redundancy_check.R
│   └── 05_shared_combined_typologies.R
├── index.qmd                        # Report landing page
├── references.bib                   # References
├── _quarto.yml                      # Quarto settings
├── README.md
└── report.qmd
```

## Data

The data needed to run the scripts are available in the project SURFdrive folder:

https://surfdrive.surf.nl/s/aqgEpftfT7bXpX4?dir=%2Fgroup-c

The processed input files should be placed in the same folder structure as used in the repository, especially:

```text
data/processed/
data/context/
```

The scripts write their outputs to:

```text
data/results/
figures/results/
```

## Software

Software used during the project:

* QGIS 3.x for data preparation, FWEI calculation, raster clipping, flood-mask creation, and visual checks.
* R 4.5.3 for metric calculation, correlation analysis, typology construction, and map/plot generation.
* Quarto 1.x for rendering the report.
* GitHub Pages for publishing the rendered report.

Main R packages:

```text
terra
sf
landscapemetrics
dplyr
readr
ggplot2
grid
```

## License

This work is licensed under a Creative Commons Attribution 4.0 International License.
