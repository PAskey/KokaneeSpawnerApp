# KokaneeSpawnerApp

This Shiny web application provides interactive visualizations and tables for exploring kokanee spawner data from Okanagan tributaries and shorelines. It supports ecotype and spawn site filtering, historical trend analysis, and comparison of specific sites to lake-wide trends.

Abundace estimates use the peak count method in Askey et al. 2023 (CJFAS).

---

## 📊 Features

- **Visualize absolute spawner abundance** using stacked bar plots by year and site.
- **Compare relative spawner trends** (percent of max peak) with ecotype-specific trend lines:
  - **Solid black** for stream-spawning kokanee
  - **Dashed black** for shore-spawning kokanee
- **Filter by Ecotype (Shore, Stream, or All)** to focus analysis.
- **Filter by Site(s)** and custom year ranges.
- **Toggle between All Sites or Selected Sites** mode.
- **View data tables vertically stacked**:
  - Estimated peak spawner abundance (`spawn_ests`)
  - Raw daily count observations (`spawn_counts`)
- **Download filtered data** for further analysis or reporting.
- **Download a data entry template** for standardizing new spawner count submissions.

---

## 📁 Data Files

Ensure the following files exist in a `data/` subfolder relative to the app directory:

| File                             | Description                                           |
|----------------------------------|-------------------------------------------------------|
| `spawn_ests.rda`                 | RDA file containing the summarized peak spawner estimates (`spawn_ests`) |
| `spawn_counts.rda`              | RDA file containing raw daily count data (`spawn_counts`) |
| `spawner_count_template.xlsx`   | Excel file template for standardized count entry      |

---

## 🗃️ Data Overview

`spawn_ests` should contain at minimum:

YEAR, STREAM, QUADRANT, ECOTYPE, peak_est, peak_percent

`spawn_counts` should contain:

YEAR, STREAM, QUADRANT, ECOTYPE, DATE, COUNT

Each record will automatically be assigned a unified `SITE` variable based on `STREAM` (for stream spawners) or `QUADRANT` (for shore spawners).

---

## 📬 Contact

For questions, data submissions, or feature requests, please contact:

**Paul Askey**, FFSBC  
📧 paul.askey@gofishbc.com

---

## 📌 Notes

Ensure `spawn_counts.rda` is up-to-date if you're rebuilding data from source scripts (e.g., `spwnr_data.R`).

TREND lines are automatically filtered based on the selected ecotype. Blank rows in input or outputs are preserved for compatibility with field data collection formats.

---
