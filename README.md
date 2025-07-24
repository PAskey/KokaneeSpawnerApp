# KokaneeSpawnerApp

This Shiny web application provides interactive visualizations and tables for exploring kokanee salmon spawner data from Okanagan Lake tributaries. It supports stream-specific filtering, historical trend analysis, and side-by-side comparison of estimated peak spawner abundance and raw count data.

---

## 📊 Features

- **Visualize absolute spawner abundance** using stacked bar plots by year and stream.
- **Compare relative spawner trends** (percent of max peak) with an average trend line.
- **Filter by stream(s)** and custom year ranges.
- **Toggle between All Streams or Selected Streams** mode.
- **View data tables side-by-side**:
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
spawn_ests should contain at minimum:

YEAR, STREAM, peak_est, peak_percent

spawn_counts should contain:

YEAR, STREAM, DATE, COUNT, or similar daily record structure

---

## 📬 Contact
For questions, data submissions, or feature requests, please contact:

Paul Askey
, FFSBC
📧 paul.askey@gofishbc.com

---

## 📌 Notes
Ensure spawn_counts.rda is up-to-date if you're rebuilding data from source scripts (e.g., spwnr_data.R).

Blank rows in input or outputs are preserved for compatibility with field data collection formats.

---

