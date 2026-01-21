# Analytics_Report_MOOC

## Project Description
This project analyses learner engagement and re-engagement behaviour in the **FutureLearn Cyber Security MOOC**.
It uses **two CRISP-DM investigation cycles**:
- **Cycle 1:** Engagement decay + inactivity onset (≥ 7 consecutive inactive days)
- **Cycle 2:** Re-engagement probability after inactivity, comparing completers vs non-completers

---

## Directory Map
- `data/` : raw datasets (.csv files for enrolments and step-activity across course runs)
- `munge/` : preprocessing scripts that clean and combine raw datasets into master datasets
- `cache/` : cached processed datasets created by ProjectTemplate
- `config/` : ProjectTemplate configuration files
- `main_report.Rmd` : main analysis report (RMarkdown)
- `Analytics_Report_MOOC.Rproj` : RStudio project file
- `renv.lock` : locked dependencies for reproducibility

---
## Setup Instructions
1. Open the project in RStudio using:
   - `Analytics_Report_MOOC.Rproj`

2. Restore package environment using renv:
3. 
```r
install.packages("renv")
renv::restore()
```
##  Run the project
1. Open main_report.Rmd
2. Click Knit to generate the PDF report

## Submission
Final submission prepared on 16 Jan 2026.
