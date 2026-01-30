# Assignment 1: Project Setup & Data Loading

**Due Date:** Feb 10 at 9.30am

## Learning Objectives

The goal of this assignment is to ease you into your workflow and setup. The actual coding is still minimal. By the end of this assignment, you should: - Set up a reproducible R project with proper structure - Use Git for version control - Use relative paths and organize your code - Document your work clearly

## Background

You'll be working with NYC Stop, Question, and Frisk (SQF) data from 2006-2012. This dataset contains records of police stops. Your task is to transform messy, repetitive code into a well-structured, reproducible project.

## Part 1: Project Setup

### 1.1 Create Project Structure

Create a new folder called `sqf-analysis` with the following structure:

```         
sqf-analysis/
├── data/
│   └── raw/
├── R/
├── scripts/
├── output/
├── README.md
└── .gitignore
```

### 1.2 Initialize Git Repository

1.  Initialize a Git repository in your project folder
2.  Create a `.gitignore` file that excludes:
    -   `data/` (large data files)
    -   `.Rproj.user/`
    -   `.Rhistory`
    -   `.RData`
3.  Make your first commit with message: "Initial project structure"

### 1.3 Write a README

Create a `README.md` file that includes: - Project title and brief description - Data source (link to NYC OpenData) - How to run the analysis - Required R packages

**Example structure:**

``` markdown
# NYC Stop and Frisk Analysis (2006-2012)

Analysis of NYPD Stop, Question, and Frisk data.

## Data Source

Data from [NYPD Stop, Question and Frisk Database](https://www1.nyc.gov/site/nypd/stats/reports-analysis/stopfrisk.page)

## Setup

1. Download 2006-2012 data files and place in `data/raw/`
2. Install required packages: `tidyverse`, `lubridate`
3. Run `scripts/01-load-data.R`

## Required Packages

- tidyverse
- lubridate
```

## Part 2: Create Loading Script

Create `scripts/01-load-data.R` that: 1. Loads all years of SQL data 2. Use relative paths to make sharing code easier 3. Prints summary statistics for year

**Example code to load data** (still using absolute path)

``` r
sqf_2006 <- read_csv("~/Downloads/2006.csv", col_types = cols(.default = col_character()))
# ... and so on
```

## Part 3: Git Workflow

Throughout your work, make meaningful commits:

**Required commits (minimum):** 1. "Initial project structure" 2. "Add data loading"

**Good commit messages:** - ✅ "Add data loading for SQF data" - ✅ "Fix: Handle missing files gracefully" - ❌ "updates" - ❌ "work"

Use `git status` and `git log` to check your progress.

## Submission

1.  **Push to GitHub**
2.  **Submit the following:**
    -   Link to GitHub repository (make sure instructor has access)
    -   Screenshot of `git log` showing your commits