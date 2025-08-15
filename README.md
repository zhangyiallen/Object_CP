# Change-Point Detection for Object-Valued Time Series

This repository contains the code and implementation details for the manuscript  
**"Change-Point Detection for Object-Valued Time Series"**

---

## Repository Overview

The project is organized into two main parts:

### 1. Simulations/  
Contains code for the following sections of the paper:  
`Section_4_1`, `Section_4_2`, `Section_4_3`, `Appendix_B`, `Appendix_C`, `Appendix_D`, and `Appendix_E`.

### 2. Real_Data_Applications/  
Contains code for the following sections of the paper:  
`Section_5_1`, `Section_5_2`, and `Appendix_F`.

Each folder includes code and a corresponding `README.md` file that provides detailed instructions on how to reproduce the figures or tables presented in the paper.

---

## Data Availability

- For **Section_5_1** and **Section_5_2**, the MNIST dataset (`.pt` files) is provided in the `MNIST/` folder under `Section_5_1`.

- For **Appendix_F**, the CCLE dataset can be obtained from the [GCIT repository](https://github.com/alexisbellot/GCIT/tree/master/CCLE%20Experiments). Please download the following datasets:
  - Response data  
  - Mutation data  
  - Expression data

Place these datasets in the appropriate locations as described in the relevant `README.md` files.

---

## Environment and Dependencies

### Python Environment

The following package versions were used:

| Package                  | Version              |
|--------------------------|----------------------|
| python                   | 3.10.x               |
| torch                    | 2.0.1+cu118          |
| numpy                    | ~1.25                |
| scipy                    | ~1.10.1              |
| tqdm                     | ~4.65                |
| scikit-learn             | 1.3.2                |
| tensorflow_probability   | ~0.21.0              |

### R Environment

The following R version was used for all R Markdown (`.Rmd`) or R script files:

- R version 4.3.3 (2024-02-29 ucrt)
