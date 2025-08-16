# Change-Point Detection for Object-Valued Time Series

This repository contains the code and implementation details for the manuscript  
**"Change-Point Detection for Object-Valued Time Series"**

---

## Repository Overview

The project is organized into two main parts:

### 1. Simulations/  
This folder contains code for Section 4.1 of the paper: to demonstrate how to use the proposed method for single change point testing. 

- `Table1_n_200.R`: reproduce the rows in Table 1 of Section 4.1 with n=200
- `Table1_n_400.R`: reproduce the rows in Table 1 of Section 4.1 with n=400
- `Figure1_DGP_1.R`: reproduce the three plots for DGP1 in Figure 1
- `Figure1_DGP_2.R`: reproduce the three plots for DGP2 in Figure 1 


### 2. Real Data Applications/  
This folder contains code for Section 5.1 of the paper: to demonstrate how to use the proposed WBS-SN algorithm for multiple change point estimation. 

- `WBS_critic_value.R`: reproduce the rows in Table 1 of Section 4.1 with n=200
- `WBS-SN.R`: reproduce the rows in Table 1 of Section 4.1 with n=400

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
