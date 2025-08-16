# Change-Point Detection for Object-Valued Time Series

This repository contains the code and implementation details for the manuscript:

**"[Change-Point Detection for Object-Valued Time Series](https://www.tandfonline.com/doi/full/10.1080/07350015.2025.2520862)"**

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

- `WBS_critic_value.R`: simulate the rejection criteria used in the WBS-SN algorithm
- `WBS-SN.R`: reproduce the results in Section 5.1 of the paper


