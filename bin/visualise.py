#!/usr/bin/env python3

import netCDF4 as nc
import matplotlib.pyplot as plt
# import numpy as np

dataset = nc.Dataset("output.nc")

phi_grid_data = dataset["phi_grid_data"][:]
rho_grid_data = dataset["rho_grid_data"][:]
E_x_grid_data = dataset["e_x_grid_data"][:]
E_y_grid_data = dataset["e_y_grid_data"][:]
r_hist = dataset["r_hist"][:]

fig, axs = plt.subplots(3,figsize=(6,12), gridspec_kw={'height_ratios': [1,1 ,2]})
fig.tight_layout()

fig.canvas.manager.set_window_title("E-Field Sim")

im = axs[0].imshow(rho_grid_data)
axs[0].title.set_text("Initial Charge Density")
axs[0].set_axis_off()

im = axs[1].imshow(phi_grid_data)
axs[1].title.set_text("Initial Potential")
axs[1].set_axis_off()

axs[2].title.set_text("Path of particle")
axs[2].plot(r_hist[0], r_hist[1])
axs[2].set_xlabel("x")
axs[2].set_ylabel("y")
axs[2].set_xlim(-1,1)
axs[2].set_ylim(-1,1)

plt.show()
