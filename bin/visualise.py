#!/usr/bin/env python

import netCDF4 as nc
import matplotlib.pyplot as plt

dataset = nc.Dataset("rho_test")

phi_grid_data = dataset["phi_grid_data"][:]
rho_grid_data = dataset["rho_grid_data"][:]
E_x_grid_data = dataset["E_x_grid_data"][:]
E_y_grid_data = dataset["E_y_grid_data"][:]
r_hist = dataset["r_hist"][:]


print(r_hist)
fig, axs = plt.subplots(3,figsize=(5, 5))
fig.tight_layout()
fig.canvas.set_window_title("E-Field Sim")

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


plt.show()
