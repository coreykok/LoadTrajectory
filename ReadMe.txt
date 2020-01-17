To run the Trajectory tool: 
> Run the simulation tool 
> Copy the results from LoadSimulation/data/Simulation to /resources/app/data/Simulation
> Open the PercentileTool shortcut (which opens the file /electron-quick-start.exe)

##########################################################################################

Plots a particular consumption trajectory based on where it ranks in a particular statistic indicating "High Usage" 

User Input:
Rank: Which statistic will be used to rank each sample
perc: Which percentile of the statistic should be plotted

Input Files/Folers:
TC.rds: Simulation of the total consumption by resampling, attaching the domination rank to the sample number

Output Files:
savefile1: Plot of the overall consumption in the selected sample
savefile2: Plot of the selected samples total consumption in each subgroup