README
07/27/2023 
Kevin Nguyen

When modifying data: Please use _underscores_ instead of spaces and keep column names consistent. 

Insect antenna are an interesting subject of study for robotics because they can be manipulated without muscles. It is a challenge in robotics to build micro-scale soft actuators because controlling movement without breaking things becomes much harder. 

  For applications, check Harvard Biodesign Lab and soft microactuators or soft robotics.
  Also, for a similar project, check "Cricket antennae shorten when bending".

My project involves measuring the strain and deflection of a cockroach antenna after applying pressure to specific parts of the antenna.
I apologize for any missing information in the folder. Hopefully you can can manage. Here is a description of all the data files.
  
  roach - images of each antenna for determining the length
  
  folders - raw images taken during experiment
    format - {initials}_{xyz}_{distance}
      x - a 2-digit ID number for your cockroach (xx, eg. 03)
      y - which antenna is being experimented on (r = right, l = left)
      z - whether the pressure is applied with the dorsal (d) side up or the ventral (v) side up
      distance - (1 = proximal, 2 = middle, 3 = distal)
  
  joined_data - angle measurements for each antenna
  
  cockroach_dex_data - initial measurements to determine maximum strain
  
  cockroach_diam_data - measurements for a full analysis of strain
  
  cockroach_scale_data - measurements to measure the total length of the antenna and the actual starting points of each measurement
    note - mark_pix is the number of pixels from the antenna tip to the the proximal site of pressure application
    
The theoretical pressure distribution I developed is very bare. This link might be useful. 

  https://news.cornell.edu/stories/2023/01/soft-robots-harness-viscous-fluids-complex-motions
  "Harnessing Nonuniform Pressure Distributions in Soft Robotic Actuators"

antenna_model is the folder where I have done all of my data analysis.
Here is a description of each R file.

  antenna_pressure.R - miscellaneous analysis on the theory
  antenna_solution.R - direct graphing of the theoretical model
  antenna_analysis.R - analysis and graphing of data (dex_data and joined_data) without including scale_data (size) or diam_data (strain)
  antenna_analysis_1.R - includes the two omitted files
  plots.R - code for plots I used in the poster

I also initalized a git repository for this, so you can keep track of changes.
Use this link to clone: https://github.com/kevichem/antenna_model.git

Miscellaneous files are indicated below
  p_dist.png - the actual distribution of pressure in conduits
  p_comp.png - comparing pressure differential with different omega.
  hysteresis_10.pdf - hysteresis graph of antenna 10
  scale_comp.pdf - comparison between all variables in scale_data using plot()


