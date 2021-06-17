# mor2
Data wrangling and analysis code for working with the ecological and household data from the CSU MOR2 Project.

Projects & Processing steps:

Herder Practices paper:


Rules & Tenure Paper:
1_RTM_datapull : wrangles the data from mor2 and jaysdata & recodes and formats, saves to an Rdata file
2_RTM-dataexplore.R : some code for crosstabs, etc., in cluding the proportion tables: Table 4 and Table S#
CompareGLMMs : when I redid the model comparisons bc I hadn't looked at it in a while and I wanted to make sure.

TODO: put the best fit models into their own script (or modify CompareGLMMs to do this) and save to objects so 
can later call them into the plotting scripts, and include a short dictionary
in those so now how to reference them

RTP_FigsandTables.Rmd : made this JUST for plotting. Pulls in data from 1_RTM_datapull but calls the models in here. need to make a new one so can just call model objects from the other scripts.

