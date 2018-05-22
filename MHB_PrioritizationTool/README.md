MISSOURI HEADWATERS BASIN CONSERVATION PRIORITIZATION TOOL


---System Requirements---

 - Windows operating system
 - RStudio (version 1.0 or later)

This tool is set up as an interactive R Notebook, which is powered by the R statistical programming environment (https://www.r-project.org/). Although there is no need for users to have any familiarity with R, users do need to download and install R and version 1.0 or later of the RStudio software. Unfortunately, because the Zonation software is not compatible with Mac operating systems, this tool can only be run on Windows machines. 

You can download and install the latest versions of R and RStudio for Windows here:
https://cran.rstudio.com/bin/windows/base/
https://download1.rstudio.org/RStudio-1.1.423.exe


---Getting Started---

This tool is designed to help identify priority subwatersheds in the Missouri Headwaters Basin where there is potential for a conservation action of interest to provide the greatest net benefits to multiple conservation targets. 

Start by opening the 'MHB_PrioritizationTool.Rmd' R Markdown file. RStudio will open a script to run the tool - DON'T PANIC! There is no need to understand or interact with the code, simply choose 'Run Document' next to the green arrow in the tab header. A graphical user interface will open to allow selection of settings and exploration of the map data. 

Start by selecting your conservation action of interest, then assign weights to conservation targets that are linked to that action, based on their relative importance to you. When the prioritization process is finished, you can use the interactive map and plots to explore the results.


---Resources---

For further instructions, more information on the input data and prioritization algorithm, and other supporting information, view or download the full documentation here: https://drive.google.com/open?id=1sOPr7kSgzBG8BNh66u3jwUP-ackppJagv9uay6Rxo3Q 


---Credits---

This custom decision support tool was developed by The Center for Large Landscape Conservation (http://www.largelandscapes.org) and Conservation Science Partners(http://www.csp-inc.org), with funding from The National Fish & Wildlife Foundation(http://www.nfwf.org). 

The prioritization is run using Zonation 4.0 (https://www.helsinki.fi/en/researchgroups/metapopulation-research-centre/software#section-14300) and an adaptation of the zonator package for R (https://github.com/cbig/zonator).

Contact: Tyler Creech (tyler@largelandscapes.org), Meredith McClure (meredith@csp-inc.org)
