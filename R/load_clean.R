

# Lorena Garcia-Perez, Developmental Dynamics lab, The Francis Crick Institute, UK
# lorena.garcia-perez@crick.ac.uk

#Another option is to execute the following:
args = c("Diff26_c2_data.csv",
         "26_c2_copy_den_plots.pdf", "26_c2_TX_plots.pdf", "26_c2_heter_plots.pdf",
         "26_c2_copyN.csv", "26_c2_TX.csv", "26_c2_heter.csv")
#
#The first argument is the data to be read,
#arguments 2-4 are the names of the pdf files to be created containing the plots,
#arguments 5-7 are the names of the csv files to be created cotaining the extracted data

###########################################################################
###########################################################################


# Colors
cbbPalette <- c("#000000", "#009E73", "#e79f00", "#9ad0f3", "#0072B2", "#D55E00", 
                "#CC79A7", "#F0E442")

###########################################################################

days <- c(4,5,6)
days.names <- c("day 4", "day 5", "day 6")

channels <- c(0, 1, 2)

genes <- c("Irx3","Sox2","Olig2")

progenitors.gene <- genes[2]

###########################################################################

file <- read.csv("./input/Diff26_c2_data.csv")

# To remove any row where the area = 0 due to clicking on a point during cell segmentation
file <- file[file$cell.area != 0, ]


spot.MaxInt.alpha <- 2 # This value should lay between 1.5 to 3
spot.area.alpha <- 2 # This value should lay between 1.5 to 3

  
# With 60X objective in BX61 microscope and previous ANDOR camera:
# Pixel size = 0.1107x0.1107 µm^2 = 0.01225 µm^2
pixel.size <- 0.01225


ploidy <- 3

#Correction factor that takes into account
#probe localization over gene:
#Irx3 reverse starnd transcribed, Sox2 and Olig2
correction.factor <- c(0.34, 0.5, 0.34)


genes.length <- c(4822, 3311, 4400) # channel 0, 1, 2 (Irx3, Sox2, Olig2)
vPol.chosen = 2040 # bp/min, as in Bahar Halpern


