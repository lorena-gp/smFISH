# Main R script to determine:
#     RNA copy number per cell
#     RNA density number per cell
#     Transcriptional properties
# Based on the output .csv table obtained with the Python script
#

# Lorena Garcia-Perez, Developmental Dynamics lab, The Francis Crick Institute, UK
# lorena.garcia-perez@crick.ac.uk

# Libraries
library(ggplot2)
library (ggbeeswarm)
require(plyr) # Am I using this in the end?
library(tidyverse)
# library(tibble) #for add_column etc


source("./R/load_clean.R")

copy.density.cell <-
  file %>%
  mutate(cell.area = cell.area * pixel.size) %>% # Convert cell area in pixels to cell area in µm^2
  group_by(channel, day, image, cell.id, cell.area) %>%
  summarise(copy = n()) %>% # RNA copy number per cell
  mutate(density = round(copy / cell.area, 3)) # RNA densities in terms of RNA molecules per µm^2


# To determine which spots can be considered transcription spots,
# we need to know the area and maximum intensity values for all the spots
# we normalize according to each channel, day and image

TX.threholds <-
  file %>%
  group_by(channel, day, image) %>% # Choose threholds specifically for each image and channel
  summarise(spot.MaxInt.median = median(max.int), spot.MaxInt.mad = mad(max.int),
            spot.area.median = median(area), spot.area.mad = mad(area)) %>%
  mutate(spot.MaxInt.threshold = spot.MaxInt.median + 3 * spot.MaxInt.alpha * spot.MaxInt.mad,
         spot.area.threshold = spot.area.median + 3 * spot.area.alpha * spot.area.mad)

TX.number.cell <-
  file %>%
  group_by(channel, day, image) %>%
  left_join(TX.threholds) %>%
  mutate(is.TX.spot = max.int > spot.MaxInt.threshold & area > spot.area.threshold) %>%
  group_by(channel, day, image, cell.id) %>%
  summarise(TX.number = sum(is.TX.spot))

TX.mol <-
  file %>%
  group_by(channel, day, image) %>%
  left_join(TX.threholds) %>%
  mutate(is.TX.spot = max.int > spot.MaxInt.threshold & area > spot.area.threshold) %>%
  mutate(spot.meanInt.median = median(mean.int)) %>%
  filter(is.TX.spot == TRUE) %>%
  mutate(probe.loc.factor = channel) %>%
  mutate(probe.loc.factor = as.numeric(recode(probe.loc.factor,
                            "0"=correction.factor[1], "1"=correction.factor[2], "2"=correction.factor[3]))) %>%
  mutate(TX.mol = mean.int * area / (probe.loc.factor * spot.meanInt.median * spot.area.median))
TX.mol$channel <- as.factor(TX.mol$channel)
levels(TX.mol$channel) <- genes



# 2. Main datasets for single cell data: ----------------------------------

single.cell.data <-
  inner_join(copy.density.cell, TX.number.cell) %>%
  ungroup()
single.cell.data$channel <- as.factor(single.cell.data$channel)
levels(single.cell.data$channel) <- genes


single.cell.expressing.data <-
  single.cell.data %>%
  filter(density >= 1)

single.cell.sox2.expressing.data <- # This could be combined within single.cell.progenitors.data with filter_if() ?
  single.cell.data %>%
  filter(channel == progenitors.gene) %>%
  filter(density >= 1) # Require sox2 density >= 1 molecule / µm^2

single.cell.progenitors.data <- # Select progenitors according to sox2 density >= 1 molecule / µm^2
  semi_join(single.cell.data, single.cell.sox2.expressing.data,
            by = c("day", "image", "cell.id", "cell.area"))

# This is the more complete, non subseted, dataset
# But, if a cell has two TX spots, it will appear on two rows and so on
withTX.single.cell.data <- 
  TX.mol %>%
  select (channel, day, image, cell.id, spot.id, TX.mol) %>% 
  full_join(single.cell.data) %>%
  arrange(channel, day, image, cell.id) %>%
  replace_na(list(TX.mol = 0))

two.alleles.data <-
  TX.mol %>%
  select(channel, day, image, cell.id, TX.mol) %>%
  group_by(channel, day, image, cell.id) %>%
  mutate(freq = n()) %>% 
  ungroup() %>%
  filter(freq == 2) %>%
  select(-freq) %>%
  mutate(TX.id = rep(c(1,2), length.out = length(.$channel))) %>% # Create a column with appropiate numebr of rows
  spread(TX.id, TX.mol) %>%
  rename(TX.id.1 = "1", TX.id.2 = "2")
two.alleles.data$channel <- as.factor(two.alleles.data$channel )
levels(two.alleles.data$channel) <- genes


source("./R/func.R")


# 3. Mean properties ------------------------------------------------------

# single.cell.progenitors.data is used, but results with single.cell.data are quite similar

mean.properties.data <-
  tibble(day = character(), gene = character(),
         prob.TX = as.numeric(), mean.copy = as.numeric(), mean.nPol = as.numeric())

for (d in days) {
  for (g in genes) {
    mean.properties.data <-
      add_row(mean.properties.data,
              day = d, gene = g,
              prob.TX = round(prob.TX.fn(single.cell.progenitors.data, d, g, ploidy),3),
              mean.copy = round(mean.copy.fn(single.cell.progenitors.data, d, g),3),
              mean.nPol = round(mean.nPol.fn(TX.mol, d, g),3))
  }
}
mean.properties.data <- mean.properties.data %>% replace_na(list(mean.nPol = 0))

mean.properties.data <-
  mean.properties.data %>%
  mutate(vPol = vPol.chosen, gene.length = rep(genes.length,3), ploidy = ploidy) %>%
  mutate(TX.rate = mean.nPol * vPol / gene.length) %>%
  mutate(deg.rate = prob.TX * TX.rate * ploidy / mean.copy) %>%
  mutate(half.life = log(2)/deg.rate) #in min


###########################################################################
###########################################################################

prob.TX.pairs.data <-
  mean.properties.data %>%
  select(day, gene, prob.TX) %>%
  rename(gene.1 = gene, prob.TX.1 = prob.TX) %>%
  mutate(gene.2 = gene.1) %>%
  expand(nesting(day, gene.1, prob.TX.1), gene.2) %>%
  group_by(day) %>%
  mutate(prob.TX.2 = rep(unique(prob.TX.1), times = 3)) %>%
  filter(gene.1 < gene.2) # To get unique combinations

# TO BE FINISHED...

for (d in days) {
  for (g in genes) {
    prob.TX.pairs.data$prob.TX.both <-
      add_row(prob.TX.pairs.data$prob.TX.both,
              prob.TX.pairs.fn())
              


prob.TX.pairs.fn <- function(data, day.chosen, gene.chosen.1, gene.chosen.2, ploidy) {
  # day.chosen and gene.schosen must be input as character (for example, "4" and "Olig2")
  # ploidy must be input as numeric
  day.gene.TX.subset <-
    filter(data, day == day.chosen & (channel == gene.chosen.1 | channel == gene.chosen.2))
  dual.TX <-
    day.gene.TX.subset %>%
    select(channel, image, day, cell.id, TX.number) %>%
    filter (TX.number != 0) %>%
    group_by(day, image, cell.id) %>%
    mutate(freq = n()) %>% 
    ungroup() %>%
    filter(freq == 2) %>%
    select(-freq) %>%
    spread(channel, TX.number) %>%
  return(nrow(dual.TX)/(nrow(day.gene.TX.subset))) # Here I do not make use of the ploidy concept...
}


  
two.alleles.data$channel <- as.factor(two.alleles.data$channel )
levels(two.alleles.data$channel) <- genes


TX.probability.all[,6] <- TX.probability.all$TX.prob.pair /
  (TX.probability.all$TX.prob.oneC * TX.probability.all$TX.prob.otherC)
colnames(TX.probability.all)[6] <- c("extrinsic.noise")

#To measure the influence of extrinsic noise:
#THIS SHOULD BE DONE WITH A CONSTITUTIVE GENE TO COMPARE!!!

###########################################################################
###########################################################################


copy.box.plot(single.cell.progenitors.data)
copy.box.plot(single.cell.progenitors.data, darkTheme)

copy.pairwise.scatter.plot(single.cell.progenitors.data, "Irx3", "Olig2")
copy.pairwise.scatter.plot(single.cell.progenitors.data, "Irx3", "Sox2")
copy.pairwise.scatter.plot(single.cell.progenitors.data, "Sox2", "Olig2")

copy.pairwise.scatter.plot(single.cell.progenitors.data, "Irx3", "Olig2", darkTheme)
copy.pairwise.scatter.plot(single.cell.progenitors.data, "Irx3", "Sox2", darkTheme)
copy.pairwise.scatter.plot(single.cell.progenitors.data, "Sox2", "Olig2", darkTheme)


concentration.box.plot(single.cell.progenitors.data)
concentration.box.plot(single.cell.progenitors.data, darkTheme)

concentration.pairwise.scatter.plot(single.cell.progenitors.data, "Irx3", "Olig2")
concentration.pairwise.scatter.plot(single.cell.progenitors.data, "Irx3", "Sox2")
concentration.pairwise.scatter.plot(single.cell.progenitors.data, "Sox2", "Olig2")

concentration.pairwise.scatter.plot(single.cell.progenitors.data, "Irx3", "Olig2", darkTheme)
concentration.pairwise.scatter.plot(single.cell.progenitors.data, "Irx3", "Sox2", darkTheme)
concentration.pairwise.scatter.plot(single.cell.progenitors.data, "Sox2", "Olig2", darkTheme)


source("26_cell_area.R")


###########################################################################
###########################################################################
###########################################################################
###########################################################################

file$channel <- as.factor(file$channel)
levels(file$channel) <- genes

rect <- data.frame(xmin=threshold.area, xmax=Inf,
                   ymin=threshold.MaxInt, ymax=Inf,
                   channel=c(0,1,2))
rect$channel <- as.factor(rect$channel)
levels(rect$channel) <- genes

###########################################################################
###########################################################################

source("26_spot_area_maxInt.R")


source("26_TXnumber_vs.R")


source("26_barTXprob.R")


source("26_barTXprobExtrinsic.R")

###########################################################################
###########################################################################


nested <-
  total %>%
  group_by(day, image, cell.id, cell.area, channel, copy, density, TX.number) %>% nest() %>%
  group_by(day, image, cell.id, cell.area, channel) %>% nest() %>%
  group_by(day, image, cell.id, cell.area) %>% nest() %>%
  group_by(day) %>% nest()

myjson <- toJSON(nested, pretty = TRUE, auto_unbox = TRUE)

write(myjson, "single-RNAs_TX-RNAs_cell.json")

library(rjson)
json_data <- fromJSON(file="single-RNAs_TX-RNAs_cell.json")


###########################################################################
###########################################################################

source("26_boxTXmolecules.R")



source("26_TX2alleles.R")

###########################################################################
###########################################################################

plots <- c(hist.spots.area,
           hist.spots.MaxInt,
           p.spots.AreaVsMaxInt,
           p.TX.numberVsCopy,
           p.TX.numberVsDensity,
           p.TX.numberVsCellArea,
           bar.TX.prob,
           bar.TX.prob.extrinsic,
           box.TX.molecules,
           p.TX.2alleles.molecules.final)

###########################################################################
###########################################################################

#Heterogeneity analysis

var(spots.cell$copy.C1) / mean (spots.cell$copy.C1)

#Fano factor = variance / mean = sigma^2 / mean
FF.expressing <- data.frame(FF.C0=as.numeric(),
                            FF.C1=as.numeric(),
                            FF.C2=as.numeric())
i <- 1
for (d in days) {
  temp <- spots.cell.expressing[[1]]
  temp2 <- spots.cell.expressing[[2]]
  temp3 <- spots.cell.expressing[[3]]
  FF.expressing[i,] <- c(var(temp$copy.C0[temp$day==d]) / mean(temp$copy.C0[temp$day==d]),
                                 var(temp2$copy.C1[temp2$day==d]) / mean(temp2$copy.C1[temp2$day==d]),
                                 var(temp3$copy.C2[temp3$day==d]) / mean(temp3$copy.C2[temp3$day==d]))
  i <- i + 1
}



###########################################################################
###########################################################################

#Save extracted data to file

write.csv(spots.cell, "spots_cell.csv", row.names=FALSE)
write.csv(spots.cell.2, "spots_cell2.csv", row.names=FALSE)

write.csv(spots.cell.pro, "spots_cell_pro.csv", row.names=FALSE)
write.csv(spots.cell.2.pro, "spots_cell2_pro.csv", row.names=FALSE)

write.csv(TX.molecules, "TX_molecules.csv", row.names=FALSE)
