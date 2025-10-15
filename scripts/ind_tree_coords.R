#-------------------------------------------------------------------------------
# Name:         ind_tree_coords.R
# Description:  Individual tree coordinates are calculated relative to the
#               center point of the plot. 
# Author:       Florian Franz
# Contact:      florian.franz@nw-fva.de
#-------------------------------------------------------------------------------



# source setup script
source('src/setup.R', local = TRUE)



# 01: data reading
#-------------------------------------------------------------------------------

# read forest inventory (BI) data
bi_data <- list.files(raw_data_dir)

bi_points <- read.table(
  file.path(raw_data_dir, 'tblDatPh2_ZE.txt'),
  header = T, sep = ';'
)

bi_trees <- read.table(
  file.path(raw_data_dir, 'tblDatPh2_Vorr_ZE.txt'),
  header = T, sep = ';'
)

# select desired forestry offices (Solling --> Neuhaus, Dassel)
bi_points <- bi_points[
  bi_points$DatOrga_Key == '268-2022-002' | 
    bi_points$DatOrga_Key == '254-2022-002',
]

bi_trees <- bi_trees[
  bi_trees$DatOrga_Key == '268-2022-002' |
    bi_trees$DatOrga_Key == '254-2022-002',
]

head(bi_points)
head(bi_trees)



# 02: data preparation
#-------------------------------------------------------------------------------

# source and apply function for data formatting
source('src/format_data.R', local = TRUE)

bi_points <- format_data(bi_points)
bi_trees <- format_data(bi_trees)

head(bi_points)
str(bi_points)
head(bi_trees)
str(bi_trees)

## delete deadwood and used trees
bi_trees <- bi_trees[!bi_trees$ba %in% seq(100,800,100),]

bi_trees <- bi_trees[bi_trees$'1' < 3 & bi_trees$'2' < 3,]

bi_trees <- bi_trees[bi_trees$art != 1 & bi_trees$art != 2 & bi_trees$bhd > 0,]

# select needed columns
bi_trees <- bi_trees[,c(1:12)]

# assign tree species groups
bi_trees$bagr <- 
  ifelse(bi_trees$ba > 0 & bi_trees$ba < 200, 'EI',
  ifelse(bi_trees$ba > 199 & bi_trees$ba < 300, 'BU',
  ifelse(bi_trees$ba > 299 & bi_trees$ba < 400, 'ALH',	
  ifelse(bi_trees$ba > 399 & bi_trees$ba < 500, 'ALN',
  ifelse(bi_trees$ba > 499 & bi_trees$ba < 600, 'FI',
  ifelse(bi_trees$ba > 599 & bi_trees$ba < 700, 'DGL',
  ifelse(bi_trees$ba > 699 & bi_trees$ba < 800, 'KI',	'LAE')
  ))))))

# DBH correction
# if not measured at 1.3 m (deviating measuring height),
# then correction to 1.3 m
bi_trees$ba1 <- bi_trees$ba

# red oak to oak, fir to spruce, hornbeam to beech
source('src/d_corr_func.R', local = TRUE)

bi_trees$ba <- input_d_korr(bi_trees$bagr)

# average DBH from 'Kreuzkluppung (bhdklup)',
# convert to cm
bi_trees$bhd <- ifelse(
  bi_trees$bhdklup > 0,
  (0.5 * (bi_trees$bhd + bi_trees$bhdklup)) / 10,
  bi_trees$bhd / 10
)

# separate data:
# trees with diameter at deviating measurement height
# and without deviating measurement height
bi_trees_2 <- bi_trees[bi_trees$bhddiff > 0,]
bi_trees <- bi_trees[bi_trees$bhddiff == 0,]

# convert diameter to DBH in case of deviating measuring height
d <- d_korr(du = bi_trees_2$bhd, abwmh = bi_trees_2$bhddiff, ba = bi_trees_2$ba)
bi_trees_2$bhd <- d

# bind the two data frames together again
# (correctly back to original number)
bi_trees <- rbind(bi_trees, bi_trees_2)
rm(bi_trees_2, d)

# add original tree species again
bi_trees$ba <- bi_trees$ba1
bi_trees$ba1 <- NULL

head(bi_trees)

# calculate number of stems per ha

# concentric sample circles:
#	r = 6 m all trees 
#	r = 13 m all trees with DBH >= 30 cm
# radius must be projected into the plane

# correct sample circle sizes with slope, calculate N_ha
# inclination from degrees in rad
bi_points$hang_rad <- (pi / 180) * bi_points$hang

bi_points_trees <- merge(
  bi_trees, bi_points[,c('key', 'kspnr', 'abt', 'hang_rad', 'rw', 'hw')],
  by = c('key', 'kspnr')
)

# r_plane = r_slope * cos(slope_rad)
bi_points_trees$nha <- ifelse(
  bi_points_trees$bhd < 30, 
  10000 / (pi * 6**2 * cos(bi_points_trees$hang_rad)),
  10000 / (pi * 13**2 * cos(bi_points_trees$hang_rad))
)



# 03: individual tree coordinates (ITC) calculation
#-------------------------------------------------------------------------------

# calculate ITC
bi_points_trees$gk3_x <- bi_points_trees$rw + bi_points_trees$abstand / 100 * sin((bi_points_trees$richtung * 2 * pi) / 400)
bi_points_trees$gk3_y <- bi_points_trees$hw + bi_points_trees$abstand / 100 * cos((bi_points_trees$richtung * 2 * pi) / 400)

# conversion into an sf object
bi_points_trees_sf <- sf::st_as_sf(
  bi_points_trees,
  coords = c('gk3_x', 'gk3_y'),
  crs = 31467
)

# transformation to ETRS89 / UTM zone 32N
bi_points_trees_utm <- sf::st_transform(bi_points_trees_sf, crs = 25832)

# write to disk
sf::st_write(bi_points_trees_utm, file.path(processed_data_dir, 'bi_points_trees_utm.gpkg'))


























