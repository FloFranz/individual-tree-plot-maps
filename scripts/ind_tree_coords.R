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

# input paths
bi_path <- file.path(raw_data_dir, 'forest_inventory')
pc_loff_path <- file.path(raw_data_dir, 'pc_leafoff_2024')
pc_lon_path <- file.path(raw_data_dir, 'pc_leafon_2023')

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