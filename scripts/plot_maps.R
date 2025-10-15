#-------------------------------------------------------------------------------
# Name:         plot_maps.R
# Description:  Creation of plot maps containing the individual tree positions
#               relative to the center point of each plot. Tree species and
#               and diameter at breast height (DBH) are provided for each tree.
# Author:       Florian Franz
# Contact:      florian.franz@nw-fva.de
#-------------------------------------------------------------------------------



# source setup script
source('src/setup.R', local = TRUE)



# 01: data reading
#-------------------------------------------------------------------------------

# read forest inventory (BI) data with
# calculated individual tree coordinates
bi_points_trees_utm <- sf::st_read(
  file.path(processed_data_dir, 'bi_points_trees_utm.gpkg')
  )
head(bi_points_trees_utm)

# read already remeasured plots (RTK-GNSS)
bi_plots_rtk <- sf::st_read(file.path(raw_data_dir, 'bi_center_points_rtk.gpkg'))

# read VPC
als_vpc <- sf::st_read(file.path(raw_data_dir, 'leafoff.vpc'))
head(als_vpc)



# 02: prepare data for plot maps
#-------------------------------------------------------------------------------

# create center points for each plot from rw/hw coordinates
plot_centers <- bi_points_trees_utm %>%
  sf::st_drop_geometry() %>%
  dplyr::select(kspnr, rw, hw) %>%
  dplyr::distinct() %>% 
  sf::st_as_sf(coords = c('rw', 'hw'), crs = 31467) %>% 
  sf::st_transform(crs = 25832)

# clip plots to AOI (optional)
als_vpc <- sf::st_transform(als_vpc, crs = 25832)
plot_centers_aoi <- sf::st_intersection(plot_centers, sf::st_as_sf(als_vpc))

# keep only the original columns 
# (remove VPC columns)
original_cols <- names(plot_centers)
plot_centers_aoi <- plot_centers_aoi[, original_cols]

# add a column to identify if the plot is already remeasured or not
plot_centers_aoi$remeasured <- 'no'

# identify matching plots based on kspnr column
# note: plot_centers_aoi has "kspnr", bi_plots_rtk has "KSPNR"
matching_plots <- plot_centers_aoi$kspnr %in% bi_plots_rtk$KSPNR

# mark remeasured plots
plot_centers_aoi$remeasured[matching_plots] <- 'yes'

# add center coordinates back to the tree data
# keep only plots which are in the AOI and not already remeasured
plot_centers_aoi <- plot_centers_aoi %>% 
  dplyr::filter(remeasured == 'no') %>%
  dplyr::mutate(
    center_x = sf::st_coordinates(.)[,1],
    center_y = sf::st_coordinates(.)[,2]
  ) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(kspnr, remeasured, center_x, center_y)

bi_points_trees_aoi <- bi_points_trees_utm %>%
  dplyr::inner_join(plot_centers_aoi, by = 'kspnr')

# calculate relative positions of the individual trees from plot center
bi_points_trees_aoi <- bi_points_trees_aoi %>%
  dplyr::mutate(
    tree_x = sf::st_coordinates(.)[,1],
    tree_y = sf::st_coordinates(.)[,2],
    rel_x = tree_x - center_x,
    rel_y = tree_y - center_y
  )



# 03: overview map of all plots in the AOI
#-------------------------------------------------------------------------------

# get all plots within AOI and add remeasured status
als_vpc_2d <- sf::st_zm(als_vpc)
plot_centers_all_aoi <- sf::st_intersection(plot_centers, als_vpc_2d)

original_cols <- names(plot_centers)
plot_centers_all_aoi <- plot_centers_all_aoi[, original_cols]

plot_centers_all_aoi$remeasured <- ifelse(
  plot_centers_all_aoi$kspnr %in% bi_plots_rtk$KSPNR, 
  'yes', 'no'
)

mapview::mapview(
  als_vpc_2d$geometry,
  alpha.regions = 0,
  map.type = 'OpenStreetMap') +
  mapview::mapview(
    plot_centers_all_aoi,
    zcol = 'remeasured',
    cex = 5,
    layer.name = 'already remeasured')



# 04: creation of plot maps
#-------------------------------------------------------------------------------

# function to create example plot map
create_plot_map <- function(plot_id, tree_data, plot_centers) {
  
  # filter data for specific plot
  plot_trees <- tree_data %>%
    dplyr::filter(kspnr == plot_id)
  
  plot_center <- plot_centers %>%
    dplyr::filter(kspnr == plot_id)
  
  if(nrow(plot_trees) == 0) {
    warning(paste('No trees found for plot', plot_id))
    return(NULL)
  }
  
  p <- ggplot() +
    
    # draw sample circles (6m and 13m radius)
    annotate('path',
             x = 6 * cos(seq(0, 2*pi, length.out = 100)),
             y = 6 * sin(seq(0, 2*pi, length.out = 100)),
             color = 'gray50',
             linetype = 'dashed',
             linewidth = 1) + 
    annotate('path',
             x = 13 * cos(seq(0, 2*pi, length.out = 100)),
             y = 13 * sin(seq(0, 2*pi, length.out = 100)),
             color = 'gray30',
             linetype = 'dashed',
             linewidth = 1) + 
    
    # add trees as points
    geom_point(
      data = plot_trees,
      aes(x = rel_x, y = rel_y, color = bagr, 
          size = bhd),
      alpha = 0.7,
      position = position_jitter(width = 0.1, height = 0.1)
    ) +
    
    # add DBH labels on each tree
    geom_text(
      data = plot_trees,
      aes(x = rel_x, y = rel_y, label = round(bhd, 1)),
      size = 5,
      color = 'black',
      position = position_jitter(width = 0.1, height = 0.1) 
    ) +
    
    # add center point
    geom_point(
      aes(x = 0, y = 0),
      color = 'black', 
      size = 3, 
      shape = 3
    ) +

    coord_equal() +
    
    # individual tree scaling - each tree gets unique size based on its DBH
    scale_size_identity(name = 'DBH (cm)') +
    
    # fixed color scale for consistent tree species colors across all plots
    scale_color_manual(
      name = 'Tree Species',
      values = c(
        'EI' = '#1f77b4',
        'BU' = '#ff7f0e', 
        'ALH' = '#2ca02c',
        'ALN' = '#d62728',
        'FI' = '#9467bd', 
        'DGL' = '#8c564b',
        'KI' = '#e377c2',    
        'LAE' = '#7f7f7f'    
      ),
      drop = F 
    ) +
    
    labs(
      title = paste('Plot Map - Kspnr:', plot_id),
      subtitle = paste('Number of trees:', nrow(plot_trees)),
      x = 'Relative X Position (m)',
      y = 'Relative Y Position (m)'
    ) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = 'right'
    )
  
  return(p)
  
}

# create example plot
if(nrow(bi_points_trees_aoi) > 0) {
  
  first_plot_id <- unique(bi_points_trees_aoi$kspnr)[89]
  example_plot <- create_plot_map(first_plot_id, bi_points_trees_aoi, plot_centers_aoi)
  
  if(!is.null(example_plot)) {
    print(example_plot)
    
  }
}
