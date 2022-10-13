# Big ideas:

# why to make one: Reduce complexity
# avoid duplication
# Hide stuff

# Good name (verb + object)
# Good variables (don't change inputs), order them well
# don't work with parameters, use local variables
# Well named inputs
# Clear communication: don't write to globals
# single purpose
# Handle bad data
# no more than 7 inputs


library(ggplot2)
library(ggspatial)
library(graphics)


########################################################THE BAD FUNCTION #######################################
# Function for plotting point data for the reproducible report.
#
# Inputs:
# 1. baseMap = map object, either areaMap or regionMap
# 2. data_sf: sf data to be plotted
#    (ideally, pre-clipped to map area with the master_intersect function, using bboxMap, or regionBox)
# 3. attribute: column name of factor data in data_sf.
#               this attribute name will appear in the legend. For single color points set to "NONE"
# 4. legendName: string, sets the name of the legend for cases where the attribute is not appropriate. Defaults to
#                the attribute.
# 5. colorMap: named list of colours used to set the scale. Names should match factors from attribute col,
#              values should be color codes.  eg. WhaleCol.
# 6. shapeMap: Similar to colorMap except with values of the R shape codes (eg, 15, 16, ...) instead of colours.
#
#
# Created by Quentin Stoyel, September 2, 2021 for reproducible reporting project

plot_points <- function(baseMap, data_sf, attribute="NONE", legendName="",
                        colorMap=NULL, shapeMap=NULL, size=2.5, shape=20, color="black",
                        continuousAttr=FALSE, minScale=NULL, maxScale=NULL,
                        labelData=NULL, labelAttribute=NULL) {

  # extract scaleBar layer to ensure it plots over polygons/study area box
  scaleBarLayer <- get_scale_bar_layer(baseMap)
  studyBoxLayer <- get_study_box_layer(baseMap)
  watermarkLayer <- get_watermark_layer(baseMap)

  # axis limits based on baseMap
  axLim <- ggplot2::coord_sf(xlim=baseMap$coordinates$limits$x,
                             ylim=baseMap$coordinates$limits$y, expand=FALSE)
  shapeLayer <- NULL

  if (toupper(attribute) == "NONE") {
    # just plot raw data (no colors, shapes, etc)
    dataLayer <- geom_sf(data = data_sf, size = size, shape = shape, color=color)
    legendLayer <- NULL
  } else {
    # need to set point style by the attribute
    if (continuousAttr) {
      legendLayer <- scale_bar_layer(data_sf, attribute, continuousAttr,
                                     minScale, maxScale, legendName, fill=FALSE)
    } else {
      data_sf[[attribute]] <- as.factor(data_sf[[attribute]])
      if (is.null(colorMap)){
        colorMap <- get_rr_color_map(data_sf[[attribute]])
      } else {
        colorMap <- colorMap[names(colorMap) %in% data_sf[[attribute]]]
        if (!is.null(shapeMap)){
          shapeMap <- shapeMap[names(shapeMap) %in% data_sf[[attribute]]]
          shapeLabels <- names(shapeMap)
          shapeValues <- unname(shapeMap)
          dataLayer <- geom_sf(data = data_sf, aes(color=!!sym(attribute), shape=!!sym(attribute)), size = size)
          shapeLayer <- scale_shape_manual(labels = shapeLabels, values = shapeValues, name=legendName)
        }
      }
      legendLayer <- scale_colour_manual(values=colorMap, name=legendName)
    }

    dataLayer <- geom_sf(data = data_sf, aes(color=!!sym(attribute)), size = size, shape = shape)

  }

  if(!is.null(labelData)) {
    labelLayer <- ggrepel::geom_label_repel(data = labelData,
                                            aes(label = !!sym(labelAttribute),
                                                geometry = geometry),
                                            stat = "sf_coordinates",
                                            min.segment.length = 0,
                                            xlim = baseMap$coordinates$limits$x * c(0.998, 1.002),
                                            ylim = baseMap$coordinates$limits$y * c(1.002, 0.998)
    )
  } else {
    labelLayer <- NULL
  }


  pointMap <- baseMap +
    dataLayer +
    shapeLayer +
    legendLayer +
    labelLayer +
    axLim +
    watermarkLayer +
    studyBoxLayer +
    scaleBarLayer

  return(pointMap)
}



# helper function, extracts the scale bar from either the areaMap or regionMap
get_scale_bar_layer <- function(inPlot) {
  scaleBarLayer <- lapply(inPlot$layers, function(inLayer) if("GeomScaleBar" %in% class(inLayer$geom)) inLayer else NULL)
  scaleBarLayer <- scaleBarLayer[!sapply(scaleBarLayer, is.null)]
  return(scaleBarLayer)
}


# helper function, extracts the watermark layer from either the areaMap or regionMap
get_watermark_layer <- function(inPlot) {
  watermarkLayer <- lapply(inPlot$layers, function(inLayer) if("GeomCustomAnn" %in% class(inLayer$geom)) inLayer else NULL)
  watermarkLayer <- watermarkLayer[!sapply(watermarkLayer, is.null)]
  return(watermarkLayer)
}


# helper function, extracts the study_box_layer from either the areaMap or regionMap
# selection criteria is based off of colour, use with care.
get_study_box_layer <- function(inPlot) {
  studyBoxLayer <- lapply(inPlot$layers, function(inLayer) if("red" %in% c(inLayer$aes_params$colour)) inLayer else NULL)
  studyBoxLayer <- studyBoxLayer[!sapply(studyBoxLayer, is.null)]
  return(studyBoxLayer)
}

# helper function to reduce tick mark counts on plots
less_x_ticks <- function(inPlot, tickNum=5) {
  xmin <- inPlot$coordinates$limits$x[1]
  xmax <- inPlot$coordinates$limits$x[2]
  #set outplot axis tick marks
  axisSigFigs <- ceiling(log10(xmax - xmin))
  digits <- ifelse(axisSigFigs < 0, -axisSigFigs, 1)
  breakVec <- round(seq(xmin, xmax, length.out = tickNum),
                    digits = digits)
  outPlot <- inPlot + scale_x_continuous(breaks = breakVec)
  return(outPlot)
}

# helper function to generate colormap when not specified.
# RR color scheme is used for first 8 colors, after which the viridis
# pallette is used.
get_rr_color_map <- function(dataCol) {
  colorNames <- unique(dataCol)
  colorNames <- colorNames[order(colorNames)]
  numColors <- length(colorNames)
  rrColorPalette <- c("#009E73", "#E69F00", "#0072B2", "#CC79A7", "#F0E442",
                      "#D55E00", "#56B4E9","#999999")
  if (numColors > 0) {
    if(numColors > length(rrColorPalette)){
      colorMap <- hcl.colors(length(colorNames))
    } else {
      colorMap <- rrColorPalette[1:numColors]
    }
    names(colorMap) <- colorNames
  }
  return(colorMap)
}

scale_bar_layer <- function(data_sf, attribute, continuous, minValue, maxValue, legendName, colorMap, fill=TRUE){
  if (continuous){
    if (is.null(minValue)){
      minScale = min(data_sf[[attribute]])
    }
    if (is.null(maxValue)){
      maxScale = max(data_sf[[attribute]])
    }
    if (fill){
      scaleBarLayer <- scale_fill_continuous(type="viridis", name=legendName,
                                             labels = scales::comma, limits=c(minValue, maxValue))
    } else {
      scaleBarLayer <- scale_color_continuous(type="viridis", name=legendName,
                                             labels = scales::comma, limits=c(minValue, maxValue))
    }

  } else {
    data_sf[[attribute]] = as.factor(data_sf[[attribute]])
    if (fill) {
      scaleBarLayer <- scale_fill_manual(values=colorMap, name=legendName)
    } else {
      scaleBarLayer <- scale_color_manual(values=colorMap, name=legendName)
    }

  }
  return(scaleBarLayer)


}




# --------Maps Setup------------
# This function produces a list of the necessary data used to generate plots in the report
#
# Inputs:
# 1. studyArea: polygon of the study area (sf object, defined by the user in the shiny app)
# 2. site: polygon of a study site (like aquaculture site);for now it is a centroid (a point, centre of studyArea)
#    in the past we plotted the site polygon and site is kept for now as a placeholder for the future
# 3. region: Polygon describing the region of the report
# 4. areaLandLayer: land borders for the areaMap, could be 10K or 50K scale (preloaded)
# 5. regionLandLayer: land borders for the regionMap, could be 10K or 50K scale (preloaded)
# 6. CANborder: Canada border, including water (preloaded: bounds_sf)
#
# Outputs: list containing 8 items
# 1. studyBox_geom, bounding box of the study area
# 2. studyArea, input studyArea
# 3. site, input site
# 4. region, input region
# 5. areaMap, map of the study area, used as a base map for plots in the report
# 6. bboxMap, bounding box of the areaMap
# 7. regionBox, bounding box of the region
# 8. regionMap, map of the study region, used as a base map for plots in the report
#

maps_setup <- function(studyArea, region, areaLandLayer, regionLandLayer, CANborder){
  site <- sf::st_centroid(studyArea)
  # The following defines studyBox geometry "look". studyBox_geom is input into area map or can be added to any map later
  studyBox_geom <- geom_sf(data=studyArea, fill=NA, col="red", size=1)

  # The following plots area map using function (output is a list)
  areaMapList <- area_map(studyArea, areaLandLayer, 5, CANborder, studyBox_geom)

  # The following separates items in the output list: first item is a map and second is a bounding box of the map
  areaMap <- areaMapList[[1]] # map
  bboxMap <- areaMapList[[2]] #bounding box of the map

  # Bounding box for the region
  regionBox <- sf::st_bbox(region)

  # Create the regional map
  regionMap <- region_map(regionBox, studyArea, regionLandLayer, CANborder)

  outlist <- list("studyBox_geom" = studyBox_geom,
                  "studyArea" = studyArea,
                  "site" = site,
                  "areaMap" = areaMap,
                  "bboxMap" = bboxMap,
                  "region" = region,
                  "regionBox" = regionBox,
                  "regionMap" = regionMap
  )
  return(outlist)
}


# ----- AREA MAP -----
# This function produces a map of the area surrounding a box of the study area using ggplot.
# The extent of the area to plot around the studyArea is defined by "bufKm" parameter in km (sets "zoom")
# This function was created for searchPEZ and it is used as a "basemap" for other plots
#
# Inputs:
# 1. studyArea: polygon of the study area (sf object, defined by the user in the shiny app)
# 2. site: polygon of a study site (like aquaculture site);for now it is a centroid (a point, centre of studyArea)
#    in the past we plotted the site polygon and site is kept for now as a placeholder for the future
# 3. landLayer: polygon developed for Atlantic canada, could be 10K or 50K scale (preloaded)
# 4. bufKm: how many km around the study area to plot (acts like "zoom")
# 5. CANborder: Canada border, including water (preloaded)
# 6. studyBoxGeom: geometry portraying study box (defines the "look" of the study box, defined in the main script)
#
# Outputs: list containing 2 items
# 1. map, that can be used as a basemap for adding data layers
# 2. bounding box of the map that can be used for cropping datasets
#

area_map <- function(studyArea, landLayer, bufKm, CANborder, studyBoxGeom) {
  site <- sf::st_centroid(studyArea)
  # buf is in km, and now converted to degrees
  bufx <- bufKm / 100
  bufy <- 0.72 * bufKm / 100 # scaled degrees

  # bounding box for study area
  bbox <- sf::st_bbox(studyArea)

  widthBbox <- ((bbox$xmax) - (bbox$xmin)) * 0.72 # in scaled degrees
  heightBbox <- (bbox$ymax) - (bbox$ymin)  # in degrees

  if (heightBbox > 2 * widthBbox) {
    bufx <- bufx + (0.5 * (heightBbox - widthBbox))
  } else if (widthBbox > 2 * heightBbox) {
    bufy <- bufy + (0.5 * (widthBbox - heightBbox))
  }

  # create bounding box for buffer (plot area)
  bboxBuf <- bbox

  bboxBuf["xmin"] <- (bbox$xmin) - bufx
  bboxBuf["xmax"] <- (bbox$xmax) + bufx
  bboxBuf["ymin"] <- (bbox$ymin) - bufy
  bboxBuf["ymax"] <- (bbox$ymax) + bufy

  # crop land to plot area to speed up plotting
  land <- sf::st_crop(landLayer, bboxBuf)

  # crop US-Canad boundary to plot area to speed up plotting
  bound <- sf::st_crop(CANborder, bboxBuf)

  # configure the plot
  outPlot <- ggplot() +
    geom_sf(data = site, fill = "yellow", col = "black", size = 0.6) +
    geom_sf(data = bound, col = "darkgrey", linetype = "dashed", size = 1.1) + # creates US boundary line, 200 nm limit
    geom_sf(data = land, fill = c("lightgrey"), col = "black", size = 0.3) +
    eval(studyBoxGeom)

  outPlot <- format_ggplot(outPlot, bboxBuf)
  outList <- list(outPlot, bboxBuf)

  return(outList)
}


# ----- REGION MAP -----
# This function produces a map of the region using ggplot.
#
# Inputs:
# 1. regionBbox: bounding box of the region (defined in intro)
# 2. studyArea: polygon of the study area (sf object, defined by the user in the shiny app)
# 3. landLayer: polygon developed for Atlantic canada, scale used for regional maps is 1:10m (land10m_sf)
# 4. CANborder: Canada border, including water (preloaded: bounds_sf)
#
# Output: map, that can be used as a basemap for adding data layers
#

region_map <- function(regionBbox, studyArea, landLayer, CANborder) {

  # subset land to plot area to speed up plotting
  land <- sf::st_crop(landLayer, regionBbox)

  # subset US-Canad boundary to plot area to speed up plotting
  bound <- sf::st_crop(CANborder, regionBbox)

  # configure the plot
  rawPlot <- ggplot() +
    geom_sf(data = bound, col = "darkgrey", linetype = "dashed", size = 1.1) + # creates US boundary line, 200 nm limit
    geom_sf(data = land, fill = c("lightgrey"), col = "black", size = 0.3) +
    geom_sf(data = studyArea, fill = NA, col = "red", size = 1)

  outPlot <- format_ggplot(rawPlot, regionBbox)

  return(outPlot)
}


#-----------Format Ggplot ----------
# Function that takes a ggplot object as input and adds preset formatting,
# and axis labels of latitude and longitude, allows all plots
# to have a consistent style.

format_ggplot <- function(ggplotIn, bbox) {

  # convert to degrees, 0.7 is bad lat/long correction factor.
  rotTheta <- (360 /(2* pi)) * atan((bbox$ymax[[1]] - bbox$ymin[[1]]) /
                                      (0.7 * (bbox$xmax[[1]] - bbox$xmin[[1]])))

  ggplotOut <- ggplotIn +
    annotation_custom(grid::textGrob("DFO Internal Use Only", rot = rotTheta,
                                     gp = grid::gpar(fontsize = 30, alpha = 0.5,
                                                   col = "grey70",
                                                   fontface = "bold")),
                      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    annotation_scale(location = "bl") +
    theme_bw() +
    labs(x = "",
         y = "",
         col = "")  +
    theme(axis.text.x = element_text(size = 14)) +
    theme(axis.text.y = element_text(size = 14))


  # crop to bbox if used:
  if (class(bbox) == "bbox") {
    ggplotOut <- ggplotOut + coord_sf(xlim = c(bbox[["xmin"]], bbox[["xmax"]]),
                                      ylim = c(bbox[["ymin"]], bbox["ymax"]),
                                      expand = FALSE)

  }

  return(ggplotOut)
}

