Treemap_AKS <- function (reducedTerms){
  tm<-treemapPlot(reducedTerms)
  tm_plot_data <- tm$tm %>% 
    # calculate end coordinates with height and width
    mutate(x1 = x0 + w,
           y1 = y0 + h) %>% 
    # get center coordinates for labels
    mutate(x = (x0+x1)/2,
           y = (y0+y1)/2) %>% 
    # mark primary groupings and set boundary thickness
    mutate(primary_group = ifelse(is.na(term), 1.2, .5)) %>% 
    # remove colors from primary groupings (since secondary is already colored)
    mutate(color = ifelse(is.na(term), NA, color))
  
  tm_plot_data$lab<-tm_plot_data$parentTerm
  tm_plot_data[!is.na(tm_plot_data$term),]$lab<-NA
  ggplot(tm_plot_data, aes(xmin = x0, ymin = y0, xmax = x1, ymax = y1)) + 
    # add fill and borders for groups and subgroups
    geom_rect(aes(fill = color, size = primary_group),
              show.legend = FALSE, color = "darkgray",alpha=0.8) +
    scale_fill_identity() +
    # set thicker lines for group borders
    scale_size(range = range(tm_plot_data$primary_group)) +
    # add labels
    ggfittext::geom_fit_text(aes(label = lab), min.size = 1,reflow = TRUE) +
    # options
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_void()
}
