ScatterPlot_AKS <- function (simMatrix, reducedTerms, size = "score", addLabel = TRUE, 
                             labelSize = 3){
  if (!all(sapply(c("ggplot2", "ggrepel"), requireNamespace, 
                  quietly = TRUE))) {
    stop("Packages ggplot2, ggrepel and/or its dependencies not available. ", 
         "Consider installing them before using this function.", 
         call. = FALSE)
  }
  x <- cmdscale(as.matrix(as.dist(1 - simMatrix)), eig = TRUE, k = 2)
  df <- cbind(as.data.frame(x$points), reducedTerms[match(rownames(x$points), 
                                                          reducedTerms$go), c("term", "parent", "parentTerm", 
                                                                              "size")])
  df<-df[df$size>1,]
  df<-df %>%
    group_by(parentTerm) %>%
    mutate(max=max(size)) %>%
    mutate(n = n())
  
  df$lab<-df$parentTerm
  df[!(df$max==df$size),]$lab<-NA
  df[df$max<100|df$n<5,]$lab<-NA
  ggplot2::ggplot(df, ggplot2::aes(x = V1, y = V2, color = parentTerm)) + 
    ggplot2::geom_point(ggplot2::aes(size = size), alpha = 0.5) + 
    ggplot2::scale_color_discrete(guide = "none") + ggplot2::scale_size_continuous(guide = "none", 
                                                                                   range = c(0, 25)) + ggplot2::scale_x_continuous(name = "") + 
    ggplot2::scale_y_continuous(name = "") + ggplot2::theme_minimal() + 
    ggplot2::theme(axis.text.x = ggplot2::element_blank(), 
                   axis.text.y = ggplot2::element_blank()) + 
    ggrepel::geom_label_repel(aes(label = lab), box.padding = grid::unit(0.05,"lines"), size = 3,max.overlaps = 50,alpha=0.75)
  
}