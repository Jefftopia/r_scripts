plotHistogram <- function(data,column,title,binw=0.30,h=5,w=5,color=FALSE,density=FALSE,facet=NULL,professional=FALSE) {

    require(ggplot2);
    require(ggthemes);
    
    plot <- ggplot(data, aes_string(x=column)) + 
	    xlab(column) +
	    ggtitle(title);
	    
	if(density & color) {
		plot <- plot + geom_density(fill="navy",alpha=0.7);
	} else if (density) {
		plot <- plot + geom_density(fill="black");
 	} else if (color){
		plot <-	plot + geom_histogram(binwidth=binw, aes(y=..density..,fill=..count..)) + scale_fill_gradient("Count",low="lightblue",high="navy");
	} else {
		plot <-	plot + geom_histogram(binwidth=binw, aes(y=..density..));
	}

	if(!is.null(facet)){
	    facets <- as.formula(paste("~",facet))
	    plot <- plot + facet_wrap(facets);
	}

	if(professional){
		plot <- plot + 
			geom_rangeframe() +
			theme_tufte();
	}
	
	ggsave(file="histo_plot.svg", plot=plot, height=h, width=w);
	
	return(plot);
}

#plotHistogram(data=economics,column="psavert",title="My Plot",professional=TRUE,color=TRUE)
