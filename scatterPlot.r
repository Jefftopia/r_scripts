plotScatterChart <- function(data,x,y,title,scale=y,h=6,w=6,color=FALSE,line=FALSE,facet=NULL,professional=FALSE) {

    require(ggplot2);
    require(ggthemes);

	plot <- ggplot(data, aes_string(x=x, y=y)) + 
		xlab(x) +
		ylab(y) +
		ggtitle(title);

	if(color) {
		plot <- plot + 
			geom_point(aes_string(color=scale),alpha=0.8) +
			scale_color_gradient(low="lightblue",high="navy");
			
		if(line){
			plot <- plot + 
				geom_smooth(method="lm",fill="red",color="purple")
		}

	} else {
		plot <- plot + 
			geom_point();
	
		if(line){
			plot <- plot +
				geom_smooth(method="lm",color="red");
		}		

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

	ggsave(file="scatter_plot.svg", plot=plot, height=h, width=w);
			
	return(plot);
}

#plotScatterChart(data=diamonds,x="price",y="carat",title="My Graph",facet="cut",color=TRUE,line=TRUE,professional=TRUE)
plotScatterChart(data=diamonds,x="price",y="carat",title="My Graph",facet="cut",professional=TRUE,line=TRUE,color=TRUE)
