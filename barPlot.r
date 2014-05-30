plotBarChart <- function(data,x,title,y=x,h=5,w=5,facet=FALSE,professional=FALSE,color=FALSE) {

	require(ggplot2);
	require(ggthemes);

	# if x is numeric, cut into quantiles, then store as factor; else, factor x  
	if (is.numeric(data[[x]])) {
		quant <- quantile(data[[x]])
		data[[x]] <- factor(cut(data[[x]],quant))
		# colnames(data) <- c("Q1", "Q2", "Q3", "Q4")
	} else {
		data[[x]] <- factor(data[[x]])
	}

	# same for y as x
	if (is.numeric(data[[y]])) {
		quant <- quantile(data[[y]])
		data[[y]] <- factor(cut(data[[y]],quant))
	} else {
		data[[y]] <- factor(data[[y]])
	}

	# facet only makes sense when y != x...
	if(facet) {
		graph <- ggplot(data, aes_string(x=y),fill=y) + 
			 geom_bar(fill=y) + 
			 facet_wrap(as.formula(paste("~", x))) +
			 theme(axis.text.x=element_text(angle=90)) + 
			 ggtitle(title);

	} else {
		graph <- ggplot(data, aes_string(x=x, fill=y)) +
			 geom_bar(fill=y) + 
			 ggtitle(title);
	}

	if(professional){
		graph <- graph + 
			theme_tufte();
	}

    ggsave(filename="bar_plot.svg",plot=graph,height=h,width=w)

    return(graph)
}

plotBarChart(data=diamonds,x="color",title="My Plot",professional=TRUE);
