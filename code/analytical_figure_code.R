## Code for figure for analytical results
options(scipen = 999)
library(ggplot2)
library(data.table)

prob = seq(0.01, 0.99, len=100)
relative_contrib = ((1 - prob) / (prob))
corr_vals = seq(0.01, 1, len=100) #c(0.1, 0.25, 0.5, 0.75, 1)
res = data.table(sapply(corr_vals, function(i) relative_contrib*i))
colnames(res) = as.character(corr_vals)
res$prob = prob
res_melt = melt(res, variable.name="corr", value.name="foi", id.var="prob")
res_melt$corr = as.numeric(as.character(res_melt$corr))


# Make a heat plot
p1 = ggplot(data=res_melt) + geom_tile(aes(x=prob, y=corr, fill=log10(foi))) + 
						geom_contour(aes(x=prob, y=corr, z=log10(foi)), breaks=c(0), color="red", linewidth=1) +
						scale_fill_gradient2(midpoint=0) + theme_classic() +
			 			ylab("Pairwise correlation") + 
  						xlab(expression(paste("Area of transmission / total area ", (A[x] / A[tot])))) +
  						guides(fill=guide_colorbar(title="log10(Relative contribution\nof correlation)"))


prob = seq(0.001, 0.1, len=100)
relative_contrib = ((1 - prob) / (prob))
corr_vals = seq(0.001, 1, len=100) #c(0.1, 0.25, 0.5, 0.75, 1)
res = data.table(sapply(corr_vals, function(i) relative_contrib*i))
colnames(res) = as.character(corr_vals)
res$prob = prob
res_melt = melt(res, variable.name="corr", value.name="foi", id.var="prob")
res_melt$corr = as.numeric(as.character(res_melt$corr))


# Make a heat plot
p2 = ggplot(data=res_melt) + geom_tile(aes(x=prob, y=corr, fill=log10(foi))) + 
						geom_contour(aes(x=prob, y=corr, z=log10(foi)), breaks=c(0), color="red", linewidth=1) +
						scale_fill_gradient2(midpoint=0) + theme_classic() +
			 			ylab("Pairwise correlation") + 
  						xlab(expression(paste("Area of transmission / total area ", (A[x] / A[tot])))) +
  						guides(fill=guide_colorbar(title="log10(Relative contribution\nof correlation)"))


ggplot(data=res_melt) + geom_line(aes(x=prob, y=foi, color=corr)) +   
			 geom_hline(aes(yintercept=1), linetype="dashed") +
			 theme_classic(base_size = 12) +
			 labs(colour="Correlation") +
			 annotate("text", x=0.5, y=10, label = "Correlation dominates FOI") +
			 annotate("text", x=0.5, y=0.01, label = "Spatial overlap dominates FOI") +
			 scale_color_manual(values=c('#fee5d9','#fcae91','#fb6a4a','#de2d26','#a50f15')) +
			 ylab("Relative contribution of correlation") + 
  xlab(expression(paste("Area of transmission / total area ", (A[x] / A[tot])))) +
  theme(legend.position = c(0,0.05), legend.justification = c(0,0), legend.background = element_rect(fill = NA))+
			 scale_y_log10(breaks=c(0.001, 0.01, 0.1, 1, 10, 100))

# ggsave("../docs/figures/correlation_analytical_figure.pdf", width=6.5, height=5)