#### Steps diagram in base R ####
x11(width = 9, height = 6, pointsize = 10, bg = "white")
par(fig = c(0/18,3.5/18,6.25/12, 9.75/12), mar = c(0,0,0,0))
plot(A$x,A$y,type = 'n',asp=1,xlab = "", ylab = "",xaxt="n", yaxt="n")
sapply(1:2, \(i) lines(A$x[601:800,i],A$y[601:800,i], col = hcl.colors(2, palette = "Dark 3")[i]))

par(fig = c(4/18, 7.5/18,6.25/12,9.75/12), new=T)
plot(A$x,A$y,type = 'n',asp=1,xlab = "", ylab = "",xaxt="n", yaxt="n")
plot(uds[[2]], col.grid=NA, DF = "PDF", level=0,
     col.level = hcl.colors(2, palette = "Dark 3"),
     col.DF = hcl.colors(2, palette = "Dark 3"), 
     xaxt="n", yaxt="n",labels=c("","0.95",""),add=T)

par(fig = c(8/18, 11/18,8.5/12,11.5/12), new=T)
plot(A$x,A$y,type = 'n',asp=1,xlab = "", ylab = "",xaxt="n", yaxt="n")
with(list(nu = 1/(24*7)), raster::plot(prods[[1]]$UD/nu, 
                                       col = hcl.colors(25, "Plasma"), bty = "n",ann=F,xaxt = 'n', yaxt = 'n', add=T, legend.width=1.8))


par(fig = c(8/18, 11/18,4.5/12,7.5/12), new=T)
plot(A$x,A$y,type = 'n',asp=1,xlab = "", ylab = "",xaxt="n", yaxt="n")
with(list(), raster::plot(prods[[1]]$SD, 
                          col = hcl.colors(25, "Plasma"), bty = "n",ann=F,xaxt = 'n', yaxt = 'n', add=T, legend.width=1.8))


corrast <- prods$`1-2`$UD
corrcoords <- xyFromCell(corrast,as.numeric(colnames(cors$`1-2`$CAB)))
corvals <- DF$cor2
values(corrast) <- 0
corrast[as.numeric(colnames(cors[[1]]$CAB))] <- corvals

par(fig = c(8/18, 11/18,0.5/12,3.5/12), new=T)
pcols <- rgb(colorRamp(hcl.colors(25, "Plasma"))(scale(corvals, center=F, scale = max(corvals))), maxColorValue = 255)
plot(A$x,A$y,type = 'n',asp=1,xlab = "", ylab = "",xaxt="n", yaxt="n")
with(list(), raster::plot(corrast, col = hcl.colors(25, "Plasma") ,  
                          yaxt="n",xaxt="n",add=T, legend.width=1.8))
# sapply(1:2, \(i) lines(A$x[601:800,i],A$y[601:800,i], col = hcl.colors(2, palette = "Pastel 1")[i]))
# points with correlation
par(fig = c(8/18, 11/18,0.5/12,3.5/12), new=T)
points(corrcoords[order(corvals),], pch = 15, col = pcols[order(corvals)], cex=0.8)

DF$FOI <- DF$udp/(1/24/7)+DF$sdp*DF$cor2

foirast <- corrast
values(foirast) <- 0
foivals <- DF$FOI
foirast[as.numeric(colnames(cors[[1]]$CAB))] <- foivals
par(fig = c(12.5/18,16/18,4.25/12,7.75/12), new=T)
plot(A$x,A$y,type = 'n',asp=1,xlab = "", ylab = "",xaxt="n", yaxt="n")
raster::plot(foirast, col = hcl.colors(15, "Rocket", rev=T), 
             add=T, legend.width = 1.8)
pcols <- rgb(colorRamp(hcl.colors(25, "Rocket", rev=T))(scale(foivals, center=F, scale = max(foivals))), maxColorValue = 255)
par(fig = c(12.5/18,16/18,4.25/12,7.75/12), new=T)
# plot.new(); plot.window(c(-100,100),c(-100,100), asp = 1)
points(corrcoords[order(foivals),], pch = 15, col = pcols[order(foivals)], cex=0.9)

#### Analytical figure ####
## Code for figure for analytical results
options(scipen = 999)
library(ggplot2)
library(data.table)

prob = seq(0.01, 0.99, len=100)
relative_contrib = ((1 - prob) / (prob))
corr_vals = c(0.1, 0.25, 0.5, 0.75, 1)
res = data.table(sapply(corr_vals, function(i) relative_contrib*i))
colnames(res) = as.character(corr_vals)
res$prob = prob
res_melt = melt(res, variable.name="corr", value.name="foi", id.var="prob")

ggplot(data=res_melt) + geom_line(aes(x=prob, y=foi, color=corr)) +   
  geom_hline(aes(yintercept=1), linetype="dashed") +
  theme_classic(base_size = 12) +
  labs(colour="Correlation") +
  annotate("text", x=0.5, y=10, label = "Correlation dominates FOI") +
  annotate("text", x=0.5, y=0.01, label = "Spatial overlap dominates FOI") +
  scale_color_manual(values=c('#fee5d9','#fcae91','#fb6a4a','#de2d26','#a50f15')) +
  ylab("Relative contribution of correlation") + 
  xlab(expression(paste("Area of contact / Total area ", (A[x] / A[tot])))) +
  theme(legend.position = c(0,0.05), legend.justification = c(0,0), legend.background = element_rect(fill = NA))+
  scale_y_log10(breaks=c(0.001, 0.01, 0.1, 1, 10, 100))

ggsave("../docs/figures/correlation_analytical_figure.pdf", width=6.5, height=5)

#### example cross-correlation functions ####
x11()
par(mfrow=c(2,2))
par(omi = c(0.4,0.4,0.2,0))
# Two individuals together, going back and forth between two patches
with(list(a = rep(rep(c(1,0),each=10),20)),plot(ccf(a,a, plot = F, lag.max = 40)[0:30,], type='l', ci.col = "gray70"), ci.type = "ma")
mtext("a)", 2, line = 1.5, at=1.45,las=1, cex=1.1)
with(list(a = rep(rep(c(1,0),each=10),20),
          b = rep(rep(c(0,1),each=10),20)),plot(ccf(a,b, plot = F, lag.max = 40)[0:30,], type='l', ci.col = "gray70",xlim = c(0,30)))
mtext("b)", 2, line = 1.5, at=1.45,las=1, cex=1.1)
with(list(a = rep(rep(c(1,0),each=10),20),
          b = rep(rep(c(1,0),each=5),20)),plot(ccf(a,b, plot = F, lag.max = 40)[0:30,], type='l', ci.col = "gray70",xlim = c(0,30)))
mtext("c)", 2, line = 1.5, at=0.2,las=1, cex=1.1)
with(list(a = rbinom(400,1,0.01)),plot(ccf(a,a, plot = F, lag.max = 40)[0:30,], type='l', ci.col = "gray70",xlim = c(0,30)))
mtext("d)", 2, line = 1.5, at=1.2,las=1, cex=1.1)
mtext("Lag",1, outer = T, line = 1, cex=1.3)
mtext("Correlation",2, outer = T, line = 1, cex=1.3)

#### Simulation results ####
outdf <- read.csv("outputs/sim_res_240906.csv")
# Relative FOI w/ vs w/o corr
p1 <- filter(outdf, nu == 0.5, Ax == 25) %>% ggplot(aes(social, foi_full1/mean(foi_ud),group = social))+
  # geom_boxplot(outlier.shape = NA, color = "darkred")+
  geom_jitter(color = "darkred")+
  # geom_boxplot(aes(y = foi_ud/mean(foi_ud)), color = "steelblue")+
  geom_jitter(aes(y = foi_ud/mean(foi_ud)), color = "steelblue")+
  theme_minimal(base_size = 14)+
  labs(x = "Interaction strength", y = "Relative FOI")+
  scale_y_log10(labels = scales::label_comma())
  
p1
# FOI ratio vs HR overlap for interaction strength = 0
p2 <- filter(outdf, social==0, nu == 0.5, Ax==25) %>%
  ggplot(aes(overlap, ((foi_full1-foi_ud)/foi_ud)))+
  geom_hline(yintercept = 1,linetype=2)+
  geom_point()+
  theme_minimal(base_size = 14)+
  labs(x = "Home Range Overlap", y = "FOI ratio")
p2

# FOI ratio vs. decay time
p4 <- filter(outdf, Ax == max(Ax), .by = sim) %>% 
  # filter(social %in% c(0,0.7,0.95,1)) %>% 
  ggplot(aes(1/nu/24,(foi_full2-foi_ud)/foi_ud,color=factor(social)))+
  geom_hline(yintercept = 1, linetype=2)+
  geom_point(show.legend = F, position = position_dodge(width = 0.2))+
  scale_y_log10(labels = scales::label_comma())+
  theme_minimal(base_size = 14)+
  labs(x = "Mean decay time (days)", y = "FOI ratio", color = "Interaction")+  
  scale_color_discrete(type=hcl.colors(4, "BluGrn", rev=T))
p4

# FOI ratio vs. relative cell size
p5 <- filter(outdf, nu==0.125) %>% 
  # group_by(sim) %>% 
  mutate(Atoti = max(Atoti), Atotj = max(Atotj), .by = sim) %>% 
  ggplot(aes(Ax/Atoti/1e4,(foi_full1-foi_ud)/foi_ud, color = factor(social)))+
  # stat_smooth(method = "lm", se = F)+
  geom_point(show.legend = F)+
  theme_minimal(base_size = 14)+
  labs(x = expression(paste("Relative cell size ",A[x]/A[tot]," (%)")), y = "FOI ratio", color = "Interaction")+
  scale_color_discrete(type=hcl.colors(4, "BluGrn", rev=T))+
  # scale_x_continuous(labels = \(x) parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x))))
  scale_x_continuous(labels = \(x) x*100)
# theme(legend.text = element_text(size = 10), legend.title = element_text(size=11))
p5

# FOI ratio vs tracking length
lendb <- read_csv("outputs/sim_res_trklen_240929.csv")
# lendb$social <- rep(rep(c(0,0.7,0.95,1),each = 4), 20)
p.len <- lendb %>% ggplot(aes(steps,(foi_full1-foi_ud)/foi_ud))+
  geom_hline(yintercept = 1, linetype =2)+
  geom_point(aes(color = factor(social)), show.legend = F, position = position_dodge(width = 100))+
  # scale_y_log10()+
  scale_color_manual(values = hcl.colors(4,"BluGrn", rev = T))+
  labs(x = "Tracking time (steps)", y = "FOI ratio")+
  theme_minimal(base_size = 12)
p.len
