# Really just a scratch area for bits of ad-hoc data munging script

library(reshape)
library(MASS)
library(igraph)

lang_corr <- read.csv('/Users/matthew/Work/github/lang_corr.csv', header=TRUE)
cond_prob_mx <- as.matrix(cast(lang_corr, to_lang ~ from_lang, value='correlation', fill=0))

cond_prob_mx <- cond_prob_mx / 100
diag(cond_prob_mx) = 1

prob_ratios <- t(cond_prob_mx) / cond_prob_mx

javascript <- 35
javascript_prob <- 0.2 # from https://github.com/languages although not entirely consistent

probs <- prob_ratios / prob_ratios[,javascript] * javascript_prob
probs[probs == Inf] = NA
probs <- apply(probs, 2, mean, na.rm=TRUE)

# cutoff <- 0.005
# cond_prob_mx <- cond_prob_mx[probs > cutoff, probs > cutoff]
# probs <- probs[probs > cutoff]

joint_prob_mx = t(apply(cond_prob_mx, 1, '*', probs))
joint_prob_mx = (joint_prob_mx + t(joint_prob_mx))/2

cov_mx = joint_prob_mx - outer(probs, probs)
stdevs = sqrt(diag(cov_mx))
cor_mx = cov_mx / outer(stdevs, stdevs)

cor_mx[cor_mx < 0.1] = 0

cor_graph <- graph.adjacency(cor_mx, mode="undirected", weighted=TRUE, diag=FALSE)
V(cor_graph)$weight = probs
write.graph(cor_graph, '/Users/matthew/Work/github/cor_graph.graphml', format='graphml')

cov_graph <- graph.adjacency(cov_mx, mode="undirected", weighted=TRUE, diag=FALSE)
V(cov_graph)$weight = probs
write.graph(cor_graph, '/Users/matthew/Work/github/cov_graph.graphml', format='graphml')

cond_prob_graph <- graph.adjacency(cond_prob_mx, mode="directed", weighted=TRUE, diag=FALSE)
V(cond_prob_graph)$weight = probs
write.graph(cond_prob_graph, '/Users/matthew/Work/github/cond_prob_graph.graphml', format='graphml')

# inv_cond_prob <- 1/cond_prob_mx
# jaccard_dist <- 1 - 1/(inv_cond_prob + t(inv_cond_prob) - 1)

# mds_coords <- isoMDS(jaccard_dist, k=2)
# points <- mds_coords$points
# names <- row.names(mds_coords$points)
# 
# # mds_coords <- cmdscale(cond_prob_mx + t(cond_prob_mx), 2)
# mds_coords <- cmdscale(jaccard_dist, 2)
# points <- mds_coords
# names <- row.names(mds_coords)
# 
# # x <- eigen(1-jaccard_dist)$vectors[,1]
# # y <- eigen(1-jaccard_dist)$vectors[,2]
# 
# plot(points[,1], points[,2], type="n")
# # plot(points[,1], points[,2], pch=20, col=sample(colours(), 20), cex=25*probs**0.5)
# text(points[,1], points[,2], labels = names, cex=sqrt(probs)*4)

ew <- E(cor_graph)$weight
l <- layout.fruchterman.reingold(cor_graph, weights=ew)
l <- layout.circle(cor_graph)
# l <- layout.mds(cor_graph)
# l <- layout.drl(cor_graph, options=list(edge.cut=0.5))
par(bg=gray(0.2))
plot(cor_graph, layout = l,
  vertex.label         = V(cor_graph)$name,
  vertex.size          = sqrt(probs)*80,
  vertex.color         = rgb(0, 0.608, 0.584, 0.9),
  vertex.frame.color   = rgb(0.114, 0.455, 0.443, 1),
  vertex.label.family  = 'Helvetica',
  vertex.label.color   = rgb(1,1,1,0.8),
  # edge.arrow.size    = abs(ew) * 10,
  edge.width           = abs(ew) * 25,
  edge.color           = rgb(1, 0.443, 0, ew/max(ew)*0.8),
  margin               = -0.1
)
