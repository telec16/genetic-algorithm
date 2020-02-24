#########################################################################
# k-patition solver (c) by Loup Plantevin and Clementine Delaunay       #
#                                                                       #
# k-patition solver is licensed under a                                 #
# Creative Commons Attribution-ShareAlike 3.0 Unported License.         #
#                                                                       #
# You should have received a copy of the license along with this        #
# work.  If not, see <http://creativecommons.org/licenses/by-sa/3.0/>.  #
#########################################################################


##########
# PRINTS #
##########

## Group display
display_groups = function(ds, solution){
  size = 0
  for (gr in 1:ds$p_part){
    size = max(size, sum(solution == (gr-1)))
  }

  groups = matrix(NA, ds$p_part, size)
  for (gr in 1:ds$p_part){
    # Group + padding
    groups[gr,] = c(ds$data[solution == (gr-1)], rep(NA, (size - sum(solution == (gr-1)))))
    # Printing
    print(sprintf("Group %d of value : %d", gr, sum(groups[gr,], na.rm=TRUE)))
    print(groups[gr,])
  }
  print(sprintf("Score of the solution : %f", score(ds, solution)))

  return(groups)
}

## Group computation
compute_groups = function(ds, solution){
  size = 0
  for (gr in 1:ds$p_part){
    size = max(size, sum(solution == (gr-1)))
  }

  groups = matrix(NA, ds$p_part, size)
  for (gr in 1:ds$p_part){
    # Group + padding
    groups[gr,] = c(ds$data[solution == (gr-1)], rep(NA, (size - sum(solution == (gr-1)))))
  }

  return(groups)
}

#########
# PLOTS #
#########

## Plot the love pie
love_pie = function(selection_pie, selected){
  num = NULL;
  for (i in (1:length(selection_pie))) {
    num = c(num,sum(selected == i))
  }

  for (i in length(selection_pie):2) {
    selection_pie[i] = selection_pie[i] - selection_pie[i-1]
  }

  labs <- paste(1:length(selection_pie), " selected ", num, " times.", sep="")
  pie(selection_pie, labels = labs, col = rev(inferno(length(labs))), main = "Love pie")
}

## Plot genes in a population
show_genes = function(population_bin, p){
  gene_distribution = rep(0, ncol(population_bin))

  for (i in 1:length(gene_distribution)) {
    gene_distribution[i] = sum(population_bin[,i]) / p$n_popu
  }

  plot(gene_distribution,type="h",col='darkgreen',ylab="% presence")

  return(gene_distribution)
}

## Plot the population
plot_pop = function(ds, population, t=""){
  print(levelplot(t(population),
                  xlab="Charges", ylab=paste("Solution", t), scales=list(alternating=0),
                  at=(0:ds$p_part-.5), col.regions=rainbow, colorkey=F))
  #if(t != "") title(t)
}
plot_bin_pop = function(population, t=""){
  print(levelplot(t(population),
                  xlab="Bin value", ylab=paste("Solution", t), scales=list(alternating=0),
                  at=(0:2-.5), col.regions=rainbow, colorkey=F))
  #if(t != "") title(t)
}

# Shuffle the colors of a scale
shuffle_colors = function(colors){
  rng = sample((1:length(colors)),length(colors),replace = FALSE)
  return(colors[rng])
}


## It's waffle time
waffle_baking = function(ds, solution){
  groups = compute_groups(ds, solution)
  grp = NULL
  scale_square = min(ds$data)
  for(i in (1:nrow(groups))){
    grp = as.integer(groups[i,!is.na(groups[i,])])
    #names(grp) = as.character(grp)
    grp = unname(grp)
    png(paste('image/waffle_grp_',format(i),'.png',sep = ""))
    print(waffle(grp/scale_square, rows = 30, size = 1, title = sprintf("Group %d",i), colors = shuffle_colors(viridis(length(grp))), flip = TRUE, legend_pos = "bottom"))
    dev.off()
  }

}

