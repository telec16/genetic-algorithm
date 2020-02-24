#########################################################################
# k-patition solver (c) by Loup Plantevin and Clementine Delaunay       #
#                                                                       #
# k-patition solver is licensed under a                                 #
# Creative Commons Attribution-ShareAlike 3.0 Unported License.         #
#                                                                       #
# You should have received a copy of the license along with this        #
# work.  If not, see <http://creativecommons.org/licenses/by-sa/3.0/>.  #
#########################################################################


###############
# GROUND BASE #
###############

# Initial population
generate_popu = function(ds, p){
    initial_popu = matrix(0, p$n_popu, ds$n_data)
  name_individu = NULL

  for (j in 1:p$n_popu) {
    initial_popu[j,] = round(runif(ds$n_data, 0, ds$p_part - 1))
    name_individu = c(name_individu, paste('N°',format(j)))
  }
  #rownames(initial_popu) = name_individu
  colnames(initial_popu) = ds$data

  return(initial_popu)
}


###########
# SCORING #
###########

# Super

get_selection_pie = function(ds, population){
  # Score and selection calculation
  tab_score = score_popu(ds, population)
  if (max(tab_score) - min(tab_score)) { # Avoid NaN creation when all individuals have the same score
    adaptation = 1 - (tab_score - min(tab_score)) / (max(tab_score) - min(tab_score))
  } else {
    adaptation = rep(1,nrow(population))
  }
  selection = adaptation/sum(adaptation)

  # Selection pie
  selection_pie = rep(selection[1], nrow(population))

  for (i in 2:nrow(population)){
    selection_pie[i] = selection_pie[i-1] + selection[i]
  }

  return(selection_pie)
}


########
# MATE #
########

## Mating season
do_mate = function(selection_pie){
  prob_gen = runif(length(selection_pie), 0, 1)
  # which(t(matrix(rep((selection_pie),10),10,10))>prob_gen, arr.ind = TRUE)
  selected = NULL

  for (arousal in prob_gen) {
    selected = c(selected, which(selection_pie >= arousal)[1])
  }

  return(selected)
}


#################
# FERTILISATION #
#################

# Atomic

## Compute the fusion vector
get_fusion_vect = function(size, p){
  fate = runif(size, 0, 1)
  return(fate < p$p_fusion)
}
## Fertilisation : by mixing one by one every bits
fusion = function(x, y, fusion_vect){
  z = NULL
  for (i in 1:length(x)) {
    z = c(z, if (fusion_vect[i]) x[i] else y[i])
  }
  return(z)
}


## Compute the cross vector
get_cross_vector = function(size, p){
  indexes = c(1, sort(sample(2:size,p$n_cross)), size + 1)
  vect = rep(0,size)
  turn = 0

  for (i in (1:(length(indexes) - 1))){
    l = (indexes[i+1] - indexes[i])
    vect[indexes[i]:(indexes[i+1] - 1)] = rep(turn,l)
    turn = 1 - turn
  }

  return(vect)
}
## Fertilisation : by mixing parts of the parents
cross = function(x, y, cross_vector){
  z = NULL
  for (i in 1:length(x)) {
    z = c(z, if (cross_vector[i]) x[i] else y[i])
  }
  return(z)
}

# Super

## Fertilisation
do_fertilise = function(bin_popu, selected, p){
  bin_offsprings = matrix(0, nrow(bin_popu), ncol(bin_popu))

  for (i in 1:(nrow(bin_popu)/2)) {
    x = bin_popu[selected[i*2-1],]
    y = bin_popu[selected[i*2],]
    if (p$type_fusion == 0) {
      v = get_fusion_vect(length(x), p)
      bin_offsprings[2*i-1,] = fusion(x, y, v)
      bin_offsprings[2*i,] = fusion(y, x, v)
    }
    else if (p$type_fusion == 1) {
      v = get_cross_vector(length(x), p)
      bin_offsprings[2*i-1,] = cross(x, y, v)
      bin_offsprings[2*i,] = cross(y, x, v)
    }
  }

  return(bin_offsprings)
}


##########
# MUTATE #
##########

# Atomic

## Mutation
mutate = function(bin_popu, prob_muta){
  muta = runif(length(bin_popu),0,1)
  return(as.integer( xor(bin_popu, muta <= prob_muta )  ))
}

# Super

## Mutation
do_mutate = function(bin_popu, prob_muta){
  for (i in 1:nrow(bin_popu)) {
    bin_popu[i,] = mutate(bin_popu[i,], prob_muta)
  }

  return(bin_popu)
}


##########
# STARVE #
##########

## Starvation
do_starve = function(population, scores, p){

  # We want a population of n_popu, meaning that we will keep half of the total population
  # of parents and offsprings. So... Median will do the job! Hopefully. To get more exploration,
  # we would keep only the offsprings.

  med = median(scores)

  index = which(scores < med)

  if (length(index) < p$n_popu) {
    index = c(index, which(scores == med)[1:(p$n_popu - length(index))])
  }

  return(population[index,])

}

