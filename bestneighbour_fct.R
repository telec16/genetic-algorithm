#########################################################################
# k-patition solver (c) by Loup Plantevin and Clementine Delaunay       #
#                                                                       #
# k-patition solver is licensed under a                                 #
# Creative Commons Attribution-ShareAlike 3.0 Unported License.         #
#                                                                       #
# You should have received a copy of the license along with this        #
# work.  If not, see <http://creativecommons.org/licenses/by-sa/3.0/>.  #
#########################################################################


###########
# ACTIONS #
###########

# Atomic

## Single coordinate change
bn_single_change = function(ds, solution){
  index = sample(1:ds$n_data, 1)
  group = sample(0:(ds$p_part - 1), 1)

  delta_score = cost_single_change(ds, solution, index, group)
  if (delta_score < 0) {
    solution[index] = group;
  }

  return(solution)
}

## Coordinates exchange
bn_exchange = function(ds, solution){
  indexes = sample(1:ds$n_data, 2)

  delta_score = cost_exchange(ds, solution, indexes)
  if (delta_score < 0) {
    tmp = solution[indexes[1]]
    solution[indexes[1]] = solution[indexes[2]]
    solution[indexes[2]] = tmp
  }

  return(solution)
}

# Super

## Simple best
do_genetic_experiments = function(ds, population, p){
  for (indiv in 1:nrow(population)) { # Do it for each individual in the population, perform a "best neighbour" step
    for (i in p$n_expe) {
      population[indiv,] = bn_single_change(ds, population[indiv,])
    }
  }

  return(population)
}

## Adaptative neighbouring
do_genetic_adaptation = function(ds, population, p, current_p=NULL){
  current_p = if (is.null(current_p)) p$p_type1 else current_p

  for (indiv in 1:nrow(population)) { # Do it for each individual in the population, perform a "best neighbour" step
    for (i in p$n_expe) {
      what_to_do = runif(1, 0, 1)
      if (what_to_do < current_p) {
        population[indiv,] = bn_single_change(ds, population[indiv,])
      } else {
        population[indiv,] = bn_exchange(ds, population[indiv,])
      }
    }
  }

  return(population)
}

