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
# SCORING #
###########

# Atomic

## Score calculation
score = function(ds, solution){
  y=rep(0, ds$p_part)
  
  for(i in 1:ds$p_part){
    y[i] = ( sum( ds$data[solution == (i-1)] ) - ds$mu ) ^ 2
  }
  
  return( sum( y ) )
}

## Score calculation for a population
score_popu = function(ds, population){
  y=rep(0,nrow(population))
  
  for (i in 1:nrow(population)){
    y[i]=score(ds, population[i,])
  }
  
  return(y)
}

# Super

## Change of cost calculation for a single coordinate change application
cost_single_change = function(ds, solution, index, group){
  # Find only the groups you want
  old_load = sum(ds$data[solution == solution[index]])
  new_load = sum(ds$data[solution == group])
  
  # Compute the change of score
  moved_weight = ds$data[index]
  delta_cost = 2 * (moved_weight * (new_load - old_load) + moved_weight^2)
  
  return(delta_cost)
}

## Change of cost calculation for a coordinate exchange application
cost_exchange = function(ds, solution, indexes){
  # Find only the groups you want
  load_1 = sum(ds$data[solution == solution[indexes[1]]])
  load_2 = sum(ds$data[solution == solution[indexes[2]]])
  
  # Compute the change of score
  moved_weights = ds$data[indexes]
  delta_1 = (load_1 - moved_weights[1] + moved_weights[2])^2 - load_1^2
  delta_2 = (load_2 - moved_weights[2] + moved_weights[1])^2 - load_2^2
  
  return(delta_1 + delta_2)
}

