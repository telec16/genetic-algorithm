#########################################################################
# k-patition solver (c) by Loup Plantevin and Clementine Delaunay       #
#                                                                       #
# k-patition solver is licensed under a                                 #
# Creative Commons Attribution-ShareAlike 3.0 Unported License.         #
#                                                                       #
# You should have received a copy of the license along with this        #
# work.  If not, see <http://creativecommons.org/licenses/by-sa/3.0/>.  #
#########################################################################


if (!exists("waffle")) install.packages("waffle")
if (!exists("viridis")) install.packages("viridis")

require(lattice)
library(waffle)
library(viridis)

# Utilities
source("debug.R")
source("bindec.R")
# Functions
source("genetic_fct.R")
source("bestneighbour_fct.R")
source("display_fct.R")
source("scoring_fct.R")

AG = function(dataset, workenv, parameters){

  start.time = Sys.time()

  print(levelplot(matrix(dataset$data), xlab="Index", ylab="Charges", scales=list(alternating=0), col.regions=rainbow))

  # Result
  if (!local(envir=workenv, expr={exists("bestSolution")})) {
    workenv$bestSolution = rep(0, dataset$n_data)
  }

  #### HERE THE REAL THINGS BEGIN (maybe)
  population = generate_popu(dataset, parameters)
  print.d("Initial population", population, DBG$LOG)

  ## Here the loop begin...

  current_p = parameters$p_type1
  last_bin_offsprings = NULL
  iteration = 0
  first_min_iteration = 0
  min_score = NA
  while (sum(score_popu(dataset, population) == 0) < 1 && (iteration < parameters$n_generation)){
    iteration = iteration + 1

    print.d("At iteration :", iteration, DBG$LOG)

    ############
    # GENETICS #
    ############

    if (parameters$use_genetic) {
      # Pie !
      selection_pie = get_selection_pie(dataset, population)

      # Mating season
      selected = do_mate(selection_pie)
      print.d("selected :", selected, DBG$VERBOSE)
      if (debug.ok(DBG$VERBOSE)) love_pie(selection_pie,selected)

      # Binarise
      bin_popu = binarise_popu(dataset, population)
      if (debug.ok(DBG$CLAIRE)) plot_bin_pop(bin_popu, "bin_popu")

      # Fertilisation
      bin_offsprings = do_fertilise(bin_popu, selected, parameters)
      if (debug.ok(DBG$CLAIRE)) plot_bin_pop(bin_offsprings, "New offsprings")

      # Mutation
      if (is.null(last_bin_offsprings) || last_bin_offsprings != bin_offsprings){
        prob_muta = parameters$prob_muta
      }else{
        prob_muta = prob_muta + parameters$prob_muta_inc
        current_p = parameters$p_type1
      }

      bin_offsprings = do_mutate(bin_offsprings, prob_muta)
      last_bin_offsprings = bin_offsprings

      if (debug.ok(DBG$CLAIRE)) plot_bin_pop(bin_offsprings, "Mutated offsprings")

      # Decimatise
      offsprings = decimatise_popu(dataset, bin_offsprings)
      colnames(offsprings) = dataset$data
      print.d("offsprings :", offsprings, DBG$VERBOSE)

      # Starvation : only the best are kept
      full_popu = rbind(population, offsprings)
      full_score = score_popu(dataset, full_popu)
      population = do_starve(full_popu, full_score, parameters)
      print.d("population :", population, DBG$VERBOSE)
    }

    ###############
    # HYBRIDATION #
    ###############

    if (parameters$use_hybridation) {
      # Hybridation time : for now as a simple coordinate change
      if ((iteration %% parameters$h_period) == 0){
        population = do_genetic_adaptation(dataset, population, parameters, current_p)
		current_p = parameters$p_decrease_ratio * current_p
	  }
    }

    ########
    # SAVE #
    ########

    cms = min(score_popu(dataset, population))
    if (is.na(min_score) ||  cms < min_score) {
      min_score = cms
      first_min_iteration = iteration
    }

    # Saving
    if (debug.ok(DBG$LOG)) {
      png(sprintf("image/popu_%03d.png", iteration))
      plot_pop(dataset, population, cms)
      dev.off()
    }


  } # End of time : last generation is born


  print.d("At the end, the scores of the last generation are:", score_popu(dataset, population), DBG$LOG)
  print.d("The first minimum iteration:", first_min_iteration, DBG$LOG)
  if (debug.ok(DBG$LOG)) show_genes(binarise_popu(dataset, population), parameters)

  # Save results
  workenv$currentBest = population[which.min(score_popu(dataset, population)),]
  workenv$score = score(dataset, workenv$currentBest)
  workenv$iteration = first_min_iteration
  workenv$time = Sys.time() - start.time

  # Save the best
  if (score(dataset, workenv$bestSolution) >= workenv$score) {
    workenv$bestSolution = workenv$currentBest
  }

  if (debug.ok(DBG$LOG)) {
    print("Our best solution is:")
    bestGroups = display_groups(dataset, workenv$bestSolution)
    print(levelplot(bestGroups, xlab="Groupes", ylab="Charges", col.regions=rainbow, scales=list(alternating=0)))
    waffle_baking(dataset, workenv$bestSolution)
  }

}


