#########################################################################
# k-patition solver (c) by Loup Plantevin and Clementine Delaunay       #
#                                                                       #
# k-patition solver is licensed under a                                 #
# Creative Commons Attribution-ShareAlike 3.0 Unported License.         #
#                                                                       #
# You should have received a copy of the license along with this        #
# work.  If not, see <http://creativecommons.org/licenses/by-sa/3.0/>.  #
#########################################################################


#############
# BIN<->DEC #
#############

# Atomic

## Convert integer to binary
binary = function(dec, n_bit=0) {
  if(n_bit == 1){
    return(dec %% 2)
  }
  
  bin = NULL
  while(dec > 0) {
    bin = c(bin, dec %% 2)
    dec = dec %/% 2
  }
  bin = rev(bin) #Little-Endian
  if(n_bit != 0){
    bin = c(rep(0, n_bit-length(bin)), bin) # Padding
  }
  
  return(bin)
}

## Convert binary to integer
decimal = function(bin) {
  dec = 0
  p = 1
  
  for(bit in rev(bin)){
    dec = dec + bit*p
    p = p*2
  }
  
  return(dec)
}

# Super

## Convert a decimal matrix to a binary one (not uselful with only 2-part)
binarise_popu = function(ds, dec_popu) {
  bin_popu = matrix(0, nrow(dec_popu), ds$n_data*ds$n_bit)
  
  for (i in 1:nrow(dec_popu)){
    b = NULL
    for (j in 1:ds$n_data){
      b = c(b, binary(dec_popu[i,j], ds$n_bit))
    }
    bin_popu[i,] = b
  }
  
  return(bin_popu)
}

## Convert a binary matrix to a decimal one (not uselful with only 2-part)
decimatise_popu = function(ds, bin_popu) {
  dec_popu = matrix(0, nrow(bin_popu), ds$n_data)
  
  for (i in 1:nrow(bin_popu)){
    d = NULL
    for (j in 1:ds$n_data){
      d = c(d, decimal(bin_popu[i, ((j-1)*ds$n_bit+1) : (j*ds$n_bit)]))
    }
    dec_popu[i,] = d
  }
  
  return(dec_popu)
}

