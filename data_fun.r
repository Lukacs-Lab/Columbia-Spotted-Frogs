		#  A function to create data for jags runs, frogs
		#  02/2015
		#  Lukacs Lab
####################################################################################################
		#  For development
#		eh <- matrix(sample(0:1, 100, replace = T), 10, 10)
####################################################################################################
		#  Function
		data_fun <- function(eh){
			#  Takes a matrix or data.frame of 1's and 0's with one row for each individual
			#   and a column for each occassion
			#  Returns a named list containing the first occassion for each individual, the number
			#   of individuals (nind), the number of occassions (n.occs), the original encounter
			#   history (eh) and initial values for the latent state z (z.init)
			
			#  Find first occassion for each individual, needed for indexing loops in JAGS
			first <- apply(eh, 1, function(x){
				min(which(x == 1))
			})
			
			#  Find number of occs, needed for indexing JAGS loops
			n.occs <- ncol(eh)
			
			#  Find number of individuals, needed to index JAGS loops
			nind <- nrow(eh)
			
			#  Initialize Z, helps JAGS start
			z.init <- matrix(1, nrow = nind, ncol = n.occs)
			for(i in 1:nind){
				z.init[i, 1:first[i]] <- NA
			}
			
		
		list(eh = eh, first = first, n.occs = n.occs, nind = nind, z.init = z.init)
		}
		