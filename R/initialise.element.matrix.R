`initialise.element.matrix` <-
function (spec.list, elt.list) 
{
    N.chem.spec <- 0
    N.elt <- length(elt.list)
    chem.spec.names <- NULL
    elt.names <- names(elt.list)
    for (i in 1:length(spec.list)) {
        if (spec.list[[i]]$type == "chemical") {
            N.chem.spec <- N.chem.spec + 1
            chem.spec.names <- c(chem.spec.names, spec.list[[i]]$name)
        }
    }
    elt.mat <- matrix(data = 0, nrow = N.chem.spec, ncol = N.elt, 
        dimnames = list(chem.spec.names, elt.names))
    return(elt.mat)
}
