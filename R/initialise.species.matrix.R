`initialise.species.matrix` <-
function (spec.list) 
{
    N.chem.spec <- 0
    N.comp.spec <- 0
    chem.spec.names <- NULL
    comp.spec.names <- NULL
    for (i in 1:length(spec.list)) {
        if (spec.list[[i]]$type == "chemical") {
            N.chem.spec <- N.chem.spec + 1
            chem.spec.names <- c(chem.spec.names, spec.list[[i]]$name)
        }
        if (spec.list[[i]]$type == "composite") {
            N.comp.spec <- N.comp.spec + 1
            comp.spec.names <- c(comp.spec.names, spec.list[[i]]$name)
        }
    }
    spec.mat <- matrix(data = 0, nrow = N.comp.spec, ncol = N.chem.spec, 
        dimnames = list(comp.spec.names, chem.spec.names))
    return(spec.mat)
}
