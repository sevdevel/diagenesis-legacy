`initialise.list.species` <-
function (spec.list, geometry = c("1D", "2D"), name, phase = c("solute", 
    "solid", "bulk"), type = c("chemical", "composite", "artificial"), 
    z = 0) 
{
    N.spec <- length(spec.list) + 1
    spec.list[[N.spec]] <- create.species(geometry, name, phase, 
        type, z)
    names(spec.list)[N.spec] <- name
    return(spec.list)
}
