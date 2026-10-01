`initialise.list.reaction` <-
function (reac.list, geometry = c("1D", "2D"), name, type = c("kinetic", 
    "equilibrium")) 
{
    N.reac <- length(reac.list) + 1
    reac.list[[N.reac]] <- create.reaction(geometry, name, type)
    names(reac.list)[N.reac] <- name
    return(reac.list)
}
