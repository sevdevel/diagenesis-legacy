`updating.chemical.species` <-
function (SL, PL) 
{
    for (i in 1:length(SL)) {
        if (SL[[i]]$type != "chemical") 
            next
        spec.name <- names(SL)[i]
        if (SL[[spec.name]]$phase == "solute") 
            VF.factor <- PL$por.grid$mid
        if (SL[[spec.name]]$phase == "solid") 
            VF.factor <- PL$svf.grid$mid
        if (SL[[spec.name]]$phase == "bulk") 
            VF.factor <- 1
        SL[[spec.name]]$reac <- SL[[spec.name]]$kin.reac + SL[[spec.name]]$eq.reac
        if (SL[[spec.name]]$geometry == "1D") {
            SL[[spec.name]]$int.eq.reac <- sum(VF.factor * SL[[spec.name]]$eq.reac * 
                PL$grid$dx)
            SL[[spec.name]]$int.kin.reac <- sum(VF.factor * SL[[spec.name]]$kin.reac * 
                PL$grid$dx)
            SL[[spec.name]]$int.reac <- sum(VF.factor * SL[[spec.name]]$reac * 
                PL$grid$dx)
            SL[[spec.name]]$int.irr <- sum(VF.factor * SL[[spec.name]]$irr * 
                PL$grid$dx)
            SL[[spec.name]]$int.ddt <- sum(VF.factor * SL[[spec.name]]$ddt * 
                PL$grid$dx)
            SL[[spec.name]]$deficit <- SL[[spec.name]]$flux.up - 
                SL[[spec.name]]$flux.down + SL[[spec.name]]$int.reac + 
                SL[[spec.name]]$int.irr - SL[[spec.name]]$int.ddt
        }
        if (SL[[spec.name]]$geometry == "2D") {
            SL[[spec.name]]$int.eq.reac <- sum(VF.factor * SL[[spec.name]]$eq.reac * 
                PL$grid$V)
            SL[[spec.name]]$int.kin.reac <- sum(VF.factor * SL[[spec.name]]$kin.reac * 
                PL$grid$V)
            SL[[spec.name]]$int.reac <- sum(VF.factor * SL[[spec.name]]$reac * 
                PL$grid$V)
            SL[[spec.name]]$int.ddt <- sum(VF.factor * SL[[spec.name]]$ddt * 
                PL$grid$V)
            SL[[spec.name]]$int.flux.x.up <- sum(SL[[spec.name]]$flux.x.up * 
                PL$A.grid$x.int[1, ])
            SL[[spec.name]]$int.flux.x.down <- sum(SL[[spec.name]]$flux.x.down * 
                PL$A.grid$x.int[nrow(PL$A.grid$x.int), ])
            SL[[spec.name]]$int.flux.y.up <- sum(SL[[spec.name]]$flux.y.up * 
                PL$A.grid$y.int[, 1])
            SL[[spec.name]]$int.flux.y.down <- sum(SL[[spec.name]]$flux.y.down * 
                PL$A.grid$y.int[, ncol(PL$A.grid$y.int)])
            SL[[spec.name]]$deficit <- SL[[spec.name]]$int.flux.x.up - 
                SL[[spec.name]]$int.flux.x.down + SL[[spec.name]]$int.flux.y.up - 
                SL[[spec.name]]$int.flux.y.down + SL[[spec.name]]$int.reac - 
                SL[[spec.name]]$int.ddt
        }
    }
    return(SL)
}
