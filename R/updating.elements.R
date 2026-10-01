`updating.elements` <-
function (elt.table, EL, SL) 
{
    for (j in 1:ncol(elt.table)) {
        elt.name <- names(elt.table)[j]
        if (EL[[elt.name]]$geometry == "1D") {
            EL[[elt.name]]$C <- 0
            EL[[elt.name]]$flux.up <- 0
            EL[[elt.name]]$flux.down <- 0
            EL[[elt.name]]$int.kin.reac <- 0
            EL[[elt.name]]$int.eq.reac <- 0
            EL[[elt.name]]$int.reac <- 0
            EL[[elt.name]]$int.irr <- 0
            EL[[elt.name]]$int.ddt <- 0
            EL[[elt.name]]$deficit <- 0
            for (i in 1:nrow(elt.table)) {
                spec.name <- row.names(elt.table)[i]
                EL[[elt.name]]$C <- EL[[elt.name]]$C + elt.table[i, 
                  j] * SL[[spec.name]]$C
                EL[[elt.name]]$flux.up <- EL[[elt.name]]$flux.up + 
                  elt.table[i, j] * SL[[spec.name]]$flux.up
                EL[[elt.name]]$flux.down <- EL[[elt.name]]$flux.down + 
                  elt.table[i, j] * SL[[spec.name]]$flux.down
                EL[[elt.name]]$int.kin.reac <- EL[[elt.name]]$int.kin.reac + 
                  elt.table[i, j] * SL[[spec.name]]$int.kin.reac
                EL[[elt.name]]$int.eq.reac <- EL[[elt.name]]$int.eq.reac + 
                  elt.table[i, j] * SL[[spec.name]]$int.eq.reac
                EL[[elt.name]]$int.reac <- EL[[elt.name]]$int.reac + 
                  elt.table[i, j] * SL[[spec.name]]$int.reac
                EL[[elt.name]]$int.irr <- EL[[elt.name]]$int.irr + 
                  elt.table[i, j] * SL[[spec.name]]$int.irr
                EL[[elt.name]]$int.ddt <- EL[[elt.name]]$int.ddt + 
                  elt.table[i, j] * SL[[spec.name]]$int.ddt
                EL[[elt.name]]$deficit <- EL[[elt.name]]$deficit + 
                  elt.table[i, j] * SL[[spec.name]]$deficit
            }
        }
        if (EL[[elt.name]]$geometry == "2D") {
            EL[[elt.name]]$int.flux.x.up <- 0
            EL[[elt.name]]$int.flux.x.down <- 0
            EL[[elt.name]]$int.flux.y.up <- 0
            EL[[elt.name]]$int.flux.y.down <- 0
            EL[[elt.name]]$int.kin.reac <- 0
            EL[[elt.name]]$int.eq.reac <- 0
            EL[[elt.name]]$int.reac <- 0
            EL[[elt.name]]$int.irr <- 0
            EL[[elt.name]]$int.ddt <- 0
            EL[[elt.name]]$deficit <- 0
            for (i in 1:nrow(elt.table)) {
                spec.name <- row.names(elt.table)[i]
                EL[[elt.name]]$int.flux.x.up <- EL[[elt.name]]$int.flux.x.up + 
                  elt.table[i, j] * SL[[spec.name]]$int.flux.x.up
                EL[[elt.name]]$int.flux.x.down <- EL[[elt.name]]$int.flux.x.down + 
                  elt.table[i, j] * SL[[spec.name]]$int.flux.x.down
                EL[[elt.name]]$int.flux.y.up <- EL[[elt.name]]$int.flux.y.up + 
                  elt.table[i, j] * SL[[spec.name]]$int.flux.y.up
                EL[[elt.name]]$int.flux.y.down <- EL[[elt.name]]$int.flux.y.down + 
                  elt.table[i, j] * SL[[spec.name]]$int.flux.y.down
                EL[[elt.name]]$int.kin.reac <- EL[[elt.name]]$int.kin.reac + 
                  elt.table[i, j] * SL[[spec.name]]$int.kin.reac
                EL[[elt.name]]$int.eq.reac <- EL[[elt.name]]$int.eq.reac + 
                  elt.table[i, j] * SL[[spec.name]]$int.eq.reac
                EL[[elt.name]]$int.reac <- EL[[elt.name]]$int.reac + 
                  elt.table[i, j] * SL[[spec.name]]$int.reac
                EL[[elt.name]]$int.irr <- EL[[elt.name]]$int.irr + 
                  elt.table[i, j] * SL[[spec.name]]$int.irr
                EL[[elt.name]]$int.ddt <- EL[[elt.name]]$int.ddt + 
                  elt.table[i, j] * SL[[spec.name]]$int.ddt
                EL[[elt.name]]$deficit <- EL[[elt.name]]$deficit + 
                  elt.table[i, j] * SL[[spec.name]]$deficit
            }
        }
    }
    return(EL)
}
