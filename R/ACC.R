`ACC` <-
function (RC) 
{
    ACC <- rep(0, ncol(RC))
    for (i in 1:ncol(RC)) ACC[i] <- sqrt(sum(RC[, i]^2)/nrow(RC))
    return(ACC)
}
