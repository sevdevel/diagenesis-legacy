\title{X}
\name{initialise.list.species}
\alias{initialise.list.species}
\keyword{utilities}
\description{X}
\usage{initialise.list.species(spec.list, geometry=c("1D","2D"), name, phase=c("solute","solid","bulk"), type=c("chemical","composite","artificial"), z=0)}
\arguments{
  \item{spec.list }{Species list to be extended with one species object}
  \item{geometry }{Model domain geometry (1D or 2D)}
  \item{name }{Object name }
  \item{phase }{Sedimentary phase (solute, solid, bulk). This defines the concentration basis of the species. For example: O2 is a solute, hence its concentration will be defined per unit of pore water.}
  \item{type }{True chemical species or a composite species. For example: CO2 is a true solute, In contrast, SumCO2 is a composite species (representing the dissolved inorganic carbon SumCO2 = [CO2] + [HCO3] + [CO3]).}
  \item{z }{Electrical charge (e.g. z=+1 for Na+, z=-1 for Cl-) }
}
\value{
   A new spec.list, which is extended with one  component (an object of class "species")" 
}
\author{Filip Meysman}
