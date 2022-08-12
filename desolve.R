#install.packages("deSolve")
library("deSolve")

chemostat <- function(time, init, parms) {
  with(as.list(c(init, parms)), {
    mu   <- mumax * P/(kp + P)  # Monod equation
    dX <- mu * X - D * X
    dP   <-  D *(P0 - P) - 1/Y * mu * X
    list(c(dX, dP), mu=mu)
  })
}
parms <- c(
  mumax = 0.5,    # 1/d
  kp    = 0.01,   # half saturation constant (mg/L)
  Y     = 41,     # yield coefficient (stoichiometric C:P ratio)
  D     = 0.1,    # 1/d
  P0    = 0.05    # P in inflow (mg/L)
)
times <- seq(0, 40, 0.1)  # (d)
init  <- c(X=0.01, P=0.05) # Phytoplankton C and Phosphorus P (mg/L)

out <- ode(init, times, chemostat, parms)
plot(out)
