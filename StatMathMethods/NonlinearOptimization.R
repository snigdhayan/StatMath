# Load libraries
library(ROI)
library(ROI.plugin.alabama)

# Formulate the nonlinear problem
 
#> Maximize a nonlinear objective function with 3 continuous variables: x2*x3 - x1*x3 + x1*x2
 
#> subject to 1 constraint of nonlinear type: x1 - 57*x1*x2 - 28*x2*x3 >= 50000

#> and 3 lower and 3 upper non-standard variable bounds: x1 in (-10, 10), x2 in (-50, 50) and x3 in (-100, 100)


prob <- OP(objective = F_objective(F = function(x) {prod(x[-1]) - prod(x[-2]) - prod(x[-3])}, 
                                   n = 3),
           constraints = F_constraint(F = function(x) {x[1] - 57*x[1]*x[2] - 28*x[2]*x[3]}, 
                                      dir = ">=", 
                                      rhs = 50000,
                                      J = function(x) c(1 - 57*x[2], -57*x[1] - 28*x[3], -28*x[2])), # Jacobian of F_constraint
           bounds = V_bound(lb = c(-10, -50, -100), ub = c(10, 50, 100), nobj = 3L),
           maximum = TRUE)

soln <- ROI_solve(prob, solver = "alabama", start = c(0, 0, 0))
soln

#> Optimal solution found.
#> The objective value is: -1.203534e+03

print(paste(c("x1 =", "x2 =", "x3 ="), soln$solution))

#> "x1 = -10.0000000495302" "x2 = 31.2880047616341"  "x3 = -36.7277188665603"