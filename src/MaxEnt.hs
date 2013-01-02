{-  | Use this package to compute maximum entropy distributions given a list of values and
      list of constraints.
      
      
-}
module MaxEnt (
    Constraint,
    constraint,
    average,
    variance,
    maxent
) where
import MaxEnt.Internal (Constraint,
                        constraint,
                        average,
                        variance,
                        maxent)
