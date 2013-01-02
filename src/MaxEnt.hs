-- |
-- Use this package to compute maximum entropy distributions given a list of values and
-- list of constraints.
-- 
-- Here is a the example from Probability the Logic of Science
-- 
-- > maxent ([1,2,3], [average 1.5])
-- 
-- Right [0.61, 0.26, 0.11]
-- 
-- The classic dice example
-- 
-- > maxent ([1,2,3,4,5,6], [average 4.5])
-- 
-- Right [.05, .07, 0.11, 0.16, 0.23, 0.34]
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
