###MAXENT

The maximum entropy method, or MAXENT, is variational approach for computing probability 
distributions given a list of moment, or expected value, constraints.

Here are a link for background info.
On the idea of maximum entropy in general: 
<http://en.wikipedia.org/wiki/Principle_of_maximum_entropy>
 

Use this package to compute discrete maximum entropy distributions over a list of values and
list of constraints.

Here is a the example from Probability the Logic of Science
<http://books.google.com/books?id=tTN4HuUNXjgC&lpg=PA355&ots=H4NxnzJxT0&dq=probability%20the%20logic%20of%20science%2011.38&pg=PA354#v=onepage&q=probability%20the%20logic%20of%20science%2011.38&f=false>

    maxent ([1,2,3], [average 1.5])
   
    Right [0.61, 0.26, 0.11]

The classic dice example

    maxent ([1,2,3,4,5,6], [average 4.5])
    
    Right [.05, .07, 0.11, 0.16, 0.23, 0.34]

One can use different constraints besides the average value there.  

As for why you want to maximize the entropy to find the probability constraint, 
I will say this for now. In the case of the average constraint 
it is a kin to choosing a integer partition with the most interger compositions. 
I doubt that makes any sense, but I will try to explain more with a blog post soon.