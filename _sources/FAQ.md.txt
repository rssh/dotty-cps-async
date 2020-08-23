## FAQ

* What are the current limitations?
    * all scala constructions are supported in async block. 
* Can we use await inside the argument of the high-order function? (like map or filter).
    * Yes, application developer or library author can provide ‘shifted’  implementation of the high-order function, which accepts  X=>M[Y] instead X=>Y.  This was previously implemented in the limited form in scala-gopher (paper: <https://arxiv.org/abs/1611.00602>)  for scala-2 and in practice, even in limited form, works quite well.
* I want to help with development. Where to start?
    * Open an issue (or select existing unassigned)  on GitHub and provide a preliminary plan for your work.  If you want to consult before choosing a task - ping me directly via e-mail or twitter.
* Is exists a version for scala-2?
    * No



   
