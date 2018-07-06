# Notes

Me learning Haskell. The model here is "progress every day." 

## Content

The HaskellBook folder is me going through the [Haskell Book](http://haskellbook.com/)

Other stuff is from [Real World Haskell](http://book.realworldhaskell.org/), 
but is also from before I realized I needed to step back from that and learn
the fundamentals of Haskell from the ground up, from a "can actually do things" 
perspective rather than the "I read [LYAH](http://learnyouahaskell.com/)"
and think I know things perspective. 


## Notes to Haskell Novices

Haskell has many features not found in other languages and for
that reason and others requires a much firmer learning approach 
than other languages seem to require. 

Examples: 

* Haskell Language Extensions change the syntax of the file 
  you are looking at making it tough to figure out the syntax you
  know from the syntax the extension has introduced. Worse, "modern"
  Haskell involves having a [working knowledge of more extensions
  than one might imagine](http://dev.stephendiehl.com/hask/#the-benign). 
* Like many Functional languages, Haskell reading requires working
  familiarity with a basic set of functions before you can even 
  tell yourself basic stories about what some code is doing. Examples
  of this include functions like '$', '.', along with data constructors,
  type constructors, tuple constructors, matching etc. 
* Thinking that "Haskell is a Lazy language" really misses the 
  reality that instead of being "eager" or "lazy" Haskell really
  is giving the programmer a much more nuanced and rich control 
  over evaluation techniques than is typically enjoyed in other
  languages. 
* Mathematical concepts, usually hidden in other languages, are 
  not only prominent in Haskell, but are extremely valuable so as to 
  be unavoidable in practice. Examples: the Lambda Calculus, functions, 
  Monoid, Functor, Applicative, Monad and many more. 
* Haskell the language and Haskell the culture places a huge amount 
  of emphasis on abstraction. The result of this is that both commonly
  discuss and implement levels of abstraction unheard of in other
  languages or communities. Examples of this are the abstraction
  ladder from value to function and then Functor, Applicative and Monad, 
  and things like Kinds. 
* In my experience, getting into Haskell with previous language experience did not prepare me for the abstractions, correctness and re-usability the language and libraries commonly express. 

By keeping the above in mind, and working on Haskell every single
day I could, and by subscribing to every [Haskell mailing-list](https://www.haskell.org/mailing-lists), [podcast](https://www.haskellcast.com/) and 
social-media feed, I have begun my journey in a way that I feel 
will lead to Haskell in production. 
