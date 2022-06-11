# formform experiments in Clojure

This is a personal repo to document some of my approaches to evaluate Clojure as a suitable host platform for my [formform library](https://www.formform.dev).


## Context

Two years ago, I was increasingly unsatisfied with my old (pure JavaScript) implementation of formform. I originally hacked it together without understanding much about what I was doing (I come from a very different background and programming has been more of a tool to help me do other things I care about).

I saw some people using my [FORM tricorder](https://tricorder.formform.dev) and [FORM plotter](https://www.formform.dev/plotter) (which build on formform) productively to learn/explore [uFORM iFORM](https://uform-iform.info), so there was a need to give this thing more solid foundation.

At the same time, I was intrigued by the functional programming paradigm (like many other JS devs at the time) and found that the rather new OCaml-based compile-to-JS language [ReScript](https://rescript-lang.org) could provide everything I need. So in 2021 I did a [complete re-write](https://github.com/formsandlines/formform/tree/rescript) on formform that I almost finished.

However, more recently I have discovered the beautiful simplicity of Lisp which earlier this year (2022) has lead me to take a closer look at the programming language [Clojure](https://clojure.org). I found its more dynamic and pragmatic approach refreshing after messing around with stubborn type systems and trying to express everything in its own, perfect data structure.

But most of all, I have come to appreciate the simplicity of Clojure and Lisps in general, which strongly reminded me (and not only because of all the parantheses) of how G. Spencer-Brown took great steps to simplify/condense logical form in his [Laws of Form](https://en.wikipedia.org/wiki/Laws_of_Form) and trace the roots of logical thought back to pure distinction. Actually, Clojure is much more differentiated than Scheme (for example) and introduces much more distinctions than it would actually need, but I am not a purist and can appreciate some intelligent pragmatism, which fits formform very well.

Now I am exploring various possibilities to tie Lisp/Clojure and FORM logic together and hopefully arrive at a simpler and more “natural” representation of FORM in a programming language. It remains to be seen if it will replace my ReScript approach, but I see some advantages already. Especially working with the REPL reminded me of how I love using programmable notebooks (like [Observable](https://observablehq.com)) for exploration and research on FORM logic. I also really like the hosted-language philosophy behind Clojure and that it would enable formform to become more of a universal library that can be used for command-line tools, desktop/mobile/web apps, running on the server to power a REPL-based notebook or whatever I can imagine.

This could be a research topic on its own and I am not qualified to make an honest attempt, but I am certainly open to suggestions from anyone who is more experienced in this intersection than I am, since this whole experiment is also a learning opportunity for me and I don’t really know many examples or previous research on this topic. So if you have any advice, I would be very thankful if you drop me a message (open a GitHub issue or whatever you like).


Peter Hofmann, 06/2022
