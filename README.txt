Hello!

This repo consists of a bunch of predicates that use the power of PROLOG to solve various fun problems.

For the moment, there are:
- A solver for various SudoKu variants: 
(real) SudoKu, KenKen, Unequal, Adjacent, Towers
- A system to tell what bid should a player open with in a game of Contract Bridge (card game)
- An implementation of Dijkstra's algorithm to find a shortest path from a source vertex
- A predicate that finds a hamiltonian path in a graph if it exists

I am currently learning the language, so please be indulgent!

The Prolog Compiler I use is GNU Prolog.

-- Notes about the documentation --

Template:
%% <name>(<mode><variable>[:<type>, ...] is <determinism>
%
% <description line 1>
% <description line 2>
% ...
 
<mode> system:
+  nonvar on entry (normally), i.e., will normally be instantiated to a term that is not an uninstantiated variable (although the term may contain an uninstantiated variable) when the predicate is called. Thought of as input.
-  var on entry (normally), i.e., will normally be uninstantiated when the predicate is called. Thought of as output.
?   Not speciﬁed, i.e., may or may not be instantiated when the predicate is called. Thought of as either input or output or both input and output.
 
<determinism> specifiers:
det      Must succeed exactly once and leave no choice-points.
semidet  Must either fail or succeed exactly once and leave no choice-points.
multi    Must succeed at least once but may leave choice-points
		 on the last success.
nondet   May either fail or succeed any number of times and may leave
		 choicepoints on last success.