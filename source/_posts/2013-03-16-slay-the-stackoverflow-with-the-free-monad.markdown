---
layout: post
title: "Slay the StackOverflow with the Free Monad"
date: 2013-03-16 13:10
comments: true
categories: [Scala, programming] 
---

One of the joys of Scala is actually being able to code in a much more functional style than you can in Java. But inevitably as you start down that route you start using the machinery of advanced functional programming: monads, monad transformers and their ilk.

So it's a bit of a shock when what looks like some perfectly innocent code StackOverflows in your face.

<!-- more -->

## A simple example ##



Okay, that's pretty bad, what's going on here? The problem is that we're building up a big Reader object (and a Reader is just a function, remember), which we only actually _execute_ at the end. And the way bind works over Readers means that our final function is built up of a chain of all the functions we used to build it, each of which must call the previous one. Ergo, we build up a big stack (proportional to the length of the list!), and so we StackOverflow.

### Uh oh ###

This is bad. We _really_ don't want to have to break out of monadic style arbitrarily where we're worried that we might end up processing a lot of data. And we just generally don't want our programs to die depending on the length of the data involved!

(Yes, we'd be okay if the Scala compiler had better tail-call optimization, but at the moment it only optimize self-recursive calls, and in this case we are calling into a legitimately different function each time: we just have a lot of them!)

### A solution? ### 

I acutally hit this problem at work, and I popped into the #scalaz IRC room to seek help. Naturally, what I got was a somewhat cryptic assertion from Tony Morris that "the Free monad" was what I needed, but this was enough to put me on the trail. And yes, it does solve our problem, but as ever, it's no fun if we don't delve into the theory for a bit. So humour me for a while (or you can skip ahead to the bit where I tell you how to fix the problem).

## Category theory time ##

Yep, as ever, the terminology leads us smack bang into category theory territory. I'm going to assume a bit more knowledge this time, if you're not familiar with the basics up to functors (and how they relate to Scala), then you might want to brush up now. I explain the basics in a [previous post](http://www.termsandtruthconditions.com/blog/2012/12/29/covariance-and-contravariance-in-scala/) as well.

Free functors, loosely, are functors from one category to another that add "as little structure as possible". For example, the free monoid functor, \\(F\_{Mon}: \mathbf{Set} \rightarrow \mathbf{Mon}\\), is the functor that takes a set of elements to the "list monoid" on that set. We all know that list is a monoid:

- Singleton sets are the basic elements.
- The monoid operation is appending sets.
- The zero of the monoid operation is the empty set.

But in a sense list is "the most basic" monoid. You can describe stuff that happens in any monoid in terms of a list of the elements involved and the "real" monoid operation: you just fold the operation over the list! A list just, well, lists the elements that were put together but "doesn't decide" which operation to use to put them together. And that's the sense in which lists are the free monoids over sets.

## Freedom and Forgetfulness ##

We can pin down what a free functor has to do a bit more precisely, however. Free functors have a relationship with another kind of functor: forgetful functors. The forgetful functor is really boring: it just takes an object and "forgets" some of its structure. So the forgetful functor \\(U\_{Mon}: \mathbf{Mon} \rightarrow \mathbf{Set}\\) just takes a monoid and gives you the set of its elements. It "forgets" that there was an operation on them.

The final concept we need is that of two functors being "adjoint". This is a bit of a complicated one, but given \\(F: \mathcal{C} \rightarrow \mathcal{D}\\) and \\(G: \mathcal{D} \rightarrow \mathcal{C}\\), we say that \\(F\\) is left-adjoint to \\(G\\) (or vice versa with "right adjoint") if there is a bijection \\(Hom(X, FY) \cong Hom(GX, Y)\\) for any X and Y.[^natural]

[^natural]: Technically, these bijections have to be "natural", which really means "behave sensibly", so don't worry about that.

That's a bit of a lot to take in, so let's do an example. Free functors are _left adjoint_ to forgetful functors! And we actually demonstrated that bijection earlier: given a set X, and a monoid Y, if there's a function from X to the set of elements of Y (i.e. \\(g: X \rightarrow U\_{Mon}Y\\), then we can make a function \\(f: Free\_{Mon}X \rightarrow Y\\) by doing (in pseudocode) \\(\lambda x \rightarrow x.map(g).fold(mzero\_{Y})(mappend\_{Y})\\).

That's just like what we described before - going from lists to any monoid by folding, it's just that this time we don't assume that the list monoid and the other monoid have to have the same basic elements: just so long as we've got a mapping between them, we're fine.[^exercise]

[^exercise]: Also, the adjunction requires a bijection, and I've only shown one direction. The reverse is left as an exercise for the reader.

## Back to monads ##

Okay, so much for monoids, what about _monads_? Monads are already functors, what's our free functor going to be, a functor functor?

Damn straight.

This isn't actually scary at all, it's just like saying that we have a type-constructor that takes _two_ type parameters. In this case one of them is going to be a functor. So in Scala we'd have something like:

``` scala
case class FunctorFunctor[F[_], A]
```

In the case of our free monad functor, we're going to claim that, given a functor ```F```, it's going to be a monad in the remaining parameter ```A```.

### The free monad ###

Okay, so what should a free monad look like? It's actually not to difficult to puzzle out. Suppose we have a functor ```F``` and we want to make a monad just from that functor. Well, let's start by just using our functor. Map is pretty easy then:

``` scala
case class Free[S[_], A](run: S[A])(implicit F: Functor[S]) {
    def map[B](f: A => B) : Free[F, B] = 
        F.map(this)(f)
}
```

We've got a functor, might as well use it.

Flatmap is a bit harder. In fact, let's just think about flatten (remember, you just need to define one or the other to have a monad). How on earth are we going to turn a ```Free[S, Free[S, A]]``` into a ```Free[S, A]```? We've just got nothing to work with.

Okay, so we're going to need to define our Free functor a bit differently. We can do whatever we like, though, so what would make it work? Well, how about if a ```Free[S, Free[S, A]]``` just _was_ a ```Free[S, A]```?

``` scala
sealed abstract class Free[S[_], A](implicit F: Functor[S] {
    def map[B](f: A => B) = this match {
        case Return(a) => Return(f(a))
        case Roll(a) => Roll(a.map(_.map(f)))
    }

    def flatten(a: Free[S, Free[S, A]]) = a match {
        // the outer Free can be either a Return or a Roll
        // a has type Free[S, A]
        case Return(a) => a
        // a has type Free[S, Free[S,A]]
//TODO FIX
        case Roll(a) => Roll(a.map(_.flatten))
    }
}
case class Return[S[_] : Functor, A](run: A) extends Free[S, A]
case class Roll[S[_] : Functor, A](run : Free[S, A]) extends Free[S, A]
```

We're going to define ```Free``` as being the union of several cases, so we do that with a sealed abstract class[^trait] and some case classes that extend it. I'm also using the "context bound" syntax in type declarations: ```Thing[A : Foo]``` is exactly the same as saying ```Thing[A](implicit x: Foo[A])```, except you don't get a name for the ```Foo```.

[^trait]: We'd normally use a trait, but we want to be able to take parameters.

So we've got a case for just putting a thing in there straight, and then we can have things that are examples of our functor wrapping something else. Which might be just a value (a ```Return```), or it might be another thing wrapped in a functor (a ```Roll```). This is a bit weird, since it's kind of recursive, but it solves our flattening problem: we don't flatten, we just declare that we've got an instance of ```Roll``` and we're done!

Also, this isn't actually that odd. Consider the recursive definition of list (this isn't exactly how it's done in scala, but near enough):

``` scala
sealed trait List[A]
case class Nil[A] extends List[A]
case class Cons[A](head: A, tail: List[A]) extends List[A]
```

This is actually very similar to our definition of Free! [^mindblown]

[^mindblown]: For the full mind-fuck, bear in mind that monads are monoids in the category of endofunctors. [And list is the free monoid functor](/images/gifs/mind-blown.gif).

### Cool stuff ###

Free monads are pretty cool, because of the properties that come from them being an adjunction, particuarly the "folding" property. That means that you can define a functor that represents stuff that you want to do, wrap it in a free monad, and end up with functions that you can use in monadic style and which produce a kind of "program". That is, it just _lists_ all the things that you've put together, but you can then "interpret" the program by folding over it with an interpreter that takes your functor objects and _does_ something with them.

This is really powerful, and it's particularly cool because you can easily stick on different interpreters, say adding on the possibility of executing your "program" in a concurrent fashion.[^more]

[^more]: If you're interested in this kind of thing, [this](http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html) is an excellent discussion of free monads in the context of interpreters.

## Return to the stack ##

This is all well and good, but how does it help us with our stack problems?





