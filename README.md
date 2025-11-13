# (cyc|ops)

```
     ___/\/\/\______________________________________/\/\______________________________________/\/\/\___
    _/\/\/\______/\/\/\/\__/\/\__/\/\____/\/\/\/\__/\/\____/\/\/\____/\/\/\/\______/\/\/\/\____/\/\/\_ 
   _/\/\______/\/\________/\/\__/\/\__/\/\________/\/\__/\/\__/\/\__/\/\__/\/\__/\/\/\/\________/\/\_
  _/\/\/\____/\/\__________/\/\/\/\__/\/\________/\/\__/\/\__/\/\__/\/\/\/\__________/\/\____/\/\/\_
 ___/\/\/\____/\/\/\/\________/\/\____/\/\/\/\__/\/\____/\/\/\____/\/\________/\/\/\/\____/\/\/\___
_______________________/\/\/\/\________________/\/\______________/\/\_____________________________
```


(👁️)

# What?

[Uzu](https://uzu.lurk.org/) (as in [TidalCycles](https://tidalcycles.org/)/[Strudel](https://strudel.cc/)) in Clojure and Overtone.

## What's Uzu

An opinionated paradigm for live-coding music featuring minimal syntax and cycle-based (loop-based) approach.

"Opinionated" because instead of offering low-level scheduling constructs, Uzu langs assume you're building your song by layering loops and optimizes for this with terse syntax for creating, merging, and manipulating loops. IMO, this makes Uzu langs less flexible but more musical (in the way that a looper pedal is more musical than a DAW).

## Why build another in Clojure?

Good question! The OG Uzu lang is Tidal(Cycles), written in Haskell (naturally), and it's great.

For something you can play in a browser, check out Strudel, which is also great!

You should definitely use both of those, and they are currently much more mature than Cyclops.

But neither solution had enough parens for my tastes. Also

- The Clojure REPL was made for live coding.
- In Clojure, we can get pretty close to mini syntax terseness, but using native language constructs.
- Which allows convenient use of Clojure's vast collection-oriented functionality in pattern composition and manipulation.
- And 

## What does it look like?

``` haskell
-- Tidal
d1 $ sound "bd*8" # pan cosine # speed (sine + 0.5)
```

``` javascript
// Strudel
sound("bd*8").pan(cosine).speed(sine.add(0.5))
```

``` clojure
;; Cyclops
(o 1 (s (x 8 :bd)) (pan cos) (speed (+| sin 0.5)))
```

## Differences

1. No mini notation. (I mean, you could add it, but intentionally trying to get on without it.)
2. No signals
   - In Strudel and Tidal, `sine` and `cosine`, and `rand` are "signals", which behave differently from events/haps/params/etc. In Cyclops they are function values (interchangeable with other values like `60` and `:c` and `:bd`). See #3.
3. Fn with values
   - Patterns are made up of values: `[1 2 3]`, `[:a :b :c]`.
   - Clojure being dynamically typed, values can be just about anything.
   - Param functions like `s`, `n`, `nt`, `mnt` provide values meaning, like "this value should be interpreted" as a sound, number, note, or midi note. (They can also transform and/or coerce values, as `mnt` takes a value like `:c4` and turns it into `60`.)
   - Values can be events, which allows overriding (some) event behavior in a pattern.
   - 
3. Fn values
   - Clojure being dynamic, the values that make up patterns can be just about anything (for better or worse.)
   - One of the most interesting things they can be is (are?) `fns`.
     - A `fn` of 0 args is called at the last possible moment, and can be used for dynamic values--`(rand)`--or arbitrary, on-the-beat execution--`#(println "And a" (:beat event))`.
     - A `fn` of 1 arg (typically) takes the value from the left during merge
       - `(+| [1 2 3] [inc #(* 2 %) (str %)])` => `[2 4 "3"]`
       - "Typically" because, while there are (ostensibly) convenient defaults, this behavior depends entirely on the selected/defined merge function.
       - A `fn` of 2 args takes (and perhaps discards) a merge value as its first, and a timing context as its second.
         - This is why `sin` and `cos` can be fn values: They can take 

## What's implemented?

- Integration with Superdirt on SuperCollider
- Many of Tidal's functions
- Many of Strudel's functions

## What's planned?

### Very Likely
- More examples
- Better documentation (isn't it always)
- More song-level constructs
- More of Tidal's functions
- More of Strudel's functions
- Better integration with Overtone instruments

### Likely
- Better loop visualization
- Quil/Processing integration
- StrudelDirt support
- Easier installation

### Maaybe
- Overdirt
- Clojurescript

## Goals
- Great live-coding ergonomics through
  - Powerful REPL
  - Great live and dead documentation
  - Structural editing (power thru parens)
  - Terse syntax without mini-notation

## Non-goals
- Feature parity/compatibility with Tidal/Strudel

## What's in a name?

`Cyclops`, `(cyc|ops)` and `(👁️)` are all currently accepted, though some are easier to type than others.

## Simple vs. Easy

Cyclops is not intended to be used as a low-level library in some larger program. It *is* intended to be typed in real time, maybe even in front of an audience!

As such, in some places, "easy" takes priority over "simple". For example:

``` clojure
(o 1 (s :piano) 
     (nt (+| :c5 (chord :cm))) 
     (pan (+| 2 sin 0.5)))
```

Not a lot of keystrokes, but a whole lot of magic (read assumptions and implicit behavior) is going on.

``` clojure
(o 1 ;; Swaps global `layers`, assoc-ing cycl to 1 key
  (s :piano) ;; Naked `:piano` is assumed to be the pattern `[:piano]`, vector is overloaded `fit`, which is a convenience wrapper around `->FitOp`, then is converted to a `cycl` of events, with the original value coerced to a string via `name` and mapped to the `:s` param.
  (nt (+| :c5 (chord :cm))) ;; `n` is usually variadic, but if it gets a single collection, assumes you wanted to "splat" (i.e., `apply`) it. Naked `:c5` is lifted into a fit pattern, and `nt` converts it to the Dirt note digit `12.0`.
  (pan (+| 2 sin 0.5))) ;; Merge sees scalar values, so lifts them into singleton patterns `[2]`, `[sin]`, and `[0.5]`, merge applies `2` to `sin`, which sin interprets as an amplitude arg, and then adds the result to `0.5`, since it's a plus merge.
```

Take away the magic, and the original could progressively be expanded into equivalent simpler and more explicit versions.

``` clojure
;; No implicit merge op
(o 1 (+| (s :piano) 
         (nt (+| :c5 (chord :cm))) 
         (pan (+| 2 sin 0.5))))


;; No variadic magic or value "lifting"
(o 1 (+| [(s [:piano]) 
          (nt (+| [[:c5] (chord :cm)])) 
          (pan (+| [[2] [sin] [0.5]]))]))
          
;; No sequence overloading
(o 1 (+| [(s (fit [:piano])) 
          (nt (+| [(fit [:c5]) (fit (chord :cm))])) 
          (pan (+| [(fit [2]) (fit [sin]) (fit [0.5])]))]))

;; Names should be descriptive!
(swap! layers assoc 1
  (merge-cycles* left-merge 
                 [(->param :sound parse-sound (->FitOp [:piano])) 
                  (->param :note parse-note (merge-cycles* m/apply-merge [(->FitOp [:c5]) (->FitOp (chord :cm))])) 
                  (->param :pan float (merge-cycles* (m/apply|maths|or|stack-merge +) [(->FitOp [2]) (->FitOp [sin]) (->FitOp [0.5])]))]))
```

All of these are equivalent (and valid, assuming you import the fns from the appropriate namespaces).

This would be pretty appalling in an API, but is hopefuly justifiable in this context.

OTOH, Cyclops strives to be hackable, which means the code base needs to be scrutable. So abominations like `lift`, `lift*`, `smart-splat`, `gimme-vec`, et. al., are consigned to the `cycl.ops` namespace, intended for use during live-coding, not necessarily during development.

Lower-level namespaces should be less terse/magical/surprising.

# Terminology

## Song Components

### Logical

These are (mostly) logical components, and not really related to the architecture of Cyclops.

#### Arrangement: 

The song. Composed of 1 or more sections.

#### Section: 

A part of the song (e.g., chorus, verse, bridge, drop, intro, outro, transition). 
Consists of 1 or more "layers" played simultaneously.

### Technical

These define architectural layers of cyclops. Some may only be important to developing or modifying
cyclops itself and not critical for most end users to understand.

#### Layer
Analogous to a song "track". Equivalent to a "pattern" in Tidal (though that
word is used differently here) and associated with an `orbit` in Superdirt, which groups
effects. A layer is composed of 1 or more merge groups.

#### Param fn

Provides semantics to abstact values represented in patterns. For instance, the number `[6]` could represent
a note or an effect value or a sample number. Only once wrapped in a `control` does gain concrete meaning:

`(n 6)`
`(sz 6)`

#### Cycl

A sequence of events.

#### Pattern

The defining characteristic of "Uzu" langs: A shorthand for representing cyclical musical events:

```haskell
s "bd [sd sd] hh*2"
```

``` clojure
(s :bd [:sd :sd] (x 2 :hh))
```

#### Pattern Op

Functions like `slow`, `cyc`, `rep` defining pattern-specific functionality, usually dictating timing or looping characteristics:

`fit` (aka `[]`): Squeezes contents into a single cycle.
`cyc`: Cycles through contents, one per iteration.

#### Event

A map or record containing:

`start`: Representing cycle-relative timing (1/2)
`length`: Representing cycle-relative event duration (which may not be the same as note duration).
`period`: Representing cycle frequency (1 for every cycle, 2 for every other, etc.)
`params`: An arbitrary map representing what the event *does*. 
  In the the default case--Superdirt--a map representing the parameters passed via OSC to the Superdirt instrument, but open for overloading to any semantics defined by your dispatch target.

#### Value

A (usually musical) value or logic for producing a value, often based on time context:

`60`

`:cm7`

#### Fn Values

A value `fn` that is executed on the beat. Optionally accepts intput values during merge and context during execution.

#### Time context

Context passed to value Fns including but not limited to:

- Which event a value is contained by
- Which param a value represents
- Current cycle number
- Current play time
- Period-realitive cycle number


## Cycle

The smallest unit of repetition in a composition. Musical equivalent to a "bar." Always repeats at `cps` per second, naturally.

## Loop

A collection associated events with a common looping period (i.e, number of cycles). May span multiple cycles. Does not contain any nested loops (of differing periods).

For instance, the following `pattern`:

```clojure
[:bd (cycle :sd :cr)]
```

Can be considered two loops: `bd`, which repeats every 1 cycle on 0, and `sd cr`, which repeats every 2 cycles at 1/2 and 3/2.

Or a single loop translated as: `[(cycle :bd :sr) (cycle :bd :cr)]`, which would sound identical though would have a different internal representation.

Loops are represented as a vector `[period events]`.

## Period

The number of cycles represented by a loop.

## Event

A single musical action having a discrete start and duration, typically converted to an OSC message and sent to Dirt/Dough (except for rests).

Represented in shorthand as a keyword (`:bd`), string (`"cm7"`), number (`7`) or `fn` for dynamic, schedule-time events.

## Event Shorthand

Abbreviated representation of events as keywords, strings, numbers or fns. Converted to event maps during processing.

## Event Maps

Expanded event, which can be created explicitly or expanded from shorthand.

## Segment

A subdivision of a cycle into which events are scheduled.

## Segment Weight

An attribute of an event indicating how many segments it occupies. Default 1.

## Pattern

A collection of simultaneoulsy played loops.

## Mini Notation (minis)

A recreation of the Tidal/Strudal mini notation.

## Ops

Operations applied to one or more events affecting timing or other attributes.

### Op Types

- Op: fit, cyc
- Op*: cyc* , rep*
- OpTx?: range
- Ctrl: n, s
- Cycl: [evt...]
- CyclTx: cycl -> cycl
- Merge: [cycl...] -> cycl
- CtxFn: i ctx -> value

## Pattern Notation

Definition of patterns via pattern functions (representing ops), and events in shorthand notation or event maps.
