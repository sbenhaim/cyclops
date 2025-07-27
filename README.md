# Shhh

Implementation of TidalCycles / Strudel, but taking advantage of Clojure's lisp syntax and collection manipulation capabilities.

The `reference/` directory contains code bases for TidalCycles, Strudel (a JS variation of Tidal), and Piratidal, an alternate and incomplete Clojure implementation of Tidal.

# Terminology

## Cycle

The smallest unit of repetition in a composition. Musical equivalent to a "bar."

## Loop

A collection of events representing a single loop iteration. May span multiple cycles. Does not contain any nested loops.

For instance, the following `pattern`:

```tidal
bd <sd cr>
```

Can be considered two loops: `bd`, which repeats every 1 cycle on 0, and `sd cr`, which repeats every 2 cycles at 1/2 and 3/2.

Or a single loop translated as: `<[bd sd] [bd cr]>`, which would sound identical but may be represented differently.

## Event

A single musical action having a discrete start and duration, typically converted to an OSC message and sent to Dirt/Dough (except for rests).

## Pattern

A collection of loops which are simultaneously active.

## Loop Order

The number of cycles represented by a loop. Equivalent to 

## Mini String Notation (minis)

A recreation of the Tidal/Strudal mini notation.

## Mini Data Notation (minid)

A port of mini notation into Clojure data to allow for easier programmatic limitation.

## Pattern Representation

The data schema of loops and events in its final form before passed to `shhh/process-pattern`, where timing and explicit loop orders are applied. Can be derived from `minis` or `minid` or produced directly (programatically) as well as manipulated at potentially finer grain than `minid`.
