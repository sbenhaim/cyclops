# Shhh

Implementation of TidalCycles / Strudel, but taking advantage of Clojure's lisp syntax and collection manipulation capabilities.

# Terminology

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

## Pattern Notation

Definition of patterns via pattern functions (representing ops), and events in shorthand notation or event maps.
