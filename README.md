# (cyc|ops)

```
     ___/\/\/\______________________________________/\/\______________________________________/\/\/\___
    _/\/\/\______/\/\/\/\__/\/\__/\/\____/\/\/\/\__/\/\____/\/\/\____/\/\/\/\______/\/\/\/\____/\/\/\_ 
   _/\/\______/\/\________/\/\__/\/\__/\/\________/\/\__/\/\__/\/\__/\/\__/\/\__/\/\/\/\________/\/\_  
  _/\/\/\____/\/\__________/\/\/\/\__/\/\________/\/\__/\/\__/\/\__/\/\/\/\__________/\/\____/\/\/\_   
 ___/\/\/\____/\/\/\/\________/\/\____/\/\/\/\__/\/\____/\/\/\____/\/\________/\/\/\/\____/\/\/\___    
_______________________/\/\/\/\________________/\/\______________/\/\_____________________________     
```

Uzu in Clojure + Overtone.

# Terminology

## Song Components

### Logical

These are (mostly) logical components, and not really realted to the architecture of Cyclops.

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

#### Merge Group
1 or more controls + merge op for combining them. Analolgous to the `#`, `+`, and `|+` functions in Tidal.

#### Control

Provides semantics to abstact values represented in patterns. For instance, the number `[6]` could represent
a note or an effect value or a sample number. Only once wrapped in a `control` does gain concrete meaning:

`(n 6)`
`(sz 6)`

#### Cycle

A record representing a collection of events with a common period. Implements the `Cyclic`
protocol for all sliceable constructs.

#### Pattern

The defining characteristic of "Uzu" langs: A shorthand for representing cyclical musical events:

``` clojure
(s :bd [:sd :sd] (x 2 :hh))
```

```
"bd [sd sd] hh*2"
```

#### Op

Functions like `slow`, `cyc`, `rep` defining pattern-specific functionality.

#### Event

A value wrapped in the cycle-relative timing information for when it should start and stop.

#### Value

A (usually musical) value or logic for producing a value, often based on time context:

`60`

`:cm7`

`(irand 10)`

`(sin 0 10 2)`

#### Time context

Where we are in the arrangement in terms of seconds, cycle, and sub-cycle.


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

## Pattern Notation

Definition of patterns via pattern functions (representing ops), and events in shorthand notation or event maps.
