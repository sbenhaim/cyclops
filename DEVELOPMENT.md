# Shhh Development

## Parsing vs Timing

Patterns as entered by musicians go through a (roughly) two-step process before becoming the collection of events that can be sent to the synth. The first step is "parsing" where the terse syntax used to represent patterns is converted to a more explicit data notation used by the scheduler.

The scheduler then navigates the data structures produced by the parser and applies cycle-relative timing.

The reason for two steps is that timing context is non-local at parse time--sub-patters are affected both by their parent patterns and children patterns.
