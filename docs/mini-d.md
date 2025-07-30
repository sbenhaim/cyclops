# Mini Data Notation

Of course. Based on the provided Tidal mini-notation grammar, here is a detailed specification for a Clojure data-oriented version.

### **Core Principles**

This specification adheres to the following core principles, established previously:

1.  **Patterns are Data:** The primary representation is pure Clojure/EDN data, which is serializable, analyzable, and portable.
2.  **Sequences are Vectors:** The fundamental structure for a sequence of events is a Clojure `vector` `[...]`.
3.  **Events are Keywords:** Individual sound events are represented by `keywords` (e.g., `:bd`, `:superpiano`).
4.  **Operators are Vectors:** Transformations and generative patterns use a special vector syntax: `[:operator <operand1> <operand2> ...]`.
5.  **Parameters are Maps:** Event-specific modifications (like gain, note, or duration) are attached as a `map` `{}`.

---

### **Clojure Data Specification**

Here is the symbol-by-symbol mapping from Tidal mini-notation to the proposed Clojure data structures.

| Symbol(s) | Tidal Description | Clojure Data Representation |
| :--- | :--- | :--- |
| `~` | Rest | `nil` |
| `[ ]`, `.` | Grouping | Nested `vector` `[...]` |
| `,` | Simultaneous Play (Stack) | `[:stack <pattern1> <pattern2> ...]` |
| `*` | Repeat / Speed Up | `[:* <pattern> <times>]` |
| `/` | Slow Down Rate | `[:slow-by <factor> <pattern>]` |
| `!` | Replicate / Concatenate | `[:rep <times> <pattern>]` |
| `_`, `@` | Elongate Event | `[:event {:duration <steps>}]` |
| `\|` | Random Choice | `set` `#{...}` |
| `< >` | Weave / Alternate | `[:weave <p1> <p2>]` |
| `?` | Randomly Remove (Degrade) | `[:degrade <probability> <pattern>]` |
| `:` | Sample Selection | `[:event {:n <index>}]` |
| `( )` | Euclidean Sequence | `[:euclid <pulses> <steps> <pattern>]` |
| `{ }` | Polymetric Sequence | `[:poly <pattern1> <pattern2> ...]` |
| `%` | Numerical Ratio Modifier | `[:ratio <factor> <pattern>]` |
| `{ }%` | Polymetric Subdivision | `[:poly {:steps <n>} <pattern>]` |

---

### **Detailed Operator Specs**

#### **`~` (Rest)**
* **Clojure:** `nil`
* **Description:** A `nil` value in a sequence represents a single step of silence.
* **Examples:**
    * Tidal: `"~ hh"`
    * Clojure: `[nil :hh]`

#### **`[ ]` and `.` (Grouping)**
* **Clojure:** Nested `vector`
* **Description:** A nested vector groups events. These events will be played sequentially within the time of a single step of the outer vector. The `.` symbol is a shorthand for this and translates identically.
* **Examples:**
    * Tidal: `"[bd sd] hh"` -> Clojure: `[[:bd :sd] :hh]`
    * Tidal: `"bd sd . hh hh"` -> Clojure: `[[:bd :sd] [:hh :hh]]`

#### **`,` (Simultaneous Play / Stack)**
* **Clojure:** `[:stack <pattern1> <pattern2> ...]`
* **Description:** Plays multiple patterns concurrently within the same step.
* **Examples:**
    * Tidal: `"[bd, hh]"` -> Clojure: `[:stack [:bd] [:hh]]`
    * Tidal: `"[bd sd, hh hh hh]"` -> Clojure: `[:stack [:bd :sd] [:hh :hh :hh]]`

#### **`*` (Repeat / Speed Up)**
* **Clojure:** `[:* <pattern> <times>]`
* **Description:** Compresses a pattern to play `<times>` within the duration of one step.
* **Examples:**
    * Tidal: `"bd*2 sd"` -> Clojure: `[[:* :bd 2] :sd]`
    * Tidal: `"[bd sd]*2"` -> Clojure: `[[:* [:bd :sd] 2]]`

#### **`/` (Slow Down Rate)**
* **Clojure:** `[:slow-by <factor> <pattern>]`
* **Description:** Slows a pattern down by a given factor, stretching its duration.
* **Examples:**
    * Tidal: `"bd/2"` -> Clojure: `[:slow-by 2 [:bd]]`
    * Tidal: `"[bd sd]/3"` -> Clojure: `[:slow-by 3 [:bd :sd]]`

#### **`!` (Replicate / Concatenate)**
* **Clojure:** `[:rep <times> <pattern>]`
* **Description:** Repeats a pattern sequentially, extending the total time. Unlike `*`, this does not compress the pattern.
* **Examples:**
    * Tidal: `"bd!3"` -> Clojure: `[:rep 3 :bd]` (Expands to `[:bd :bd :bd]`)
    * Tidal: `"[bd sd]!2"` -> Clojure: `[:rep 2 [:bd :sd]]` (Expands to `[:bd :sd :bd :sd]`)

#### **`_` and `@` (Elongate Event)**
* **Clojure:** `[:event {:duration <steps>}]`
* **Description:** A parameter on an event that makes it last for `<steps>` number of steps.
* **Examples:**
    * Tidal: `"bd _ _"` -> Clojure: `[:bd {:duration 3}]`
    * Tidal: `"superpiano@3"` -> Clojure: `[:superpiano {:duration 3}]`
    * Tidal: `"bd _ ~ sd@2"` -> Clojure: `[[:bd {:duration 2}] nil [:sd {:duration 2}]]`

#### **`|` (Random Choice)**
* **Clojure:** `set` `#{...}`
* **Description:** A Clojure set represents a choice of events. On each step, one item will be chosen at random from the set.
* **Examples:**
    * Tidal: `"[bd|cp|hh]"` -> Clojure: `#{:bd :cp :hh}`
    * Tidal: `"sd [bd|cp]"` -> Clojure: `[:sd #{:bd :cp}]`

#### **`< >` (Weave / Alternate)**
* **Clojure:** `[:weave <pattern1> <pattern2>]`
* **Description:** Interleaves `<pattern1>` with each element of `<pattern2>`.
* **Examples:**
    * Tidal: `"bd <sd hh cp>"`
    * Clojure: `[:weave [:bd] [:sd :hh :cp]]` (Expands to `[:bd :sd :bd :hh :bd :cp]`)

#### **`?` (Randomly Remove / Degrade)**
* **Clojure:** `[:degrade <probability> <pattern>]` or `[:degrade <pattern>]` for a default.
* **Description:** A wrapper that randomly removes events from a pattern. The `?` symbol implies a default 50% chance of removal.
* **Examples:**
    * Tidal: `"bd?"` -> Clojure: `[:degrade 0.5 :bd]`
    * Tidal: `"[bd sd cp hh]?"` -> Clojure: `[:degrade 0.5 [:bd :sd :cp :hh]]`

#### **`:` (Sample Selection)**
* **Clojure:** `[:event {:n <index>}]`
* **Description:** A parameter on an event to select a specific sample from a folder (e.g., `bd:3` might be the fourth bass drum sound).
* **Examples:**
    * Tidal: `"bd:3"` -> Clojure: `[:bd {:n 3}]`
    * Tidal: `"[bd:0 sd:1]"` -> Clojure: `[[:bd {:n 0}] [:sd {:n 1}]]`

#### **`( )` (Euclidean Sequence)**
* **Clojure:** `[:euclid <pulses> <steps> <pattern>]`
* **Description:** Generates a Euclidean rhythm with `<pulses>` spread across `<steps>`, using events from `<pattern>`.
* **Examples:**
    * Tidal: `"bd(3,8)"` -> Clojure: `[:euclid 3 8 :bd]`
    * Tidal: `"[bd sd](5,8)"` -> Clojure: `[:euclid 5 8 [:bd :sd]]`

#### **`{ }` (Polymetric Sequence)**
* **Clojure:** `[:poly <pattern1> <pattern2> ...]`
* **Description:** Divides one cycle into equal parts and plays each sub-pattern within one of those parts.
* **Examples:**
    * Tidal: `"{bd bd, cp cp cp}"` -> Clojure: `[:poly [:bd :bd] [:cp :cp :cp]]`

#### **`%` (Numerical Ratio Modifier)**
* **Clojure:** `[:ratio <factor> <pattern>]`
* **Description:** Acts as a wrapper to modify the speed of the enclosed pattern by a ratio. For `%n`, the factor is `1/n`.
* **Examples:**
    * Tidal: `"bd*4%2"` -> Clojure: `[:ratio 1/2 [:* :bd 4]]`

#### **`{ }%` (Polymetric Subdivision)**
* **Clojure:** `[:poly {:steps <n>} <pattern>]`
* **Description:** A modifier on a polymetric sequence that forces the enclosed pattern to fit into `<n>` subdivisions of a cycle.
* **Examples:**
    * Tidal: `"{bd cp hh}%8"` -> Clojure: `[:poly {:steps 8} [:bd :cp :hh]]`

