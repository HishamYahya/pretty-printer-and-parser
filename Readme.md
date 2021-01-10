# Summative Exercise

Exercise for COMP26020 (PL & Paradigms).

### Brief Instructions
* To get started, on any reasonable linux, run (in this directory)
  ./all.sh

  on other systems, make sure cabal is installed, and run
  ```bash
    cabal update
    cabal install --only-dependencies
    cabal build
    cabal run
  ```
  this will install the necessary dependencies, and verify that the code compiles and runs.
  Note: As you have not implemented anything, the automated tests will fail. However, the last line of the output MUST READ:
  ```
    TOTAL ### 0
  ```
* Look at the AST specified in While.hs.
* Implement the pretty-printer by amending Pretty.hs, and the parser by amending Parse.hs. These files also contain more detailed instructions.
* Do not change any of the other files. However, you're welcome to add your own modules/files.
* Use "cabal run" to run the automated tests, and see an estimation of your final mark.
* Commit and push all your changes.
* After the deadline, you'll be contacted by a TA for a short <10min discord session, in which you explain your solution.


## Syntax of the Language
  Semi-formal description. If this does not match the test-cases, it's an error in here, and you should take the test cases!

  Spaces are ignored (except if they separate keywords or ids)

  Ids start with a letter or underscore, and continue with letters, underscores, or digits.
  Numbers start with an optional -, followed by digits
  keywords are composed from lower-case letters.
  Ids or keywords must be separated by non-id characters, i.e.
  ```
    whilex  -- id:"whilex"
    while x -- kw:"while" id:"x"
    738 -- number
    -42 -- number

    78x -- we do not specify if this is valid or not. If you choose it to be valid, parse as: number, id
  ```
  Expressions consist of variables, numbers, unary minus, left-associative binary operators, and can use parenthesis to override default priorities.
  The operator binding priority, from highest to lowest, is as follows
  ```
    unary minus
    *,/
    +,-
    <,<=,==
    &&
    ||
  ```
  Exception: an unary minus must not occur adjacent to another "-", e.g., "--42" and "--x" are invalid expressions!

  commands have the following syntax:
  ```
    id = expr                   -- Assignment
    { c1; ...; cN }             -- Sequential composition
      -- note: {} is the empty sequential composition

    if expr then com else com   -- If-then-else
    while expr do com           -- while
  ```
