# Summative Exercise

Summative exercise for COMP26020 (PL & Paradigms).
Submission deadline: 17.01.2021, 23:59

## Assignment
  Write a pretty printer and a parser for a simple While language, using Haskell!

### Brief Instructions
* To get started, on student-VM (or any reasonable linux), run (in this directory)
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
## How to Solve
  See "Grading Remarks" below for grading of partial solutions. The test-cases partially depend on each other.

  Here are some remarks on features:

### Expression Pretty-Printer
  (epr-1) Pretty print expressions, do not print any parenthesis. You can assume that expressions have at most one operator.
  (epr-2) Omit parenthesis only when not required by operator priority.
          You can assume that there are no chains of same-priority operators, like "a+b+c".
  (epr-3) Correctly print all expressions. For chains of same-priority operators, you may print superfluous parentheses.
  (epr-4) operators associate to the left, so omit parenthesis in the "(a+b)+c" - case, but still print them for "a+(b+c)"

### Command Pretty-Printer
  (cpr-1) Pretty-print commands, ignoring indentation
  (cpr-2) Also with correct indentation

### Expression Parser
  (epa-1) correctly parse expressions that do not contain chains of same-priority operators.
  (epa-2) correctly parse expressions produced by your pretty-printer
  (epa-3) correctly parse expressions, including left-associativity of operators, e.g. "a+b+c".

### Command Parser
  (cpa) correctly parse commands, with whatever expression-parser level you have reached

### Dependency Chart of Tests
  This is how the tests (and thus the achievable marks) depend on the features from above:

  | Test Name                           |   Marks    |   Depends on        |
  | -----------------                   | -------    | --------            |
  | "Simple expressions"                |  (2)       | epr-2               |
  | "Random expressions"                |  (1)       | epr-4               |
  | "Random commands (no indentation)"  |  (1)       | epr-4, cpr-1        |
  | "Random commands (with indentation)"|  (1)       | epr-4, cpr-2        |
  |                                     |            |                     |
  |"parse simple exp"                   |  (2)       | epa-1               |
  |"parse simple com"                   |  (1)       | epa-1, cpa, epr-1   |
  |"parse-pretty-inv (exp)"             |  (1)       | epr-4, epa-3        |
  |"parse-pretty-inv (com)"             |  (1)       | cpa, epr-4, epa-3   |



### Recommended Strategy
A good strategy would be:

  1. implement pretty printing for simple expressions, i.e., don't care about omitting parenthesis for left-associativity yet.
    this will give you the "simple expressions" test (2 marks)
  2. implement parsing that works for simple expressions. This will give you the "parse (simple-exp)"-test (2 marks)

  3. implement pretty-printing of commands, not caring about indentation. "random commands (no-indent)" (1 mark)
  3. implement parsing of commands. Note, you need at least simple expressions to work! "parse (simple-com)" (1 mark)

  4. augment your expression pretty-printer to, at least, produce *parseable* expressions for left-associative operators,
  and augment your parser to parse the expressions output by your pretty-printer.
  This will give you the inverse(exp) and inverse(com) tests (2 mark)

  5. make your expression pretty printer produce minimal parentheses in all cases. This will give you the "random expressions" tests (1 marks)

  6. implement correct indentation in command pretty-printer. "random commands (indent)" (1 mark)


## Before Submitting:
  IMPORTANT: A submission that does not compile will automatically result in halving your score!
  (and, as a non-compiling submission is unlikely to be correct, you're score will likely be very low)

  To check if your submission compiles, type (in this directory)
  ```bash
    cabal build
  ```

  If you don't use any extra packages (like parsec), this should work out of the box.
  Otherwise, be sure to update the project configuration in summative.cabal.
  (more info on: <https://cabal.readthedocs.io/en/3.4/getting-started.html#creating-a-new-application>)

  Note: as on the various different architectures out there, different problems are to be expected,
  use the student VM if you run into any trouble on your own box/OS!
  I WILL NOT BE ABLE TO ANSWER QUESTIONS LIKE:
  "I have Windows 10, tried to install cabal and Haskell, and when compiling get some strange error message. What should I do?"
  (THE ANSWER IS: Try again, on the student VM! In a terminal, change to the folder where you downloaded/pulled the assignment, and run
  ```bash
    ./all.sh
  ```
  now everything should be configured.
  )



## Tests
  This repository includes the tests that we will run to semi-automatically grade your submission.
  This means, you can run them yourself prior to submission!
  In this folder, typing
  ```bash
    cabal run
  ```
  should successfully build and run the test program, and output a summary of the tests, like
  ```
    OK ### 2 Simple expressions
    OK ### 1 Random expressions
    OK ### 1 Random commands (no indentation)
    OK ### 1 Random commands (with indentation)
    OK No parse test
    OK ### 2 parse simple exp
    OK ### 1 parse simple com
    +++ OK, passed 100 tests.
    OK ### 1 parse-pretty-inv (exp)
    +++ OK, passed 100 tests.
    OK ### 1 parse-pretty-inv (com)
    TOTAL ### 10
  ```
  if tests fail, you can get more information with passing verbose as command line argument, i.e.
  ```bash
    cabal run verbose
  ```

  but, most likely, you will have to inspect the respective test program (Test_Pretty.hs or Test_Parse.hs)
  to see what test exactly might be going wrong. (You can load them into ghci as usual!)

  Do not modify the test programs! (We will replace them with the original versions anyway, before trying to run your code)


## How to Submit:
  To submit your assignment, push your code to the
  repository in the department GitLab (where you cloned it from!).
  Make sure all changes are pushed. Any modification to the repository past the deadline will be
  considered a late submission.


## Grading
Grading is out of 10.

To get your score, you will have to explain your program to a TA, in a short (<10min) discord session.

If everything builds and all tests run through, you can expect full score.

Otherwise, the score is split 5-5 between pretty-printing and parsing.

### Pretty-Printing:
  Each successful test gives this number of points

  | Test name                  | Marks |
  | ----                       | ----  |
  |simple expressions          |  2    |
  |random expressions          |  1    |
  |random commands (no-indent) |  1    |
  |random commands (indent)    |  1    |

### Parsing:
  Each test gives this number of points (negative points count when test fails, and the parsing part score cannot fall below 0)

  | Test name                  | Marks |
  | ----                       | ----  |
  |no-parse                    | -1    |
  |parse (simple-exp)          |  2    |
  |parse (simple-com)          |  1    |
  |inverse (exp)               |  1    |
  |inverse (com)               |  1    |


## Grading Remarks
  You can still achieve a significant score without fully solving all tasks.
  Here are some guidelines:

  * for "simple expressions", you do not need to implement the left-associativity correctly, as they don't contain such cases.
  Similar, for parsing simple expressions, you don't need to be able to parse sequences of same-priority operators, such as a+b+c.
  with a pretty printer and parser that can only handle simple expressions, you can still achieve 5/10.

  * you can focus on expressions only. With only expression printer and parser, you're still in for 6/10.

  * correctly implementing the indentation rules is worth 1 point. If you struggle with them, just separate every token by a single space,
  and you can still achieve 9/10

## Remarks for Markers (TAs)

(intentionally included in here)

You are to have a short <10min chat with each student, in which they explain their solution to you.
I will try to make the results of the automated tests available to you in advance.
The goal of this session is mainly to verify that the student actually produced the code themselves,
and understands what they did.
Ideally, the student would share their screen, and explain the program to you.

Here are some questions you might want to ask (you don't need to ask all of them!):

### Easy
  * what are parser combinators?
  * what would be the type of a parser for an Int?

  * show me your expression pretty printer! how did you omit unnecessary parentheses?
  * show me your expression parser! how did you make sure operator priorities are respected, e.g., 1+2*3 is parsed as 1+(2*3)?

### Medium
  * show me your exp parser! how did you parse 'chunks' of the same operator associating to the left, e.g. 1+2+3+4 as ((1+2)+3)+4.
  * show me your command pretty printer. How did you handle indentation?
  * how would you modify your pretty-printer to print "if b then c else {}" as "if b then c"?
  * how would you modify your parser to parse "if b then c" as "if b then c else {}"?

### Hard
  * (ref to previous q) When omitting else-parts, can you see any problem with nested ifs? Consider, e.g., the command
    ```
      "if b then if c then com1 else com2"
    ```
  * how would you include error reporting in your parser?






