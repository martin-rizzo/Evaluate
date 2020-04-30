
In this table:

 * The highest precedence operators are at the top of the list.
 * The highest precedence operators are evaluated first.
 * Operators of equal precedence are evaluated from left to right.


### Operator Precedence (C like)


| Pre | Operator  | Description                           |
|-----|-----------|---------------------------------------|
|  12 | + - ! ~   | Unary plus/minus, Logical/Bitwise NOT |
|  11 | * / %     | Mult, div and remainder               |
|  10 | + -       | Addition and subtraction              |
|   9 | << >>     | Bitwise left and right shift          |
|   8 | < <= > >= | Comparation                           |
|   7 | == !=     | Comparation Equal, Not Equal          |
|   6 | &         | Bitwise AND                           |
|   5 | ^         | Bitwise XOR                           |
|   4 | \|        | Bitwise OR                            |
|   3 | &&        | Logical AND                           |
|   2 | \|\|      | Logical OR                            |
|   1 | ?:        | Ternary conditional                   |



Notes:

* Operator precedence determines the order in which the terms of an expression will be evaluated.
* Operators with the highest precedence will be evaluated before operators with lower precedence.
* Operators of equal precedence are evaluated from left to right.
* Operations in parentheses are performed first. 



----

### Legacy Assembler Operator Precedence


| Pre | Operator          | Description                         |
|-----|-------------------|-------------------------------------|
|  12 | + -               | Unary plus/minus                    |
|  11 | * / MOD SHL SHR   | Mult, div, remainder, bitwise shift |
|  10 | + -               | Addition and subtraction            |
|   9 |                   |                                     |
|   8 | EQ NE LT LE GT GE | Comparation                         |
|   7 |                   |                                     |
|   6 | AND               | Bitwise AND                         |
|   5 | XOR               | Bitwise XOR                         |
|   4 | OR                | Bitwise OR                          |


