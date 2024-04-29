module AST

type Expression =
| Number of double
| Variable of char
| Addition of Expression list
| Multiplication of Expression * Expression
| Parentheses of Expression
| Sequence of Expression list