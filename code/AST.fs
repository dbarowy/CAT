module AST

type Expression =
| Number of double
| Variable of char
| Addition of Expression list
| Multiplication of Expression list
| Exponentiation of Expression * Expression
| Sequence of Expression list