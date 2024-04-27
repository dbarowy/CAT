module AST

type Expression =
| Number of double
| Variable of char
| Assignment of Expression * Expression
| Sequence of Expression list