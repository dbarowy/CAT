namespace test

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting

open AST
open Parser
open Evaluator

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member this.numbersTest () =
        let program = File.ReadAllText "/Users/michael/Documents/cs334/cs334-project-mjf5/code/test/test_programs/numbers.cat"
        let some_ast = parse program false
        match some_ast with
        | Some (ast) -> 
            Assert.AreEqual 
                ([Number 3; Number -1; Number 30; Number (7.0/8.0);
                    Number 16; Number 132; Number 132; Number 132; Number 10],
                (fst (evaluate ast)))
        | None ->
            Assert.IsTrue false
    
    [<TestMethod>]
    member this.variablesTest () =
        let program = File.ReadAllText "/Users/michael/Documents/cs334/cs334-project-mjf5/code/test/test_programs/variables.cat"
        let some_ast = parse program false
        match some_ast with
        | Some (ast) -> 
            Assert.AreEqual 
                ([
                    Variable 'x'; 
                    Addition [Variable 'y'; Multiplication [Number 2; Variable 'x']];
                    Number 0;
                    Addition [
                        Exponentiation (Variable 'x', Number 2);
                        Exponentiation (Variable 'y', Number 2);
                        Exponentiation (Variable 'z', Number 2)
                    ]; 
                    Addition [
                        Multiplication [
                            Number 2;
                            Addition [
                                Multiplication [
                                    Variable 'y';
                                    Variable 'z'
                                ];
                                Multiplication [
                                    Variable 'x';
                                    Addition [
                                        Variable 'y';
                                        Variable 'z'
                                    ]
                                ]
                            ]
                        ]
                        Exponentiation (Variable 'x', Number 2);
                        Exponentiation (Variable 'y', Number 2);
                        Exponentiation (Variable 'z', Number 2)
                    ]; 
                    Number -1
                    Multiplication [
                        Number 4;
                        Exponentiation (Number 16, Variable 'x')
                    ]],
                (fst (evaluate ast)))
        | None ->
            Assert.IsTrue false
