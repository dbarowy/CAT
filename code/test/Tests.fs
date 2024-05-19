namespace test

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting

open Combinator
open AST
open Parser
open Evaluator

[<TestClass>]
type TestClass () =
    [<TestMethod>]
    member this.weirdButLegalParse () =
        let program = "1 --   (-  x   ^9 - --   1)"
        match grammar (prepare program) with
        | Success(_, _) -> Assert.IsTrue(true)
        | Failure(_, _) -> Assert.IsTrue(false)

    [<TestMethod>]
    member this.weirdIllegalParse () =
        let program = "1 --   (-  x   ^ - --   1)"
        match grammar (prepare program) with
        | Success(_, _) -> Assert.IsTrue(false)
        | Failure(_, _) -> Assert.IsTrue(true)

    [<TestMethod>]
    member this.numberTest () =
        let program = "1"
        let some_ast = parse program false
        match some_ast with
        | Some (ast) -> 
            Assert.AreEqual 
                ([Number 1],
                (fst (evaluate ast)))
        | None ->
            Assert.IsTrue false
    
    [<TestMethod>]
    member this.variableTest () =
        let program = "x"
        let some_ast = parse program false
        match some_ast with
        | Some (ast) -> 
            Assert.AreEqual 
                ([Variable 'x'],
                (fst (evaluate ast)))
        | None ->
            Assert.IsTrue false
    
    [<TestMethod>]
    member this.additionTest () =
        let program = "1+2"
        let some_ast = parse program false
        match some_ast with
        | Some (ast) -> 
            Assert.AreEqual 
                ([Number 3],
                (fst (evaluate ast)))
        | None ->
            Assert.IsTrue false
    
    [<TestMethod>]
    member this.subtractionTest () =
        let program = "1-2"
        let some_ast = parse program false
        match some_ast with
        | Some (ast) -> 
            Assert.AreEqual 
                ([Number -1],
                (fst (evaluate ast)))
        | None ->
            Assert.IsTrue false

    [<TestMethod>]
    member this.multiplicationTest () =
        let program = "2*3"
        let some_ast = parse program false
        match some_ast with
        | Some (ast) -> 
            Assert.AreEqual 
                ([Number 6],
                (fst (evaluate ast)))
        | None ->
            Assert.IsTrue false
    
    [<TestMethod>]
    member this.divisionTest () =
        let program = "2/10"
        let some_ast = parse program false
        match some_ast with
        | Some (ast) -> 
            Assert.AreEqual 
                ([Number 0.2],
                (fst (evaluate ast)))
        | None ->
            Assert.IsTrue false

    [<TestMethod>]
    member this.expTest () =
        let program = "2^4"
        let some_ast = parse program false
        match some_ast with
        | Some (ast) -> 
            Assert.AreEqual 
                ([Number 16],
                (fst (evaluate ast)))
        | None ->
            Assert.IsTrue false

    [<TestMethod>]
    member this.sqrtTest () =
        let program = "4^.5"
        let some_ast = parse program false
        match some_ast with
        | Some (ast) -> 
            Assert.AreEqual 
                ([Number 2],
                (fst (evaluate ast)))
        | None ->
            Assert.IsTrue false
    
    [<TestMethod>]
    member this.sequenceTest () =
        let program = "1 + 2 \n 3 - 2"
        let some_ast = parse program false
        match some_ast with
        | Some (ast) -> 
            Assert.AreEqual 
                ([Number 3; Number 1],
                (fst (evaluate ast)))
        | None ->
            Assert.IsTrue false
    
    [<TestMethod>]
    member this.expAssocTest () =
        let program = "3^3^2"
        let some_ast = parse program false
        match some_ast with
        | Some (ast) -> 
            Assert.AreEqual 
                ([Number (3.0 ** 9.0)],
                (fst (evaluate ast)))
        | None ->
            Assert.IsTrue false

    [<TestMethod>]
    member this.distributeTest () =
        let program = "3(5 + 4(2 + 1) + 3)"
        let some_ast = parse program false
        match some_ast with
        | Some (ast) -> 
            Assert.AreEqual 
                ([Number 60],
                (fst (evaluate ast)))
        | None ->
            Assert.IsTrue false
    
    [<TestMethod>]
    member this.combineLikeTermsTest () =
        let program = "3x + 5y - x + -2y"
        let some_ast = parse program false
        match some_ast with
        | Some (ast) -> 
            Assert.AreEqual 
                ([
                    Addition [
                        Multiplication [Number 2; Variable 'x'];
                        Multiplication [Number 3; Variable 'y']
                    ]
                ],
                (fst (evaluate ast)))
        | None ->
            Assert.IsTrue false
    
    [<TestMethod>]
    member this.factorTest () =
        let program = "3x - 5y + 3z - 5w"
        let some_ast = parse program false
        match some_ast with
        | Some (ast) -> 
            Assert.AreEqual 
                ([
                    Addition [
                        Multiplication [Number -5; Addition [Variable 'w'; Variable 'y']];
                        Multiplication [Number 3; Addition [Variable 'x'; Variable 'z']]
                    ]
                ],
                (fst (evaluate ast)))
        | None ->
            Assert.IsTrue false

    [<TestMethod>]
    member this.multiplyByOneTest () =
        let program = "1x"
        let some_ast = parse program false
        match some_ast with
        | Some (ast) -> 
            Assert.AreEqual 
                ([
                    Variable 'x'
                ],
                (fst (evaluate ast)))
        | None ->
            Assert.IsTrue false

    [<TestMethod>]
    member this.multiplyByZeroTest () =
        let program = "0x"
        let some_ast = parse program false
        match some_ast with
        | Some (ast) -> 
            Assert.AreEqual 
                ([
                    Number 0
                ],
                (fst (evaluate ast)))
        | None ->
            Assert.IsTrue false
        
    [<TestMethod>]
    member this.addZeroTest () =
        let program = "x + 0"
        let some_ast = parse program false
        match some_ast with
        | Some (ast) -> 
            Assert.AreEqual 
                ([
                    Variable 'x'
                ],
                (fst (evaluate ast)))
        | None ->
            Assert.IsTrue false

    [<TestMethod>]
    member this.factorExpTest () =
        let program = "x^2y^2 + (xy)^2z"
        let some_ast = parse program false
        match some_ast with
        | Some (ast) -> 
            Assert.AreEqual 
                ([
                    Multiplication [
                        Addition [Number 1; Variable 'z']
                        Exponentiation (Multiplication [Variable 'x'; Variable 'y'], Number 2);
                    ]
                ],
                (fst (evaluate ast)))
        | None ->
            Assert.IsTrue false

    // File tests commented out because they are machine specific
    // (the testing library doesn't run from the local directory)
    // [<TestMethod>]
    // member this.fileNumbersTest () =
    //     let program = File.ReadAllText "/Users/michael/Documents/cs334/cs334-project-mjf5/code/test/test_programs/numbers.cat"
    //     let some_ast = parse program false
    //     match some_ast with
    //     | Some (ast) -> 
    //         Assert.AreEqual 
    //             ([Number 3; Number -1; Number 30; Number (7.0/8.0);
    //                 Number 16; Number 132; Number 132; Number 132; Number 10],
    //             (fst (evaluate ast)))
    //     | None ->
    //         Assert.IsTrue false
    
    // [<TestMethod>]
    // member this.fileVariablesTest () =
    //     let program = File.ReadAllText "/Users/michael/Documents/cs334/cs334-project-mjf5/code/test/test_programs/variables.cat"
    //     let some_ast = parse program false
    //     match some_ast with
    //     | Some (ast) -> 
    //         Assert.AreEqual 
    //             ([
    //                 Variable 'x'; 
    //                 Addition [Variable 'y'; Multiplication [Number 2; Variable 'x']];
    //                 Number 0;
    //                 Addition [
    //                     Exponentiation (Variable 'x', Number 2);
    //                     Exponentiation (Variable 'y', Number 2);
    //                     Exponentiation (Variable 'z', Number 2)
    //                 ]; 
    //                 Addition [
    //                     Multiplication [
    //                         Number 2;
    //                         Addition [
    //                             Multiplication [
    //                                 Variable 'y';
    //                                 Variable 'z'
    //                             ];
    //                             Multiplication [
    //                                 Variable 'x';
    //                                 Addition [
    //                                     Variable 'y';
    //                                     Variable 'z'
    //                                 ]
    //                             ]
    //                         ]
    //                     ]
    //                     Exponentiation (Variable 'x', Number 2);
    //                     Exponentiation (Variable 'y', Number 2);
    //                     Exponentiation (Variable 'z', Number 2)
    //                 ]; 
    //                 Number -1
    //                 Multiplication [
    //                     Number 4;
    //                     Exponentiation (Number 16, Variable 'x')
    //                 ]],
    //             (fst (evaluate ast)))
    //     | None ->
    //         Assert.IsTrue false
