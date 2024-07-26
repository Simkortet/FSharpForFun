module FSharpForFun.StackCalculator
    type Stack = StackContents of float list
    
    let push x (StackContents contents) =
        StackContents (x::contents)
    
    let EMPTY = StackContents []
    let ONE = push 1.0
    let TWO = push 2.0
    let THREE = push 3.0
    let FOUR = push 4.0
    let FIVE = push 5.0
    let START = EMPTY
    
    let pop (StackContents contents) =
        match contents with
        | top::rest ->
            let newStack = StackContents rest
            let topAsDigit = top 
            (topAsDigit,newStack)
        | [] ->
            failwith "Stack underflow"

    let binary operation stack = 
        let a, stack' = pop stack
        let b, stack'' = pop stack'
        push (operation b a) stack''
    
    let ADD = binary (+)
    let SUB = binary (-)
    let MUL = binary (*)
    let DIV = binary (/)
    
    let unary f stack =
        let x,stack' = pop stack  //pop the top of the stack
        push (f x) stack'         //push the function value on the stack

    let NEG = unary (fun x -> -x)

    let SHOW stack =
        let x,_ = pop stack
        printfn $"The answer is %f{x}"
        stack

    /// Duplicate the top value on the stack
    let DUP stack =
        // get the top of the stack
        let x,_ = pop stack
        // push it onto the stack again
        push x stack

    /// Swap the top two values
    let SWAP stack =
        let x,s = pop stack
        let y,s' = pop s
        push y (push x s')
    
    // Using composition
    let SQUARE =
        DUP >> MUL
        
    let CUBE =
        DUP >> DUP >> MUL >> MUL
    
    // Sums numbers from 1 to number using summation formula
    let SUM_NUMBERS_UPTO =
        DUP      // n, n           2 items on stack
        >> ONE   // n, n, 1        3 items on stack
        >> ADD   // n, (n+1)       2 items on stack
        >> MUL   // n(n+1)         1 item on stack
        >> TWO   // n(n+1), 2      2 items on stack
        >> DIV   // n(n+1)/2       1 item on stack

    
    let run =
        START
            |> ONE |> TWO |> SHOW |> ignore 

        START
            |> ONE |> TWO |> ADD |> SHOW
            |> THREE |> ADD |> SHOW |> ignore

        START
            |> THREE |> DUP |> DUP |> MUL |> MUL |> ignore

        START
            |> ONE |> TWO |> ADD |> SHOW  // 3
            |> THREE |> MUL |> SHOW       // 9
            |> TWO |> DIV |> SHOW |> ignore        // 9 div 2 = 4.5
        
        START |> THREE |> SQUARE |> SUM_NUMBERS_UPTO |> SHOW |> ignore // 45
