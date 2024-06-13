// Lasse Andresen, Nicolai Pavliuc, Shadman Al Islam     13-06-2024

namespace TreeLib

open FsCheck

module TreeTests =
    // Correctness
    let propOne tree =
        let rec propOne' subtree =
            match subtree with
            | []
            | [ _ ] -> true
            | (Node((_, x), xr)) :: (Node((l, y), yr)) :: rest ->
                if x + 1.0 > y then
                    false
                else
                    (propOne' xr) && (propOne' (Node((l, y), yr) :: rest))

        let resultTree, _ = Tree.design tree
        let (Node((_, _), subResultTree)) = resultTree
        propOne' subResultTree


    let propTwo tree =

        let resultTree, _ = Tree.design tree
        let (Node((_, x), subresultree)) = resultTree
        let withIn (tol: float) x y = abs (x - y) < tol

        let rec propTwo' subtree =
            match subtree with
            | [] -> true
            | [ (Node((_, y), _)) ] -> if withIn 1.0E-10 y x then true else false
            | (Node((_, first), subsubtree)) :: rest ->
                let (Node((_, last), _)) = List.last rest

                if withIn 1.0E-10 first (-1.0 * last) then
                    propTwo' subsubtree
                else
                    false

        propTwo' subresultree

    let propThree tree =

        let rec reflect (Node(a, subtree)) =
            Node(a, List.map reflect (List.rev subtree))

        let rec reflectpos (Node((l, x), subtree)) =
            if x = 0.0 then
                Node((l, x), List.map reflectpos subtree)
            else
                Node((l, -1.0 * x), List.map reflectpos subtree)

        let rec compareTrees tree1 tree2 =
            match tree1, tree2 with
            | [], [] -> true
            | [], _
            | _, [] -> false
            | (Node((l1, x1), subtree1)) :: rest1, (Node((l2, x2), subtree2)) :: rest2 ->
                if l1 <> l2 || (x1 <> 0.0 && x2 <> 0.0 && x1 <> -1.0 * x2) then
                    false
                else
                    compareTrees subtree1 (List.rev subtree2) && compareTrees rest1 rest2

        let result, _ = Tree.design tree
        let reflectResult = reflect (reflectpos result)

        let (Node(a, resultTree)) = result
        let (Node(a', reflectTree)) = reflectResult

        compareTrees resultTree (List.rev reflectTree) && a = a'

module TreeWithLabelWidthsConsideredTests =
    //property tests
    let maxLabelWidthShouldBeNonNegative (tree: Tree<string>) =
        let result = TreeWithLabelWidthsConsidered.maxLabelWidth tree
        result >= 0

    let maxLabelWidthShouldBeAtLeastWidthOfRoot (tree: Tree<string>) =
        match tree with
        | Node(label, ch) ->
            let result = TreeWithLabelWidthsConsidered.maxLabelWidth tree
            result >= String.length label

    let maxLabelWidthForSingleNodeShouldBeTheLengthOfLabel (label: string) =
        let tree = Node(label, [])
        let result = TreeWithLabelWidthsConsidered.maxLabelWidth tree
        result = String.length label

    let maxLabelWidthShouldIncludeMaximumWidthOfChildrensLabels (label: string) (children: Tree<string> list) =
        let tree = Node(label, children)
        let result = TreeWithLabelWidthsConsidered.maxLabelWidth tree

        let maxChildWidth =
            if List.isEmpty children then
                0
            else
                List.map TreeWithLabelWidthsConsidered.maxLabelWidth children |> List.max

        result >= maxChildWidth

    //todo mention that had to modify fit to pass test
    let fitShouldBeNonNegative (ps: Extent) (qs: Extent) (labelWidth: float) =
        TreeWithLabelWidthsConsidered.fit (ps, qs, labelWidth) >= 0.0


    let fitIncreasingLabelShouldNotDecreaseFit (ps: Extent, qs: Extent) =
        let result1 = TreeWithLabelWidthsConsidered.fit (ps, qs, 1.0)
        let result2 = TreeWithLabelWidthsConsidered.fit (ps, qs, 10.0)
        result2 >= result1

    let fitlistlShouldReturnListOfSameLength (es: Extent list, labelWidth: float) =
        let result = TreeWithLabelWidthsConsidered.fitlistl (es, labelWidth)
        List.length result = List.length es

    let fitlistlIncreasingLabelShouldNotDecreaseFit (es: Extent list) =
        let result1 = TreeWithLabelWidthsConsidered.fitlistl (es, 1.0)
        let result2 = TreeWithLabelWidthsConsidered.fitlistl (es, 10.0)
        List.forall2 (fun x y -> y >= x) result1 result2      

    let fitListIncreasingLabelShouldNotDecreaseFit (ps: Extent, qs: Extent) =
        let result1 = TreeWithLabelWidthsConsidered.fit (ps, qs, 1.0)
        let result2 = TreeWithLabelWidthsConsidered.fit (ps, qs, 10.0)
        result2 >= result1
        
    let designWithLabelShouldProduceADifferentResultFromRegularDesign (tree: Tree<string>) =
        let (t1, extents1) = Tree.design tree
        let (t2, extents2) = TreeWithLabelWidthsConsidered.design tree
        
        if (List.length extents1 > 0) then
            let (s1,e1) = List.head extents1
            let (s2,e2) = List.head extents2
            
            e2 >= e1
        else
            true

    let propOne tree =
        let labelWidth = float (TreeWithLabelWidthsConsidered.maxLabelWidth tree)

        let rec propOne' subtree =
            match subtree with
            | [] | [_] -> true 
            | Node((_, x), xr) :: Node((l, y), yr) :: rest ->
                if x + labelWidth + 1.0 > y then //The condition x + labelWidth + 1.0 > y ensures that there is at least a 'labelWidth + 1.0' units of space between consecutive nodes
                    false
                else
                    propOne' xr && propOne' (Node((l, y), yr) :: rest)

        let resultTree, _ = TreeWithLabelWidthsConsidered.design tree
        match resultTree with
        | Node((_, _), subResultTree) -> propOne' subResultTree
        | _ -> true

    let propTwo tree =
        let resultTree, _ = TreeWithLabelWidthsConsidered.design tree
        let (Node((_, x), subresultree)) = resultTree
        let withIn (tol: float) x y = abs (x - y) < tol

        let rec propTwo' subtree = //Checks if first is symmetrically opposite to last within a small tolerance
            match subtree with
            | [] -> true
            | [ (Node((_, y), _)) ] -> if withIn 1.0E-10 y x then true else false
            | (Node((_, first), subsubtree)) :: rest ->
                let (Node((_, last), _)) = List.last rest

                if withIn 1.0E-10 first (-1.0 * last) then
                    propTwo' subsubtree
                else
                    false

        propTwo' subresultree

    let propThree tree =
        let rec reflect (Node(a, subtree)) =
            Node(a, List.map reflect (List.rev subtree))

        let rec reflectpos (Node((l, x), subtree)) =
            if x = 0.0 then
                Node((l, x), List.map reflectpos subtree)
            else
                Node((l, -1.0 * x), List.map reflectpos subtree)

        let rec compareTrees tree1 tree2 =
            match tree1, tree2 with
            | [], [] -> true
            | [], _
            | _, [] -> false
            | (Node((l1, x1), subtree1)) :: rest1, (Node((l2, x2), subtree2)) :: rest2 ->
                if l1 <> l2 || (x1 <> 0.0 && x2 <> 0.0 && x1 <> -1.0 * x2) then
                    false
                else
                    compareTrees subtree1 (List.rev subtree2) && compareTrees rest1 rest2

        let result, _ = TreeWithLabelWidthsConsidered.design tree
        let reflectResult = reflect (reflectpos result)

        let (Node(a, resultTree)) = result
        let (Node(a', reflectTree)) = reflectResult

        compareTrees resultTree (List.rev reflectTree) && a = a'