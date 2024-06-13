// Lasse Andresen, Nicolai Pavliuc, Shadman Al Islam     13-06-2024

open FsCheck
open TreeVisualizationLib
open TreeLib

// Generate a random tree
let genTree = TreeGenerator.genTree (Arb.generate<string>)

let sampleTree =
    Node("A", [
        Node("B", [
            Node("C", [
                Node("D", [])
                Node("E", [
                    Node("F", [
                        Node("G", [

                        ])
                        Node("H", [
                            Node("I", [])
                            Node("J", [])
                            Node("K", [])
                            Node("L", [])
                        ])
                        Node("M", [])
                        Node("N", [
                            Node("O", [])
                        ])
                    ])
                ])
            ])
            Node("P", [
                Node("Q", [])
                Node("R", [])
            ])
        ])
        Node("S", [
            Node("T", [
                Node("U", [])
                Node("V", [

                ])
                Node("W", [
                    Node("X", [
                        Node("Y", [])
                    ])
                    Node("Z", [
                        Node("a", [])
                        Node("b", [])
                        Node("c", [])
                        Node("d", [])
                    ])
                ])
            ])
            Node("e", [
                Node("f", [
                    Node("g", [])
                ])
                Node("h", [

                ])
                Node("i", [
                    Node("j", [
                        Node("k", [])
                        Node("l", [])
                        Node("m", [])
                        Node("n", [])
                    ])
                ])
            ])
        ])
        Node("o", [
            Node("p", [
                Node("q", [
                    Node("r", [])
                    Node("s", [])
                    Node("t", [])
                    Node("u", [])
                ])
                Node("v", [
                     Node("w", [])
                     Node("x", [
                         Node("y", [])
                         Node("z", [])
                     ])
                     Node("0", [])
                     Node("1", [])
                ])
                Node("2", [])
            ])
        ])
    ])

// let sampleTree = Gen.sample 5 5 genTree
// let resultTree, _ = Tree.design (Node("root", sampleTree))
// let resultTree, _ = Tree.design (exampleTree)
// let resultTree1, _ = Tree.design (sampleTree)
let resultTree1, _ = TreeWithLabelWidthsConsidered.design (sampleTree)

// Plot the tree
VisualizeTree.plot resultTree1
// VisualizeTree.plotWithScale 2 resultTree1

// Property tests
let _ = Check.Quick TreeTests.propOne
let _ = Check.Quick TreeTests.propTwo
let _ = Check.Quick TreeTests.propThree
let _ = Check.Quick TreeWithLabelWidthsConsideredTests.maxLabelWidthShouldBeNonNegative
let _ = Check.Quick TreeWithLabelWidthsConsideredTests.maxLabelWidthShouldBeAtLeastWidthOfRoot
let _ = Check.Quick TreeWithLabelWidthsConsideredTests.maxLabelWidthForSingleNodeShouldBeTheLengthOfLabel
let _ = Check.Quick TreeWithLabelWidthsConsideredTests.maxLabelWidthShouldIncludeMaximumWidthOfChildrensLabels
let _ = Check.Quick TreeWithLabelWidthsConsideredTests.fitShouldBeNonNegative
let _ = Check.Quick TreeWithLabelWidthsConsideredTests.fitIncreasingLabelShouldNotDecreaseFit
let _ = Check.Quick TreeWithLabelWidthsConsideredTests.fitlistlShouldReturnListOfSameLength
let _ = Check.Quick TreeWithLabelWidthsConsideredTests.fitlistlIncreasingLabelShouldNotDecreaseFit
let _ = Check.Quick TreeWithLabelWidthsConsideredTests.fitListIncreasingLabelShouldNotDecreaseFit
let _ = Check.Quick TreeWithLabelWidthsConsideredTests.designWithLabelShouldProduceADifferentResultFromRegularDesign
let _ = Check.Quick TreeWithLabelWidthsConsideredTests.propOne
let _ = Check.Quick TreeWithLabelWidthsConsideredTests.propTwo
let _ = Check.Quick TreeWithLabelWidthsConsideredTests.propThree




