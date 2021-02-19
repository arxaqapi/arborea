type 'a binary_tree = 
  | Empty
  | Leaf of 'a
  | Node of 'a * 'a binary_tree * 'a binary_tree

let example_tree =
  Node("a", Node("b", Leaf("d"), Leaf("e")), Node("c", Empty, Node("f", Leaf("g"), Empty)))

let postorder_traversal tree = 
  let rec traversal tree depth = match tree with
    | Leaf(s) -> s :: []
    | Node(s, l, r) -> traversal l (depth + 1) @ traversal r (depth + 1) @ [s]
    | Empty -> []
    in traversal tree 0

let () =
    print_endline (String.concat " > " (postorder_traversal example_tree));
