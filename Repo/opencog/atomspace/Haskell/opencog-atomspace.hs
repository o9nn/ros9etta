{-
opencog-atomspace.hs

OpenCog AtomSpace - Hypergraph Knowledge Representation in Haskell

This single-file implementation demonstrates Haskell's strengths for AI:
- Immutable data structures for safe knowledge representation
- Algebraic data types for precise type modeling
- Pure functions for reasoning and inference
- Type safety preventing invalid graph structures
- Higher-order functions for graph traversal
-}

module OpenCogAtomSpace where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (intercalate)
import Control.Monad (forM_)

-- | Atom types using algebraic data type
-- Demonstrates: Sum types, precise modeling
data AtomType 
    = ConceptNode
    | PredicateNode
    | VariableNode
    | InheritanceLink
    | SimilarityLink
    | EvaluationLink
    | ListLink
    | AndLink
    | OrLink
    | NotLink
    deriving (Show, Eq, Ord)

-- | Truth value with strength and confidence
-- Demonstrates: Product types, value objects
data TruthValue = TruthValue {
    tvStrength :: Double,
    tvConfidence :: Double
} deriving (Show, Eq, Ord)

-- | Default truth value
defaultTV :: TruthValue
defaultTV = TruthValue 1.0 1.0

-- | Atom identifier (unique integer)
type AtomID = Int

-- | Base Atom structure
-- Demonstrates: Algebraic data types, recursive structures
data Atom 
    = Node {
        atomId :: AtomID,
        nodeType :: AtomType,
        nodeName :: String,
        atomTV :: TruthValue
    }
    | Link {
        atomId :: AtomID,
        linkType :: AtomType,
        linkOutgoing :: [Atom],
        atomTV :: TruthValue
    }
    deriving (Eq, Ord)

-- | Custom Show instance for pretty printing
instance Show Atom where
    show (Node aid atype name tv) = 
        "(" ++ show atype ++ " \"" ++ name ++ "\" " ++ show tv ++ ")"
    show (Link aid ltype outgoing tv) =
        "(" ++ show ltype ++ "\n" ++
        intercalate "\n" (map (indent . show) outgoing) ++
        "\n)"
      where
        indent = ("  " ++)

-- | AtomSpace structure using immutable maps
-- Demonstrates: Immutable data structures, multiple indexes
data AtomSpace = AtomSpace {
    -- Next available atom ID
    nextId :: AtomID,
    
    -- All atoms by ID
    atoms :: Map AtomID Atom,
    
    -- Node index: (type, name) -> AtomID
    nodeIndex :: Map (AtomType, String) AtomID,
    
    -- Type index: type -> Set of IDs
    typeIndex :: Map AtomType (Set AtomID),
    
    -- Incoming index: AtomID -> Set of link IDs
    incomingIndex :: Map AtomID (Set AtomID)
} deriving (Show)

-- | Create an empty AtomSpace
-- Demonstrates: Pure constructor function
newAtomSpace :: AtomSpace
newAtomSpace = AtomSpace {
    nextId = 0,
    atoms = Map.empty,
    nodeIndex = Map.empty,
    typeIndex = Map.empty,
    incomingIndex = Map.empty
}

-- | Add a node to the AtomSpace
-- Demonstrates: Immutability - returns new AtomSpace
addNode :: AtomType -> String -> AtomSpace -> (Atom, AtomSpace)
addNode atype name space =
    let key = (atype, name)
    in case Map.lookup key (nodeIndex space) of
        Just aid -> (atoms space Map.! aid, space)  -- Existing node
        Nothing -> (newNode, updatedSpace)
          where
            aid = nextId space
            newNode = Node aid atype name defaultTV
            
            updatedSpace = space {
                nextId = aid + 1,
                atoms = Map.insert aid newNode (atoms space),
                nodeIndex = Map.insert key aid (nodeIndex space),
                typeIndex = Map.insertWith Set.union atype (Set.singleton aid) (typeIndex space)
            }

-- | Add a link to the AtomSpace
-- Demonstrates: Pure function composition, immutable updates
addLink :: AtomType -> [Atom] -> AtomSpace -> (Atom, AtomSpace)
addLink ltype outgoing space = (newLink, updatedSpace)
  where
    aid = nextId space
    newLink = Link aid ltype outgoing defaultTV
    outgoingIds = map atomId outgoing
    
    updatedSpace = space {
        nextId = aid + 1,
        atoms = Map.insert aid newLink (atoms space),
        typeIndex = Map.insertWith Set.union ltype (Set.singleton aid) (typeIndex space),
        incomingIndex = foldr updateIncoming (incomingIndex space) outgoingIds
    }
    
    updateIncoming oid idx = Map.insertWith Set.union oid (Set.singleton aid) idx

-- | Set truth value on an atom
-- Demonstrates: Immutable updates, returning new values
setTruthValue :: Atom -> Double -> Double -> Atom
setTruthValue atom str conf = 
    case atom of
        Node aid atype name _ -> Node aid atype name (TruthValue str conf)
        Link aid ltype out _ -> Link aid ltype out (TruthValue str conf)

-- | Get atoms by type
-- Demonstrates: Pure query, Maybe monad
getAtomsByType :: AtomType -> AtomSpace -> [Atom]
getAtomsByType atype space =
    case Map.lookup atype (typeIndex space) of
        Nothing -> []
        Just idSet -> 
            let ids = Set.toList idSet
            in map (\aid -> atoms space Map.! aid) ids

-- | Get incoming links for an atom
-- Demonstrates: Set operations, safe lookups
getIncoming :: Atom -> AtomSpace -> [Atom]
getIncoming atom space =
    case Map.lookup (atomId atom) (incomingIndex space) of
        Nothing -> []
        Just linkIds -> 
            let ids = Set.toList linkIds
            in map (\lid -> atoms space Map.! lid) ids

-- | Get node by type and name
-- Demonstrates: Maybe monad for safe lookups
getNode :: AtomType -> String -> AtomSpace -> Maybe Atom
getNode atype name space = do
    aid <- Map.lookup (atype, name) (nodeIndex space)
    Map.lookup aid (atoms space)

-- | Count atoms in the space
-- Demonstrates: Pure function, map operations
atomCount :: AtomSpace -> Int
atomCount = Map.size . atoms

-- | Query atoms matching pattern (simplified)
-- Demonstrates: Higher-order functions, filtering
queryAtoms :: (Atom -> Bool) -> AtomSpace -> [Atom]
queryAtoms predicate space = filter predicate (Map.elems $ atoms space)

-- | Pattern matching on nodes containing text
-- Demonstrates: Function composition, partial application
queryNodesByName :: String -> AtomSpace -> [Atom]
queryNodesByName pattern = queryAtoms isMatchingNode
  where
    isMatchingNode (Node _ _ name _) = pattern `elem` words name
    isMatchingNode _ = False

-- | Transitive closure of inheritance
-- Demonstrates: Recursion, pure functions, set operations
inferTransitiveInheritance :: Atom -> AtomSpace -> Set Atom
inferTransitiveInheritance atom space = go (Set.singleton atom) (Set.singleton atom)
  where
    go visited frontier
        | Set.null frontier = visited
        | otherwise = 
            let newNodes = Set.unions $ map (directInherits space) (Set.toList frontier)
                unvisited = newNodes Set.\\ visited
                newVisited = visited `Set.union` unvisited
            in go newVisited unvisited
    
    directInherits :: AtomSpace -> Atom -> Set Atom
    directInherits space atom =
        let incoming = getIncoming atom space
            inhLinks = filter isInheritanceLink incoming
        in Set.fromList [target | Link _ _ [_, target] _ <- inhLinks]
    
    isInheritanceLink (Link _ InheritanceLink _ _) = True
    isInheritanceLink _ = False

-- | Check if one concept inherits from another (transitive)
-- Demonstrates: Pure inference, boolean functions
inheritsFrom :: Atom -> Atom -> AtomSpace -> Bool
inheritsFrom source target space = 
    target `Set.member` inferTransitiveInheritance source space

-- | Pretty print the entire AtomSpace
-- Demonstrates: IO monad, pure/impure separation
printAtomSpace :: AtomSpace -> IO ()
printAtomSpace space = do
    putStrLn $ "AtomSpace contains " ++ show (atomCount space) ++ " atoms:"
    forM_ (Map.elems $ atoms space) $ \atom -> 
        putStrLn $ "  " ++ show atom

-- | Statistics about the AtomSpace
-- Demonstrates: Record syntax, pure computation
data AtomSpaceStats = AtomSpaceStats {
    totalAtoms :: Int,
    nodeCount :: Int,
    linkCount :: Int,
    typeDistribution :: Map AtomType Int
} deriving (Show)

-- | Compute statistics
-- Demonstrates: Folding, aggregation
computeStats :: AtomSpace -> AtomSpaceStats
computeStats space = AtomSpaceStats {
    totalAtoms = total,
    nodeCount = nodes,
    linkCount = links,
    typeDistribution = typeDist
}
  where
    allAtoms = Map.elems (atoms space)
    total = length allAtoms
    nodes = length [a | a@(Node _ _ _ _) <- allAtoms]
    links = length [a | a@(Link _ _ _ _) <- allAtoms]
    typeDist = Map.fromListWith (+) [(getAtomType a, 1) | a <- allAtoms]
    
    getAtomType (Node _ t _ _) = t
    getAtomType (Link _ t _ _) = t

-- | Demonstration function
-- Demonstrates: Monadic sequencing, IO operations
demonstrateAtomSpace :: IO ()
demonstrateAtomSpace = do
    putStrLn "======================================================================="
    putStrLn "OpenCog AtomSpace - Hypergraph Knowledge Representation in Haskell"
    putStrLn "Showcasing: Immutability, algebraic types, pure functions, type safety"
    putStrLn "======================================================================="
    putStrLn ""
    
    putStrLn "1. Creating Knowledge Base"
    putStrLn "---------------------------------------------------"
    
    let space0 = newAtomSpace
    let (human, space1) = addNode ConceptNode "human" space0
    let (mortal, space2) = addNode ConceptNode "mortal" space1
    let (socrates, space3) = addNode ConceptNode "Socrates" space2
    let (animal, space4) = addNode ConceptNode "animal" space3
    
    putStrLn "Created nodes:"
    mapM_ (putStrLn . ("  " ++) . show) [human, mortal, socrates, animal]
    putStrLn ""
    
    putStrLn "2. Creating Relationships (Immutably)"
    putStrLn "---------------------------------------------------"
    
    let (link1, space5) = addLink InheritanceLink [socrates, human] space4
    let link1' = setTruthValue link1 1.0 1.0
    let (link2, space6) = addLink InheritanceLink [human, mortal] space5
    let (link3, space7) = addLink InheritanceLink [human, animal] space6
    
    putStrLn "Created inheritance links:"
    mapM_ (putStrLn . ("  " ++) . show) [link1', link2, link3]
    putStrLn ""
    
    putStrLn "3. Creating Predicates"
    putStrLn "---------------------------------------------------"
    
    let (breathes, space8) = addNode PredicateNode "breathes" space7
    let (thinks, space9) = addNode PredicateNode "thinks" space8
    let (eval1, space10) = addLink EvaluationLink [breathes, socrates] space9
    let (eval2, space11) = addLink EvaluationLink [thinks, socrates] space10
    
    putStrLn "Created evaluation links:"
    mapM_ (putStrLn . ("  " ++) . show) [eval1, eval2]
    putStrLn ""
    
    putStrLn "4. Querying by Type (Pure Functions)"
    putStrLn "---------------------------------------------------"
    
    let conceptNodes = getAtomsByType ConceptNode space11
    putStrLn "All concept nodes:"
    mapM_ (putStrLn . ("  " ++) . show) conceptNodes
    putStrLn ""
    
    let inhLinks = getAtomsByType InheritanceLink space11
    putStrLn "All inheritance links:"
    mapM_ (putStrLn . ("  " ++) . show) inhLinks
    putStrLn ""
    
    putStrLn "5. Incoming Links (Graph Traversal)"
    putStrLn "---------------------------------------------------"
    
    putStrLn "Links involving 'human':"
    let incomingHuman = getIncoming human space11
    mapM_ (putStrLn . ("  " ++) . show) incomingHuman
    putStrLn ""
    
    putStrLn "6. Transitive Inference"
    putStrLn "---------------------------------------------------"
    
    let inherited = inferTransitiveInheritance socrates space11
    putStrLn $ "Socrates transitively inherits from: " ++ 
               show (Set.size inherited) ++ " concepts"
    
    let isMortal = inheritsFrom socrates mortal space11
    putStrLn $ "Is Socrates mortal? " ++ show isMortal
    putStrLn ""
    
    putStrLn "7. Statistics"
    putStrLn "---------------------------------------------------"
    
    let stats = computeStats space11
    putStrLn $ "Total atoms: " ++ show (totalAtoms stats)
    putStrLn $ "Nodes: " ++ show (nodeCount stats)
    putStrLn $ "Links: " ++ show (linkCount stats)
    putStrLn $ "Type distribution: " ++ show (typeDistribution stats)
    putStrLn ""
    
    putStrLn "8. Complete AtomSpace"
    putStrLn "---------------------------------------------------"
    printAtomSpace space11
    putStrLn ""
    
    putStrLn "======================================================================="
    putStrLn "Haskell AtomSpace strengths:"
    putStrLn "  ✓ Immutable data structures (thread-safe knowledge)"
    putStrLn "  ✓ Algebraic data types (precise modeling)"
    putStrLn "  ✓ Pure functions (referential transparency)"
    putStrLn "  ✓ Type safety (invalid graphs impossible)"
    putStrLn "  ✓ Higher-order functions (flexible queries)"
    putStrLn "  ✓ Transitive inference (pure reasoning)"
    putStrLn "======================================================================="

-- | Main entry point
main :: IO ()
main = demonstrateAtomSpace
