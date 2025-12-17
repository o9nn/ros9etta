{-
opencog-cogserver.hs

OpenCog CogServer - Network Server for AtomSpace Access in Haskell

This single-file implementation demonstrates Haskell's strengths:
- Pure functional command system
- IO monad for effects isolation
- Immutable state management
- Type-safe command registry
- Pattern matching for command parsing
-}

module OpenCogCogServer where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Time.Clock (getCurrentTime, UTCTime, diffUTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.List (intercalate, isPrefixOf)
import Control.Monad (when, forever, unless)
import System.IO (hFlush, stdout, isEOF)
import Text.Read (readMaybe)

-- | Command result type
-- Demonstrates: Algebraic data types for result handling
data CommandResult 
    = Success String
    | Failure String
    | Exit
    deriving (Show, Eq)

-- | Check if command succeeded
isSuccess :: CommandResult -> Bool
isSuccess (Success _) = True
isSuccess _ = False

-- | Get result message
getMessage :: CommandResult -> String
getMessage (Success msg) = msg
getMessage (Failure msg) = "Error: " ++ msg
getMessage Exit = "Exiting..."

-- | Server session state
-- Demonstrates: Immutable state record
data Session = Session {
    sessionId :: String,
    createdAt :: UTCTime,
    authenticated :: Bool,
    sessionData :: Map String String
} deriving (Show)

-- | Create a new session
newSession :: String -> UTCTime -> Session
newSession sid time = Session {
    sessionId = sid,
    createdAt = time,
    authenticated = False,
    sessionData = Map.empty
}

-- | Set session data (returns new session)
-- Demonstrates: Immutable updates
setSessionData :: String -> String -> Session -> Session
setSessionData key value session = 
    session { sessionData = Map.insert key value (sessionData session) }

-- | Get session data with default
getSessionData :: String -> String -> Session -> String
getSessionData key defaultVal session = 
    Map.findWithDefault defaultVal key (sessionData session)

-- | Session age in seconds
sessionAge :: Session -> UTCTime -> Double
sessionAge session now = realToFrac $ diffUTCTime now (createdAt session)

-- | Command handler type
-- Demonstrates: Function types, higher-order functions
type CommandHandler = [String] -> ServerState -> IO (CommandResult, ServerState)

-- | Command registry
-- Demonstrates: Map of functions, first-class functions
type CommandRegistry = Map String (String, CommandHandler)

-- | Server state
-- Demonstrates: Composite state management
data ServerState = ServerState {
    port :: Int,
    commands :: CommandRegistry,
    sessions :: Map String Session,
    atomCount :: Int,
    running :: Bool,
    startTime :: UTCTime
} deriving (Show)

-- | Create initial server state
-- Demonstrates: Pure initialization
newServerState :: Int -> UTCTime -> ServerState
newServerState serverPort time = ServerState {
    port = serverPort,
    commands = Map.empty,
    sessions = Map.empty,
    atomCount = 42,  -- Simulated
    running = True,
    startTime = time
}

-- | Register a command
-- Demonstrates: Immutable map updates
registerCommand :: String -> String -> CommandHandler -> ServerState -> ServerState
registerCommand name desc handler state = 
    state { commands = Map.insert name (desc, handler) (commands state) }

-- | Execute a command
-- Demonstrates: Maybe monad, error handling
executeCommand :: String -> [String] -> ServerState -> IO (CommandResult, ServerState)
executeCommand cmdName args state = 
    case Map.lookup cmdName (commands state) of
        Nothing -> return (Failure $ "Unknown command: " ++ cmdName, state)
        Just (_, handler) -> handler args state

-- | Built-in commands
-- Demonstrates: Pure command implementations

-- | Help command
helpCommand :: CommandHandler
helpCommand _ state = do
    let cmdList = Map.toList (commands state)
    let helpText = unlines $ 
            "Available commands:" : 
            ["  " ++ name ++ " - " ++ desc | (name, (desc, _)) <- cmdList]
    return (Success helpText, state)

-- | Status command
statusCommand :: CommandHandler
statusCommand _ state = do
    now <- getCurrentTime
    let uptime = realToFrac $ diffUTCTime now (startTime state)
    let statusText = unlines [
            "CogServer Status:",
            "  Port: " ++ show (port state),
            "  Uptime: " ++ show (uptime :: Double) ++ " seconds",
            "  AtomSpace size: " ++ show (atomCount state) ++ " atoms",
            "  Active sessions: " ++ show (Map.size $ sessions state),
            "  Server: " ++ (if running state then "Running" else "Stopped")
            ]
    return (Success statusText, state)

-- | List command (show simulated atoms)
listCommand :: CommandHandler
listCommand _ state = do
    let atomList = unlines [
            "Simulated AtomSpace contents:",
            "  (ConceptNode \"human\")",
            "  (ConceptNode \"mortal\")",
            "  (ConceptNode \"Socrates\")",
            "  (InheritanceLink",
            "    (ConceptNode \"Socrates\")",
            "    (ConceptNode \"human\"))"
            ]
    return (Success atomList, state)

-- | Add node command
addNodeCommand :: CommandHandler
addNodeCommand args state 
    | length args < 2 = return (Failure "Usage: addnode <type> <name>", state)
    | otherwise = do
        let nodeType = head args
        let nodeName = args !! 1
        let newState = state { atomCount = atomCount state + 1 }
        let msg = "Added node: (" ++ nodeType ++ " \"" ++ nodeName ++ "\")"
        return (Success msg, newState)

-- | Query command
queryCommand :: CommandHandler
queryCommand args state
    | null args = return (Failure "Usage: query <pattern>", state)
    | otherwise = do
        let pattern = unwords args
        let result = unlines [
                "Query results for '" ++ pattern ++ "':",
                "  Found 3 matching atoms",
                "  (ConceptNode \"" ++ pattern ++ "\")"
                ]
        return (Success result, state)

-- | Echo command
echoCommand :: CommandHandler
echoCommand args state = return (Success $ unwords args, state)

-- | Info command
infoCommand :: CommandHandler
infoCommand _ state = do
    let info = unlines [
            "CogServer Information:",
            "  Port: " ++ show (port state),
            "  Commands registered: " ++ show (Map.size $ commands state),
            "  Active sessions: " ++ show (Map.size $ sessions state)
            ]
    return (Success info, state)

-- | Sessions command
sessionsCommand :: CommandHandler
sessionsCommand _ state = do
    now <- getCurrentTime
    if Map.null (sessions state)
        then return (Success "No active sessions", state)
        else do
            let sessionList = Map.elems (sessions state)
            let formatSession s = "  Session(" ++ sessionId s ++ 
                                ", age=" ++ show (sessionAge s now) ++ "s)"
            let sessionsText = unlines $ 
                    "Active sessions:" : map formatSession sessionList
            return (Success sessionsText, state)

-- | Exit command
exitCommand :: CommandHandler
exitCommand _ state = return (Exit, state { running = False })

-- | Setup default commands
-- Demonstrates: Function composition, state transformation
setupCommands :: ServerState -> ServerState
setupCommands state = 
    foldl (\s (name, desc, handler) -> registerCommand name desc handler s) state
        [ ("help", "List available commands", helpCommand)
        , ("status", "Display server status", statusCommand)
        , ("list", "List atoms in AtomSpace", listCommand)
        , ("addnode", "Add a node: addnode <type> <name>", addNodeCommand)
        , ("query", "Query atoms by pattern", queryCommand)
        , ("echo", "Echo back the arguments", echoCommand)
        , ("info", "Display server information", infoCommand)
        , ("sessions", "List active sessions", sessionsCommand)
        , ("exit", "Exit the shell", exitCommand)
        , ("quit", "Exit the shell", exitCommand)
        ]

-- | Parse command line into command name and arguments
-- Demonstrates: Pattern matching, list operations
parseCommand :: String -> (String, [String])
parseCommand input = 
    case words input of
        [] -> ("", [])
        (cmd:args) -> (cmd, args)

-- | REPL (Read-Eval-Print Loop)
-- Demonstrates: IO monad, recursion for loops
repl :: ServerState -> IO ()
repl state = do
    -- Print prompt
    putStr "cogserver> "
    hFlush stdout
    
    -- Read input
    eof <- isEOF
    if eof
        then do
            putStrLn ""
            putStrLn "Exiting..."
            return ()
        else do
            line <- getLine
            let trimmed = dropWhile (== ' ') line
            
            -- Skip empty lines
            unless (null trimmed) $ do
                let (cmdName, args) = parseCommand trimmed
                
                -- Execute command
                (result, newState) <- executeCommand cmdName args state
                
                -- Handle result
                case result of
                    Exit -> putStrLn "Shutting down CogServer..."
                    Success msg -> do
                        putStrLn msg
                        repl newState
                    Failure msg -> do
                        putStrLn msg
                        repl newState
            
            -- Continue for empty lines
            when (null trimmed) $ repl state

-- | Print server introduction
-- Demonstrates: IO formatting
printIntro :: ServerState -> IO ()
printIntro state = do
    putStrLn ""
    putStrLn "╔═══════════════════════════════════════════════════════════════════╗"
    putStrLn "║         OpenCog CogServer - Haskell Interactive Shell             ║"
    putStrLn "║         Type 'help' to list commands                              ║"
    putStrLn "╚═══════════════════════════════════════════════════════════════════╝"
    putStrLn ""
    putStrLn $ "Starting CogServer on port " ++ show (port state)
    putStrLn "Haskell implementation showcasing:"
    putStrLn "  ✓ Pure functional command system"
    putStrLn "  ✓ IO monad for effects isolation"
    putStrLn "  ✓ Immutable state management"
    putStrLn "  ✓ Type-safe command registry"
    putStrLn "  ✓ Pattern matching for parsing"
    putStrLn ""

-- | Create demo sessions
-- Demonstrates: Monadic operations, time handling
createDemoSessions :: ServerState -> IO ServerState
createDemoSessions state = do
    now <- getCurrentTime
    let session1 = newSession "client-001" now
    let session1' = setSessionData "username" "alice" session1
    let session2 = newSession "client-002" now
    let session2' = setSessionData "username" "bob" session2
    
    let newSessions = Map.fromList [
            (sessionId session1', session1'),
            (sessionId session2', session2')
            ]
    
    return $ state { sessions = newSessions }

-- | Start the interactive server
-- Demonstrates: IO coordination, composition
startServer :: Int -> IO ()
startServer serverPort = do
    now <- getCurrentTime
    let state = newServerState serverPort now
    let stateWithCmds = setupCommands state
    
    printIntro stateWithCmds
    
    -- Create demo sessions
    stateWithSessions <- createDemoSessions stateWithCmds
    
    -- Start REPL
    repl stateWithSessions

-- | Demonstrate features
demonstrateFeatures :: IO ()
demonstrateFeatures = do
    putStrLn "======================================================================="
    putStrLn "OpenCog CogServer - Haskell Implementation"
    putStrLn "======================================================================="
    putStrLn ""
    
    putStrLn "1. Pure Functional Command System"
    putStrLn "---------------------------------------------------"
    putStrLn "Commands are pure functions: [String] -> ServerState -> IO (Result, State)"
    putStrLn "Type-safe and composable"
    putStrLn ""
    
    putStrLn "2. Immutable State Management"
    putStrLn "---------------------------------------------------"
    putStrLn "All state updates return new state"
    putStrLn "Original state remains unchanged"
    putStrLn "Thread-safe by design"
    putStrLn ""
    
    putStrLn "3. Algebraic Data Types"
    putStrLn "---------------------------------------------------"
    putStrLn "CommandResult = Success | Failure | Exit"
    putStrLn "Pattern matching for exhaustive handling"
    putStrLn ""
    
    putStrLn "4. IO Monad for Effects"
    putStrLn "---------------------------------------------------"
    putStrLn "Pure logic separated from IO effects"
    putStrLn "Referential transparency maintained"
    putStrLn ""
    
    putStrLn "======================================================================="
    putStrLn "Starting interactive shell..."
    putStrLn "======================================================================="
    putStrLn ""

-- | Main entry point
main :: IO ()
main = do
    demonstrateFeatures
    startServer 17001
