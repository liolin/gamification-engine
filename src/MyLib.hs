{-# LANGUAGE DeriveGeneric #-}

module MyLib ( run
             , user1
             , variable1
             , goal1
             , action1
             , startDb
             ) where

-- import Data.UUID (UUID)
import GHC.Generics

-- type ID = UUID
type ID = Int
type UserID = ID
type VariableID = ID
type StateID = ID
type GoalID = ID
type ActionID = ID

data DataType = DString String
              | DInt Int
              | DArray DataType
              deriving (Show)

data CompareOperator = In
                     | Equal
                     | Greater
                     | GreaterEqual
                     | Less
                     | LessEqual
                     deriving (Show)

data ActionOperation = Increment Int          -- Decrement is just Increment with a negative value
                     | Append DataType        -- Append `v` to the array
                     deriving (Show)

data User = User
  { userId :: UserID
  , userName :: String
  } deriving (Show)

data Variable = Variable
  { variableId :: VariableID
  , variableName :: String
  , variableState :: DataType
  } deriving (Generic, Show)

data State = State
  { stateId :: StateID
  , stateVariableId :: VariableID
  , stateUserId :: UserID
  , stateState :: DataType  -- Must be the same as variableType, how to enforce this?
  } deriving (Show)

data Goal = Goal
  { goalId :: GoalID
  , goalVariableId :: VariableID
  , goalRequired :: DataType
  , goalOperator :: CompareOperator
  } deriving (Generic, Show)

data Action = Action
  { actionId :: ActionID
  , actionUserId :: UserID
  , actionVariableId :: VariableID
  , actionOperation :: ActionOperation
  } deriving (Generic, Show)


variable1 :: Variable
variable1 = Variable
            { variableId = 1
            , variableName = "submitted_feedback"
            , variableState = DInt 0
            }

goal1 :: Goal
goal1 = Goal
        { goalId = 1
        , goalVariableId = 1
        , goalRequired = DInt 2
        , goalOperator = GreaterEqual
        }

action1 :: Action
action1 = Action
          { actionId = 1
          , actionUserId = 1
          , actionVariableId = 1
          , actionOperation = Increment 1
          }

user1 :: User
user1 = User
        { userId = 1
        , userName = "Oli"
        }

data DB = DB
        { dbVariables :: [Variable]
        , dbGoals :: [Goal]
        , dbUsers :: [User]
        , dbStates :: [State]
        , dbActions :: [Action]
        }


startDb :: DB
startDb = DB
          { dbVariables = [variable1]
          , dbGoals = [goal1]
          , dbUsers = [user1]
          , dbStates = []
          , dbActions = []
          }


createEmptyState :: Variable -> User -> State
createEmptyState v u = State
                       { stateId = 2
                       , stateVariableId = variableId v
                       , stateUserId = userId u
                       , stateState = variableState v
                       }


applyAction :: State -> Action -> Maybe State
applyAction s a = (\s' -> s { stateState = s' }) <$> applyOperation (actionOperation a) (stateState s)


applyOperation :: ActionOperation -> DataType -> Maybe DataType
applyOperation (Increment i) (DInt j) = Just $ DInt $ i + j
applyOperation (Increment i) _ = Nothing
applyOperation a d = undefined


findStates :: DB -> VariableID -> UserID -> [State]
findStates db v u = filter (\s -> stateUserId s == u) $ dbStates db

getState :: DB -> Variable -> User -> State
getState d v u | null ss = createEmptyState v u
               | otherwise = head ss  -- This is safe because I check ss before using head
  where
    ss = findStates d (variableId v) (userId u)




run :: DB -> User -> Variable -> Action -> Maybe State
run db u v = applyAction (getState db v u)
