module DES.LogicGates where


data Signal = High | Low | Undefined

type Wire = String

data Gate = Inverter Wire Wire
          | AndGate Wire Wire Wire
          | Complex Model
          
data Model = Model { _gates ::[Gate]
                   , _inputs :: [Wire] 
                   , _outputs ::[Wire] }
                   
type State = [(Wire, Signal)]
                   

initState :: Model -> State
initState _m = [] 

-- Simulation step
step :: Model -> State -> State
step _m s = s


-- Demo model
wireA :: Wire
wireA = "A"

wireB :: Wire
wireB = "B"

model1 :: Model
model1 = Model [Inverter wireA wireB] [wireA] [wireB]


run :: [Signal] -> Model -> [Signal]
run _ _ = []