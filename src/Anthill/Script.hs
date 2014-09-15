module Anthill.Script where


type DeviceName = String
type MessageId = String

data ParamType = ParamString String | ParamNumber Float | ParamBool Bool
type Params = [(String, ParamType)]                             
 
data InputMessage = InputMessage DeviceName MessageId Params

data OutputMessage = Message DeviceName MessageId Params
                   | Event MessageId Params
                             
-- | Filter messages
type Filter = InputMessage -> Bool
-- | Convert messages
type Converter = InputMessage -> [OutputMessage]
-- | Subscription to message stream wit
data Subscription = Subscription Filter Converter

type Main = [Subscription]


-- ----------------------------------------------------------------------------
-- Implementation
-- ----------------------------------------------------------------------------

filterA :: Filter
filterA (InputMessage _device _id _params) = True

convertA :: Converter
convertA (InputMessage _device _id _params) = [ Message "device3" "SetState" []
                                              , Event "Alert" [] ]
                                              
execute :: [Subscription] -> InputMessage -> [OutputMessage]
execute _ _ = []