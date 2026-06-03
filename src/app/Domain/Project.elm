module Domain.Project exposing (..)

import UUID exposing (UUID)


type alias TextNode =
    { text : String
    , status : Bool
    }


type NodeData
    = Text TextNode


type alias Node =
    { id : UUID
    , data : NodeData
    }


type alias Column =
    { order : Int
    , name : Maybe String
    , nodes : List Node
    , forkedFromNode : Maybe UUID
    , joinedToNode : Maybe UUID
    }


type alias Project =
    { id : UUID
    , name : String
    }
