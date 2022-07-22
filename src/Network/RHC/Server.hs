module Network.RHC.Server where

server = "Hello"

-- data Transport = HTTP --WS is not implemented yet | WS

data Request prm = Request {
        resVersion :: String,
        method :: String,
        params :: Maybe prm, -- structure
        reqId :: Maybe String -- note: should change request type to sum type
                        -- between notification and ordinary request
}

data Response res = Response {
        reqVersion :: String,
        result :: Maybe res, -- structure, determined structure by method
        resError :: Maybe String, -- must be an object
        resId :: Maybe String
}
