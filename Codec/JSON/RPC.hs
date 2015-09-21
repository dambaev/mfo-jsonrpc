{-#LANGUAGE FlexibleContexts#-}
{-#LANGUAGE RecordWildCards#-}
{-#LANGUAGE OverloadedStrings#-}
{-#LANGUAGE ScopedTypeVariables#-}
{-#LANGUAGE BangPatterns #-}
module Codec.JSON.RPC 
    where

import Data.Aeson
import Data.Text
import Data.Text as E
import Data.Maybe
import Yesod
import Data.Aeson.Parser
import Data.Conduit
import Data.Conduit.Attoparsec
import Data.Conduit.List as CL
import Data.Conduit.Binary as CB
import Yesod.Auth
import Network.Wai.Conduit as NWC
import Control.Monad.Catch
import Control.Applicative
import Network.HTTP.Conduit as NHC
import Data.Text as T
import Data.Text.Encoding as E
import Control.Monad.Trans.Resource
import Data.ByteString as B
import Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import Data.List as L
import Control.Exception as E


-- data (ToJSON a) => JSONCommand a = JSONCommand
--     { commandPayload:: a
--     , commandHash:: Text
--     }
-- 
-- instance (ToJSON a) => ToJSON (JSONCommand a) where
--     toJSON JSONCommand {..} = object
--         [ "name" .= ("JSONCommand" :: Text)
--         , "data" .= 
--             [ "payload" .= commandPayload
--             , "hash" .= ("":: Text)
--             ]
--         ]
-- 

instance ToJSON Cookie where
    toJSON p@(Cookie {..}) = let
            name = E.decodeUtf8 $! cookie_name
            value = E.decodeUtf8 $! cookie_value
            domain = E.decodeUtf8 $! cookie_domain
            path = E.decodeUtf8 $! cookie_path
        in object 
        [ "name" .= name 
        , "value" .=  value
        , "expire" .= cookie_expiry_time
        , "domain" .= domain
        , "path" .= path
        , "creation" .= cookie_creation_time
        , "last_access" .= cookie_last_access_time
        , "persistent" .= cookie_persistent
        , "host_only" .= cookie_host_only
        , "secure_only" .= cookie_secure_only
        , "http_only" .= cookie_http_only
        ]

instance FromJSON Cookie where
    parseJSON parent@(Object v) = Cookie
        <$> ( v .: "name" >>= return . E.encodeUtf8)
        <*> ( v .: "value" >>= return . E.encodeUtf8)
        <*> v .: "expire"
        <*> ( v .: "domain" >>= return . E.encodeUtf8 )
        <*> ( v .: "path" >>= return . E.encodeUtf8 )
        <*> v .: "creation"
        <*> v .: "last_access"
        <*> v .: "persistent"
        <*> v .: "host_only"
        <*> v .: "secure_only"
        <*> v .: "http_only"



-- getJSONRequest:: FromJSON a => HandlerT Auth (HandlerT site IO) (Result a)
getJSONRequest:: (FromJSON a, Monad master
    , MonadBaseControl IO master
    , MonadIO master
    , MonadThrow master
    ) => 
    HandlerT app master (Result a)
getJSONRequest = do
    yesodReq <- getRequest
    let request = reqWaiRequest yesodReq
    value' <- sourceRequestBody request $$ sinkParser json
    liftIO $ print $ show value'
    return $! fromJSON value'

    
callPost:: (FromJSON a, Monad master
    , MonadBaseControl IO master
    , MonadIO master
    , MonadThrow master
    , Show a
    ) 
    => Text
    -> [(ByteString, ByteString)]
    -> HandlerT app master (Result a)
callPost url opts = do
    mgwSession <- lookupSession "gwSession"
    liftIO $! print $! "mgwsession = " ++ (show mgwSession)
    let new_cookie = case mgwSession of
            Nothing -> []
            Just gwSession -> case decodeStrict $! E.encodeUtf8 $! gwSession of
                Just some -> [some]
                _ -> []
                
    liftIO $ print $ "parsing url" ++ show (new_cookie)
    initReq <- parseUrl $! T.unpack url
    let req = urlEncodedBody opts initReq
            { --  method = "POST"
            --, requestBody = RequestBodyBS $! B.concat $ L.map (\(k,v)-> k `B.append` "=" `B.append` v `B.append` "&") opts
            redirectCount = 0
            , cookieJar = Just $! createCookieJar new_cookie
            }
    liftIO $ print $ "serving request: " ++ (show req)
    resp <- withManager $! \manager -> http req manager
    resValue <- runResourceT $ responseBody resp $$+- sinkParser json
    let respCookie = destroyCookieJar $ responseCookieJar resp
    liftIO $ print $ show resValue
    liftIO $ print $ show respCookie
    _ <- case L.filter (\x-> cookie_name x == "_SESSION" )respCookie of
        [] -> return ()
        some:_ -> setSession "gwSession" $! E.decodeUtf8 $! BL.toStrict $!  encode some
    return $! fromJSON $! resValue


jsonCall:: (ToJSON req
    , FromJSON a
    , Monad master
    , MonadBaseControl IO master
    , MonadIO master
    , MonadThrow master
    , MonadCatch master
    ) 
    => Text
    -> req 
    -> HandlerT app master (Result a)
jsonCall url request = do
    mgwSession <- lookupSession "gwSession"
    let new_cookie = case mgwSession of
            Nothing -> Just $! createCookieJar []
            Just gwSession -> Just $! createCookieJar $! [
                fromJust $! decodeStrict $! E.encodeUtf8 $! gwSession
                ]
    initReq <- parseUrl $! T.unpack url 
    let req = initReq
            { method = "POST"
            , NHC.requestBody = RequestBodyLBS $! encode $! request
            , redirectCount = 0
            , cookieJar = new_cookie
            }
    eresValue::Either SomeException (Value, NHC.Response (ResumableSource (ResourceT IO) ByteString )) <-  liftIO $ E.try $! do
        resp <- withManager $! \manager -> http req manager
        ret <- runResourceT $ responseBody resp $$+- sinkParser json
        return (ret, resp)
    case eresValue of
        Left e -> return $! Error $! show e
        Right (resValue, resp) -> do
            let respCookie = destroyCookieJar $ responseCookieJar resp 
            liftIO $ print $ show resValue
            liftIO $ print $ show respCookie
            _ <- case L.filter (\x-> cookie_name x == "_SESSION" )respCookie of
                [] -> return ()
                some:_ -> setSession "gwSession" $! E.decodeUtf8 $! BL.toStrict $!  encode some
            return $! fromJSON $! resValue

callGet:: ( Monad master
    , MonadBaseControl IO master
    , MonadIO master
    , MonadThrow master
    ) 
    => Text
--    -> [(ByteString, ByteString)]
    -> Sink ByteString (ResourceT (HandlerT app master)) a
    -> HandlerT app master a
callGet url sink = do
    mgwSession <- lookupSession "gwSession"
    let new_cookie = case mgwSession of
            Nothing -> []
            Just gwSession -> case decodeStrict $! E.encodeUtf8 $! gwSession of
                Just some -> [some]
                _ -> []
                
    initReq <- parseUrl $! T.unpack url
    let req = initReq
            { redirectCount = 0
            , cookieJar = Just $! createCookieJar new_cookie
            }
    (resp,resValue) <- withManager $! \manager -> do
        resp <- http req manager
        !resValue <- responseBody resp $$+- sink
        return (resp, resValue)
    let respCookie = destroyCookieJar $ responseCookieJar resp
    _ <- case L.filter (\x-> cookie_name x == "_SESSION" )respCookie of
        [] -> return ()
        some:_ -> setSession "gwSession" $! E.decodeUtf8 $! BL.toStrict $!  encode some
    return $! resValue


