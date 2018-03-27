{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Network.ABCI.Types (

-- * Type-safe 'Request' / 'Response' GADTs

  Request (..)
, Response (..)

-- * Code types

, CodeType(..)
, codeTypeOK
, codeTypeEncodingError
, codeTypeBadNonce
, codeTypeUnauthorized
, codeTypeBadOption

-- * An ABCI application type

, App (..)


-- * 'MsgType' kind and associated types. Used to tag
--   with a phantom type the 'Request' and associated 'Response'
--   types

, MsgType
, Echo
, Flush
, Info
, SetOption
, DeliverTx
, CheckTx
, Commit
, Query
, InitChain
, BeginBlock
, EndBlock

-- * protobuf/GADT 'Request'/'Response' conversion

, toProtoResponse
, withProtoRequest

-- * Re-exports
, def
-- * Protobuf safe types re-exports

, Proto.BlockID (..)
, Proto.ConsensusParams
, Proto.Header (..)
, Proto.PartSetHeader (..)
, Proto.Validator (..)
) where

import qualified Proto.Network.ABCI.Types as Proto
import qualified Proto.Github.Com.Tendermint.Tmlibs.Common.Types as Proto

import           Data.ByteString (ByteString)
import           Data.Default (Default(def))
import           Data.Int (Int64, Int32)
import           Data.Text (Text)
import           Data.Word (Word32)
import           Lens.Micro

-- | An 'App' is a monadic function from 'Request' to 'Response'.
--   We tag both with the 'MsgType' to enforce at the type-level that
--   the 'Request'/'Response' types match
newtype App m = App (forall (t :: MsgType). Request t -> m (Response t))

-- | Type-level "tags" representing the type of 'Request'/'Response' pairs
data MsgType
  = Echo
  | Flush
  | Info
  | SetOption
  | DeliverTx
  | CheckTx
  | Commit
  | Query
  | InitChain
  | BeginBlock
  | EndBlock

-- We create type synonyms for the promoted constructors so our users don't
-- need to enable the 'DataKinds' GHC extension

type Echo       = 'Echo
type Flush      = 'Flush
type Info       = 'Info
type SetOption  = 'SetOption
type DeliverTx  = 'DeliverTx
type CheckTx    = 'CheckTx
type Commit     = 'Commit
type Query      = 'Query
type InitChain  = 'InitChain
type BeginBlock = 'BeginBlock
type EndBlock   = 'EndBlock

-- | A type-safe 'Request' GADT tagged with the 'MsgType'
data Request (t :: MsgType) where

  RequestEcho ::
    { requestEcho'message :: !Text
    } -> Request Echo

  RequestFlush :: Request Flush

  RequestInfo :: {
    requestInfo'version :: !Text
  } -> Request Info

  RequestSetOption  ::
    { requestSetOption'key   :: !Text
    , requestSetOption'value :: !Text
    } -> Request SetOption

  RequestDeliverTx ::
    { requestDeliverTx'tx :: !ByteString
    } -> Request DeliverTx

  RequestCheckTx ::
    { requestCheckTx'tx :: !ByteString
    } -> Request CheckTx

  RequestCommit :: Request Commit

  RequestQuery  ::
    { requestQuery'data   :: !ByteString
    , requestQuery'path   :: !Text
    , requestQuery'height :: !Int64
    , requestQuery'prove  :: !Bool
    } -> Request Query

  RequestInitChain ::
    { requestInitChain'validators    :: ![Proto.Validator]
    , requestInitChain'appStateBytes :: !ByteString
    } -> Request InitChain

  RequestBeginBlock ::
    { requestInitBlock'hash   :: !ByteString
    , requestInitBlock'header :: !(Maybe Proto.Header)
    , requestInitBlock'absent_validators :: ![Int32]
    , requestInitBlock'byzantine_validators :: ![Proto.Evidence]
    } -> Request BeginBlock

  RequestEndBlock ::
    { requestEndBlock'height :: !Int64
    } -> Request EndBlock

deriving instance Show (Request t)

-- | A type-safe 'Response' GADT tagged with the 'MsgType'
data Response (t :: MsgType) where

  ResponseException ::
    { responseException'error :: !Text
    } -> Response t

  ResponseEcho ::
    { responseEcho'message :: !Text
    } -> Response Echo

  ResponseFlush :: Response Flush

  ResponseInfo ::
    { responseInfo'data             :: !Text
    , responseInfo'version          :: !Text
    , responseInfo'lastBlockHeight  :: !Int64
    , responseInfo'lastBlockAppHash :: !ByteString
    } -> Response Info

  ResponseSetOption ::
    { responseSetOption'code :: !CodeType
    , responseSetOption'log  :: !Text
    , responseSetOption'info :: !Text
    } -> Response SetOption

{-
data ResponseDeliverTx = ResponseDeliverTx{_ResponseDeliverTx'code
                                           :: !Data.Word.Word32,
                                           _ResponseDeliverTx'data' :: !Data.ByteString.ByteString,
                                           _ResponseDeliverTx'log :: !Data.Text.Text,
                                           _ResponseDeliverTx'info :: !Data.Text.Text,
                                           _ResponseDeliverTx'gasWanted :: !Data.Int.Int64,
                                           _ResponseDeliverTx'gasUsed :: !Data.Int.Int64,
                                           _ResponseDeliverTx'tags ::
                                           ![Proto.Github.Com.Tendermint.Tmlibs.Common.Types.KVPair],
                                           _ResponseDeliverTx'fee ::
                                           !(Prelude.Maybe
                                               Proto.Github.Com.Tendermint.Tmlibs.Common.Types.KI64Pair)}
                       deriving (Prelude.Show, Prelude.Eq, Prelude.Ord)
-}

  ResponseDeliverTx ::
    { responseDeliverTx'code      :: !CodeType
    , responseDeliverTx'data      :: !ByteString
    , responseDeliverTx'log       :: !Text
    , responseDeliverTx'info      :: !Text
    , responseDeliverTx'gasWanted :: !Int64
    , responseDeliverTx'gasUsed   :: !Int64
    , responseDeliverTx'tags      :: ![Proto.KVPair]
    , responseDeliverTx'fee       :: !(Maybe Proto.KI64Pair)
    } -> Response DeliverTx

  ResponseCheckTx ::
    { responseCheckTx'code      :: !CodeType
    , responseCheckTx'data      :: !ByteString
    , responseCheckTx'log       :: !Text
    , responseCheckTx'info      :: !Text
    , responseCheckTx'gasWanted :: !Int64
    , responseCheckTx'gasUsed   :: !Int64
    , responseCheckTx'tags      :: ![Proto.KVPair]
    , responseCheckTx'fee       :: !(Maybe Proto.KI64Pair)
    } -> Response CheckTx

  ResponseCommit ::
    { responseCommit'data :: !ByteString
    } -> Response Commit

  ResponseQuery ::
    { responseQuery'code   :: !CodeType
    , responseQuery'log    :: !Text
    , responseQuery'info   :: !Text
    , responseQuery'index  :: !Int64
    , responseQuery'key    :: !ByteString
    , responseQuery'value  :: !ByteString
    , responseQuery'proof  :: !ByteString
    , responseQuery'height :: !Int64
    } -> Response Query

  ResponseInitChain :: Response InitChain

  ResponseBeginBlock :: Response BeginBlock

  ResponseEndBlock ::
    { responseEndBlock'diffs :: ![Proto.Validator]
    , responseEndBlock'consensus :: !(Maybe Proto.ConsensusParams)
    } -> Response EndBlock

deriving instance Show (Response t)

instance Default (Response Flush) where
  def = ResponseFlush

instance Default (Response Info) where
  def = ResponseInfo "" "" 0 ""

instance Default (Response SetOption) where
  def = ResponseSetOption def "" ""

instance Default (Response Query) where
  def = ResponseQuery def "" "" 0 "" "" "" 0

instance Default (Response InitChain) where
  def = ResponseInitChain

instance Default (Response BeginBlock) where
  def = ResponseBeginBlock

instance Default (Response EndBlock) where
  def = ResponseEndBlock [] Nothing

------------------------------------
-- code types

newtype CodeType = CodeType { unCodeType :: Word32
  } deriving (Show, Eq, Ord)

instance Default CodeType where
  def = CodeType 0


codeTypeOK, codeTypeEncodingError, codeTypeBadNonce, codeTypeUnauthorized, codeTypeBadOption :: CodeType

codeTypeOK            = CodeType 0
codeTypeEncodingError = CodeType 1
codeTypeBadNonce      = CodeType 2
codeTypeUnauthorized  = CodeType 3
codeTypeBadOption     = CodeType 101


{- from file: https://github.com/tendermint/abci/blob/master/example/code/code.go
  CodeTypeOK            uint32 = 0
  CodeTypeEncodingError uint32 = 1
  CodeTypeBadNonce      uint32 = 2
  CodeTypeUnauthorized  uint32 = 3
  CodeTypeBadOption uint32 = 101
-}

------------------------------------

-- | Translates type-safe 'Response' GADT to the unsafe
--   auto-generated 'Proto.Response'
toProtoResponse :: Response t -> Proto.Response
toProtoResponse (ResponseException error') =
  def & Proto.exception .~ Proto.ResponseException error'
toProtoResponse (ResponseEcho msg) =
  def & Proto.echo .~ Proto.ResponseEcho msg
toProtoResponse ResponseFlush =
  def & Proto.flush .~ Proto.ResponseFlush
toProtoResponse (ResponseInfo d v h ah) =
  def & Proto.info .~ Proto.ResponseInfo d v h ah
toProtoResponse (ResponseSetOption (CodeType code') log' info') =
  def & Proto.setOption .~ Proto.ResponseSetOption code' log' info'
toProtoResponse (ResponseDeliverTx (CodeType code') data'' log' info' gasWanted' gasUsed' tags' fee') =
  def & Proto.deliverTx .~ Proto.ResponseDeliverTx code' data'' log' info' gasWanted' gasUsed' tags' fee'
toProtoResponse (ResponseCheckTx (CodeType code') data'' log' info' gasWanted' gasUsed' tags' fee') =
  def & Proto.checkTx .~ Proto.ResponseCheckTx code' data'' log' info' gasWanted' gasUsed' tags' fee'
toProtoResponse (ResponseCommit data'') =
  def & Proto.commit .~ Proto.ResponseCommit data''
toProtoResponse (ResponseQuery (CodeType c) l i x k v p h) =
  def & Proto.query .~ Proto.ResponseQuery c l i x k v p h
toProtoResponse ResponseInitChain =
  def & Proto.initChain .~ Proto.ResponseInitChain
toProtoResponse ResponseBeginBlock =
  def & Proto.beginBlock .~ Proto.ResponseBeginBlock
toProtoResponse (ResponseEndBlock vs consensus) =
  def & Proto.endBlock .~ Proto.ResponseEndBlock vs consensus


-- | Translates the unsafe auto-generated 'Proto.Request' to a type-safe
--   'Request GADT so users can safely pattern-match on it
--   (ie: the compiler will warn if any case is not covered)
--
--   Note that we need to use a rank-n-types continuation since the
--   'Request' GADT carries a phantom-type 'MsgType' "tag" and Haskell
--   does not allow a polymorphic return type on a "normal" function
--   (only those belonging to a type classes)
withProtoRequest
  :: Proto.Request
  -> (forall (t :: MsgType). Maybe (Request t) -> a)
  -> a
withProtoRequest r f
  | Just (Proto.RequestEcho msg)          <- r^.Proto.maybe'echo       = f (Just (RequestEcho msg))
  | Just (Proto.RequestFlush)             <- r^.Proto.maybe'flush      = f (Just RequestFlush)
  | Just (Proto.RequestInfo version)      <- r^.Proto.maybe'info       = f (Just (RequestInfo version))
  | Just (Proto.RequestSetOption k v)     <- r^.Proto.maybe'setOption  = f (Just (RequestSetOption k v))
  | Just (Proto.RequestDeliverTx tx)      <- r^.Proto.maybe'deliverTx  = f (Just (RequestDeliverTx tx))
  | Just (Proto.RequestCheckTx tx)        <- r^.Proto.maybe'checkTx    = f (Just (RequestCheckTx tx))
  | Just (Proto.RequestCommit)            <- r^.Proto.maybe'commit     = f (Just RequestCommit)
  | Just (Proto.RequestQuery d p h pr)    <- r^.Proto.maybe'query      = f (Just (RequestQuery d p h pr))
  | Just (Proto.RequestInitChain vs asb)  <- r^.Proto.maybe'initChain  = f (Just (RequestInitChain vs asb))
  | Just (Proto.RequestBeginBlock ah hdr av bv) <- r^.Proto.maybe'beginBlock = f (Just (RequestBeginBlock ah hdr av bv))
  | Just (Proto.RequestEndBlock h)        <- r^.Proto.maybe'endBlock   = f (Just (RequestEndBlock h))
  | otherwise                                                          = f Nothing



