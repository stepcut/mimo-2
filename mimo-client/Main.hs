{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverlappingInstances #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language DeriveDataTypeable #-}
module Main where

import           Control.Lens ((&), (^.), (.~), (%~), makeLenses, to)
import           Control.Lens.At (at)
import           Control.Monad
import           Data.Data (Data, Typeable)
import           Data.Aeson (ToJSON(..), FromJSON(..), Value, withText)
import Data.Aeson.TH as A
import           Data.Bool
import           Data.Char (toLower, chr)
import           Data.Foldable (toList)
import           Data.Maybe (catMaybes)
import           Data.Monoid
import           Data.Proxy (Proxy(..))
import           Data.JSString.Text (textToJSString)
import           Data.Text     (Text)
import qualified Data.Text     as T
import           Data.Map (Map)
import qualified Data.Map as Map
-- import           Data.Vector (Vector)
-- import qualified Data.Vector as Vector
import           GHC.Generics (Generic)
import           GHCJS.DOM
import           GHCJS.DOM.CharacterData
import           GHCJS.DOM.Document            hiding (drop, getLocation, focus, input)
import           GHCJS.DOM.Element             (removeAttribute, setAttribute, focus)
import           GHCJS.DOM.Event               (Event)
import qualified GHCJS.DOM.Event               as E
import           GHCJS.DOM.EventTarget         (addEventListener)
import           GHCJS.DOM.EventTargetClosures
import qualified GHCJS.DOM.Node                as Node
import           GHCJS.DOM.Node                hiding (getNextSibling)
import           GHCJS.DOM.NodeList            hiding (getLength)
import qualified GHCJS.DOM.Storage             as S
import           GHCJS.DOM.Types               hiding (Event, Attr, CSS)
import           GHCJS.DOM.Window              (getLocalStorage, getSessionStorage)
import           GHCJS.Foreign                 hiding (Object, Number)
import qualified GHCJS.Foreign.Internal        as Foreign
import           GHCJS.Marshal
import           GHCJS.Marshal.Pure
import qualified GHCJS.Types                   as G
import           Language.Haskell.Exts.Parser     (ParseResult(..), parseDecl)
import           Language.Haskell.Exts.Pretty     (prettyPrint)
import           Language.Haskell.Exts.ExactPrint (exactPrint)
import           Language.Haskell.Exts.SrcLoc     (SrcLoc(..),SrcSpanInfo(..))
import           Language.Haskell.Exts.Syntax     (Decl(..))
import           Language.Haskell.HSX.QQ (hsx)
import           Miso          as Miso hiding (update)
import           Miso.Html.Types.Event hiding (Action)
-- import           Miso.Html (Attribute(..), View, HasEvent(..), getField, getEventField, getTarget, mkNodeHtml, on, onClick, onInput, text_)
--import           Miso.HSX
--import           Miso.Html     (VTree, Attribute(..), Event(OnClick))
-- import           Prelude       hiding (div, span, id)
-- import           Types         (Update(..), MonthFlag(..))

-- * copied from relatable until we build relatable for ghcjs

data Index ty = Index { _index :: Int } | Auto
  deriving (Read, Show, Eq, Ord, Data, Typeable, Generic)

instance ToJSVal (Index ty)
instance FromJSVal (Index ty)

index :: Functor f => (Int -> f Int) -> Index t -> f (Index t)
index f (Index i) = fmap Index (f i)

incr :: Index t -> Index t
incr (Index i) = Index (succ i)

class Indexed a where
  idLens :: Functor f => ((Index a) -> f (Index a)) -> a -> f a

data Collection a = Collection
     { _nextIndex  :: Index a
     , _collection :: Map (Index a) a
     }
     deriving (Show, Eq, Ord, Data, Typeable)
makeLenses ''Collection


emptyCollection :: (Indexed a) => Collection a
emptyCollection =
  (Collection { _nextIndex = (Index 1)
              , _collection = Map.empty
              })

lookupC :: Index a
       -> Collection a
       -> Maybe a
lookupC i c = Map.lookup i (_collection c)

insertC :: (Indexed a) => a -> Collection a -> (a, Collection a)
insertC a (Collection ni c) =
  case a ^. idLens of
    Auto ->
      let a' = a & idLens .~ ni
          ni' = incr ni
      in (a', Collection ni' (Map.insert ni a' c))


class (Indexed c) => HasCollection st c where
  colLens :: Functor f => ((Collection c) -> f (Collection c)) -> st -> f st

-- * Move to Miso.Html.HSX

genElement :: (Maybe MisoString, MisoString) -> [ Attribute action ] -> [[ View action ]] -> View action
genElement (Just d, t) _attrs _children  = Prelude.error $ "elements with a domain not supported: " ++ show (d, t)
genElement (Nothing, t) attrs children = nodeHtml t attrs (concat children)

genEElement :: (Maybe MisoString, MisoString) -> [ Attribute action ] -> View action
genEElement (d, t) a = genElement (d, t) a []

fromStringLit :: String -> MisoString
fromStringLit = misoPack

class AsChild action c where
  asChild :: c -> [View action]

instance AsChild action String where
  asChild t = [ text $ misoPack t ]

instance AsChild action MisoString where
  asChild t = [ text t ]

instance AsChild action T.Text where
  asChild t = [ text $ textToJSString t ]

instance (action ~ action') => AsChild action (View action') where
  asChild v = [v]

instance (action ~ action') => AsChild action [View action'] where
  asChild vs = vs

data KV k v = k := v

class AsAttr action a where
  asAttr :: a -> Attribute action

instance AsAttr action (KV MisoString MisoString) where
  asAttr (k := v) = A k v

instance (action ~ action') => AsAttr action (Attribute action') where
  asAttr a = a

data DomRect = DomRect
  { _rectBottom :: Double
  , _rectHeight :: Double
  , _rectLeft :: Double
  , _rectRight :: Double
  , _rectTop :: Double
  , _rectWidth :: Double
  } deriving (Eq, Generic, Show)
makeLenses ''DomRect

deriveJSON A.defaultOptions { fieldLabelModifier = map toLower . drop 5 } ''DomRect
instance ToJSVal DomRect
instance FromJSVal DomRect

instance HasEvent "click" ((Double, Double), DomRect) where
  parseEvent _ = do
    target <- getTarget
    (Just x) <- getEventField "x"
    (Just y) <- getEventField "y"
    (Just result) <- applyFunction target "getBoundingClientRect" ([]::[Value])
    pure ((x,y), result)

data FontMetric = FontMetric
  { _fmWidth :: Double
  , _fwHeight :: Double
  }
  deriving (Eq, Show, Generic)

{-
instance HasEvent "input" MonospaceText where
  parseEvent _ = do
    pure Nothing
-}

data ADT = ADT
  { _adtIndex :: Index ADT
  , _adtValue :: Either T.Text (Decl SrcSpanInfo)
  }
  deriving (Eq, Show, Generic)
makeLenses ''ADT


instance Indexed ADT where
  idLens = adtIndex

data Model = Model
  { _adts       :: Collection ADT
  , _newADT     :: T.Text
  , _editADT    :: Maybe (Index ADT)
--  , _nextIndex  :: Index ADT
  , _parseError :: Maybe String
  , _editorPos  :: Maybe ((Double, Double), DomRect)
  }
  deriving Eq

makeLenses ''Model

initialModel = Model
  { _adts = emptyCollection
  , _newADT = ""
  , _editADT = Nothing
--  , _nextIndex = Index 0
  , _parseError = Nothing
  , _editorPos = Nothing
  }

data Action
  = UpdateADT Int
  | HandleKeyDown EditorKey
  | SubmitADT (Index ADT)
  | EditADT (Index ADT)
  | EditorPos ((Double, Double), DomRect)
    deriving (Show, Generic)

instance ToJSVal Action
instance FromJSVal Action

update' :: Action -> Model -> Effect Action Model
update' action model =
  case action of
    EditorPos dr -> noEff $ model & editorPos .~ Just dr
    EditADT i -> noEff $ model & editADT .~ Just i
    UpdateADT t  -> noEff $ model & newADT %~ (\t' -> t' <> T.pack [chr t])
    HandleKeyDown (EditorKey i)
      | i == 8 -> noEff $ model & newADT %~ T.init
      | otherwise -> noEff model
    SubmitADT i    ->
      case parseDecl (model ^. newADT ^. to T.unpack) of
        ParseOk a ->
              noEff $ model & newADT .~ ""
                            & adts %~ (\c -> snd $ insertC (ADT i (Right a)) c)
                            & parseError .~ Nothing
        ParseFailed sl err ->
          noEff $ model & parseError .~ (Just err)

showADT :: Model -> ADT -> View Action
showADT model adt
  | model ^. editADT == Just (adt ^. adtIndex) =
        adtInput model (Just adt)
  | otherwise =
    case adt ^. adtValue of
      (Right decl) ->
        [hsx|
            <pre class="ui segment" [onDoubleClick (EditADT $ adt ^. adtIndex)]><code class="item"><% exactPrint decl [] %></code></pre>
        |]
  where
    doubleclick :: Proxy "doubleclick"
    doubleclick = Proxy

-- colorize =

newtype EditorKey = EditorKey { unEditorKey :: Int }
 deriving (Eq, Show, Generic)
instance ToJSVal EditorKey
instance FromJSVal EditorKey

instance HasEvent "keydown" EditorKey where
  parseEvent _  = do
    do i <- keyGrammar
--       when (i == 8) preventDefault
       pure (EditorKey i)
         where
           -- | Retrieves either "keyCode", "which" or "charCode" field in `Grammar`
           keyGrammar :: Grammar Int
           keyGrammar = do
             keyCode <- getEventField "keyCode"
             which <- getEventField "which"
             charCode <- getEventField "charCode"
             pure $ head $ catMaybes [ keyCode, which, charCode ]


adtInput :: Model
         -> Maybe ADT
         -> View Action
adtInput model madt =
  let index = case madt of Nothing -> Auto ; (Just adt) -> adt ^. adtIndex
  in
  [hsx|
    <div>
     <div><% case model ^. parseError of
               Nothing -> ""
               (Just e) -> e %></div>
--         <div class="ui right labeled fluid input">
     <div class="ui segment" style="font-family: monospace;" tabindex="1"
       [ on click EditorPos
       , on keydown HandleKeyDown
       , onKeyPress UpdateADT
       , onBlur (SubmitADT index)
       , prop "value" (model ^. newADT)]><% model ^. newADT %></div>
--      <a class="ui tag label" [onClick (SubmitADT $ Auto)]>Add ADT</a>
--     </div>
    </div>
  |]
  where
    input :: Proxy "input"
    input = Proxy
    click :: Proxy "click"
    click = Proxy
    keydown :: Proxy "keydown"
    keydown = Proxy

viewAdts :: Model
         -> View Action
viewAdts model =
  [hsx|
    <div class="ui list">
     <% map (showADT model) (model ^. adts ^. collection ^. to Map.elems) %>
    </div>
  |]

view' :: Model -> View Action
view' model = [hsx|
  <div class="ui container">
    <h1 class="ui header">Algebraic Data Types</h1>
    <div class="ui segments">
     <% adtInput model Nothing %>
     <% map (showADT model) (model ^. adts  ^. collection ^. to Map.elems) %>
    </div>
    <% show $ model ^. editorPos %>
  </div>
  |]

main :: IO ()
main =
  do startApp initialModel view' update' defaultSettings

{-
keyMapSignal :: IO (Signal (M.Map Char Int))
keyMapSignal = do
   (source, sink) <- signal
   document.addEventListener "load" $ do
       map <- constructKeyMap
       sink map
```

[2:20]  
then add it to `extraSignals`
-}
