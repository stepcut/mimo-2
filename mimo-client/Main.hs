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
module Main where

import           Control.Lens
import           Control.Lens.At (at)
import           Control.Monad
--import           Data.Array (Array, Ix(..))
--import qualified Data.Array as Array
--import           Data.Array.MArray(newListArray)
--import           Data.Array.ST (STArray, runSTArray)
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
import           Prelude       hiding (div, span, id)
-- import           Types         (Update(..), MonthFlag(..))


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

data Model = Model
  { _adts       :: [Decl SrcSpanInfo]
  , _newADT     :: T.Text
  , _parseError :: Maybe String
  , _editorPos  :: Maybe ((Double, Double), DomRect)
  }
  deriving Eq

makeLenses ''Model

initialModel = Model
  { _adts = [ ]
  , _newADT = ""
  , _parseError = Nothing
  , _editorPos = Nothing
  }

data Action
  = UpdateADT Int
  | HandleKeyDown EditorKey
  | SubmitADT
  | EditorPos ((Double, Double), DomRect)
    deriving (Show, Generic)

instance ToJSVal Action
instance FromJSVal Action

update' :: Action -> Model -> Effect Action Model
update' action model =
  case action of
    EditorPos dr -> noEff $ model & editorPos .~ Just dr
    UpdateADT t  -> noEff $ model & newADT %~ (\t' -> t' <> T.pack [chr t])
    HandleKeyDown (EditorKey i)
      | i == 8 -> noEff $ model & newADT %~ T.init
      | otherwise -> noEff model
    SubmitADT    ->
      case parseDecl (model ^. newADT ^. to T.unpack) of
        ParseOk a ->
          noEff $ model & newADT .~ ""
                        & adts %~ (\l -> l ++ [a])
                        & parseError .~ Nothing
        ParseFailed sl err ->
          noEff $ model & parseError .~ (Just err)

showADT :: Decl SrcSpanInfo -> View Action
showADT t = [hsx| <pre class="ui segment"><code class="item"><% exactPrint t [] %></code></pre> |]

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
         -> View Action
adtInput model =
  [hsx|
    <div>
     <div><% show $ model ^. parseError %></div>
--         <div class="ui right labeled fluid input">
     <div class="ui segment" style="font-family: monospace;" tabindex="1"
       [ on click EditorPos
-- , onWithOptions (Miso.Options False True) keydown HandleKeyDown
       , on keydown HandleKeyDown
       , onKeyPress UpdateADT
       , prop "value" (model ^. newADT)]><% model ^. newADT %></div>
{-
      <textarea cols="80" rows="5" style="font-family: monospace;" placeholder="Enter a new data type"
        [ on click EditorPos
        , onInput UpdateADT
        , prop "value" (model ^. newADT)]></textarea>
        -}
      <a class="ui tag label" [onClick SubmitADT]>Add ADT</a>
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
     <% map showADT (model ^. adts) %>
    </div>
  |]


view' :: Model -> View Action
view' model = [hsx|
  <div class="ui container">
    <h1 class="ui header">Algebraic Data Types</h1>
    <div class="ui segments">
     <% adtInput model %>
     <% map showADT (model ^. adts) %>
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
