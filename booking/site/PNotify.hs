-- | Adapted from https://github.com/cutsea110/yesod-pnotify
-- Since the package has not been updated to be compatible with latest version of Yesod Forms
-- I download the code and use it within the project itself instead

module PNotify
       ( PNotify(..)
       , NotifyType(..)
       , NotifyStyling(..)
       , YesodJqueryPnotify(..)
       , getPNotify
       , setPNotify
         -- Utility
       , pnotify
       ) where

import Prelude

import Yesod
import Yesod.Form.Jquery

import Data.Text (Text)
import Data.Monoid ((<>), mempty)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Control.Monad.Trans.Maybe
import Data.Char (toLower)
import Text.Julius (RawJS(..))

data PNotify = PNotify 
               { sty :: NotifyStyling
               , typ :: NotifyType
               , ttl :: Text
               , msg :: Text
               }
             deriving (Show, Read)

data NotifyType = Notice | Info | Success | Error
                deriving (Show, Read)

data NotifyStyling = JqueryUI | Bootstrap
                   deriving (Show, Read)

class YesodJquery a => YesodJqueryPnotify a where
  urlPnotifyJs :: a -> Either (Route a) Text
  urlPnotifyJs _ = Right "http://cdn.jsdelivr.net/pnotify/1.3.1/jquery.pnotify.min.js"
  urlPnotifyCss :: a -> Either (Route a) Text
  urlPnotifyCss _ = Right "http://cdn.jsdelivr.net/pnotify/1.3.1/jquery.pnotify.default.css"

notifyKey :: Text
notifyKey = "_PNotify"

_setPNotify :: [PNotify] -> HandlerT site IO ()
_setPNotify = setSession notifyKey . T.concat . TL.toChunks . TL.pack . show

getPNotify :: HandlerT site IO (Maybe [PNotify])
getPNotify = runMaybeT $ do
  ns <- MaybeT $ lookupSession notifyKey
  lift $ deleteSession notifyKey
  return $ read $ T.unpack ns

setPNotify :: PNotify -> HandlerT site IO ()
setPNotify n = do
  mns <- getPNotify
  _setPNotify (n:maybe [] id mns)

pnotify :: YesodJqueryPnotify site => site -> WidgetT site IO ()
pnotify y = do
  mnotify <- handlerToWidget getPNotify
  case mnotify of
    Nothing -> return ()
    Just ps -> do
      addScriptEither $ urlJqueryJs y
      addScriptEither $ urlJqueryUiJs y
      addStylesheetEither $ urlJqueryUiCss y
      addScriptEither $ urlPnotifyJs y
      addStylesheetEither $ urlPnotifyCss y
      let toJs p = [julius|{styling:'#{rawJS $ map toLower $ show $ sty p}',title:'#{rawJS $ ttl p}',text:'#{rawJS $ msg p}',type:'#{rawJS $ map toLower $ show $ typ p}'},|]
          ws = foldr ((<>).toJs) mempty ps
      toWidget [julius|$(document).ready(function(e){var ws=[^{ws}];for(var i in ws){$.pnotify(ws[i]);}});|]

