{-# LANGUAGE TupleSections #-}
module Auditor.SVG
  ( makeProfileSVG
  ) where

import           Flipstone.Prelude
import           Auditor.Types

import qualified Data.List as L
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as IO
import           Data.Tuple (fst, snd)
import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze as Blaze
import qualified Text.Blaze.Svg11 as Svg
import qualified Text.Blaze.Svg11.Attributes as Svg
import           Text.Blaze.Svg.Renderer.Pretty (renderSvg)

makeProfileSVG :: Map.Map LanguageName Language -> [CommitRecord] -> IO ()
makeProfileSVG langMap records = do
  styles <- H.toHtml <$> IO.readFile (T.unpack stylesheetFilepathFromAuditor)

  IO.putStrLn "Writing SVG..."
  IO.writeFile (T.unpack svgFilepathFromAuditor)
    $ T.pack
    $ renderSvg
    $ profileSVG styles
    $ L.reverse
    $ L.sortOn (commitRecordInsertions . fst)
    $ L.filter ((==) Programming . languageType . snd)
    $ flip mapMaybe records
    $ \rec -> (rec,) <$> Map.lookup (commitRecordLanguage rec) langMap

stylesheetFilepathFromAuditor :: Filepath
stylesheetFilepathFromAuditor = "./style.css"

svgFilepathFromAuditor :: Filepath
svgFilepathFromAuditor = "../profile-card.svg"

profileSVG :: H.Html -> [(CommitRecord, Language)] -> Blaze.Markup
profileSVG styles langs =
  Svg.svg
    ! Svg.fill "none"
    ! Svg.viewbox "0 0 600 400"
    ! Svg.width "600"
    ! Svg.height "400"
    $ Svg.foreignobject
        ! Svg.width "100%"
        ! Svg.height "100%"
        $ profileContent styles langs

profileContent :: H.Html -> [(CommitRecord, Language)] -> Blaze.Markup
profileContent styles _langs = -- TODO
  H.div ! A.xmlns "http://www.w3.org/1999/xhtml" $ do
    H.style styles
    H.div ! A.class_ "body" $ do
      H.div ! A.class_ "container" $ do
        H.h1 $ do
          H.a ! A.href "https://github.com/AugmenTab" $ do
            H.span $ H.em "tyler baum"
            H.span $ H.em "tyler baum"
            H.span $ H.em "tyler baum"
        H.section ! A.class_ "items" $ do
          H.ul $ do
            H.li "Haskell Developer"
          H.ul $ do
            H.li $ do
              H.text "at "
              H.a ! A.href "https://flipstone.com/" $ do
                H.text "Flipstone Technology Partners"
          H.ul $ do
            H.li $ do
              H.a
                ! A.href "mailto:tyler.baum@protonmail.com"
                ! A.role "button"
                $ H.span ! A.class_ "hi" $ "👋"
