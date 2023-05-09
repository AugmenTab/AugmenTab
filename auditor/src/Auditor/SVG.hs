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
  Svg.svg !* [ Svg.fill "none"
             , Svg.viewbox "0 0 600 400"
             , Svg.width "600"
             , Svg.height "400"
             ] $ do
    Svg.foreignobject !* [ Svg.width "100%", Svg.height "100%" ] $ do
        profileContent styles langs

profileContent :: H.Html -> [(CommitRecord, Language)] -> Blaze.Markup
profileContent styles langs =
  H.div ! A.xmlns "http://www.w3.org/1999/xhtml" $ do
    H.style styles
    H.div ! A.class_ "body" $ do
      H.div ! A.class_ "container" $ do
        H.h1 $ do
          H.a ! A.href "https://github.com/AugmenTab" $ do
            H.span $ H.em "Tyler Baum"
            H.span $ H.em "Tyler Baum"
            H.span $ H.em "Tyler Baum"
        H.section ! A.class_ "slider-container" $ do
          H.div ! A.class_ "slider" $ do
            H.div ! A.class_ "slides" $ do
              makeDetailCard
              makeLanguageCard langs
              makeFoundryCard

previousCardLink :: Svg.AttributeValue -> Blaze.Markup
previousCardLink link =
  H.a !* [ A.class_ "slide__prev", A.href link, A.title "Prev"] $ mempty

nextCardLink :: Svg.AttributeValue -> Blaze.Markup
nextCardLink link =
  H.a !* [ A.class_ "slide__next", A.href link, A.title "Next"] $ mempty

makeDetailCard :: Blaze.Markup
makeDetailCard =
  H.div !* [ A.id "slides__1", A.class_ "slide" ] $ do
    H.div ! A.class_ "details" $ do
      H.p "Haskell Developer"
      H.p $ do
        H.a ! A.href "https://flipstone.com/" $ do
          H.text "Flipstone Technology Partners"
      H.p $ do
        H.a !* [ A.href "mailto:tyler.baum@protonmail.com"
               , A.role "button"
               ] $ do
          H.text "EMAIL ME"
    previousCardLink "#slides__3"
    nextCardLink "#slides__2"

makeLanguageCard :: [(CommitRecord, Language)] -> Blaze.Markup
makeLanguageCard _langs =
  H.div !* [ A.id "slides__2", A.class_ "slide" ] $ do
 -- H.text "" TODO
    previousCardLink "#slides__1"
    nextCardLink "#slides__3"

makeFoundryCard :: Blaze.Markup
makeFoundryCard =
  H.div !* [ A.id "slides__3", A.class_ "slide" ] $ do
 -- H.text "" TODO
    previousCardLink "#slides__2"
    nextCardLink "#slides__1"

{-| Takes an element and a list of attributes, and applies the attributes to
   the element in the order given. The operator is meant to convey: perform the
   (!) operation multiple times.
 -}
(!*) :: (H.Html -> H.Html) -> [Svg.Attribute] -> H.Html -> H.Html
element !* attributes = L.foldl' (!) element attributes
