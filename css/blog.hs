{-# LANGUAGE OverloadedStrings #-}

import           Prelude hiding ((**), div, span)
import           Control.Monad
import           Clay
import qualified Clay.Media as M
import           Data.Monoid
import qualified Data.Text.Lazy.IO as T


-- colors
blogRed         = "#6D3B34"
blogLightGray   = "#E7E4E7"
blogGreen       = "#9F9B5C"
blogBlue        = "#1E335A"
blogDarkGray    = "#7A707B"
syntaxBgColor   = "#303030"
syntaxFontColor = "#CCCCCC"

-- fonts
headerFontStack = ["Roboto Slab"]
syntaxFontStack = ["Droid Sans Mono", "Lucida Console", "Monaco"]


hs = h1 <> h2 <> h3 <> h4 <> h5 <> h6

linkStates = a
  <> (a # link)
  <> (a # visited)
  <> (a # active)
  <> (a # hover)

blog :: Css
blog = do
  hs ? do
    fontFamily headerFontStack [serif]
    a ? do
    textDecoration none
    hover & textDecoration underline

  hs ** span <> ".date-stamp" ? do
    color blogDarkGray
    fontSize $ pct 40

  ".date-stamp" ? lineHeight (px 10)

  h2 ** linkStates ? color blogRed

  ".pull-right" ? float floatRight

  ul # ".inline" ** li ? display inline

  header ** h1 ? do
    borderBottom solid (px 1) blogLightGray
    marginBottom (px 20)
    marginTop (px 40)
    paddingBottom (px 10)
    ul ** li ? do
      display inline

  ".separateable" ? do
    li ? do
      borderRight solid (px 1) blogDarkGray
      float floatLeft
      lineHeight (px 14)
      marginRight (px 10)
      padding (px 0) (px 10) (px 0) (px 0)
      ":last-child" & do
        borderRightStyle none
        marginRight (px 0)
        paddingRight (px 0)

  "#be-social" ** li ? do
    fontSize (px 14)
    marginLeft (px 15)
    marginRight 0
    linkStates ? do
      color blogDarkGray
      fontSizeCustom smaller

  div # ".post" ? do
    h2 ? do
      marginBottom (px 20)
      ".subtitle" ? do
        color blogDarkGray
        fontSizeCustom smaller
    borderBottom solid (px 1) blogLightGray
    marginBottom (px 30)
    ":last-child" & do
      borderBottomStyle none
      marginBottom 0
    ".read-more" ? do
      marginBottom (px 10)
      a ? do
        color blogRed
        fontSizeCustom smaller
        textDecoration none
      a # hover ? textDecoration underline
    let refs = ".references"
    refs ? fontSize (px 17)
    refs |+ ul ? do
      "list-style" -: "circle inside"
      li ? marginBottom (px 0)

  footer ? do
    borderTop solid (px 1) blogLightGray
    fontSizeCustom smaller
    marginTop (px 40)
    sym2 padding (px 10) 0

  tabletsAndPhones
  syntax

tabletsAndPhones = queryOnly M.screen [M.maxWidth (px 959)] $ do
  "#be-social" <> "#made-with" ? display displayNone

syntaxMap = 
  [ (".kw", "#f0dfaf")
  , (".dt", "#dfdfbf")
  , (".dv", "#dcdccc")
  , (".bn", "#dca3a3")
  , (".fl", "#c0bed1")
  , (".ch", "#dca3a3")
  , (".st", "#cc9393")
  , (".co", "#7f9f7f")
  , (".ot", "#efef8f")
  , (".al", "#ffcfaf")
  , (".fu", "#efef8f")
  , (".re", "#ffd7a7")
  , (".er", "#c3bf9f")
  ]

syntax :: Css
syntax = do
  pre # ".sourceCode" <> table # ".sourceCode" ? do
    backgroundColor syntaxBgColor
    sym borderRadius (px 10)
    color syntaxFontColor
    display block
    fontFamily syntaxFontStack [monospace, sansSerif]
    sym2 margin (px 20) (px 0)
    overflowX scroll
    sym padding (px 10)

  ".lineNumbers" ? do
    paddingRight (px 15)
    textAlign (alignSide sideRight)

  ".sourceCode" ? do
    forM_ syntaxMap $ \(s,c) -> span # s ? color c

  code # ":not(.sourceCode)" ? do
    backgroundColor syntaxBgColor
    sym borderRadius (px 2)
    color syntaxFontColor
    sym2 padding (px 2) (px 4)


main :: IO ()
main = T.putStr $ render blog
