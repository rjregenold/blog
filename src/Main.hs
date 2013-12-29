{-# LANGUAGE OverloadedStrings #-}

import           Data.Monoid ((<>))
import           Hakyll


hakyllConfiguration :: Configuration
hakyllConfiguration = defaultConfiguration {
  deployCommand =
    "rsync -ave ssh _site/ " ++
    "binarylionstudios.com:/var/www/binarylionstudios.com"
}

main :: IO ()
main = hakyllWith hakyllConfiguration $ do
  match "templates/*" $ compile templateCompiler

  match "static/*" $ do
    route stripStatic
    compile copyFileCompiler

  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "css/*.hs" $ do
    route   $ setExtension "css"
    compile $ getResourceString >>= withItemBody (unixFilter "runghc" ["-package-db=.cabal-sandbox/x86_64-osx-ghc-7.6.3-packages.conf.d"])

  match "css/*.css" $ do
    route   idRoute
    compile compressCssCompiler

  match "posts/*" $ do
    route $ setExtension "html"
    compile $
      pandocCompiler
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/post.html" postContext
      >>= loadAndApplyTemplate "templates/default.html" postContext

  match "content/index.html" $ do
    route stripContent
    compile $ do
      posts <- latestPosts
      let indexContext = listField "posts" (postTeaserContext "content") (return posts) <> defaultContext
      getResourceBody
        >>= applyAsTemplate indexContext
        >>= loadAndApplyTemplate "templates/default.html" indexContext

  create ["rss.xml"] $ do
    route idRoute
    compile $ do
      let feedContext = postContext <> bodyField "description"
      posts <- latestPosts
      renderRss feedConfiguration feedContext posts


latestPosts :: Compiler [Item String]
latestPosts = recentFirst =<< loadAllSnapshots "posts/*" "content"

postContext :: Context String
postContext = dateField "date" "%A, %B %e, %Y" 
           <> defaultContext

postTeaserContext :: String -> Context String
postTeaserContext snapshot = teaserField "teaser" snapshot <> postContext

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
  { feedTitle       = "Binary Lion Studios feed"
  , feedDescription = "Feed for Binary Lion Studios blog."
  , feedAuthorName  = "RJ Regenold"
  , feedAuthorEmail = "rj[deletethis]regenold@gmail.com"
  , feedRoot        = "http://binarylionstudios.com"
  }

stripRoute :: String -> Routes
stripRoute = flip gsubRoute $ const ""

stripContent :: Routes
stripContent = stripRoute "content/"

stripStatic :: Routes
stripStatic = stripRoute "static/"
