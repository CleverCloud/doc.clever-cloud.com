--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative ((<$>))
import           Data.Monoid         (mappend)
import           Hakyll
import           System.FilePath.Posix  (takeBaseName,takeDirectory,(</>),splitFileName)
--------------------------------------------------------------------------------

main :: IO ()
main = hakyll $ do

    -- Copy files and images
    match ("assets/images/*" .||. "assets/js/*" .||. "assets/font/*" .||. "assets/fancybox/*") $ do
        route   idRoute
        compile copyFileCompiler

    -- Compress css
    match "assets/css/*.css" $ do
        route   idRoute
        compile compressCssCompiler

    -- Compile less
    match "assets/css/*.less" $ do
        route   $ setExtension "css"
        compile $ getResourceString >>=
            withItemBody (unixFilter "lessc" ["-","--yui-compress","-O2"])

    -- Render posts
    match "posts/*/*" $ do
        route $ niceRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            let indexCtx = field "posts" $ \_ -> postList (take 3 . recentFirst)

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    --dateField "date"-- "%B %e, %Y" `mappend`
    defaultContext


--------------------------------------------------------------------------------
postList :: ([Item String] -> [Item String]) -> Compiler String
postList sortFilter = do
    posts   <- sortFilter <$> loadAll "posts/*/*"
    itemTpl <- loadBody "templates/post-item.html"
    list    <- applyTemplateList itemTpl postCtx posts
    return list

--------------------------------------------------------------------------------
--
-- replace a foo/bar.md by foo/bar/index.html
-- this way the url looks like: foo/bar in most browsers
niceRoute :: Routes
niceRoute = customRoute createIndexRoute
  where
    createIndexRoute ident = takeBaseName p </> "index.html"
      where p = toFilePath ident