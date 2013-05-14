--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative ((<$>))
import           Control.Monad (mapM,(>=>))
import           Data.List          (intersperse)
import           Data.Maybe          (fromMaybe)
import           Data.Monoid         (mappend,mempty)
import           Hakyll
import           System.FilePath.Posix  (dropExtension,(</>),splitDirectories,joinPath,takeBaseName)
import qualified Data.Map as M
--------------------------------------------------------------------------------

main :: IO ()
main = hakyll $ do

    -- Copy files and images
    match ("assets/images/*" .||. "assets/js/*" .||. "assets/font/*" .||. "assets/magnific-popup/*") $ do
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
    match "categories/*/*" $ do
        route $ niceRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            let indexCtx = field "posts" $ \_ -> postList

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler
    match "categories/*.md" $ compile getResourceBody


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    --dateField "date"-- "%B %e, %Y" `mappend`
    field "sidebar" makeSideBar `mappend`
    defaultContext


--------------------------------------------------------------------------------
postList :: Compiler String
postList = do
    posts   <- loadAll "categories/*/*"
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
    createIndexRoute ident = withoutCategory </> "index.html"
      where p = toFilePath ident
            withoutCategory = joinPath . drop 1 . splitDirectories $ dropExtension p

makeSideBar :: Item String -> Compiler String
makeSideBar item = do
    categories_md <- getAllMetadata "categories/*.md"
    blocks <- mapM (makeCategoryMenuItem item) categories_md
    mapM_ (debugCompiler . show) blocks
    tpl <- loadBody "templates/menu-category.html"
    applyTemplateList tpl ctx blocks
  where
    ctx = defaultContext

makeCategoryMenuItem :: Item String -> (Identifier,Metadata) -> Compiler (Item String)
makeCategoryMenuItem current (id, md) = mkItem $ do
    pages_md <- getAllMetadata (fromGlob $ "categories/" ++ category_id ++ "/*.md")
    tpl <- loadBody "templates/menu-item.html"
    applyTemplateListWithContexts tpl (makeItemContextPairListWith pages_md mkCtx)
  where
    mkItem = fmap (Item id)
    category_name = fromMaybe "" $ M.lookup "name" md
    category_id = takeBaseName . toFilePath $ id
    mkCtx id = constField "cc-sidebar__item__active" $
        if id == itemIdentifier current then " class=\"active\""
        else ""

makeDefaultContext :: (Identifier, Metadata) -> Context String
makeDefaultContext (i, m) =
        makeUrlField i `mappend`
        makeMetadataContext m
    where
        makeMetadataContext m =
            (Context $ \k _ -> do
                return $ fromMaybe "" $ M.lookup k m)

        makeUrlField id =
            field "url" $ \_ -> do
                fp <- getRoute id
                return $ fromMaybe "" $ fmap toUrl fp

makeItemContextPairList :: [(Identifier, Metadata)] -> [(Context String, Item String)]
makeItemContextPairList ims =
    makeItemContextPairListWith ims (const mempty)

makeItemContextPairListWith :: [(Identifier, Metadata)]
                            -> (Identifier -> Context String)
                            -> [(Context String, Item String)]
makeItemContextPairListWith ims a = map f ims
    where
    f p = ((a $ fst p) `mappend` makeDefaultContext p, Item (fst p) "")

applyTemplateListWithContexts :: Template
                              -> [(Context a, Item a)]
                              -> Compiler String
applyTemplateListWithContexts =
    applyJoinTemplateListWithContexts ""

applyJoinTemplateListWithContexts :: String
                                  -> Template
                                  -> [(Context a, Item a)]
                                  -> Compiler String
applyJoinTemplateListWithContexts delimiter tpl pairs = do
    items <- mapM (\p -> applyTemplate tpl (fst p) (snd p)) pairs
    return $ concat $ intersperse delimiter $ map itemBody items

