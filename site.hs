--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import Text.Pandoc.Highlighting (Style, breezeDark, styleToCss)
import Text.Pandoc.Options      (ReaderOptions (..), WriterOptions (..))
import Text.Pandoc

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "favicon.ico" $ do
        route   idRoute
        compile copyFileCompiler

    match "images/**" $ do
        route   idRoute
        compile copyFileCompiler
        
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler
    
    -- css for syntax highlighting
    create ["css/syntax.css"] $ do
        route idRoute
        compile $ do
            makeItem $ styleToCss pandocCodeStyle

    --match (fromList ["about.rst", "contact.markdown"]) $ do
    match (fromList []) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler'
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler'
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts)
                    <> defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y"
    <> readingTimeField "readingtime"
    <> defaultContext

pandocCodeStyle :: Style
pandocCodeStyle = breezeDark

pandocCompiler' :: Compiler (Item String)
pandocCompiler' =
  pandocCompilerWith
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
      { writerHighlightStyle  = Just pandocCodeStyle
      , writerTableOfContents = True
      , writerTOCDepth        = 3
      , writerTemplate        = Just tocTemplate
      }

tocTemplate =
    either error id $ either (error . show) id $
    runPure $ runWithDefaultPartials $
    compileTemplate "" "$toc$\n$body$"

readingTimeField :: String -> Context String
readingTimeField key =
  field key calculate
  where
    -- M. Brysbaert, Journal of Memory and Language (2009) vol 109.
    -- DOI: 10.1016/j.jml.2019.104047
    calculate :: Item String -> Compiler String
    calculate s = pure $ show $ length (words $ itemBody s) `div` 238
