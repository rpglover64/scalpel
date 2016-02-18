import Text.HTML.Scalpel
import Data.Maybe


exampleHtml :: String
exampleHtml = "<html>\
\    <body>\
\        <div class='comments'>\
\            <div class='comment container'>\
\                <span class='comment author'>Sally</span>\
\                <div class='comment text'>Woo hoo!</div>\
\            </div>\
\            <div class='comment container'>\
\                <span class='comment author'>Bill</span>\
\                <img class='comment image' src='http://example.com/cat.gif' />\
\            </div>\
\            <div class='comment container'>\
\                <span class='comment author'>Susan</span>\
\                <div class='comment text'>WTF!?!</div>\
\            </div>\
\        </div>\
\    </body>\
\</html>"

type Author = String

data Comment
    = TextComment Author String
    | ImageComment Author URL
    deriving (Show, Eq)

orElse :: Scraper str (Maybe a) -> Scraper str (Maybe a) -> Scraper str (Maybe a)
orElse x y = do
  mx <- x
  maybe y (return . Just) mx

main :: IO ()
main = print $ scrapeStringLike exampleHtml comments
    where
    comments :: Scraper String [Comment]
    comments = catMaybes <$> chroots ("div" @: [hasClass "container"]) comment

    comment :: Scraper String (Maybe Comment)
    comment = textComment `orElse` imageComment

    textComment :: Scraper String (Maybe Comment)
    textComment = do
        author      <- text $ "span" @: [hasClass "author"]
        commentText <- text $ "div"  @: [hasClass "text"]
        return $ TextComment <$> author <*> commentText

    imageComment :: Scraper String (Maybe Comment)
    imageComment = do
        author   <- text       $ "span" @: [hasClass "author"]
        imageURL <- attr "src" $ "img"  @: [hasClass "image"]
        return $ ImageComment <$> author <*> imageURL
