{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Forum where

import Import

-- TODO a better approach would be to have a MenuItem { menuItemRoute :: Either Text (RouteApp) }
-- and not have this handler alltogether
getForumR :: Handler ()
getForumR = redirect ("http://advaitaworld.com" :: Text)
