{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
-- {-# LANGUAGE OverloadedLists #-}
import XMonad 
import XMonad.Util.EZConfig 
import XMonad.Actions.SpawnOn
-- import XMonad.Actions. -- 
-- import XMonad.Actions.GridSelect -- show all windows in grid, focus by clicking
-- import XMonad.Actions.CopyWindow -- fake menubar, i.e. same window with constant location in each workspace

import qualified Data.Map as M
import Control.Applicative
import Data.Foldable

main = xmonad myConfig

myConfig = defaultConfig
 { modMask = mod4Mask  -- super instead of alt (usually Windows key)
 , terminal = "xterm"
 , clickJustFocuses = False
 , focusFollowsMouse = False
 , manageHook = myManageHook
 , startupHook = myStartupHook
 , workspaces = myWorkspaces
 , keys = myKeys
 }

-- called on (re/)loading
myStartupHook = do
  traverse_ (spawnOn "1")
    [ terminal myConfig
    -- , "chromium"
    -- , "emacs"
    ]

-- called on new window
myManageHook = manageSpawn <+> manageHook defaultConfig

myWorkspaces = show <$> [1..3]

myKeys = newKeys <+> keys defaultConfig

newKeys XConfig{modMask} = M.fromList []
 -- [ ((modMask, xK_F12), xmonadPrompt defaultXPConfig)          
 -- , ((modMask, xK_F3 ), shellPrompt  defaultXPConfig)
 -- ]
    
-- http://hackage.haskell.org/packages/archive/X11/latest/doc/html/Graphics-X11-Types.html

{-


http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Hooks-ServerMode.html


workflow-linux: (workflow-X11)
XMonad.Util.Paste: A module for sending key presses to windows. This modules provides generalized and specialized functions for this task.
XMonad.Hooks.SetCursor: Set a default mouse cursor on startup.
XMonad.Util.XSelection: A module for accessing and manipulating X Window's mouse selection (the buffer used in copy and pasting). getSelection and putSelection are adaptations of Hxsel.hs and Hxput.hs from the XMonad-utils


XMonad.Util.Types: Miscellaneous commonly used types.


XMonad.Util.WindowProperties: EDSL for specifying window properties; various utilities related to window properties.



   property =? match --> action
property:
title: the window's title
resource: the resource name
className: the resource class name.
stringProperty somestring: the contents of the property somestring.


(You can retrieve the needed information using the X utility named xprop; for example, to find the resource class name, you can type
 xprop | grep WM_CLASS
at a prompt, then click on the window whose resource class you want to know.)
 if a window matches multiple hooks, each hook is run in order


   import XMonad.Hooks.DynamicLog
 , logHook = dynamicLog


XMonad.Hooks.ManageHelpers


makeHook property match action = property =? match --> action


className = makeHook X.className
title = makeHook X.title
resource = makeHook X.resource


className “emacs” $ do


import qualified XMonad.StackSet as W
doF $ W.shift "<workspace>"



-}
