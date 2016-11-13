{-# LANGUAGE LambdaCase, RecordWildCards, NamedFieldPuns, OverloadedStrings #-}
{-# LANGUAGE AutoDeriveTypeable, DeriveDataTypeable #-}
-- {-# LANGUAGE OverloadedLists #-}
import XMonad 
import XMonad.Util.EZConfig                           --
import XMonad.Util.CustomKeys                         -- 
import XMonad.Util.Run                                -- 
import XMonad.Actions.SpawnOn                         -- 
import XMonad.Util.ExtensibleState                    -- 
import qualified XMonad.Util.ExtensibleState as XS    -- 
import XMonad.Actions.WindowGo                        -- 
-- import XMonad.Layout.ResizableTile -- for vertical resizing
-- import XMonad.Actions.GridSelect -- show all windows in grid, focus by clicking
-- import XMonad.Actions.CopyWindow -- fake menubar, i.e. same window with constant location in each workspace
-- import XMonad.Actions.                          -- 

import qualified Data.Map as M
import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Function
import GHC.Exts(IsString(..))
import System.Exit

--------------------------------------------------------------------------------

main = xmonad myConfig

myConfig = defaultConfig
 { modMask = mod4Mask  -- super instead of alt (usually Windows key)
 , terminal = myTerminal&appExecutable
 , clickJustFocuses = False
 , focusFollowsMouse = False
 , manageHook = myManageHook
 , startupHook = myStartupHook
 , workspaces = myWorkspaces
 , keys = myKeys
 }

myWorkspaces = show <$> [1..3]

--------------------------------------------------------------------------------
-- apps
-- find `className`s via `xprop | grep CLASS`

myTerminal = App "xterm" "XTerm"
-- terminal myConfig

myBrowser = App "chromium" "chromium-browser"

myEditor = App "emacs" "Emacs" 

--------------------------------------------------------------------------------

-- called on new window
myManageHook = manageSpawn <+> (defaultConfig&manageHook)

--------------------------------------------------------------------------------

myKeys = customKeys delKeys addKeys

delKeys XConfig{modMask} =
   [ hk [modMask] xK_E
   , hk [modMask, shiftMask]  xK_q -- too easy to press
   ]

addKeys XConfig{modMask} = -- TODO doesnt work
 [ kb [modMask] xK_E $ bringApp myEditor
 , hk [modMask] xK_B -: bringApp myBrowser
 , hk [modMask, shiftMask, controlMask] xK_q -: quitXMonad
 , hk [modMask] xK_C -: closeWindow
 ]

{-
myKeys = newKeys <+> (defaultConfig&keys)
newKeys XConfig{modMask} = M.fromList
 [ (modMask, xK_E) -: bringApp myEditor
 , (modMask, xK_B) -: bringApp myBrowser
 ]
-}

-- http://hackage.haskell.org/packages/archive/X11/latest/doc/html/Graphics-X11-Types.html

--------------------------------------------------------------------------------

--TODO state reset on reload
data MyState = MyState {sIsReload :: Bool } deriving (Typeable,Show,Read)
instance ExtensionClass MyState where
   initialValue = MyState False

myStartupHook = onReload

-- first load and every reload
onReload = do
  -- only run start up hook the first time (ie not after reloading)
  unlessM (sIsReload <$> XS.get) $ do
    XS.modify $ (\c -> c{sIsReload = True})
    onLoad

-- first load only
onLoad = do
  -- traverse_ (spawnOn "1")
  traverse_ (launchApp&uncurry) 
    [ myTerminal -: []
    , myBrowser  -: []
    , myEditor   -: ["~/.xmonad/xmonad.hs"]
    ]

--------------------------------------------------------------------------------

data App = App
  { appExecutable :: String
  , appClassName :: String
  }

bringApp :: App -> X()
bringApp App{..} = runOrRaise appExecutable (className =? appClassName)

-- does nothing if the window exists
launchApp :: App -> [String] -> X()
launchApp App{..} arguments = ifWindows (className =? appClassName) (const nothing) (safeSpawn appExecutable arguments)

mapAppExecutable f app = app{appExecutable = f (app&appExecutable)}

--------------------------------------------------------------------------------

quitXMonad = io (exitWith ExitSuccess)

closeWindow = kill

masks = foldr (.|.) noModMask

-- hotkey
hk ms k = (masks ms, k)

-- keybinding
kb ms k a = hk ms k -: a

--------------------------------------------------------------------------------

(-:) = (,)

nothing = return()

filterBlanks :: (IsString k, Eq k) => [(k,v)] -> [(k,v)]
filterBlanks = filter $ \case
 ("",_) -> False
 (_, _) -> True

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM b s = b >>= (\t -> unless t s)

--------------------------------------------------------------------------------

{-


XMonad.Layout.ResizableTile


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
