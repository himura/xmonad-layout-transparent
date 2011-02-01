{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleContexts #-}
----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Transparent
--
-- Stability   :  unstable
-- Portability :  not portable
--
-- Makes the windows transparent
--
-----------------------------------------------------------------------------

module XMonad.Layout.Transparent (
        applyTransp,
        withTransp,
        Transparent
    ) where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Layout.LayoutModifier
import XMonad.Hooks.FadeInactive(fadeOut)
import Data.List

data Transparent a = Transparent Rational deriving (Show, Read)
instance LayoutModifier Transparent Window where
    modifyLayout (Transparent opa) = applyTransp opa

applyTransp :: (LayoutClass l Window) =>
               Rational
            -> W.Workspace WorkspaceId (l Window) Window
            -> Rectangle
            -> X ([(Window, Rectangle)], Maybe (l Window))
applyTransp opa wksp rect = do
    let stack = W.stack wksp
    let ws = W.integrate' $ stack
    mapM_ (fadeOut opa) ws
    runLayout wksp rect

withTransp :: Rational -> l a -> ModifiedLayout Transparent l a
withTransp opa = ModifiedLayout $ Transparent opa
