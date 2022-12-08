module Site.Storybook where

import Prelude

import Color (rgb, rgba)
import Data.Tuple.Nested ((/\))
import Tecton (CSS, alignItems, auto, backgroundColor, block, body, bold, borderBottomStyle, borderLeftStyle, borderStyle, borderTopStyle, borderWidth, center, color, display, flex, fontFamily, fontSize, fontWeight, fr, grid, gridColumnStart, gridRowEnd, gridRowStart, gridTemplateColumns, gridTemplateRows, height, hover, listStyleType, margin, nil, none, overflow, overflowY, padding, paddingLeft, px, rem, sansSerif, solid, textDecorationLine, textTransform, universal, uppercase, vh, (&.), (&:), (:=), (?), (~))
import Tecton.Internal (borderColor)
import Tecton.Rule as Rule

css :: CSS
css = do
  body ? Rule.do
    margin := nil
    fontFamily := "Noto Sans" /\ sansSerif
  universal &. "Storybook" ? Rule.do
    height := vh 100
    display := grid
    gridTemplateColumns := rem 20 /\ fr 1
    gridTemplateRows := rem 4 /\ fr 1
  universal &. "Storybook-logo" /\ universal &. "Storybook-nav" ? Rule.do
    borderColor := rgba 0 0 0 0.08
    borderWidth := px 1
    borderStyle := solid
    borderTopStyle := none
    borderLeftStyle := none
  universal &. "Storybook-logo" ? Rule.do
    display := flex
    alignItems := center
    paddingLeft := rem 2
    textDecorationLine := none
    backgroundColor := rgb 250 250 250
    color := rgb 40 40 40
  universal &. "Storybook-nav" ? Rule.do
    gridRowStart := 2
    overflowY := auto
    fontSize := rem 0.875
    backgroundColor := rgb 250 250 250
    borderBottomStyle := none
  universal &. "Storybook-nav-list" ? Rule.do
    listStyleType := none
    margin := nil
    padding := nil
  universal &. "Storybook-nav-section" ?
    margin := rem 1 ~ nil
  universal &. "Storybook-nav-section-title" ? Rule.do
    color := rgb 58 58 58
    textTransform := uppercase
    fontWeight := bold
    padding := rem 0.625 ~ rem 2
  universal &. "Storybook-link" ? Rule.do
    display := block
    textDecorationLine := none
    padding := rem 0.625 ~ rem 2
    -- wordWrap := breakWord -- TODO
    color := rgb 40 40 40
  ( universal &. "Storybook-link" &: hover
    /\ universal &. "Storybook-link" &. "is-active"
  ) ?
    color := rgb 0 140 255
  universal &. "Storybook-main" ? Rule.do
    gridColumnStart := 2
    gridRowStart := 1
    gridRowEnd := -1
    padding := rem 2
    overflow := auto
