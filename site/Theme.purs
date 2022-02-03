module Site.Theme where

import CSS (GenericFontFamily, sansSerif)
import Data.Tuple.Nested (type (/\), (/\))
import Color (Color, hsl)
import Data.NonEmpty (NonEmpty, singleton)

sans :: Array String /\ NonEmpty Array GenericFontFamily
sans = ["Noto Sans"] /\ singleton sansSerif

mono :: Array String /\ NonEmpty Array GenericFontFamily
mono = ["Noto Sans Mono"] /\ singleton sansSerif

blue100 = hsl 210.0 0.923 0.949 :: Color
blue200 = hsl 210.0 0.938 0.873 :: Color
blue300 = hsl 210.0 0.942 0.796 :: Color
blue400 = hsl 210.0 0.945 0.643 :: Color
blue50 = hsl 208.0 1.0 0.975 :: Color
blue500 = hsl 210.0 0.984 0.49 :: Color
blue600 = hsl 210.0 0.982 0.441 :: Color
blue700 = hsl 210.0 0.979 0.369 :: Color
blue800 = hsl 210.0 0.987 0.294 :: Color
blue900 = hsl 210.0 0.984 0.241 :: Color
crimson100 = hsl 356.0 0.68 0.951 :: Color
crimson200 = hsl 356.0 0.71 0.878 :: Color
crimson300 = hsl 357.0 0.697 0.806 :: Color
crimson400 = hsl 356.0 0.701 0.659 :: Color
crimson50 = hsl 353.0 0.692 0.975 :: Color
crimson500 = hsl 356.0 0.702 0.514 :: Color
crimson600 = hsl 356.0 0.661 0.463 :: Color
crimson700 = hsl 356.0 0.665 0.386 :: Color
crimson800 = hsl 355.0 0.669 0.308 :: Color
crimson900 = hsl 356.0 0.659 0.253 :: Color
gray100 = hsl 206.0 0.171 0.92 :: Color
gray200 = hsl 212.0 0.168 0.802 :: Color
gray300 = hsl 209.0 0.166 0.68 :: Color
gray400 = hsl 210.0 0.212 0.443 :: Color
gray50 = hsl 210.0 0.2 0.961 :: Color
gray500 = hsl 210.0 0.654 0.204 :: Color
gray600 = hsl 210.0 0.656 0.182 :: Color
gray700 = hsl 211.0 0.646 0.155 :: Color
gray800 = hsl 211.0 0.651 0.124 :: Color
gray900 = hsl 211.0 0.647 0.1 :: Color
green100 = hsl 130.0 0.6 0.961 :: Color
green200 = hsl 129.0 0.56 0.902 :: Color
green300 = hsl 127.0 0.57 0.845 :: Color
green400 = hsl 128.0 0.568 0.727 :: Color
green50 = hsl 130.0 0.6 0.98 :: Color
green500 = hsl 127.0 0.568 0.61 :: Color
green600 = hsl 128.0 0.443 0.549 :: Color
green700 = hsl 128.0 0.365 0.457 :: Color
green800 = hsl 128.0 0.366 0.365 :: Color
green900 = hsl 127.0 0.359 0.3 :: Color
yellow100 = hsl 50.0 1.0 0.976 :: Color
yellow200 = hsl 48.0 1.0 0.939 :: Color
yellow300 = hsl 49.0 1.0 0.902 :: Color
yellow400 = hsl 49.0 1.0 0.829 :: Color
yellow50 = hsl 50.0 1.0 0.988 :: Color
yellow500 = hsl 49.0 1.0 0.755 :: Color
yellow600 = hsl 49.0 0.693 0.68 :: Color
yellow700 = hsl 49.0 0.421 0.567 :: Color
yellow800 = hsl 49.0 0.325 0.453 :: Color
yellow900 = hsl 49.0 0.323 0.371 :: Color
