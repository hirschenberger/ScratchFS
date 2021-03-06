{-
  ScratchFs is a size limited temp filesystem based on fuse
  Copyright (C) 2011  Falco Hirschenberger <hirsch@bigfoot.de>

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

module Utils (parseSize) where

import qualified Text.Regex as TR

{-| Parse a string defining a file size in bytes. The base unit is kilobytes.
    Allowed units are:

        * /MB/ for megabytes

        * /GB/ for gigabytes

        * /TB/ for terabytes
-}
parseSize :: String         -- ^ The string to parse
          -> Maybe Integer  -- ^ The size multiplied with the unit-factor
parseSize s = case TR.matchRegex (TR.mkRegex "([0-9]+)[ ]*(MB|GB|TB)?") s of
                Just [num, ""]   -> Just $ (read num ::Integer) * 1024
                Just [num, "MB"] -> Just $ (read num ::Integer) * 1024^(2::Integer)
                Just [num, "GB"] -> Just $ (read num ::Integer) * 1024^(3::Integer)
                Just [num, "TB"] -> Just $ (read num ::Integer) * 1024^(4::Integer)
                _ -> Nothing


