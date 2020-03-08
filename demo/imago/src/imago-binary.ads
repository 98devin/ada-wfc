------------------------------------------------------------------------------
-- EMAIL: <darkestkhan@gmail.com>                                           --
-- License: ISC                                                             --
--                                                                          --
--                    Copyright © 2015 darkestkhan                          --
------------------------------------------------------------------------------
-- Permission to use, copy, modify, and/or distribute this software for any --
-- purpose with or without fee is hereby granted, provided that the above   --
-- copyright notice and this permission notice appear in all copies.        --
--                                                                          --
-- The software is provided "as is" and the author disclaims all warranties --
-- with regard to this software including all implied warranties of         --
-- merchantability and fitness. In no event shall the author be liable for  --
-- any special, direct, indirect, or consequential damages or any damages   --
-- whatsoever resulting from loss of use, data or profits, whether in an    --
-- action of contract, negligence or other tortious action, arising out of  --
-- or in connection with the use or performance of this software.           --
------------------------------------------------------------------------------

  --------------------------------------------------------------------------
  -- This package is based on Lumen.Binary package from Lumen library.    --
  --------------------------------------------------------------------------

package Imago.Binary is

  --------------------------------------------------------------------------

  pragma Pure;

  --------------------------------------------------------------------------

                          -----------------------
                          -- C O N S T A N T S --
                          -----------------------

  --------------------------------------------------------------------------
  -- Basic sizes of fundamental binary data types.
  Byte_Bits : constant := 8;
  Short_Bits: constant := 16;
  Word_Bits : constant := 32;
  Long_Bits : constant := 64;

  --------------------------------------------------------------------------
  -- Derived sizes.
  Short_Bytes: constant := Short_Bits / Byte_Bits;
  Word_Bytes : constant := Word_Bits  / Byte_Bits;
  Long_Bytes : constant := Long_Bits  / Byte_Bits;

  --------------------------------------------------------------------------
  -- "Last-bit" values for use in rep clauses.
  Byte_LB : constant := Byte_Bits  - 1;
  Short_LB: constant := Short_Bits - 1;
  Word_LB : constant := Word_Bits  - 1;
  Long_LB : constant := Long_Bits  - 1;

  --------------------------------------------------------------------------

                              ---------------
                              -- T Y P E S --
                              ---------------

  --------------------------------------------------------------------------
  -- Unsigned types.
  type Byte   is mod 2 ** Byte_Bits;
  type Short  is mod 2 ** Short_Bits;
  type Word   is mod 2 ** Word_Bits;
  type Long   is mod 2 ** Long_Bits;

  for Byte'Size   use Byte_Bits;
  for Short'Size  use Short_Bits;
  for Word'Size   use Word_Bits;
  for Long'Size   use Long_Bits;

  --------------------------------------------------------------------------
  -- Signed types.
  type S_Byte   is new Integer range -(2 ** Byte_LB)  .. (2 ** Byte_LB)  - 1;
  type S_Short  is new Integer range -(2 ** Short_LB) .. (2 ** Short_LB) - 1;
  type S_Word   is new Integer range -(2 ** Word_LB)  .. (2 ** Word_LB)  - 1;
  type S_Long   is new Long_Integer range -(2 ** Long_LB) .. (2 ** Long_LB) - 1;

  for S_Byte'Size   use Byte_Bits;
  for S_Short'Size  use Short_Bits;
  for S_Word'Size   use Word_Bits;
  for S_Long'Size   use Long_Bits;

  --------------------------------------------------------------------------
  -- Array types.
  type Byte_Array     is array (Natural range <>) of Byte;
  type Short_Array    is array (Natural range <>) of Short;
  type Word_Array     is array (Natural range <>) of Word;
  type Long_Array     is array (Natural range <>) of Long;

  type S_Byte_Array   is array (Natural range <>) of S_Byte;
  type S_Short_Array  is array (Natural range <>) of S_Short;
  type S_Word_Array   is array (Natural range <>) of S_Word;
  type S_Long_Array   is array (Natural range <>) of S_Long;

  ---------------------------------------------------------------------------

end Imago.Binary;
