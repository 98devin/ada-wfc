

with Imago; with Imago.IL;
use  Imago;

with WFC;

with GNAT.Command_Line;
use  GNAT.Command_Line;

package Util is
  
  Argument_Error  : exception;
  Execution_Error : exception;

  type Image_Color is (R, G, B);

  type Image_Pixel is
    array (Image_Color) of IL.UByte
    with Pack;

  type Image_Matrix is
    array (Natural range <>, Natural range <>) of Image_Pixel
    with Pack;

  package Image_WFC is new WFC(Image_Pixel, Image_Matrix);
  -- The generic instantiation we will use
  -- when provided an image at the command line.

  type Character_Matrix is
    array (Natural range <>, Natural range <>) of Character;

  package Character_WFC is new WFC(Character, Character_Matrix);
  -- The generic instantiation we will use
  -- when instead provided a simple text file.

  procedure Define_CLI_Switches (Config : in out Command_Line_Configuration);

  procedure Process_Command_Arguments;

end;