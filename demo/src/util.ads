

with Imago.IL;
use  Imago;

with Wave_Function_Collapse;


package Util is
  
  type Image_Color is (R, G, B)

  type Image_Pixel is
    array (Image_Color) of IL.Byte;

  type Image_Matrix is
    array (Natural range <>, Natural range <>) of Image_Pixel;

  package Image_WFC is new
    Wave_Function_Collapse(Image_Pixel, Image_Matrix);


  type Character_Matrix is
    array (Natural range <>, Natural range <>) of Character;

  package Character_WFC is new
    Wave_Function_Collapse(Character, Character_Matrix);

  procedure Load_Input_Image (Switch, Filename : String);

end;