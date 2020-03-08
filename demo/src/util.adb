
with Ada.Text_IO; use Ada.Text_IO;

package body Util is

  procedure Load_Input_Image (Switch, Filename : String) is
    pragma Unreferenced (Switch);
  begin
    Put_Line("--input=" & Filename);
    -- IL.Bind_Image(Input_Image);
    -- IL.Load_Image(Filename);
  end;

end;