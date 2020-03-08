
with Ada.Text_IO;         use  Ada.Text_IO;
with Ada.Integer_Text_IO; use  Ada.Integer_Text_IO;
with Ada.Real_Time;       use  Ada.Real_Time;
with Ada.Execution_Time;  use  Ada.Execution_Time;

with GNAT.Command_Line;   use  GNAT.Command_Line;

with Imago.IL;
use  Imago;

with Util;

with Wave_Function_Collapse;

procedure Main is


  subtype Element is Natural;
  
  type Element_Matrix is
    array (Natural range <>, Natural range <>) of Element;

  procedure Put_Element_Matrix(The_Matrix : in Element_Matrix) is
  begin
    for X in The_Matrix'Range(1) loop
      for Y in The_Matrix'Range(2) loop
        Put( Element'Image(The_Matrix(X, Y)) & " " );
      end loop;
      New_Line;
    end loop;
  end;

  package W_F_C is new Wave_Function_Collapse(Element, Element_Matrix);
  use     W_F_C;

  Config   : Command_Line_Configuration;
  N, M     : aliased Integer;
  Rot, Ref : aliased Boolean;



  IL_Images : aliased array (1 .. 2) of IL.Uint;

  Input_Image  : IL.Uint renames IL_Images(1);
  Output_Image : IL.Uint renames IL_Images(2);

  -- procedure Load_Input_Image (Switch, Filename : String) is
  --   pragma Unreferenced (Switch);
  -- begin
  --   Put_Line("--input=" & Filename);
  --   -- IL.Bind_Image(Input_Image);
  --   -- IL.Load_Image(Filename);
  -- end;

begin

  IL.Init;
  IL.Gen_Images(IL_Images'Length, IL_Images'Address);

  Define_Switch(Config, N'Access, "-n:", Initial => 2);
  Define_Switch(Config, M'Access, "-m:", Initial => 2);
  Define_Switch(Config,
    Switch => "-i:",
    Long_Switch => "--input=",
    Callback => Util.Load_Input_Image'Access
  );

  Define_Switch(Config, Rot'Access, "-r", Long_Switch => "--rot");
  Define_Switch(Config, Ref'Access, "-f", Long_Switch => "--ref");

  Getopt(Config);

  Put_Line(IL_Images(1)'Image);
  Put_Line(IL_Images(2)'Image);


  declare 
    Elements : constant Element_Matrix :=
      (
        (0, 0, 0, 0, 0, 2, 2, 1),
        (1, 0, 0, 0, 0, 0, 0, 2),
        (2, 1, 1, 1, 0, 0, 0, 0),
        (0, 2, 2, 2, 1, 1, 0, 0),
        (0, 0, 0, 0, 2, 2, 1, 1),
        (1, 1, 1, 0, 0, 0, 2, 2),
        (2, 2, 2, 1, 1, 0, 0, 0),
        (0, 0, 0, 2, 2, 1, 1, 0)
      );

    Start_Time : CPU_Time := Clock;

    Inst : constant Instance :=
      Initialize_From_Sample(
        Elements
        , Include_Rotations   => Rot
        , Include_Reflections => Ref
        , N => N
        , M => M
      );

    Total_Time : Duration := To_Duration(CPU_Time'(Clock) - Start_Time);

    Last : Integer;
    Out_Width, Out_Height : Natural;
  begin
     
    Put_Line("Num_Tiles =>"       & Inst.Num_Tiles'Image);
    Put_Line("Instance_Size =>"   & Inst'Size'Image);
    Put_Line("Instance_Bytes =>"  & Integer'Image(Inst'Size / 8));
    Put_Line("Adjacency_Bytes =>" & Integer'Image(Inst.Adjacencies'Size / 8));
    Put_Line("Time =>"            & Total_Time'Image);
    
    New_Line;

    loop
      Get(From => Get_Argument, Item => Out_Width,  Last => Last);
      Get(From => Get_Argument, Item => Out_Height, Last => Last);

      declare
        Num_Attempts : Natural := 0;
        Synthesized : Element_Matrix (1 .. Out_Width, 1 .. Out_Height);
      begin

        Start_Time := Clock;

        loop
          Num_Attempts := Num_Attempts + 1;
          exit when Inst.Collapse_Within(Synthesized);

          if Num_Attempts mod 1000 = 0 then
            Put_Line("Attempts =>" & Natural'Image(Num_Attempts));
          end if;
        end loop;

        Total_Time := To_Duration(CPU_Time'(Clock) - Start_Time);

        Put_Line("Attempts =>" & Natural'Image(Num_Attempts));
        Put_Line("Time =>" & Duration'Image(Total_Time));
        New_Line;

        Put_Element_Matrix(Synthesized);
        New_Line;

      end;
    end loop;

  exception
    when End_Error => return;
  end;

end;