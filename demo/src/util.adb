

with Ada.Containers.Indefinite_Vectors;

with Imago.IL; with Imago.ILU;
use  Imago.IL; use  Imago.ILU;

use type Imago.IL.Bool;
use type Imago.IL.UInt;

with Ada.Text_IO;
use  Ada.Text_IO;

with Ada.Integer_Text_IO;
use  Ada.Integer_Text_IO;

with GNATCOLL.Strings;
use  GNATCOLL.Strings;

with Ada.Strings.Maps;
with Ada.Strings.Fixed;


package body Util is

  subtype IL_Image is IL.UInt;

  type Im_Instance_Access is access Image_WFC.Instance;
  type Ch_Instance_Access is access Character_WFC.Instance;

  generic
    with package The_WFC is new WFC(<>);
  procedure Put_Instance_Info (Inst : in The_WFC.Instance);
  
  procedure Put_Instance_Info (Inst : in The_WFC.Instance) is
  begin
    Put_Line(Standard_Error, "WFC Instance Info:");
    Put_Line(Standard_Error, "        Num_Tiles =>" & Inst.Num_Tiles'Image);
    Put_Line(Standard_Error, "   Instance_Bytes =>" & Integer'Image(Inst'Size / 8));
    Put_Line(Standard_Error, "  Adjacency_Bytes =>" & Integer'Image(Inst.Adjacencies'Size / 8));
    Put_Line(Standard_Error, "    Enabler_Bytes =>" & Integer'Image(Inst.Enablers'Size / 8));
    New_Line(Standard_Error);
  end;

  procedure Put_Im_Instance_Info is
    new Put_Instance_Info(Image_WFC);

  procedure Put_Ch_Instance_Info is
    new Put_Instance_Info(Character_WFC);

  Input_Image, Output_Image : IL_Image;

  Input_File   : File_Type;
  Output_File  : File_Type;

  Im_Instance : Im_Instance_Access;
  Ch_Instance : Ch_Instance_Access;

  type Input_Kind is (None, Pictoral, Textual);
  -- The type of the last received input sample.
  -- We want to be able to process both actual images
  -- as well as simpler, textual files, for ease of use.

  Last_Input_Kind : Input_Kind := None;
  -- Remember whether we've had an input yet, and if so,
  -- what type it was.

  N, M : aliased Integer := 2;
  -- The width and height of the tiles
  -- to be used in the instantiation.

  Rot, Ref : aliased Boolean := False;
  -- Whether to include rotations, and reflections,
  -- respectively, in the instantiation tileset.

  Use_Stdout : aliased Boolean := False;
  -- Whether to output text-only instance results
  -- on stdout rather than using a separate file.

  Output_Scale : aliased Integer := 1;
  -- When in image mode, how much to scale up the output image.

  Out_Name : XString;
  -- The name of the file we will produce as output (sans extension, size info, id)

  Out_Ct : Natural := 0;
  -- How many outputs we've handled so far.

  procedure Parse_Output_Command (Spec : String; W, H : out Natural) is
    use Ada.Strings.Maps;
    use Ada.Strings.Fixed;
    
    Separator_Set : constant Character_Set := To_Set("xX,/:");
    Separator_Ix  : constant Natural := Index(Spec, Separator_Set);
    Last : Natural;
  begin
    if Separator_Ix = 0 then
      raise Argument_Error with "Cannot parse argument: (" & Spec & ")";
    end if;
    declare
      Prefix : String renames Spec (Spec'First .. Separator_Ix - 1);
      Suffix : String renames Spec (Separator_Ix + 1 .. Spec'Last);      
    begin
      Get(Prefix, Item => W, Last => Last);
      if Last /= Prefix'Last then
        raise Argument_Error with "Cannot parse integer: (" & Prefix & ")";
      end if;
      Get(Suffix, Item => H, Last => Last);
      if Last /= Suffix'Last then
        raise Argument_Error with "Cannot parse integer: (" & Suffix & ")";
      end if;
    end;
  exception
    when Data_Error =>
      raise Argument_Error with "Cannot parse argument: (" & Spec & ")";
  end;

  function Construct_Output_Filename (W, H : Natural) return String is
    use Ada.Strings.Fixed;
    use Ada.Strings;

    W_Str  : constant String := Trim(W'Image, Both);
    H_Str  : constant String := Trim(H'Image, Both);
    Ct_Str : constant String := Trim(Out_Ct'Image, Both);

  begin
    Out_Ct := Out_Ct + 1;
    return W_Str & "x" & H_Str & "_" & Ct_Str & "_" & To_String(Out_Name);
  end;

  function Initialize_Image_Instance return Im_Instance_Access is

    Im_Width  : constant UInt := UInt(IL.Get_Integer(IL_IMAGE_WIDTH));
    Im_Height : constant UInt := UInt(IL.Get_Integer(IL_IMAGE_HEIGHT));

    Im_Data : Image_Matrix (1 .. Natural(Im_Width), 1 .. Natural(Im_Height));
    Im_Size : IL.UInt;

  begin

    IL.Bind_Image(Input_Image);
    Im_Size := 
      IL.Copy_Pixels(
        XOff  => 0,        YOff   => 0,         ZOff  => 0,
        Width => Im_Width, Height => Im_Height, Depth => 1,
        Format  => IL_RGB,
        Type_Of => IL_UNSIGNED_BYTE,
        Data    => Im_Data'Address
      );

    if Im_Size = 0 then
      raise Execution_Error with "Failed to load image data.";
    end if;

    return new Image_WFC.Instance'(
      Image_WFC.Initialize_From_Sample(
        N => N, M => M,
        Sample => Im_Data,
        Include_Rotations => Rot,
        Include_Reflections => Ref
      )
    );
  end;

  procedure Output_From_Image_Instance (W, H : Natural) is
    pragma Assert (Im_Instance /= null);

    Output_Filename : constant String := Construct_Output_Filename(W, H);

    Im_Data : Image_Matrix (1 .. W, 1 .. H);

    Num_Attempts : Natural := 0;
    Success : IL.Bool;

  begin

    if Use_Stdout then
      raise Argument_Error with "Cannot output image data to stdout.";
    end if;

    if Output_Scale < 1 then
      raise Argument_Error with "Output scale cannot be less than 1.";
    end if;

    loop
      exit when Image_WFC.Collapse_Within(Im_Instance.all, Im_Data);
      Num_Attempts := Num_Attempts + 1;
      if Num_Attempts >= 100 then
        raise Execution_Error with "Failed to collapse within 100 (!) attempts.";
      end if;
    end loop;

    IL.Bind_Image(Output_Image);
    Success := IL.Tex_Image(
      Width => UInt(W), Height => UInt(H), Depth => 1, Num_Channels => 3,
      Format  => IL_RGB,
      Type_Of => IL_UNSIGNED_BYTE,
      Data    => Im_Data'Address
    );
    if Success /= IL_TRUE then
      raise Execution_Error with "Failed to create output image texture.";
    end if;

    ILU.Image_Parameter(ILU_FILTER, ILU_NEAREST);
    Success := ILU.Scale(
      Width  => UInt(Output_Scale * W),
      Height => UInt(Output_Scale * H),
      Depth  => 1
    );
    if Success /= IL_True then
      raise Execution_Error with "Failed to scale image texture.";
    end if;

    Success := IL.Save_Image(Output_Filename);
    if Success /= IL_TRUE then
      raise Execution_Error with "Failed to save output image file.";
    end if;

  end;


  function Initialize_Character_Instance return Ch_Instance_Access is

    package String_Vectors is new
      Ada.Containers.Indefinite_Vectors(Positive, String);

    Mat_Width  : Natural := 0;
    File_Lines : String_Vectors.Vector;

    pragma Assert (Is_Open(Input_File));

  begin
    while not End_Of_File(Input_File) loop
      File_Lines.Append(Get_Line(Input_File));
      declare
        Last_Line : String renames File_Lines.Last_Element;         
      begin
        if Mat_Width = 0 then
          Mat_Width := Last_Line'Length;
        elsif Mat_Width /= Last_Line'Length then
          raise Argument_Error with "Input file rows are not even.";
        end if;
      end;
    end loop;

    declare
      Mat_Height : constant Natural := File_Lines.Last_Index;
      Char_Input : Character_Matrix(1 .. Mat_Width, 1 .. Mat_Height);
    begin
      for Y in 1 .. Mat_Height loop
        for X in 1 .. Mat_Width loop
          Char_Input(X, Y) := File_Lines(Y)(X);
        end loop;
      end loop;

      pragma Debug (Put_Line(Standard_Error, "Initialized Matrix from file (" & Name(Input_File) & ")"));
      pragma Debug (Put_Line(Standard_Error, "   Matrix_Width =>" & Mat_Width'Image));
      pragma Debug (Put_Line(Standard_Error, "  Matrix_Height =>" & Mat_Height'Image));
      pragma Debug (New_Line(Standard_Error, 1));

      Close(Input_File);

      return new Character_WFC.Instance'(
        Character_WFC.Initialize_From_Sample(
          N => N, M => M,
          Sample => Char_Input,
          Include_Rotations => Rot,
          Include_Reflections => Ref
        )
      );
    end;
  end;

  procedure Output_From_Character_Instance (W, H : Natural) is
    pragma Assert (Ch_Instance /= null);
  
    Out_Filename : constant String := Construct_Output_Filename(W, H);
    Char_Data : Character_Matrix (1 .. W, 1 .. H)
      := (others => (others => ' '));
    
    procedure Put_To_File(F : File_Type) is
    begin
      for Y in 1..H loop
        for X in 1..W loop
          Put(F, Char_Data(X, Y));
        end loop;
        New_Line(F);
      end loop;
    end;

    Num_Attempts : Natural := 0;

  begin
    
    loop
      exit when Character_WFC.Collapse_Within(Ch_Instance.all, Char_Data);
      Num_Attempts := Num_Attempts + 1;
      if Num_Attempts >= 100 then
        raise Execution_Error with "Failed to collapse within 100 (!) attempts.";
      end if;
    end loop;

    if Use_Stdout then
      Put_To_File(Standard_Output);
      New_Line(Standard_Output);
    else
      Create(Output_File, Out_File, Out_Filename);
      Put_To_File(Output_File);
      Close(Output_File);
    end if;

  end;

  procedure Set_Input_Source (Switch, Filename : String) is
    pragma Unreferenced (Switch);
  begin

    if Last_Input_Kind /= None then
      raise Argument_Error with "Multiple input sources specified.";
    end if;

    if Out_Name = Null_XString then
      Out_Name.Set(Filename);
    end if;

    pragma Debug (Put_Line(Standard_Error, "Input source: (" & Filename & ")"));

    IL.Bind_Image(Input_Image);
    if IL.Load_Image(Filename) = IL_TRUE then
      pragma Debug (Put_Line(Standard_Error, "  Type => Pictoral"));
      pragma Debug (New_Line(Standard_Error, 1));

      Last_Input_Kind := Pictoral;

    else
      pragma Debug (Put_Line(Standard_Error, "  Type => Textual (inferred)"));
      pragma Debug (New_Line(Standard_Error, 1));

      begin
        Open(Input_File, In_File, Filename);
      exception
        when Name_Error | Status_Error =>
          raise Argument_Error with "File cannot be opened: (" & Filename & ")";
      end;

      Last_Input_Kind := Textual;

    end if;

  end;

  procedure Set_Output_Name (Switch, Name : String) is
    pragma Unreferenced (Switch);
  begin
    pragma Debug (Put_Line(Standard_Error, "Output name set: (" & Name & ")"));
    pragma Debug (New_Line(Standard_Error, 1));
    Out_Name.Set(Name);
  end;

  procedure Define_CLI_Switches (Config : in out Command_Line_Configuration) is
  begin
    
    Define_Switch(Config, N'Access,
      Switch => "-n:",
      Initial => 2
    );
    
    Define_Switch(Config, M'Access,
      Switch => "-m:",
      Initial => 2
    );

    Define_Switch(Config, Rot'Access,
      Switch => "-r", Long_Switch => "--rot"
    );
    
    Define_Switch(Config, Ref'Access,
      Switch => "-f", Long_Switch => "--ref"
    );

    Define_Switch(Config, Use_Stdout'Access,
      Switch => "-s", Long_Switch => "--stdout"
    );

    Define_Switch(Config, Output_Scale'Access,
      Switch => "-x:", Long_Switch => "--scale=",
      Initial => 1
    );

    Define_Switch(Config, Callback => Set_Input_Source'Access,
      Switch => "-i:", Long_Switch => "--input="
    );

    Define_Switch(Config, Callback => Set_Output_Name'Access,
      Switch => "-o:", Long_Switch => "--output="
    );
  end;


  procedure Process_Command_Arguments is

    type Output_Handler_Type is
      access procedure (W, H : Natural);
    
    function Handle_Arg (Handler : not null Output_Handler_Type) return Boolean is
      Arg : constant String := Get_Argument;
      Out_W, Out_H : Natural;
    begin
      if Arg = "" then
        return False;
      end if;

      Parse_Output_Command(Arg, Out_W, Out_H);
      Handler(Out_W, Out_H);

      return True;
    end;

    Output_Handler : Output_Handler_Type;

  begin
    case Last_Input_Kind is
      when None =>
        return;
      
      when Pictoral =>
        Im_Instance := Initialize_Image_Instance;
        Output_Handler := Output_From_Image_Instance'Access;
        pragma Debug (Put_Im_Instance_Info(Im_Instance.all));
      
      when Textual =>
        Ch_Instance := Initialize_Character_Instance;
        Output_Handler := Output_From_Character_Instance'Access;
        pragma Debug (Put_Ch_Instance_Info(Ch_Instance.all));
    end case;

    loop
       exit when not Handle_Arg(Output_Handler);
    end loop;

  end;

begin

  IL.Init; 
  pragma Debug (Put_Line(Standard_Error, "Initialized libIL"));
  pragma Debug (Put_Line(Standard_Error, "  IL_Version =>" & IL.Get_Integer(IL.IL_VERSION_NUM)'Image));
  pragma Debug (New_Line(Standard_Error, 1));
  
  ILU.Init;
  pragma Debug (Put_Line(Standard_Error, "Initialized libILU"));
  pragma Debug (Put_Line(Standard_Error, "  ILU_Version =>" & ILU_VERSION'Image));
  pragma Debug (New_Line(Standard_Error, 1));
  
  declare
    Success : constant IL.Bool := IL.Enable(IL_FILE_OVERWRITE);
  begin
    if Success /= IL_TRUE then
      Put_Line(Standard_Error, "Warning: failed to initialize libIL file overwrite mode.");
    end if;
  end;

  Input_Image  := IL.Gen_Image;
  Output_Image := IL.Gen_Image;

end;