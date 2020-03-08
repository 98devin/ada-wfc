------------------------------------------------------------------------------
-- EMAIL: <darkestkhan@gmail.com>                                           --
-- License: ISC                                                             --
--                                                                          --
--                    Copyright © 2015 - 2016 darkestkhan                   --
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
with Ada.Characters.Latin_1;

with Interfaces.C;
with Interfaces.C.Strings;

package body Imago.IL is

  --------------------------------------------------------------------------

                            -------------------
                            -- R E N A M E S --
                            -------------------

  --------------------------------------------------------------------------

  package ASCII renames Ada.Characters.Latin_1;
  package IC renames Interfaces.C;
  package CStrings renames Interfaces.C.Strings;

  --------------------------------------------------------------------------

                  -------------------------------------------
                  -- S U B P R O G R A M S '   B O D I E S --
                  -------------------------------------------

  --------------------------------------------------------------------------

  function Apply_Pal (File_Name: in String) return Bool
  is
    function ilApplyPal (FileName: in Pointer) return Bool
      with Import => True, Convention => StdCall, External_Name => "ilApplyPal";

    CString: constant String := File_Name & ASCII.NUL;
  begin
    return ilApplyPal (CString'Address);
  end Apply_Pal;

  --------------------------------------------------------------------------

  function Apply_Profile
    ( In_Profile: in String; Out_Profile: in String
    ) return Bool
  is
    function ilApplyProfile
      ( InProfile : in Pointer;
        OutProfile: in Pointer
      ) return Bool
      with Import => True, Convention => StdCall,
           External_Name => "ilApplyProfile";

    C_In_Profile : constant String := In_Profile & ASCII.NUL;
    C_Out_Profile: constant String := Out_Profile & ASCII.NUL;
  begin
    return ilApplyProfile (C_In_Profile'Address, C_Out_Profile'Address);
  end Apply_Profile;

  --------------------------------------------------------------------------

  function Determine_Type (File_Name: in String) return Enum
  is
    function ilDetermineType (FileName: in Pointer) return Enum
      with Import => True, Convention => StdCall,
           External_Name => "ilDetermineType";

    CString: constant String := File_Name & ASCII.NUL;
  begin
    return ilDetermineType (CString'Address);
  end Determine_Type;

  --------------------------------------------------------------------------

  function Get_String (String_Name: in Enum) return String
  is
    function ilGetString (String_Name: in Enum) return CStrings.chars_ptr
      with Import => True, Convention => StdCall,
           External_Name => "ilGetString";
  begin
    return IC.To_Ada (CStrings.Value (ilGetString (String_Name)));
  end Get_String;

  --------------------------------------------------------------------------

  function Is_Valid (Type_Of: in Enum; File_Name: in String) return Bool
  is
    function ilIsValid (T: in Enum; F: in Pointer) return Bool
      with Import => True, Convention => StdCall, External_Name => "ilIsValid";

    CString: constant String := File_Name & ASCII.NUL;
  begin
    return ilIsValid (Type_Of, CString'Address);
  end Is_Valid;

  --------------------------------------------------------------------------

  function Load (Type_Of: in Enum; File_Name: in String) return Bool
  is
    function ilLoad (T: in Enum; F: in Pointer) return Bool
      with Import => True, Convention => StdCall, External_Name => "ilLoad";

    CString: constant String := File_Name & ASCII.NUL;
  begin
    return ilLoad (Type_Of, CString'Address);
  end Load;

  --------------------------------------------------------------------------

  function Load_Data
    ( File_Name: in String;
      Width: in UInt; Height: in UInt;
      Depth: in UInt; BPP: in UByte
    ) return Bool
  is
    function ilLoadData
      ( FileName: in Pointer;
        Width: in UInt; Height: in UInt;
        Depth: in UInt; BPP: in UByte
      ) return Bool
    with Import => True, Convention => StdCall, External_Name => "ilLoadData";

    CString: constant String := File_Name & ASCII.NUL;
  begin
    return ilLoadData (CString'Address, Width, Height, Depth, BPP);
  end Load_Data;

  --------------------------------------------------------------------------

  function Load_Image (File_Name: in String) return Bool
  is
    function ilLoadImage (FileName: in Pointer) return Bool
      with Import => True, Convention => StdCall,
           External_Name => "ilLoadImage";

    CString: constant String := File_Name & ASCII.NUL;
  begin
    return ilLoadImage (CString'Address);
  end Load_Image;

  --------------------------------------------------------------------------

  function Load_Pal (File_Name: in String) return Bool
  is
    function ilLoadPal (FileName: in Pointer) return Bool
      with Import => True, Convention => StdCall,
           External_Name => "ilLoadImage";

    CString: constant String := File_Name & ASCII.NUL;
  begin
    return ilLoadPal (CString'Address);
  end Load_Pal;

  --------------------------------------------------------------------------

  function Remove_Load (Ext: in String) return Bool
  is
    function ilRemoveLoad (Ext: in Pointer) return Bool
      with Import => True, Convention => StdCall,
           External_Name => "ilRemoveLoad";

    CString: constant String := Ext & ASCII.NUL;
  begin
    return ilRemoveLoad (CString'Address);
  end Remove_Load;

  --------------------------------------------------------------------------

  function Remove_Save (Ext: in String) return Bool
  is
    function ilRemoveSave (Ext: in Pointer) return Bool
      with Import => True, Convention => StdCall,
           External_Name => "ilRemoveSave";

    CString: constant String := Ext & ASCII.NUL;
  begin
    return ilRemoveSave (CString'Address);
  end Remove_Save;

  --------------------------------------------------------------------------

  function Save (Type_Of: in Enum; File_Name: in String) return Bool
  is
    function ilSave
      ( Type_Of: in Enum; FileName: in Pointer
      ) return Bool
      with Import => True, Convention => StdCall, External_Name => "ilSave";

    CString: constant String := File_Name & ASCII.NUL;
  begin
    return ilSave (Type_Of, CString'Address);
  end Save;

  --------------------------------------------------------------------------

  function Save_Data (File_Name: in String) return Bool
  is
    function ilSaveData (FileName: in Pointer) return Bool
      with Import => True, Convention => StdCall, External_Name => "ilSaveData";

    CString: constant String := File_Name & ASCII.NUL;
  begin
    return ilSaveData (CString'Address);
  end Save_Data;

  --------------------------------------------------------------------------

  function Save_Image (File_Name: in String) return Bool
  is
    function ilSaveImage (FileName: in Pointer) return Bool
      with Import => True, Convention => StdCall,
           External_Name => "ilSaveImage";

    CString: constant String := File_Name & ASCII.NUL;
  begin
    return ilSaveImage (CString'Address);
  end Save_Image;

  --------------------------------------------------------------------------

  function Save_Pal (File_Name: in String) return Bool
  is
    function ilSavePal (FileName: in Pointer) return Bool
      with Import => True, Convention => StdCall, External_Name => "ilSavePal";

    CString: constant String := File_Name & ASCII.NUL;
  begin
    return ilSavePal (CString'Address);
  end Save_Pal;

  --------------------------------------------------------------------------

  procedure Set_String (Mode: in Enum; Value: in String)
  is
    procedure ilSetString (Mode : in Enum; Value: in Pointer)
      with Import => True, Convention => StdCall,
           External_Name => "ilSetString";

    CString: constant String := Value & ASCII.NUL;
  begin
    ilSetString (Mode, CString'Address);
  end Set_String;

  ---------------------------------------------------------------------------

  function Type_From_Ext (File_Name: in String) return Enum
  is
    function ilTypeFromExt (FileName: in Pointer) return Enum
      with Import => True, Convention => StdCall,
           External_Name => "ilTypeFromExt";

    CString: constant String := File_Name & ASCII.NUL;
  begin
    return ilTypeFromExt (CString'Address);
  end Type_From_Ext;

  --------------------------------------------------------------------------

end Imago.IL;
