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
with Imago.IL;
use Imago;

package Imago.ILU is

  --------------------------------------------------------------------------

                            ---------------
                            -- T Y P E S --
                            ---------------

  --------------------------------------------------------------------------

  -- NOTE: Really, this one should be replaced with type whose definition
  -- is much more Ada-like.
  type Info is
  record
    ID          : IL.UInt;    -- the image's ID
    Data        : IL.Pointer; -- the image's data
    Width       : IL.UInt;    -- the image's width
    Height      : IL.UInt;    -- the image's height
    Depth       : IL.UInt;    -- the image's depth
    Bpp         : IL.UByte;   -- bytes per pixel (not bits) of the image
    Size_Of_Data: IL.UInt;    -- the total size of the data (in bytes)
    Format      : IL.Enum;    -- image format (in IL.Enum style)
    Of_Type     : IL.Enum;    -- image type (in IL.Enum style)
    Origin      : IL.Enum;    -- origin of the image
    Palette     : IL.Pointer; -- the image's palette
    Palette_Type: IL.Enum;    -- palette size
    Palette_Size: IL.UInt;    -- palette type
    Cube_Flags  : IL.Enum;    -- flags for what cube map sides are present
    Num_Next    : IL.UInt;    -- number of images following
    Num_Mips    : IL.UInt;    -- number of mipmaps
    Num_Layers  : IL.UInt;    -- number of layers
  end record
    with Convention => C;

  type Point_F is
  record
    X: Float;
    Y: Float;
  end record
    with Convention => C;

  type Point_I is
  record
    X: IL.Int;
    Y: IL.Int;
  end record
    with Convention => C;

  --------------------------------------------------------------------------

                        -----------------------
                        -- C O N S T A N T S --
                        -----------------------

  --------------------------------------------------------------------------

  ILU_VERSION_1_7_8                     : constant IL.Enum := 1;
  ILU_VERSION                           : constant IL.Enum := 178;

  ILU_FILTER                            : constant IL.Enum := 16#2600#;
  ILU_NEAREST                           : constant IL.Enum := 16#2601#;
  ILU_LINEAR                            : constant IL.Enum := 16#2602#;
  ILU_BILINEAR                          : constant IL.Enum := 16#2603#;
  ILU_SCALE_BOX                         : constant IL.Enum := 16#2604#;
  ILU_SCALE_TRIANGLE                    : constant IL.Enum := 16#2605#;
  ILU_SCALE_BELL                        : constant IL.Enum := 16#2606#;
  ILU_SCALE_BSPLINE                     : constant IL.Enum := 16#2607#;
  ILU_SCALE_LANCZOS3                    : constant IL.Enum := 16#2608#;
  ILU_SCALE_MITCHELL                    : constant IL.Enum := 16#2609#;

  -- Error types.
  ILU_INVALID_ENUM                      : constant IL.Enum := 16#0501#;
  ILU_OUT_OF_MEMORY                     : constant IL.Enum := 16#0502#;
  ILU_INTERNAL_ERROR                    : constant IL.Enum := 16#0504#;
  ILU_INVALID_VALUE                     : constant IL.Enum := 16#0505#;
  ILU_ILLEGAL_OPERATION                 : constant IL.Enum := 16#0506#;
  ILU_INVALID_PARAM                     : constant IL.Enum := 16#0509#;

  -- Values.
  ILU_PLACEMENT                         : constant IL.Enum := 16#0700#;
  ILU_LOWER_LEFT                        : constant IL.Enum := 16#0701#;
  ILU_LOWER_RIGHT                       : constant IL.Enum := 16#0702#;
  ILU_UPPER_LEFT                        : constant IL.Enum := 16#0703#;
  ILU_UPPER_RIGHT                       : constant IL.Enum := 16#0704#;
  ILU_CENTER                            : constant IL.Enum := 16#0705#;
  ILU_CONVOLUTION_MATRIX                : constant IL.Enum := 16#0710#;

  ILU_VERSION_NUM                       : constant IL.Enum := IL.IL_VERSION_NUM;
  ILU_VENDOR                            : constant IL.Enum := IL.IL_VENDOR;

  -- Languages.
  ILU_ENGLISH                           : constant IL.Enum := 16#0800#;
  ILU_ARABIC                            : constant IL.Enum := 16#0801#;
  ILU_DUTCH                             : constant IL.Enum := 16#0802#;
  ILU_JAPANESE                          : constant IL.Enum := 16#0803#;
  ILU_SPANISH                           : constant IL.Enum := 16#0804#;
  ILU_GERMAN                            : constant IL.Enum := 16#0805#;
  ILU_FRENCH                            : constant IL.Enum := 16#0806#;

  --------------------------------------------------------------------------

                        ---------------------------
                        -- S U B P R O G R A M S --
                        ---------------------------

  --------------------------------------------------------------------------

  function Alienify return IL.Bool
    with Import => True, Convention => StdCall, External_Name => "iluAlienify";

  function Blur_Avg (Iter: in IL.UInt) return IL.Bool
    with Import => True, Convention => StdCall, External_Name => "iluBlurAvg";

  function Blur_Gaussian (Iter: in IL.UInt) return IL.Bool
    with Import => True, Convention => StdCall,
         External_Name => "iluBlurGaussian";

  function Build_Mipmaps return IL.Bool
    with Import => True, Convention => StdCall,
         External_Name => "iluBuildMipmaps";

  function Colors_Used return IL.UInt
    with Import => True, Convention => StdCall,
         External_Name => "iluColoursUsed";

  function Colours_Used return IL.UInt
    with Import => True, Convention => StdCall,
         External_Name => "iluColoursUsed";

  function Compare_Image (Comp: in IL.UInt) return IL.Bool
    with Import => True, Convention => StdCall,
         External_Name => "iluCompareImage";

  function Contrast (Contrats: in Float) return IL.Bool
    with Import => True, Convention => StdCall, External_Name => "iluContrast";

  function Crop
    ( XOff: in IL.UInt; YOff: in IL.UInt; ZOff: in IL.UInt;
      Width: in IL.UInt; Height: in IL.UInt; Depth: in IL.UInt
    ) return IL.Bool
    with Import => True, Convention => StdCall, External_Name => "iluCrop";

  procedure Delete_Image (ID: in IL.UInt)
    with Import => True, Convention => StdCall,
         External_Name => "iluDeleteImage";

  function Edge_Detect_E return IL.Bool
    with Import => True, Convention => StdCall,
         External_Name => "iluEdgeDetectE";

  function Edge_Detect_P return IL.Bool
    with Import => True, Convention => StdCall,
         External_Name => "iluEdgeDetectP";

  function Edge_Detect_S return IL.Bool
    with Import => True, Convention => StdCall,
         External_Name => "iluEdgeDetectS";

  function Emboss return IL.Bool
    with Import => True, Convention => StdCall, External_Name => "iluEmboss";

  function Enlarge_Canvas
    ( Width: in IL.UInt; Height: in IL.UInt; Depth: in IL.UInt
    ) return IL.Bool
    with Import => True, Convention => StdCall,
         External_Name => "iluEnlargeCanvas";

  function Enlarge_Image
    ( XDim: in Float; YDim: in Float; ZDim: in Float
    ) return IL.Bool
    with Import => True, Convention => StdCall,
         External_Name => "iluEnlargeImage";

  function Error_String (String_Name: in IL.Enum) return String
    with Inline => True;

  function Equalize return IL.Bool
    with Import => True, Convention => StdCall, External_Name => "iluEqualize";

  function Convolution
    ( Matrix: in IL.Pointer; Scale: in IL.Int; Bias: in IL.Int
    ) return IL.Bool
    with Import => True, Convention => StdCall,
         External_Name => "iluConvolution";

  function Flip_Image return IL.Bool
    with Import => True, Convention => StdCall, External_Name => "iluFlipImage";

  function Gamma_Correct (Gamma: in Float) return IL.Bool
    with Import => True, Convention => StdCall,
         External_Name => "iluGammaCorrect";

  function Gen_Image return IL.UInt
    with Import => True, Convention => StdCall, External_Name => "iluGenImage";

  procedure Get_Image_Info (Item: out Info)
    with Import => True, Convention => StdCall,
         External_Name => "iluGetImageInfo";

  function  Get_Integer (Mode: in IL.Enum) return IL.Int
    with Import => True, Convention => StdCall,
         External_Name => "iluGetInteger";

  procedure Get_Integer (Mode: in IL.Enum; Param: in IL.Pointer)
    with Import => True, Convention => StdCall,
         External_Name => "iluGetIntegerv";

  function Get_String (String_Name: in IL.Enum) return String
    with Inline => True;

  procedure Image_Parameter (P_Name: in IL.Enum; Param: in IL.Enum)
    with Import => True, Convention => StdCall,
         External_Name => "iluImageParameter";

  procedure Init
    with Import => True, Convention => StdCall, External_Name => "iluInit";

  function Invert_Alpha return IL.Bool
    with Import => True, Convention => StdCall,
         External_Name => "iluInvertAlpha";

  function Load_Image (File_Name: in String) return IL.UInt
    with Inline => True;

  function Mirror return IL.Bool
    with Import => True, Convention => StdCall, External_Name => "iluMirror";

  function Negative return IL.Bool
    with Import => True, Convention => StdCall, External_Name => "iluNegative";

  function Noisify (Tolerance: in IL.ClampF) return IL.Bool
    with Import => True, Convention => StdCall, External_Name => "iluNoisify";

  function Pixelize (Pix_Size: in IL.UInt) return IL.Bool
    with Import => True, Convention => StdCall, External_Name => "iluPixelize";

  procedure Region_F (Points: in IL.Pointer; N: in IL.UInt)
    with Import => True, Convention => StdCall, External_Name => "iluRegionfv";

  procedure Region_I (Points: in IL.Pointer; N: in IL.UInt)
    with Import => True, Convention => StdCall, External_Name => "iluRegioniv";

  function Replace_Color
    ( Red: in IL.UByte; Green: in IL.UByte;
      Blue: in IL.UByte; Tolerance: in Float
    ) return IL.Bool
    with Import => True, Convention => StdCall,
         External_Name => "iluReplaceColour";

  function Replace_Colour
    ( Red: in IL.UByte; Green: in IL.UByte;
      Blue: in IL.UByte; Tolerance: in Float
    ) return IL.Bool
    with Import => True, Convention => StdCall,
         External_Name => "iluReplaceColour";

  function Rotate (Angle: in Float) return IL.Bool
    with Import => True, Convention => StdCall, External_Name => "iluRotate";

  function Rotate
    ( X: in Float; Y: in Float; Z: in Float; Angle: in Float
    ) return IL.Bool
    with Import => True, Convention => StdCall, External_Name => "iluRotate3D";

  function Saturate (Saturation: in Float) return IL.Bool
    with Import => True, Convention => StdCall,
         External_Name => "iluSaturate1f";

  function Saturate
    ( R: in Float; G: in Float; B: in Float; Saturation: in Float
    ) return IL.Bool
    with Import => True, Convention => StdCall,
         External_Name => "iluSaturate4f";

  function Scale
    ( Width: in IL.UInt; Height: in IL.UInt; Depth: in IL.UInt
    ) return IL.Bool
    with Import => True, Convention => StdCall, External_Name => "iluScale";

  function Scale_Alpha (Scale: in Float) return IL.Bool
    with Import => True, Convention => StdCall,
         External_Name => "iluScaleAlpha";

  function Scale_Colors (R: in Float; G: in Float; B: in Float) return IL.Bool
    with Import => True, Convention => StdCall,
         External_Name => "iluScaleColours";

  function Scale_Colours (R: in Float; G: in Float; B: in Float) return IL.Bool
    with Import => True, Convention => StdCall,
         External_Name => "iluScaleColours";

  function Set_Language (Language: in IL.Enum) return IL.Bool
    with Import => True, Convention => StdCall,
         External_Name => "iluSetLanguage";

  function Sharpen (Factor: in Float; Iter: in IL.UInt) return IL.Bool
    with Import => True, Convention => StdCall, External_Name => "iluSharpen";

  function Swap_Colors return IL.Bool
    with Import => True, Convention => StdCall,
         External_Name => "iluSwapColours";

  function Swap_Colours return IL.Bool
    with Import => True, Convention => StdCall,
         External_Name => "iluSwapColours";

  function Wave (Angle: in Float) return IL.Bool
    with Import => True, Convention => StdCall, External_Name => "iluWave";

  --------------------------------------------------------------------------

end Imago.ILU;
