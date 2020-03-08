
package body Wave_Function_Collapse is
    
  generic
    type Matrix_Element is private;
    type Matrix_Index is range <>;
    type Matrix is array (Matrix_Index range <>, Matrix_Index range <>) of Matrix_Element;
  function Modular_Index (The_Matrix : in Matrix; X, Y : Matrix_Index'Base) return Matrix_Element;

  function Modular_Index (The_Matrix : in Matrix; X, Y : Matrix_Index'Base) return Matrix_Element is
    Matrix_X : constant Matrix_Index := ((X - The_Matrix'First(1)) mod The_Matrix'Length(1)) + The_Matrix'First(1);
    Matrix_Y : constant Matrix_Index := ((Y - The_Matrix'First(2)) mod The_Matrix'Length(2)) + The_Matrix'First(2);
  begin
    return The_Matrix(Matrix_X, Matrix_Y);
  end;

  generic
    type Matrix_Element is private;
    type Matrix_Index is range <>;
    type Matrix is array (Matrix_Index range <>, Matrix_Index range <>) of Matrix_Element;
  function Invert_Horizontal (The_Matrix : in Matrix) return Matrix;

  function Invert_Horizontal (The_Matrix : in Matrix) return Matrix is
    Out_Matrix : Matrix (The_Matrix'Range(1), The_Matrix'Range(2));
    Out_X : Matrix_Index;
  begin
    for The_X in The_Matrix'Range(1) loop
      for The_Y in The_Matrix'Range(2) loop
        Out_X := The_Matrix'First(1) + The_Matrix'Last(1) - The_X;
        Out_Matrix(Out_X, The_Y) := The_Matrix(The_X, The_Y);
      end loop;
    end loop;

    return Out_Matrix;
  end;

  generic
    type Matrix_Element is private;
    type Matrix_Index is range <>;
    type Matrix is array (Matrix_Index range <>, Matrix_Index range <>) of Matrix_Element;
  function Invert_Vertical (The_Matrix : in Matrix) return Matrix;

  function Invert_Vertical (The_Matrix : in Matrix) return Matrix is
    Out_Matrix : Matrix (The_Matrix'Range(1), The_Matrix'Range(2));
    Out_Y : Matrix_Index;
  begin
    for The_X in The_Matrix'Range(1) loop
      for The_Y in The_Matrix'Range(2) loop
        Out_Y := The_Matrix'First(2) + The_Matrix'Last(2) - The_Y;
        Out_Matrix(The_X, Out_Y) := The_Matrix(The_X, The_Y);
      end loop;
    end loop;
    return Out_Matrix;
  end;

  generic
    type Matrix_Element is private;
    type Matrix_Index is range <>;
    type Matrix is array (Matrix_Index range <>, Matrix_Index range <>) of Matrix_Element;
  function Rotate_Clockwise (The_Matrix : in Matrix) return Matrix;

  function Rotate_Clockwise (The_Matrix : in Matrix) return Matrix is
    Out_Matrix : Matrix (The_Matrix'Range(2), The_Matrix'Range(1));
    Out_X : Matrix_Index;
  begin
    for The_X in The_Matrix'Range(1) loop
      for The_Y in The_Matrix'Range(2) loop
        Out_X := The_Matrix'First(2) + The_Matrix'Last(2) - The_Y;
        Out_Matrix(Out_X, The_X) := The_Matrix(The_X, The_Y);
      end loop;
    end loop;
    return Out_Matrix;
  end;

  function Opposite_Direction(Dir : Adjacency_Direction) return Adjacency_Direction is
    (case Dir is
      when Upwards    => Downwards,
      when Downwards  => Upwards,
      when Leftwards  => Rightwards,
      when Rightwards => Leftwards);

  type Tile_ID_Matrix is
    array (Natural range <>, Natural range <>) of Tile_ID;

  function Initialize_From_Sample
    ( Sample              : in Element_Matrix;
      Include_Rotations   : in Boolean  := False;
      Include_Reflections : in Boolean  := False;
      N, M                : in Positive := 2
    ) return Instance
  is separate;

  function Collapse_Within
    (Parameters : in Instance; World : out Element_Matrix) return Boolean
  is separate;

end;