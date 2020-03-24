
package body WFC is

  function Modular_Index (Matrix : in Element_Matrix; X, Y : Natural) return Element_Type
    with Inline
  is
    Matrix_X : constant Natural := ((X - Matrix'First(1)) mod Matrix'Length(1)) + Matrix'First(1);
    Matrix_Y : constant Natural := ((Y - Matrix'First(2)) mod Matrix'Length(2)) + Matrix'First(2);
  begin
    return Matrix(Matrix_X, Matrix_Y);
  end;

  function Invert_Horizontal (Matrix : in Element_Matrix) return Element_Matrix
    with Inline
  is
    Out_Matrix : Element_Matrix (Matrix'Range(1), Matrix'Range(2));
    Out_X : Natural;
  begin
    for The_X in Matrix'Range(1) loop
      for The_Y in Matrix'Range(2) loop
        Out_X := Matrix'First(1) + Matrix'Last(1) - The_X;
        Out_Matrix(Out_X, The_Y) := Matrix(The_X, The_Y);
      end loop;
    end loop;

    return Out_Matrix;
  end;

  function Invert_Vertical (Matrix : in Element_Matrix) return Element_Matrix
    with Inline
  is
    Out_Matrix : Element_Matrix (Matrix'Range(1), Matrix'Range(2));
    Out_Y : Natural;
  begin
    for The_X in Matrix'Range(1) loop
      for The_Y in Matrix'Range(2) loop
        Out_Y := Matrix'First(2) + Matrix'Last(2) - The_Y;
        Out_Matrix(The_X, Out_Y) := Matrix(The_X, The_Y);
      end loop;
    end loop;
    return Out_Matrix;
  end;

  function Rotate_Clockwise (Matrix : in Element_Matrix) return Element_Matrix
    with Inline
  is
    Out_Matrix : Element_Matrix (Matrix'Range(2), Matrix'Range(1));
    Out_X : Natural;
  begin
    for The_X in Matrix'Range(1) loop
      for The_Y in Matrix'Range(2) loop
        Out_X := Matrix'First(2) + Matrix'Last(2) - The_Y;
        Out_Matrix(Out_X, The_X) := Matrix(The_X, The_Y);
      end loop;
    end loop;
    return Out_Matrix;
  end;

  function Opposite_Direction(Dir : Adjacency_Direction) return Adjacency_Direction is
    (case Dir is
      when Upwards    => Downwards,
      when Downwards  => Upwards,
      when Leftwards  => Rightwards,
      when Rightwards => Leftwards)
  with Inline;

  function Initialize_From_Sample
    ( Sample              : in Element_Matrix;
      Include_Rotations   : in Boolean  := False;
      Include_Reflections : in Boolean  := False;
      N, M                : in Positive := 2
    ) return Instance
  is separate;

  package body Extended_Interfaces is separate;

  function Collapse_Within
    (Parameters : in Instance; World : out Element_Matrix) return Boolean
  is
    subtype X_Dim is Natural range World'First(1) .. World'Last(1);
    subtype Y_Dim is Natural range World'First(2) .. World'Last(2);

    package Extended is new Extended_Interfaces(X_Dim, Y_Dim);

    subtype Collapse_Info is Extended.Collapse_Info'Class;

    procedure Set_Resulting_Element (X : X_Dim; Y : Y_Dim; Element : Element_Type)
      with Inline is
    begin
      World(X, Y) := Element;
    end;

    procedure Upon_Collapse (X : X_Dim; Y : Y_Dim; Info : in Collapse_Info'Class) is null;

    function Collapse is new
      Extended.Generic_Collapse_Within
          ( Set_Resulting_Element => Set_Resulting_Element
          , Upon_Collapse         => Upon_Collapse
          );

  begin
    return Collapse(Parameters);
  end;

end;