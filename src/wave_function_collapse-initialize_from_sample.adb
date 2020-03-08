
with Ada.Text_IO;

with Ada.Strings.Fixed;

with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;

separate (Wave_Function_Collapse)
function Initialize_From_Sample
  ( Sample              : in Element_Matrix;
    Include_Rotations   : in Boolean := False;
    Include_Reflections : in Boolean := False;
    N, M                : in Positive := 2
  ) return Instance
is

  type Tile is
    array (1 .. N, 1 .. M) of Element;
  -- The type of a particular tile within the
  -- image, an N x M rectangle of elements.

  function "<" (T1, T2 : in Tile) return Boolean is
  begin
      for X in Tile'Range(1) loop
          for Y in Tile'Range(2) loop
              if T1(X, Y) /= T2(X, Y) then
                  return T1(X, Y) < T2(X, Y);
              end if;
          end loop;
      end loop;
      return False;
  end;

  function Tile_At (Sample : in Element_Matrix; X, Y : Natural) return Tile is
      function Sample_Modular_Index is new Modular_Index(Element, Natural, Element_Matrix);
      The_Tile : Tile;
  begin
      for Tile_X in Tile'Range(1) loop
          for Tile_Y in Tile'Range(2) loop
              The_Tile(Tile_X, Tile_Y) := Sample_Modular_Index(Sample, X + Tile_X - 1, Y + Tile_Y - 1);
          end loop;
      end loop;

      return The_Tile;
  end;

  type Tile_Array is
    array (Tile_ID range <>) of Tile;

  package Tile_Maps is new Ada.Containers.Ordered_Maps(Tile, Tile_ID);
  package Freq_Vecs is new Ada.Containers.Vectors(Tile_ID, Natural);

  Tile_Set : Tile_Maps.Map;
  Frequencies_Vec : Freq_Vecs.Vector;
  Frequency_Total : Natural := 0;

  procedure Lookup_Tile (The_Tile : Tile; The_Tile_ID : out Tile_ID) is
    Inserted : Boolean;
    Position : Tile_Maps.Cursor;
  begin
    Tile_Set.Insert(The_Tile, Position, Inserted);
    if Inserted then
      Frequencies_Vec.Append(0);
      Tile_Set(Position) := Frequencies_Vec.Last_Index;
    end if;
    The_Tile_ID := Tile_Set(Position);
  end;

  procedure Count_Tile (The_Tile_ID : Tile_ID) is
    Frequency : Natural renames Frequencies_Vec(The_Tile_ID);
  begin
    Frequency := Frequency + 1;
    Frequency_Total := Frequency_Total + 1;
  end;

  procedure Count_Tiles (All_Tiles : in Tile_ID_Matrix) is
  begin
    for The_Tile of All_Tiles loop
      Count_Tile(The_Tile);
    end loop;
  end;

  function Populate_Tiles_From_Sample (Sample : in Element_Matrix) return Tile_ID_Matrix is
    Sample_Tiles : Tile_ID_Matrix (Sample'Range(1), Sample'Range(2));
  begin
    for Sample_X in Sample'Range(1) loop
      for Sample_Y in Sample'Range(2) loop
        Lookup_Tile(
          The_Tile    => Tile_At(Sample, Sample_X, Sample_Y),
          The_Tile_ID => Sample_Tiles(Sample_X, Sample_Y)
        );
      end loop;
    end loop;
    return Sample_Tiles;
  end;

  function Invert_Sample_Horizontal is
    new Invert_Horizontal(Element, Natural, Element_Matrix);

  function Invert_Sample_Vertical is
    new Invert_Vertical(Element, Natural, Element_Matrix);

  function Rotate_Sample_Clockwise is
    new Rotate_Clockwise(Element, Natural, Element_Matrix);

  Sample_Normal       : Element_Matrix renames Sample;
  Sample_Reflect_H    : constant Element_Matrix := Invert_Sample_Horizontal(Sample_Normal);
  Sample_Reflect_V    : constant Element_Matrix := Invert_Sample_Vertical(Sample_Normal);
  Sample_Reflect_HV   : constant Element_Matrix := Invert_Sample_Vertical(Sample_Reflect_H);

  Sample_Rotate_90    : constant Element_Matrix := Rotate_Sample_Clockwise(Sample_Normal);
  Sample_Rotate_90_H  : constant Element_Matrix := Invert_Sample_Horizontal(Sample_Rotate_90);
  Sample_Rotate_90_V  : constant Element_Matrix := Invert_Sample_Vertical(Sample_Rotate_90);
  Sample_Rotate_90_HV : constant Element_Matrix := Invert_Sample_Vertical(Sample_Rotate_90_H);
  
  function Process_Tile_Counts return Natural is
    procedure Process_Sample (Sample : in Element_Matrix) is
      Sample_Tiles : constant Tile_ID_Matrix := Populate_Tiles_From_Sample(Sample);
    begin
      Count_Tiles(Sample_Tiles);
    end;
  begin
    Process_Sample(Sample_Normal);
    if Include_Reflections and Include_Rotations then
      Process_Sample(Sample_Reflect_H);
      Process_Sample(Sample_Reflect_V);
      Process_Sample(Sample_Reflect_HV);
      Process_Sample(Sample_Rotate_90);
      Process_Sample(Sample_Rotate_90_H);
      Process_Sample(Sample_Rotate_90_V);
      Process_Sample(Sample_Rotate_90_HV);
    elsif Include_Reflections and not Include_Rotations then
      Process_Sample(Sample_Reflect_H);
      Process_Sample(Sample_Reflect_V);
      Process_Sample(Sample_Reflect_HV);
    elsif not Include_Reflections and Include_Rotations then
      Process_Sample(Sample_Reflect_HV);
      Process_Sample(Sample_Rotate_90);
      Process_Sample(Sample_Rotate_90_HV);
    end if;

    return Frequencies_Vec.Last_Index;
  end;

  function Frequencies_To_Array (Num_Tiles : Natural) return Frequency_Array is
    Frequencies : Frequency_Array(1 .. Num_Tiles);
  begin
    for Index in Frequencies'Range loop
      Frequencies(Index) := Frequencies_Vec(Index);
    end loop;
    return Frequencies;
  end;

  function Tiles_To_Array (Num_Tiles : Natural) return Tile_Array is
    Tiles : Tile_Array(1 .. Num_Tiles);
  begin
    for C in Tile_Set.Iterate loop
      Tiles(Tile_Maps.Element(C)) := Tile_Maps.Key(C);
    end loop;
    return Tiles;
  end;

  function Tiles_To_Elements (Tile_Set : in Tile_Array) return Tile_Element_Array is
    Tile_Elements : Tile_Element_Array(Tile_Set'Range);
  begin
    for Tile_ID in Tile_Set'Range loop
      Tile_Elements(Tile_ID) := Tile_Set(Tile_ID)(1, 1);
    end loop;
    return Tile_Elements;
  end;

  function Process_Adjacencies (Tile_Set : in Tile_Array) return Adjacency_Matrix is
    
    Adjacencies : Adjacency_Matrix(Tile_Set'Range, Tile_Set'Range)
      := (others => (others => (others => False)));

    function Adjacent_Rightwards (T_1, T_2 : Tile_ID) return Boolean is
      Tile_1 : Tile renames Tile_Set(T_1);
      Tile_2 : Tile renames Tile_Set(T_2);
    begin
      for X in 1 .. N - 1 loop
        for Y in 1 .. M loop
          if Tile_1(X + 1, Y) /= Tile_2(X, Y) then
            return False;
          end if;
        end loop;
      end loop;
      return True;
    end;

    function Adjacent_Downwards (T_1, T_2 : Tile_ID) return Boolean is
      Tile_1 : Tile renames Tile_Set(T_1);
      Tile_2 : Tile renames Tile_Set(T_2);
    begin
      for X in 1 .. N loop
        for Y in 1 .. M - 1 loop
          if Tile_1(X, Y + 1) /= Tile_2(X, Y) then
            return False;
          end if;
        end loop;
      end loop;
      return True;
    end;

  begin
    for T_1 in Adjacencies'Range(1) loop
      for T_2 in Adjacencies'Range(2) loop
        if Adjacent_Downwards(T_1, T_2) then
          Adjacencies(T_2, T_1)(Upwards)   := True;
          Adjacencies(T_1, T_2)(Downwards) := True;
        end if;

        if Adjacent_Rightwards(T_1, T_2) then
          Adjacencies(T_2, T_1)(Leftwards)  := True;
          Adjacencies(T_1, T_2)(Rightwards) := True;
        end if;
      end loop;
    end loop;

    return Adjacencies;
  end;

  function Count_Enablers (Adjacencies : in Adjacency_Matrix) return Enabler_Counts is
    procedure Inc (N : in out Small_Integer) is
    begin
      N := N + 1;
    end;

    Enablers : Enabler_Counts(Adjacencies'Range(1))
      := (others => (others => 0));
  begin
    for Tile_ID_1 in Enablers'Range loop
      for Tile_ID_2 in Enablers'Range loop
        for Dir in Adjacency_Direction loop
          if Adjacencies(Tile_ID_1, Tile_ID_2)(Dir) then
            Inc( Enablers(Tile_ID_1)(Dir) );
          end if;
        end loop;
      end loop;
    end loop;

    return Enablers;
  end;

begin
  declare
    Num_Tiles     : constant Natural := Process_Tile_Counts;
    
    Frequencies   : constant Frequency_Array    := Frequencies_To_Array(Num_Tiles);

    Tiles         : constant Tile_Array := Tiles_To_Array(Num_Tiles);
    Tile_Elements : constant Tile_Element_Array := Tiles_To_Elements(Tiles);
    Adjacencies   : constant Adjacency_Matrix := Process_Adjacencies(Tiles);

    Enablers      : constant Enabler_Counts   := Count_Enablers(Adjacencies);
  begin
    return Instance'(
      Num_Tiles       => Num_Tiles,
      Tile_Elements   => Tile_Elements,
      Frequencies     => Frequencies,
      Frequency_Total => Frequency_Total,
      Adjacencies     => Adjacencies,
      Enablers        => Enablers
    );
  end;
end;