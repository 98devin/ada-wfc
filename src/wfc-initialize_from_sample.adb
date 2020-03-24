
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;

separate (WFC)
function Initialize_From_Sample
  ( Sample              : in Element_Matrix;
    Include_Rotations   : in Boolean := False;
    Include_Reflections : in Boolean := False;
    N, M                : in Positive := 2
  ) return Instance
is

  type Tile is
    array (1 .. N, 1 .. M) of Element_Type;
  -- The type of a particular tile within the
  -- image, an N x M rectangle of elements.

  type Tile_ID_Matrix is
    array (Natural range <>, Natural range <>) of Tile_ID;

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
  -- The less-than relation is not predefined for
  -- multidimensional arrays in Ada, apparently.

  function Tile_At (Sample : in Element_Matrix; X, Y : Natural) return Tile is
      The_Tile : Tile;
  begin
      for Tile_X in Tile'Range(1) loop
          for Tile_Y in Tile'Range(2) loop
              The_Tile(Tile_X, Tile_Y) := Modular_Index(Sample, X + Tile_X - 1, Y + Tile_Y - 1);
          end loop;
      end loop;

      return The_Tile;
  end;
  -- For each position in the sample, we want to build up the Tile
  -- which has its top-left at this position. While doing so,
  -- we maintain a modular indexing scheme such that the sample is taken to
  -- loop back to itself in both directions. This is to ensure that we
  -- can find a full tile's worth of information at each position,
  -- regardless of the input matrix size.

  package Tile_Maps is new Ada.Containers.Ordered_Maps(Tile, Tile_ID); use Tile_Maps;
  package Freq_Vecs is new Ada.Containers.Vectors(Tile_ID, Natural);   use Freq_Vecs;

  Tile_Set        : Tile_Maps.Map;
  Frequencies_Vec : Freq_Vecs.Vector;
  -- To track the set of tiles we've encountered, and their counts,
  -- we use a combination of a mapping from tile matrices to ids for
  -- uniqueness tests, and a mapping from tile ids to counts (in the form of a vector)
  -- for their frequency.

  Frequency_Total : Natural := 0;
  -- We'll track this total just because it's easy to do that now.

  function Lookup_Tile (The_Tile : Tile) return Tile_ID is
    Inserted : Boolean;
    Position : Tile_Maps.Cursor;
  begin
    Tile_Set.Insert(The_Tile, Position, Inserted);
    if Inserted then
      Frequencies_Vec.Append(0);                        -- insert new empty count (we'll increment it later)
      Tile_Set(Position) := Frequencies_Vec.Last_Index; -- the new last index is this tile's unique ID
    end if;
    return Tile_Set(Position);
  end;
  -- Check whether we've seen the tile before, and if not,
  -- initialize its tile ID and frequency. Return the unique tile id
  -- associated with this tile, new or not.  

  procedure Count_Tile (The_Tile_ID : Tile_ID) is
    Frequency : Natural renames Frequencies_Vec(The_Tile_ID);
  begin
    Frequency := Frequency + 1;
    Frequency_Total := Frequency_Total + 1;
  end;
  -- Simply increment the specific tile's frequency,
  -- while also tracking the total across all tiles.

  procedure Count_Tiles (All_Tiles : in Tile_ID_Matrix) is
  begin
    for The_Tile of All_Tiles loop
      Count_Tile(The_Tile);
    end loop;
  end;
  -- Count all the tiles within the provided matrix.

  function Populate_Tiles_From_Sample (Sample : in Element_Matrix) return Tile_ID_Matrix is
    Sample_Tiles : Tile_ID_Matrix (Sample'Range(1), Sample'Range(2));
  begin
    for Sample_X in Sample'Range(1) loop
      for Sample_Y in Sample'Range(2) loop
        Sample_Tiles(Sample_X, Sample_Y) := Lookup_Tile(Tile_At(Sample, Sample_X, Sample_Y));
      end loop;
    end loop;
    return Sample_Tiles;
  end;
  -- Given a sample matrix, lookup each NxM tile within it
  -- to produce a matrix of the same size, but holding the ID of the tile
  -- found at each position.

  Sample_Normal       : Element_Matrix renames Sample;
  Sample_Reflect_H    : constant Element_Matrix := Invert_Horizontal(Sample_Normal);
  Sample_Reflect_V    : constant Element_Matrix := Invert_Vertical(Sample_Normal);
  Sample_Reflect_HV   : constant Element_Matrix := Invert_Vertical(Sample_Reflect_H);

  Sample_Rotate_90    : constant Element_Matrix := Rotate_Clockwise(Sample_Normal);
  Sample_Rotate_90_H  : constant Element_Matrix := Invert_Horizontal(Sample_Rotate_90);
  Sample_Rotate_90_V  : constant Element_Matrix := Invert_Vertical(Sample_Rotate_90);
  Sample_Rotate_90_HV : constant Element_Matrix := Invert_Vertical(Sample_Rotate_90_H);
  
  -- We compute each possible re-orientation we may need.
  -- Fortunately there are only 8 such resulting changes by combining
  -- vertical and horizontal reflections and rotations (some combinations overlap).
  -- We don't want to do more processing than necessary, so they are enumerated upfront.

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
      Process_Sample(Sample_Reflect_HV);   -- 180 degrees
      Process_Sample(Sample_Rotate_90);    --  90 degrees
      Process_Sample(Sample_Rotate_90_HV); -- 270 degrees
    end if;

    return Frequencies_Vec.Last_Index;
  end;
  -- Count all occurrences of every tile in each desired orientation,
  -- and return the total number of tiles found during this process
  -- (since counting will also involve tile ID creation).

  function Frequencies_To_Array (Num_Tiles : Natural) return Frequency_Array is
    Frequencies : Frequency_Array(1 .. Num_Tiles);
  begin
    for Index in Frequencies'Range loop
      Frequencies(Index) := Frequencies_Vec(Index);
    end loop;
    return Frequencies;
  end;
  -- Collapse the dynamically-sized vector into an array
  -- with a known length, so we guarantee this length in our result.

  function Tile_Set_To_Elements (Num_Tiles : Natural) return Tile_Element_Array is
    Tile_Elements : Tile_Element_Array(1 .. Num_Tiles);
  begin
    for C in Tile_Set.Iterate loop
      Tile_Elements(Element(C)) := Key(C)(1, 1);
    end loop;
    return Tile_Elements;
  end;
  -- Collapse the tile set into an array of just the top-left-most element
  -- of each tile we know about.

  function Tile_Set_To_Adjacencies (Num_Tiles : Natural) return Adjacency_Matrix is
    
    Adjacencies : Adjacency_Matrix(1 .. Num_Tiles, 1 .. Num_Tiles)
      := (others => (others => (others => False)));

    function Adjacent_Rightwards (Tile_1, Tile_2 : Tile) return Boolean is
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

    function Adjacent_Downwards (Tile_1, Tile_2 : Tile) return Boolean is
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

    -- Both the above functions merely compare that, if the tiles were positioned
    -- adjacently, then all of their elements would be equal except for those
    -- which do not overlap in such a positioning. This is basically just comparing
    -- that submatrices ignoring the first/last column/row respectively are equal.

  begin
    for C_1 in Tile_Set.Iterate loop
      for C_2 in Tile_Set.Iterate loop
        declare
          T_ID_1 : constant Tile_ID := Element(C_1);
          T_1    : constant Tile    := Key(C_1);

          T_ID_2 : constant Tile_ID := Element(C_2);
          T_2    : constant Tile    := Key(C_2);
        begin
          if Adjacent_Downwards(T_1, T_2) then
            Adjacencies(T_ID_2, T_ID_1)(Upwards)   := True;
            Adjacencies(T_ID_1, T_ID_2)(Downwards) := True;
          end if;

          if Adjacent_Rightwards(T_1, T_2) then
            Adjacencies(T_ID_2, T_ID_1)(Leftwards)  := True;
            Adjacencies(T_ID_1, T_ID_2)(Rightwards) := True;
          end if;
        end;
      end loop;
    end loop;

    return Adjacencies;
  end;
  -- Compute adjacency information from the set of tiles and their contents.

  function Count_Enablers (Adjacencies : in Adjacency_Matrix) return Enabler_Counts is
    procedure Inc (N : in out Small_Integer) with Inline is
    begin
      N := N + 1;
    end;
    -- Convenience definition just so we don't have
    -- to repeat the name of the array location in order to increment it.

    Enablers : Enabler_Counts(Adjacencies'Range(1))
      := (others => (others => 0));

  begin
    for Tile_ID_1 in Enablers'Range loop
      for Tile_ID_2 in Enablers'Range loop
        for Dir in Adjacency_Direction loop
          if Adjacencies(Tile_ID_1, Tile_ID_2)(Dir) then

            Inc( Enablers(Tile_ID_1)(Dir) );
            -- Notice that we don't increment the opposite case
            -- in the same loop iteration, e.g. Enablers(Tile_ID_2)(Opposite(Dir))
            -- because we don't want to double count these.
            --
            -- If we were to count one or more enablers twice,
            -- then the algorithm itself would be unable to properly
            -- restrict the placements of tiles and the output
            -- would not resemble the input.

          end if;
        end loop;
      end loop;
    end loop;

    return Enablers;
  end;

begin
  declare
    Num_Tiles     : constant Natural            := Process_Tile_Counts;
    
    Frequencies   : constant Frequency_Array    := Frequencies_To_Array(Num_Tiles);

    Tile_Elements : constant Tile_Element_Array := Tile_Set_To_Elements(Num_Tiles);
    Adjacencies   : constant Adjacency_Matrix   := Tile_Set_To_Adjacencies(Num_Tiles);

    Enablers      : constant Enabler_Counts     := Count_Enablers(Adjacencies);
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