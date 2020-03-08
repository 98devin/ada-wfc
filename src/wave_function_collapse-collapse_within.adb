
with Ada.Numerics.Float_Random;
with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Generic_Elementary_Functions;

with Ada.Containers.Vectors;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Priority_Queues;

use type Ada.Containers.Count_Type;

with Ada.Text_IO;
use  Ada.Text_IO;

with Ada.Strings.Fixed;
use  Ada.Strings.Fixed;

separate (Wave_Function_Collapse)
function Collapse_Within
  (Parameters : in Instance; World : out Element_Matrix) return Boolean
is

  Num_Tiles       : Natural            renames Parameters.Num_Tiles;
  Tile_Elements   : Tile_Element_Array renames Parameters.Tile_Elements;
  Frequency_Total : Natural            renames Parameters.Frequency_Total;
  Frequencies     : Frequency_Array    renames Parameters.Frequencies;
  Adjacencies     : Adjacency_Matrix   renames Parameters.Adjacencies;
  Init_Enablers   : Enabler_Counts     renames Parameters.Enablers;

  Unsatisfiable_Tile_Constraint : exception;

  subtype Tile_ID_Range is
    Tile_ID range 1 .. Num_Tiles;

  type Wave_Function is
    array (Tile_ID_Range) of Boolean
    with Pack;

  package Float_Functions is
    new Ada.Numerics.Generic_Elementary_Functions(Float);

  function Frequency_Log_Frequency (Frequency : Natural) return Float is
    Freq_Float : constant Float := Float(Frequency);
  begin
    return Freq_Float * Float_Functions.Log(Freq_Float);
  end;

  function Init_Log_Frequency_Total return Float is
    Total : Float := 0.0;
  begin
    for Frequency of Frequencies loop
      Total := Total + Frequency_Log_Frequency(Frequency);
    end loop;
    return Total;
  end;

  function Log_Frequency_Noise return Float is
    use Ada.Numerics.Float_Random;
    G : Generator;
  begin
    Reset(G);
    return Random(G) * 1.0E-4;
  end;

  Log_Frequency_Total : constant Float := Init_Log_Frequency_Total;

  type Entropy_Cell is record
    Frequency_Sum     : Natural := Frequency_Total;
    Log_Frequency_Sum : Float   := Log_Frequency_Total;
    Noise             : Float   := Log_Frequency_Noise;
  end record;

  type Wave_Function_Cell is record
    Collapsed : Boolean := False;
    Possible  : Wave_Function := (others => True);
    Enablers  : Enabler_Counts(Tile_ID_Range) := Init_Enablers;
    Entropy   : Entropy_Cell;
  end record;

  function Num_Remaining_Possibilities (Cell : in Wave_Function_Cell) return Natural is
    Result : Natural := 0;
  begin
    for The_Tile_ID in Tile_ID_Range loop
      if Cell.Possible(The_Tile_ID) then
        Result := Result + 1;
      end if;
    end loop;

    return Result;
  end;

  function Random_Remaining_Tile (Cell : in Wave_Function_Cell) return Tile_ID_Range is
    subtype Choice_Range is Natural range 1 .. Cell.Entropy.Frequency_Sum;
    package Random_Choice is new Ada.Numerics.Discrete_Random(Choice_Range);
    use Random_Choice;

    G : Generator;
    Choice : Choice_Range;
  begin
    Reset(G);
    Choice := Random(G);

    for The_Tile_ID in Tile_ID_Range loop
      if Cell.Possible(The_Tile_ID) then
        if Choice <= Frequencies(The_Tile_ID) then
          return The_Tile_ID;
        else
          Choice := Choice - Frequencies(The_Tile_ID);
        end if;
      end if;
    end loop;

    raise Constraint_Error;
  end;

  function Entropy_Value (Cell : in Wave_Function_Cell) return Float is
    Float_Freq_Sum : constant Float := Float(Cell.Entropy.Frequency_Sum);
    Log_Freq_Sum   : constant Float := Float_Functions.Log(Float_Freq_Sum);
  begin
    return Cell.Entropy.Noise + Log_Freq_Sum - 
      (Cell.Entropy.Log_Frequency_Sum / Float_Freq_Sum);
  end;
  
  Remaining_Uncollapsed : Natural := World'Length(1) * World'Length(2);

  Wave_Function_Matrix : array (World'Range(1), World'Range(2)) of Wave_Function_Cell;
  Collapsed_Tile_Choices : Tile_ID_Matrix(World'Range(1), World'Range(2));

  pragma Debug (Put_Line("Tile_ID_Bytes =>" & Integer'Image(Tile_ID_Range'Object_Size / 8)));
  pragma Debug (Put_Line("Small_Bytes =>" & Integer'Image(Small_Integer'Object_Size / 8)));
  pragma Debug (Put_Line("Cell_Bytes =>" & Integer'Image(Wave_Function_Cell'Object_Size / 8)));
  pragma Debug (Put_Line("Wave_Bytes =>" & Integer'Image(Wave_Function'Object_Size / 8)));
  pragma Debug (Put_Line("Enabler_Bytes =>" & Integer'Image(Init_Enablers'Size / 8)));
  pragma Debug (Put_Line("Matrix_Bytes =>" & Integer'Image(Wave_Function_Matrix'Size / 8)));
  pragma Debug (New_Line(1));

  type Cell_Coord is record
    X : Natural;
    Y : Natural;
  end record
    with Dynamic_Predicate =>
      Cell_Coord.X in World'Range(1) and
      Cell_Coord.Y in World'Range(2);

  function Neighbor (Coord : in Cell_Coord; Direction : in Adjacency_Direction) return Cell_Coord is
    X : Natural renames Coord.X;
    Y : Natural renames Coord.Y;
  begin
    return (case Direction is
      when Upwards    =>
        (X, ((Y - 1 - World'First(2)) mod World'Length(2)) + World'First(2)),
      when Downwards  =>
        (X, ((Y + 1 - World'First(2)) mod World'Length(2)) + World'First(2)),
      when Leftwards  =>
        (((X - 1 - World'First(1)) mod World'Length(1)) + World'First(1), Y),
      when Rightwards =>
        (((X + 1 - World'First(1)) mod World'Length(1)) + World'First(1), Y)
    );
  end;


  type Entropy_Cell_ID is record
    Coord   : Cell_Coord;
    Entropy : Float;
  end record;

  function Entropy_Cell_At (X, Y : Natural) return Entropy_Cell_ID
  is
    Cell    : Wave_Function_Cell renames Wave_Function_Matrix(X, Y);
    Entropy : constant Float := Entropy_Value(Cell);
  begin
    return Entropy_Cell_ID'((X, Y), Entropy);
  end;

  function Entropy_Cell_Priority (Cell_ID : in Entropy_Cell_ID) return Float is
    ( Cell_ID.Entropy );

  package Entropy_Queue_Interfaces is
    new Ada.Containers.Synchronized_Queue_Interfaces(Entropy_Cell_ID);

  package Entropy_Priority_Queues is
    new Ada.Containers.Unbounded_Priority_Queues
      ( Entropy_Queue_Interfaces
      , Queue_Priority => Float
      , Get_Priority   => Entropy_Cell_Priority
      , Before         => "<");

  Entropy_Heap : Entropy_Priority_Queues.Queue;

  type Removal_Update is record
    Coord : Cell_Coord;
    Removed_Tile_ID : Tile_ID_Range;
  end record;

  package Removal_Vectors is
    new Ada.Containers.Vectors(Natural, Removal_Update);

  Propagation_Removals : Removal_Vectors.Vector;

  procedure Collapse (X, Y : Natural) is
    Cell : Wave_Function_Cell renames Wave_Function_Matrix(X, Y);
    Collapsed_Tile : constant Tile_ID_Range := Random_Remaining_Tile(Cell);
  begin
    if Cell.Collapsed then
      return;
    end if;

    Cell.Collapsed := True;
    Cell.Entropy.Frequency_Sum := Frequencies(Collapsed_Tile);
    Cell.Entropy.Log_Frequency_Sum := Frequency_Log_Frequency(Cell.Entropy.Frequency_Sum);

    for Tile_Possibility in Tile_ID_Range loop
      if Tile_Possibility /= Collapsed_Tile and Cell.Possible(Tile_Possibility) then
        Cell.Possible(Tile_Possibility) := False;
        Propagation_Removals.Append(
          Removal_Update'((X, Y), Tile_Possibility));
      end if;
    end loop;

    if Num_Remaining_Possibilities(Cell) = 0 then
      raise Unsatisfiable_Tile_Constraint with
        Natural'Image(Cell.Entropy.Frequency_Sum) & " " &
        Float'Image(Cell.Entropy.Log_Frequency_Sum) & " " & 
        Natural'Image(X) & " " &
        Natural'Image(Y);
    end if;

    Collapsed_Tile_Choices(X, Y) := Collapsed_Tile;

    Remaining_Uncollapsed := Remaining_Uncollapsed - 1;
  end;

  procedure Remove_Tile_Possibility (X, Y : Natural; T : Tile_ID_Range) is
    Cell   : Wave_Function_Cell renames Wave_Function_Matrix(X, Y);
    T_Freq : constant Natural := Frequencies(T);
  begin
    if not Cell.Possible(T) then
      return;
    end if;

    Cell.Possible(T) := False;

    Cell.Entropy.Frequency_Sum :=
      Cell.Entropy.Frequency_Sum - T_Freq;

    Cell.Entropy.Log_Frequency_Sum :=
      Cell.Entropy.Log_Frequency_Sum - Frequency_Log_Frequency(T_Freq);

    if Num_Remaining_Possibilities(Cell) = 0 then
      raise Unsatisfiable_Tile_Constraint with
        Natural'Image(Cell.Entropy.Frequency_Sum) & " " &
        Float'Image(Cell.Entropy.Log_Frequency_Sum) & " " & 
        Natural'Image(X) & " " &
        Natural'Image(Y);
    end if;

    Entropy_Heap.Enqueue(Entropy_Cell_At(X, Y));
    Propagation_Removals.Append(Removal_Update'((X, Y), T));
  end;

  procedure Handle_Propagation_Removal (Update : in Removal_Update) is
    Coord           : Cell_Coord    renames Update.Coord;
    Removed_Tile_ID : Tile_ID_Range renames Update.Removed_Tile_ID;
  begin
    for Direction in Adjacency_Direction loop
      for Adjacent_Tile in Tile_ID_Range loop
        if Adjacencies(Removed_Tile_ID, Adjacent_Tile)(Direction) then
          declare
            Nbor_Coord : constant Cell_Coord := Neighbor(Coord, Direction);
            Nbor_Cell : Wave_Function_Cell renames
              Wave_Function_Matrix(Nbor_Coord.X, Nbor_Coord.Y);

            Opposite : constant Adjacency_Direction := Opposite_Direction(Direction);
            Enablers : Enablers_By_Direction renames Nbor_Cell.Enablers(Adjacent_Tile);
          begin
            if (for all Count of Enablers => Count /= 0) then
              Enablers(Opposite) := Enablers(Opposite) - 1;
              if Enablers(Opposite) = 0 then
                Remove_Tile_Possibility(Nbor_Coord.X, Nbor_Coord.Y, Adjacent_Tile);
              end if;
            end if;
          end;
        end if;
      end loop;
    end loop;
  end;

begin

  -- Initialize cell entropy heap
  for X in World'Range(1) loop
    for Y in World'Range(2) loop
      Entropy_Heap.Enqueue(Entropy_Cell_At(X, Y));
    end loop;
  end loop;

  while Remaining_Uncollapsed > 0 loop
    declare
      Min_Entropy : Entropy_Cell_ID;
      Min_X : Natural renames Min_Entropy.Coord.X;
      Min_Y : Natural renames Min_Entropy.Coord.Y;
      Next_Update : Removal_Update;
    begin
      Entropy_Heap.Dequeue(Min_Entropy);
      Collapse(Min_X, Min_Y);
      
      while Propagation_Removals.Length > 0 loop
        Next_Update := Propagation_Removals.Last_Element;
        Propagation_Removals.Delete_Last;
        Handle_Propagation_Removal(Next_Update);
      end loop;

    exception
      when Unsatisfiable_Tile_Constraint => return False;
    end;
  end loop;
  
  for X in World'Range(1) loop
    for Y in World'Range(2) loop
      declare
        Collapsed_Tile : constant Tile_ID_Range := Collapsed_Tile_Choices(X, Y);
      begin
        Put(Tail(Tile_ID'Image(Collapsed_Tile), 4));
        World(X, Y) := Tile_Elements(Collapsed_Tile);
      end;
    end loop;
    New_Line;
  end loop;
  New_Line;

  return True;

end;
