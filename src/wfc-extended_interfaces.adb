
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

separate (WFC)
package body Extended_Interfaces is

  function Generic_Collapse_Within (Parameters : in Instance) return Boolean is

    X_Dim_Length : constant X_Dim := X_Dim'Last - X_Dim'First + 1;
    Y_Dim_Length : constant Y_Dim := Y_Dim'Last - Y_Dim'First + 1;

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
    
    Remaining_Uncollapsed : Natural :=
      Natural(X_Dim_Length) * Natural(Y_Dim_Length);

    type Wave_Function_Matrix_Type is
      array (X_Dim, Y_Dim) of Wave_Function_Cell;

    type Wave_Function_Matrix_Access is
      access Wave_Function_Matrix_Type;

    Wave_Function_Matrix : constant Wave_Function_Matrix_Access
      := new Wave_Function_Matrix_Type;
    

    type Info is new Collapse_Info with null record;

    overriding
    function Is_Possible
      (This : in Info; X : X_Dim; Y : Y_Dim; T : Tile_ID) return Boolean;
      
    overriding
    function Is_Possible
      (This : in Info; X : X_Dim; Y : Y_Dim; E : Element_Type) return Boolean;

    overriding
    function Is_Possible
      (This : in Info; X : X_Dim; Y : Y_Dim; T : Tile_ID) return Boolean
    is
      pragma Unreferenced (This);
      Cell : Wave_Function_Cell renames Wave_Function_Matrix(X, Y);
    begin
      return (T in Tile_ID_Range) and then Cell.Possible(T);
    end;

    overriding
    function Is_Possible
      (This : in Info; X : X_Dim; Y : Y_Dim; E : Element_Type) return Boolean
    is
      pragma Unreferenced (This);
      Cell : Wave_Function_Cell renames Wave_Function_Matrix(X, Y);
    begin
      return (for some Tile_ID in Tile_ID_Range =>
        Cell.Possible(Tile_ID) and then Tile_Elements(Tile_ID) = E);
    end;


    Collapsed_Tile_Choices : array (X_Dim, Y_Dim) of Tile_ID_Range;

    type Cell_Coord is record
      X : X_Dim;
      Y : Y_Dim;
    end record;

    function Neighbor (Coord : in Cell_Coord; Direction : in Adjacency_Direction) return Cell_Coord is
      X : X_Dim renames Coord.X;
      Y : Y_Dim renames Coord.Y;
    begin
      return (case Direction is
        when Upwards =>
          (X => X, Y => ((Y - 1 - Y_Dim'First) mod Y_Dim_Length) + Y_Dim'First),
        when Downwards =>
          (X => X, Y => ((Y + 1 - Y_Dim'First) mod Y_Dim_Length) + Y_Dim'First),
        when Leftwards =>
          (Y => Y, X => ((X - 1 - X_Dim'First) mod X_Dim_Length) + X_Dim'First),
        when Rightwards =>
          (Y => Y, X => ((X + 1 - X_Dim'First) mod X_Dim_Length) + X_Dim'First)
      );
    end;


    type Entropy_Cell_ID is record
      Coord   : Cell_Coord;
      Entropy : Float;
    end record;

    function Entropy_Cell_At (X : X_Dim; Y : Y_Dim) return Entropy_Cell_ID
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

    procedure Collapse (X : X_Dim; Y : Y_Dim) is
      Cell : Wave_Function_Cell renames Wave_Function_Matrix(X, Y);
      Collapsed_Tile : Tile_ID_Range;
    begin
      if Cell.Collapsed then
        return;
      end if;

      Collapsed_Tile := Random_Remaining_Tile(Cell);
      Cell.Collapsed := True;

      for Tile_Possibility in Tile_ID_Range loop
        if Tile_Possibility /= Collapsed_Tile and Cell.Possible(Tile_Possibility) then
          Cell.Possible(Tile_Possibility) := False;
          Propagation_Removals.Append(
            Removal_Update'((X, Y), Tile_Possibility));
        end if;
      end loop;

      if Num_Remaining_Possibilities(Cell) = 0 then
        raise Unsatisfiable_Tile_Constraint with
          Cell.Entropy.Frequency_Sum'Image
          & " " & Cell.Entropy.Log_Frequency_Sum'Image
          & " " & X'Image
          & " " & Y'Image;
      end if;

      Collapsed_Tile_Choices(X, Y) := Collapsed_Tile;
      Remaining_Uncollapsed := Remaining_Uncollapsed - 1;

      Upon_Collapse(X, Y, Info'(null record));
    end;

    procedure Remove_Tile_Possibility (X : X_Dim; Y : Y_Dim; T : Tile_ID_Range) is
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
          Cell.Entropy.Frequency_Sum'Image
          & " " & Cell.Entropy.Log_Frequency_Sum'Image
          & " " & X'Image
          & " " & Y'Image;
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

                Upon_Removal(Nbor_Coord.X, Nbor_Coord.Y, Info'(null record));
              end if;
            end;
          end if;
        end loop;
      end loop;
    end;

    procedure Put_Debug_Info is
    begin
      Put_Line(Standard_Error, "Tile_ID_Bytes =>" & Integer'Image(Tile_ID_Range'Object_Size / 8));
      Put_Line(Standard_Error, "  Small_Bytes =>" & Integer'Image(Small_Integer'Object_Size / 8));
      Put_Line(Standard_Error, "   Cell_Bytes =>" & Integer'Image(Wave_Function_Cell'Object_Size / 8));
      Put_Line(Standard_Error, "   Wave_Bytes =>" & Integer'Image(Wave_Function'Object_Size / 8));
      Put_Line(Standard_Error, "Enabler_Bytes =>" & Integer'Image(Init_Enablers'Size / 8));
      Put_Line(Standard_Error, " Matrix_Bytes =>" & Integer'Image(Wave_Function_Matrix.all'Size / 8));
      New_Line(Standard_Error, 1);

      for Y in Y_Dim loop
        for X in X_Dim loop
          declare
            Collapsed_Tile : constant Tile_ID_Range := Collapsed_Tile_Choices(X, Y);
          begin
            Put(Standard_Error, Tail(Tile_ID'Image(Collapsed_Tile), 4));
          end;
        end loop;
        New_Line(Standard_Error);
      end loop;
      New_Line(Standard_Error);
    end;

  begin
    
    declare
      type Settings is new Info and Collapse_Settings with null record;

      overriding
      procedure Require (This : Settings; X : X_Dim; Y : Y_Dim; T : Tile_ID);

      overriding
      procedure Require (This : Settings; X : X_Dim; Y : Y_Dim; E : Element_Type);

      overriding
      procedure Require (This : Settings; X : X_Dim; Y : Y_Dim; T : Tile_ID) is
        pragma Unreferenced (This);
      begin
        for Other_T in Tile_ID_Range loop
          if Other_T /= T then
            Remove_Tile_Possibility(X, Y, Other_T);
          end if;
        end loop;
      end;
      
      overriding
      procedure Require (This : Settings; X : X_Dim; Y : Y_Dim; E : Element_Type) is
        pragma Unreferenced (This);
      begin
        for T in Tile_ID_Range loop
          if Tile_Elements(T) /= E then
            Remove_Tile_Possibility(X, Y, T);
          end if;
        end loop;
      end;

    begin
      Set_Initial_Requirements(Settings'(null record));

      while Propagation_Removals.Length > 0 loop
        declare 
          Next_Update : constant Removal_Update := Propagation_Removals.Last_Element;
        begin
          Propagation_Removals.Delete_Last;
          Handle_Propagation_Removal(Next_Update);
        end;
      end loop;

    end;

    -- Initialize cell entropy heap
    for X in X_Dim loop
      for Y in Y_Dim loop
        Entropy_Heap.Enqueue(Entropy_Cell_At(X, Y));
      end loop;
    end loop;

    while Remaining_Uncollapsed > 0 loop
      declare
        Min_Entropy : Entropy_Cell_ID;
        Min_X : X_Dim renames Min_Entropy.Coord.X;
        Min_Y : Y_Dim renames Min_Entropy.Coord.Y;
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

    pragma Debug (Put_Debug_Info);

    for X in X_Dim loop
      for Y in Y_Dim loop
        declare
          Collapsed_Tile : constant Tile_ID_Range := Collapsed_Tile_Choices(X, Y);
        begin
          Set_Resulting_Element(X, Y, Tile_Elements(Collapsed_Tile));
        end;
      end loop;
    end loop;

    return True;

  end;

end;