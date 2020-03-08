
generic
  type Element is private;
  -- The type of picture elements within the input

  type Element_Matrix is
    array (Natural range <>, Natural range <>) of Element;
  -- The array type we'll take as input and provide as output.
  -- We treat these as a toroidal surface which infinitely tiles.

  with function "<" (E1, E2 : Element) return Boolean is <>;
  with function "=" (E1, E2 : Element) return Boolean is <>;
  -- Because the element type might be a record, we don't
  -- constrain it to be discrete, but for various purposes
  -- we at least require it to be orderable.

package Wave_Function_Collapse is

  subtype Tile_ID is Positive;
  -- A tile represents a part of the input array as a submatrix of elements.
  -- Because passing around arrays by value is
  -- more expensive, instead we represent tiles as
  -- indices into arrays of their properties, for simplicity.

  type Tile_Element_Array is
    array (Tile_ID range <>) of Element;
  -- Mapping the id to the tile itself is somewhat
  -- more than required, as each tile contributes only one
  -- pixel to the output. Here, we map each tile to
  -- the pixel it will contribute. The adjacency information
  -- will guarantee their combined behavior is as desired.

  type Frequency_Array is
    array (Tile_ID range <>) of Natural;
  -- Mapping each tile to its frequency in the input

  type Adjacency_Direction is
    (Upwards, Downwards, Leftwards, Rightwards);
  -- The directions in which we record tile adjacency information.

  type Adjacency is 
    array (Adjacency_Direction) of Boolean
    with Pack;
  -- A bitvector recording for each direction
  -- whether there is an adjacency.

  type Adjacency_Matrix is
    array (Tile_ID range <>, Tile_ID range <>) of Adjacency
    with Pack;
  -- For every pair of tiles, we store whether they are
  -- allowed to be adjacent to each other, in each direction.
  -- For efficiency we also pack together the bitvectors;
  -- when the number of tiles is large this still becomes the majority
  -- of the data we need to store, even when each adjacency
  -- is stored in one bit.

  type Small_Integer is mod 16384;

  type Enablers_By_Direction is
    array (Adjacency_Direction) of Small_Integer
    with Pack;

  type Enabler_Counts is
    array (Tile_ID range <>) of Enablers_By_Direction;
  -- The number of "enablers" of a particular tile
  -- in a direction is the number of other tiles which
  -- can be adjacent in that direction.

  type Instance (Num_Tiles : Natural) is tagged record
    Tile_Elements   : Tile_Element_Array (1 .. Num_Tiles);
    Frequency_Total : Natural;
    Frequencies     : Frequency_Array (1 .. Num_Tiles);
    Adjacencies     : Adjacency_Matrix (1 .. Num_Tiles, 1 .. Num_Tiles);
    Enablers        : Enabler_Counts (1 .. Num_Tiles); 
  end record;
  -- All information we need to record about a particular input
  -- in order to generate similar outputs via wave function collapse.
  
  function Initialize_From_Sample
    ( Sample              : in Element_Matrix;
      Include_Rotations   : in Boolean  := False;
      Include_Reflections : in Boolean  := False;
      N, M                : in Positive := 2
    ) return Instance;
  -- Create an instantiation representing the empirical tile and frequency
  -- information from a sample matrix provided. If desired,
  -- this information will be combined with rotated and reflected
  -- versions of the tiles for more variety. The size of each tile
  -- is specified with the N and M parameters.

  function Collapse_Within 
    (Parameters : in Instance; World : out Element_Matrix) return Boolean;
  -- Given an instance and a matrix to populate, attempt a wave function
  -- collapse within the space with the tiles from the parameters.
  -- This is an operation which is (although likely) not guaranteed to
  -- succeed, and so the return value indicates whether it was successful.

end;
