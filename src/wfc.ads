
generic
  type Element_Type is private;
  -- The type of picture elements within the input.

  type Element_Matrix is
    array (Natural range <>, Natural range <>) of Element_Type;
  -- The array type we'll take as input and provide as output.
  -- We treat these as a toroidal surface which infinitely tiles.

  with function "<" (E1, E2 : Element_Type) return Boolean is <>;
  with function "=" (E1, E2 : Element_Type) return Boolean is <>;
  -- Because the element type might be a record, we don't
  -- constrain it to be discrete, but for various purposes
  -- we at least require it to be orderable.

package WFC is

  subtype Tile_ID is Positive;
  -- A tile represents a part of the input array as a submatrix of elements.
  -- Because passing around arrays by value is
  -- more expensive, instead we represent tiles as
  -- indices into arrays of their properties, for simplicity.

  type Tile_Element_Array is
    array (Tile_ID range <>) of Element_Type;
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

  type Small_Integer is mod 2 ** 16;
  -- This only needs to be large enough to hold a number
  -- as high as a particular instance's Num_Tiles value.
  --
  -- The necessary size of adjacency matrices in the general case
  -- becomes far larger than is reasonable long before
  -- the number of tiles reaches 65536, but more than 255 tiles
  -- is possible, so we just use two bytes to count enablers.

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
  --
  -- If this fails many times it may indicate that the tileset is unable
  -- to seamlessly tile a matrix of the size given; this may happen if
  -- the size of a single tile is larger than the matrix especially, or
  -- if every tile in the sample used to create the instance was unique,
  -- leading to a tileset which can only tile spaces with size equal
  -- to the sample modulo its dimensions.
  --
  -- In such cases the recommendation is simply to use larger samples,
  -- or decrease the N/M tile parameter until there is wider representation
  -- of different tiles and adjacencies.


  generic
    type X_Dim is range <>;
    type Y_Dim is range <>;
  package Extended_Interfaces is

    -- If more control over generation, data structures, etc.
    -- is required, then the extended interfaces package
    -- provides an way of allowing more internals of the collapse
    -- algorithm to be inspected or configured as need be.

    type Collapse_Info is interface;
    -- For the more generic collapsing procedure, we allow returning
    -- information to the user by querying an instance of this type.

    function Is_Possible
      ( Info : in Collapse_Info;
        X : in X_Dim;
        Y : in Y_Dim;
        E : in Element_Type) return Boolean is abstract;

    function Is_Possible
      ( Info : in Collapse_Info;
        X : in X_Dim;
        Y : in Y_Dim;
        T : in Tile_ID) return Boolean is abstract;

    -- Determine whether it's possible for a particular element or
    -- tile_id to occur at the position specified.

    type Collapse_Settings is interface and Collapse_Info;
    -- This type not only allows querying information
    -- about what is possible, but also configuring
    -- certain requirements within the 

    procedure Require
      (Info : in Collapse_Settings;
      X : in X_Dim;
      Y : in Y_Dim;
      E : in Element_Type) is abstract;

    procedure Require
      (Info : in Collapse_Settings;
      X : in X_Dim;
      Y : in Y_Dim;
      T : in Tile_ID) is abstract;

    -- inject requirements for certain positions to have
    -- certain tile or element values.
    
    generic
      with procedure Set_Resulting_Element
        (X : X_Dim; Y : Y_Dim; Element : Element_Type) is <>;

      with procedure Set_Initial_Requirements
        (Info : in Collapse_Settings'Class) is null;

      with procedure Upon_Collapse
        (X : X_Dim; Y : Y_Dim; Info : in Collapse_Info'Class) is null;

      with procedure Upon_Removal
        (X : X_Dim; Y : Y_Dim; Info : in Collapse_Info'Class) is null;

    function Generic_Collapse_Within (Parameters : in Instance) return Boolean;

  end;

end;
