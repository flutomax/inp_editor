// converter constants

const
  ELTYPES: array[0..86,0..2] of integer = (
    (0,  0, 0),                 // empty.
    (11, 11, 1),                 // Rod
    (11, 21, 1),                 // Linear beam
    (12, 22, 1),                 // Tapered beam
    ( 0, 23, 1),                 // Curved beam
    (12, 24, 1),                 // Parabolic beam
    ( 0, 31, 1),                 // Straight pipe
    ( 0, 32, 1),                 // Curved pipe
    ( 7, 41, 0),                 // Plane Stress Linear Triangle
    ( 8, 42, 0),                 // Plane Stress Parabolic Triangle
    ( 0, 43, 0),                 // Plane Stress Cubic Triangle
    ( 9, 44, 0),                 // Plane Stress Linear Quadrilateral
    (10, 45, 0),                 // Plane Stress Parabolic Quadrilateral
    ( 0, 46, 0),                 // Plane Strain Cubic Quadrilateral
    ( 7, 51, 0),                 // Plane Strain Linear Triangle
    ( 8, 52, 0),                 // Plane Strain Parabolic Triangle
    ( 0, 53, 0),                 // Plane Strain Cubic Triangle
    ( 9, 54, 0),                 // Plane Strain Linear Quadrilateral
    (10, 55, 0),                 // Plane Strain Parabolic Quadrilateral
    ( 0, 56, 0),                 // Plane Strain Cubic Quadrilateral
    ( 7, 61, 0),                 // Plate Linear Triangle
    ( 8, 62, 0),                 // Plate Parabolic Triangle
    ( 0, 63, 0),                 // Plate Cubic Triangle
    ( 9, 64, 0),                 // Plate Linear Quadrilateral
    (10, 65, 0),                 // Plate Parabolic Quadrilateral
    ( 0, 66, 0),                 // Plate Cubic Quadrilateral
    ( 9, 71, 0),                 // Membrane Linear Quadrilateral
    ( 8, 72, 0),                 // Membrane Parabolic Triangle
    (10, 73, 0),                 // Membrane Cubic Triangle
    ( 7, 74, 0),                 // Membrane Linear Triangle
    ( 0, 75, 0),                 // Membrane Parabolic Quadrilateral
    ( 0, 76, 0),                 // Membrane Cubic Quadrilateral
    ( 7, 81, 0),                 // Axisymetric Solid Linear Triangle
    ( 8, 82, 0),                 // Axisymetric Solid Parabolic Triangle
    ( 9, 84, 0),                 // Axisymetric Solid Linear Quadrilateral
    (10, 85, 0),                 // Axisymetric Solid Parabolic Quadrilateral
    ( 7, 91, 0),                 // Thin Shell Linear Triangle
    ( 8, 92, 0),                 // Thin Shell Parabolic Triangle
    ( 0, 93, 0),                 // Thin Shell Cubic Triangle
    ( 9, 94, 0),                 // Thin Shell Linear Quadrilateral
    (10, 95, 0),                 // Thin Shell Parabolic Quadrilateral
    ( 0, 96, 0),                 // Thin Shell Cubic Quadrilateral
    ( 0,101, 0),                 // Thick Shell Linear Wedge
    ( 0,102, 0),                 // Thick Shell Parabolic Wedge
    ( 0,103, 0),                 // Thick Shell Cubic Wedge
    ( 0,104, 0),                 // Thick Shell Linear Brick
    ( 0,105, 0),                 // Thick Shell Parabolic Brick
    ( 0,106, 0),                 // Thick Shell Cubic Brick
    ( 3,111, 0),                 // Solid Linear Tetrahedron
    ( 2,112, 0),                 // Solid Linear Wedge
    ( 5,113, 0),                 // Solid Parabolic Wedge
    ( 0,114, 0),                 // Solid Cubic Wedge
    ( 1,115, 0),                 // Solid Linear Brick
    ( 4,116, 0),                 // Solid Parabolic Brick
    ( 0,117, 0),                 // Solid Cubic Brick
    ( 6,118, 0),                 // Solid Parabolic Tetrahedron
    ( 0,121, 0),                 // Rigid Bar
    ( 0,122, 0),                 // Rigid Element
    ( 0,136, 0),                 // Node To Node Translational Spring
    ( 0,137, 0),                 // Node To Node Rotational Spring
    ( 0,138, 0),                 // Node To Ground Translational Spring
    ( 0,139, 0),                 // Node To Ground Rotational Spring
    ( 0,141, 0),                 // Node To Node Damper
    ( 0,142, 0),                 // Node To Gound Damper
    ( 0,151, 0),                 // Node To Node Gap
    ( 0,152, 0),                 // Node To Ground Gap
    ( 0,161, 0),                 // Lumped Mass
    ( 0,171, 0),                 // Axisymetric Linear Shell
    ( 0,172, 0),                 // Axisymetric Parabolic Shell
    ( 0,181, 0),                 // Constraint
    ( 0,191, 0),                 // Plastic Cold Runner
    ( 0,192, 0),                 // Plastic Hot Runner
    ( 0,193, 0),                 // Plastic Water Line
    ( 0,194, 0),                 // Plastic Fountain
    ( 0,195, 0),                 // Plastic Baffle
    ( 0,196, 0),                 // Plastic Rod Heater
    ( 0,201, 0),                 // Linear node-to-node interface
    ( 0,202, 0),                 // Linear edge-to-edge interface
    ( 0,203, 0),                 // Parabolic edge-to-edge interface
    ( 0,204, 0),                 // Linear face-to-face interface
    ( 0,208, 0),                 // Parabolic face-to-face interface
    ( 0,212, 0),                 // Linear axisymmetric interface
    ( 0,213, 0),                 // Parabolic axisymmetric interface
    ( 0,221, 0),                 // Linear rigid surface
    ( 0,222, 0),                 // Parabolic rigin surface
    ( 0,231, 0),                 // Axisymetric linear rigid surface
    ( 0,232, 0)                  // Axisymetric parabolic rigid surface
  );

  MAPUNV: array[TBuffIndex,0..20] of integer = (
    ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0), // null
    ( 1, 1, 2, 3, 4, 5, 6, 7, 8, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0), // C3D8
    ( 2, 1, 2, 3, 4, 5, 6, 0, 0, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0), // C3D6
    ( 3, 1, 2, 3, 4, 0, 0, 0, 0, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0), // C3D4
    ( 4, 1, 3, 5, 7,13,15,17,19, 2, 4,  6, 8,14,16,18,20, 9,10,11,12), // C3D20
    ( 5, 1, 3, 5,10,12,14, 2, 4, 6,11, 13,15, 7, 8, 9, 0, 0, 0, 0, 0), // C3D15
    ( 6, 1, 3, 5,10, 2, 4, 6, 7, 8, 9,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0), // C3D10
    ( 7, 1, 3, 2, 0, 0, 0, 0, 0, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0), // S3
    ( 8, 1, 5, 3, 6, 4, 2, 0, 0, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0), // S6
    ( 9, 1, 4, 3, 2, 0, 0, 0, 0, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0), // S4R
    (10, 1, 7, 5, 3, 8, 6, 4, 2, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0), // S8R
    (11, 1, 2, 0, 0, 0, 0, 0, 0, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0), // B31
    (12, 1, 2, 3, 0, 0, 0, 0, 0, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0), // B32
    ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0), // null
    ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0), // null
    ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0)  // null
    );

  FACES: array[0..5,0..6,0..4] of integer = (
    (
      (0, 0, 0, 0, 0),                       // empty.
      (0, 0, 0, 0, 0),
      (0, 0, 0, 0, 0),
      (0, 0, 0, 0, 0),
      (0, 0, 0, 0, 0),
      (0, 0, 0, 0, 0),
      (0, 0, 0, 0, 0)
    ),
    (
      (5, 0, 0, 0, 0),                       // wedges 6/15 nodes.
      (3, 1, 2, 3, 0),
      (3, 4, 6, 5, 0),
      (4, 1, 4, 5, 2),
      (4, 2, 5, 6, 3),
      (4, 3, 6, 4, 1),
      (0, 0, 0, 0, 0)
    ),
    (
      (6, 0, 0, 0, 0),                       // Bricks 8/20 nodes.
      (4, 1, 2, 3, 4),
      (4, 5, 8, 7, 6),
      (4, 1, 5, 6, 2),
      (4, 2, 6, 7, 3),
      (4, 3, 7, 8, 4),
      (4, 4, 8, 5, 1)
    ),
    (
      (4, 0, 0, 0, 0),                       // tetrahedra), 4/10 nodes.
      (3, 1, 2, 3, 0),
      (3, 1, 4, 2, 0),
      (3, 2, 4, 3, 0),
      (3, 3, 4, 1, 0),
      (0, 0, 0, 0, 0),
      (0, 0, 0, 0, 0)
    ),
    (
      (4, 0, 0, 0, 0),                       // quadrilaterals), 4/8 nodes.
      (2, 1, 2, 0, 0),
      (2, 2, 3, 0, 0),
      (2, 3, 4, 0, 0),
      (2, 4, 1, 0, 0),
      (0, 0, 0, 0, 0),
      (0, 0, 0, 0, 0)
    ),
    (
      (3, 0, 0, 0, 0),                       // triangles), 3/6 nodes.
      (2, 1, 2, 0, 0),
      (2, 2, 3, 0, 0),
      (2, 3, 1, 0, 0),
      (0, 0, 0, 0, 0),
      (0, 0, 0, 0, 0),
      (0, 0, 0, 0, 0)
    )
  );

  NOMABQ: array[TBuffIndex] of string = (
    'null', 'C3D8', 'C3D6', 'C3D4', 'C3D20', 'C3D15', 'C3D10', 'S3',
    'S6', 'S4R', 'S8R', 'B31', 'B32', 'null', 'null', 'null'
  );

  NUMNODES: TIntBuffer = (0, 8, 6, 4,20,15,10, 3, 6, 4, 8, 2, 3, 0, 0, 0);
  DIMENSIONS: TIntBuffer = (0, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 1, 1, 0, 0, 0);
  ELPOINTER: TIntBuffer = (0, 2, 1, 3, 2, 1, 3, 5, 5, 4, 4, 0, 0, 0, 0, 0);

  SMATCH: array[0..1,TBuffIndex] of integer = (
    (0, 9, 7, 7,10,10, 8,11,12,11,12, 0, 0, 0, 0, 0),
    (0, 0, 9, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  );

