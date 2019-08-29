(object tri
  (shaders
    (vertex-shader #p"simple.vert")
    (fragment-shader #p"flat-pink.frag"))
  (vertices
    (0 positions float 3
      -0.5 -0.5 0.0
       0.5 -0.5 0.0
       0.0  0.5 0.0)
    (1 colors float 3
      1.0 0.0 0.0
      0.0 1.0 0.0
      0.0 0.0 1.0))
  (uniforms
    (3 model mat4
     identity)))
