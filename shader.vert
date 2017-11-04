#version 440 core
layout (location = 0) in vec4 vertex;
layout (location = 1) in mat4 model;
layout (location = 5) in int _image_idx;
layout (location = 6) in vec3 _spriteColor;

out vec2 TexCoords;
out vec3 spriteColor;
out int idx;

uniform mat4 projection;


void main()
{
  TexCoords = vertex.zw;
  spriteColor = _spriteColor;
  idx = _image_idx;
  
  // gl_Position = projection * model * vec4(vertex.xy, 0.0, 1.0);
  gl_Position = projection*model*vec4(vertex.xy,0.0,1.0);
}
