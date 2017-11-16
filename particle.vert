#version 400 core
layout (location = 0) in vec4 vertex;
layout (location = 1) in vec2 offset;
layout (location = 2) in int _image_idx;
layout (location = 3) in vec3 _spriteColor;

out vec2 TexCoords;
out vec3 spriteColor;
flat out int idx;

uniform mat4 projection;


void main()
{
  float scale = 10.0f;
  TexCoords = vertex.zw;
  spriteColor = _spriteColor;
  idx = _image_idx;
  
  // gl_Position = projection * model * vec4(vertex.xy, 0.0, 1.0);
  gl_Position = projection*vec4(vertex.xy*scale+offset,0.0,1.0);
}
