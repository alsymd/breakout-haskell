#version 400 core
layout (location = 0) in vec4 vertex;
layout (location = 1) in vec2 offset;
layout (location = 2) in vec4 _spriteColor;

out vec2 TexCoords;
out vec4 spriteColor;

uniform mat4 projection;


void main()
{
  const float scale = 10.0f;
  TexCoords = vertex.zw;
  spriteColor = _spriteColor;
  
  gl_Position = projection*vec4(vertex.xy*scale+offset,0.0,1.0);
}
