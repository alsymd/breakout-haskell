#version 400 core
in vec2 TexCoords;
in vec4 spriteColor;

out vec4 color;

uniform sampler2D image;

void main()
{
  color = spriteColor * texture(image,TexCoords);
}
