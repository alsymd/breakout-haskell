#version 400 core
in vec2 TexCoords;
in vec3 spriteColor;
flat in int idx;

out vec4 color;

uniform sampler2D image[5];

void main()
{
  color = vec4(spriteColor, 1.0) * texture(image[idx],TexCoords);
}
