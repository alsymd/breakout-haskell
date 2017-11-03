#version 440 core
in vec2 TexCoords;
flat in int instanceID;
out vec4 color;

uniform sampler2D image[3];
uniform uint image_idx[100];
uniform vec3 spriteColor[100];

void main()
{
  color = vec4(spriteColor[instanceID], 1.0) * texture(image[image_idx[instanceID]],TexCoords);
  // color = vec4(0.0,1.0,0.0,1.0);
}
