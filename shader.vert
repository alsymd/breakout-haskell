#version 440 core
layout (location = 0) in vec4 vertex;

out vec2 TexCoords;
out int instanceID;
uniform mat4 projection;
uniform mat4 model[100];

void main()
{
  TexCoords = vertex.zw;
  gl_Position = projection * model[gl_InstanceID] *vec4(vertex.xy, 0.0, 1.0);
  // gl_Position = projection * vec4(vertex.xy, 0.0, 1.0);
  // gl_Position = vec4(3*vertex.xy,0.0,1.0);
  instanceID = gl_InstanceID;
}
