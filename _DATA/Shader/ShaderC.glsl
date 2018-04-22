#version 430

writeonly uniform image2D _Imager;

layout( local_size_x = 32,
        local_size_y = 32,
        local_size_z =  1 ) in;

layout( std430 ) buffer _Values0
{
    float Values0[];
};

layout( std430 ) buffer _Values1
{
    float Values1[];
};

void main()
{
  ivec2 storePos = ivec2( gl_GlobalInvocationID.xy );

  float localCoef = length( vec2( 256, 256 ) - vec2( ivec2( gl_GlobalInvocationID.xy ) ) );

  imageStore( _Imager,
              storePos,
              vec4( 0.5 + 0.5 * sin( localCoef * 0.05 ), 0.5, Values0[ 0 ], 1 ) );

  Values0[ 1 ] = 1;
  Values0[ 2 ] = 2;
  Values0[ 3 ] = 3;
  Values0[ 4 ] = 4;
}