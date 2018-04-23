#version 430

layout( local_size_x = 10,
        local_size_y = 10,
        local_size_z =  1 ) in;

//############################################################################## ■

layout( std430 ) buffer _Values0
{
    float Values0[];
};

layout( std430 ) buffer _Values1
{
    float Values1[];
};

layout( std430 ) buffer TPosBuf
{
    float _PosBuf[];
};

layout( std430 ) buffer TNorBuf
{
    float _NorBuf[];
};

writeonly uniform image2D _Imager;

////////////////////////////////////////////////////////////////////////////////

vec3 GetPoins( int X, int Y )
{
    vec3 Result;

    int I3 = 3 * ( 100 * Y + X );

    Result.x = _PosBuf[ I3+0 ];
    Result.x = _PosBuf[ I3+1 ];
    Result.x = _PosBuf[ I3+2 ];

    return Result;
}

void SetPoins( int X, int Y, vec3 P )
{
    int I3 = 3 * ( 100 * Y + X );

    _PosBuf[ I3+0 ] = P.x;
    _PosBuf[ I3+1 ] = P.y;
    _PosBuf[ I3+2 ] = P.z;
}

//############################################################################## ■

void main()
{
  ivec2 storePos = ivec2( gl_GlobalInvocationID.xy );
  int   I = 100 * storePos.y + storePos.x;
  vec2  T = vec2( storePos ) / 100.0;

  float localCoef = length( vec2( 256, 256 ) - vec2( ivec2( gl_GlobalInvocationID.xy ) ) );

  imageStore( _Imager,
              storePos,
              vec4( 0.5 + 0.5 * sin( localCoef * 0.05 ), 0.5, Values0[ 0 ], 1 ) );

  vec3 P;

  P.x = 2.0 * storePos.x / 100.0 - 1.0;
  P.y = 0;
  P.z = 2.0 * storePos.y / 100.0 - 1.0;

  SetPoins( storePos.x, storePos.y, P );
}

//############################################################################## ■