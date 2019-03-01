#version 430

#extension GL_ARB_compute_variable_group_size : enable

//layout( local_size_variable ) in;
  layout( local_size_x = 10,
          local_size_y = 10,
          local_size_z =  1   ) in;

////////////////////////////////////////////////////////////////////////////////

  ivec3 _WorkGrupsN = ivec3( gl_NumWorkGroups );

//ivec3 _WorkItemsN = ivec3( gl_LocalGroupSizeARB );
  ivec3 _WorkItemsN = ivec3( gl_WorkGroupSize     );

  ivec3 _WorksN     = _WorkGrupsN * _WorkItemsN;

  ivec3 _WorkID     = ivec3( gl_GlobalInvocationID );

//############################################################################## ■

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDoubleC

struct TDoubleC
{
    double R;
    double I;
};

//------------------------------------------------------------------------------

TDoubleC Add( TDoubleC A, TDoubleC B )
{
    TDoubleC Result;

    Result.R = A.R + B.R;
    Result.I = A.I + B.I;

    return Result;
}

TDoubleC Sub( TDoubleC A, TDoubleC B )
{
    TDoubleC Result;

    Result.R = A.R - B.R;
    Result.I = A.I - B.I;

    return Result;
}

TDoubleC Mul( TDoubleC A, TDoubleC B )
{
    TDoubleC Result;

    Result.R = A.R * B.R - A.I * B.I;
    Result.I = A.R * B.I + A.I * B.R;

    return Result;
}

TDoubleC Pow2( TDoubleC C )
{
    return Mul( C, C );
}

double Abs( TDoubleC C )
{
    return sqrt( C.R * C.R + C.I * C.I );
}

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TAreaC

struct TAreaC
{
    TDoubleC Min;
    TDoubleC Max;
};

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【外部変数】

layout( std430 ) buffer TBuffer
{
    TAreaC _RangeC;
};

writeonly uniform image2D _Imager;

uniform sampler1D _Textur;

//############################################################################## ■

TDoubleC ScreenToComplex( ivec2 S )
{
    TDoubleC Result;

    Result.R = ( _RangeC.Max.R - _RangeC.Min.R ) * ( S.x + 0.5 ) / _WorksN.x + _RangeC.Min.R;
    Result.I = ( _RangeC.Min.I - _RangeC.Max.I ) * ( S.y + 0.5 ) / _WorksN.y + _RangeC.Max.I;

    return Result;
}

vec4 ComplexToColor( TDoubleC C )
{
    TDoubleC Z = TDoubleC( 0, 0 );

    for ( int N = 1; N < 500; N++ )
    {
        Z = Add( Pow2( Z ), C );

        if ( Abs( Z ) > 2 ) return texture( _Textur, N / 500.0 );
    }

    return texture( _Textur, 1 );
}

////////////////////////////////////////////////////////////////////////////////

void main()
{
  TDoubleC C = ScreenToComplex( _WorkID.xy );

  vec4 L = ComplexToColor( C );

  imageStore( _Imager, _WorkID.xy, L );
}

//############################################################################## ■