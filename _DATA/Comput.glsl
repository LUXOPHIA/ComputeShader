#version 430

#extension GL_ARB_compute_variable_group_size : enable

//layout( local_size_variable ) in;
layout( local_size_x = 10,
        local_size_y = 10,
        local_size_z =  1 ) in;

////////////////////////////////////////////////////////////////////////////////

const uvec3 _WorkGrupsN = gl_NumWorkGroups;

//const uvec3 _WorkItemsN = gl_LocalGroupSizeARB;
const uvec3 _WorkItemsN = gl_WorkGroupSize;

const uvec3 _WorksN = _WorkGrupsN * _WorkItemsN;

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

    Result.R = ( _RangeC.Max.R - _RangeC.Min.R ) / _WorksN.x * ( S.x + 0.5 ) + _RangeC.Min.R;
    Result.I = ( _RangeC.Min.I - _RangeC.Max.I ) / _WorksN.y * ( S.y + 0.5 ) + _RangeC.Max.I;

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
  ivec2 I = ivec2( gl_GlobalInvocationID.xy );

  TDoubleC C = ScreenToComplex( I );

  vec4 L = ComplexToColor( C );

  imageStore( _Imager, I, L );
}

//############################################################################## ■