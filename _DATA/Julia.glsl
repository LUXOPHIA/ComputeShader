#version 430

#extension GL_ARB_compute_variable_group_size : enable

layout( local_size_variable ) in;

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

layout( std430 ) buffer TJuliCons
{
    TDoubleC _JuliCons;
};

writeonly uniform image2D _JuliImag;

//############################################################################## ■

TDoubleC ScreenToComplex( ivec2 S )
{
    const uvec3 _ItemSize = gl_LocalGroupSizeARB * gl_NumWorkGroups;

    const TAreaC _JuliArea = TAreaC( TDoubleC( -1, -1 ),
                                     TDoubleC( +1, +1 ) );

    TDoubleC Result;

    Result.R = ( _JuliArea.Max.R - _JuliArea.Min.R ) / _ItemSize.x * ( S.x + 0.5 ) + _JuliArea.Min.R;
    Result.I = ( _JuliArea.Min.I - _JuliArea.Max.I ) / _ItemSize.y * ( S.y + 0.5 ) + _JuliArea.Max.I;

    return Result;
}

vec4 ComplexToColor( TDoubleC C )
{
    vec4 Color0 = vec4( 0, 0, 0, 1 );
    vec4 Color1 = vec4( 1, 1, 1, 1 );

    TDoubleC Z = C;

    for ( int N = 1; N < 256; N++ )
    {
        Z = Add( Pow2( Z ), _JuliCons );

        if ( Abs( Z ) > 2 ) return ( Color1 - Color0 ) * N / 256 + Color0;
    }

    return Color1;
}

////////////////////////////////////////////////////////////////////////////////

void main()
{
  ivec2 I = ivec2( gl_GlobalInvocationID.xy );

  TDoubleC C = ScreenToComplex( I );

  vec4 L = ComplexToColor( C );

  imageStore( _JuliImag, I, L );
}

//############################################################################## ■