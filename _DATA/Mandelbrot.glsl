#version 430

layout( local_size_x = 10,
        local_size_y = 10,
        local_size_z =  1 ) in;

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

layout( std430 ) buffer TMandArea
{
    TAreaC _MandArea;
};

writeonly uniform image2D _MandImag;

//############################################################################## ■

TDoubleC ScrToCom( ivec2 S )
{
    TDoubleC Result;

    Result.R = ( _MandArea.Max.R - _MandArea.Min.R ) / 400.0 * ( S.x + 0.5 ) + _MandArea.Min.R;
    Result.I = ( _MandArea.Min.I - _MandArea.Max.I ) / 400.0 * ( S.y + 0.5 ) + _MandArea.Max.I;

    return Result;
}

vec4 ComToCol( TDoubleC C )
{
    vec4 Color0 = vec4( 0, 0, 0, 1 );
    vec4 Color1 = vec4( 1, 1, 1, 1 );

    TDoubleC Z = TDoubleC( 0, 0 );

    for ( int N = 1; N < 256; N++ )
    {
        Z = Add( Pow2( Z ), C );

        if ( Abs( Z ) > 2 ) return ( Color1 - Color0 ) * N / 256 + Color0;
    }

    return Color1;
}

////////////////////////////////////////////////////////////////////////////////

void main()
{
  ivec2 I = ivec2( gl_GlobalInvocationID.xy );

  TDoubleC C = ScrToCom( I );

  vec4 L = ComToCol( C );

  imageStore( _MandImag, I, L );
}

//############################################################################## ■