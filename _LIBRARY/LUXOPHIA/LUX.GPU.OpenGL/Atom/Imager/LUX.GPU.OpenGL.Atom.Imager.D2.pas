unit LUX.GPU.OpenGL.Atom.Imager.D2;

interface //#################################################################### ■

uses Winapi.OpenGL, Winapi.OpenGLext,
     LUX, LUX.Data.Lattice.T2, LUX.GPU.OpenGL.Atom.Imager;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TGLImager2D<_TTexel_,_TTexels_>

     IGLImager2D = interface( IGLImager )
     ['{69B48023-273B-46B0-A8E4-AD79BABB51FD}']
     {protected}
     {public}
     end;

     //-------------------------------------------------------------------------

     TGLImager2D<_TTexel_:record;_TTexels_:constructor,TArray2D<_TTexel_>> = class( TGLImager<_TTexel_,_TTexels_>, IGLImager2D )
     private
     protected
     public
       constructor Create;
       destructor Destroy; override;
       ///// メソッド
       procedure SendData; override;
       procedure SendPixBuf; override;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TGLPoiIma2D<_TTexel_,_TTexels_>

     IGLPoiIma2D = interface( IGLImager2D )
     ['{655EF4B2-51B3-4B2F-A9A6-25DBDC399981}']
     {protected}
     {public}
     end;

     //-------------------------------------------------------------------------

     TGLPoiIma2D<_TTexel_:record> = class( TGLImager2D<_TTexel_,TPoinArray2D<_TTexel_>>, IGLPoiIma2D )
     private
     protected
     public
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TGLCelIma2D<_TTexel_,_TTexels_>

     IGLCelIma2D = interface( IGLImager2D )
     ['{1088510C-E25B-47B9-8756-924DBB06BD03}']
     {protected}
     {public}
     end;

     //-------------------------------------------------------------------------

     TGLCelIma2D<_TTexel_:record> = class( TGLImager2D<_TTexel_,TCellArray2D<_TTexel_>>, IGLCelIma2D )
     private
     protected
     public
     end;

//const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

implementation //############################################################### ■

uses System.Math;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TGLImager2D<_TTexel_,_TTexels_>

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TGLImager2D<_TTexel_,_TTexels_>.Create;
begin
     inherited Create( GL_TEXTURE_2D );

end;

destructor TGLImager2D<_TTexel_,_TTexels_>.Destroy;
begin

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TGLImager2D<_TTexel_,_TTexels_>.SendData;
begin
     Bind;
       glTexImage2D( _Kind, 0, _TexelF, _Texels.ElemsX,
                                        _Texels.ElemsY, 0,
                               _PixelF,
                               _PixelT,
                               _Texels.Elem0P );
     Unbind;
end;

//------------------------------------------------------------------------------

procedure TGLImager2D<_TTexel_,_TTexels_>.SendPixBuf;
begin
     glTexImage2D( _Kind, 0, _TexelF, _Texels.ElemsX,
                                      _Texels.ElemsY, 0,
                             _PixelF,
                             _PixelT, nil );
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TGLPoiIma2D<_TTexel_,_TTexels_>

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TGLCelIma2D<_TTexel_,_TTexels_>

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■