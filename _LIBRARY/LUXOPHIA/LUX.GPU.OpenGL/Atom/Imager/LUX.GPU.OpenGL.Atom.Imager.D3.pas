unit LUX.GPU.OpenGL.Atom.Imager.D3;

interface //#################################################################### ■

uses Winapi.OpenGL, Winapi.OpenGLext,
     LUX, LUX.Data.Lattice.T3, LUX.GPU.OpenGL.Atom.Imager;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TGLImager3D<_TTexel_,_TTexels_>

     IGLImager3D = interface( IGLImager )
     ['{EBD2C427-B4C8-4649-8654-E79708545A23}']
     {protected}
     {public}
     end;

     //-------------------------------------------------------------------------

     TGLImager3D<_TTexel_:record;_TTexels_:constructor,TArray3D<_TTexel_>> = class( TGLImager<_TTexel_,_TTexels_>, IGLImager3D )
     private
     protected
     public
       constructor Create;
       destructor Destroy; override;
       ///// メソッド
       procedure SendData; override;
       procedure SendPixBuf; override;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TGLPoiIma3D<_TTexel_,_TTexels_>

     IGLPoiIma3D = interface( IGLImager3D )
     ['{35C2AB20-C24E-4BC7-8500-29DD6E51357B}']
     {protected}
     {public}
     end;

     //-------------------------------------------------------------------------

     TGLPoiIma3D<_TTexel_:record> = class( TGLImager3D<_TTexel_,TPoinArray3D<_TTexel_>>, IGLPoiIma3D )
     private
     protected
     public
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TGLCelIma3D<_TTexel_,_TTexels_>

     IGLCelIma3D = interface( IGLImager3D )
     ['{2786645A-C1E4-4210-83D1-0F7CD7871778}']
     {protected}
     {public}
     end;

     //-------------------------------------------------------------------------

     TGLCelIma3D<_TTexel_:record> = class( TGLImager3D<_TTexel_,TCellArray3D<_TTexel_>>, IGLCelIma3D )
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

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TGLImager3D<_TTexel_,_TTexels_>

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TGLImager3D<_TTexel_,_TTexels_>.Create;
begin
     inherited Create( GL_TEXTURE_3D );

end;

destructor TGLImager3D<_TTexel_,_TTexels_>.Destroy;
begin

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TGLImager3D<_TTexel_,_TTexels_>.SendData;
begin
     Bind;
       glTexImage3D( _Kind, 0, _TexelF, _Texels.ElemsX,
                                        _Texels.ElemsY,
                                        _Texels.ElemsZ, 0,
                               _PixelF,
                               _PixelT,
                               _Texels.Elem0P );
     Unbind;
end;

//------------------------------------------------------------------------------

procedure TGLImager3D<_TTexel_,_TTexels_>.SendPixBuf;
begin
     glTexImage3D( _Kind, 0, _TexelF, _Texels.ElemsX,
                                      _Texels.ElemsY,
                                      _Texels.ElemsZ, 0,
                             _PixelF,
                             _PixelT, nil );
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TGLPoiIma3D<_TTexel_,_TTexels_>

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TGLCelIma3D<_TTexel_,_TTexels_>

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■