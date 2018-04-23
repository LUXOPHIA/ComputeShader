unit LUX.GPU.OpenGL.Comput;

interface //#################################################################### ■

uses System.SysUtils, System.UITypes,
     Winapi.OpenGL, Winapi.OpenGLext,
     LUX,
     LUX.Data.Dictionary,
     LUX.GPU.OpenGL,
     LUX.GPU.OpenGL.Atom.Buffer,
     LUX.GPU.OpenGL.Atom.Buffer.VerBuf,
     LUX.GPU.OpenGL.Atom.Buffer.StoBuf,
     LUX.GPU.OpenGL.Atom.Imager,
     LUX.GPU.OpenGL.Atom.Shader,
     LUX.GPU.OpenGL.Atom.Engine;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TGLComput

     IGLComput = interface
     ['{13071090-B024-474A-BDA2-AB604AD10B16}']
     {protected}
       ///// アクセス
       function GetEngine  :TGLEngine;
       function GetShaderC :TGLShaderC;
       function GetImagers :TIndexDictionary<String,IGLImager>;
       function GetBuffers :TIndexDictionary<String,IGLBuffer>;
     {public}
       ///// プロパティ
       property Engine  :TGLEngine                          read GetEngine ;
       property ShaderC :TGLShaderC                         read GetShaderC;
       property Imagers :TIndexDictionary<String,IGLImager> read GetImagers;
       property Buffers :TIndexDictionary<String,IGLBuffer> read GetBuffers;
       ///// メソッド
       procedure Run( const WorksX_,WorksY_,WorksZ_:GLuint );
     end;

     //-------------------------------------------------------------------------

     TGLComput = class( TInterfacedObject, IGLComput )
     private
     protected
       _Engine  :TGLEngine;
       _ShaderC :TGLShaderC;
       _Imagers :TIndexDictionary<String,IGLImager>;
       _Buffers :TIndexDictionary<String,IGLBuffer>;
       ///// アクセス
       function GetEngine  :TGLEngine;
       function GetShaderC :TGLShaderC;
       function GetImagers :TIndexDictionary<String,IGLImager>;
       function GetBuffers :TIndexDictionary<String,IGLBuffer>;
     public
       constructor Create;
       destructor Destroy; override;
       ///// プロパティ
       property Engine  :TGLEngine                          read GetEngine ;
       property ShaderC :TGLShaderC                         read GetShaderC;
       property Imagers :TIndexDictionary<String,IGLImager> read GetImagers;
       property Buffers :TIndexDictionary<String,IGLBuffer> read GetBuffers;
       ///// メソッド
       procedure Run( const WorksX_,WorksY_,WorksZ_:GLuint );
     end;

//const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

implementation //############################################################### ■

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TGLComput

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TGLComput.GetEngine :TGLEngine;
begin
     Result := _Engine;
end;

//------------------------------------------------------------------------------

function TGLComput.GetShaderC :TGLShaderC;
begin
     Result := _ShaderC;
end;

//------------------------------------------------------------------------------

function TGLComput.GetImagers :TIndexDictionary<String,IGLImager>;
begin
     Result := _Imagers;
end;

function TGLComput.GetBuffers :TIndexDictionary<String,IGLBuffer>;
begin
     Result := _Buffers;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TGLComput.Create;
begin
     inherited;

     _Engine  := TGLEngine .Create;
     _ShaderC := TGLShaderC.Create;

     _Imagers := TIndexDictionary<String,IGLImager>.Create;
     _Buffers := TIndexDictionary<String,IGLBuffer>.Create;

     _Engine.Attach( _ShaderC{Shad} );
end;

destructor TGLComput.Destroy;
begin
     _Imagers.DisposeOf;
     _Buffers.DisposeOf;

     _Engine .DisposeOf;
     _ShaderC.DisposeOf;

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TGLComput.Run( const WorksX_,WorksY_,WorksZ_:GLuint );
var
   K :String;
begin
     for K in _Imagers.Keys do
     begin
          with _Imagers[ K ] do
          begin
               _Engine.Imagers.Add( Index{BinP}, K{Name} );
          end;
     end;

     for K in _Buffers.Keys do
     begin
          with _Buffers[ K ] do
          begin
               _Engine.StoBufs.Add( Index{BinP}, K{Name} );
          end;
     end;

     _Engine.Link;

     _Engine.Use;

     for K in _Imagers.Keys do
     begin
          with _Imagers[ K ] do Value.UseComput( Index );
     end;

     for K in _Buffers.Keys do
     begin
          with _Buffers[ K ] do glBindBufferBase( GL_SHADER_STORAGE_BUFFER, Index, Value.ID );
     end;

     glDispatchCompute( WorksX_, WorksY_, WorksZ_ );

     _Engine.Unuse;
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■
