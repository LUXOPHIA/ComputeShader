unit Main;

interface //#################################################################### ■

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects,
  Winapi.OpenGL, Winapi.OpenGLext,
  LUX, LUX.D1, LUX.D2, LUX.D3, LUX.D4x4, LUX.Complex, LUX.FMX.Controls,
  LUX.GPU.OpenGL,
  LUX.GPU.OpenGL.Atom.Buffer.StoBuf,
  LUX.GPU.OpenGL.Atom.Buffer.PixBuf.D1,
  LUX.GPU.OpenGL.Atom.Buffer.PixBuf.D2,
  LUX.GPU.OpenGL.Atom.Imager,
  LUX.GPU.OpenGL.Atom.Imager.D1.Preset,
  LUX.GPU.OpenGL.Atom.Imager.D2.Preset,
  LUX.GPU.OpenGL.Atom.Textur.D1.Preset,
  LUX.GPU.OpenGL.Atom.Textur.D2.Preset,
  LUX.GPU.OpenGL.Comput;

type
  TForm1 = class(TForm)
    Image1: TImage;
    Panel1: TPanel;
      Image2: TImage;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Image1MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
    procedure Timer1Timer(Sender: TObject);
  private
    { private 宣言 }
  public
    { public 宣言 }
    _ImageW :Integer;
    _ImageH :Integer;
    _RangeC :TDoubleAreaC;
    _Comput :TGLComput;
    _Buffer :TGLStoBuf<TDoubleAreaC>;
    _Textur :TGLPoiTex1D_TAlphaColorF;
    _Imager :TGLCelIma2D_TAlphaColorF;
    ///// メソッド
    procedure InitComput;
    procedure InitBuffer;
    procedure InitTextur;
    procedure InitImager;
  end;

var
  Form1: TForm1;

implementation //############################################################### ■

uses System.Math;

{$R *.fmx}

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

/////////////////////////////////////////////////////////////////////// メソッド

procedure TForm1.InitComput;
begin
     _Comput.ItemsX := 10;
     _Comput.ItemsY := 10;
     _Comput.ItemsZ :=  1;

     _Comput.WorksX := _ImageW;
     _Comput.WorksY := _ImageH;
     _Comput.WorksZ :=       1;

     _Comput.ShaderC.Source.LoadFromFile( '..\..\_DATA\Comput.glsl' );

     Assert( _Comput.ShaderC.Status, _Comput.ShaderC.Errors.Text );

     Assert( _Comput.Engine.Status, _Comput.Engine.Errors.Text );

     _Comput.Buffers.Add( 'TBuffer', _Buffer );
     _Comput.Imagers.Add( '_Imager', _Imager );
     _Comput.Texturs.Add( '_Textur', _Textur );
end;

procedure TForm1.InitBuffer;
begin
     _Buffer[ 0 ] := _RangeC;
end;

procedure TForm1.InitTextur;
var
   D :TGLPoiPixIter1D<TAlphaColorF>;
begin
     _Textur.Imager.Grid.PoinsX := 4;

     D := _Textur.Imager.Grid.Map;

     D[ 0 ] := TAlphaColorF.Create( 0, 0, 0 );
     D[ 1 ] := TAlphaColorF.Create( 1, 0, 0 );
     D[ 2 ] := TAlphaColorF.Create( 1, 1, 0 );
     D[ 3 ] := TAlphaColorF.Create( 1, 1, 1 );

     D.DisposeOf;

     _Textur.Imager.CopyTo( Image2.Bitmap );
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
     //_Comput.RunARB;
     _Comput.Run;

     _Imager.CopyTo( Image1.Bitmap );
end;

procedure TForm1.InitImager;
begin
     _Imager.Grid.CellsX := _ImageW;
     _Imager.Grid.CellsY := _ImageH;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

procedure TForm1.FormCreate(Sender: TObject);
begin
     _Comput := TGLComput.Create;
     _Buffer := TGLStoBuf<TDoubleAreaC>.Create( GL_STATIC_DRAW );
     _Textur := TGLPoiTex1D_TAlphaColorF.Create;
     _Imager := TGLCelIma2D_TAlphaColorF.Create;

     _ImageW := 600;
     _ImageH := 600;

     _RangeC := TDoubleAreaC.Create( -2, -2, +2, +2 );

     InitComput;
     InitBuffer;
     InitTextur;
     InitImager;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
     _Comput.DisposeOf;
     _Buffer.DisposeOf;
     _Textur.DisposeOf;
     _Imager.DisposeOf;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TForm1.Image1MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
var
   S :Double;
   C :TDoubleC;
begin
     S := Power( 1.1, WheelDelta / 120 );

     with _RangeC do
     begin
          with Image1.MousePos do
          begin
               C.R := Min.R + SizeR * X / Image1.Width ;
               C.I := Max.I - SizeI * Y / Image1.Height;
          end;

          Min := ( Min - C ) * S + C;
          Max := ( Max - C ) * S + C;
     end;

     _Buffer[ 0 ] := _RangeC;
end;

end. //######################################################################### ■
