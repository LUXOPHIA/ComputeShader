unit Main;

interface //#################################################################### ■

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects,
  Winapi.OpenGL, Winapi.OpenGLext,
  LUX, LUX.D1, LUX.D2, LUX.D3, LUX.M4, LUX.Complex, LUX.FMX.Controls,
  LUX.GPU.OpenGL,
  LUX.GPU.OpenGL.Atom.Buffer.StoBuf,
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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Image1MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
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
    procedure Init;
    procedure Draw;
  end;

var
  Form1: TForm1;

implementation //############################################################### ■

uses System.Math;

{$R *.fmx}

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

/////////////////////////////////////////////////////////////////////// メソッド

procedure TForm1.Init;
begin
     _ImageW := 600;
     _ImageH := 600;

     _RangeC := TDoubleAreaC.Create( -2, -2, +2, +2 );

     with _Comput do
     begin
          ItemsX := 10;
          ItemsY := 10;
          ItemsZ :=  1;

          WorksX := _ImageW;
          WorksY := _ImageH;
          WorksZ :=       1;

          with ShaderC do
          begin
               Source.LoadFromFile( '..\..\_DATA\Comput.glsl' );

               Assert( Status, Errors.Text );
          end;

          with Engine do Assert( Status, Errors.Text );

          Buffers.Add( 'TBuffer', _Buffer );
          Imagers.Add( '_Imager', _Imager );
          Texturs.Add( '_Textur', _Textur );
     end;

     _Buffer[ 0 ] := _RangeC;

     with _Textur.Imager do
     begin
          with Grider do
          begin
               PoinsX := 3;
          end;

          Grider[ 0 ] := TAlphaColorF.Create( 0, 0, 0 );
          Grider[ 1 ] := TAlphaColorF.Create( 1, 0, 0 );
          Grider[ 2 ] := TAlphaColorF.Create( 1, 1, 1 );

          SendData;

          CopyTo( Image2.Bitmap );
     end;

     with _Imager do
     begin
          with Grider do
          begin
               CellsX := _ImageW;
               CellsY := _ImageH;
          end;

          SendData;
     end;
end;

//------------------------------------------------------------------------------

procedure TForm1.Draw;
begin
     //_Comput.RunARB;
     _Comput.Run;

     _Imager.CopyTo( Image1.Bitmap );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

procedure TForm1.FormCreate(Sender: TObject);
begin
     _Comput := TGLComput.Create;
     _Buffer := TGLStoBuf<TDoubleAreaC>.Create( GL_STATIC_DRAW );
     _Textur := TGLPoiTex1D_TAlphaColorF.Create;
     _Imager := TGLCelIma2D_TAlphaColorF.Create;

     Init;
     Draw;
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

     Draw;
end;

end. //######################################################################### ■
