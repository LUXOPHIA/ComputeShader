﻿unit Main;

interface //#################################################################### ■

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Objects,
  Winapi.OpenGL, Winapi.OpenGLext,
  LUX, LUX.D1, LUX.D2, LUX.D3, LUX.M4,
  LUX.GPU.OpenGL,
  LUX.GPU.OpenGL.Viewer,
  LUX.GPU.OpenGL.Atom.Shader,
  LUX.GPU.OpenGL.Atom.Buffer.StoBuf,
  LUX.GPU.OpenGL.Scener,
  LUX.GPU.OpenGL.Camera,
  LUX.GPU.OpenGL.Shaper,
  LUX.GPU.OpenGL.Matery,
  LUX.GPU.OpenGL.Matery.Imager.Preset,
  LUX.GPU.OpenGL.Atom.Imager.D2.Preset,
  LUX.GPU.OpenGL.Comput,
  LUX.GPU.OpenGL.Render,
  LUX.Complex,
  LUX.FMX.Controls, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm1 = class(TForm)
    Image1: TImage;
    Timer1: TTimer;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Image1MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
  private
    { private 宣言 }
  public
    { public 宣言 }
    _ImageW   :Integer;
    _ImageH   :Integer;
    _MandComp :TGLComput;
    _MandArea :TGLStoBuf<TDoubleAreaC>;
    _MandImag :TGLBricer2D_TAlphaColorF;
    ///// メソッド
    procedure InitMand;
    procedure DrawMand;
  end;

var
  Form1: TForm1;

implementation //############################################################### ■

uses System.Math;

{$R *.fmx}

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

/////////////////////////////////////////////////////////////////////// メソッド

procedure TForm1.InitMand;
begin
     _MandComp := TGLComput.Create;

     _MandArea := TGLStoBuf<TDoubleAreaC>.Create( GL_STATIC_DRAW );

     _MandImag := TGLBricer2D_TAlphaColorF.Create;

     with _MandComp do
     begin
          ItemsX := 20;
          ItemsY := 20;
          ItemsZ :=  1;

          WorksX := _ImageW;
          WorksY := _ImageH;
          WorksZ :=       1;

          ShaderC.Source.LoadFromFile( '..\..\_DATA\Mandelbrot.glsl' );

          with Engine do Assert( Status, Errors.Text );

          Buffers.Add( 'TMandArea', _MandArea );

          Imagers.Add( '_MandImag', _MandImag );
     end;

     _MandArea[ 0 ] := TDoubleAreaC.Create( -2, -1.5, +2, +1.5 );

     with _MandImag do
     begin
          with Texels do
          begin
               BricsX := _ImageW;
               BricsY := _ImageH;
          end;

          SendData;
     end;
end;

//------------------------------------------------------------------------------

procedure TForm1.DrawMand;
begin
     _MandComp.Run;

     _MandImag.ExportTo( Image1.Bitmap );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

procedure TForm1.FormCreate(Sender: TObject);
begin
     _ImageW := 800;
     _ImageH := 600;

     InitMand;
     DrawMand;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
     _MandComp.DisposeOf;
     _MandArea.DisposeOf;
     _MandImag.DisposeOf;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TForm1.Image1MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
var
   S :Double;
   C :TDoubleC;
   A :TDoubleAreaC;
begin
     S := Power( 1.1, WheelDelta / 120 );

     with _MandArea.Map do
     begin
          with Items[ 0 ] do
          begin
               with Image1.MousePos do
               begin
                    C.R := Min.R + SizeR * X / Image1.Width ;
                    C.I := Max.I - SizeI * Y / Image1.Height;
               end;

               A.Min := ( Min - C ) * S + C;
               A.Max := ( Max - C ) * S + C;
          end;

          Items[ 0 ] := A;

          DisposeOf;
     end;

     DrawMand;
end;

end. //######################################################################### ■
