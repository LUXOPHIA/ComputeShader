unit Main;

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
  LUX.Complex;

type
  TForm1 = class(TForm)
    Image1: TImage;
    Image2: TImage;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Image2MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure Image2MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure Image2MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure Timer1Timer(Sender: TObject);
  private
    { private 宣言 }
    _MouseS :TShiftState;
    _MouseP :TPointF;
  public
    { public 宣言 }
    _ImageW   :Integer;
    _ImageH   :Integer;
    _MandComp :TGLComput;
    _MandArea :TGLStoBuf<TDoubleAreaC>;
    _MandImag :TGLBricer2D_TAlphaColorF;
    _JuliComp :TGLComput;
    _JuliCons :TGLStoBuf<TDoubleC>;
    _JuliImag :TGLBricer2D_TAlphaColorF;
    ///// メソッド
    procedure InitMand;
    procedure InitJuli;
    procedure DrawMand;
    procedure DrawJuli;
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
          with ShaderC do
          begin
               Source.LoadFromFile( '..\..\_DATA\Mandelbrot.glsl' );

               Assert( Status, Errors.Text );
          end;

          with Engine do Assert( Status, Errors.Text );

          Buffers.Add( 'TMandArea', _MandArea );

          Imagers.Add( '_MandImag', _MandImag );
     end;

     _MandArea[ 0 ] := TDoubleAreaC.Create( -1.5, -1.0, +0.5, +1.0 );

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

procedure TForm1.InitJuli;
begin
     _JuliComp := TGLComput.Create;

     _JuliCons := TGLStoBuf<TDoubleC>.Create( GL_STATIC_DRAW );
     _JuliImag := TGLBricer2D_TAlphaColorF.Create;

     with _JuliComp do
     begin
          with ShaderC do
          begin
               Source.LoadFromFile( '..\..\_DATA\Julia.glsl' );

               Assert( Status, Errors.Text );
          end;

          with Engine do Assert( Status, Errors.Text );

          Buffers.Add( 'TJuliCons', _JuliCons );

          Imagers.Add( '_JuliImag', _JuliImag );
     end;

     with _JuliImag.Texels do
     begin
          BricsX := _ImageW;
          BricsY := _ImageH;
     end;

     _JuliImag.SendData;
end;

//------------------------------------------------------------------------------

procedure TForm1.DrawMand;
begin
     _MandComp.Run( _ImageW div 10, _ImageH div 10, 1 );

     _MandImag.ExportTo( Image2.Bitmap );
end;

procedure TForm1.DrawJuli;
begin
     _JuliComp.Run( _ImageW div 10, _ImageH div 10, 1 );

     _JuliImag.ExportTo( Image1.Bitmap );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

procedure TForm1.FormCreate(Sender: TObject);
begin
     _ImageW := 400;
     _ImageH := 400;

     InitMand;
     InitJuli;

     _MouseP := TPointF.Create( 3/4 * Image2.Width,
                                1/2 * Image2.Height );

     DrawMand;
     DrawJuli;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
     _MandComp.DisposeOf;
     _MandArea.DisposeOf;
     _MandImag.DisposeOf;

     _JuliComp.DisposeOf;
     _JuliCons.DisposeOf;
     _JuliImag.DisposeOf;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TForm1.Timer1Timer(Sender: TObject);
var
   C :TDoubleC;
begin
     C.R :=     2 * _MouseP.X / Image2.Width - 1.5;
     C.I := 1 - 2 * _MouseP.Y / Image2.Height     ;

     _JuliCons[ 0 ] := C;

     DrawJuli;
end;

//------------------------------------------------------------------------------

procedure TForm1.Image2MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
     _MouseS := Shift;
     _MouseP := TPointF.Create( X, Y );
end;

procedure TForm1.Image2MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
     if ssLeft in _MouseS then _MouseP := TPointF.Create( X, Y );
end;

procedure TForm1.Image2MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
     Image2MouseMove( Sender, Shift, X, Y );

     _MouseS := [];
end;

end. //######################################################################### ■
