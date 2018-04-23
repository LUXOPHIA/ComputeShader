unit Main;

interface //#################################################################### ■

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
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
  LUX.GPU.OpenGL.Render, FMX.Objects, FMX.Controls.Presentation, FMX.ScrollBox,
  FMX.Memo;

type
  TForm1 = class(TForm)
    GLViewer1: TGLViewer;
    Image1: TImage;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GLViewer1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure GLViewer1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure GLViewer1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
  private
    { private 宣言 }
    _MouseA :TSingle2D;
    _MouseS :TShiftState;
    _MouseP :TSingle2D;
  public
    { public 宣言 }
    _Scener :TGLScener;
    _Camera :TGLCameraPers;
    _Comput :TGLComput;
    _Matery :TGLMateryImag;
    _Shaper :TGLShaperFace;
    _StoBuf0 :TGLStoBuf<Single>;
    _StoBuf1 :TGLStoBuf<Single>;
    ///// メソッド
    procedure MakeMatery;
    procedure MakeComput;
    procedure MakeShaper;
  end;

var
  Form1: TForm1;

implementation //############################################################### ■

uses System.Math;

{$R *.fmx}

procedure TForm1.MakeMatery;
begin
     _Matery := TGLMateryImag.Create;

     with _Matery do
     begin
          with Imager do
          begin
               LoadFromFile( '..\..\_DATA\Spherical_2048x1024.png' );
          end;

          with ShaderV do
          begin
               Source.LoadFromFile( '..\..\_DATA\Shader\ShaderV.glsl' );
          end;

          with ShaderF do
          begin
               Source.LoadFromFile( '..\..\_DATA\Shader\ShaderF.glsl' );
          end;

          with Engine do Assert( Status, Errors.Text );
     end;
end;

procedure TForm1.MakeComput;
begin
     _Comput := TGLComput.Create;

     with _Comput do
     begin
          with ShaderC do
          begin
               Source.LoadFromFile( '..\..\_DATA\Shader\ShaderC.glsl' );

               Assert( Status, Errors.Text );
          end;

          Imagers.Add( '_Imager', _Matery.Imager );

          Buffers.Add( '_Values0', _StoBuf0 );
          Buffers.Add( '_Values1', _StoBuf1 );
          Buffers.Add( 'TPosBuf', _Shaper.PosBuf );
          Buffers.Add( 'TNorBuf', _Shaper.NorBuf );
     end;
end;

function BraidedTorus( const T_:TdSingle2D ) :TdSingle3D;
const
     LoopR :Single = 1.0;  LoopN :Integer = 3;
     TwisR :Single = 0.5;  TwisN :Integer = 5;
     PipeR :Single = 0.3;
var
   T :TdSingle2D;
   cL, cT, cP, TX, PX, R,
   sL, sT, sP, TY, PY, H :TdSingle;
begin
     T := Pi2 * T_;

     CosSin( LoopN * T.U, cL, sL );
     CosSin( TwisN * T.U, cT, sT );
     CosSin(         T.V, cP, sP );

     TX := TwisR * cT;  PX := PipeR * cP;
     TY := TwisR * sT;  PY := PipeR * sP;

     R := LoopR * ( 1 + TX ) + PX  ;
     H := LoopR * (     TY   + PY );

     with Result do
     begin
          X := R * cL;
          Y := H     ;
          Z := R * sL;
     end;
end;

procedure TForm1.MakeShaper;
begin
     _Shaper := TGLShaperFace.Create( _Scener );

     with _Shaper do
     begin
          LoadFromFunc( BraidedTorus, 100-1, 100-1 );

          Matery := _Matery;
     end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
     _Scener := TGLScener.Create;

     _Camera := TGLCameraPers.Create( _Scener );

     with _Camera do
     begin
          Angl := DegToRad( 60{°} );

          Pose := TSingleM4.RotateX( DegToRad( -45 ) )
                * TSingleM4.Translate( 0, 0, +2 );
     end;

     GLViewer1.Camera := _Camera;

     _StoBuf0 := TGLStoBuf<Single>.Create( GL_STATIC_DRAW ); _StoBuf0.Count := 5;
     _StoBuf1 := TGLStoBuf<Single>.Create( GL_STATIC_DRAW ); _StoBuf1.Count := 5;

     _StoBuf0[ 0 ] := 0.5;


     MakeMatery;
     MakeShaper;
     MakeComput;

     _Comput.Run( 10, 10, 1 );

     _Matery.Imager.ReceData;

     _Matery.Imager.ExportTo( Image1.Bitmap );

     Memo1.Lines.Add( _StoBuf0[ 0 ].ToString );
     Memo1.Lines.Add( _StoBuf0[ 1 ].ToString );
     Memo1.Lines.Add( _StoBuf0[ 2 ].ToString );
     Memo1.Lines.Add( _StoBuf0[ 3 ].ToString );
     Memo1.Lines.Add( _StoBuf0[ 4 ].ToString );
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
     _StoBuf0.DisposeOf;
     _StoBuf1.DisposeOf;

     _Scener.DisposeOf;
end;

procedure TForm1.GLViewer1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
     _MouseS := Shift;
     _MouseP := TSingle2D.Create( X, Y );
end;

procedure TForm1.GLViewer1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
var
   P :TSingle2D;
begin
     if ssLeft in _MouseS then
     begin
          P := TSingle2D.Create( X, Y );

          _MouseA := _MouseA + ( P - _MouseP );

          _Shaper.Pose := TSingleM4.RotateX( DegToRad( _MouseA.Y ) )
                        * TSingleM4.RotateY( DegToRad( _MouseA.X ) );

          GLViewer1.Repaint;

          _MouseP := P;
     end;
end;

procedure TForm1.GLViewer1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
     GLViewer1MouseMove( Sender, Shift, X, Y );

     _MouseS := [];
end;

end. //######################################################################### ■
