unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  FPImage, GraphType, GR32_Image, math, GR32_ColorPicker, IntfGraphics;

type

Tvector2d = record
  X,Y:real;
end;

Camera = record
X,Y,Z,Azimuth,Inclination: Real;
ViewWidth,ViewHeight:Real;
HalfWidth,HalfHeight:Real;//Must be precomputed to prevent recalculating over and over
Front,Right: Tvector2d;
	end;

Character = record
Health,X,Y,Z,Azimuth,Inclination,Height: Real;
Front,Right: Tvector2d;
end;

ByteCol = record
R,G,B,Null:Byte;
end;


//ByteRow = array of ByteCol;


//Texture = record
//width:ShortInt;
//height:ShortInt;
//Rows:array of ByteRow;
//end;
Texture = record
width,height:Integer;
Pixels:array of array of ByteCol;
end;



  { TForm1 }

  TForm1 = class(TForm)
    FPSLabel: TLabel;
    GrassTexImage: TImage;
    HUD: TImage;
    Minimap: TImage;
    SkyBoxImage: TImage;
    Screen: TImage;
    Timer1: TTimer;
    function BitmapToTexture(inputimage:TBitmap):Texture;


function PixelToByteCol(incol:TColor):ByteCol;
    function vector2d(X,Y:real):Tvector2d;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure HUDClick(Sender: TObject);
    procedure ResizeScreen;
    procedure InitialiseCamera(X:real;Y:real;Z:real;Azimuth:real;Inclination:real);
    function HorizonHeight(Cam:Camera):integer;
    function Calculate_ByteColRay(Cam:Camera;Azimuth:Real;Inclination:Real):ByteCol;
    procedure SkyBoxImageClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ScanlineDraw(Cam:Camera);
    function BlendCols(AR:integer;AG:integer;AB:integer;BR:integer;BG:integer;BB:integer;r:real):TColor;
    function BlendByteCols(AR:integer;AG:integer;AB:integer;BR:integer;BG:integer;BB:integer;r:real):ByteCol;
    procedure MovePlayer;
    procedure MoveCamera;
    procedure CenterMouse;
    procedure IniEngine;
    procedure DrawMinimap;
    function wrap(x:real;denominator:real):real;

  private

  public
    MyBitmap: TBitmap;

    procedure PaintToRGB32bitScanLine(Row, ImgWidth,ImgHeight: integer; LineStart: Pointer; Cam:Camera);
  end;

var
     FogColour:TColor;
FogR:byte;
FogG:byte;
FogB:byte;
  Form1: TForm1;
  PlayerX: real;
  PlayerY: real;
  PlayerZ: real;
  PlayerHeight:real;
  halfpi,tau:real;
  PlayerAzimuth:real;
  PlayerInclination:real;
  MainCamera:Camera;
  HalfWidth:integer;
  HalfHeight:integer;
  frame:integer;
  movingforward:boolean;
  movingbackward:boolean;
  leftstrafe:boolean;
  rightstrafe:boolean;
  crouching:boolean;
  jumping:boolean;
  lookingleft:boolean;
  lookingright:boolean;
  lookingup:boolean;
  lookingdown:boolean;


  ///Efficient Drawing Vars
  xstep:real;
  ystep:real;
  GrassTexture:Texture;
  SkyTexture:Texture;


  ///
  MouseSensitivityX:Double;
  MouseSensitivityY:Double;



////Implement ground as a list of layers of planes, where each plane has an associated rectangular boundary (objects and light can't collide with the plane outside the boundary)

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.IniEngine;
begin
MouseSensitivityX:=0.01;
MouseSensitivityY:=0.005;
FogR:=8;   //set fog colour
FogG:=0;
FogB:=8;
FogColour:=RGBToColor(FogR,FogG,FogB);
frame:=0;
PlayerHeight:=2;
InitialiseCamera(0,2,0,0.3,0.15);
ResizeScreen;
//DrawSimpleScene(MainCamera);
halfpi:=pi/2;
tau:=pi*2;

//LoadTextures
GrassTexture:=BitmapToTexture(GrassTexImage.Picture.Bitmap);
SkyTexture:=BitmapToTexture(SkyBoxImage.Picture.Bitmap);
GrassTexImage.Destroy;
SkyBoxImage.Destroy;

//ScanlineDraw(MainCamera);
xstep:=0.01;
Timer1.Enabled:=True;
end;

function Tform1.wrap(x:real;denominator:real):real;
begin
if (x<denominator) and (x>0) then result:=x
else if x <0 then result :=wrap(x+denominator,denominator)
else result :=wrap(x-denominator,denominator)

end;


function TForm1.PixelToByteCol(incol:TColor):ByteCol;
var
outcol:bytecol;
tempcol:TFPColor;
  begin
tempcol:=TColorToFPColor(incol);
outcol.R:=tempcol.red;
outcol.G:=tempcol.green;
outcol.B:=tempcol.blue;
outcol.Null:=0;
result:=outcol
  end;

procedure TForm1.CenterMouse;
begin
Mouse.CursorPos:=Point(HalfWidth+Form1.Left,HalfHeight+Form1.Top);

end;

function TForm1.BitmapToTexture(inputimage:TBitmap):Texture;
var
  currentx:integer;
  currenty:integer;
  OutTex:texture;
  tempcol:TFPColor;
  tempBC:ByteCol;
  temprow:Array of ByteCol;
begin
OutTex.width:=inputimage.width;
OutTex.height:=inputimage.height;
tempBC:=PixelToByteCol(clWhite);
SetLength(OutTex.Pixels,inputimage.width);
for currentx := 0 to OutTex.width-1 do begin
     OutTex.Pixels[currentx]:=[TempBC];//init the array with a temp bytecol
     SetLength(OutTex.Pixels[currentx],inputimage.height);//allocate memory needed for the row
//     OutTex.Rows[0].Pixels:= Array[0..OutTex.height-1] of ByteCol;
//     system.SetLength(OutTex.Rows[0].Pixels,OutTex.width);
//     OutTex.Rows[currentrow]:=[tempBC];
       for currenty := 0 to OutTex.height-1 do begin
           OutTex.Pixels[currentx][currenty]:=PixelToByteCol(inputimage.Canvas.Pixels[currentx,currenty]);

//         temprow.length:=OutTex.width;
//          tempcol:=TColorToFPColor(inputimage.Canvas.Pixels[currentpixel,currentrow]);
//          tempBC.R:=tempcol.Red;
//          tempBC.G:=tempcol.Green;
//         tempBC.B:=tempcol.Blue;
//          temprow.Pixels[currentpixel]:=tempBC;
          end;

 end;
result:=OutTex;
end;





function TForm1.BlendCols(AR:integer;AG:integer;AB:integer;BR:integer;BG:integer;BB:integer;r:real):TColor;
begin
     result:=RGBToColor(round(AR*r+BR*(1-r)),round(AG*r+BG*(1-r)),round(AB*r+BB*(1-r)));
end;
function TForm1.BlendByteCols(AR:integer;AG:integer;AB:integer;BR:integer;BG:integer;BB:integer;r:real):ByteCol;
var
  outcol:ByteCol;
begin
     outcol.R:=round(AR*r+BR*(1-r));
     outcol.G:=round(AG*r+BG*(1-r));
     outcol.B:=round(AB*r+BB*(1-r));
     result:=outcol;
end;

procedure TForm1.ScanlineDraw(Cam:Camera);
var
  IntfImage: TLazIntfImage;
ScanLineImage: TLazIntfImage;
y: Integer;
ImgFormatDescription: TRawImageDescription;
begin
  ScanLineImage:=TLazIntfImage.Create(0,0);
  ImgFormatDescription.Init_BPP32_B8G8R8_BIO_TTB(Screen.Width,Screen.Height);
  ScanLineImage.DataDescription:=ImgFormatDescription;

  // call the pf24bit specific drawing function
  for y:=1 to ScanLineImage.Height do
    PaintToRGB32bitScanLine(ScanLineImage.Height-y,ScanLineImage.Width,ScanLineImage.Height,
                            ScanLineImage.GetDataLineStart(y-1),
                            MainCamera);

  // create IntfImage with the format of the current LCL interface
   IntfImage:=Screen.Picture.Bitmap.CreateIntfImage;
  IntfImage.CopyPixels(ScanLineImage);
  Screen.Picture.Bitmap.LoadFromIntfImage(IntfImage);
  ScanLineImage.Free;
  IntfImage.Free;
end;

procedure TForm1.InitialiseCamera(X:real;Y:real;Z:real;Azimuth:real;Inclination:real);
begin
MainCamera.X:=X;
MainCamera.Y:=Y;
MainCamera.Z:=Z;
MainCamera.Azimuth:=Azimuth;
MainCamera.Inclination:=Inclination;
MainCamera.ViewWidth:=0.6;
MainCamera.ViewHeight:=1;
MainCamera.HalfWidth:=0.3;
MainCamera.HalfHeight:=0.5;
MainCamera.Front:=vector2d(cos(Azimuth),sin(Azimuth));
MainCamera.Right:=vector2d(cos(Azimuth+halfpi),sin(Azimuth+halfpi));
end;

function Tform1.Vector2d(X,Y:real):Tvector2d;
var
outvector:Tvector2d;
begin
outvector.X:=X;
outvector.Y:=Y;
result:=outvector;
end;

procedure TForm1.PaintToRGB32bitScanLine(Row, ImgWidth,ImgHeight: integer;
  LineStart: Pointer; Cam:Camera );
// LineStart is pointer to the start of a scanline with the following format:
// 4 bytes per pixel. First byte is blue, second green, third is red.
// Black is 0,0,0, white is 255,255,255
var
   i: Integer;
   datalen:integer;
   inclination:real;
   azimuth:real;
   xr,yr:real;
   cellcol:ByteCol;
   azstep:real;
begin
i:=0;
datalen:= (ImgWidth*4)-1;
yr:=(row/HalfHeight-1);
azimuth:=Cam.Azimuth-Cam.HalfWidth;
azstep:=Cam.ViewWidth/ImgWidth;
inclination:=cam.inclination+yr*Cam.HalfHeight;
while (i<datalen) do begin
//xr:=xr+xstep;
azimuth:=azimuth+azstep;
cellcol:= Calculate_ByteColRay(Cam,azimuth,inclination);
PByte(LineStart)[i]:=cellcol.B;  Inc(i);
PByte(LineStart)[i]:=cellcol.G; Inc(i);
PByte(LineStart)[i]:=cellcol.R;   Inc(i);
PByte(LineStart)[i]:=0;Inc(i);
end;
end;

function TForm1.HorizonHeight(Cam:Camera):integer;
begin
result:=HalfHeight-round(Cam.Inclination/MainCamera.HalfHeight*HalfHeight) //horizon below centre
end;


procedure TForm1.MoveCamera;
begin
MainCamera.Azimuth:=MainCamera.Azimuth-(HalfWidth-(Mouse.CursorPos.X-Form1.Left))*MouseSensitivityX;
MainCamera.Inclination:=MainCamera.Inclination+(HalfHeight-(Mouse.CursorPos.Y-Form1.Top))*MouseSensitivityY;

if MainCamera.Azimuth>pi then MainCamera.Azimuth:=MainCamera.Azimuth-pi*2;
if MainCamera.Inclination<-pi*0.25 then MainCamera.Inclination:=-pi*0.25;

if MainCamera.Azimuth<-pi then MainCamera.Azimuth:=MainCamera.Azimuth+pi*2;
if MainCamera.Inclination>pi*0.25 then MainCamera.Inclination:=pi*0.25;
CenterMouse;
MainCamera.Front:=vector2d(cos(MainCamera.Azimuth),sin(MainCamera.Azimuth));
MainCamera.Right:=vector2d(cos(MainCamera.Azimuth+halfpi),sin(MainCamera.Azimuth+halfpi));
end;

procedure TForm1.MovePlayer;

var
     movespeed:real;
begin
movespeed:=0.2;
//2dvector type has attributes named X&Y but here the Y attribute represents the Z axis since we don't walk into the sky

///for now just move camera, eventually it will move player and there will be two cameras, one in front of player, and one that trails player third person
if movingforward=True then
begin
   MainCamera.X:=MainCamera.X+MainCamera.Front.X*movespeed;
   MainCamera.Z:=MainCamera.Z+MainCamera.Front.Y*movespeed;
end
else if movingbackward=True then begin
   MainCamera.X:=MainCamera.X-MainCamera.Front.X*movespeed;
   MainCamera.Z:=MainCamera.Z-MainCamera.Front.Y*movespeed;
end;

//check for strafing, ignoring it if player is trying to strafe left and right at once
if (leftstrafe=True) and (rightstrafe=False) then begin
   MainCamera.X:=MainCamera.X-MainCamera.Right.X*movespeed;
   MainCamera.Z:=MainCamera.Z-MainCamera.Right.Y*movespeed;
end
else if (rightstrafe=True) and (leftStrafe=False)then begin
   MainCamera.X:=MainCamera.X+MainCamera.Right.X*movespeed;
   MainCamera.Z:=MainCamera.Z+MainCamera.Right.Y*movespeed;
end


end;

procedure TForm1.FormCreate(Sender: TObject);
begin
IniEngine;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin

if Key=87 then movingforward:=True
else if Key=83 then movingbackward:=True;

if Key=65 then leftstrafe:=True
else if Key=68 then rightstrafe:=True;


end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key=87 then movingforward:=False;
if Key=83 then movingbackward:=False;
if Key=65 then leftstrafe:=False;
if Key=68 then rightstrafe:=False;


end;


function TForm1.Calculate_ByteColRay(Cam:Camera;Azimuth:Real;Inclination:Real):ByteCol;
var
  raylength:real;
  heightdifference:real;
  YintersectX:real;
    YintersectZ:real;
    outcol:ByteCol;
   texcol:ByteCol;
begin
heightdifference:=2;//Cam.Y+PlayerHeight;//How far the camera is from the Ground
if not(inclination=0) then begin
raylength:=sqrt(sqr(heightdifference/math.tan(inclination))+sqr(heightdifference));
YintersectX:=Cam.X+cos(Azimuth)*raylength;
YintersectZ:=Cam.Z+sin(Azimuth)*raylength;
end;

if abs(inclination)<0.03 then begin
outcol.R:=FogR;
outcol.G:=FogG;
outcol.B:=FogB;
result:=outcol;
end
else if inclination <0  then  begin
   texcol:=GrassTexture.Pixels[floor(wrap(YintersectX*250,GrassTexture.width))][floor(wrap(YintersectZ*250,GrassTexture.height))];
   result:=BlendByteCols(texcol.R,texcol.G,texcol.B,FogR,FogG,FogB,(1-ArcTan(raylength/2)/halfpi))
end
else begin
texcol:=SkyTexture.Pixels[floor(wrap(azimuth,tau)/tau*SkyTexture.width)][floor(wrap(inclination,tau)/tau*SkyTexture.height)];
result:=BlendByteCols(texcol.R,texcol.G,texcol.B,FogR,FogG,FogB,(1-ArcTan(raylength/2)/halfpi));
end
end;

procedure TForm1.SkyBoxImageClick(Sender: TObject);
begin

end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  MovePlayer;
  MoveCamera;
FPSLabel.caption:= inttostr(frame)+'  '+FloatToStr(MainCamera.inclination)+' , '+FloatToStr(MainCamera.azimuth)+' Direction: ('+floattostr(MainCamera.Front.X)+', '+floattostr(MainCamera.Front.Y)+')';
  frame:=frame+1;
ScanlineDraw(MainCamera);
end;

procedure TForm1.FormResize(Sender: TObject);
begin
     ResizeScreen;

end;

procedure TForm1.DrawMinimap;
begin
     Minimap.Canvas.Ellipse(96,94,100,98);

end;

procedure TForm1.HUDClick(Sender: TObject);
begin
  ShowMessage('testing');

end;

procedure TForm1.ResizeScreen;
begin

     Screen.Width:=Form1.Width-2;
     Screen.Height:=Form1.Height-120;
     HalfHeight:=Screen.Height div 2;
     HalfWidth:=Screen.Width div 2;
     xstep:=2/Screen.Width;
     ystep:=2/Screen.Height;
     Screen.Picture:=TPicture.Create;
  Screen.Picture.Bitmap.Width:=Screen.Width;
   Screen.Picture.Bitmap.Height:=Screen.Height;
   HUD.Top:=Form1.Height-120;
end;

end.

