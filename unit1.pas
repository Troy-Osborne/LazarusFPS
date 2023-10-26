unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  FPImage, GraphType, GR32_Image, math, GR32_ColorPicker, IntfGraphics;
//Some of these are from legacy versions, and no longer used, will be cleaned soon.

type

Tvector2d = record
  X,Y:real;
end;

Camera = record
X,Y,Z,Azimuth,Inclination: Real;
ViewWidth,ViewHeight:Real;
HalfWidth,HalfHeight:Real;//Must be precomputed to prevent recalculating over and over each frame
Front,Right: Tvector2d;
	end;

Character = record
Health,X,Y,Z,Azimuth,Inclination,Height: Real;
POV:Camera;
Front,Right: Tvector2d;
crouching:boolean;
jumping:boolean;
movingforward:boolean;
movingbackward:boolean;
leftstrafe:boolean;
rightstrafe:boolean;
InAir:boolean;
jumpvelocity:real;

end;

ByteCol = record
R,G,B,Null:Byte;
end;

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
    procedure UpdateView;
    procedure CenterMouse;
    procedure IniEngine;
    procedure IniPlayer;
    procedure DrawMinimap;
    procedure UpdatePOV;
    function FogLevel(distance:real):real;
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
  halfpi,tau:real;
  HalfWidth:integer;
  HalfHeight:integer;
  frame:integer;
  SinglePlayer:Character;
  AmbientLight:real;//from 0 to 1
  FogThickness:real;//from 0 to 1


  ///Efficient Drawing Vars
  xstep:real;
  ystep:real;
  GrassTexture:Texture;
  SkyTexture:Texture;


  //Mouse Movement
  MouseSensitivityX:Double;
  MouseSensitivityY:Double;



////Implement ground as a list of layers of planes,
//where each plane has an associated rectangular boundary 
//So objects and light can't collide with the plane outside the boundary
//This will allow for multiple heights in the terrain

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.IniPlayer;
begin
SinglePlayer.crouching:=False;
SinglePlayer.jumping:=False;
SinglePlayer.leftstrafe:=False;
SinglePlayer.rightstrafe:=False;
SinglePlayer.movingbackward:=False;
SinglePlayer.movingforward:=False;
SinglePlayer.jumping:=False;
SinglePlayer.crouching:=False;
SinglePlayer.Azimuth:=0;
SinglePlayer.Inclination:=0;
SinglePlayer.Health:=100;
SinglePlayer.Height:=2;
SinglePlayer.X:=0;
SinglePlayer.Y:=0;
SinglePlayer.Z:=0;
SinglePlayer.InAir:=False;
SinglePlayer.jumpvelocity:=0;
end;

procedure TForm1.IniEngine;
begin
MouseSensitivityX:=0.01;
MouseSensitivityY:=0.005;
FogR:=8;   //set fog colour
FogG:=0;
FogB:=8;
AmbientLight:=0.2;
FogThickness:=0.5;
FogColour:=RGBToColor(FogR,FogG,FogB);
frame:=0;
InitialiseCamera(0,0,0,0.3,0.15);
ResizeScreen;
//DrawSimpleScene(SinglePlayer);
halfpi:=pi/2;
tau:=pi*2;

//LoadTextures
GrassTexture:=BitmapToTexture(GrassTexImage.Picture.Bitmap);
SkyTexture:=BitmapToTexture(SkyBoxImage.Picture.Bitmap);
GrassTexImage.Destroy;
SkyBoxImage.Destroy;

//ScanlineDraw(SinglePlayer);
xstep:=0.01;
Timer1.Enabled:=True;
end;

function Tform1.wrap(x:real;denominator:real):real; //An alternative to mod which always returns a positive number to ensure textures are tiled correctly
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
//Return the mouse to the center of the screen
Mouse.CursorPos:=Point(HalfWidth+Form1.Left,HalfHeight+Form1.Top);
end;

function TForm1.FogLevel(distance:real):real;
begin
//This will be updated to depend on fog thickness and ambient light, for the time being fog thickness is constant depending only on the distance of the rendered point
result:= 1-ArcTan(distance/2)/halfpi;
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
SetLength(OutTex.Pixels,inputimage.width); //Allocate the number of columns (array of bytecol) which appear in the texture
for currentx := 0 to OutTex.width-1 do begin
     OutTex.Pixels[currentx]:=[TempBC];//init the array with a temp bytecol
     SetLength(OutTex.Pixels[currentx],inputimage.height);//allocate memory needed for the row
       for currenty := 0 to OutTex.height-1 do begin
//copy pixels from the TBitmap.Canvas into the Texture type's 2d array of ByteCols (A custom record)
           OutTex.Pixels[currentx][currenty]:=PixelToByteCol(inputimage.Canvas.Pixels[currentx,currenty]);
          end;

 end;
result:=OutTex;
end;


function TForm1.BlendCols(AR:integer;AG:integer;AB:integer;BR:integer;BG:integer;BB:integer;r:real):TColor;
begin
//Legacy Version which outputs a TColor, no longer used.
//See BlendByteCols below for the current version
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
                            SinglePlayer.POV);

  // create IntfImage with the format of the current LCL interface
   IntfImage:=Screen.Picture.Bitmap.CreateIntfImage;
  IntfImage.CopyPixels(ScanLineImage);
  Screen.Picture.Bitmap.LoadFromIntfImage(IntfImage);
  ScanLineImage.Free;
  IntfImage.Free;
end;

procedure TForm1.InitialiseCamera(X:real;Y:real;Z:real;Azimuth:real;Inclination:real);
begin
SinglePlayer.POV.X:=X;
SinglePlayer.POV.Y:=Y;
SinglePlayer.POV.Z:=Z;
SinglePlayer.POV.Azimuth:=Azimuth;
SinglePlayer.POV.Inclination:=Inclination;
SinglePlayer.POV.ViewWidth:=0.6;
SinglePlayer.POV.ViewHeight:=1;
SinglePlayer.POV.HalfWidth:=0.3;
SinglePlayer.POV.HalfHeight:=0.5;
SinglePlayer.POV.Front:=vector2d(cos(Azimuth),sin(Azimuth));
SinglePlayer.POV.Right:=vector2d(cos(Azimuth+halfpi),sin(Azimuth+halfpi));
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
result:=HalfHeight-round(Cam.Inclination/Cam.HalfHeight*HalfHeight) //horizon below centre
end;


procedure TForm1.UpdateView;
begin
SinglePlayer.Azimuth:=SinglePlayer.Azimuth-(HalfWidth-(Mouse.CursorPos.X-Form1.Left))*MouseSensitivityX;
SinglePlayer.Inclination:=SinglePlayer.Inclination+(HalfHeight-(Mouse.CursorPos.Y-Form1.Top))*MouseSensitivityY;

if SinglePlayer.Azimuth>pi then SinglePlayer.Azimuth:=SinglePlayer.Azimuth-pi*2;
if SinglePlayer.Inclination<-pi*0.25 then SinglePlayer.Inclination:=-pi*0.25;

if SinglePlayer.Azimuth<-pi then SinglePlayer.Azimuth:=SinglePlayer.Azimuth+pi*2;
if SinglePlayer.Inclination>pi*0.25 then SinglePlayer.Inclination:=pi*0.25;
CenterMouse;
SinglePlayer.Front:=vector2d(cos(SinglePlayer.Azimuth),sin(SinglePlayer.Azimuth));
SinglePlayer.Right:=vector2d(cos(SinglePlayer.Azimuth+halfpi),sin(SinglePlayer.Azimuth+halfpi));
UpdatePOV;

end;

procedure TForm1.UpdatePOV;
begin
//Update POV Cam
SinglePlayer.POV.Azimuth:=SinglePlayer.Azimuth;
SinglePlayer.POV.Inclination:=SinglePlayer.Inclination;
SinglePlayer.POV.Front:=SinglePlayer.Front;
SinglePlayer.POV.Right:=SinglePlayer.Right;
end;

procedure TForm1.MovePlayer;

var
     movespeed:real;
begin
movespeed:=0.2;

if SinglePlayer.movingforward=True then
begin
   SinglePlayer.X:=SinglePlayer.X+SinglePlayer.Front.X*movespeed;
   SinglePlayer.Z:=SinglePlayer.Z+SinglePlayer.Front.Y*movespeed;
end
else if SinglePlayer.movingbackward=True then begin
   SinglePlayer.X:=SinglePlayer.X-SinglePlayer.Front.X*movespeed;
   SinglePlayer.Z:=SinglePlayer.Z-SinglePlayer.Front.Y*movespeed;
end;

//check for strafing, ignoring it if player is trying to strafe left and right at once
if (SinglePlayer.leftstrafe=True) and (SinglePlayer.rightstrafe=False) then begin
   SinglePlayer.X:=SinglePlayer.X-SinglePlayer.Right.X*movespeed;
   SinglePlayer.Z:=SinglePlayer.Z-SinglePlayer.Right.Y*movespeed;
end
else if (SinglePlayer.rightstrafe=True) and (SinglePlayer.leftStrafe=False)then begin
   SinglePlayer.X:=SinglePlayer.X+SinglePlayer.Right.X*movespeed;
   SinglePlayer.Z:=SinglePlayer.Z+SinglePlayer.Right.Y*movespeed;
end;

if SinglePlayer.jumping and (not SinglePlayer.InAir) then begin
   SinglePlayer.InAir:=True;
   SinglePlayer.jumpvelocity:=1;
end;

//If player is in the air keep dropping jump velocity until player hits the ground
if (SinglePlayer.InAir) then begin
SinglePlayer.jumpvelocity:= SinglePlayer.jumpvelocity-0.2;
SinglePlayer.Y:= SinglePlayer.Y+SinglePlayer.jumpvelocity;
   end;
//if player is on or below the ground then.
if SinglePlayer.Y<=0 then begin
   SinglePlayer.InAir:=False;
   SinglePlayer.Y:=0;
end;
//After Updating the Player Position update the First Person Perspective Camera
//Eventually a third person trailing camera will also be added here

SinglePlayer.POV.X:=SinglePlayer.X;
SinglePlayer.POV.Z:=SinglePlayer.Z;
SinglePlayer.POV.Y:=SinglePlayer.Y+SinglePlayer.Height;

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
IniEngine;
IniPlayer;
//

end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin

if Key=87 then SinglePlayer.movingforward:=True
else if Key=83 then SinglePlayer.movingbackward:=True;

if Key=65 then SinglePlayer.leftstrafe:=True
else if Key=68 then SinglePlayer.rightstrafe:=True;

If Key=32 then SinglePlayer.jumping:=True;


end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
//process movement
if Key=87 then SinglePlayer.movingforward:=False;
if Key=83 then SinglePlayer.movingbackward:=False;
if Key=65 then SinglePlayer.leftstrafe:=False;
if Key=68 then SinglePlayer.rightstrafe:=False;
If Key=32 then SinglePlayer.jumping:=False;

//process crouch


//process jump


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
heightdifference:=SinglePlayer.Y+SinglePlayer.Height;//Cam.Y+PlayerHeight;//How far the camera is from the Ground
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
   result:=BlendByteCols(texcol.R,texcol.G,texcol.B,FogR,FogG,FogB,1-ArcTan(raylength/2)/halfpi)
end
else begin
texcol:=SkyTexture.Pixels[floor(wrap(azimuth,tau)/tau*SkyTexture.width)][floor(wrap(inclination,tau)/tau*SkyTexture.height)];
result:=BlendByteCols(texcol.R,texcol.G,texcol.B,FogR,FogG,FogB,1-ArcTan(raylength/2)/halfpi);
end
end;

procedure TForm1.SkyBoxImageClick(Sender: TObject);
begin

end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  MovePlayer;
  UpdateView;
FPSLabel.caption:= inttostr(frame)+'  '+FloatToStr(SinglePlayer.POV.inclination)+' , '+FloatToStr(SinglePlayer.azimuth)+' Direction: ('+floattostr(SinglePlayer.Front.X)+', '+floattostr(SinglePlayer.Front.Y)+')';
  frame:=frame+1;
ScanlineDraw(SinglePlayer.POV);
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
