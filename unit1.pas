unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  GR32_Image, math, GR32_ColorPicker;

type

Camera = record
X,Y,Z,Azimuth,Inclination: Real;
ViewWidth,ViewHeight:Real;
HalfWidth,HalfHeight:Real;//Must be precomputed to prevent recalculating over and over
	end;

Character = record
Health,X,Y,Z,Azimuth,Inclination,Height: Real;
end;

  { TForm1 }

  TForm1 = class(TForm)
    FPSLabel: TLabel;
    Screen: TImage;
    Timer1: TTimer;
    procedure ColorPickerRGBA1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure ResizeScreen;
    procedure DrawSimpleScene(Cam:Camera);
    procedure RaycastScene(Cam:Camera);
    procedure ScanlineRaycast(Cam:Camera);
    procedure InitialiseCamera(X:real;Y:real;Z:real;Azimuth:real;Inclination:real);
    function HorizonHeight(Cam:Camera):integer;
    procedure ScreenResize(Sender: TObject);
    function Calculate_Ray(Cam:Camera;Azimuth:Real;Inclination:Real):TColor;
    procedure Timer1Timer(Sender: TObject);
    function BlendCols(AR:integer;AG:integer;AB:integer;BR:integer;BG:integer;BB:integer;r:real):TColor;
    procedure MovePlayer;
  private

  public

  end;

var
     FogColour:TColor;
FogR:TColor;
FogG:TColor;
FogB:TColor;
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

////Implement ground as a list of layers of planes, where each plane has an associated rectangular boundary (objects and light can't collide with the plane outside the boundary)

implementation

{$R *.lfm}

{ TForm1 }
function TForm1.BlendCols(AR:integer;AG:integer;AB:integer;BR:integer;BG:integer;BB:integer;r:real):TColor;
begin
     result:=RGBToColor(round(AR*r+BR*(1-r)),round(AG*r+BG*(1-r)),round(AB*r+BB*(1-r)));
end;

procedure TForm1.InitialiseCamera(X:real;Y:real;Z:real;Azimuth:real;Inclination:real);
begin
MainCamera.X:=X;
MainCamera.Y:=Y;
MainCamera.Z:=Z;
MainCamera.Azimuth:=Azimuth;
MainCamera.Inclination:=Inclination;
MainCamera.ViewWidth:=0.8;
MainCamera.ViewHeight:=0.6;
MainCamera.HalfWidth:=0.4;
MainCamera.HalfHeight:=0.3;
end;



function TForm1.HorizonHeight(Cam:Camera):integer;
begin
result:=HalfHeight-round(Cam.Inclination/MainCamera.HalfHeight*HalfHeight) //horizon below centre
end;

procedure TForm1.ScreenResize(Sender: TObject);
begin
  Screen.Picture:=TPicture.Create;
    Screen.Picture.Bitmap.Width:=Screen.Width;
    Screen.Picture.Bitmap.Height:=Screen.Height;

end;

procedure TForm1.MovePlayer;
begin
    ///for now just move camera, eventually it will move player and there will be two cameras, one in front of player, and one that trails player third person
if movingforward=True then
begin
   MainCamera.X:=MainCamera.X+cos(MainCamera.Azimuth)*0.1;
   MainCamera.Z:=MainCamera.Z+sin(MainCamera.Azimuth)*0.1;
end
else if movingbackward=True then begin
   MainCamera.X:=MainCamera.X-cos(MainCamera.Azimuth)*0.1;
   MainCamera.Z:=MainCamera.Z-sin(MainCamera.Azimuth)*0.1
end;

//check for strafing, ignoring it if player is trying to strafe left and right at once
if (leftstrafe=True and rightstrafe=False) then begin
   MainCamera.X:=MainCamera.X+cos(MainCamera.Azimuth+halfpi)*0.1;
   MainCamera.Z:=MainCamera.Z+sin(MainCamera.Azimuth+halfpi)*0.1
end
else if (rightstrafe=True and leftStrafe=False)then begin
   MainCamera.X:=MainCamera.X+cos(MainCamera.Azimuth-halfpi)*0.1;
   MainCamera.Z:=MainCamera.Z+sin(MainCamera.Azimuth-halfpi)*0.1
end


end;

procedure TForm1.FormCreate(Sender: TObject);
begin
FogR:=128;
FogG:=128;
FogB:=128;
FogColour:=RGBToColor(FogR,FogG,FogB);
frame:=0;
PlayerHeight:=2;
InitialiseCamera(0,2,0,0.3,0.15);
ResizeScreen;
DrawSimpleScene(MainCamera);
halfpi:=pi/2;
tau:=pi*2;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin

if Key=87 then movingforward:=True;
if Key=83 then movingbackward:=True;
if Key=65 then leftstrafe:=True;
if Key=68 then rightstrafe:=True;


end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key=87 then movingforward:=False;
if Key=83 then movingbackward:=False;
if Key=65 then leftstrafe:=False;
if Key=68 then rightstrafe:=False;


end;

function TForm1.Calculate_Ray(Cam:Camera;Azimuth:Real;Inclination:Real):TColor;
var
  raylength:real;
  heightdifference:real;
  YintersectX:real;
    YintersectZ:real;
begin
heightdifference:=Cam.Y+PlayerHeight;//How far the camera is from the plane
raylength:=sqrt(sqr(heightdifference/math.tan(inclination))+sqr(heightdifference));
YintersectX:=Cam.X+cos(Azimuth)*raylength;
YintersectZ:=Cam.Z+sin(Azimuth)*raylength;
if inclination=0 then
result:=FogColour
else if inclination <0  then
   result:=BlendCols(0,255,0,FogR,FogG,FogB,(1-ArcTan(raylength/5)/halfpi))
else
result:=BlendCols(100,160,220,FogR,FogG,FogB,(1-ArcTan(raylength/5)/halfpi));
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  MovePlayer();
FPSLabel.caption:= inttostr(frame);
  frame:=frame+1;
  MainCamera.inclination:=sin(frame*0.1);
//  DrawSimpleScene(MainCamera);
RaycastScene(MainCamera);
end;

procedure TForm1.ColorPickerRGBA1Click(Sender: TObject);
begin

end;

procedure TForm1.FormResize(Sender: TObject);
begin
     ResizeScreen;

end;

procedure TForm1.RaycastScene(Cam:Camera);
var
    az,inc:real;
    scale:integer;
    i,j:integer;
    x,y:integer;
begin
scale:=6;//cells are 2x2
for j:= 0 to (Screen.Height div scale) do begin
     y:=Screen.Height-j*scale;
     inc:=Cam.Inclination+((j*scale)/HalfHeight-1)*Cam.HalfHeight;
     for i:= 0 to (Screen.Width div scale) do begin
            az:=Cam.Azimuth+((i*scale)/HalfWidth-1)*Cam.HalfWidth;
            x:=i*scale;

                Screen.Canvas.Brush.Color:=Calculate_Ray(Cam,az,inc);
                Screen.Canvas.FillRect(x,y,x+scale,y+scale);
          end;
       end;
end;

procedure TForm1.ScanlineRaycast(Cam:Camera);  /////////WORK HERE
var
    az,inc:real;
    scale:integer;
    i,j:integer;
    x,y:integer;
begin
scale:=6;//cells are 2x2
for j:= 0 to (Screen.Height div scale) do begin
     y:=Screen.Height-j*scale;
     inc:=Cam.Inclination+((j*scale)/HalfHeight-1)*Cam.HalfHeight;
     for i:= 0 to (Screen.Width div scale) do begin
            az:=Cam.Azimuth+((i*scale)/HalfWidth-1)*Cam.HalfWidth;
            x:=i*scale;

                Screen.Canvas.Brush.Color:=Calculate_Ray(Cam,az,inc);
                Screen.Canvas.FillRect(x,y,x+scale,y+scale);
          end;
       end;
end;

procedure TForm1.DrawSimpleScene(Cam:Camera);
var
     Middle:integer;
begin
     //calculate horizon_line
     Middle:=HorizonHeight(MainCamera);
     //draw horizon_line
     if Middle<0 then //ground only
             begin
                  Screen.Canvas.Brush.Color:=clGreen;
                  Screen.Canvas.FillRect(0,0,Screen.Width,Screen.Height);
             end
     else if Middle>=Screen.Height then
             begin
                Screen.Canvas.Brush.Color:=clBlue;
                Screen.Canvas.FillRect(0,0,Screen.Width,Screen.Height);
             end
     else
     begin
          Screen.Canvas.Brush.Color:=clBlue;
          Screen.Canvas.FillRect(0,0,Screen.Width,Middle);
          Screen.Canvas.Brush.Color:=clGreen;
          Screen.Canvas.FillRect(0,middle,Screen.Width,Screen.Height);
          Screen.Canvas.Pen.Color:=clWhite;
          Screen.Canvas.Line(0,Middle,Screen.Width,Middle);
     end;



end;

procedure TForm1.ResizeScreen;
begin
     Screen.Width:=Form1.Width-2;
     Screen.Height:=Form1.Height-16;
     HalfHeight:=Screen.Height div 2;
     HalfWidth:=Screen.Width div 2;
end;

end.

