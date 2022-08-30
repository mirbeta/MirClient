{********************************************************************}
{ TFORMSHAPE component                                               }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ written by                                                         }
{                                                                    }
{   TMS Software                                                     }
{   enhanced by Brian Shepherd                                       }
{   copyright © 1998-2012                                            }
{   Email : info@tmssoftware.com                                     }
{   Web : http://www.tmssoftware.com                                 }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The component can be freely used in any application. The source    }
{ code remains property of the author and may not be distributed     }
{ freely as such.                                                    }
{********************************************************************}

unit frmshape;

{$I TMSDEFS.INC}
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Types;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 3; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 1; // Build nr.

  // version history
  // v1.0.0.0 : First release
  // v1.3.0.1 : Fixed : Issue with Win64 and Windows 8

type
  EFormShapeError = class(Exception);

  TBalloonPosition = (bpTopLeft,bpLeftTop,bpTopRight,bpRightTop,
                      bpBottomLeft,bpLeftBottom,bpBottomRight,bpRightBottom,bpNone);

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TFormshape = class(TComponent)
  private
    { Private declarations }
    FReshape:boolean;
    FOnPaint:TNotifyEvent;
    FOnPaintAssigned:boolean;
    FBalloonPosition:TBalloonPosition;
    FBalloonEllips:integer;
    FBalloonIndent:integer;
    FBalloonColor:Tcolor;
    FBorderColor:TColor;
    FBorderWidth:integer;
    OldWndProc:TFarProc;
    NewWndProc:Pointer;
    FDragBalloon:boolean;
    procedure PaintForm(sender:tobject);
    procedure SetBalloonIndent(avalue:integer);
    procedure SetBalloonEllips(avalue:integer);
    {New procedures}
    procedure SetBalloonColor(avalue:TColor);
    procedure SetBorderColor(avalue:TColor);
    procedure SetBorderWidth(avalue:integer);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
  protected
    { Protected declarations }
    function GetVersionNr: Integer;
    procedure Loaded; override;
    procedure HookWndProc(var Msg: TMessage);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property Reshape:boolean read FReshape write FReshape;
    property BalloonPosition:TBalloonposition read FBalloonPosition write FBalloonPosition;
    property BalloonEllips:integer read FBalloonEllips write SetBalloonEllips;
    property BalloonIndent:integer read FBalloonIndent write SetBalloonIndent;
    {New Properties}
    property BalloonColor:TColor read FBalloonColor write SetBalloonColor;
    property BorderColor:TColor read FBorderColor write SetBorderColor;
    property BorderWidth:integer read FBorderWidth write SetBorderWidth;
    property DragBalloon:boolean read FDragBalloon write FDragBalloon;
    property Version: string read GetVersion write SetVersion;
  end;


implementation

type
  {$IFDEF DELPHIXE_LVL}
  LInteger = LONG_PTR;
  LIntParam = LPARAM;
  {$ENDIF}
  {$IFNDEF DELPHIXE_LVL}
  LInteger = Integer;
  LIntParam = Integer;
  {$ENDIF}


constructor TFormshape.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FBalloonIndent:=15;
  FBalloonEllips:=20;
  {new attributes}
  FBalloonColor := clWhite;
  FBorderColor  := clBlack;
  FBorderWidth  :=1;

  if not (Owner is TForm) then
    raise EFormShapeError.Create('Control parent must be a form!');

  if (csDesigning in ComponentState) then Exit;
end;

destructor TFormShape.Destroy;
begin
 if not (csDesigning in ComponentState) then
  begin
  { UnHook parent }
   if (Owner <> nil) and Assigned(OldWndProc) then
  {$IFDEF DELPHI_UNICODE}
      SetWindowLongPtr((Owner as TForm).Handle, GWL_WNDPROC, LInteger(OldWndProc));
  {$ENDIF}
  {$IFNDEF DELPHI_UNICODE}
      SetWindowLong((Owner as TForm).Handle, GWL_WNDPROC, LInteger(OldWndProc));
  {$ENDIF}
   if Assigned(NewWndProc) then FreeObjectInstance(NewWndProc);
  end;

 inherited;
end;

procedure TFormShape.SetBalloonIndent(avalue:integer);
begin
 if (avalue>0) then FBalloonIndent:=avalue;
end;

procedure TFormShape.SetBalloonEllips(avalue:integer);
begin
 if (avalue>0) then FBalloonEllips:=avalue;
end;

procedure TFormShape.SetBalloonColor(avalue:TColor);
begin
  FBalloonColor := avalue;
end;

procedure TFormShape.SetBorderColor(avalue:TColor);
begin
  FBorderColor := avalue;
end;

procedure TFormShape.SetBorderWidth(avalue:integer);
begin
 if (avalue >0) then FBorderWidth := avalue;
end;
{---------------------------------------------}

procedure TFormShape.Loaded;
var
  hrgn1: THandle;
  hrgn2: THandle;
  hrgn: THandle;
  triangle:array[0..2] of tpoint;
  i: Integer;

begin
  inherited Loaded;

  if not freshape then Exit;
  if (csDesigning in ComponentState) then Exit;

  with (Owner as TForm) do
  begin
    FOnPaintAssigned:=false;
    if (assigned(OnPaint)) then
    begin
      FOnPaint := OnPaint;
      FOnPaintAssigned := true;
    end;
    OnPaint := PaintForm;
  end;

  with (Owner as TForm) do
  begin
    borderstyle:=bsNone;

    Height:=Height-GetSystemMetrics(SM_CYCAPTION);

    for i := 1 to ComponentCount do
    begin
      if (Components[i-1] is TControl) then
        if (Components[i-1] as TControl).Parent = Owner then
          (Components[i-1] as TControl).Top := (Components[i-1] as TControl).Top+GetSystemMetrics(SM_CYCAPTION);
    end;

    hrgn := CreateRectRgn(0,0,clientrect.right,clientrect.bottom);

    case BalloonPosition of
    bpNone:hrgn1:=CreateRoundRectRgn(0,0,clientrect.right,clientrect.bottom,balloonellips,balloonellips);
    bpTopLeft,bpTopRight:hrgn1:=CreateRoundRectRgn(0,balloonindent,clientrect.right,clientrect.bottom,balloonellips,balloonellips);
    bpLeftTop,bpLeftBottom:hrgn1:=CreateRoundRectRgn(balloonindent,0,clientrect.right,clientrect.bottom,balloonellips,balloonellips);
    bpRightTop,bpRightBottom:hrgn1:=CreateRoundRectRgn(0,0,clientrect.right-balloonindent,clientrect.bottom,balloonellips,balloonellips);
    bpBottomLeft,bpBottomRight:hrgn1:=CreateRoundRectRgn(0,0,clientrect.right,clientrect.bottom-balloonindent,balloonellips,balloonellips);
    else
      hrgn1:=CreateRoundRectRgn(0,0,clientrect.right,clientrect.bottom,balloonellips,balloonellips);
    end;

    case BalloonPosition of
    bpTopLeft:
      begin
        triangle[0] := Point(balloonellips,0);
        triangle[1] := Point(balloonellips,balloonindent);
        triangle[2] := Point(balloonellips+balloonindent,balloonindent);
      end;
    bpTopRight:begin
                 triangle[0] := Point(clientrect.right-balloonellips,0);
                 triangle[1] := Point(clientrect.right-balloonellips,balloonindent);
                 triangle[2] := Point(clientrect.right-(balloonellips+balloonindent),balloonindent);
               end;
    bpBottomLeft:begin
                 triangle[0] := Point(balloonellips,clientrect.bottom);
                 triangle[1] := Point(balloonellips,clientrect.bottom-balloonindent-1);
                 triangle[2] := Point(balloonellips+balloonindent,clientrect.bottom-balloonindent-1);
              end;
    bpBottomRight:begin
                 triangle[0] := Point(clientrect.right-balloonellips,clientrect.bottom);
                 triangle[1] := Point(clientrect.right-balloonellips,clientrect.bottom-balloonindent-1);
                 triangle[2] := Point(clientrect.right-(balloonellips+balloonindent),clientrect.bottom-balloonindent-1);
               end;
    bpLeftTop:begin
                 triangle[0] := Point(0,balloonellips);
                 triangle[1] := Point(balloonindent,balloonellips);
                 triangle[2] := Point(balloonindent,balloonellips+balloonindent);
              end;
    bpLeftBottom:begin
                 triangle[0] := Point(0,clientrect.bottom-balloonellips);
                 triangle[1] := Point(balloonindent,clientrect.bottom-balloonellips);
                 triangle[2] := Point(balloonindent,clientrect.bottom-(balloonellips+balloonindent));
                 end;
    bpRightTop:begin
                 triangle[0] := Point(clientrect.right,balloonellips);
                 triangle[1] := Point(clientrect.right-balloonindent-1,balloonellips);
                 triangle[2] := Point(clientrect.right-balloonindent-1,balloonellips+balloonindent);
              end;
    bpRightBottom:begin
                 triangle[0] := Point(clientrect.right,clientrect.bottom-balloonellips);
                 triangle[1] := Point(clientrect.right-balloonindent-1,clientrect.bottom-balloonellips);
                 triangle[2] := Point(clientrect.right-balloonindent-1,clientrect.bottom-(balloonellips+balloonindent));
                 end;
    end;

    if BalloonPosition<>bpNone then
    begin
      hrgn2 := CreatePolygonRgn(triangle,3,WINDING);
      CombineRgn(hrgn,hrgn1,hrgn2,RGN_OR);
      SetWindowRgn(handle,hrgn,true);
      DeleteObject(hrgn2);
    end
    else
      SetWindowRgn(handle,hrgn1,true);

    DeleteObject(hrgn);
    DeleteObject(hrgn1);
  end;

   { Hook parent }
  {$IFDEF DELPHI_UNICODE}
  OldWndProc := TFarProc(GetWindowLongPtr((Owner as TForm).Handle, GWL_WNDPROC));
  {$ENDIF}
  {$IFNDEF DELPHI_UNICODE}
  OldWndProc := TFarProc(GetWindowLong((Owner as TForm).Handle, GWL_WNDPROC));
  {$ENDIF}

  NewWndProc := MakeObjectInstance(HookWndProc);

  {$IFDEF DELPHI_UNICODE}
  SetWindowLongPtr((Owner as TForm).Handle, GWL_WNDPROC, LInteger(NewWndProc));
  {$ENDIF}
  {$IFNDEF DELPHI_UNICODE}
  SetWindowLong((Owner as TForm).Handle, GWL_WNDPROC, LInteger(NewWndProc));
  {$ENDIF}
end;

procedure TFormShape.PaintForm(Sender: TObject);
var
 triangle:array[0..2] of tpoint;
 r:trect;

begin
 with (Owner as TForm) do
  begin
  {Code changes}
   canvas.Pen.Color:=FBorderColor;
   canvas.Pen.Width:=FBorderWidth;
   canvas.Brush.Color:=ColorToRGB(FBalloonColor);
   {-----------------------------------}
   case BalloonPosition of
   bpNone:RoundRect(canvas.handle,0,0,clientrect.right-1,clientrect.bottom-1,balloonellips,balloonellips);
   bpTopLeft,bpTopRight:RoundRect(canvas.handle,0,balloonindent,clientrect.right-1,clientrect.bottom-1,balloonellips,balloonellips);
   bpLeftTop,bpLeftBottom:RoundRect(canvas.handle,balloonindent,0,clientrect.right-1,clientrect.bottom-1,balloonellips,balloonellips);
   bpRightTop,bpRightBottom:RoundRect(canvas.handle,0,0,clientrect.right-balloonindent-1,clientrect.bottom-1,balloonellips,balloonellips);
   bpBottomLeft,bpBottomRight:RoundRect(canvas.handle,0,0,clientrect.right-1,clientrect.bottom-balloonindent-1,balloonellips,balloonellips);
   end;

   case BalloonPosition of
   bpTopLeft:begin
                triangle[0]:=point(balloonellips,1);
                triangle[1]:=point(balloonellips,balloonindent);
                triangle[2]:=point(balloonellips+balloonindent-1,balloonindent);
             end;
   bpTopRight:begin
                triangle[0]:=point(clientrect.right-balloonellips-1,1);
                triangle[1]:=point(clientrect.right-balloonellips-1,balloonindent);
                triangle[2]:=point(clientrect.right-(balloonellips+balloonindent),balloonindent);
              end;
   bpBottomLeft:begin
                 triangle[0]:=point(balloonellips,clientrect.bottom-1);
                 triangle[1]:=point(balloonellips,clientrect.bottom-balloonindent-2);
                 triangle[2]:=point(balloonellips+balloonindent,clientrect.bottom-balloonindent-2);
                end;
   bpBottomRight:begin
                  triangle[0]:=point(clientrect.right-balloonellips-1,clientrect.bottom-1);
                  triangle[1]:=point(clientrect.right-balloonellips-1,clientrect.bottom-balloonindent-2);
                  triangle[2]:=point(clientrect.right-(balloonellips+balloonindent)+1,clientrect.bottom-balloonindent-2);
                 end;
   bpLeftTop:begin
              triangle[0]:=point(0,balloonellips);
              triangle[1]:=point(balloonindent,balloonellips);
              triangle[2]:=point(balloonindent,balloonellips+balloonindent);
             end;
   bpLeftBottom:begin
                 triangle[0]:=point(1,clientrect.bottom-balloonellips-1);
                 triangle[1]:=point(balloonindent,clientrect.bottom-balloonellips-1);
                 triangle[2]:=point(balloonindent,clientrect.bottom-(balloonellips+balloonindent));
                end;
   bpRightTop:begin
               triangle[0]:=point(clientrect.right-1,balloonellips);
               triangle[1]:=point(clientrect.right-balloonindent-2,balloonellips);
               triangle[2]:=point(clientrect.right-balloonindent-2,balloonellips+balloonindent);
              end;
   bpRightBottom:begin
                  triangle[0]:=point(clientrect.right-2,clientrect.bottom-balloonellips-1);
                  triangle[1]:=point(clientrect.right-balloonindent-2,clientrect.bottom-balloonellips-1);
                  triangle[2]:=point(clientrect.right-balloonindent-2,clientrect.bottom-(balloonellips+balloonindent));
                 end;
   end;

    if BalloonPosition<>bpNone then
    begin
      Canvas.Polygon(triangle);
      Canvas.Pen.Color := BalloonColor;

      case BalloonPosition of
      bpTopLeft:r:=rect(balloonellips,balloonindent,balloonellips+balloonindent-1,balloonindent);
      bpTopRight:r:=rect(clientrect.right-balloonellips,balloonindent,clientrect.right-(balloonellips+balloonindent)+1,balloonindent);
      bpLeftTop:r:=rect(balloonindent,balloonellips,balloonindent,balloonindent+balloonellips-1);
      bpLeftBottom:r:=rect(balloonindent,clientrect.bottom-balloonellips-1,balloonindent,clientrect.bottom-(balloonindent+balloonellips));
      bpRightTop:r:=rect(clientrect.right-balloonindent-2,balloonellips,clientrect.right-balloonindent-2,balloonindent+balloonellips);
      bpRightBottom:r:=rect(clientrect.right-balloonindent-2,clientrect.bottom-balloonellips-1,clientrect.right-balloonindent-2,clientrect.bottom-(balloonindent+balloonellips));
      bpBottomLeft:r:=rect(balloonellips,clientrect.bottom-balloonindent-2,balloonellips+balloonindent-1,clientrect.bottom-balloonindent-2);
      bpBottomRight:r:=rect(clientrect.right-balloonellips,clientrect.bottom-balloonindent-2,clientrect.right-(balloonellips+balloonindent)+1,clientrect.bottom-balloonindent-2);
      end;

      canvas.moveto(r.right,r.top);
      canvas.lineto(r.left,r.bottom);
    end;

  end;

  //call existing handler
  if (FOnPaintAssigned) then
    FOnPaint(sender);
end;


procedure TFormshape.HookWndProc(var Msg: TMessage);
begin

 Msg.Result:=CallWindowProc(OldWndProc, (Owner as TForm).Handle, Msg.Msg , Msg.wParam, Msg.lParam);

 case Msg.Msg of
 WM_NCHITTEST:begin
               if fDragBalloon then msg.result:=HTCAPTION;
              end;
 end;
end;

function TFormShape.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TFormShape.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TFormShape.SetVersion(const Value: string);
begin

end;

end.
