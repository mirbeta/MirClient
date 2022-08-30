{***************************************************************************}
{ TAdvMemo component                                                        }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2001 - 2013                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of TMS software.                                    }
{***************************************************************************}
{$I TMSDEFS.INC}

unit AdvCodeHint;

interface

uses
  Windows, Classes, Controls, Forms, Messages, Graphics, SysUtils, AdvCodeListLib,
  Advmemo, Types;

const
  HintLimit = 15;

type
  TCustomHintWindow = class(THintWindow)
  private
    FRegion:THandle;
    procedure FreeCurrentRegion;
  protected
    procedure Paint;override;
    procedure WndProc(var Message: TMessage); override;
    procedure CreateParams(var Params:TCreateParams);override;
  public
    destructor Destroy;override;
    procedure ActivateHint(Rect:TRect;const AHint:string);override;
  end;

procedure SetActiveStyler(AStyler: TAdvCustomMemoStyler);

implementation

uses
  AdvCodeList;

var
  styler: TAdvCustomMemoStyler;

procedure SetActiveStyler(AStyler: TAdvCustomMemoStyler);
begin
  styler := AStyler;
end;

function VarPos(sub,s:string; var vp: Integer): Integer;
begin
  vp := pos(sub,s);
  Result := vp;
end;

procedure StringToStringList(s:string; st: tstringlist);
var
  vp: Integer;
begin
  while varpos(#13#10, s, vp) > 0 do
  begin
    ST.Add(copy(s,1,vp-1));
    delete(s,1,vp+1);
  end;
  if length(s) > 0 then
    ST.Add(s);
end;

destructor TCustomHintWindow.Destroy;
begin
  inherited;
end;

procedure TCustomHintWindow.FreeCurrentRegion;
{Regions,like other API objects,should be freed when you are }
{through using them.Note,however,that you cannot delete a }
{region which is currently set in a window,so this method sets }
{the window region to 0 before deleting the region object.}
begin
  if FRegion <>0 then
  begin //if Region is alive...
    SetWindowRgn(Handle,0,True);//set win region to 0
    DeleteObject(FRegion);//kill the region
    FRegion :=0;//zero out field
  end;
end;

//Called when the hint is activated by putting the mouse pointer above a control.
procedure TCustomHintWindow.ActivateHint(Rect:TRect;const AHint:string);
var
  ST: TStringList;
  NewH, NewW, i: integer;
  s: string;
begin
  if UpperCase(Copy(AHint,1,5)) = 'BEGIN' then
  begin
    ST := TStringList.Create;
    s := AHint;
    StringToStringList(Copy(s,6,length(s)),st);

    If ST.Count <= HintLimit then
      NewH := 15 + ST.Count * Canvas.TextHeight('gh')
    else
      NewH := 15 + HintLimit * Canvas.TextHeight('gh');

    NewW := 20;

    for i := 0 to ST.Count - 1 do
    begin
      if NewW < Canvas.TextWidth(ST[i]) then
        NewW := Canvas.TextWidth(ST[i]) + Length(ST[i]);
    end;

    Rect.Right := Rect.Left + NewW + 15;
    Rect.Bottom := Rect.Top + NewH;

    BoundsRect := Rect;
    FreeCurrentRegion;
    with BoundsRect do
    {Create a round rectangular region to display the hint window }

      //FRegion := CreateRoundRectRgn(0,0,Width,Height,Width,Height);
      FRegion := CreateRoundRectRgn(0,0,NewW,NewH,0,0);
    if FRegion <>0 then
      SetWindowRgn(Handle,FRegion,True);//set win region

    ST.Free;
  end
  else
  begin
   // with Rect do
     // Right :=Right +Canvas.TextWidth('WWWW ');//add some slop
    //Rect.Bottom:= Rect.Bottom+30;
    BoundsRect := Rect;

    FreeCurrentRegion;
    with BoundsRect do
    {Create a round rectangular region to display the hint window }

      //FRegion :=CreateRoundRectRgn(0,0,Width,Height,Width,Height);
      FRegion := CreateRoundRectRgn(0,0,self.Width,self.Height+4,0,0);
    if FRegion <>0 then
      SetWindowRgn(Handle,FRegion,True);//set win region
  end;
  inherited ActivateHint(Rect,AHint);//call inherited
end;

procedure TCustomHintWindow.CreateParams(var Params:TCreateParams);
{We need to remove the border created on the Windows API-level }
{when the window is created.}
begin
  inherited CreateParams(Params);
  //Params.Style :=Params.Style and not ws_Border;//remove border
end;

procedure TCustomHintWindow.Paint;
{This method gets called by the WM_PAINT handler.It is }
{responsible for painting the hint window.}
var
  R:TRect;
  DC: HDC;
  ST: TStringList;
  i, allb: integer;
  InternalStyles: TAdvCustomMemoStyler; //TAdvPascalMemoStyler;
  delimiters, tmpNoStart, tmpNo, tmpNoHex: String;
  Style: TStyle;

  procedure DCFrame3D(var R: TRect; const TopLeftColor, BottomRightColor: TColor);
  var
    Pen, SavePen: HPEN;
    P: array[0..2] of TPoint;
  begin
    Pen := CreatePen(PS_SOLID, 1, ColorToRGB(TopLeftColor));
    SavePen := SelectObject(DC, Pen);
    P[0] := Point(R.Left, R.Bottom-2);
    P[1] := Point(R.Left, R.Top);
    P[2] := Point(R.Right-1, R.Top);
    PolyLine(DC, P, 3);
    SelectObject(DC, SavePen);
    DeleteObject(Pen);

    Pen := CreatePen(PS_SOLID, 1, ColorToRGB(BottomRightColor));
    SavePen := SelectObject(DC, Pen);
    P[0] := Point(R.Left, R.Bottom-1);
    P[1] := Point(R.Right-1, R.Bottom-1);
    P[2] := Point(R.Right-1, R.Top-1);
    PolyLine(DC, P, 3);
    SelectObject(DC, SavePen);
    DeleteObject(Pen);
  end;

  function FindAdvCodeListControl: TAdvCodeList;
  var
    I: Integer;
  begin
    Result := nil;
    with Application.MainForm do
    for I := 0 to ComponentCount-1 do
      if Components[I] is TAdvCodeList then
      begin
        Result := TAdvCodeList(Components[I]);
        Break;
      end;
  end;

begin
  DC := Canvas.Handle;

  R :=ClientRect;//get bounding rectangle

  DCFrame3D(R, cl3DLight, cl3DDkShadow);

  Inc(R.Left,1);//move left side slightly

  if UpperCase(Copy(Caption,1,5))<> 'BEGIN' then
  begin
    Canvas.Font.Color :=clInfoText;//set to proper color
    {paint string in the center of the round rect }
    DrawText(Canvas.Handle,PChar(Caption),Length(Caption),R,
    DT_NOPREFIX or DT_WORDBREAK or DT_CENTER or DT_VCENTER);
  end
  else
  begin
    ST := TStringList.Create;

    StringToStringList(Copy(Caption,6,Length(Caption)),ST);

    {
    aAdvCodeList := FindAdvCodeListControl;

    if Assigned(aAdvCodeList) then
      InternalStyles := aAdvCodeList.SyntaxStyles;
    }

    InternalStyles := styler;

    if Assigned(InternalStyles) then
    begin
      for allb := 0 to InternalStyles.AllStyles.Count - 1 do
      begin
        if InternalStyles.AllStyles.Items[allb].StyleType <> stSymbol then
          Continue;
        delimiters := delimiters + InternalStyles.AllStyles.Items[allb].Symbols;
      end;

      tmpNoStart := UpperCase(InternalStyles.NumericChars + InternalStyles.HexIdentifier);
      tmpNo := UpperCase(InternalStyles.NumericChars) + 'E';
      tmpNoHex := Uppercase(InternalStyles.HexIdentifier);
    end
    else
    begin
      tmpNoStart := '';
      tmpNo := '';
    end;

    Style.index:= -1;
    Style.isComment := 0;
    Style.isBracket := False;
    Style.isnumber := False;
    Style.iskeyWord := false;
    Style.isdelimiter := False;
    Style.isURL := False;
    Style.index := -1;
    Style.EndBracket := #0;

    for i := 0 to ST.Count - 1 do
    begin
      if i >= HintLimit then
      begin
        DrawCustomLine(Canvas,'...', Style, dmScreen, R, InternalStyles, delimiters, tmpNoStart, tmpNoHex, tmpNo);
        R.Top := R.Top + Canvas.TextHeight('gh');
      end
      else
      begin
        DrawCustomLine(Canvas,St[i], Style, dmScreen, R, InternalStyles, delimiters, tmpNoStart, tmpNoHex, tmpNo);
        R.Top := R.Top + Canvas.TextHeight('gh');
      end;
    end;
    St.Free;
  end;
end;

procedure TCustomHintWindow.WndProc(var Message: TMessage);
begin
  if Message.Msg = WM_DESTROY then
  begin
    FreeCurrentRegion;
  end;
  inherited;
end;

end.
