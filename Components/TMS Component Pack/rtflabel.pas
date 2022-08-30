{************************************************************************}
{ TRTFLabel component                                                    }
{ for Delphi & C++Builder                                                }
{                                                                        }
{ written by                                                             }
{  TMS Software                                                          }
{  copyright © 1999 - 2015                                               }
{  Email : info@tmssoftware.com                                          }
{  Website : http://www.tmssoftware.com                                  }
{                                                                        }
{ The source code is given as is. The author is not responsible          }
{ for any possible damage done due to the use of this code.              }
{ The component can be freely used in any application. The complete      }
{ source code remains property of the author and may not be distributed, }
{ published, given or sold in any form as such. No parts of the source   }
{ code can be included in any other component or application without     }
{ written authorization of the author.                                   }
{************************************************************************}

unit RTFLabel;

{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  StdCtrls, richedit, comctrls, forms
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 3; // Minor version nr.
  REL_VER = 2; // Release nr.
  BLD_VER = 3; // Build nr.

  // version history
  // v1.0.0.0 : First release
  // v1.3.1.0 : New : Added public method PaintTo()
  // v1.3.2.0 : Improved : Performance for handling rich text strings
  // v1.3.2.1 : Fixed : Painting issues
  // v1.3.2.2 : Fixed : Issue with accessing RTFLabel.RTF helper rich edit control
  // v1.3.2.3 : Fixed : Design-time painting issue

type
  TRichText = string;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TRTFLabel = class(TCustomLabel)
  private
    { Private declarations }
    FRichedit: TRichEdit;
    FRichtext: TRichText;
    Fupdatecount: integer;
    FWordWrap: boolean;
    FPainting: boolean;
    procedure RTFPaint(Canvas: TCanvas; ARect: TRect);
    procedure RTFResizeRequest(Sender: TObject; ARect: TRect);
    function GetRichEdit: TRichEdit;
    procedure SetWordWrap(const Value: boolean);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure SetRichText(const Value: TRichText);
  protected
    { Protected declarations }
    function GetVersionNr: Integer; virtual;
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;  X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    property RTF: TRichEdit read GetRichEdit;
    procedure ShowRTF;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure CopyFromRichEdit(richedit:TRichEdit);
    procedure CopyToRichEdit(richedit:TRichEdit);
    procedure Print(Caption:string);
    procedure PaintTo(ACanvas: TCanvas);
  published
    { Published declarations }
    property RichText: TRichText read FRichText write SetRichText;
    property Align;
    property Color;
    property Transparent;
    property Visible;
    property Hint;
    property ShowHint;
    property FocusControl;
    property ParentShowHint;
    property DragCursor;
    property DragMode;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property PopupMenu;
    property OnEndDock;
    property OnStartDock;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property WordWrap:boolean read FWordWrap write SetWordWrap;
    property Version: string read GetVersion write SetVersion;
  end;

implementation

procedure TRTFLabel.BeginUpdate;
begin
  inc(FUpdateCount);
end;

procedure TRTFLabel.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    dec(FUpdateCount);
    if (FUpdateCount = 0) then
      ShowRTF;
  end;
end;

function TRTFLabel.GetRichEdit: TRichEdit;
begin
  if Assigned(Parent) and Assigned(FRichEdit) then
  begin
     FRichEdit.Parent := Parent;
//     if FRichEdit.Parent.HandleAllocated then
//     begin
//        FRichEdit.Lines.Clear;
//        FRichEdit.Selattributes := FRichEdit.DefAttributes;
//     end;
     Result := FRichEdit;
  end
  else
    Result := nil;
end;

procedure TRTFLabel.ShowRTF;
var
  ms: TStringStream;
begin
  if not Assigned(Parent) then
    Exit;

  if not Assigned(FRichEdit) then
    Exit;

  if not Parent.HandleAllocated then
    Exit;

  FRichEdit.Parent := Parent;
  ms := TStringStream.Create('');

  try
    FRichEdit.Lines.SaveToStream(ms);
    RichText := ms.DataString;
  finally
    ms.Free;
  end;

  Repaint;
  FRichEdit.Parent := nil;
end;

procedure TRTFLabel.RTFPaint(Canvas: TCanvas; ARect:TRect);
const
  RTF_OFFSET: integer = 1;
type
  rFormatRange = record
    hdcSrc: HDC;
    hdcTarget: HDC;
    rc: TRect;
    rcPage: TRect;
    chrg: TCharRange;
  end;

var
  ms: TStringStream;
  fr: rFORMATRANGE;
  nLogPixelsX,nLogPixelsY: Integer;

begin
  ms := TStringStream.Create('');
  try
    ms.WriteString(RichText);
    ms.Position := 0;
    FRichedit.Lines.LoadFromStream(ms);
  finally
    ms.free;
  end;

  FillChar(fr, SizeOf(TFormatRange), 0);

  nLogPixelsX := GetDeviceCaps(Canvas.Handle,LOGPIXELSX);
  nLogPixelsY := GetDeviceCaps(Canvas.Handle,LOGPIXELSY);

  if nLogPixelsX = 0 then
    nLogPixelsX := 96;

  if nLogPixelsY = 0 then
    nLogPixelsY := 96;

  if Transparent then
    Sendmessage(FRichedit.Handle,EM_SETBKGNDCOLOR,0,ColorToRGB(Parent.Brush.Color))
  else
    Sendmessage(FRichedit.Handle,EM_SETBKGNDCOLOR,0,ColorToRGB(Color));

  with fr do
  begin
    fr.hdcSrc := Canvas.Handle;
    fr.hdcTarget := Canvas.Handle;
    fr.rcPage.left:=round(((arect.left+RTF_OFFSET)/nLogPixelsX)*1440);
    fr.rcPage.top:=round(((arect.top+RTF_OFFSET)/nLogPixelsY)*1440);
    fr.rcPage.right:=fr.rcPage.left+round(((arect.right-arect.left-RTF_OFFSET)/nLogPixelsX) * 1440);
    fr.rcPage.bottom:=(fr.rcPage.top+round(((arect.bottom-arect.top-RTF_OFFSET)/nLogPixelsY) * 1440));
    fr.rc.left:=fr.rcPage.left;  { 1440 TWIPS = 1 inch. }
    fr.rc.top:=fr.rcPage.top;
    fr.rc.right:=fr.rcPage.right;
    fr.rc.bottom:=fr.rcPage.bottom;
    fr.chrg.cpMin := 0;
    fr.chrg.cpMax := -1;
  end;

  SendMessage(FRichEdit.Handle,EM_FORMATRANGE,1,LParam(@fr));
  //clear the richtext cache
  SendMessage(FRichEdit.Handle,EM_FORMATRANGE,0,0);
end;


procedure TRTFLabel.Paint;
var
  r: TRect;
  es: Integer;
begin
  if RichText = '' then
    inherited Paint;

  if FUpdateCount > 0 then
    Exit;

  if not Assigned(FRichEdit) then
    Exit;

  if FPainting then
    Exit;

  FPainting := true;

  FRichEdit.Parent := Parent;
  if not FRichedit.Parent.HandleAllocated then
  begin
    FPainting := false;
    Exit;
  end;

  if not (csDesigning in ComponentState) then
  begin
    es := GetWindowLong(FRichEdit.Handle, GWL_EXSTYLE);
    es := es OR WS_EX_TRANSPARENT;
    SetWindowLong(FRichEdit.Handle, GWL_EXSTYLE, es);
  end;

  frichedit.Brush.Style := bsClear;
  frichedit.Height := 0;

  r.left := 0;
  r.right := r.left + Width;
  r.top := 0;
  r.bottom := r.top + Height;

  SetBkMode(Canvas.Handle, Windows.TRANSPARENT);
  RTFPaint(Canvas, r);
  FRichedit.Parent := nil;

  FPainting := false;
end;

procedure TRTFLabel.PaintTo(ACanvas: TCanvas);
begin
  FRichEdit.Parent := Parent;
  RTFPaint(ACanvas, ClientRect);
  FRichedit.Parent := nil;
end;

constructor TRTFLabel.Create(AOwner: TComponent);
begin
  inherited;
  FRichEdit := TRichEdit.Create(self);
  FRichEdit.OnResizeRequest := RTFResizeRequest;
  AutoSize := false;
  Caption := '';
  FUpdateCount := 0;
end;

destructor TRTFLabel.Destroy;
begin
  FRichEdit.Free;
  FRichEdit := nil;
  inherited;
end;

procedure TRTFLabel.Loaded;
var
  s: string;
  i: integer;
begin
  inherited;

  FRichEdit.Visible := False;
  FRichEdit.Left := 0;
  FRichEdit.Top := 0;
  FRichEdit.Width := 10;
  FRichEdit.Height := 10;
  FRichEdit.WordWrap := False;
  FRichEdit.BorderStyle := bsNone;

  s := FRichText;

  {$IFDEF DELPHI_UNICODE}
  if (Length(s) > 0) and (ord(s[1]) > 255) then
  {$ENDIF}
  begin
    for i := 1 to length(s) do
      s[i] := char($FF and ord(s[i]));
    FRichText := s;
  end;

  Caption := '';
end;

procedure TRTFLabel.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseDown(Button,Shift,X,Y);
end;

procedure TRTFLabel.CopyFromRichEdit(RichEdit: TRichEdit);
var
  ss: TStringStream;
begin
  ss := TStringStream.Create('');
  try
    RichEdit.Lines.SaveToStream(ss);
    RichText := ss.DataString;
  finally
    ss.Free;
  end;
  Repaint;
end;

procedure TRTFLabel.CopyToRichEdit(richedit: TRichEdit);
var
  ms: TStringStream;
begin
  ms := TStringStream.Create('');
  try
    ms.WriteString(RichText);
    ms.Position := 0;
    RichEdit.PlainText := False;
    RichEdit.Lines.LoadFromStream(ms);
  finally
    ms.Free;
  end;
end;

procedure TRTFLabel.Print(Caption: string);
begin
  if Assigned(Parent) and Assigned(FRichEdit) then
  begin
    FRichEdit.Parent := Parent;
    if FRichEdit.Parent.Handleallocated then
    begin
      CopyToRichEdit(FRichEdit);
      FRichEdit.Print(Caption);
    end;
    FRichEdit.Parent := nil;
  end;
end;


procedure TRTFLabel.RTFResizeRequest(Sender: TObject; ARect: TRect);
begin
// outputdebugstring(pchar(inttostr(rect.left)+':'+inttostr(rect.top)+':'+inttostr(rect.right)+':'+inttostr(rect.bottom)));
// self.BoundsRect:=rect;
end;

procedure TRTFLabel.SetWordWrap(const Value: boolean);
begin
  FWordWrap := Value;
  if Assigned(Parent) and Assigned(FRichEdit) and not (csLoading in ComponentState) then
    FRichEdit.WordWrap := Value;
end;

function TRTFLabel.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TRTFLabel.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TRTFLabel.SetRichText(const Value: TRichText);
begin
  FRichText := Value;
  Repaint;
end;

procedure TRTFLabel.SetVersion(const Value: string);
begin

end;

end.
