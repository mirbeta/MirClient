{************************************************************************}
{ TAdvLabel component                                                    }
{ for Delphi & C++Builder                                                }
{                                                                        }
{ written by                                                             }
{  TMS Software                                                          }
{  copyright © 2014                                                      }
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

unit AdvLabel;

{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  StdCtrls, Richedit, comctrls, forms
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 1; // Build nr.

  // version history
  // v1.0.0.0 : First release
  // v1.0.0.1 : Fixed : Design-time painting issue

type
  TRichText = string;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvLabel = class(TCustomLabel)
  private
    { Private declarations }
    FRichedit: TRichEdit;
    FRichtext: TRichText;
    Fupdatecount: integer;
    FWordWrap: boolean;
    FPainting: boolean;
    FAlignment: TAlignment;
    procedure RTFPaint(Canvas:TCanvas;ARect:TRect);
    procedure RTFResizeRequest(Sender: TObject; Rect: TRect);
    function GetRichEdit:TRichEdit;
    procedure SetWordWrap(const Value: boolean);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure SetRichText(const Value: TRichText);
    procedure SetAlignment(const Value: TAlignment);
  protected
    { Protected declarations }
    function GetVersionNr: Integer; virtual;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    property RTF: TRichEdit read GetRichEdit;
    procedure ShowRTF;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure CopyFromRichEdit(richedit: TCustomRichEdit);
    procedure CopyToRichEdit(richedit: TCustomRichEdit);
    procedure Print(Caption:string);
    procedure PaintTo(ACanvas: TCanvas);
  published
    { Published declarations }
    property Align;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
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
    property Text: TRichText read FRichText write SetRichText;
    property WordWrap:boolean read FWordWrap write SetWordWrap;
    property Version: string read GetVersion write SetVersion;
  end;

implementation

procedure TAdvLabel.BeginUpdate;
begin
  inc(FUpdateCount);
end;

procedure TAdvLabel.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    dec(FUpdateCount);
    if (FUpdateCount = 0) then
      ShowRTF;
  end;
end;

function TAdvLabel.GetRichEdit: TRichEdit;
begin
  if Assigned(Parent) and Assigned(FRichEdit) then
  begin
     FRichEdit.Parent := Parent;
     Result := FRichEdit;
  end
  else
    Result := nil;
end;

procedure TAdvLabel.ShowRTF;
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
    Text := ms.DataString;
  finally
    ms.Free;
  end;

  Repaint;
  FRichEdit.Parent := nil;
end;

procedure TAdvLabel.RTFPaint(Canvas: TCanvas; ARect:TRect);
const
  RTF_OFFSET :integer = 1;
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
    ms.WriteString(Text);
    ms.position := 0;
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
    fr.hdctarget := Canvas.Handle;
    fr.rcPage.left := round(((arect.left+RTF_OFFSET)/nLogPixelsX)*1440);
    fr.rcPage.top := round(((arect.top+RTF_OFFSET)/nLogPixelsY)*1440);
    fr.rcPage.right := fr.rcPage.left+round(((arect.right-arect.left-RTF_OFFSET)/nLogPixelsX) * 1440);
    fr.rcPage.bottom := (fr.rcPage.top+round(((arect.bottom-arect.top-RTF_OFFSET)/nLogPixelsY) * 1440));
    fr.rc.left := fr.rcPage.left;  { 1440 TWIPS = 1 inch. }
    fr.rc.top := fr.rcPage.top;
    fr.rc.right := fr.rcPage.right;
    fr.rc.bottom := fr.rcPage.bottom;
    fr.chrg.cpMin := 0;
    fr.chrg.cpMax := -1;
  end;

  SendMessage(FRichEdit.Handle,EM_FORMATRANGE,1,LParam(@fr));
  //clear the richtext cache
  SendMessage(FRichEdit.Handle,EM_FORMATRANGE,0,0);
end;


procedure TAdvLabel.Paint;
var
  r: TRect;
  es: Integer;
  style: DWORD;

begin
  if Text = '' then
    inherited Paint;

  if FUpdateCount > 0 then
    Exit;

  if not Assigned(FRichEdit) then
    Exit;

  if FPainting then
    Exit;

  R := ClientRect;

  if Pos('{\', Text) = 1 then
  begin
    FPainting := true;

    FRichedit.Height := 0;
    FRichEdit.Parent := GetParentForm(Self);
    if not FRichedit.Parent.HandleAllocated then
    begin
      FPainting := false;
      Exit;
    end;

    if not (csDesigning in ComponentState) then
    begin
      es := GetWindowLong(FRichEdit.handle,GWL_EXSTYLE);
      es := es or WS_EX_TRANSPARENT;
      SetWindowLong(FRichEdit.Handle,GWL_EXSTYLE,es);
    end;

    FRichedit.Brush.Style := bsClear;

    r := ClientRect;
    OffsetRect(R,0,3);
    SetBkMode(Canvas.Handle,1);
    RTFPaint(Canvas,r);

    FRichedit.Parent := nil;
    FPainting := false;
  end
  else
  begin
    style := DT_LEFT;
    case Alignment of
    taRightJustify: style := DT_RIGHT;
    taCenter: style := DT_CENTER;
    end;

    if FWordWrap then
    begin
      style := style or DT_WORDBREAK;
      OffsetRect(R,3,4);
    end
    else
      style := style or DT_SINGLELINE or DT_VCENTER or DT_END_ELLIPSIS;

    Canvas.Brush.Style := bsClear;
    DrawText(Canvas.Handle, PChar(Text), Length(Text), R, style);
  end;
end;

procedure TAdvLabel.PaintTo(ACanvas: TCanvas);
begin
  FRichEdit.Parent := Parent;
  RTFPaint(ACanvas, ClientRect);
  FRichedit.Parent := nil;
end;

constructor TAdvLabel.Create(AOwner: TComponent);
begin
  inherited;
  FRichEdit := TRichEdit.Create(self);
  FRichEdit.OnResizeRequest := RTFResizeRequest;
  AutoSize := false;
  Caption := '';
  FUpdateCount := 0;
  FAlignment := taLeftJustify;
end;

destructor TAdvLabel.Destroy;
begin
  FRichEdit.Free;
  FRichEdit := nil;
  inherited;
end;

procedure TAdvLabel.Loaded;
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

procedure TAdvLabel.CopyFromRichEdit(RichEdit: TCustomRichEdit);
var
  ss: TStringStream;
begin
  ss := TStringStream.Create('');

  try
    (RichEdit as TRichEdit).Lines.SaveToStream(ss);
    Text := ss.DataString;
  finally
    ss.Free;
  end;
  Repaint;
end;

procedure TAdvLabel.CopyToRichEdit(richedit: TCustomRichEdit);
var
  ms: TStringStream;
begin
  ms := TStringStream.Create('');
  try
    ms.WriteString(Text);
    ms.Position := 0;
    if RichEdit is TRichEdit then
      (RichEdit as TRichEdit).PlainText := False;
    RichEdit.Lines.LoadFromStream(ms);
  finally
    ms.Free;
  end;
end;

procedure TAdvLabel.Print(Caption: string);
begin
  if Assigned(Parent) and Assigned(FRichEdit) then
  begin
    FRichEdit.Parent := Parent;
    if FRichEdit.Parent.Handleallocated then
    begin
      Copytorichedit(fRichEdit);
      FRichEdit.print(Caption);
    end;
    FRichEdit.Parent := nil;
  end;
end;

procedure TAdvLabel.RTFResizeRequest(Sender: TObject; Rect: TRect);
begin
// self.BoundsRect:=rect;
end;

procedure TAdvLabel.SetWordWrap(const Value: boolean);
begin
  FWordWrap := Value;
  if Assigned(Parent) and Assigned(FRichEdit) and not (csLoading in ComponentState) then
    FRichEdit.WordWrap := Value;
end;

function TAdvLabel.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TAdvLabel.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TAdvLabel.SetAlignment(const Value: TAlignment);
begin
  if (FAlignment <> Value) then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

procedure TAdvLabel.SetRichText(const Value: TRichText);
begin
  FRichText := Value;
  Repaint;
end;

procedure TAdvLabel.SetVersion(const Value: string);
begin

end;

end.
