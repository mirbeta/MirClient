{***************************************************************************}
{ TAdvLookupBar component                                                   }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2013                                               }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

unit AdvLookupBar;

{$I TMSDEFS.INC}

interface

uses
  Windows, Classes, Graphics, Math, StdCtrls, Forms, Controls, SysUtils, ImgList,
  AdvStyleIF, Contnrs
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : First release
  // v1.0.1.0 : New : Windows 8, Office 2013 styles added



type
  TAdvLookupBar = class;

  TLookUpBarOrder = (loNumericFirst, loNumericLast);

  TLookUpBarCategoryType = (alphanumeric, custom);

  TLookupBarCategory = class(TCollectionItem)
  private
    FOwner: TAdvLookupBar;
    FText: String;
    FID: integer;
    FTag: integer;
    FImageIndex: integer;
    FLookupText: String;
    procedure SetText(const Value: String);
    procedure SetId(const Value: integer);
    procedure SetImageIndex(const Value: integer);
    procedure SetTag(const Value: integer);
    procedure SetLookupText(const Value: String);
  protected
    procedure Changed;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property ImageIndex: integer read FImageIndex write SetImageIndex default -1;
    property Tag: integer read FTag write SetTag;
    property Text: String read FText write SetText;
    property LookupText: String read FLookupText write SetLookupText;
    property Id: integer read FID write SetId;
  end;

  {$HINTS OFF}
  TShadowedCollection = class(TPersistent)
  private
    FItemClass: TCollectionItemClass;
    FItems: TList;
  end;
  {$HINTS ON}

  TLookUpBarCategories = class(TCollection)
  private
    FOwner: TAdvLookupBar;
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TLookupBarCategory;
    procedure SetItem(Index: Integer; const Value: TLookupBarCategory);
  protected
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    function GetOwner: TPersistent; override;
    function Compare(Item1, Item2 : TLookupBarCategory) : integer; virtual;
    procedure QuickSort(L, R: Integer);
  public
    constructor Create(AOwner: TAdvLookupBar);
    property Items[Index: Integer]: TLookupBarCategory read GetItem write SetItem; default;
    function ItemById(id: integer): TLookupBarCategory;
    function ItemIndexById(id: integer): integer;
    function Add: TLookupBarCategory;
    function Insert(Index: Integer): TLookupBarCategory;
    procedure Delete(Index: Integer);
    procedure Sort;
    procedure Clear;
  end;

  TCharRecMode = (rmNormal, rmHover, rmSelected);

  TCharRec = record
    Str: String;
    Tag: Integer;
    Active: Boolean;
    Mode: TCharRecMode;
    Category: TLookupBarCategory;
  end;

  TOnLookUpEvent = procedure(Sender: TObject; LookUp: TCharRec) of object;

  TTransparentHint = class(THintWindow)
  private
    FTransparency: byte;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
  public
    property Transparency: byte read FTransparency write FTransparency;
    constructor Create(AOwner : TComponent); override;
  end;

  TCategoryString = record
    Text: String;
    ID: Integer;
  end;

  TCategoryStrings = array of TCategoryString;

  TAdvLookupBar = class(TCustomControl, ITMSStyle)
  private
    FScrollHint: TTransparentHint;
    FScrolllbl: TLabel;
    FCurrentChar: TCharRec;
    FMouseDown: Boolean;
    FChar: array[1..36] of TCharRec;
    FCustomChar: array of TCharRec;
    FColor: TColor;
    FOrder: TLookUpBarOrder;
    FColorTo: TColor;
    FSpacing: integer;
    FNumeric: Boolean;
    FRotated: Boolean;
    FCategories: TLookUpBarCategories;
    FCategoryType: TLookUpBarCategoryType;
    FImages: TCustomImageList;
    FBorderColor: TColor;
    FSelectedFont: TFont;
    FActiveFont: TFont;
    FInActiveFont: TFont;
    FScrollColor: TColor;
    FScrollFont: TFont;
    FAutoSize: Boolean;
    FOnLookUp: TOnLookUpEvent;
    FOnLookUpClick: TOnLookUpEvent;
    FTransparent: Boolean;
    FRounded: Boolean;
    FProgLookup: Boolean;
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure SetColor(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
    procedure SetNumeric(const Value: Boolean);
    procedure SetOrder(const Value: TLookUpBarOrder);
    procedure SetRotated(const Value: Boolean);
    procedure SetSpacing(const Value: integer);
    procedure SetCategories(const Value: TLookUpBarCategories);
    procedure SetCategoryType(const Value: TLookUpBarCategoryType);
    procedure SetActiveFont(const Value: TFont);
    procedure SetBorderColor(const Value: TColor);
    procedure SetInActiveFont(const Value: TFont);
    procedure SetScrollColor(const Value: TColor);
    procedure SetScrollFont(const Value: TFont);
    procedure SetSelectedFont(const Value: TFont);
    procedure SetAS(const Value: Boolean);
    procedure SetRounded(const Value: Boolean);
    procedure SetTransparent(const Value: Boolean);
  protected
    function GetVersionNr: Integer;
    procedure DrawLookUpBar(ACanvas: TCanvas; R: TRect);
    procedure FontChanged(Sender: TObject);
    procedure UpdateScrollHint(X, Y: integer);
    function GetCategoryByID(ID: integer): TLookupBarCategory;
    function XYToLookUpItem(pX, pY: Integer): TCharRec;
    function XYToLookUpCategory(pX, pY: Integer): TCharRec;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure DoLookupClick(ACurrentChar: TCharRec); virtual;
    procedure DoLookup(ACurrentChar: TCharRec); virtual;
    function CalcMinSize: integer;
    procedure Loaded; override;
    property ProgLookup: boolean read FProgLookup write FProgLookup;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure Changed;
    function XYToLookUp(X, Y: integer): TCharRec;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure SelectLookup(Lookup: String);
    property SelectedLookup: TCharRec read FCurrentChar;
    procedure InitLookupBar(LookupStrings: TStrings);
    procedure InitLookupBarCategories;
    procedure SetComponentStyle(AStyle: TTMSStyle);
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure AddLookupCategory(var LookupCategories: TCategoryStrings; LookupCategory: TCategoryString);
  published
    property AutoSize: Boolean read FAutoSize write SetAS default true;
    property Version: string read GetVersion write SetVersion;
    property BorderColor: TColor read FBorderColor write SetBorderColor default $B99D7F;
    property Color: TColor read FColor write SetColor default $00F3F3F3;
    property ColorTo: TColor read FColorTo write SetColorTo default $00D7D7D7;
    property ScrollColor: TColor read FScrollColor write SetScrollColor default clInfoBk;
    property Numeric: Boolean read FNumeric write SetNumeric default false;
    property Order: TLookUpBarOrder read FOrder write SetOrder default loNumericLast;
    property ActiveFont: TFont read FActiveFont write SetActiveFont;
    property InActiveFont: TFont read FInActiveFont write SetInActiveFont;
    property SelectedFont: TFont read FSelectedFont write SetSelectedFont;
    property ScrollFont: TFont read FScrollFont write SetScrollFont;
    property Spacing: integer read FSpacing write SetSpacing default 1;
    property Rotated: Boolean read FRotated write SetRotated default false;
    property CategoryType: TLookUpBarCategoryType read FCategoryType write SetCategoryType default alphanumeric;
    property Categories: TLookUpBarCategories read FCategories write SetCategories;
    property Images: TCustomImageList read FImages write FImages;
    property OnLookUp: TOnLookUpEvent read FOnLookUp write FOnLookUp;
    property OnLookUpClick: TOnLookUpEvent read FOnLookUpClick write FOnLookUpClick;
    property Transparent: Boolean read FTransparent write SetTransparent default false;
    property Rounded: Boolean read FRounded write SetRounded default False;

    property Align;
    property Anchors;
    property Ctl3D;
    property Constraints;
    property PopupMenu;
    property TabOrder;
    property ParentShowHint;
    property ShowHint;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseDown;
    {$IFDEF DELPHI2006_LVL}
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
    property DragKind;
    property DragMode;
    property OnResize;
    property OnDblClick;
    property OnClick;
    property OnEnter;
    property OnExit;
    property OnStartDrag;
    property OnEndDrag;
    property OnDragDrop;
    property OnDragOver;
    property Visible;
    property TabStop default true;
{$IFDEF DELPHI_TOUCH}
    property OnGesture;
    property Touch;
{$ENDIF}
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property BevelWidth;
    property BiDiMode;
    property DockSite;
    property DoubleBuffered;
    property DragCursor;
    property Enabled;
{$IFDEF DELPHI2006_LVL}
    property Padding;
    property ParentBackground default False;
{$ENDIF}
    property ParentBiDiMode;
    property ParentCtl3D;
    property ParentFont;
    property OnCanResize;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnEndDock;
    property OnGetSiteInfo;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnStartDock;
    property OnUnDock;
  end;

implementation

{ TAdvLookupBar }

procedure DrawGradient(ACanvas: TCanvas; FromColor, ToColor: TColor; Steps: Integer; R: TRect; Direction: Boolean);
var
  diffr, startr, endr: Integer;
  diffg, startg, endg: Integer;
  diffb, startb, endb: Integer;
  rstepr, rstepg, rstepb, rstepw: Real;
  i, stepw: Word;
  DoPaint : Boolean;
begin
  if Steps = 0 then
    Steps := 1;

  FromColor := ColorToRGB(FromColor);
  ToColor := ColorToRGB(ToColor);

  startr := (FromColor and $0000FF);
  startg := (FromColor and $00FF00) shr 8;
  startb := (FromColor and $FF0000) shr 16;
  endr := (ToColor and $0000FF);
  endg := (ToColor and $00FF00) shr 8;
  endb := (ToColor and $FF0000) shr 16;

  diffr := endr - startr;
  diffg := endg - startg;
  diffb := endb - startb;

  rstepr := diffr / steps;
  rstepg := diffg / steps;
  rstepb := diffb / steps;

  if Direction then
    rstepw := (R.Right - R.Left) / Steps
  else
    rstepw := (R.Bottom - R.Top) / Steps;

  with ACanvas do
  begin
    for i := 0 to steps - 1 do
    begin
      endr := startr + Round(rstepr * i);
      endg := startg + Round(rstepg * i);
      endb := startb + Round(rstepb * i);
      stepw := Round(i * rstepw);
      Pen.Color := endr + (endg shl 8) + (endb shl 16);
      Brush.Color := Pen.Color;
      if Direction then
       Rectangle(R.Left + stepw, R.Top, R.Left + stepw + Round(rstepw) + 1, R.Bottom)
      else
      begin
        DoPaint := (( R.Top + stepw + Round(rstepw) + 1) <= R.Bottom);
        if DoPaint then
          Rectangle(R.Left, R.Top + stepw, R.Right, R.Top + stepw + Round(rstepw) + 1);
      end;
    end;
  end;
end;


procedure TAdvLookupBar.AddLookupCategory(var LookupCategories: TCategoryStrings;
  LookupCategory: TCategoryString);
begin
  SetLength(LookupCategories, Length(LookupCategories) + 1);
  LookupCategories[Length(LookupCategories) - 1].Text := LookupCategory.Text;
  LookupCategories[Length(LookupCategories) - 1].ID := LookupCategory.ID;
end;

procedure TAdvLookupBar.Changed;
begin
  invalidate;
end;

constructor TAdvLookupBar.Create(AOwner: TComponent);
begin
  inherited;
  DoubleBuffered := true;
  FAutoSize := True;
  FColor := $00F3F3F3;
  FColorTo := $00D7D7D7;
  FNumeric := False;
  FOrder := loNumericLast;
  FBorderColor := $B99D7F;
  FScrollColor := clInfoBk;

  FActiveFont := TFont.Create;
  FInActiveFont := TFont.Create;
  FSelectedFont := TFont.Create;
  FScrollFont := TFont.Create;

  FSpacing := 1;
  FRotated := False;
  FCategoryType := alphanumeric;
  FCategories := TLookUpBarCategories.Create(Self);

  {$IFNDEF DELPHI7_LVL}
  FActiveFont.Name := 'Tahoma';
  FInActiveFont.Name := 'Tahoma';
  FSelectedFont.Name := 'Tahoma';
  FScrollFont.Name := 'Tahoma';
  {$ENDIF}

  FSelectedFont.Style := [fsBold];
  FSelectedFont.Color := clHighlight;
  FScrollFont.Style := [fsBold];
  FScrollFont.Size := 12;

  FInActiveFont.Color := clSilver;


  FActiveFont.OnChange := FontChanged;
  FInActiveFont.OnChange := FontChanged;
  FSelectedFont.OnChange := FontChanged;
  FScrollFont.OnChange := FontChanged;

  Width := 20;
  Height := 275;

  FScrollHint := TTransparentHint.Create(Application);
  FScrollHint.Parent := Self;
  FScrollHint.Visible := False;
  Fscrollhint.Transparency := 175;

  FScrolllbl := TLabel.Create(FScrollHint);
  FScrolllbl.Parent := FScrollHint;
  FScrolllbl.Alignment := taCenter;
  FScrolllbl.Align := alClient;
  FScrolllbl.Color := ScrollColor;
  FScrolllbl.Transparent := False;

  FTransparent := False;
  FRounded := False;
end;

destructor TAdvLookupBar.Destroy;
begin
  FActiveFont.Free;
  FInActiveFont.Free;
  FSelectedFont.Free;
  FScrollFont.Free;
  FCategories.Free;
  inherited;
end;

procedure TAdvLookupBar.DoLookup(ACurrentChar: TCharRec);
begin
  if Assigned(OnLookUp) then
    OnLookUp(Self, ACurrentChar);
end;

procedure TAdvLookupBar.DoLookupClick(ACurrentChar: TCharRec);
begin
  if Assigned(OnLookUpClick) then
    OnLookUpClick(Self, ACurrentChar);
end;

procedure TAdvLookupBar.DrawLookUpBar(ACanvas: TCanvas; R: TRect);
var
  ch: Char;
  customch: String;
  I: integer;
  pt: TPoint;
  ximg, x, y: Integer;
  tw, th: integer;
  stop: integer;
  lf: TLogFont;
  tf: TFont;
  DrawR: TRect;
  toprgn, bottomrgn, rgn, outrgn, resrgn: HRGN;
begin

  resrgn := 0;
  if Rounded and not Transparent then
  begin
    toprgn := CreateEllipticRgn(r.Left, r.Top, r.Right + 1, r.Right);
    bottomrgn := CreateEllipticRgn(r.Left, r.Bottom - (r.Right - r.Left), r.Right + 1, r.Bottom);
    rgn := CreateRectRgn(r.Left, r.Top + (r.Right - r.Left) div 2, r.Right, r.Bottom - (r.Right - r.Left)  div 2);
    outrgn := CreateRectRgn(0, 0, 0, 0);
    resrgn := CreateRectRgn(0, 0, 0, 0);

    CombineRgn(outrgn, rgn, toprgn, RGN_OR);
    CombineRgn(resrgn, outrgn, bottomrgn, RGN_OR);

    DeleteObject(toprgn);
    DeleteObject(bottomrgn);
    DeleteObject(rgn);
    DeleteObject(outrgn);

    SelectClipRgn(ACanvas.Handle, resrgn);
  end;

  if not Transparent then
    DrawGradient(ACanvas, Color, ColorTo, 100, R, True);

  ACanvas.Brush.Style := bsClear;

  DrawR := Rect(r.Left + 3, r.Top + 3, r.Right - 3, r.Bottom - 3);

  y := DrawR.Top;
  case CategoryType of
    alphanumeric:
    begin
      if Numeric then
        stop := 37
      else
        stop := 27;

      for I := 1 to stop - 1 do
      begin
        if (Order = loNumericLast) or not Numeric then
        begin
          if I < 27 then
            ch := chr(ord('A') + (i - 1))
          else
            ch := chr(ord('0') + (i - 27));
        end
        else
        begin
          if I < 11 then
            ch := chr(ord('0') + (i - 1))
          else
            ch := chr(ord('A') + (i - 11));
        end;

        if (FChar[I].Str = FCurrentChar.Str) and (FCurrentChar.Str <> '') then
          ACanvas.Font.Assign(SelectedFont)
        else
        begin
          if FChar[I].Active then
            ACanvas.Font.Assign(ActiveFont)
          else
            ACanvas.Font.Assign(InActiveFont);
        end;

        if Rotated then
        begin
          th := ACanvas.TextWidth(ch);
          tw := ACanvas.TextHeight(ch);
        end
        else
        begin
          tw := ACanvas.TextWidth(ch);
          th := ACanvas.TextHeight(ch);
        end;

        if rotated then
        begin
          tf := TFont.Create;
          tf.Assign(ACanvas.Font);
          GetObject(tf.Handle, SizeOf(lf), @lf);
          lf.lfEscapement := -900;
          lf.lfOrientation := -900;
          tf.Handle := CreateFontIndirect(lf);
          ACanvas.Font.Assign(tf);
          tf.Free;
        end;

        if AutoSize then
          y := DrawR.Top + Round((DrawR.Bottom - DrawR.Top - Spacing) / (stop - 1) * (I - 1) + (Round((DrawR.Bottom - DrawR.Top - Spacing) / (stop - 1) - th) / 2));

        if rotated then
          ACanvas.TextOut(DrawR.Left + tw + (DrawR.Right - DrawR.Left - tw) div 2, y , ch)
        else
          ACanvas.TextOut(DrawR.Left + (DrawR.Right - DrawR.Left - tw) div 2, y, ch);

        if not AutoSize then
          y := y + th + Spacing
      end;
    end;
    custom:
    begin
      stop := Categories.Count;

      for I := 0 to stop - 1 do
      begin
        with Categories[I] do
        begin
          if LookupText <> '' then
            customch := LookupText
          else
            customch := Text;

          if Length(FCustomChar) > ID then
          begin
            if FCustomChar[ID].Str = FCurrentChar.Str then
              ACanvas.Font.Assign(SelectedFont)
            else
            begin
              if FCustomChar[ID].Active then
                ACanvas.Font.Assign(ActiveFont)
              else
                ACanvas.Font.Assign(InActiveFont);
            end;
          end;

          if Rotated then
          begin
            th := ACanvas.TextWidth(customch);
            tw := ACanvas.TextHeight(customch);
          end
          else
          begin
            tw := ACanvas.TextWidth(customch);
            th := ACanvas.TextHeight(customch);
          end;

          if rotated then
          begin
            tf := TFont.Create;
            tf.Assign(ACanvas.Font);
            GetObject(tf.Handle, SizeOf(lf), @lf);
            lf.lfEscapement := -900;
            lf.lfOrientation := -900;
            tf.Handle := CreateFontIndirect(lf);
            ACanvas.Font.Assign(tf);
            tf.Free;
          end;


          x := DrawR.Left + (DrawR.Right - DrawR.Left - tw) div 2;
          ximg := DrawR.left;
          if Assigned(FImages) and Rotated then
            if (FImageIndex > -1) and (FImageIndex < FImages.Count) then
              ximg := DrawR.Left + (DrawR.Right - DrawR.Left - 4 - FImages.Width) div 2;

          if Assigned(FImages) and not Rotated then
            if (FImageIndex > -1) and (FImageIndex < FImages.Count) then
              x := x + Images.Width div 2;

        if AutoSize then
          y := DrawR.Top + Round((DrawR.Bottom - DrawR.Top - Spacing) / stop * I + (Round((DrawR.Bottom - DrawR.Top - Spacing) / stop - th) / 2));

          pt := Point(x, y);
          if Assigned(FImages) and Rotated then
            if (FImageIndex > -1) and (FImageIndex < FImages.Count) then
              pt := Point(x, y + FImages.Height);


          if rotated then
            ACanvas.TextOut(DrawR.Left + tw + (DrawR.Right - DrawR.Left - tw) div 2, pt.y , customch)
          else
            ACanvas.TextOut(DrawR.Left + (DrawR.Right - DrawR.Left - tw) div 2, pt.y, customch);

          if Assigned(FImages) then
          begin
            if (ImageIndex <> -1) and (ImageIndex < FImages.Count) then
            begin
              if Rotated then
                FImages.Draw(ACanvas, Round(ximg), Round(y + 1), ImageIndex)
              else
                FImages.Draw(ACanvas, Round(ximg + 3), Round(y + 1), ImageIndex)
            end;
          end;

          if not AutoSize then
          begin
            y := y + th + Spacing;
            if Assigned(FImages) and Rotated then
              if (FImageIndex > -1) and (FImageIndex < FImages.Count) then
                y := y + FImages.Height;
          end;
        end;
      end;
    end;
  end;


  if not Transparent then
  begin
    ACanvas.Pen.Color := BorderColor;
    if Rounded then
    begin
      SelectClipRgn(ACanvas.Handle,0);
      DeleteObject(resrgn);
      ACanvas.MoveTo(r.Left, r.Top + (r.Right - r.Left) div 2);
      ACanvas.Lineto(r.Left, r.bottom - (r.Right - r.Left) div 2);
      ACanvas.Arc(r.Left, r.Top, r.Right, r.right, r.Right, r.Top + (r.Right - r.Left) div 2, r.Left, r.Top + (r.Right - r.Left) div 2);
      ACanvas.Arc(r.Left, r.Bottom , r.Right, r.Bottom - 1 - (r.Right - r.Left), r.Left, r.Bottom - (r.Right - r.Left) div 2, r.Right, r.Bottom - (r.Right - r.Left)  div 2);
      ACanvas.MoveTo(r.Right - 1, r.Top + (r.Right - r.Left) div 2);
      ACanvas.Lineto(r.Right - 1, r.bottom - (r.Right - r.Left) div 2);
    end
    else
      ACanvas.Rectangle(R);
  end;
end;

procedure TAdvLookupBar.FontChanged(Sender: TObject);
begin
  Changed;
end;

function TAdvLookupBar.GetCategoryByID(ID: integer): TLookupBarCategory;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Categories.Count - 1 do
  begin
    if Categories[i].Id = ID then
    begin
      result := Categories[i];
      break;
    end;
  end;
end;

function TAdvLookupBar.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TAdvLookupBar.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TAdvLookupBar.InitLookupBar(LookUpStrings: TStrings);
var
  I, j: Integer;
  s: String;
begin
  if (csDestroying in ComponentState) then
    Exit;

  case CategoryType of
    alphanumeric:
    begin
      for I := 1 to 36 do
      begin
        if (Order = loNumericLast) or not Numeric then
        begin
          if I < 27 then
            FChar[I].Str := chr(ord('A') + (i - 1))
          else
            FChar[I].Str := chr(ord('0') + (i - 27));
        end
        else
        begin
          if I < 11 then
            FChar[I].Str := chr(ord('0') + (i - 1))
          else
            FChar[I].Str := chr(ord('A') + (i - 11));
        end;
        FChar[I].Active := false;
      end;

      for I := 0 to LookupStrings.Count - 1 do
      begin
        S := AnsiUpperCase(LookupStrings[i]);
        if (S <> '') then
        begin
          if (Ord(s[1]) <= 90) then
          begin
            j := Ord(s[1]) - 64;
            if (j < 27) and (J > 0) then
            begin
              if (Order = loNumericLast) or not Numeric then
              begin
                if not FChar[j].Active then
                begin
                  FChar[j].Active := true;
                  FChar[j].Tag := I;
                end;
              end
              else
              begin
                if not FChar[10 + J].Active then
                begin
                  FChar[10 + j].Active := true;
                  FChar[10 + j].Tag := I;
                end;
              end;
            end
            else
            begin
              j := Ord(s[1]) - 48;
              if (j < 11) and (j >= 0) then
              begin
                if (Order = loNumericLast) or not Numeric then
                begin
                  if not FChar[27 + J].Active then
                  begin
                    FChar[27 + j].Active := true;
                    FChar[27 + j].Tag := I;
                  end;
                end
                else
                begin
                  if not FChar[j + 1].Active then
                  begin
                    FChar[j + 1].Active := true;
                    FChar[j + 1].Tag := I;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TAdvLookupBar.InitLookupBarCategories;
var
  I, j: Integer;
  s: String;
begin
  if (csDestroying in ComponentState) then
    Exit;

  case CategoryType of
    custom:
    begin
      for I := 0 to Categories.Count - 1 do
      begin
        SetLength(FCustomChar, I + 1);
        FCustomChar[I].Active := false;
        FCustomChar[i].Str := Categories[I].Text;
      end;

      for I := 0 to Categories.Count - 1 do
      begin
        S := Categories[i].Text;
        if (S <> '') and (Categories[i].Tag <> -1) then
        begin
          j := Categories[i].ID;
          if (j < Categories.Count) and (J > -1) then
          begin
            if not FCustomChar[j].Active then
            begin
              FCustomChar[j].Active := true;
              FCustomChar[j].Category := GetCategoryByID(j);
              FCustomChar[j].Tag := Categories[i].Tag;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TAdvLookupBar.Loaded;
var
  y: integer;
begin
  inherited;

  y := CalcMinSize;
  if Height < y + 10 then
    Height := y + 10;

end;

procedure TAdvLookupBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  chr: TCharRec;
begin
  inherited;
  FMouseDown := True;
  chr := XYToLookUp(Width div 2, Y);
  if chr.Str <> '' then
  begin
//    if chr.Str <> FCurrentChar.Str then
    begin
      if chr.Active then
      begin
        FCurrentChar := chr;
        DoLookupClick(FCurrentChar);

        UpdateScrollHint(X, Y);

        invalidate;
      end;
    end;
  end;
end;

procedure TAdvLookupBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  chr: TCharRec;
  pt: TPoint;
begin
  inherited;
  if FMouseDown then
  begin
    chr := XYToLookUp(Width div 2, Y);
    if Assigned(FScrollHint) then
    begin
      pt := Point((Width - FScrollHint.Width) div 2, Max(0, Min(Height, Y)) - (FScrollHint.Height div 2));
      pt := ClientToScreen(pt);
      FScrollHint.Left := pt.X;
      FScrollHint.Top := pt.Y;
    end;
    if chr.Str <> '' then
    begin
      if chr.Str <> FCurrentChar.Str then
      begin
        if chr.Active then
        begin
          FCurrentChar := chr;
          DoLookup(FCurrentChar);
          UpdateScrollHint(X, Y);

          invalidate;
        end;
      end;
    end;
  end;
end;

procedure TAdvLookupBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  FMouseDown := False;
  if Assigned(FScrollHint) then
  begin
    FScrollHint.Hide;
    FScrollHint.Visible := False;
  end;
end;

procedure TAdvLookupBar.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = FImages) then
    FImages := nil;
end;

procedure TAdvLookupBar.Paint;
begin
  DrawLookUpBar(Canvas, ClientRect);
end;

procedure TAdvLookupBar.SelectLookup(Lookup: String);
var
  i: integer;
begin
  case CategoryType of
    alphanumeric:
    begin
      for I := 1 to 36 do
      begin
        if FChar[i].Str = Lookup then
        begin
          FCurrentChar := FChar[i];
          DoLookup(FCurrentChar);
          break;
        end;
      end;
    end;
    custom:
    begin
      for I := 0 to Length(FCustomChar) - 1 do
      begin
        if FCustomChar[i].Str = Lookup then
        begin
          FCurrentChar := FCustomChar[i];
          DoLookup(FCurrentChar);
          break;
        end;
      end;
    end;
  end;

  Invalidate;
end;

procedure TAdvLookupBar.SetActiveFont(const Value: TFont);
begin
  if FActiveFont <> Value then
  begin
    FActiveFont.Assign(Value);
  end;
end;

procedure TAdvLookupBar.SetAS(const Value: Boolean);
begin
  if FAutoSize <> value then
  begin
    FAutoSize := Value;
    Changed;
  end;
end;

procedure TAdvLookupBar.SetBorderColor(const Value: TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    Changed;
  end;
end;

function TAdvLookupBar.CalcMinSize: integer;
var
  ch: Char;
  customch: String;
  I: integer;
  y: Integer;
  th: integer;
  stop: integer;
  bmp: TBitmap;
begin

  bmp := TBitmap.Create;
  y := 0;
  case CategoryType of
    alphanumeric:
    begin
      if Numeric then
        stop := 37
      else
        stop := 27;

      for I := 1 to stop - 1 do
      begin
        if (Order = loNumericLast) or not Numeric then
        begin
          if I < 27 then
            ch := chr(ord('A') + (i - 1))
          else
            ch := chr(ord('0') + (i - 27));
        end
        else
        begin
          if I < 11 then
            ch := chr(ord('0') + (i - 1))
          else
            ch := chr(ord('A') + (i - 11));
        end;

        if (FChar[I].Str = FCurrentChar.Str) and (FCurrentChar.Str <> '') then
          bmp.Canvas.Font.Assign(SelectedFont)
        else
        begin
          if FChar[I].Active then
            bmp.Canvas.Font.Assign(ActiveFont)
          else
            bmp.Canvas.Font.Assign(InActiveFont);
        end;

        if Rotated then
          th := bmp.Canvas.TextWidth(ch)
        else
          th := bmp.Canvas.TextHeight(ch);

         y := y + th + Spacing;
      end;
    end;
    custom:
    begin
      stop := Categories.Count;

      for I := 0 to stop - 1 do
      begin
        with Categories[I] do
        begin
          if LookupText <> '' then
            customch := LookupText
          else
            customch := Text;


          if Length(FCustomChar) > 0 then
          begin
            if FCustomChar[ID].Str = FCurrentChar.Str then
              bmp.Canvas.Font.Assign(SelectedFont)
            else
            begin
              if FCustomChar[ID].Active then
                bmp.Canvas.Font.Assign(ActiveFont)
              else
                bmp.Canvas.Font.Assign(InActiveFont);
            end;
          end;

          if Rotated then
            th := bmp.Canvas.TextWidth(customch)
          else
            th := bmp.Canvas.TextHeight(customch);

          y := y + th + Spacing;
          if Assigned(FImages) and Rotated then
            if (FImageIndex > -1) and (FImageIndex < FImages.Count) then
              y := y + FImages.Height;
        end;
      end;
    end;
  end;

  bmp.Free;

  Result := y;
end;

procedure TAdvLookupBar.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  y: Integer;
begin
  y := 0;
  if not (csLoading in ComponentState) then
  begin
    y := CalcMinSize;
  end;

  inherited SetBounds(ALeft, ATop, AWidth, Max(y + 10, AHeight));
end;

procedure TAdvLookupBar.SetCategories(const Value: TLookUpBarCategories);
begin
  if FCategories <> Value then
  begin
    FCategories.Assign(Value);
    Changed;
  end;
end;

procedure TAdvLookupBar.SetCategoryType(const Value: TLookUpBarCategoryType);
begin
  if FCategoryType <> Value then
  begin
    FCategoryType := Value;
    Changed;
  end;
end;

procedure TAdvLookupBar.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TAdvLookupBar.SetColorTo(const Value: TColor);
begin
  if FColorTo <> Value then
  begin
    FColorTo := Value;
    Changed;
  end;
end;

procedure TAdvLookupBar.SetComponentStyle(AStyle: TTMSStyle);
begin
  // TODO : do color settings here
 case AStyle of
    tsOffice2003Blue:
      begin
        Color := $FCE1CB;
        ColorTo := $E0A57D;
        SelectedFont.Color := $1595EE;
        ActiveFont.Color := clBlack;
        ScrollColor := $EBFDFF;
        ScrollFont.Color := SelectedFont.Color;
        InActiveFont.Color := $00F2F2F2;
      end;
    tsOffice2003Silver:
      begin
        Color := $ECE2E1;
        ColorTo := $B39698;
        SelectedFont.Color := $1595EE;
        ActiveFont.Color := clBlack;
        ScrollColor := $EBFDFF;
        ScrollFont.Color := SelectedFont.Color;
        InActiveFont.Color := $00F2F2F2;
      end;
    tsOffice2003Olive:
      begin
        Color := $CFF0EA;
        ColorTo := $8CC0B1;
        SelectedFont.Color := $1595EE;
        ActiveFont.Color := clBlack;
        InActiveFont.Color := $00F2F2F2;
        ScrollColor := $EBFDFF;
        ScrollFont.Color := SelectedFont.Color;
      end;
    tsOffice2003Classic:
      begin
       Color := $F0F0F0;
        ColorTo := Color;
        ActiveFont.Color := clBlack;
        InActiveFont.Color := $00B6B6B6;
        ScrollColor := $EFD3C6;
        ScrollFont.Color := clHighlight;
        SelectedFont.Color := clHighlight;
      end;
    tsOffice2007Luna:
      begin
        Color := $FBF9F6;
        ColorTo := $E8DBD2;

        ActiveFont.Color := clBlack;
        SelectedFont.Color := $1595EE;

        ScrollColor := $EBFDFF;
        ScrollFont.Color := SelectedFont.Color;
        InActiveFont.Color := $00F2F2F2;
      end;
    tsOffice2007Obsidian:
      begin
        Color := $F7F7F7;
        ColorTo := $DEDEDE;

        ActiveFont.Color := clBlack;
        SelectedFont.Color := $1595EE;

        ScrollColor := $EBFDFF;
        ScrollFont.Color := SelectedFont.Color;
        InActiveFont.Color := clGray;
      end;
    tsWindowsXP:
      begin
        Color := $F0F0F0;
        ColorTo := Color;
        ActiveFont.Color := clBlack;
        InActiveFont.Color := $00B6B6B6;
        ScrollColor := $EFD3C6;
        ScrollFont.Color := clHighlight;
        SelectedFont.Color := clHighlight;
      end;
    tsWhidbey:
      begin
        Color := $F5F9FA;
        ColorTo := $F5F9FA;
        ActiveFont.Color := clWhite;
        InActiveFont.Color := $00F2F2F2;
        ScrollColor := $EBFDFF;
        SelectedFont.Color := $AAD9FF;
      end;
    tsCustom: ;
    tsOffice2007Silver:
      begin
        Color := $F3F3F1;
        ColorTo := $CAC9C8;
        SelectedFont.Color := $1595EE;
        ScrollColor := $EBFDFF;
        ScrollFont.Color := SelectedFont.Color;
        ActiveFont.Color := clBlack;
        InActiveFont.Color := $00F2F2F2;
      end;
      tsWindowsVista:
      begin
        Color := $FFFFFF;
        ColorTo := $F4F2F1;
        ActiveFont.Color := clBlack;
        InActiveFont.Color := clSilver;
        ScrollColor := $FFFDF9;
        SelectedFont.Color := $EACAB6;
        ScrollFont.Color := SelectedFont.Color;
      end;
      tsWindows7:
      begin

        Color := $FDFCFA;
        ColorTo := $F7E9DD;

        ActiveFont.Color := clBlack;
        InActiveFont.Color := clSilver;
        ScrollColor := $FDFBFA;
        SelectedFont.Color := clHighlight;
        ScrollFont.Color := SelectedFont.Color;
      end;
      tsTerminal:
      begin
        Color := clBtnFace;
        ColorTo := clBtnFace;
        ActiveFont.Color := clBlack;
        InActiveFont.Color := clSilver;
        ScrollColor := clGray;
        SelectedFont.Color := clHighLight;
      end;
      tsOffice2010Blue:
      begin
        Color := $F5E7DA;
        ColorTo := Color;
        ActiveFont.Color := clBlack;
        InActiveFont.Color := $DEC1A9;
        ScrollColor := $FFEBDB;
        SelectedFont.Color := clHighLight;
        ScrollFont.Color := SelectedFont.Color;
      end;
      tsOffice2010Silver:
      begin
        Color := $E8E3DF;
        ColorTo := Color;

        ActiveFont.Color := RGB(50,50,50);
        InActiveFont.Color := clSilver;
        ScrollColor := $EEDDCF;
        SelectedFont.Color := clBlack;
        ScrollFont.Color := clBlack;

      end;
      tsOffice2010Black:
      begin
        Color := $6A6A6A;
        ColorTo := Color;
        ActiveFont.Color := clWhite;
        InActiveFont.Color := clSilver;

        ScrollColor := $EEDDCF;
        SelectedFont.Color := $FFDBB7;

        ScrollFont.Color := clBlack;
      end;
    tsWindows8:
      begin
        BorderColor := $E4E3E2;
        Color := $F7F6F5;
        ColorTo := Color;
        ActiveFont.Color := clBlack;
        InActiveFont.Color := clSilver;

        ScrollColor := $F7EFE8;
        SelectedFont.Color := clBlack;

        ScrollFont.Color := clBlack;
      end;
    tsOffice2013White:
      begin
        BorderColor := $D4D4D4;
        Color := clWhite;
        ColorTo := Color;
        ActiveFont.Color := clBlack;
        InActiveFont.Color := clSilver;

        ScrollColor := $FCF0E4;
        SelectedFont.Color := clBlack;

        ScrollFont.Color := clBlack;
      end;
    tsOffice2013LightGray:
      begin
        BorderColor := $C6C6C6;
        Color := $F6F6F6;
        ColorTo := Color;
        ActiveFont.Color := clBlack;
        InActiveFont.Color := clSilver;

        ScrollColor := $FCF0E4;
        SelectedFont.Color := clBlack;

        ScrollFont.Color := clBlack;
      end;
    tsOffice2013Gray:
      begin
        BorderColor := $ABABAB;
        Color := $E5E5E5;
        ColorTo := Color;
        ActiveFont.Color := clBlack;
        InActiveFont.Color := clSilver;

        ScrollColor := $FCF0E4;
        SelectedFont.Color := clBlack;

        ScrollFont.Color := clBlack;
      end;

  end;
end;

procedure TAdvLookupBar.SetInActiveFont(const Value: TFont);
begin
  if FInActiveFont <> Value then
  begin
    FInActiveFont.Assign(Value);
    Changed;
  end;
end;

procedure TAdvLookupBar.SetNumeric(const Value: Boolean);
begin
  if FNumeric <> Value then
  begin
    FNumeric := Value;
    Changed;
  end;
end;

procedure TAdvLookupBar.SetOrder(const Value: TLookUpBarOrder);
begin
  if FOrder <> Value then
  begin
    FOrder := Value;
    Changed;
  end;
end;

procedure TAdvLookupBar.SetRotated(const Value: Boolean);
begin
  if FRotated <> Value then
  begin
    FRotated := Value;
    Changed;
  end;
end;

procedure TAdvLookupBar.SetRounded(const Value: Boolean);
begin
  if FRounded <> Value then
  begin
    FRounded := Value;
    Changed;
  end;
end;

procedure TAdvLookupBar.SetScrollColor(const Value: TColor);
begin
  if FScrollColor <> Value then
  begin
    FScrollColor := Value;
    if Assigned(FScrolllbl) then
      FScrolllbl.Color := Value;
    Changed;
  end;
end;

procedure TAdvLookupBar.SetScrollFont(const Value: TFont);
begin
  if FScrollFont <> Value then
  begin
    FScrollFont.Assign(Value);
    Changed;
  end;
end;

procedure TAdvLookupBar.SetSelectedFont(const Value: TFont);
begin
  if FSelectedFont <> Value then
  begin
    FSelectedFont.Assign(Value);
    Changed;
  end;
end;

procedure TAdvLookupBar.SetSpacing(const Value: integer);
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    Changed;
  end;
end;

procedure TAdvLookupBar.SetTransparent(const Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    Changed;
  end;
end;

procedure TAdvLookupBar.SetVersion(const Value: string);
begin

end;

procedure TAdvLookupBar.UpdateScrollHint(X, Y: integer);
var
  tw, th: integer;
  pt: TPoint;
begin
  if Assigned(FScrollHint) then
  begin
    FScrollHint.Left := pt.X;
    FScrollHint.Top := pt.Y;

    FScrollHint.Canvas.Font.Assign(ScrollFont);
    tw := FScrollHint.Canvas.TextWidth(FCurrentChar.Str);
    th := FScrollHint.Canvas.TextHeight(FCurrentChar.Str);
    FScrollHint.Width := tw + 20;
    FScrollHint.Height := th + 2;
    pt := Point(X, Max(0, Min(Height, Y)));
    pt := ClientToScreen(pt);
    FScrollHint.Font.Assign(ScrollFont);
    FScrolllbl.Caption := FCurrentChar.Str;
    if not FScrollHint.Visible then
    begin
      FScrollHint.Visible := True;
      FScrollHint.Show;
    end;
  end;
end;

function TAdvLookupBar.XYToLookUp(X, Y: integer): TCharRec;
begin
  case CategoryType of
    alphanumeric: Result := XYToLookUpItem(X, Y);
    custom: result := XYToLookUpCategory(X, Y);
  end;
end;

function TAdvLookupBar.XYToLookUpCategory(pX, pY: Integer): TCharRec;
var
  customch: String;
  I: integer;
  pt: TPoint;
  x, y: Integer;
  tw, th: integer;
  stop: integer;
  lf: TLogFont;
  tf: TFont;
  txtr: TRect;
  DrawR, R: TRect;
  bmp: TBitmap;
begin
  bmp := TBitmap.Create;
  R := ClientRect;
  DrawR := Rect(r.Left + 3, r.Top + 3, r.Right - 3, r.Bottom - 3);

  y := DrawR.Top;
  case CategoryType of
    custom:
    begin
      stop := Categories.Count;

      for I := 0 to stop - 1 do
      begin
        with Categories[I] do
        begin
          if LookupText <> '' then
            customch := LookupText
          else
            customch := Text;

          if Length(FCustomChar) > 0 then
          begin
            if FCustomChar[ID].Str = FCurrentChar.Str then
              bmp.Canvas.Font.Assign(SelectedFont)
            else
            begin
              if FCustomChar[ID].Active then
                bmp.Canvas.Font.Assign(ActiveFont)
              else
                bmp.Canvas.Font.Assign(InActiveFont);
            end;
          end;

          if Rotated then
          begin
            th := bmp.Canvas.TextWidth(customch);
            tw := bmp.Canvas.TextHeight(customch);
          end
          else
          begin
            tw := bmp.Canvas.TextWidth(customch);
            th := bmp.Canvas.TextHeight(customch);
          end;

          if rotated then
          begin
            tf := TFont.Create;
            tf.Assign(bmp.Canvas.Font);
            GetObject(tf.Handle, SizeOf(lf), @lf);
            lf.lfEscapement := -900;
            lf.lfOrientation := -900;
            tf.Handle := CreateFontIndirect(lf);
            bmp.Canvas.Font.Assign(tf);
            tf.Free;
          end;

          x := DrawR.Left + (DrawR.Right - DrawR.Left - tw) div 2;

          if Assigned(FImages) and not Rotated then
            if (FImageIndex > -1) and (FImageIndex < FImages.Count) then
              x := x + Images.Width div 2;

          if AutoSize then
            y := DrawR.Top + Round((DrawR.Bottom - DrawR.Top - Spacing) / stop * I + (Round((DrawR.Bottom - DrawR.Top - Spacing) / stop - th) / 2));

          pt := Point(x, y);
          if Assigned(FImages) and Rotated then
            if (FImageIndex > -1) and (FImageIndex < FImages.Count) then
              pt := Point(x, y + FImages.Height);

          if AutoSize then
            txtr := Bounds(DrawR.Left, pt.y , DrawR.Right - DrawR.Left, Round(((DrawR.Bottom - DrawR.Top) / stop)))
          else
            txtr := Bounds(DrawR.Left, pt.y , DrawR.Right - DrawR.Left, th + Spacing);

          if PtInRect(txtr, Point(pX, pY)) then
          begin
            if (ID >= 0) and (ID <= Length(FCustomChar) - 1) then
            begin
              Result := FCustomChar[ID];
              break;
            end;
          end;

          if not AutoSize then
          begin
            y := y + th + Spacing;
            if Assigned(FImages) and Rotated then
              if (FImageIndex > -1) and (FImageIndex < FImages.Count) then
                y := y + FImages.Height;
          end;
        end;
      end;
    end;
  end;

  bmp.Free;
end;

function TAdvLookupBar.XYToLookUpItem(pX, pY: Integer): TCharRec;
var
  ch: Char;
  I: integer;
  y: Integer;
  th: integer;
  stop: integer;
  lf: TLogFont;
  tf: TFont;
  txtr: TRect;
  DrawR, R: TRect;
  bmp: TBitmap;
begin
  bmp := Tbitmap.create;
  R := ClientRect;
  DrawR := Rect(r.Left + 3, r.Top + 3, r.Right - 3, r.Bottom - 3);

  y := DrawR.Top;
  case CategoryType of
    alphanumeric:
    begin
      if Numeric then
        stop := 37
      else
        stop := 27;

      for I := 1 to stop - 1 do
      begin
        if (Order = loNumericLast) or not Numeric then
        begin
          if I < 27 then
            ch := chr(ord('A') + (i - 1))
          else
            ch := chr(ord('0') + (i - 27));
        end
        else
        begin
          if I < 11 then
            ch := chr(ord('0') + (i - 1))
          else
            ch := chr(ord('A') + (i - 11));
        end;

        if FChar[I].Str = FCurrentChar.Str then
          bmp.Canvas.Font.Assign(SelectedFont)
        else
        begin
          if FChar[I].Active then
            bmp.Canvas.Font.Assign(ActiveFont)
          else
            bmp.Canvas.Font.Assign(InActiveFont);
        end;

        if Rotated then
          th := bmp.Canvas.TextWidth(ch)
        else
          th := bmp.Canvas.TextHeight(ch);

        if rotated then
        begin
          tf := TFont.Create;
          tf.Assign(bmp.Canvas.Font);
          GetObject(tf.Handle, SizeOf(lf), @lf);
          lf.lfEscapement := -900;
          lf.lfOrientation := -900;
          tf.Handle := CreateFontIndirect(lf);
          bmp.Canvas.Font.Assign(tf);
          tf.Free;
        end;

        if AutoSize then
          y := DrawR.Top + Round((DrawR.Bottom - DrawR.Top - Spacing) / (stop - 1) * (I - 1) + (Round((DrawR.Bottom - DrawR.Top - Spacing) / (stop - 1) - th) / 2));

        if AutoSize then
          txtr := Bounds(DrawR.Left, y , DrawR.Right - DrawR.Left, Round(((DrawR.Bottom - DrawR.Top) / stop)))
        else
          txtr := Bounds(DrawR.Left, y , DrawR.Right - DrawR.Left, th + Spacing);


        if Ptinrect(txtr, Point(pX, pY)) then
        begin
          result := FChar[i];
          break;
        end;

        if not AutoSize then
          y := y + th + Spacing
      end;
    end;
  end;

  bmp.Free;
end;

{ TLookupBarCategory }

procedure TLookupBarCategory.Assign(Source: TPersistent);
begin
  if (Source is TLookupBarCategory) then
  begin
    FText := (Source as TLookupBarCategory).Text;
    FLookupText := (Source as TLookupBarCategory).LookupText;
    FId := (Source as TLookupBarCategory).Id;
    FImageIndex := (Source as TLookupBarCategory).ImageIndex;
    Changed;
  end;
end;

procedure TLookupBarCategory.Changed;
begin
  FOwner.Changed;
end;

constructor TLookupBarCategory.Create(Collection: TCollection);
begin
  inherited;
  Fowner := (Collection as TLookupBarCategories).FOwner;
  FID := (Collection as TLookupBarCategories).Count - 1;
  FImageIndex := -1;
  FOwner.Changed;
end;

destructor TLookupBarCategory.Destroy;
begin
  inherited;
  FOwner.Changed;
end;

procedure TLookupBarCategory.SetId(const Value: integer);
begin
  if FId <> value then
  begin
    FId := Value;
    Changed;
  end;
end;

procedure TLookupBarCategory.SetImageIndex(const Value: integer);
begin
  if FImageIndex <> value then
  begin
    FImageIndex := Value;
    Changed;
  end;
end;

procedure TLookupBarCategory.SetLookupText(const Value: String);
begin
  if FLookupText <> value then
  begin
    FLookupText := Value;
    Changed;
  end;
end;

procedure TLookupBarCategory.SetTag(const Value: integer);
begin
  if FTag <> value then
  begin
    FTag := Value;
    Changed;
  end;
end;

procedure TLookupBarCategory.SetText(const Value: String);
begin
  if FText <> value then
  begin
    FText := Value;
    Changed;
  end;
end;

{ TLookupBarCategories }

function TLookupBarCategories.Add: TLookupBarCategory;
begin
  Result := TLookupBarCategory(inherited Add);
end;

procedure TLookupBarCategories.Clear;
begin
  if Count > 0 then
  begin
    while Count > 0 do
      TCollectionItem(Items[Count - 1]).Free;
  end;
end;

function TLookupBarCategories.Compare(Item1,
  Item2: TLookupBarCategory): integer;
begin
  if item1.Text < item2.Text then
    result :=  -1
  else if item1.Text > item2.Text then
    result := 1
  else result := 0;
end;

constructor TLookupBarCategories.Create(AOwner: TAdvLookupBar);
begin
  inherited Create(TLookupBarCategory);
  FOwner := AOwner;
end;

procedure TLookupBarCategories.Delete(Index: Integer);
begin
  Items[Index].Free;
end;

function TLookupBarCategories.GetItem(
  Index: Integer): TLookupBarCategory;
begin
  Result := TLookupBarCategory(inherited Items[Index]);
end;

function TLookupBarCategories.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TLookupBarCategories.Insert(
  Index: Integer): TLookupBarCategory;
begin
  Result := TLookupBarCategory(inherited Insert(Index));
end;

function TLookupBarCategories.ItemById(
  id: integer): TLookupBarCategory;
var
  i: integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    Result := Items[I];
    if Result.ID = id then
      break;
  end;
end;

function TLookupBarCategories.ItemIndexByID(id: integer): integer;
var
  ci: TLookupBarCategory;
begin
  ci := ItemByID(id);
  if Assigned(ci) then
    result := ci.Index
  else
    result := -1;
end;

procedure TLookupBarCategories.QuickSort(L, R: Integer);
var
  I, J, p: Integer;
  Save: TCollectionItem;
  SortList: TList;
begin
  //This cast allows us to get at the private elements in the base class
  SortList := TShadowedCollection(Self).FItems;

  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while Compare(Items[I], Items[P]) < 0 do
        Inc(I);
      while Compare(Items[J], Items[P]) > 0 do
        Dec(J);
      if I <= J then begin
        Save              := SortList.Items[I];
        SortList.Items[I] := SortList.Items[J];
        SortList.Items[J] := Save;
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(L, J);
    L := I;
  until I >= R;
end;

procedure TLookupBarCategories.SetItem(Index: Integer;
  const Value: TLookupBarCategory);
begin
  inherited Items[Index] := Value;
end;

procedure TLookupBarCategories.Sort;
begin
  if Count > 1 then
    QuickSort(0, pred(Count));

  FOwner.Changed;
end;

{ TTransparentHint }

constructor TTransparentHint.Create(AOwner: TComponent);
begin
  inherited;
  FTransparency := 100;
end;

procedure TTransparentHint.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle := Params.ExStyle or WS_EX_LAYERED;
end;

procedure TTransparentHint.CreateWnd;
var
  SetLayeredWindowAttributes: TSetLayeredWindowAttributes;
begin
  inherited;
  SetLayeredWindowAttributes := GetProcAddress(GetModulehandle(user32), 'SetLayeredWindowAttributes');
  SetLayeredWindowAttributes(Handle, 0, FTransparency, LWA_ALPHA);
end;

end.