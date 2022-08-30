{***************************************************************************}
{ TAdvPicturePane component                                                 }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2014                                               }
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
unit AdvPicturePane;

{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, ExtCtrls, Graphics, Forms,
  StdCtrls, JPEG, Math, AdvGDIP
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

type
  TAdvPicturePane = class;

  TItemClickEvent = procedure(Sender: TObject; Index: Integer) of object;

  TAspectRatio = (arNone, arStretch, arShrink);

  TPictureItem = class(TCollectionItem)
  private
    { Private declarations }
    FOwner: TAdvPicturePane;
    FCaption: string;
    FHint: string;
    FPicture: TPicture;
    FSelected: Boolean;
    FX: Integer;
    FY: Integer;
    function GetHeigthByWidth: Double;
    function GetShrinkable: Boolean;
    function GetScaleByWidth: Boolean;
    function GetPaintRectangle: TRect;
    function GetWidthByHeight: Double;
    procedure SetCaption(const Value: string);
    procedure SetHint(const Value: string);
    procedure SetPicture(const Value: TPicture);
    procedure SetSelected(const Value: Boolean);
    procedure SetX(const Value: Integer);
    procedure SetY(const Value: Integer);
  protected
    { Protected declarations }
    property HeightByWidth: Double read GetHeigthByWidth;
    property PaintRectangle: TRect read GetPaintRectangle;
    property ScaleByWidth: Boolean read GetScaleByWidth;
    property Shrinkable: Boolean read GetShrinkable;
    property WidthByHeight: Double read GetWidthByHeight;
    procedure PictureChanged(Sender: TObject);
    procedure ItemChanged;
    function GetDisplayName: String; override;
    property Selected: Boolean read FSelected write SetSelected;
    property X: Integer read FX write SetX;
    property Y: Integer read FY write SetY;
  public
    { Public declarations }
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    { Published declarations }
    property Caption: string read FCaption write SetCaption;
    property Hint: string read FHint write SetHint;
    property Picture: TPicture read FPicture write SetPicture;
  end;

  TPictureList = class(TOwnedCollection)
  private
    { Private declarations }
    FOnChange: TNotifyEvent;
    procedure SetOnChange(const Value: TNotifyEvent);
  protected
    { Protected declarations }
    function GetItem(Index: Integer): TPictureItem;
    procedure SetItem(Index: Integer; Value: TPictureItem);
    procedure Update(Item: TCollectionItem); override;
  public
    { Public declarations }
    constructor Create(AOwner: TAdvPicturePane);
    function Add: TPictureItem;
    function Insert(Index: Integer): TPictureItem;
    property Items[Index: Integer]: TPictureItem read GetItem write SetItem; default;
  published
    { Published declarations }
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;

  TAdvPicturePane = class(TCustomControl)
  private
    { Private declarations }
    FDragIndex: Integer;
    FHoverPicture: TPictureItem;
    FItemIndex: Integer;
    FMarginX: Integer;
    FMarginY: Integer;
    FMaxOffsetX: Integer;
    FMaxPic: Integer;
    FMinPic: Integer;
    FOffsetX: Integer;
    FPictureHeight: Integer;
    FPictures: TPictureList;
    FPictureWidth: Integer;
    FOnItemClick: TItemClickEvent;
    FScroll: TScrollBar;
    FAspectRatio: TAspectRatio;
    FPictureBackground: TColor;
    FPictureSelectedBackground: TColor;
    // WM
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    // CM
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
    procedure CMHintShow(var Msg: TMessage); message CM_HINTSHOW;
    //normal procs
    function GetVersion: string;
    function GetVersionNr: Integer;
    function ItemAtXY(X: Integer; Y: Integer): TPictureItem;
    function IndexAtXPos(X: Integer): Integer;
    procedure PicturesChanged(Sender: TObject);
    procedure SetItemIndex(const Value: Integer);
    procedure SetInternalPos(Sender: TObject);
    procedure SetMarginY(const Value: Integer);
    procedure SetMarginX(const Value: Integer);
    procedure SetOffset;
    procedure SetPictureHeight(const Value: Integer);
    procedure SetPictures(const Value: TPictureList);
    procedure SetPictureWidth(const Value: Integer);
    procedure SetPictureXY;
    procedure SetScrollBar;
    procedure SetVersion(const Value: string);
    procedure SetAspectRatio(const Value: TAspectRatio);
    procedure SetPictureBackground(const Value: TColor);
    procedure SetPictureSelectedBackground(const Value: TColor);
  protected
    { Protected declarations }
    procedure DragOver(Source: TObject; X: Integer; Y: Integer;
      State: TDragState; var Accept: Boolean); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure Paint; override;
    procedure Resize; override;
    procedure DoEnter; override;
    procedure DoExit; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ItemAtCursor: TPictureItem;
    procedure DragDrop(Source: TObject; X: Integer; Y: Integer); override;
  published
    { Published declarations }
    property Align;
    property AspectRatio: TAspectRatio read FAspectRatio write SetAspectRatio default arNone;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property Color;
    property Cursor;
    property DoubleBuffered default true;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ItemIndex: Integer read FItemIndex write SetItemIndex default 0;
    property Font;
    property Height default 300;
    property MarginX: Integer read FMarginX write SetMarginX default 25;
    property MarginY: Integer read FMarginY write SetMarginY default 20;
    property PictureBackground: TColor read FPictureBackground write SetPictureBackground default clWindow;
    property PictureSelectedBackground: TColor read FPictureSelectedBackground write SetPictureSelectedBackground default clHighlight;
    property PictureHeight: Integer read FPictureHeight write SetPictureHeight default 200;
    property Pictures: TPictureList read FPictures write SetPictures;
    property PictureWidth: Integer read FPictureWidth write SetPictureWidth default 100;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Touch;
    property Version: string read GetVersion write SetVersion;
    property Width default 400;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnItemClick: TItemClickEvent read FOnItemClick write FOnItemClick;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

{ TAdvPicturePane }

procedure TAdvPicturePane.CMFontChanged(var Msg: TMessage);
begin
  Invalidate;
end;

procedure TAdvPicturePane.CMHintShow(var Msg: TMessage);
var
  pt: TPoint;
begin
  with TCMHintShow(Msg).HintInfo^ do
  begin
    pt := CursorPos;
    if FHoverPicture <> nil then
      HintStr := FHoverPicture.Hint;
  end;
end;

constructor TAdvPicturePane.Create(AOwner: TComponent);
begin
  inherited;
  DoubleBuffered := True;
  Width := 400;
  Height := 300;
  FPictureWidth := 100;
  FPictureHeight := 200;
  FMarginX := 25;
  FMarginY := 20;
  FOffsetX := 0;
  FItemIndex := 0;
  FPictures := TPictureList.Create(Self);
  FPictures.OnChange := PicturesChanged;
  FScroll := TScrollBar.Create(Self);
  FScroll.Parent := Self;
  FScroll.Height := 15;
  FScroll.Top := Height - 15;
  FScroll.LargeChange := 100;
  FScroll.OnChange := SetInternalPos;
  FScroll.ShowHint := False;
  FAspectRatio := arNone;
  FPictureBackground := clWindow;
  FPictureSelectedBackground := clHighlight;
  TabStop := true;
end;

destructor TAdvPicturePane.Destroy;
begin
  FPictures.Free;
  FScroll.Free;
  inherited;
end;

procedure TAdvPicturePane.DoEnter;
begin
  inherited;
  Invalidate;
end;

procedure TAdvPicturePane.DoExit;
begin
  inherited;
  Invalidate;
end;

procedure TAdvPicturePane.DragDrop(Source: TObject; X, Y: Integer);
var
  DragPane: TAdvPicturePane;
begin
  if Assigned(OnDragDrop) then
    OnDragDrop(Self, Source, X, Y);
  if (Source is TAdvPicturePane) and (Source <> Self) then
  begin
    DragPane := Source as TAdvPicturePane;
    if IndexAtXPos(X) = -1 then
      Pictures.Add.Assign(DragPane.Pictures[DragPane.FDragIndex])
    else
      Pictures.Insert(IndexAtXPos(X)).Assign(DragPane.Pictures[DragPane.FDragIndex]);
  end;
end;

procedure TAdvPicturePane.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := (Source is TAdvPicturePane) and (Source <> Self);
  if Assigned(OnDragOver) then
    OnDragOver(Self, Source, X, Y, State, Accept);
end;

function TAdvPicturePane.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn))) + '.' + IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

function TAdvPicturePane.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

function TAdvPicturePane.IndexAtXPos(X: Integer): Integer;
var
  Pic: TPictureItem;
begin
  Pic := ItemAtXY(X, FMarginY + 2);
  if Pic <> nil then
    Result := Pic.Index
  else
  begin
    Pic := ItemAtXY(X + FMarginX, FMarginY + 2);
    if Pic <> nil then
      Result := Pic.Index
    else
    begin
      Pic := ItemAtXY(X - FMarginX, FMarginY + 2);
      if Pic <> nil then
        Result := Min(Pic.Index + 1, Pictures.Count - 1)
      else
        Result := -1;
    end;
  end;
end;

function TAdvPicturePane.ItemAtCursor: TPictureItem;
begin
  Result := FHoverPicture;
end;

function TAdvPicturePane.ItemAtXY(X, Y: Integer): TPictureItem;
var
  I: Integer;
  Pic: TPictureItem;
begin
  Result := nil;
  if Pictures.Count <> 0 then
  begin
    for I := FMinPic to FMaxPic do
    begin
      Pic := FPictures.Items[I];
      if (X >= Pic.X) and (X <= (Pic.X + FPictureWidth)) then
      begin
        if (Y >= Pic.Y) and (Y <= Pic.Y + FPictureHeight) then
        begin
          Result := Pic;
          Break;
        end;
      end;
    end;
  end;
end;

procedure TAdvPicturePane.SetOffset;
begin
  if (FItemIndex >= 0) and (FItemIndex <= FPictures.Count - 1) then
  begin
    if FPictures.Items[FItemIndex].X < 0 then
      FOffsetX := FOffsetX + FPictures.Items[FItemIndex].X - FMarginX
    else if (FPictures.Items[FItemIndex].X + FPictureWidth) > Width then
      FOffsetX := FOffsetX + FPictures.Items[FItemIndex].X + FPictureWidth - Width + FMarginX;
  end;
end;

procedure TAdvPicturePane.SetPictureXY;
var
  Pic: TPictureItem;
  I: Integer;
  MinPicSet: Boolean;
begin
  MinPicSet := False;
  if Pictures.Count > 0 then
  begin
    FItemIndex := Min(FItemIndex, Pictures.Count - 1);
    for I := 0 to Pictures.Count - 1 do
    begin
      if I < Pictures.Count then
      begin
        Pic := FPictures.Items[I];
        Pic.X := (FMarginX + FPictureWidth) * I + FMarginX - FOffsetX;
        Pic.Y := FMarginY;
        Pic.Selected := (FItemIndex = I);
        //if Pic.Hint = '' then
        //  Pic.Hint := 'Hint for Item #' + IntToStr(I);
        if not MinPicSet and ((Pic.X + FPictureWidth) > 0) then
        begin
          FMinPic := I;
          MinPicSet :=  True;
        end;
        if MinPicSet and (Pic.X < Width) then
          FMaxPic := I;
      end;
    end;
  end;
end;

procedure TAdvPicturePane.SetScrollBar;
var
  Pic: TPictureItem;
begin
  FScroll.Visible := False;
  if Assigned(FPictures) and (FPictures.Count > 0) then
  begin
    Pic := FPictures.Items[FPictures.Count - 1];
    if (Pic.X + FMarginX + FPictureWidth + FOffsetX) > Self.Width then
    begin
      FScroll.Visible := True;
      FScroll.Min := 0;
      FMaxOffsetX := Pic.X + FMarginX + FPictureWidth + FOffsetX - Width;
      if FScroll.Position > FMaxOffsetX then
        FScroll.Position := FMaxOffsetX;
      FScroll.PageSize := Width;
      FScroll.Max := Width + FMaxOffsetX;
    end
    else
    begin
      FOffsetX := 0;
      SetPictureXY;
    end;
  end;
end;

procedure TAdvPicturePane.SetVersion(const Value: string);
begin

end;

procedure TAdvPicturePane.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  inherited;
  Msg.Result := DLGC_WANTARROWS;
end;

procedure TAdvPicturePane.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Assigned(OnKeyDown) then
    OnKeyDown(Self, Key, Shift);
  case Key of
    vk_Left: FItemIndex := Max(FItemIndex - 1, 0);
    vk_Up: FItemIndex := Max(FItemIndex - 1, 0);
    vk_Right: FItemIndex := Min(FItemIndex + 1, FPictures.Count -1);
    vk_Down: FItemIndex := Min(FItemIndex + 1, FPictures.Count -1);
    vk_Prior: FItemIndex := Max(FItemIndex - (FMaxPic - FMinPic), 0);
    vk_Next: FItemIndex := Min(FItemIndex + (FMaxPic - FMinPic), FPictures.Count -1);
    vk_Home: FItemIndex := 0;
    vk_End: FItemIndex := FPictures.Count - 1;
  end;
  SetOffset;
  FScroll.Position := FOffsetX;
  SetInternalPos(Self);
  Invalidate;
end;

procedure TAdvPicturePane.Loaded;
begin
  inherited;
  SetPictureXY;
  SetScrollBar;
  Invalidate;
end;

procedure TAdvPicturePane.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  Pic: TPictureItem;
begin
  inherited;
  if Assigned(OnMouseDown) then
    OnMouseDown(Self, Button, Shift, X, Y);
  if not (csDesigning in ComponentState) and CanFocus then
  begin
    SetFocus;
    Pic := ItemAtXY(X, Y);
    if Assigned(Pic) then
    begin
      FItemIndex := Pic.Index;
      SetOffset;
      FScroll.Position := FOffsetX;
      SetInternalPos(Self);
      Invalidate;
      if Assigned(OnItemClick) then
        OnItemClick(Self, FItemIndex);
      FDragIndex := Pic.Index;
      BeginDrag(False, 10);
    end;
  end;
end;

procedure TAdvPicturePane.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Pic: TPictureItem;
begin
  inherited;

  Pic := ItemAtXY(X, Y);
  if Pic <> FHoverPicture then
    Application.CancelHint;

  FHoverPicture := Pic;
end;

procedure TAdvPicturePane.Paint;
var
  I: Integer;
  Pic: TPictureItem;
  r, PicRect: TRect;
  h: integer;
begin
  inherited;

  if Pictures.Count > 0 then
  begin
    for I := FMinPic to FMaxPic do
    begin
      Pic := Pictures.Items[I];
      PicRect := Rect(Pic.X, Pic.Y, Pic.X + FPictureWidth, Pic.Y + FPictureHeight);

      Canvas.Brush.Style := bsSolid;

      if Pic.Selected then
        Canvas.Brush.Color := FPictureSelectedBackground
      else
        Canvas.Brush.Color := FPictureBackground;

      Canvas.FillRect(PicRect);

      if Pic.Selected and Focused then
        DrawFocusRect(Canvas.Handle, PicRect);

      Canvas.StretchDraw(Pic.PaintRectangle, Pic.Picture.Graphic);
      Canvas.Font.Assign(Font);
      h := Canvas.TextHeight('gh');
      r := Rect(Pic.X, Pic.Y + FPictureHeight + 5, Pic.X + FPictureWidth, Pic.Y + FPictureHeight + 5 + h);
      Canvas.Brush.Style := bsClear;
      DrawText(Canvas.Handle, PWideChar(Pic.Caption), Length(Pic.Caption), r, DT_CENTER or DT_END_ELLIPSIS);
    end;
  end;
end;

procedure TAdvPicturePane.PicturesChanged(Sender: TObject);
begin
  SetPictureXY;
  SetScrollBar;
  Invalidate;
end;

procedure TAdvPicturePane.Resize;
begin
  inherited;
  FScroll.Height := 15;
  FScroll.Top := Height - 15;
  FScroll.Width := Width;
  Loaded;
end;

procedure TAdvPicturePane.SetAspectRatio(const Value: TAspectRatio);
begin
  if Value <> FAspectRatio then
  begin
    FAspectRatio := Value;
    PicturesChanged(Self);
  end;
end;

procedure TAdvPicturePane.SetInternalPos(Sender: TObject);
begin
  FOffsetX :=  Min(FScroll.Position, FMaxOffsetX);
  FScroll.Position := FOffsetX;
  SetPictureXY;
  Invalidate;
end;

procedure TAdvPicturePane.SetItemIndex(const Value: Integer);
begin
  if (Value < Pictures.Count) and (FItemIndex <> Value) then
  begin
    FItemIndex := Value;
    SetOffset;
    FScroll.Position := FOffsetX;
    SetPictureXY;
    SetInternalPos(Self);
    Invalidate;
  end;
end;

procedure TAdvPicturePane.SetMarginY(const Value: Integer);
begin
  if FMarginY <> Value then
  begin
    FMarginY := Value;
    PicturesChanged(Self);
  end;
end;

procedure TAdvPicturePane.SetMarginX(const Value: Integer);
begin
  if FMarginX <> Value then
  begin
    FOffsetX := FOffsetX + (Value - FMarginX) * FMinPic;
    FMarginX := Value;
    PicturesChanged(Self);
  end;
end;

procedure TAdvPicturePane.SetPictureBackground(const Value: TColor);
begin
  if (FPictureBackground <> Value) then
  begin
    FPictureBackground := Value;
    Invalidate;
  end;
end;

procedure TAdvPicturePane.SetPictureHeight(const Value: Integer);
begin
  if FPictureHeight <> Value then
  begin
    FPictureHeight := Value;
    PicturesChanged(Self);
  end;
end;

procedure TAdvPicturePane.SetPictures(const Value: TPictureList);
begin
  FPictures.Assign(Value);
  PicturesChanged(Self);
end;

procedure TAdvPicturePane.SetPictureSelectedBackground(const Value: TColor);
begin
  if (FPictureSelectedBackground <> Value) then
  begin
    FPictureSelectedBackground := Value;
    Invalidate;
  end;
end;

procedure TAdvPicturePane.SetPictureWidth(const Value: Integer);
begin
  if FPictureWidth <> Value then
  begin
    FPictureWidth := Value;
    FScroll.LargeChange := Value;
    PicturesChanged(Self);
  end;
end;

{ TPictureItem }

procedure TPictureItem.Assign(Source: TPersistent);
begin
  if (Source is TPictureItem) then
  begin
    FCaption := (Source as TPictureItem).Caption;
    FHint := (Source as TPictureItem).Hint;
    FPicture.Assign((Source as TPictureItem).Picture);
    ItemChanged;
  end;
end;

constructor TPictureItem.Create(Collection: TCollection);
begin
  inherited;
  FOwner := TPictureList(Collection).Owner as TAdvPicturePane;
  FPicture := TPicture.Create;
  FPicture.OnChange := PictureChanged;
  FHint := 'Hint for Item #' + IntToStr(Index);
end;

destructor TPictureItem.Destroy;
begin
  FPicture.Free;
  inherited;
end;

function TPictureItem.GetDisplayName: String;
begin
  Result := Format('Picture %d',[Index]);
end;

function TPictureItem.GetHeigthByWidth: Double;
begin
  if Picture.Width <> 0 then
    Result := Picture.Height / Picture.Width
  else
    Result := 1;
end;

function TPictureItem.GetPaintRectangle: TRect;
var
  XFrom, XTo, YFrom, YTo: Integer;
  h,w: integer;
  dostretch: boolean;

begin
  if Assigned(FOwner) then
  begin
    dostretch := (FOwner.AspectRatio = arStretch) or ((FOwner.AspectRatio = arShrink) and Shrinkable);

    if (FOwner.AspectRatio = arNone) then
    begin
      XFrom := X;
      XTo := X + FOwner.PictureWidth;
      YFrom := Y;
      YTo := Y + FOwner.PictureHeight;
    end
    else
    begin
      if dostretch then
      begin
        if ScaleByWidth then
        begin
          XFrom := X;
          XTo := X + FOwner.PictureWidth;

          h := Round(FOwner.PictureWidth * HeightByWidth);

          YFrom := Y + Max(0, (FOwner.PictureHeight - h) div 2);
          YTo := YFrom + h;
        end
        else
        begin
          YFrom := Y;
          YTo := Y + FOwner.PictureHeight;

          w := Round(FOwner.PictureHeight * WidthByHeight);

          XFrom := X + Max(0, (FOwner.PictureWidth - w) div 2);
          XTo :=  XFrom + w;
        end;
      end
      else
      begin
        XFrom := X + (FOwner.PictureWidth - Picture.Width) div 2;
        XTo := XFrom + Picture.Width;

        YFrom := Y + (FOwner.PictureHeight - Picture.Height) div 2;
        YTo := YFrom + Picture.Height;
      end;
    end;
    Result := Rect(XFrom, YFrom, XTo, YTo);
  end;
end;

function TPictureItem.GetScaleByWidth: Boolean;
var
  ph, pw: Integer;
begin
  Result := False;
  if Assigned(FOwner) then
  begin
    pw := FOwner.PictureWidth;
    ph := FOwner.PictureHeight;
    Result := FPicture.Width / pw > FPicture.Height / ph;
  end;
end;

function TPictureItem.GetShrinkable: Boolean;
begin
  Result := False;
  if Assigned(FOwner) then
  begin
    Result := (FOwner.PictureWidth < FPicture.Width) or (FOwner.PictureHeight < FPicture.Height);
  end;
end;

function TPictureItem.GetWidthByHeight: Double;
begin
  if Picture.Height <> 0 then
    Result := Picture.Width / Picture.Height
  else
    Result := 1;
end;

procedure TPictureItem.ItemChanged;
begin
  if Assigned(FOwner) then
    FOwner.Invalidate;
end;

procedure TPictureItem.PictureChanged(Sender: TObject);
begin
  ItemChanged;
end;

procedure TPictureItem.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    ItemChanged;
  end;
end;

procedure TPictureItem.SetHint(const Value: string);
begin
  if FHint <> Value then
  begin
    FHint := Value;
    ItemChanged;
  end;
end;

procedure TPictureItem.SetPicture(const Value: TPicture);
begin
  FPicture.Assign(Value);
end;

procedure TPictureItem.SetSelected(const Value: Boolean);
begin
  if FSelected <> Value then
  begin
    FSelected := Value;
    ItemChanged;
  end;
end;

procedure TPictureItem.SetX(const Value: Integer);
begin
  if FX <> Value then
  begin
    FX := Value;
    ItemChanged;
  end;
end;

procedure TPictureItem.SetY(const Value: Integer);
begin
  if FY <> Value then
  begin
    FY := Value;
    ItemChanged;
  end;
end;

{ TPictureList }

function TPictureList.Add: TPictureItem;
begin
  Result := TPictureItem(inherited Add);
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TPictureList.Create(AOwner: TAdvPicturePane);
begin
  inherited Create(AOwner, TPictureItem);
end;

function TPictureList.GetItem(Index: Integer): TPictureItem;
begin
  Result := TPictureItem(inherited GetItem(Index));
end;

function TPictureList.Insert(Index: Integer): TPictureItem;
begin
  Result := TPictureItem(inherited Insert(Index));
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TPictureList.SetItem(Index: Integer; Value: TPictureItem);
begin
  inherited SetItem(Index, Value);
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TPictureList.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
end;

procedure TPictureList.Update(Item: TCollectionItem);
begin
  inherited;
  (Owner as TAdvPicturePane).PicturesChanged(Self);
end;

end.
