{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2015                                        }
{            Email : info@tmssoftware.com                            }
{            Web : http://www.tmssoftware.com                        }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The complete source code remains property of the author and may    }
{ not be distributed, published, given or sold in any form as such.  }
{ No parts of the source code can be included in any other component }
{ or application without written authorization of the author.        }
{********************************************************************}

unit AdvScrollMenu;

interface

{$I TMSDEFS.INC}

uses
  Classes, Controls, Graphics, Types, Generics.Collections, Messages
  {$IFDEF DELPHIXE6_LVL}
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
  // v1.0.0.1 : Fixed: SelectedItemIndex of -1
  // v1.0.1.0 : Fixed: Issue with drawing imagelist images transparent
  //          : New: Appearance.Spacing property added


type
  TAdvScrollMenu = class;

  TAdvScrollItems = class;

  TAdvScrollItem = class;

  TAdvScrollItemTextAlign = (sitaCenter, sitaLeading, sitaTrailing);

  TAdvItemState = (itsNormal, itsSelected, itsDisabled);

  TOnSelectItemEvent = procedure(Sender: TObject; ACollection: TAdvScrollItems; AItemIndex: Integer; var Allow: Boolean) of object;
  TOnItemAppearanceItemEvent = procedure(Sender: TObject; Acollection: TAdvScrollItems; AItemIndex: Integer; AState: TAdvItemState; var AFill: TBrush; var AFont: TFont; var AStroke: TPen; var ATextColor: TColor) of object;

  TAdvItemAppearance = class(TPersistent)
  private
    FDisabledFont: TFont;
    FSelectedFont: TFont;
    FVerticalTextAlign: TAdvScrollItemTextAlign;
    FDisabledFill: TBrush;
    FFont: TFont;
    FSelectedFill: TBrush;
    FDisabledStroke: TPen;
    FSelectedStroke: TPen;
    FFill: TBrush;
    FHorizontalTextAlign: TAdvScrollItemTextAlign;
    FStroke: TPen;
    FTextColor: TColor;
    FSpacing: Integer;
    FDefaultWidth: Integer;
    FOnChange: TNotifyEvent;
    FSelectedTextColor: TColor;
    FDisabledTextColor: TColor;
    procedure SetDisabledFill(const Value: TBrush);
    procedure SetDisabledFont(const Value: TFont);
    procedure SetDisabledStroke(const Value: TPen);
    procedure SetFill(const Value: TBrush);
    procedure SetFont(const Value: TFont);
    procedure SetHorizontalTextAlign(const Value: TAdvScrollItemTextAlign);
    procedure SetSelectedFill(const Value: TBrush);
    procedure SetSelectedFont(const Value: TFont);
    procedure SetSelectedStroke(const Value: TPen);
    procedure SetStroke(const Value: TPen);
    procedure SetVerticalTextAlign(const Value: TAdvScrollItemTextAlign);
    procedure SetTextColor(const Value: TColor);
    procedure SetDefaultWidth(const Value: Integer);
    procedure SetSelectedTextColor(const Value: TColor);
    procedure SetDisabledTextColor(const Value: TColor);
    procedure SetSpacing(const Value: integer);
  protected
    procedure Change;
    procedure Changed(Sender: TObject);
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    destructor Destroy; override;
  published
    property DefaultWidth: Integer read FDefaultWidth write SetDefaultWidth;

    property Fill: TBrush read FFill write SetFill;
    property Stroke: TPen read FStroke write SetStroke;
    property Font: TFont read FFont write SetFont;

    property SelectedFill: TBrush read FSelectedFill write SetSelectedFill;
    property SelectedStroke: TPen read FSelectedStroke write SetSelectedStroke;
    property SelectedFont: TFont read FSelectedFont write SetSelectedFont;

    property DisabledFill: TBrush read FDisabledFill write SetDisabledFill;
    property DisabledStroke: TPen read FDisabledStroke write SetDisabledStroke;
    property DisabledFont: TFont read FDisabledFont write SetDisabledFont;

    property HorizontalTextAlign: TAdvScrollItemTextAlign read FHorizontalTextAlign write SetHorizontalTextAlign default sitaCenter;
    property VerticalTextAlign: TAdvScrollItemTextAlign read FVerticalTextAlign write SetVerticalTextAlign default sitaCenter;

    property Spacing: integer read FSpacing write SetSpacing default 2;
    property TextColor: TColor read FTextColor write SetTextColor default clBlack;
    property DisabledTextColor: TColor read FDisabledTextColor write SetDisabledTextColor default clBlack;
    property SelectedTextColor: TColor read FSelectedTextColor write SetSelectedTextColor default clBlack;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;

  end;

  TAdvScrollItem = class(TCollectionItem)
  private
    FOwner: TAdvScrollItems;
    FSubItems: TAdvScrollItems;
    FText: string;
    FTextColor: TColor;
    FTag: Integer;
    FState: TAdvItemState;
    FIconIndex: Integer;
    procedure SetSubItems(const Value: TAdvScrollItems);
    procedure SetText(const Value: string);
    procedure SetState(const Value: TAdvItemState);
    procedure SetIconIndex(const Value: Integer);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function AddSubItem(AText: string): TAdvScrollItem; virtual;
  published
    property IconIndex: Integer read FIconIndex write SetIconIndex;
    property SubItems: TAdvScrollItems read FSubItems write SetSubItems;
    property State: TAdvItemState read FState write SetState default itsNormal;
    property Tag: Integer read FTag write FTag default 0;
    property Text: string read FText write SetText;
  end;

  TAdvScrollItemDrawRect = record
    DrawRect, TextRect: TRect;
    Item: TAdvScrollItem;
  end;

  TAdvScrollDrawItems = TList<TAdvScrollItemDrawRect>;

  TAdvScrollItems = class(TOwnedCollection)
  private
    FDrawRects: TAdvScrollDrawItems;
    FOwner: TAdvScrollMenu;
    FScrollPos, FDrawPos: Integer;
    FSelectedItemIndex: Integer;
    FMaxHeight: Integer;
    FMaxMinX: Single;
    FStartX: Integer;
    FStartY: Single;
    FEndY: Single;
    FAppearance: TAdvItemAppearance;
    FAutoSize: Boolean;
    function GetItems(Index: Integer): TAdvScrollItem;
    procedure SetItems(Index: Integer; const Value: TAdvScrollItem);
    procedure SetAppearance(const Value: TAdvItemAppearance);
    procedure SetAutoSize(const Value: Boolean);
    procedure SetSelectedItemIndex(const Value: Integer);
  protected
    procedure UpdateControl;
    procedure Update(Item: TCollectionItem); override;
    procedure CalculateItems; virtual;
    procedure CalculateMaxMin;
    procedure UpdateDisplayList; virtual;
    procedure ResetAppearance; virtual;
    procedure AppearanceChanged(Sender: TObject); virtual;
    function XYToCollection(X, Y: Single): TAdvScrollItems; virtual;
    property DrawRects: TAdvScrollDrawItems read FDrawRects;
  public
    constructor Create(AOwner: TAdvScrollMenu);
    destructor Destroy; override;
    function Add: TAdvScrollItem; overload;
    property Items[Index: Integer]: TAdvScrollItem read GetItems write SetItems; default;
    property MaxMinX: Single read FMaxMinX write FMaxMinX;
    property SelectedItemIndex: Integer read FSelectedItemIndex write SetSelectedItemIndex default 0;
  published
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
    property Appearance: TAdvItemAppearance read FAppearance write SetAppearance;
  end;

  TAdvScrollMenu = class(TCustomControl)
  private
    FMoveFlag: Boolean;
    FMouseX: Single;
    FMouseY: Single;
    FIsMouseClick: Boolean;
    FUpdateCount: Integer;
    FItems: TAdvScrollItems;
    FActiveCollection: TAdvScrollItems;
    FImages: TImageList;
    FOnSelect: TOnSelectItemEvent;
    FOnItemAppearance: TOnItemAppearanceItemEvent;
    procedure SetItems(const Value: TAdvScrollItems);
    procedure SetImages(const Value: TImageList);
    function GetVersion: String;
    procedure SetVersion(const Value: String);
  protected
    procedure Init;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure Resize; override;
    procedure CalculateItems;
    procedure DrawItems(AItems: TAdvScrollItems; X, Y: Integer);
    function XToCollectionItem(X: Single; ACollection: TAdvScrollItems): TAdvScrollItem;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    function AddItem(AText: string): TAdvScrollItem; virtual;
    procedure Loaded; override;
  published
    property Align;
    property Anchors;
    property Hint;
    property Items: TAdvScrollItems read FItems write SetItems;
    property Images: TImageList read FImages write SetImages;
    property ShowHint;
    property Version: String read GetVersion write SetVersion;
    property Visible;

    property OnItemAppearance: TOnItemAppearanceItemEvent read FOnItemAppearance write FOnItemAppearance;
    property OnSelect: TOnSelectItemEvent read FOnSelect write FOnSelect;
  end;

implementation

uses
  Math, SysUtils, Windows;

{ TAdvItemAppearance }

procedure TAdvItemAppearance.Assign(Source: TPersistent);
begin
  if Source is TAdvItemAppearance then
  begin
    FDefaultWidth := (Source as TAdvItemAppearance).DefaultWidth;
    FFill.Assign((Source as TAdvItemAppearance).Fill);
    FStroke.Assign((Source as TAdvItemAppearance).Stroke);
    FFont.Assign((Source as TAdvItemAppearance).Font);
    FTextColor := (Source as TAdvItemAppearance).TextColor;
    FSelectedFill.Assign((Source as TAdvItemAppearance).SelectedFill);
    FSelectedStroke.Assign((Source as TAdvItemAppearance).SelectedStroke);
    FSelectedTextColor := (Source as TAdvItemAppearance).SelectedTextColor;
    FDisabledFill.Assign((Source as TAdvItemAppearance).DisabledFill);
    FDisabledStroke.Assign((Source as TAdvItemAppearance).DisabledStroke);
    FDisabledFont.Assign((Source as TAdvItemAppearance).DisabledFont);
    FDisabledTextColor := (Source as TAdvItemAppearance).DisabledTextColor;
    FHorizontalTextAlign := (Source as TAdvItemAppearance).HorizontalTextAlign;
    FVerticalTextAlign := (Source as TAdvItemAppearance).VerticalTextAlign;
  end;
end;

procedure TAdvItemAppearance.Change;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TAdvItemAppearance.Changed(Sender: TObject);
begin
  Change;
end;

constructor TAdvItemAppearance.Create;
begin
  inherited;

  FHorizontalTextAlign := sitaCenter;
  FVerticalTextAlign := sitaCenter;

  FFill := TBrush.Create;
  FFill.Color := clLtGray;
  FStroke := TPen.Create;
  FStroke.Color := clDkGray;
  FSelectedFill := TBrush.Create;
  FSelectedFill.Color := clSkyBlue;
  FSelectedStroke := TPen.Create;
  FSelectedStroke.Color := clDkGray;
  FDisabledFill := TBrush.Create;
  FDisabledFill.Color := clDkGray;
  FDisabledStroke := TPen.Create;
  FDisabledStroke.Color := clDkGray;

  FFill.OnChange := Changed;
  FStroke.OnChange := Changed;
  FSelectedFill.OnChange := Changed;
  FSelectedStroke.OnChange := Changed;
  FDisabledFill.OnChange := Changed;
  FDisabledStroke.OnChange := Changed;


  FDefaultWidth := 100;
  FFont := TFont.Create;
  FFont.OnChange := Changed;
  FSelectedFont := TFont.Create;
  FSelectedFont.OnChange := Changed;
  FDisabledFont := TFont.Create;
  FDisabledFont.OnChange := Changed;

  FSpacing := 2;
  FTextColor := clBlack;
  FSelectedTextColor := clBlack;
  FDisabledTextColor := clBlack;
end;

destructor TAdvItemAppearance.Destroy;
begin
  FFill.Free;
  FStroke.Free;
  FFont.Free;

  FSelectedFill.Free;
  FSelectedStroke.Free;
  FSelectedFont.Free;

  FDisabledFill.Free;
  FDisabledStroke.Free;
  FDisabledFont.Free;
  inherited;
end;

procedure TAdvItemAppearance.SetDefaultWidth(const Value: Integer);
begin
  if FDefaultWidth <> Value then
  begin
    FDefaultWidth := Value;
    Change;
  end;
end;

procedure TAdvItemAppearance.SetDisabledFill(const Value: TBrush);
begin
  if FDisabledFill <> Value then
  begin
    FDisabledFill.Assign(Value);
    Change;
  end;
end;

procedure TAdvItemAppearance.SetDisabledFont(const Value: TFont);
begin
  if FDisabledFont <> Value then
  begin
    FDisabledFont.Assign(Value);
    Change;
  end;
end;

procedure TAdvItemAppearance.SetDisabledStroke(const Value: TPen);
begin
  if FDisabledStroke <> Value then
  begin
    FDisabledStroke.Assign(Value);
    Change;
  end;
end;

procedure TAdvItemAppearance.SetDisabledTextColor(const Value: TColor);
begin
  if FDisabledTextColor <> Value then
  begin
    FDisabledTextColor := Value;
    Change;
  end;
end;

procedure TAdvItemAppearance.SetFill(const Value: TBrush);
begin
  if FFill <> Value then
  begin
    FFill.Assign(Value);
    Change;
  end;
end;

procedure TAdvItemAppearance.SetFont(const Value: TFont);
begin
  if FFont <> Value then
  begin
    FFont.Assign(Value);
    Change;
  end;
end;

procedure TAdvItemAppearance.SetHorizontalTextAlign(const Value: TAdvScrollItemTextAlign);
begin
  if FHorizontalTextAlign <> Value then
  begin
    FHorizontalTextAlign := Value;
    Change;
  end;
end;

procedure TAdvItemAppearance.SetSelectedFill(const Value: TBrush);
begin
  if FSelectedFill <> Value then
  begin
    FSelectedFill.Assign(Value);
    Change;
  end;
end;

procedure TAdvItemAppearance.SetSelectedFont(const Value: TFont);
begin
  if FSelectedFont <> Value then
    FSelectedFont.Assign(Value);
end;

procedure TAdvItemAppearance.SetSelectedStroke(const Value: TPen);
begin
  if FSelectedStroke <> Value then
  begin
    FSelectedStroke.Assign(Value);
    Change;
  end;
end;

procedure TAdvItemAppearance.SetSelectedTextColor(const Value: TColor);
begin
  if FSelectedTextColor <> Value then
  begin
    FSelectedTextColor := Value;
    Change;
  end;
end;

procedure TAdvItemAppearance.SetSpacing(const Value: integer);
begin
  if (FSpacing <> Value) then
  begin
    FSpacing := Value;
    Change;
  end;
end;

procedure TAdvItemAppearance.SetStroke(const Value: TPen);
begin
  if FStroke <> Value then
  begin
    FStroke.Assign(Value);
    Change;
  end;
end;

procedure TAdvItemAppearance.SetTextColor(const Value: TColor);
begin
  if FTextColor <> Value then
  begin
    FTextColor := Value;
    Change;
  end;
end;

procedure TAdvItemAppearance.SetVerticalTextAlign(const Value: TAdvScrollItemTextAlign);
begin
  if FVerticalTextAlign <> Value then
  begin
    FVerticalTextAlign := Value;
    Change;
  end;
end;

{ TAdvScrollItem }

function TAdvScrollItem.AddSubItem(AText: string): TAdvScrollItem;
begin
  Result := nil;
  if Assigned(SubItems) then
  begin
    Result := SubItems.Add;
    Result.Text := AText;
  end;
end;

constructor TAdvScrollItem.Create(Collection: TCollection);
begin
  inherited;
  FOwner := (Collection as TAdvScrollItems);
  FSubItems := TAdvScrollItems.Create(FOwner.FOwner);
  FState := itsNormal;
  FTextColor := clBlack;
  FTag := 0;
  FIconIndex := -1;

  FText := 'Item '+ IntToStr(Index + 1);
end;

destructor TAdvScrollItem.Destroy;
begin
  FSubItems.Free;
  inherited;
end;

procedure TAdvScrollItem.SetIconIndex(const Value: Integer);
begin
  if FIconIndex <> Value then
  begin
    FIconIndex := Value;
    Changed(False);
  end;
end;

procedure TAdvScrollItem.SetState(const Value: TAdvItemState);
begin
  if (FState <> Value) and (Self <> nil) then
  begin
//    FOwner.Items[FOwner.SelectedItemIndex].FState := itsNormal;

    FState := Value;

    FOwner.FOwner.FActiveCollection := FOwner;
//    if Value = itsSelected then
//    begin
//      FOwner.SelectedItemIndex := Index;
//    end;

    Changed(False);
  end;
end;

procedure TAdvScrollItem.SetSubItems(const Value: TAdvScrollItems);
begin
  FSubItems.Assign(Value);
end;

procedure TAdvScrollItem.SetText(const Value: string);
begin
  if (FText <> Value) then
  begin
    FText := Value;
    Changed(false);
  end;
end;

{ TAdvScrollItems }

function TAdvScrollItems.Add: TAdvScrollItem;
begin
  Result := TAdvScrollItem(inherited Add);
  UpdateControl;
  FOwner.Width := FOwner.Width;
end;

procedure TAdvScrollItems.AppearanceChanged(Sender: TObject);
begin
  UpdateControl;
end;

procedure TAdvScrollItems.CalculateItems;
var
  I: Integer;
  it: TAdvScrollItem;
  bmp: Graphics.TBitmap;
  dr: TAdvScrollItemDrawRect;
  x, y, tw, th: Integer;
  txt: String;
  isz, ic: Integer;
  s, e: Integer;
begin
  if (csLoading in FOwner.ComponentState) or (FOwner.FUpdateCount > 0) then
    Exit;

  CalculateMaxMin();

  FDrawRects.Clear;
  bmp := Graphics.TBitmap.Create;

  isz := 0;
  if Not AutoSize then
  begin
    //FIXED ITEM SIZE
    s := 0;
    e := -1;
    isz := Appearance.DefaultWidth;
    ic := Count * isz;
    if ic > 0 then
    begin
      s := Max(0, Min(Count - 1, Floor(-FScrollPos / ic * Count - 1)));
      e := Max(0, Min(Count - 1, Ceil((-FScrollPos + FOwner.Width) / ic * Count - 1)));
    end;
  end
  else
  begin
    // CALCULATED ITEM SIZE
    s := 0;
    e := Count - 1;
  end;

  x := 0;
  y := 0;
  FMaxHeight := 0;
  for I := s to e do
  begin
    it := Items[I];
    bmp.Canvas.Font.Assign(Appearance.Font);
    txt := it.Text;
    if txt <> '' then
      th := bmp.Canvas.TextHeight(txt) + 10
    else
      th := bmp.Canvas.TextHeight('gh') + 10;

    if (Not AutoSize) then
    begin
     //FIXED ITEM SIZE
      tw := isz;
    end
    else
    begin
      //CALCULATED ITEM SIZE
      tw := bmp.Canvas.TextWidth(txt) + 2 * Appearance.Spacing;
      if Assigned(FOwner.Images) and (it.IconIndex >= 0) then
        tw := tw + FOwner.Images.Width + 2;
    end;

    dr.DrawRect := Rect(x, y, x + tw, y + th);

    if (X <= (FOwner.Width - FScrollPos)) then
    begin
      if dr.DrawRect.Bottom - dr.DrawRect.Top > FMaxHeight then
        FMaxHeight := dr.DrawRect.Bottom - dr.DrawRect.Top;

      dr.Item := it;
      x := x + dr.DrawRect.Right - dr.DrawRect.Left;
      FDrawRects.Add(dr);

      if Assigned(it.SubItems) then
      begin
        it.SubItems.FDrawPos := FDrawPos + FMaxHeight;
        it.SubItems.CalculateItems;
      end;
    end
    else
      Break;
  end;
  bmp.Free;
end;

procedure TAdvScrollItems.CalculateMaxMin;
var
  I: Integer;
  r: TRect;
  it: TAdvScrollItem;
  x, y, tw, isz: integer;
  bmp: Graphics.TBitmap;
  txt: String;
begin

  isz := 0;
  if Not AutoSize then
  begin
    isz := Appearance.DefaultWidth;
  end;
  x := 0;
  y := 0;
  bmp := Graphics.TBitmap.Create;
  for I := 0 to Self.Count - 1 do
  begin
    it := Items[I];
    txt := it.Text;
    bmp.Canvas.Font.Assign(Appearance.Font);

    if (Not AutoSize) then
    begin
     //FIXED ITEM SIZE
      tw := isz;
    end
    else
    begin
      //CALCULATED ITEM SIZE
      tw := bmp.Canvas.TextWidth(txt) + 20;
    end;

    r := Rect(x, y, x + tw, y);

    x := x + (r.Right - r.Left);

  end;

  MaxMinX := FOwner.Width - x;
  if MaxMinX > 0 then
    MaxMinX := 0;

  bmp.Free;
end;

constructor TAdvScrollItems.Create(AOwner: TAdvScrollMenu);
begin
  inherited Create(AOwner, TAdvScrollItem);
  FAutoSize := True;
  FMaxMinX := 0;
  FSelectedItemIndex := 0;
  FOwner := AOwner;
  FDrawRects := TList<TAdvScrollItemDrawRect>.Create;
  FAppearance := TAdvItemAppearance.Create;
  FAppearance.OnChange := AppearanceChanged;
end;

destructor TAdvScrollItems.Destroy;
begin
  FDrawRects.Free;
  FAppearance.Free;
  inherited;
end;

function TAdvScrollItems.GetItems(Index: Integer): TAdvScrollItem;
begin
  Result := TAdvScrollItem(inherited Items[Index]);
end;

procedure TAdvScrollItems.ResetAppearance;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Items[I].State:= itsNormal;
  end;
end;

procedure TAdvScrollItems.SetAppearance(const Value: TAdvItemAppearance);
begin
  if FAppearance <> Value then
    FAppearance.Assign(Value);
end;

procedure TAdvScrollItems.SetAutoSize(const Value: Boolean);
begin
  FAutoSize := Value;
  UpdateControl;
end;

procedure TAdvScrollItems.SetItems(Index: Integer; const Value: TAdvScrollItem);
begin
  inherited Items[Index] := Value;
end;

procedure TAdvScrollItems.SetSelectedItemIndex(const Value: Integer);
begin
  if (Count > 0) and (Value < Count) then
  begin
    FSelectedItemIndex := Value;

    ResetAppearance;

    if Value > -1 then
      Items[Value].State := itsSelected;
  end;
end;

procedure TAdvScrollItems.Update(Item: TCollectionItem);
begin
  inherited;
  UpdateControl;
end;

procedure TAdvScrollItems.UpdateControl;
begin
  FOwner.CalculateItems;
  FOwner.Repaint;
end;

procedure TAdvScrollItems.UpdateDisplayList;
begin

end;

function TAdvScrollItems.XYToCollection(X, Y: Single): TAdvScrollItems;
var
  Mh: Integer;
  r: TRect;
begin
  if (SelectedItemIndex > DrawRects.Count) or (SelectedItemIndex < 0) then
    SelectedItemIndex := 0;

  if (not Assigned(DrawRects[SelectedItemIndex].Item)) then
    Exit(nil);

  r := DrawRects[SelectedItemIndex].DrawRect;
  Mh := r.Bottom - r.Top;

  if Mh < Y then
  begin
    if Items[SelectedItemIndex].SubItems.Count > 0 then
      Result := Items[SelectedItemIndex].SubItems.XYToCollection(X, Y - Mh)
    else
      Result := nil;
  end
  else
    Result := Self;

  if not Assigned(Result) then
    Result := nil;
end;

{ TAdvScrollMenu }

function TAdvScrollMenu.AddItem(AText: string): TAdvScrollItem;
begin
  Result := Items.Add;
  Result.Text := AText;
  CalculateItems;
  Repaint;
end;

procedure TAdvScrollMenu.CalculateItems;
begin
  if (csLoading in ComponentState) or (csDestroying in ComponentState) or (FUpdateCount > 0) then
    Exit;

  Items.CalculateItems;
end;

constructor TAdvScrollMenu.Create(AOwner: TComponent);
var
  FDesignTime: Boolean;
begin
  inherited;
  Self.DoubleBuffered := True;
  Brush.Color := clWhite;
  FIsMouseClick := False;
  FItems := TAdvScrollItems.Create(Self);

  FDesignTime := (csDesigning in ComponentState) and not
                 ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  if FDesignTime then
    Init;
end;

destructor TAdvScrollMenu.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TAdvScrollMenu.DrawItems(AItems: TAdvScrollItems; X, Y: Integer);
var
  I: Integer;
  dr: TAdvScrollItemDrawRect;
  it: TAdvScrollItem;
  r, br: TRect;
  col: TAdvScrollItems;
  fill: TBrush;
  stroke: TPen;
  font: TFont;
  color: TColor;
  style: Cardinal;
begin
  //Canvas
  if not Assigned(AItems) then
    Exit;

  //FIXED ITEM SIZE
  if AItems.AutoSize then
  begin
    if AItems.DrawRects.Count > 0 then
      X := X + AItems.Appearance.DefaultWidth * AItems.DrawRects[0].Item.Index + AItems.FScrollPos;
  end
  else
  begin
  //CALCULATED ITEM SIZE
    X := X + AItems.FScrollPos;
  end;

  stroke := TPen.Create;
  stroke.Color := clBlack;
  fill := TBrush.Create;
  fill.Color := clwhite;
  font := TFont.Create;
  color := clwhite;

  for I := 0 to AItems.DrawRects.Count - 1 do
  begin
    dr := AItems.DrawRects[I];
    it := dr.Item;
    r := dr.DrawRect;
    r.Top := r.Top + Y - 1;
    r.Left := R.Left + X;
    r.Right := r.Right + X + 1;
    r.Bottom := r.Bottom + Y;

    case It.State of
      itsNormal:
      begin
        fill.Assign(AItems.Appearance.Fill);
        stroke.Assign(AItems.Appearance.Stroke);
        font.Assign(AItems.Appearance.Font);
        color := AItems.Appearance.TextColor;
      end;
      itsSelected:
      begin
        fill.Assign(AItems.Appearance.SelectedFill);
        stroke.Assign(AItems.Appearance.SelectedStroke);
        font.Assign(AItems.Appearance.SelectedFont);
        color := AItems.Appearance.SelectedTextColor;
      end;
      itsDisabled:
      begin
        fill.Assign(AItems.Appearance.DisabledFill);
        stroke.Assign(AItems.Appearance.DisabledStroke);
        font.Assign(AItems.Appearance.DisabledFont);
        color := AItems.Appearance.DisabledTextColor;
      end;
    end;

    if Assigned(OnItemAppearance) then
      OnItemAppearance(Self, AItems, I, it.State, fill, font, stroke, color);

    Canvas.Brush.Assign(fill);
    Canvas.Pen.Assign(stroke);
    Canvas.Font.Assign(font);

    Canvas.Rectangle(r.Left, r.Top, r.Right, r.Bottom);

    r.Left := r.Left + AItems.Appearance.Spacing;
    r.Right := r.Right - AItems.Appearance.Spacing;

    if Assigned(it) and Assigned(it.FOwner) and Assigned(it.SubItems) then
    begin

      style := DT_END_ELLIPSIS or DT_NOPREFIX or DT_SINGLELINE or DT_EDITCONTROL;
      case it.FOwner.Appearance.HorizontalTextAlign of
        sitaCenter:
        begin
          style := style or DT_CENTER;
        end;
        sitaLeading:
        begin
          style := style or DT_Left;
          r.Left := r.Left + 1;
        end;
        sitaTrailing:
        begin
          style := style or DT_RIGHT;
          r.Right := r.Right - 1;
        end;
      end;

      case it.FOwner.Appearance.VerticalTextAlign of
        sitaLeading:
        begin
          style := style or DT_TOP;
          r.Top := R.Top + 1;
        end;
        sitaCenter:
          style := style or DT_VCENTER;
        sitaTrailing:
        begin
          style := style or DT_BOTTOM;
          r.Bottom := r.Bottom - 1;
        end;
      end;

      {$REGION 'images'}
      if (Images <> nil) and (it.IconIndex <> - 1) then
      begin
        if it.FOwner.Appearance.HorizontalTextAlign <> sitaTrailing then
        begin
          br := Rect(r.Left + 0, r.Top, r.Left + Images.Width + 0, r.Top + Images.Height);
        end
        else
        begin
          br := Rect(r.Right - Images.Width - 2, r.Top, r.Right - 2, r.Top + Images.Height);
        end;

        case it.FOwner.Appearance.VerticalTextAlign of
          sitaLeading: ;
          sitaCenter:
          begin
            br.Top := r.Top + Round(((r.Bottom - r.Top) / 2) - (images.Height / 2));
            br.Bottom := br.Top + Images.Height;
          end;
          sitaTrailing:
          begin
            br.Top := r.Bottom - Images.Height - 2;
            br.Bottom := br.Top + Images.Height;
          end;
        end;

        Images.Draw(Canvas, Br.Left, Br.Top, it.IconIndex);

        case it.FOwner.Appearance.HorizontalTextAlign of
          sitaCenter: r.Left := r.Left + Images.Width + 2;
          sitaLeading: r.Left := r.Left + Images.Width + 5;
          sitaTrailing: r.Right := r.Right - Images.Width - 5;
        end;
      end;
      {$ENDREGION}

      DrawText(Canvas.Handle, Pchar(it.Text), Length(it.Text), r, Style);
    end;

    col := (it.Collection as TAdvScrollItems);
    if Assigned(col) and (col.SelectedItemIndex <> -1) then
    begin
      it := col.Items[col.SelectedItemIndex];

      if Assigned(it.SubItems) and (it.SubItems.Count > 0) then
      begin
        if AItems.Appearance.Stroke.Color <> it.SubItems.Appearance.Stroke.Color then
          DrawItems(it.SubItems, 1, it.SubItems.FDrawPos + 1)
        else
          DrawItems(it.SubItems, 1, it.SubItems.FDrawPos);
      end;
    end;
  end;

  fill.Free;
  stroke.Free;
  font.Free;
end;

function Hiword(L: DWORD): integer;
begin
  Result := L shr 16;
end;

function LoWord(L: DWORD): Integer;
begin
  Result := L AND $FFFF;
end;

function MakeWord(b1,b2: integer): integer;
begin
  Result := b1 or b2 shl 8;
end;

function MakeLong(i1,i2: integer): integer;
begin
  Result := i1 or i2 shl 16;
end;

function GetVersionNumber(AMaj, AMin, ARel, ABld: ShortInt): string;
var
  vn: Integer;
begin
  vn := MakeLong(MakeWord(ABld, ARel),MakeWord(AMin, AMaj));
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TAdvScrollMenu.GetVersion: String;
begin
  Result := GetVersionNumber(MAJ_VER, MIN_VER, REL_VER, BLD_VER);
end;

procedure TAdvScrollMenu.Init;
begin
  Items.Appearance.SelectedFill.Color := clSkyBlue;

  Width := 500;
  Height := 100;

  AddItem('Item 1');
  Items[0].AddSubItem('Subitem 1').FOwner.AutoSize := False;
  Items[0].AddSubItem('Subitem 2');
  AddItem('Item 2');
  AddItem('Item 3');
end;

procedure TAdvScrollMenu.Loaded;
begin
  inherited;
  CalculateItems;
end;

procedure TAdvScrollMenu.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Col: TAdvScrollItems;
begin
  inherited;
  FMouseX := X;
  FMouseY := Y;
  FMoveFlag := True;
  Col := Items.XYToCollection(X, Y);
  if Col <> nil then
  begin
    FActiveCollection := Col;
    Col.FStartX := X;
    Col.FStartY := Y;
    Col.FEndY := Y + Col.FMaxHeight;
  end;
end;

procedure TAdvScrollMenu.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NextScroll: Integer;
begin
  inherited;
  FIsMouseClick := (Abs(FMouseX - X) <= 4) and (Abs(FMouseY - Y) <= 4);

  if FMoveFlag and (not FIsMouseClick) then
  begin
    if Assigned(FActiveCollection) then
    begin
      NextScroll := FActiveCollection.FScrollPos - (FActiveCollection.FStartX - X);
      FActiveCollection.CalculateMaxMin;
      if (NextScroll >= FActiveCollection.MaxMinX) and (NextScroll <= 0) then
      begin
        FActiveCollection.FScrollPos := NextScroll;
        FActiveCollection.FStartX := X;
        FActiveCollection.CalculateItems;
        Repaint;
      end;
    end;
  end;
end;

procedure TAdvScrollMenu.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  col: TadvScrollItems;
  colI: TAdvScrollItem;
  allow: Boolean;
begin
  inherited;
  FMoveFlag := False;

  if FIsMouseClick or ((X = FMouseX) and (Y = FMouseY)) then
  begin
    col := items.XYToCollection(FMouseX, FMouseY);
    if Col <> nil then
    begin
      col.ResetAppearance;
      FActiveCollection := Col;
      colI := XToCollectionItem(FMouseX, col);
      if Assigned(colI) then
      begin
        colI.State:= itsSelected;
        col.SelectedItemIndex := colI.Index;

        Allow := True;
        if Assigned(OnSelect) then
          OnSelect(Self, Col, col.SelectedItemIndex, Allow);
      end;
    end;
  end;
end;

procedure TAdvScrollMenu.Notification(AComponent: TComponent; AOperation: TOperation);
begin
  inherited;
  if (AComponent = FImages) and (AOperation = opRemove) then
    FImages := nil;
end;

procedure TAdvScrollMenu.Paint;
begin
  inherited;
  DrawItems(Items, 1, 1);
end;

procedure TAdvScrollMenu.Resize;
begin
  inherited;
  CalculateItems;
  Repaint;
end;

procedure TAdvScrollMenu.SetImages(const Value: TImageList);
begin
  FImages := Value;
end;

procedure TAdvScrollMenu.SetItems(const Value: TAdvScrollItems);
begin
  FItems.Assign(Value);
end;

procedure TAdvScrollMenu.SetVersion(const Value: String);
begin

end;

function TAdvScrollMenu.XToCollectionItem(X: Single;
  ACollection: TAdvScrollItems): TAdvScrollItem;
var
  I: Integer;
  Tx: Single;
  r: TRect;
begin
  Result := nil;

  X := X - ACollection.FScrollPos;

  Tx := 0;

  for I := 0 to ACollection.Count - 1 do
  begin
    r := ACollection.DrawRects[I].DrawRect;
    Tx := Tx + r.Right - r.Left;
    if X <= Tx then
      Exit(ACollection.FDrawRects[I].Item);
  end;
end;

{$IFDEF FREEWARE}
function Scramble(s:string): string;
var
  r:string;
  i: integer;
  c: char;
  b: byte;
begin
  r := '';
  for i := 1 to length(s) do
  begin
    b := ord(s[i]);
    b := (b and $E0) + ((b and $1F) xor 5);
    c := chr(b);
    r := r + c;
  end;
  Result := r;
end;
{$ENDIF}
initialization
begin
{$IFDEF FREEWARE}
   if  (FindWindow(PChar(Scramble('QDuuilfdqljk')), nil) = 0) OR
       (FindWindow(PChar(Scramble('QDuuGplia`w')), nil) = 0) then
   begin
     MessageBox(0,PChar(Scramble('Duuilfdqljk%pv`v%qwldi%s`wvljk%jc%QHV%vjcqrdw`%fjhujk`kqv+')+#13#10+Scramble('Fjkqdfq%QHV%vjcqrdw`%mqqu?**rrr+qhvvjcqrdw`+fjh%cjw%sdila%ilf`kvlkb+')),PChar(Scramble('Rdwklkb')),MB_OK);
   end;
{$ENDIF}
end;

end.
