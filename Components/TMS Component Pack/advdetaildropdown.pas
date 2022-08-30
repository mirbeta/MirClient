{ *************************************************************************** }
{ TAdvDetailDropDown components                                               }
{ for Delphi & C++Builder                                                     }
{                                                                             }
{ written by TMS Software                                                     }
{ copyright © 2011 - 2015                                                     }
{ Email : info@tmssoftware.com                                                }
{ Web : http://www.tmssoftware.com                                            }
{                                                                             }
{ The source code is given as is. The author is not responsible               }
{ for any possible damage done due to the use of this code.                   }
{ The component can be freely used in any application. The complete           }
{ source code remains property of the author and may not be distributed,      }
{ published, given or sold in any form as such. No parts of the source        }
{ code can be included in any other component or application without          }
{ written authorization of the author.                                        }
{ *************************************************************************** }

{$I TMSDEFS.INC}

unit AdvDetailDropDown;

interface

uses
  Classes, Windows, Graphics, Controls, Messages, ImgList, ExtCtrls, Math,
  AdvDropDown, AsgHTMLE, AdvStyleIF, SysUtils;

type
  // Fixed : Issue with setting ItemIndex = -1

{$IFNDEF DELPHI2006_LVL}
  TVerticalAlignment = (taAlignTop, taAlignBottom, taVerticalCenter);
{$ENDIF}

  TDetailItem = class(TCollectionItem)
  private
    FCaption: TCaption;
    FImageIndex: Integer;
    FEnabled: Boolean;
    FImage: TPicture;
    FRect: TRect;
    FNotes: string;
    FItemObject: TObject;
    FTag: integer;
    procedure SetCaption(const Value: TCaption);
    procedure SetImage(const Value: TPicture);
    procedure SetImageIndex(const Value: Integer);
  protected
    procedure Changed;
    property Rect: TRect read FRect write FRect;
    property Enabled: Boolean read FEnabled write FEnabled default True;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: TCaption read FCaption write SetCaption;
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    property Image: TPicture read FImage write SetImage;
    property Notes: string read FNotes write FNotes;
    property Tag: integer read FTag write FTag;
    property ItemObject: TObject read FItemObject write FItemObject;
  end;

  TDetailItems = class(TCollection)
  private
    FMyOwner: TPersistent;
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TDetailItem;
    procedure SetItem(Index: Integer; const Value: TDetailItem);
  protected
    procedure Change;
  public
    constructor Create(AOwner: TPersistent);
    property Items[Index: Integer]: TDetailItem read GetItem write SetItem; default;
    function Add: TDetailItem;
    function Insert(Index: Integer): TDetailItem;
    function GetOwner: TPersistent; override;
    function IndexOf(AValue: string): integer;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvDetailDropDown = class(TAdvCustomDropDown, ITMSStyle)
  private
    FItemSelector: TAdvCustomItemSelector;
    FLayout: TItemLayout;
    FItems: TDetailItems;
    FItemIndex: Integer;
    FOnSelect: TNotifyEvent;
    FOnDrawSelectedImage: TOnDrawSelectedItem;
    FItemAppearance: TItemAppearance;
    FInternalCall: Boolean;
    FKeyTimer: TTimer;
    FCurSearch: string;
    FOldItemIndex: Integer;
    FItemHeight: integer;
    FCaptionFont: TFont;
    FNotesFont: TFont;
    FWorkMode: boolean;
    FItemChange: Boolean;
    FItemIdx: Integer;
    FItemSel: Integer;
    FOldValue: string;
    FMatchCase: boolean;
    FMatchStart: boolean;
    FImageVAlign: TVerticalAlignment;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMKeyDown(var Msg: TWMKeyDown); message WM_KEYDOWN;
    //procedure DrawSelectedImage;
    procedure OnItemsChanged(Sender: TObject);
    procedure OnSelectorItemSelect(Sender: TObject);
    procedure OnKeyTimerTime(Sender: TObject);
    procedure SetItemIndex(const Value: Integer);
    procedure SetLayout(const Value: TItemLayout);
    procedure SetItems(const Value: TDetailItems);
    procedure AssignedItemsToItemSelector;
    procedure SetSelectorProperties;
    procedure SetItemAppearance(const Value: TItemAppearance);
    procedure SetCaptionFont(const Value: TFont);
    procedure SetNotesFont(const Value: TFont);
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CreateDropDownForm; override;
    procedure BeforeDropDown; override;
    procedure OnHideDropDown; override;
    procedure UpdateDropDownSize; override;
    procedure DoHideDropDown(Cancelled: Boolean); override;
    procedure DoShowDropDown; override;
    procedure SetEditRect; override;
    procedure SetSelectionColorStyle(const Value: TSelectionColorStyle); override;
    procedure DrawItem(Sender: TObject; Canvas: TCanvas; R: TRect; Index: Integer);
    procedure ItemSize(Sender: TObject; var ASize: TSize);
    procedure DropDownResize(Sender: TObject);
    procedure OnDropDownSizing; override;
    procedure Change; override;
    procedure HandleMouseWheelDown; override;
    procedure HandleMouseWheelUp; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetComponentStyle(AStyle: TTMSStyle); override;

    procedure SelectFirst;
    procedure SelectLast;
    procedure SelectNext;
    procedure SelectPrevious;
    property MatchCase: Boolean read FMatchCase write FMatchCase default False;
    property MatchStart: Boolean read FMatchStart write FMatchStart default true;

  published
    property ImageVAlign: TVerticalAlignment read FImageVAlign write FImageVAlign default taVerticalCenter;
    property Items: TDetailItems read FItems write SetItems;
    property ItemIndex: Integer read FItemIndex write SetItemIndex default - 1;
    property ItemHeight: Integer read FItemHeight write FItemHeight default 22;
    property Layout: TItemLayout read FLayout write SetLayout default ilCaptionRight;
    property ItemAppearance: TItemAppearance read FItemAppearance write SetItemAppearance;
    property CaptionFont: TFont read FCaptionFont write SetCaptionFont;
    property NotesFont: TFont read FNotesFont write SetNotesFont;

    property Align;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Constraints;
    property Ctl3D;
    property BorderColor;
    property DisabledBorder;
    property FocusBorderColor;

    property ImeMode;
    property ImeName;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;

    property DropDownColor;
    property DropDownBorderColor;
    property DropDownBorderWidth;
    property DropDownShadow;
    property DropDownWidth;
    property DropDownHeight;
    property DropPosition;
    property DropDownButtonWidth;
    property DropDownButtonHint;
    property DropDownButtonGlyph;
    property EditorEnabled;
    property Enabled;
    property Font;
    property Images;

    property LabelCaption;
    property LabelPosition;
    property LabelMargin;
    property LabelTransparent;
    property LabelAlwaysEnabled;
    property LabelFont;

    property Version;
    property ButtonAppearance;
    property DropDownHeader;
    property DropDownFooter;
    property DragCursor;
    property DragKind;
    property DragMode;
    property TabStop;
    property TabOrder;
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
    property OnDrawSelectedImage: TOnDrawSelectedItem read FOnDrawSelectedImage write FOnDrawSelectedImage;
    property OnEnter;
    property OnExit;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    {$IFDEF DELPHI2006_LVL}
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnBeforeDropDown;
    property OnDropDown;
    property OnDropUp;
    property OnDropDownHeaderButtonClick;
    property OnDropDownFooterButtonClick;
    property OnDrawHeader;
    property OnDrawFooter;
    property OnGetHeaderText;
    property OnGetFooterText;
    property OnGetDropDownPos;
  end;

implementation

uses StdCtrls;

type
  TInternalItemSelector = class(TAdvCustomItemSelector);
  TInternalItemAppearance = class(TItemAppearance);

// ------------------------------------------------------------------------------

{ TDetailItem }

procedure TDetailItem.Assign(Source: TPersistent);
begin
  if (Source is TDetailItem) then
  begin
    Caption := (Source as TDetailItem).Caption;
    ImageIndex := (Source as TDetailItem).ImageIndex;
    Image.Assign((Source as TDetailItem).Image);
    Enabled := (Source as TDetailItem).Enabled;
    Tag := (Source as TDetailItem).Tag;
    ItemObject := (Source as TDetailItem).ItemObject;
  end
  else
    inherited Assign(Source);
end;

// ------------------------------------------------------------------------------

procedure TDetailItem.Changed;
begin
  TDetailItems(Collection).Change;
end;

// ------------------------------------------------------------------------------

constructor TDetailItem.Create(Collection: TCollection);
begin
  inherited;
  FCaption := '';
  FImageIndex := -1;
  FImage := TPicture.Create;
  FEnabled := True;
end;

// ------------------------------------------------------------------------------

destructor TDetailItem.Destroy;
begin
  FImage.Free;
  inherited;
end;

// ------------------------------------------------------------------------------

procedure TDetailItem.SetCaption(const Value: TCaption);
begin
  if (FCaption <> Value) then
  begin
    FCaption := Value;
    Changed;
  end;
end;

// ------------------------------------------------------------------------------

procedure TDetailItem.SetImage(const Value: TPicture);
begin
  FImage.Assign(Value);
end;

// ------------------------------------------------------------------------------

procedure TDetailItem.SetImageIndex(const Value: Integer);
begin
  if (FImageIndex <> Value) then
  begin
    FImageIndex := Value;
    Changed;
  end;
end;

// ------------------------------------------------------------------------------

{ TDetailItems }

function TDetailItems.Add: TDetailItem;
begin
  Result := TDetailItem( inherited Add);
end;

// ------------------------------------------------------------------------------

procedure TDetailItems.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

// ------------------------------------------------------------------------------

constructor TDetailItems.Create(AOwner: TPersistent);
begin
  inherited Create(TDetailItem);
  FMyOwner := AOwner;
end;

// ------------------------------------------------------------------------------

function TDetailItems.GetItem(Index: Integer): TDetailItem;
begin
  Result := TDetailItem( inherited Items[Index]);
end;

// ------------------------------------------------------------------------------

function TDetailItems.GetOwner: TPersistent;
begin
  Result := FMyOwner;
end;

// ------------------------------------------------------------------------------

function TDetailItems.IndexOf(AValue: string): integer;
var
  i: integer;
begin
  Result := -1;

  for i := 0 to Count - 1 do
  begin
    if Items[i].Caption = AValue then
    begin
      Result := i;
      break;
    end;
  end;
end;

function TDetailItems.Insert(Index: Integer): TDetailItem;
begin
  Result := TDetailItem(inherited Insert(Index));
end;

// ------------------------------------------------------------------------------

procedure TDetailItems.SetItem(Index: Integer; const Value: TDetailItem);
begin
  inherited Items[Index] := Value;
end;

// ------------------------------------------------------------------------------

{ TAdvDetailDropDown }

function upstr(s: string; docase: Boolean): string;
begin
  if docase then
    Result := s
  else
    Result := AnsiUpperCase(s);
end;

// ------------------------------------------------------------------------------

procedure TAdvDetailDropDown.Change;
var
  c, c1, NewT: string;
  i: Integer;
  UsrStr, AutoAdd: string;
begin
  FItemChange := False;

  inherited Change;

  if csDesigning in ComponentState then
    Exit;

  if not FWorkMode then
    Exit;

  if Text <> FOldValue then
    Modified := true
  else
    Modified := False;

  NewT := Copy(Text, 1, selstart);
  c1 := upstr(Text, FMatchCase);
  c := Copy(c1, 1, selstart);

  if (Items.Count > 0) then
  begin
    for i := 0 to Items.Count - 1 do
    begin
      if pos(c, upstr(Items[i].Caption, FMatchCase)) = 1 then
      begin
        UsrStr := Copy(Text, 1, length(c));
        AutoAdd := Copy(Items[i].Caption, length(c) + 1, 255);

        FItemIndex := i;
        FItemIdx := i;
        FItemSel := length(c);
        FItemChange := true;
        Text := UsrStr + AutoAdd;

        SendMessage(Handle, EM_SETSEL, length(Text), length(c));
        Exit;
      end;
    end;

    if (NewT <> '') then
    begin
      Text := NewT;
      SendMessage(Handle, EM_SETSEL, length(Text), length(NewT));
    end;
  end;

end;

constructor TAdvDetailDropDown.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TDetailItems.Create(Self);
  FItems.OnChange := OnItemsChanged;
  FItemAppearance := TItemAppearance.Create(Self);
  FItemIndex := -1;
  FOldItemIndex := FItemIndex;
  FLayout := ilCaptionRight;
  AutoSize := False;
  EditorEnabled := True;
  DropDownEnabled := True;
  FCurSearch := '';
  FKeyTimer := TTimer.Create(Self);
  FKeyTimer.Enabled := False;
  FKeyTimer.Interval := 500;
  FKeyTimer.OnTimer := OnKeyTimerTime;
  FItemHeight := 22;
  FCaptionFont := TFont.Create;
  FNotesFont := TFont.Create;
  FCaptionFont.Assign(Font);
  FNotesFont.Assign(Font);
  FWorkMode := true;
  FImageVAlign := taVerticalCenter;
end;

// ------------------------------------------------------------------------------

destructor TAdvDetailDropDown.Destroy;
begin
  FCaptionFont.Free;
  FNotesFont.Free;
  FKeyTimer.Enabled := False;
  FKeyTimer.Free;
  FItems.Free;
  FItemAppearance.Free;
  inherited;
end;

// ------------------------------------------------------------------------------

procedure TAdvDetailDropDown.CreateDropDownForm;
begin
  inherited;
  if not Assigned(FItemSelector) then
  begin
    FItemSelector := TAdvCustomItemSelector.Create(Self);
    FItemSelector.Parent := FDropDownForm;
    // FItemSelector.AdvDropDown := Self;
    FItemSelector.SelectorType := stImage;
    FItemSelector.Left := 0;
    FItemSelector.Top := 0;
    FItemSelector.Height := 150;
    FItemSelector.DoubleBuffered := true;
    // FItemSelector.Initialize;
    FItemSelector.OnDrawItem := DrawItem;
    FItemSelector.OnItemSize := ItemSize;
    FItemSelector.UpdateSelectorPanel;
    FItemSelector.OnResize := DropDownResize;

//    FDropDownForm.OnResize := DropDownResize;
  end;
  FItemSelector.Color := DropDownColor;
  TInternalItemSelector(FItemSelector).UpdateSelectorPanel;
  Control := FItemSelector;
end;

// ------------------------------------------------------------------------------

procedure TAdvDetailDropDown.SetSelectorProperties;
begin
  if Assigned(FItemSelector) then
  begin
    FItemSelector.OnItemSelect := nil;
    FItemSelector.Columns := 1;
    FItemSelector.Images := Images;
    FItemSelector.ItemLayout := FLayout;
    FItemSelector.Color := DropDownColor;
    // FItemSelector.ItemAppearance.ColorStyle := SelectionColorStyle;
    FItemSelector.ItemAppearance.Assign(ItemAppearance);
    AssignedItemsToItemSelector;
    TInternalItemSelector(FItemSelector).UpdateSelectorPanel;
    FItemSelector.ItemIndex := FItemIndex;
    FItemSelector.OnItemSelect := OnSelectorItemSelect;
  end;
end;

// ------------------------------------------------------------------------------

procedure TAdvDetailDropDown.BeforeDropDown;
begin
  inherited;
  SetSelectorProperties;
  FOldItemIndex := ItemIndex;
end;

// ------------------------------------------------------------------------------

procedure TAdvDetailDropDown.AssignedItemsToItemSelector;
var
  i: Integer;
begin
  if not Assigned(FItemSelector) then
    Exit;

  FItemSelector.Items.Clear;

  for i := 0 to Items.Count - 1 do
  begin
    with FItemSelector.Items.Add do
    begin
      Caption := Items[i].Caption;
      Image.Assign(Items[i].Image);
      ImageIndex := Items[i].ImageIndex;
    end;
  end;
end;

// ------------------------------------------------------------------------------

procedure TAdvDetailDropDown.UpdateDropDownSize;
begin
  if not Assigned(FItemSelector) then
  begin
    inherited;
    Exit;
  end;

  FItemSelector.Align := alNone;

  if (DropDownWidth <= 0) then
    FItemSelector.Width := Width;

  if (DropDownHeight <= 0) then
    FItemSelector.Height := 100;

  inherited;
end;

// ------------------------------------------------------------------------------

procedure TAdvDetailDropDown.DoHideDropDown(Cancelled: Boolean);
begin
  inherited;

  if Cancelled then
  begin
    if Assigned(FItemSelector) and (FOldItemIndex <> -1) then
      ItemIndex := FOldItemIndex;
  end;
end;

procedure TAdvDetailDropDown.HandleMouseWheelDown;
var
  i: integer;
begin
  inherited;
  if (DroppedDown) and Assigned(FItemSelector) then
  begin
    if (FItemSelector.ItemIndex < 0) then
      i := 0
    else
      i := FItemSelector.ItemIndex + 1;
    if (i >= 0) and (i < FItemSelector.Items.Count) then
    begin
      FInternalCall := True;
      FItemSelector.ItemIndex := i;
      FItemSelector.ScrollItemInView(FItemSelector.ItemIndex);
      FInternalCall := False;
    end;
    Invalidate;
  end
  else
    SelectNext;
end;

procedure TAdvDetailDropDown.HandleMouseWheelUp;
var
  i: integer;
begin
  inherited;
  if (DroppedDown) and Assigned(FItemSelector) then
  begin
    if (FItemSelector.ItemIndex < 0) then
      i := 0
    else
      i := FItemSelector.ItemIndex - 1;
    if (i >= 0) and (i < FItemSelector.Items.Count) then
    begin
      FInternalCall := True;
      FItemSelector.ItemIndex := i;
      FItemSelector.ScrollItemInView(FItemSelector.ItemIndex);
      FInternalCall := False;
    end;
    Invalidate;
  end
  else
    SelectPrevious;
end;

procedure TAdvDetailDropDown.DoShowDropDown;
begin
  inherited;
  //
  FItemSelector.SetFocus;
end;

// ------------------------------------------------------------------------------

procedure TAdvDetailDropDown.OnDropDownSizing;
begin
  inherited;
  FItemSelector.UpdateSelectorPanel;
end;

procedure TAdvDetailDropDown.OnHideDropDown;
begin
  inherited;
  // if Assigned(FItemSelector) then
  // FItemSelector.OnItemSelect := nil;
end;

// ------------------------------------------------------------------------------

procedure TAdvDetailDropDown.WMKeyDown(var Msg: TWMKeyDown);

var
  IsAlt, IsCtrl: Boolean;
  i: Integer;
begin
  IsAlt := (GetKeyState(VK_MENU) and $8000 = $8000);
  IsCtrl := GetKeyState(VK_CONTROL) and $8000 = $8000;
  if Enabled and not IsAlt then
  begin
    case Msg.CharCode of
      VK_UP:
        begin
          if (DroppedDown) and Assigned(FItemSelector) then
          begin
            if (FItemSelector.ItemIndex < 0) then
              i := 0
            else
              i := FItemSelector.ItemIndex - 1;
            if (i >= 0) and (i < FItemSelector.Items.Count) then
            begin
              FInternalCall := True;
              FItemSelector.ItemIndex := i;
              FItemSelector.ScrollItemInView(FItemSelector.ItemIndex);
              FInternalCall := False;
            end;
            Invalidate;
          end
          else
            SelectPrevious;
        end;
      VK_DOWN:
        begin
          if (DroppedDown) and Assigned(FItemSelector) then
          begin
            if (FItemSelector.ItemIndex < 0) then
              i := 0
            else
              i := FItemSelector.ItemIndex + 1;
            if (i >= 0) and (i < FItemSelector.Items.Count) then
            begin
              FInternalCall := True;
              FItemSelector.ItemIndex := i;
              FItemSelector.ScrollItemInView(FItemSelector.ItemIndex);
              FInternalCall := False;
            end;
            Invalidate;
          end
          else
            SelectNext;
        end;
      VK_LEFT:
        begin
          if not EditorEnabled and DroppedDown then
          begin
            FInternalCall := True;
            FItemSelector.SelectPrevious;
            FInternalCall := False;
          end;
        end;
      VK_RIGHT:
        begin
          if not EditorEnabled and DroppedDown then
          begin
            FInternalCall := True;
            FItemSelector.SelectNext;
            FInternalCall := False;
          end;
        end;
      VK_HOME:
        begin
          if (DroppedDown) and Assigned(FItemSelector) then
          begin
            FInternalCall := True;
            FItemSelector.SelectFirst;
            FInternalCall := False;
          end
          else
            if not EditorEnabled then
              SelectFirst;
        end;
      VK_END:
        begin
          if (DroppedDown) and Assigned(FItemSelector) then
          begin
            FInternalCall := True;
            FItemSelector.SelectLast;
            FInternalCall := False;
          end
          else
            if not EditorEnabled then
              SelectLast;
        end;
      VK_PRIOR:
        begin
          if Assigned(FItemSelector) then
          begin
            if (FItemSelector.Items.Count <> Items.Count) then
              SetSelectorProperties;
            i := FItemSelector.GetVisibleItemCount;
            if (i > 0) then
            begin
              i := Max(FItemSelector.ItemIndex - i, 0);
              if (i >= 0) and (i < FItemSelector.Items.Count) then
              begin
                FInternalCall := True;
                FItemSelector.ItemIndex := i;
                FItemSelector.ScrollItemInView(FItemSelector.ItemIndex);
                FInternalCall := False;
              end;
            end;
          end;
        end;
      VK_NEXT:
        begin
          if Assigned(FItemSelector) then
          begin
            if (FItemSelector.Items.Count <> Items.Count) then
              SetSelectorProperties;
            i := FItemSelector.GetVisibleItemCount;
            if (i > 0) then
            begin
              i := Min(FItemSelector.ItemIndex + i, Items.Count - 1);
              if (i >= 0) and (i < FItemSelector.Items.Count) then
              begin
                FInternalCall := True;
                FItemSelector.ItemIndex := i;
                FItemSelector.ScrollItemInView(FItemSelector.ItemIndex);
                FInternalCall := False;
              end;
            end;
          end;
        end;
    else
      begin
        if not IsAlt and not IsCtrl and Assigned(FItemSelector) then
        begin
          FCurSearch := FCurSearch + Char(Msg.CharCode);
          FInternalCall := True;
          FItemSelector.LookupItem(FCurSearch);
          FInternalCall := False;
          FKeyTimer.Enabled := True;
        end;
      end;
    end;
  end;

  if DroppedDown and (Msg.CharCode  in [VK_UP,VK_DOWN,VK_PRIOR,VK_NEXT,VK_LEFT,VK_RIGHT]) then
  begin
    Msg.CharCode  := 0;
    Msg.Result := 1;
    Exit;
  end;

  inherited;
end;

// ------------------------------------------------------------------------------

procedure TAdvDetailDropDown.WMPaint(var Message: TWMPaint);
begin
  inherited;
end;

// ------------------------------------------------------------------------------

procedure TAdvDetailDropDown.DrawItem(Sender: TObject; Canvas: TCanvas;
  R: TRect; Index: Integer);

var
  s,a,sv,fa,ah: string;
  x: integer;
  xs,ys,hl,ml: integer;
  hr,cr: TRect;
  CID,CV,CT: string;
  vp: integer;
begin
  if Assigned(FOnDrawSelectedImage) then
    FOnDrawSelectedImage(Self, Canvas, R)
  else
  if (Index >= 0) then
  begin
    s := Items[Index].Caption;
    if Assigned(Items[Index].Image.Graphic) and not Items[Index].Image.Graphic.Empty then
    begin
      if (s = '') then
        x := R.Left + (R.Right - R.Left - Items[Index].Image.Width) div 2
      else
        x := R.Left + 2;

      if Items[Index].Image.Graphic is TBitmap then
      begin
        (Items[Index].Image.Graphic as TBitmap).TransparentMode := tmAuto;
        (Items[Index].Image.Graphic as TBitmap).Transparent := true;
      end;

      vp := 0;
      case ImageVAlign of
      taAlignBottom: vp := R.Bottom - R.Top - Items[Index].Image.Height;
      taVerticalCenter: vp := (R.Bottom - R.Top - Items[Index].Image.Height) div 2;
      end;

      Canvas.Draw(x, R.Top + vp, Items[Index].Image.Graphic);
      R.Left := R.Left + Items[Index].Image.Width + 4;
    end
    else if Assigned(Images) and (Items[Index].ImageIndex >= 0) then
    begin
      if (s = '') then
        x := R.Left + (R.Right - R.Left - Images.Width) div 2
      else
        x := R.Left + 2;

      vp := 0;
      case ImageVAlign of
      taAlignBottom: vp := R.Bottom - R.Top - Images.Height;
      taVerticalCenter: vp := (R.Bottom - R.Top - Images.Height) div 2;
      end;

      Images.Draw(Canvas, x, R.Top + vp, Items[Index].ImageIndex);
      R.Left := R.Left + Images.Width + 4;
    end;

    Canvas.Font.Assign(CaptionFont);

    if Index = FItemSelector.ItemHot then
      Canvas.Font.Color := ItemAppearance.ColorHotText;

    if (Index = ItemIndex) then
    begin
      Canvas.Font.Color := ItemAppearance.ColorSelectedText;
    end;

    Canvas.Brush.Style := bsClear;
    R.Left := R.Left + 4;
    R.Top := R.Top + 2;
    R.Right := R.Right - 4;
    DrawText(Canvas.Handle, PChar(s), length(s), R, DT_LEFT or DT_END_ELLIPSIS);

    if Items[Index].Notes <> '' then
    begin
      R.Top := R.Top + Canvas.TextHeight('gh');
      Canvas.Font.Assign(NotesFont);

      if Index = FItemSelector.ItemHot then
        Canvas.Font.Color := ItemAppearance.ColorHotText;

      if (Index = ItemIndex) then
        Canvas.Font.Color := ItemAppearance.ColorSelectedText;

      HTMLDrawEx(Canvas, Items[Index].Notes,R,Images,-1,-1,-1,-1,2,false,false,false,false,false,false,true,false,'',1.0,clBlue,clNone,clNone,clGray,a,sv,fa,ah,xs,ys,hl,ml,hr,cr, CID,CV,CT,nil,nil,0);
    end;
  end;
end;

procedure TAdvDetailDropDown.DropDownResize(Sender: TObject);
begin
  FItemSelector.UpdateSelectorPanel;
end;

(*
procedure TAdvDetailDropDown.DrawSelectedImage;

var
  DC: HDC;
  Canvas: TCanvas;
  R: TRect;
  s: string;
  x: Integer;
begin
  DC := GetWindowDC(Handle);
  try
    Canvas := TCanvas.Create;
    Canvas.Handle := DC;
    R := GetEditRect;

    if Assigned(FOnDrawSelectedImage) then
      FOnDrawSelectedImage(Self, Canvas, R)
    else if (ItemIndex >= 0) then
    begin
      s := Items[ItemIndex].Caption;
      if Assigned(Items[ItemIndex].Image.Graphic) and not Items[ItemIndex].Image.Graphic.Empty then
      begin
        if (s = '') then
          x := R.Left + (R.Right - R.Left - Items[ItemIndex].Image.Width) div 2
        else
          x := R.Left + 2;
        Canvas.Draw
          (x, R.Top + (R.Bottom - R.Top - Items[ItemIndex].Image.Height) div 2, Items[ItemIndex].Image.Graphic);
        R.Left := R.Left + Items[ItemIndex].Image.Width + 4;
      end
      else if Assigned(Images) and (Items[ItemIndex].ImageIndex >= 0) then
      begin
        if (s = '') then
          x := R.Left + (R.Right - R.Left - Images.Width) div 2
        else
          x := R.Left + 2;
        Images.Draw(Canvas, x, R.Top + (R.Bottom - R.Top - Images.Height) div 2, Items[ItemIndex].ImageIndex);
        R.Left := R.Left + Images.Width + 4;
      end;

      // --- Draw Text
      if (s <> '') then
      begin
        Canvas.Font.Assign(Font);
        if Focused then
          Canvas.Font.Color := clHighlightText;
        Canvas.Brush.Style := bsClear;
        DrawText(Canvas.Handle, PChar(s), -1, R,
          DT_SINGLELINE or DT_VCENTER or DT_LEFT);
      end;
    end;

    Canvas.Free;
  finally
    ReleaseDC(Handle, DC);
  end;
end;
*)


procedure TAdvDetailDropDown.ItemSize(Sender: TObject; var ASize: TSize);
begin
  if DropDownWidth > 0 then
  begin
    if FItemSelector.VertScrollBar.IsScrollBarVisible then
      ASize.cx := DropDownWidth - GetSystemMetrics(SM_CXVSCROLL) - 2 * GetSystemMetrics(SM_CXBORDER) - 4
    else
      ASize.cx := DropDownWidth - 2 * GetSystemMetrics(SM_CXBORDER) - 4;

    //ASize.cx := DropDownWidth - 8
  end
  else
  begin
    if FItemSelector.VertScrollBar.IsScrollBarVisible then
      ASize.cx := FDropDownForm.Width - GetSystemMetrics(SM_CXVSCROLL) - 2 * GetSystemMetrics(SM_CXBORDER) - 4
    else
      ASize.cx := FDropDownForm.Width - 2 * GetSystemMetrics(SM_CXBORDER) - 4;
  end;

  ASize.cy := FItemHeight;
end;

procedure TAdvDetailDropDown.KeyDown(var Key: Word; Shift: TShiftState);
var
  idx: integer;
begin
  if (Items.Count = 0) then
  begin
    inherited;
    Exit;
  end;

  case Key of
    VK_ESCAPE, VK_BACK, VK_DELETE:
      FWorkMode := False;
    VK_RETURN:
      begin
        idx := Items.IndexOf(Text);
        if (idx <> -1) then
        begin
          Text := Items[idx].Caption;
          ItemIndex := idx;
          Change;
        end;
      end;
  else
    FWorkMode := true;
  end;
  inherited KeyDown(Key, Shift);

  if (Key = VK_ESCAPE) then
  begin
    idx := Items.IndexOf(Text);
    if (idx >= 0) then
      ItemIndex := idx;
  end;
end;

procedure TAdvDetailDropDown.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;

  if FItemChange then
  begin
    ItemIndex := FItemIdx;
    SendMessage(Handle, CB_SETEDITSEL, 0, makelong(FItemSel, length(Text)));
    FItemChange := False;
  end;

end;

// ------------------------------------------------------------------------------

procedure TAdvDetailDropDown.SetEditRect;
begin
  inherited;
end;

// ------------------------------------------------------------------------------

procedure TAdvDetailDropDown.SetSelectionColorStyle
  (const Value: TSelectionColorStyle);
begin
  inherited;

  if Assigned(FItemSelector) then
    TInternalItemAppearance(FItemSelector.ItemAppearance).ColorStyle := Value;
end;

// ------------------------------------------------------------------------------

procedure TAdvDetailDropDown.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

end;

// ------------------------------------------------------------------------------

procedure TAdvDetailDropDown.SelectFirst;
begin
  if (Items.Count <= 0) then
    Exit;

  ItemIndex := 0;
end;

// ------------------------------------------------------------------------------

procedure TAdvDetailDropDown.SelectLast;
begin
  if (Items.Count <= 0) then
    Exit;

  ItemIndex := Items.Count - 1;
end;

// ------------------------------------------------------------------------------

procedure TAdvDetailDropDown.SelectNext;
begin
  if (Items.Count <= 0) then
    Exit;

  if (FItemIndex < 0) then
    ItemIndex := 0
  else
  begin
    if ((ItemIndex + 1) < Items.Count) then
      ItemIndex := ItemIndex + 1;
  end;
end;

// ------------------------------------------------------------------------------

procedure TAdvDetailDropDown.SelectPrevious;
begin
  if (Items.Count <= 0) then
    Exit;

  if (FItemIndex < 0) then
    ItemIndex := 0
  else
  begin
    if ((ItemIndex - 1) >= 0) then
      ItemIndex := ItemIndex - 1;
  end;
end;

// ------------------------------------------------------------------------------

procedure TAdvDetailDropDown.SetCaptionFont(const Value: TFont);
begin
  FCaptionFont.Assign(Value);
end;

// ------------------------------------------------------------------------------

procedure TAdvDetailDropDown.SetComponentStyle(AStyle: TTMSStyle);
begin
  inherited;
  SetAppearanceStyle(ItemAppearance, AStyle);
end;

// ------------------------------------------------------------------------------

procedure TAdvDetailDropDown.SetItemIndex(const Value: Integer);
begin
  if (FItemIndex <> Value) and (Value < FItems.Count) and (Value >= 0) then
  begin
    FItemIndex := Value;
    Text := Items[FITemIndex].Caption;
    SelStart := 0;
    SelLength := Length(Text);

    Invalidate;

    if Assigned(FItemSelector) then
      FItemSelector.Invalidate;

    if Assigned(FOnSelect) then
      FOnSelect(Self);
  end;

  if (FItemIndex <> Value) and (Value = -1) then
  begin
    Text := '';
    FItemIndex := Value;
  end;

end;

// ------------------------------------------------------------------------------

procedure TAdvDetailDropDown.SetLayout(const Value: TItemLayout);
begin
  FLayout := Value;
end;

procedure TAdvDetailDropDown.SetNotesFont(const Value: TFont);
begin
  FNotesFont.Assign(Value);
end;

// ------------------------------------------------------------------------------

procedure TAdvDetailDropDown.SetItems(const Value: TDetailItems);
begin
  FItems.Assign(Value);
end;

// ------------------------------------------------------------------------------

procedure TAdvDetailDropDown.OnItemsChanged(Sender: TObject);
begin

end;

procedure TAdvDetailDropDown.OnKeyTimerTime(Sender: TObject);
begin
  FKeyTimer.Enabled := False;
  FCurSearch := '';
end;

// ------------------------------------------------------------------------------

procedure TAdvDetailDropDown.OnSelectorItemSelect(Sender: TObject);
begin
  if Assigned(FItemSelector) then
  begin
    ItemIndex := FItemSelector.ItemIndex;
    Text := Items[ItemIndex].Caption;
    SelStart := 0;
    SelLength := Length(Text);

    if not FInternalCall then
    begin
      DoHideDropDown(False);
    end;
  end;
end;

// ------------------------------------------------------------------------------

procedure TAdvDetailDropDown.SetItemAppearance(const Value: TItemAppearance);
begin
  FItemAppearance.Assign(Value);
end;

// ------------------------------------------------------------------------------

end.
