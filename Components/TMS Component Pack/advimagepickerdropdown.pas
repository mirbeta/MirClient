{***************************************************************************}
{ TAdvImagePickerDropDown components                                        }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2009 - 2013                                        }
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

{$I TMSDEFS.INC}

unit AdvImagePickerDropDown;

interface

uses
  Classes, Windows, Graphics, Controls, Messages, ImgList, ExtCtrls, Math,
  AdvDropDown, AdvStyleIF, SysUtils;

type

  TImageItem = class(TCollectionItem)
  private
    FCaption: TCaption;
    FImageIndex: Integer;
    FEnabled: Boolean;
    FImage: TPicture;
    FRect: TRect;
    FHint: string;
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
    property Hint: string read FHint write FHint;
  end;

  TImageItems = class(TCollection)
  private
    FMyOwner: TPersistent;
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TImageItem;
    procedure SetItem(Index: Integer; const Value: TImageItem);
  protected
    procedure Change;
  public
    constructor Create(AOwner: TPersistent);
    property Items[Index: Integer]: TImageItem read GetItem write SetItem; default;
    function Add: TImageItem;
    function Insert(Index: Integer): TImageItem;
    function GetOwner: TPersistent; override;
    {$IFNDEF DELPHI6_LVL}
    property Owner: TPersistent read FMyOwner;
    {$ENDIF}
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvImagePickerDropDown = class(TAdvCustomDropDown)
  private
    FItemSelector: TAdvCustomItemSelector;
    FColumns: Integer;
    FLayout: TItemLayout;
    FItems: TImageItems;
    FItemIndex: Integer;
    FOnSelect: TNotifyEvent;
    FOnDrawSelectedImage: TOnDrawSelectedItem;
    FItemAppearance: TItemAppearance;
    FInternalCall: Boolean;
    FKeyTimer: TTimer;
    FCurSearch: string;
    FOldItemIndex: Integer;
    FOnDrawItem: TDrawItemEvent;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMKeyDown(var Msg: TWMKeyDown); message WM_KEYDOWN;
    procedure DrawSelectedImage;
    procedure OnItemsChanged(Sender: TObject);
    procedure OnSelectorItemSelect(Sender: TObject);
    procedure OnKeyTimerTime(Sender: TObject);
    procedure OnItemSelectorDrawItem(Sender: TObject; Canvas: TCanvas; R: TRect; Index: Integer);
    procedure SetColumns(const Value: Integer);
    procedure SetItemIndex(const Value: Integer);
    procedure SetLayout(const Value: TItemLayout);
    procedure SetItems(const Value: TImageItems);
    procedure AssignedItemsToItemSelector;
    procedure SetSelectorProperties;
    procedure SetItemAppearance(const Value: TItemAppearance);
    function GetImageIndex: integer;
    procedure SetImageIndex(const Value: integer);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CreateDropDownForm; override;
    procedure BeforeDropDown; override;
    procedure OnHideDropDown; override;
    procedure UpdateDropDownSize; override;
    procedure DoHideDropDown(Canceled: Boolean); override;
    procedure SetEditRect; override;
    procedure SetSelectionColorStyle(const Value: TSelectionColorStyle); override;
    procedure HandleMouseWheelDown; override;
    procedure HandleMouseWheelUp; override;
    procedure DrawBackGround; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetComponentStyle(AStyle: TTMSStyle); override;
    procedure SelectFirst;
    procedure SelectLast;
    procedure SelectNext;
    procedure SelectPrevious;
    procedure AddImagesFromFolder(AFolder: String; SetImageCaption: boolean = false);
    procedure AddImagesFromImageList;
    property ImageIndex: integer read GetImageIndex write SetImageIndex;
  published
    property Columns: Integer read FColumns write SetColumns default 1;
    property Items: TImageItems read FItems write SetItems;
    property ItemIndex: Integer read FItemIndex write SetItemIndex default -1;
    property Layout: TItemLayout read FLayout write SetLayout default ilCaptionRight;
    property ItemAppearance: TItemAppearance read FItemAppearance write SetItemAppearance;

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
    property Cursor default crArrow;
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
    property DropDownSizeable;
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
    property OnDrawItem: TDrawItemEvent read FOnDrawItem write FOnDrawItem;
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

//------------------------------------------------------------------------------

{ TImageItem }

procedure TImageItem.Assign(Source: TPersistent);
begin
  if (Source is TImageItem) then
  begin
    Caption := (Source as TImageItem).Caption;
    ImageIndex := (Source as TImageItem).ImageIndex;
    Image.Assign((Source as TImageItem).Image);
    Enabled := (Source as TImageItem).Enabled;
    Hint := (Source as TImageItem).Hint;
  end
  else
    inherited Assign(Source);
end;

//------------------------------------------------------------------------------

procedure TImageItem.Changed;
begin
  TImageItems(Collection).Change;
end;

//------------------------------------------------------------------------------

constructor TImageItem.Create(Collection: TCollection);
begin
  inherited;
  FCaption := '';
  FImageIndex := -1;
  FImage := TPicture.Create;
  FEnabled := True;
end;

//------------------------------------------------------------------------------

destructor TImageItem.Destroy;
begin
  FImage.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TImageItem.SetCaption(const Value: TCaption);
begin
  if (FCaption <> Value) then
  begin
    FCaption := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TImageItem.SetImage(const Value: TPicture);
begin
  FImage.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TImageItem.SetImageIndex(const Value: Integer);
begin
  if (FImageIndex <> Value) then
  begin
    FImageIndex := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

{ TImageItems }

function TImageItems.Add: TImageItem;
begin
  Result := TImageItem(inherited Add);
end;

//------------------------------------------------------------------------------

procedure TImageItems.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//------------------------------------------------------------------------------

constructor TImageItems.Create(AOwner: TPersistent);
begin
  inherited Create(TImageItem);
  FMyOwner := AOwner;
end;

//------------------------------------------------------------------------------

function TImageItems.GetItem(Index: Integer): TImageItem;
begin
  Result := TImageItem(inherited Items[Index]);
end;

//------------------------------------------------------------------------------

function TImageItems.GetOwner: TPersistent;
begin
  Result := FMyOwner;
end;

//------------------------------------------------------------------------------

function TImageItems.Insert(Index: Integer): TImageItem;
begin
  Result := TImageItem(inherited Insert(Index));
end;

//------------------------------------------------------------------------------

procedure TImageItems.SetItem(Index: Integer;
  const Value: TImageItem);
begin
  inherited Items[Index] := Value;
end;

//------------------------------------------------------------------------------

{ TAdvImagePickerDropDown }

constructor TAdvImagePickerDropDown.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TImageItems.Create(Self);
  FItems.OnChange := OnItemsChanged;
  FItemAppearance := TItemAppearance.Create(Self);
  FColumns := 1;
  FItemIndex := -1;
  FOldItemIndex := FItemIndex;
  FLayout := ilCaptionRight;
  AutoSize := False;
  EditorEnabled := False;
  DropDownEnabled := True;
  FCurSearch := '';
  FKeyTimer := TTimer.Create(Self);
  FKeyTimer.Enabled := False;
  FKeyTimer.Interval := 500;
  FKeyTimer.OnTimer := OnKeyTimerTime;
  Cursor := crArrow;
end;

//------------------------------------------------------------------------------

destructor TAdvImagePickerDropDown.Destroy;
begin
  FKeyTimer.Enabled := False;
  FKeyTimer.Free;
  FItems.Free;
  FItemAppearance.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvImagePickerDropDown.CreateDropDownForm;
begin
  inherited;
  if not Assigned(FItemSelector) then
  begin
    FItemSelector := TAdvCustomItemSelector.Create(Self);
    FItemSelector.Parent := FDropDownForm;
    //FItemSelector.AdvDropDown := Self;
    FItemSelector.SelectorType := stImage;
    FItemSelector.Left := 0;
    FItemSelector.Top := 0;
    FItemSelector.Height := 150;
    //FItemSelector.Initialize;
  end;
  FItemSelector.Color := DropDownColor;
  TInternalItemSelector(FItemSelector).UpdateSelectorPanel;
  Control := FItemSelector;
end;

//------------------------------------------------------------------------------

procedure TAdvImagePickerDropDown.SetSelectorProperties;
begin
  if Assigned(FItemSelector) then
  begin
    FItemSelector.OnItemSelect := nil;
    FItemSelector.Columns := FColumns;
    FItemSelector.Images := Images;
    FItemSelector.ItemLayout := FLayout;
    FItemSelector.Color := DropDownColor;
    //FItemSelector.ItemAppearance.ColorStyle := SelectionColorStyle;
    FItemSelector.ItemAppearance.Assign(ItemAppearance);
    AssignedItemsToItemSelector;
    TInternalItemSelector(FItemSelector).UpdateSelectorPanel;
    FItemSelector.ItemIndex := FItemIndex;
    FItemSelector.OnItemSelect := OnSelectorItemSelect;
    FItemSelector.ShowHint := ShowHint;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvImagePickerDropDown.BeforeDropDown;
begin
  inherited;
  SetSelectorProperties;
  if Assigned(OnDrawItem) then
    FItemSelector.OnDrawItem := OnItemSelectorDrawItem
  else
    FItemSelector.OnDrawItem := nil;
  FOldItemIndex := ItemIndex;
end;

//------------------------------------------------------------------------------

procedure TAdvImagePickerDropDown.AddImagesFromFolder(AFolder: String;
  SetImageCaption: boolean);
var
  SR: TSearchRec;

  procedure AddToList(s: string);
  begin
    with Items.Add do
    begin
      try
        Image.LoadFromFile(s);
        if SetImageCaption then
        begin
          FCaption := ExtractFileName(s);
        end;
      except
        Image.Assign(nil);
      end;
    end;
  end;

begin
  if FindFirst(AFolder,faAnyFile,SR) = 0 then
  begin
    AddToList(ExtractFilePath(AFolder) + SR.Name);
    while FindNext(SR) = 0 do
      AddToList(ExtractFilePath(AFolder) + SR.Name);
  end;
  FindClose(SR);
end;

//------------------------------------------------------------------------------

procedure TAdvImagePickerDropDown.AddImagesFromImageList;
var
  i: integer;
begin
  if Assigned(Images) then
  begin
    for i := 0 to Images.Count - 1 do
    begin
      Items.Add.ImageIndex := i;
    end;
  end;
end;

procedure TAdvImagePickerDropDown.AssignedItemsToItemSelector;
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
      Hint := Items[i].Hint;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvImagePickerDropDown.UpdateDropDownSize;
var
  sz: TSize;
begin
  if not Assigned(FItemSelector) then
  begin
    inherited;
    Exit;
  end;

  FItemSelector.Align := alNone;
  sz := TInternalItemSelector(FItemSelector).GetItemPanelSize;
  if (DropDownWidth <= 0) then
    FItemSelector.Width := sz.cx;

  if (DropDownHeight <= 0) then
    FItemSelector.Height := sz.cy;

  inherited;

  if FItemSelector.VertScrollBar.IsScrollBarVisible then
  begin
    FDropDownForm.Width := FDropDownForm.Width + GetSystemMetrics(SM_CXVSCROLL);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvImagePickerDropDown.DoHideDropDown(Canceled: Boolean);
begin
  inherited;
  if Canceled then
  begin
    if Assigned(FItemSelector) then
      ItemIndex := FOldItemIndex;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvImagePickerDropDown.HandleMouseWheelDown;
begin
  inherited;
  if Enabled and not ReadOnly then
  begin
    if DroppedDown then
      FItemSelector.HotNext
    else
    begin
      FInternalCall := True;
      FItemSelector.SelectNext;
      FInternalCall := False;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvImagePickerDropDown.HandleMouseWheelUp;
begin
  inherited;
  if Enabled and not ReadOnly then
  begin
    if DroppedDown then
      FItemSelector.HotPrevious
    else
    begin
      FInternalCall := True;
      FItemSelector.SelectPrevious;
      FInternalCall := False;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvImagePickerDropDown.OnHideDropDown;
begin
  inherited;
  //if Assigned(FItemSelector) then
    //FItemSelector.OnItemSelect := nil;
end;

//------------------------------------------------------------------------------

procedure TAdvImagePickerDropDown.WMKeyDown(var Msg: TWMKeyDown);
var
  IsAlt, IsCtrl, NewSel: Boolean;
  i: Integer;
begin
  NewSel := False;
  if Enabled and DroppedDown then
  begin
    if (Msg.CharCode = VK_RETURN) then
    begin
      if Assigned(FItemSelector) then
      begin
        if (FItemSelector.ItemHot >= 0) then
        begin
          FItemSelector.ItemIndex := FItemSelector.ItemHot;
          NewSel := True;
        end;
      end;
    end;
  end;

  inherited;
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
            i := FItemSelector.ItemIndex - Columns;
          if (i >= 0) and (i < FItemSelector.Items.Count) then
          begin
            FInternalCall := True;
            FItemSelector.ItemIndex := i;
            FItemSelector.ScrollItemInView(FItemSelector.ItemIndex);
            FInternalCall := False;
          end;
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
            i := FItemSelector.ItemIndex + Columns;
          if (i >= 0) and (i < FItemSelector.Items.Count) then
          begin
            FInternalCall := True;
            FItemSelector.ItemIndex := i;
            FItemSelector.ScrollItemInView(FItemSelector.ItemIndex);
            FInternalCall := False;
          end;
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
      VK_RETURN:
      begin
        if not NewSel and Assigned(FOnSelect) then
          FOnSelect(Self);
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
end;

//------------------------------------------------------------------------------

procedure TAdvImagePickerDropDown.WMPaint(var Message: TWMPaint);
begin
  inherited;
  DrawSelectedImage;
end;

//------------------------------------------------------------------------------

procedure TAdvImagePickerDropDown.DrawBackGround;
begin
  inherited;
  DrawSelectedImage;
end;

//------------------------------------------------------------------------------

procedure TAdvImagePickerDropDown.DrawSelectedImage;
var
  DC: HDC;
  Canvas: TCanvas;
  R: TRect;
  pic: TPicture;
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
      pic := Items[ItemIndex].Image;
      if Assigned(pic.Graphic) and not pic.Graphic.Empty then
      begin
        Canvas.Draw(R.Left + (R.Right - R.Left - Items[ItemIndex].Image.Width) div 2, R.Top + (R.Bottom - R.Top - Items[ItemIndex].Image.Height) div 2, Items[ItemIndex].Image.Graphic);
      end
      else if Assigned(Images) and (Items[ItemIndex].ImageIndex >= 0) then
      begin
        Images.Draw(Canvas, R.Left + (R.Right - R.Left - Images.Width) div 2, R.Top + (R.Bottom - R.Top - Images.Height) div 2, Items[ItemIndex].ImageIndex);
      end;
    end;

    Canvas.Free;
  finally
    ReleaseDC(Handle,DC);
  end;
end;

function TAdvImagePickerDropDown.GetImageIndex: integer;
begin
  if ItemIndex >= 0 then
    Result := Items[ItemIndex].ImageIndex
  else
    Result := -1;
end;

//------------------------------------------------------------------------------

procedure TAdvImagePickerDropDown.SetEditRect;
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvImagePickerDropDown.SetSelectionColorStyle(
  const Value: TSelectionColorStyle);
begin
  inherited;

  if Assigned(FItemSelector) then
    TInternalItemAppearance(FItemSelector.ItemAppearance).ColorStyle := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvImagePickerDropDown.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TAdvImagePickerDropDown.SelectFirst;
begin
  if (Items.Count <= 0) then
    Exit;

  ItemIndex := 0;  
end;

//------------------------------------------------------------------------------

procedure TAdvImagePickerDropDown.SelectLast;
begin
  if (Items.Count <= 0) then
    Exit;

  ItemIndex := Items.Count - 1;
end;

//------------------------------------------------------------------------------

procedure TAdvImagePickerDropDown.SelectNext;
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

//------------------------------------------------------------------------------

procedure TAdvImagePickerDropDown.SelectPrevious;
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

//------------------------------------------------------------------------------

procedure TAdvImagePickerDropDown.SetColumns(const Value: Integer);
begin
  FColumns := Value;
end;

procedure TAdvImagePickerDropDown.SetComponentStyle(AStyle: TTMSStyle);
begin
  inherited;
  SetAppearanceStyle(ItemAppearance, AStyle);
end;

//------------------------------------------------------------------------------

procedure TAdvImagePickerDropDown.SetItemIndex(const Value: Integer);
begin
  if (FItemIndex <> Value) and (Value < FItems.Count) then
  begin
    FItemIndex := Value;
    Invalidate;

    if Assigned(OnChange) then
      OnChange(Self);

    if Assigned(FOnSelect) then
      FOnSelect(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvImagePickerDropDown.SetLayout(const Value: TItemLayout);
begin
  FLayout := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvImagePickerDropDown.SetItems(const Value: TImageItems);
begin
  FItems.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvImagePickerDropDown.OnItemsChanged(Sender: TObject);
begin

end;

//------------------------------------------------------------------------------

procedure TAdvImagePickerDropDown.OnItemSelectorDrawItem(Sender: TObject;
  Canvas: TCanvas; R: TRect; Index: Integer);
begin
  if Assigned(OnDrawItem) then
    OnDrawItem(Self, Canvas, R, Index);
end;

//------------------------------------------------------------------------------

procedure TAdvImagePickerDropDown.OnKeyTimerTime(Sender: TObject);
begin
  FKeyTimer.Enabled := False;
  FCurSearch := '';
end;

//------------------------------------------------------------------------------

procedure TAdvImagePickerDropDown.OnSelectorItemSelect(Sender: TObject);
begin
  if Assigned(FItemSelector) then
  begin
    ItemIndex := FItemSelector.ItemIndex;
    if not FInternalCall then    
      DoHideDropDown(False);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvImagePickerDropDown.SetImageIndex(const Value: integer);
var
  i: integer;
  found: boolean;
begin
  found := false;
  for i := 0 to Items.Count - 1 do
  begin
    if  Items[i].ImageIndex = Value then
    begin
      ItemIndex := i;
      found := true;
      break;
    end;
  end;
  if not found then
    ItemIndex := -1;
end;

procedure TAdvImagePickerDropDown.SetItemAppearance(
  const Value: TItemAppearance);
begin
  FItemAppearance.Assign(Value);
end;

//------------------------------------------------------------------------------

end.
