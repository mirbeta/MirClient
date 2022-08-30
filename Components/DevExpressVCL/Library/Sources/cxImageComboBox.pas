{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEditors                                           }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSEDITORS AND ALL                }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit cxImageComboBox;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Messages, ComCtrls, Types, Variants, SysUtils, Classes, Graphics,
  Controls, Forms, StdCtrls, ExtCtrls, Clipbrd, ImgList,
  dxCore, dxCoreClasses, cxVariants, cxClasses, cxGraphics, cxControls, cxContainer,
  cxDataStorage, cxDataUtils, cxEdit, cxDropDownEdit, cxTextEdit, cxFilterControlUtils;

type
  { TcxImageComboBoxItem }

  TcxImageComboBoxItem = class(TCollectionItem)
  strict private
    FDescription: TCaption;
    FImageIndex: TcxImageIndex;
    FTag: TcxTag;
    FValue: Variant;

    function IsStoredValue: Boolean;
    function IsTagStored: Boolean;
    procedure SetDescription(const Value: TCaption);
    procedure SetImageIndex(const Value: TcxImageIndex);
    procedure SetValue(const AValue: Variant);
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Description: TCaption read FDescription write SetDescription;
    property ImageIndex: TcxImageIndex read FImageIndex write SetImageIndex default -1;
    property Tag: TcxTag read FTag write FTag stored IsTagStored;
    property Value: Variant read FValue write SetValue stored IsStoredValue;
  end;

  { TcxImageComboBoxItems }

  TcxImageComboBoxItems = class(TOwnedCollection)
  strict private
    function GetItems(Index: Integer): TcxImageComboBoxItem;
    procedure SetItems(Index: Integer; const Value: TcxImageComboBoxItem);
  protected
    procedure InternalChanged;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TcxImageComboBoxItem;
    property Items[Index: Integer]: TcxImageComboBoxItem read GetItems write SetItems; default;
  end;

  { TcxImageComboBoxListBox }

  TcxCustomImageComboBox = class;
  TcxCustomImageComboBoxProperties = class;

  TcxImageComboBoxListBox = class(TcxComboBoxListBox)
  strict private
    FClientWidth: Integer;
    FHasScrollbar: Boolean;

    function GetEdit: TcxCustomImageComboBox;
    function GetProperties: TcxCustomImageComboBoxProperties;
    function IsDescriptionsAssigned: Boolean;
  protected
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    function GetDrawTextFlags: Cardinal; override;
    procedure MeasureItem(Index: Integer; var Height: Integer); override;
    procedure RecreateWindow; override;
    function GetImageRect(AImages: TCustomImageList; var R: TRect): TRect;
    function GetImages: TCustomImageList;
    function GetMaxItemWidth: Integer; virtual;

    property Edit: TcxCustomImageComboBox read GetEdit;
    property Properties: TcxCustomImageComboBoxProperties read GetProperties;
  public
    constructor Create(AOwner: TComponent); override;
    function GetHeight(ARowCount: Integer; AMaxHeight: Integer): Integer; override;
    function GetItemWidth(AIndex: Integer): Integer; override;
  end;

  { TcxImageComboBoxLookupData }

  TcxImageComboBoxLookupData = class(TcxComboBoxLookupData)
  strict private
    function GetActiveProperties: TcxCustomImageComboBoxProperties;
    function GetItems: TcxImageComboBoxItems;
    function GetList: TcxImageComboBoxListBox;
  protected
    function GetListBoxClass: TcxCustomEditListBoxClass; override;
    function GetImageIndex(Index: Integer): Integer;
    function GetItem(Index: Integer): string; override;
    function GetItemCount: Integer; override;
    property ActiveProperties: TcxCustomImageComboBoxProperties read GetActiveProperties;
    property Items: TcxImageComboBoxItems read GetItems;
    property List: TcxImageComboBoxListBox read GetList;
  public
    procedure TextChanged; override;
  end;

  { TcxImageComboBoxViewData }

  TcxImageComboBoxViewData = class(TcxCustomDropDownEditViewData)
  strict private
    function GetProperties: TcxCustomImageComboBoxProperties;
  protected
    function InternalEditValueToDisplayText(AEditValue: TcxEditValue): string; override;
    function InternalGetEditConstantPartSize(ACanvas: TcxCanvas; AIsInplace: Boolean;
      AEditSizeProperties: TcxEditSizeProperties; var MinContentSize: TSize;
      AViewInfo: TcxCustomEditViewInfo): TSize; override;
    function IsComboBoxStyle: Boolean; override;
  public
    procedure Calculate(ACanvas: TcxCanvas; const ABounds: TRect;
      const P: TPoint; Button: TcxMouseButton; Shift: TShiftState;
      ViewInfo: TcxCustomEditViewInfo; AIsMouseEvent: Boolean); override;
    procedure DisplayValueToDrawValue(const ADisplayValue: TcxEditValue;
      AViewInfo: TcxCustomEditViewInfo); override;
    procedure EditValueToDrawValue(const AEditValue: TcxEditValue;
      AViewInfo: TcxCustomEditViewInfo); override;
    property Properties: TcxCustomImageComboBoxProperties read GetProperties;
  end;

  { TcxImageComboBoxViewInfo }

  TcxImageAlign = (iaLeft, iaRight);

  TcxImageComboBoxViewInfo = class(TcxCustomTextEditViewInfo)
  protected
    procedure InternalPaint(ACanvas: TcxCanvas); override;
  public
    ImageRect: TRect;
    ShowDescriptions: Boolean;
    ImageAlign: TcxImageAlign;
    ImageIndex: TcxImageIndex;
    Images: TCustomImageList;

    procedure DrawText(ACanvas: TcxCanvas); override;
    procedure Offset(DX, DY: Integer); override;
  end;

  { TcxCustomImageComboBoxProperties }

  TcxCustomImageComboBoxProperties = class(TcxCustomComboBoxProperties)
  private
    FDefaultDescription: string;
    FDefaultImageIndex: TcxImageIndex;
    FImageAlign: TcxImageAlign;
    FImages: TCustomImageList;
    FImagesChangeLink: TChangeLink;
    FItems: TcxImageComboBoxItems;
    FLargeImages: TCustomImageList;
    FLargeImagesChangeLink: TChangeLink;
    FMultiLineText: Boolean;
    FShowDescriptions: Boolean;
    FShowImageIndexInsteadDescription: Boolean;

    function GetItems: TcxImageComboBoxItems;
    procedure SetDefaultDescription(const Value: string);
    procedure SetDefaultImageIndex(const Value: TcxImageIndex);
    procedure SetImageAlign(const Value: TcxImageAlign);
    procedure SetImages(Value: TCustomImageList);
    procedure SetItems(const Value: TcxImageComboBoxItems);
    procedure SetLargeImages(Value: TCustomImageList);
    procedure SetMultiLineText(const Value: Boolean);
    procedure SetShowDescriptions(const Value: Boolean);
    procedure SetShowImageIndexInsteadDescription(AValue: Boolean);
  protected
    function FindItemByText(const AText: string): TcxImageComboBoxItem;
    function FindLookupText(const AText: string): Boolean; override;

    procedure DoAssign(AProperties: TcxCustomEditProperties); override;
    procedure FreeNotification(Sender: TComponent); override;
    function GetDisplayFormatOptions: TcxEditDisplayFormatOptions; override;
    function GetItemDescription(AItem: TcxImageComboBoxItem; AImageIndex: Integer): string;
    class function GetLookupDataClass: TcxInterfacedPersistentClass; override;
    class function GetViewDataClass: TcxCustomEditViewDataClass; override;
    function HasDisplayValue: Boolean; override;

    procedure InternalGetImageComboBoxDisplayValue(AItem: TcxImageComboBoxItem; out AText: string;
      out AImageIndex: TcxImageIndex; AAlwaysShowDescription: Boolean = False); virtual;

    property ShowImageIndexInsteadDescription: Boolean read FShowImageIndexInsteadDescription write SetShowImageIndexInsteadDescription;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    function CompareDisplayValues(const AEditValue1, AEditValue2: TcxEditValue): Boolean; override;
    function FindItemByValue(const AValue: Variant): TcxImageComboBoxItem;
    class function GetContainerClass: TcxContainerClass; override;
    function GetDisplayText(const AEditValue: TcxEditValue; AFullText: Boolean = False; AIsInplace: Boolean = True): string; override;
    function GetEditValueSource(AEditFocused: Boolean): TcxDataEditValueSource; override;
    procedure GetImageComboBoxDisplayValue(const AEditValue: TcxEditValue; out AText: string; out AImageIndex: TcxImageIndex);
    function GetSupportedOperations: TcxEditSupportedOperations; override;
    class function GetViewInfoClass: TcxContainerViewInfoClass; override;
    function IsEditValueValid(var AEditValue: TcxEditValue; AEditFocused: Boolean): Boolean; override;
    procedure PrepareDisplayValue(const AEditValue: TcxEditValue; var DisplayValue: TcxEditValue; AEditFocused: Boolean); override;
    // !!!
    property DefaultDescription: string read FDefaultDescription write SetDefaultDescription;
    property DefaultImageIndex: TcxImageIndex read FDefaultImageIndex write SetDefaultImageIndex default -1;
    property ImageAlign: TcxImageAlign read FImageAlign write SetImageAlign default iaLeft;
    property Images: TCustomImageList read FImages write SetImages;
    property Items: TcxImageComboBoxItems read GetItems write SetItems;
    property LargeImages: TCustomImageList read FLargeImages write SetLargeImages;
    property MultiLineText: Boolean read FMultiLineText write SetMultiLineText default False;
    property ShowDescriptions: Boolean read FShowDescriptions write SetShowDescriptions default True;
  end;

  { TcxImageComboBoxProperties }

  TcxImageComboBoxProperties = class(TcxCustomImageComboBoxProperties)
  published
    property Alignment;
    property AssignedValues;
    property ButtonGlyph;
    property ClearKey;
    property DefaultDescription;
    property DefaultImageIndex;
    property DropDownRows;
    property ImageAlign;
    property Images;
    property ImeMode;
    property ImeName;
    property ImmediateDropDownWhenActivated;
    property ImmediateDropDownWhenKeyPressed;
    property ImmediatePost;
    property ImmediateUpdateText;
    property IncrementalFiltering;
    property IncrementalFilteringOptions;
    property Items;
    property LargeImages;
    property MultiLineText;
    property PopupAlignment;
    property PostPopupValueOnTab;
    property ReadOnly;
    property Revertable;
    property ShowDescriptions;
    property ValidateOnEnter;
    property ValidationErrorIconAlignment;
    property ValidationOptions;
    property OnButtonClick;
    property OnChange;
    property OnCloseQuery;
    property OnCloseUp;
    property OnEditValueChanged;
    property OnInitPopup;
    property OnPopup;
    property OnValidate;
  end;

  { TcxCustomImageComboBox }

  TcxCustomImageComboBox = class(TcxCustomComboBox)
  private
    function GetProperties: TcxCustomImageComboBoxProperties;
    function GetActiveProperties: TcxCustomImageComboBoxProperties;
    function GetLookupData: TcxImageComboBoxLookupData;
    procedure SetProperties(const Value: TcxCustomImageComboBoxProperties);
  protected
    function GetItemObject: TObject; override;
    function GetPopupWindowClientPreferredSize: TSize; override;
    function InternalGetEditingValue: TcxEditValue; override;
    function IsValidChar(AChar: Char): Boolean; override;
    function LookupKeyToEditValue(const AKey: TcxEditValue): TcxEditValue; override;
    procedure SynchronizeDisplayValue; override;
    procedure UpdateDrawValue; override;
    property LookupData: TcxImageComboBoxLookupData read GetLookupData;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    procedure PrepareEditValue(const ADisplayValue: TcxEditValue; out EditValue: TcxEditValue; AEditFocused: Boolean); override;
    property ActiveProperties: TcxCustomImageComboBoxProperties read GetActiveProperties;
    property Properties: TcxCustomImageComboBoxProperties read GetProperties write SetProperties;
  end;

  { TcxImageComboBox }

  TcxImageComboBox = class(TcxCustomImageComboBox)
  private
    function GetActiveProperties: TcxImageComboBoxProperties;
    function GetProperties: TcxImageComboBoxProperties;
    procedure SetProperties(Value: TcxImageComboBoxProperties);
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxImageComboBoxProperties read GetActiveProperties;
  published
    property Anchors;
    property AutoSize;
    property BeepOnEnter;
    property BiDiMode;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EditValue;
    property Enabled;
    property ItemIndex;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Properties: TcxImageComboBoxProperties read GetProperties write SetProperties;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnEditing;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

  { TcxFilterImageComboBoxHelper }

  TcxFilterImageComboBoxHelper = class(TcxFilterComboBoxHelper)
  public
    class function GetFilterEditClass: TcxCustomEditClass; override;
    class function GetSupportedFilterOperators(
      AProperties: TcxCustomEditProperties;
      AValueTypeClass: TcxValueTypeClass;
      AExtendedSet: Boolean = False): TcxFilterControlOperators; override;
    class procedure InitializeProperties(AProperties,
      AEditProperties: TcxCustomEditProperties; AHasButtons: Boolean); override;
  end;

implementation

uses
  Dialogs, Math, cxGeometry, cxButtons, cxEditConsts, cxEditUtils, cxScrollBar,
  cxLookAndFeelPainters, cxDWMApi;

type
  TcxCustomTextEditAccess = class(TcxCustomTextEdit);
  TcxControlAccess = class(TcxControl);

{ TcxImageComboBoxItem }

constructor TcxImageComboBoxItem.Create(Collection: TCollection);
var
  AImages: TCustomImageList;
begin
  FValue := Null; // for D5 variants
  inherited Create(Collection);
  AImages := TcxCustomImageComboBoxProperties(Collection.Owner).Images;
  if (AImages <> nil) and (AImages.Count >= Collection.Count) then
    FImageIndex := Collection.Count - 1
  else
    FImageIndex := -1;
end;

function TcxImageComboBoxItem.IsStoredValue: Boolean;
begin
  Result := not VarIsNull(FValue);
end;

function TcxImageComboBoxItem.IsTagStored: Boolean;
begin
  Result := FTag <> 0;
end;

procedure TcxImageComboBoxItem.SetDescription(const Value: TCaption);
begin
  if FDescription <> Value then
  begin
    FDescription := Value;
    TcxImageComboBoxItems(Collection).InternalChanged;
  end;
end;

procedure TcxImageComboBoxItem.SetImageIndex(const Value: TcxImageIndex);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    TcxImageComboBoxItems(Collection).InternalChanged;
  end;
end;

procedure TcxImageComboBoxItem.SetValue(const AValue: Variant);
begin
  if not InternalVarEqualsExact(FValue, AValue) then
  begin
    FValue := AValue;
    TcxImageComboBoxItems(Collection).InternalChanged;
  end;
end;

procedure TcxImageComboBoxItem.Assign(Source: TPersistent);
begin
  if Source is TcxImageComboBoxItem then
    with TcxImageComboBoxItem(Source) do
    begin
      Self.Description := Description;
      Self.ImageIndex := ImageIndex;
      Self.Tag := Tag;
      Self.Value := Value;
    end
  else
    inherited Assign(Source);
end;

{ TcxImageComboBoxItems }

function TcxImageComboBoxItems.GetItems(Index: Integer): TcxImageComboBoxItem;
begin
  Result := TcxImageComboBoxItem(inherited Items[Index]);
end;

procedure TcxImageComboBoxItems.SetItems(Index: Integer;
  const Value: TcxImageComboBoxItem);
begin
  inherited Items[Index] := Value;
end;

procedure TcxImageComboBoxItems.InternalChanged;
begin
  Changed;
end;

procedure TcxImageComboBoxItems.Update(Item: TCollectionItem);
begin
  (Owner as TcxCustomImageComboBoxProperties).Changed;
end;

constructor TcxImageComboBoxItems.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TcxImageComboBoxItem);
end;

function TcxImageComboBoxItems.Add: TcxImageComboBoxItem;
begin
  Result := TcxImageComboBoxItem(inherited Add);
end;

{ TcxImageComboBoxListBox }

constructor TcxImageComboBoxListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BorderStyle := bsNone;
  Style := lbOwnerDrawVariable;
end;

function TcxImageComboBoxListBox.GetHeight(ARowCount: Integer; AMaxHeight: Integer): Integer;
var
  I, H: Integer;
  R: TRect;
begin
  if Properties.MultiLineText then
  begin
    R := Edit.GetPopupWindowOwnerControlBounds;
    FClientWidth := R.Right - R.Left;
    R := Edit.PopupWindow.ViewInfo.GetClientExtent;
    Dec(FClientWidth, R.Left + R.Right);
  end
  else
    FClientWidth := 0;
  Result := 0;
  for I := 0 to ARowCount - 1 do
  begin
    H := 0;
    MeasureItem(I, H);
    Inc(Result, H);
  end;
  if Properties.MultiLineText then
  begin
    FHasScrollbar := (Result > AMaxHeight) or (ARowCount < Items.Count);
    if FHasScrollbar then
    begin
      Dec(FClientWidth, TcxControlAccess(Container).GetVScrollBarDefaultAreaWidth);
      Result := 0;
      for I := 0 to ARowCount - 1 do
      begin
        H := 0;
        MeasureItem(I, H);
        Inc(Result, H);
      end;
    end;
  end;
end;

function TcxImageComboBoxListBox.GetItemWidth(AIndex: Integer): Integer;
begin
  if Properties.MultiLineText then
    Result := 0
  else
    Result := inherited GetItemWidth(AIndex);
end;

procedure TcxImageComboBoxListBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);

  procedure BufferedFillRect;
  var
    ATempBitmap: TcxBitmap;
  begin
     ATempBitmap := TcxBitmap.CreateSize(Rect, pfDevice);
     try
       ATempBitmap.cxCanvas.FillRect(ATempBitmap.ClientRect, Canvas.Brush.Color);
       cxBitBlt(Canvas.Handle, ATempBitmap.Canvas.Handle, Rect, cxNullPoint, SRCCOPY);
     finally
       ATempBitmap.Free;
     end;
  end;

var
  AText: string;
  AImages: TCustomImageList;
  AImageIndex: Integer;
begin
  if not DoDrawItem(Index, Rect, State) then
  begin
    if IsWinSeven then
      BufferedFillRect
    else
      Canvas.FillRect(Rect);

    if (Index > -1) and (Index < Edit.LookupData.GetFilteredItemCount) then
    begin
      if UseRightToLeftAlignment then
        Dec(Rect.Right, ScaleFactor.Apply(2))
      else
        Inc(Rect.Left, ScaleFactor.Apply(2));

      AText := GetItem(Index);
      AImages := GetImages;
      AImageIndex := Edit.LookupData.GetImageIndex(Index);
      if IsImageAssigned(AImages, AImageIndex) then
      begin
        cxDrawImage(Canvas, GetImageRect(AImages, Rect), nil, AImages,
          AImageIndex, ifmNormal, EnabledImageDrawModeMap[Enabled], False, nil, ScaleFactor);
      end;
      if not IsRectEmpty(Rect) then
        if IsHighlightSearchText then
          DrawItemText(AText, Rect)
        else
          cxDrawText(Canvas, AText, Rect, GetDrawTextFlags);
    end;
  end;
end;

function TcxImageComboBoxListBox.GetDrawTextFlags: Cardinal;
begin
  if Properties.MultiLineText and not IsHighlightSearchText then
    Result := DrawTextBiDiModeFlags(DT_EXPANDTABS or DT_NOPREFIX or DT_WORDBREAK)
  else
    Result := inherited GetDrawTextFlags;
end;

procedure TcxImageComboBoxListBox.MeasureItem(Index: Integer; var Height: Integer);
var
  AData: string;
  AImageSize: TSize;
  R: TRect;
  W, H: Integer;
begin
  W := FClientWidth - ScaleFactor.Apply(2);
  AImageSize := dxGetImageSize(GetImages, ScaleFactor);
  if not cxSizeIsEmpty(AImageSize) then
  begin
    Dec(W, AImageSize.cx + ScaleFactor.Apply(4));
    H := AImageSize.cy + ScaleFactor.Apply(2);
  end
  else
    H := 0;

  if Properties.MultiLineText and (W > 0) then
  begin
    R := Rect(0, 0, W, H);
    AData := GetItem(Index);
    DrawText(Canvas.Handle, PChar(AData), Length(AData), R, GetDrawTextFlags or DT_CALCRECT);
    H := Max(H, R.Bottom - R.Top + ScaleFactor.Apply(2));
  end
  else
    H := Max(cxTextHeight(Canvas.Handle) + ScaleFactor.Apply(2), H);

  Height := H;
  dxAdjustToTouchableSize(Height, ScaleFactor);
  if (Index >= 0) and Edit.IsOnMeasureItemEventAssigned then
    Edit.DoOnMeasureItem(Index, Canvas, Height);
end;

procedure TcxImageComboBoxListBox.RecreateWindow;
begin
  InternalRecreateWindow;
end;

function TcxImageComboBoxListBox.GetImageRect(AImages: TCustomImageList; var R: TRect): TRect;
var
  AImageSize: TSize;
begin
  if AImages <> nil then
  begin
    Result := R;
    AImageSize := dxGetImageSize(AImages, ScaleFactor);
    if IsDescriptionsAssigned or Properties.ShowDescriptions then
    begin
      if (Properties.ImageAlign = iaLeft) xor UseRightToLeftAlignment then
      begin
        Result.Right := Result.Left + AImageSize.cx + ScaleFactor.Apply(4);
        R.Left := Result.Right;
      end
      else
      begin
        Result.Left := Result.Right - (AImageSize.cx + ScaleFactor.Apply(4));
        R.Right := Result.Left;
      end;
    end;
    Result := cxRectCenter(Result, AImageSize);
  end
  else
    Result := cxEmptyRect;
end;

function TcxImageComboBoxListBox.GetImages: TCustomImageList;
begin
  Result := Properties.LargeImages;
  if Result = nil then
    Result := Properties.Images;
end;

function TcxImageComboBoxListBox.GetMaxItemWidth: Integer;
var
  AImageSize: TSize;
  I: Integer;
begin
  Result := 0;
  for I := 0 to Properties.Items.Count - 1 do
    Result := Max(Result, Canvas.TextWidth(Properties.GetItemDescription(
      Properties.Items[I], Properties.Items[I].ImageIndex)));

  AImageSize := dxGetImageSize(GetImages, ScaleFactor);
  if cxSizeIsEmpty(AImageSize) then
    Inc(Result, ScaleFactor.Apply(4))
  else
    Inc(Result, ScaleFactor.Apply(4) * 2 + AImageSize.cx);

  if Properties.DropDownRows < Items.Count then
    Inc(Result, TcxControlAccess(Container).GetVScrollBarDefaultAreaWidth);
end;

function TcxImageComboBoxListBox.GetEdit: TcxCustomImageComboBox;
begin
  Result := TcxCustomImageComboBox(inherited Edit);
end;

function TcxImageComboBoxListBox.GetProperties: TcxCustomImageComboBoxProperties;
begin
  Result := Edit.ActiveProperties;
end;

function TcxImageComboBoxListBox.IsDescriptionsAssigned: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Properties.Items.Count - 1 do
  begin
    Result := Result or (Properties.Items[I].Description <> '');
    if Result then
      Break;
  end;
end;

{ TcxImageComboBoxLookupData }

procedure TcxImageComboBoxLookupData.TextChanged;
var
  AItem: TcxImageComboBoxItem;
begin
  if (Edit as TcxCustomImageComboBox).EditModeSetting then
    Exit;
  AItem := ActiveProperties.FindItemByValue(Edit.EditValue);
  if AItem <> nil then
    InternalSetCurrentKey(AItem.Index)
  else
    InternalSetCurrentKey(-1);
end;

function TcxImageComboBoxLookupData.GetListBoxClass: TcxCustomEditListBoxClass;
begin
  Result := TcxImageComboBoxListBox;
end;

function TcxImageComboBoxLookupData.GetImageIndex(Index: Integer): Integer;
begin
  Result := Items[GetLookupItemIndexFromFilteredItemIndex(Index)].ImageIndex;
end;

function TcxImageComboBoxLookupData.GetItem(Index: Integer): string;
begin
  if ActiveProperties.ShowImageIndexInsteadDescription then
    Result := IntToStr(Items[Index].ImageIndex)
  else
    if (Index > -1) and (Index < Items.Count) then
      Result := Items[Index].Description
    else
      Result := ''
end;

function TcxImageComboBoxLookupData.GetItemCount: Integer;
begin
  Result := Items.Count;
end;

function TcxImageComboBoxLookupData.GetActiveProperties: TcxCustomImageComboBoxProperties;
begin
  Result := inherited ActiveProperties as TcxCustomImageComboBoxProperties;
end;

function TcxImageComboBoxLookupData.GetItems: TcxImageComboBoxItems;
begin
  Result := ActiveProperties.Items;
end;

function TcxImageComboBoxLookupData.GetList: TcxImageComboBoxListBox;
begin
  Result := inherited List as TcxImageComboBoxListBox;
end;

{ TcxImageComboBoxViewData }

procedure TcxImageComboBoxViewData.Calculate(ACanvas: TcxCanvas;
  const ABounds: TRect; const P: TPoint; Button: TcxMouseButton;
  Shift: TShiftState; ViewInfo: TcxCustomEditViewInfo; AIsMouseEvent: Boolean);
var
  AImageSize: TSize;
  AProperties: TcxCustomImageComboBoxProperties;
  ATextOffset: Integer;
  AViewInfo: TcxImageComboBoxViewInfo;
  R: TRect;
begin
  inherited Calculate(ACanvas, ABounds, P, Button, Shift, ViewInfo, AIsMouseEvent);

  AProperties := TcxCustomImageComboBoxProperties(Properties);
  AViewInfo := TcxImageComboBoxViewInfo(ViewInfo);

  AViewInfo.ImageAlign := AProperties.ImageAlign;
  if UseRightToLeftAlignment then
    case AViewInfo.ImageAlign of
      iaLeft:
        AViewInfo.ImageAlign := iaRight;
      iaRight:
        AViewInfo.ImageAlign := iaLeft;
    end;
  AViewInfo.Images := AProperties.Images;
  AViewInfo.ShowDescriptions := AProperties.ShowDescriptions;
  if IsInplace then
    R := AViewInfo.TextRect
  else
    R := AViewInfo.ClientRect;

  if Assigned(AViewInfo.Images) then
  begin
    AImageSize := dxGetImageSize(AViewInfo.Images, ScaleFactor);
    AViewInfo.ImageRect := cxRectInflate(AViewInfo.ClientRect, -cxTextOffset, 0);
    ATextOffset := ScaleFactor.Apply(cxTextOffset);
    if IsInplace then
      ATextOffset := 2 * ATextOffset;

    if cxRectWidth(AViewInfo.ImageRect) > AImageSize.cx then
    begin
      if AViewInfo.ShowDescriptions then
        if AViewInfo.ImageAlign = iaLeft then
        begin
          AViewInfo.ImageRect.Right := AViewInfo.ImageRect.Left + AImageSize.cx;
          R.Left := AViewInfo.ImageRect.Right + ATextOffset;
        end
        else
        begin
          AViewInfo.ImageRect.Left := AViewInfo.ImageRect.Right - AImageSize.cx;
          R.Right := AViewInfo.ImageRect.Left - ATextOffset;
        end
      else
      begin
        AViewInfo.ImageRect.Left := AViewInfo.ImageRect.Left + (cxRectWidth(AViewInfo.ImageRect) - AImageSize.cx) div 2;
        AViewInfo.ImageRect.Right := AViewInfo.ImageRect.Left + AImageSize.cx;
      end;
    end
    else
      R.Left := R.Right;

    if cxRectHeight(AViewInfo.ImageRect) > AImageSize.cy then
    begin
      AViewInfo.ImageRect.Top := AViewInfo.ImageRect.Top + (cxRectHeight(AViewInfo.ImageRect) - AImageSize.cy) div 2;
      AViewInfo.ImageRect.Bottom := AViewInfo.ImageRect.Top + AImageSize.cy;
    end;
  end;

  if not IsInplace then
  begin
    AViewInfo.ClientRect := R;
    InflateRect(R, -cxTextOffset, -1);
    AViewInfo.DrawSelectionBar := False;
  end;
  AViewInfo.TextRect := R;
  if not AViewInfo.ShowDescriptions then
    AViewInfo.Text := '';
end;

procedure TcxImageComboBoxViewData.EditValueToDrawValue(
  const AEditValue: TcxEditValue; AViewInfo: TcxCustomEditViewInfo);
var
  AText: string;
  AImageIndex: TcxImageIndex;
begin
  PrepareSelection(AViewInfo);
  Properties.GetImageComboBoxDisplayValue(AEditValue, AText, AImageIndex);
  TcxImageComboBoxViewInfo(AViewInfo).ImageIndex := AImageIndex;
  DoOnGetDisplayText(AText);
  TcxImageComboBoxViewInfo(AViewInfo).Text := AText;
end;

procedure TcxImageComboBoxViewData.DisplayValueToDrawValue(
  const ADisplayValue: TcxEditValue; AViewInfo: TcxCustomEditViewInfo);
var
  ACurrentKey: TcxEditValue;
  AItem: TcxImageComboBoxItem;
  AText: string;
  AImageIndex: TcxImageIndex;
begin
  if Edit = nil then
    Exit;
  ACurrentKey := TcxCustomImageComboBox(Edit).ILookupData.CurrentKey;
  if ACurrentKey = -1 then
    AItem := Properties.FindItemByValue(Edit.EditValue)
  else
    AItem := Properties.Items[ACurrentKey];
  Properties.InternalGetImageComboBoxDisplayValue(AItem, AText, AImageIndex);
  TcxImageComboBoxViewInfo(AViewInfo).Text := AText;
  TcxImageComboBoxViewInfo(AViewInfo).ImageIndex := AImageIndex;
end;

function TcxImageComboBoxViewData.InternalEditValueToDisplayText(AEditValue: TcxEditValue): string;
var
  AIndex: TcxImageIndex;
begin
  Properties.GetImageComboBoxDisplayValue(AEditValue, Result, AIndex);
end;

function TcxImageComboBoxViewData.InternalGetEditConstantPartSize(
  ACanvas: TcxCanvas; AIsInplace: Boolean; AEditSizeProperties: TcxEditSizeProperties;
  var MinContentSize: TSize; AViewInfo: TcxCustomEditViewInfo): TSize;
var
  AImageSize: TSize;
  AImages: TCustomImageList;
begin
  Result := inherited InternalGetEditConstantPartSize(ACanvas, AIsInplace, AEditSizeProperties, MinContentSize, AViewInfo);
  AImages := TcxCustomImageComboBoxProperties(Properties).Images;
  if Assigned(AImages) then
  begin
    AImageSize := dxGetImageSize(AImages, ScaleFactor);
    if AImages.Height > MinContentSize.cy then
      MinContentSize.cy := AImageSize.cy;
    Inc(Result.cx, AImageSize.cx + ScaleFactor.Apply(5));
  end
  else
    Inc(Result.cx, 1);
end;

function TcxImageComboBoxViewData.IsComboBoxStyle: Boolean;
begin
  Result := IsWinVistaOrLater;
end;

function TcxImageComboBoxViewData.GetProperties: TcxCustomImageComboBoxProperties;
begin
  Result := TcxCustomImageComboBoxProperties(FProperties);
end;

{ TcxImageComboBoxViewInfo }

procedure TcxImageComboBoxViewInfo.Offset(DX, DY: Integer);
begin
  inherited Offset(DX, DY);
  OffsetRect(ImageRect, DX, DY);
end;

procedure TcxImageComboBoxViewInfo.DrawText(ACanvas: TcxCanvas);
var
  R: TRect;
begin
  inherited;

  if IsImageAssigned(Images, ImageIndex) then
  begin
    ACanvas.SaveClipRegion;
    try
      IntersectRect(R, ImageRect, BorderRect);
      ACanvas.IntersectClipRect(R);

      if not (Transparent or IsWinVistaOrLater and NativeStyle) then
        ACanvas.FillRect(ImageRect, BackgroundColor);
      cxDrawImage(ACanvas, ImageRect, nil, Images, ImageIndex, ifmNormal,
        EnabledImageDrawModeMap[Enabled], False, nil, ScaleFactor)
    finally
      ACanvas.RestoreClipRegion;
    end;
  end;
end;

procedure TcxImageComboBoxViewInfo.InternalPaint(ACanvas: TcxCanvas);
begin
  if RectVisible(ACanvas.Handle, Bounds) then
    inherited InternalPaint(ACanvas);
end;

{ TcxCustomImageComboBoxProperties }

constructor TcxCustomImageComboBoxProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FShowDescriptions := True;
  FDefaultImageIndex := -1;
  FImageAlign := iaLeft;
  FItems := TcxImageComboBoxItems.Create(Self);
  DropDownListStyle := lsFixedList;
  FImagesChangeLink := TChangeLink.Create;
  FImagesChangeLink.OnChange := ChangeHandler;
  FLargeImagesChangeLink := TChangeLink.Create;
  FLargeImagesChangeLink.OnChange := ChangeHandler;
end;

destructor TcxCustomImageComboBoxProperties.Destroy;
begin
  FImagesChangeLink.Free;
  FLargeImagesChangeLink.Free;
  FItems.Free;
  inherited Destroy;
end;

function TcxCustomImageComboBoxProperties.GetItems: TcxImageComboBoxItems;
begin
  Result := FItems;
end;

procedure TcxCustomImageComboBoxProperties.SetDefaultDescription(const Value: string);
begin
  if FDefaultDescription <> Value then
  begin
    FDefaultDescription := Value;
    Changed;
  end;
end;

procedure TcxCustomImageComboBoxProperties.SetDefaultImageIndex(
  const Value: TcxImageIndex);
begin
  if FDefaultImageIndex <> Value then
  begin
    FDefaultImageIndex := Value;
    Changed;
  end;
end;

procedure TcxCustomImageComboBoxProperties.SetImageAlign(
  const Value: TcxImageAlign);
begin
  if FImageAlign <> Value then
  begin
    FImageAlign := Value;
    Changed;
  end;
end;

procedure TcxCustomImageComboBoxProperties.SetImages(Value: TCustomImageList);
begin
  cxSetImageList(Value, FImages, FImagesChangeLink, FreeNotificator);
end;

procedure TcxCustomImageComboBoxProperties.SetLargeImages(Value: TCustomImageList);
begin
  cxSetImageList(Value, FLargeImages, FLargeImagesChangeLink, FreeNotificator);
end;

procedure TcxCustomImageComboBoxProperties.SetItems(
  const Value: TcxImageComboBoxItems);
begin
  FItems.Assign(Value);
  Changed;
end;

procedure TcxCustomImageComboBoxProperties.SetMultiLineText(
  const Value: Boolean);
begin
  if FMultiLineText <> Value then
  begin
    FMultiLineText := Value;
    Changed;
  end;
end;

procedure TcxCustomImageComboBoxProperties.SetShowDescriptions(
  const Value: Boolean);
begin
  if FShowDescriptions <> Value then
  begin
    FShowDescriptions := Value;
    Changed;
  end;
end;

procedure TcxCustomImageComboBoxProperties.SetShowImageIndexInsteadDescription(AValue: Boolean);
begin
  if FShowImageIndexInsteadDescription <> AValue then
  begin
    FShowImageIndexInsteadDescription := AValue;
    Changed;
  end;
end;

function TcxCustomImageComboBoxProperties.FindItemByText(const AText: string):
  TcxImageComboBoxItem;
var
  I: Integer;
begin
  Result := nil;
  if ShowDescriptions then
    for I := 0 to Items.Count - 1 do
      if InternalCompareString(Items[I].Description, AText, False) then
      begin
        Result := Items[I];
        Break;
      end;
end;

function TcxCustomImageComboBoxProperties.FindLookupText(const AText: string): Boolean;
begin
  Result := FindItemByText(AText) <> nil;
end;

procedure TcxCustomImageComboBoxProperties.DoAssign(AProperties: TcxCustomEditProperties);
begin
  inherited;
  if AProperties is TcxCustomImageComboBoxProperties then
    with TcxCustomImageComboBoxProperties(AProperties) do
    begin
      Self.DefaultDescription := DefaultDescription;
      Self.DefaultImageIndex := DefaultImageIndex;
      Self.ImageAlign := ImageAlign;
      Self.ShowImageIndexInsteadDescription := ShowImageIndexInsteadDescription;
      Self.Images := Images;
      Self.Items.Assign(Items);
      Self.LargeImages := LargeImages;
      Self.MultiLineText := MultiLineText;
      Self.ShowDescriptions := ShowDescriptions;
    end;
end;

procedure TcxCustomImageComboBoxProperties.FreeNotification(Sender: TComponent);
begin
  inherited FreeNotification(Sender);
  if Sender = FImages then
    FImages := nil;
  if Sender = FLargeImages then
    FLargeImages := nil;
end;

function TcxCustomImageComboBoxProperties.GetDisplayFormatOptions: TcxEditDisplayFormatOptions;
begin
  Result := [];
end;

function TcxCustomImageComboBoxProperties.GetItemDescription(AItem: TcxImageComboBoxItem; AImageIndex: Integer): string;
begin
  if ShowImageIndexInsteadDescription then
    Result := IntToStr(AImageIndex)
  else
    if AItem = nil then
      Result := DefaultDescription
    else
      Result := AItem.Description
end;

class function TcxCustomImageComboBoxProperties.GetLookupDataClass: TcxInterfacedPersistentClass;
begin
  Result := TcxImageComboBoxLookupData;
end;

class function TcxCustomImageComboBoxProperties.GetViewDataClass: TcxCustomEditViewDataClass;
begin
  Result := TcxImageComboBoxViewData;
end;

function TcxCustomImageComboBoxProperties.HasDisplayValue: Boolean;
begin
  Result := False;
end;

procedure TcxCustomImageComboBoxProperties.InternalGetImageComboBoxDisplayValue(
  AItem: TcxImageComboBoxItem; out AText: string; out AImageIndex: TcxImageIndex;
  AAlwaysShowDescription: Boolean = False);
begin
  if AItem = nil then
    AImageIndex := DefaultImageIndex
  else
    AImageIndex := AItem.ImageIndex;

  if AAlwaysShowDescription or ShowDescriptions then
    AText := GetItemDescription(AItem, AImageIndex)
  else
    AText := '';
end;

function TcxCustomImageComboBoxProperties.CompareDisplayValues(
  const AEditValue1, AEditValue2: TcxEditValue): Boolean;
var
  AImageIndex1, AImageIndex2: TcxImageIndex;
  AText1, AText2: string;
begin
  GetImageComboBoxDisplayValue(AEditValue1, AText1, AImageIndex1);
  GetImageComboBoxDisplayValue(AEditValue2, AText2, AImageIndex2);
  Result := InternalCompareString(AText1, AText2, True) and ((Images = nil) or
    (AImageIndex1 = AImageIndex2));
end;

function TcxCustomImageComboBoxProperties.FindItemByValue(
  const AValue: Variant): TcxImageComboBoxItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Items.Count - 1 do
    with Items[I] do
      if VarEqualsExact(AValue, Value) then
      begin
        Result := Items[I];
        Break;
      end;
end;

class function TcxCustomImageComboBoxProperties.GetContainerClass: TcxContainerClass;
begin
  Result := TcxImageComboBox;
end;

function TcxCustomImageComboBoxProperties.GetDisplayText(const AEditValue: TcxEditValue; AFullText: Boolean = False;
  AIsInplace: Boolean = True): string;
var
  ADisplayValue: TcxEditValue;
begin
  PrepareDisplayValue(AEditValue, ADisplayValue, False);
  Result := ADisplayValue;
end;

function TcxCustomImageComboBoxProperties.GetEditValueSource(AEditFocused: Boolean): TcxDataEditValueSource;
begin
  Result := evsValue;
end;

procedure TcxCustomImageComboBoxProperties.GetImageComboBoxDisplayValue(
  const AEditValue: TcxEditValue; out AText: string;
  out AImageIndex: TcxImageIndex);
begin
  InternalGetImageComboBoxDisplayValue(FindItemByValue(AEditValue), AText, AImageIndex);
end;

function TcxCustomImageComboBoxProperties.GetSupportedOperations: TcxEditSupportedOperations;
begin
  Result := [esoEditing, esoFiltering, esoHorzAlignment, esoSorting,
    esoSortingByDisplayText];
  if Buttons.Count > 0 then
    Include(Result, esoHotTrack);
  if ShowDescriptions then
    Include(Result, esoIncSearch);
end;

class function TcxCustomImageComboBoxProperties.GetViewInfoClass: TcxContainerViewInfoClass;
begin
  Result := TcxImageComboBoxViewInfo;
end;

function TcxCustomImageComboBoxProperties.IsEditValueValid(
  var AEditValue: TcxEditValue; AEditFocused: Boolean): Boolean;
begin
  Result := True;
end;

procedure TcxCustomImageComboBoxProperties.PrepareDisplayValue(
  const AEditValue: TcxEditValue; var DisplayValue: TcxEditValue;
  AEditFocused: Boolean);
var
  AImageIndex: TcxImageIndex;
  AText: string;
begin
  InternalGetImageComboBoxDisplayValue(FindItemByValue(AEditValue), AText, AImageIndex, True);
  DisplayValue := AText;
end;

{ TcxCustomImageComboBox }

function TcxCustomImageComboBox.GetProperties: TcxCustomImageComboBoxProperties;
begin
  Result := TcxCustomImageComboBoxProperties(inherited Properties);
end;

function TcxCustomImageComboBox.GetActiveProperties: TcxCustomImageComboBoxProperties;
begin
  Result := TcxCustomImageComboBoxProperties(InternalGetActiveProperties);
end;

function TcxCustomImageComboBox.GetLookupData: TcxImageComboBoxLookupData;
begin
  Result := TcxImageComboBoxLookupData(inherited LookupData);
end;

procedure TcxCustomImageComboBox.SetProperties(const Value: TcxCustomImageComboBoxProperties);
begin
  Properties.Assign(Value);
end;

function TcxCustomImageComboBox.IsValidChar(AChar: Char): Boolean;
begin
  Result := IsTextChar(AChar);
end;

function TcxCustomImageComboBox.GetItemObject: TObject;
begin
  Result := nil;
end;

function TcxCustomImageComboBox.GetPopupWindowClientPreferredSize: TSize;
begin
  Result := inherited GetPopupWindowClientPreferredSize;
  if not ActiveProperties.MultiLineText then
  begin
    if (LookupData.ActiveControl <> nil) and (LookupData.ActiveControl is TcxImageComboBoxListBox) then
      with TcxImageComboBoxListBox(LookupData.ActiveControl) do
        Result.cx := Max(GetMaxItemWidth, Result.cx);
  end
  else
  begin
    Result.cx := 0;
    if LookupData <> nil then
      LookupData.List.RecreateWindow;
  end;
end;

function TcxCustomImageComboBox.InternalGetEditingValue: TcxEditValue;
begin
  PrepareEditValue(Null, Result, True);
end;

function TcxCustomImageComboBox.LookupKeyToEditValue(const AKey: TcxEditValue): TcxEditValue;
begin
  if not VarEqualsExact(AKey, -1) then
    Result := ActiveProperties.Items[AKey].Value
  else
    Result := Null;
end;

procedure TcxCustomImageComboBox.SynchronizeDisplayValue;
var
  APrevLookupKey: TcxEditValue;
begin
  SaveModified;
  try
    APrevLookupKey := ILookupData.CurrentKey;
    LockClick(True);
    try
      ILookupData.TextChanged;
    finally
      LockClick(False);
      if (*ModifiedAfterEnter and *)not VarEqualsExact(APrevLookupKey, ILookupData.CurrentKey) then
        DoClick;
    end;
  finally
    RestoreModified;
    ResetOnNewDisplayValue;
    UpdateDrawValue;
  end;
end;

procedure TcxCustomImageComboBox.UpdateDrawValue;
begin
  inherited UpdateDrawValue;
  SetInternalDisplayValue(ViewInfo.Text);
end;

class function TcxCustomImageComboBox.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCustomImageComboBoxProperties;
end;

procedure TcxCustomImageComboBox.PrepareEditValue(
  const ADisplayValue: TcxEditValue; out EditValue: TcxEditValue;
  AEditFocused: Boolean);
begin
  if VarEqualsExact(LookupData.CurrentKey, -1) then
    EditValue := Null
  else
    EditValue := ActiveProperties.Items[LookupData.CurrentKey].Value;
end;

{ TcxImageComboBox }

class function TcxImageComboBox.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxImageComboBoxProperties;
end;

function TcxImageComboBox.GetActiveProperties: TcxImageComboBoxProperties;
begin
  Result := TcxImageComboBoxProperties(InternalGetActiveProperties);
end;

function TcxImageComboBox.GetProperties: TcxImageComboBoxProperties;
begin
  Result := TcxImageComboBoxProperties(inherited Properties);
end;

procedure TcxImageComboBox.SetProperties(Value: TcxImageComboBoxProperties);
begin
  Properties.Assign(Value);
end;

{ TcxFilterImageComboBoxHelper }

class function TcxFilterImageComboBoxHelper.GetFilterEditClass: TcxCustomEditClass;
begin
  Result := TcxImageComboBox;
end;

class function TcxFilterImageComboBoxHelper.GetSupportedFilterOperators(
  AProperties: TcxCustomEditProperties;
  AValueTypeClass: TcxValueTypeClass;
  AExtendedSet: Boolean = False): TcxFilterControlOperators;
begin
  Result := [fcoEqual, fcoNotEqual, fcoBlanks, fcoNonBlanks];
  if AExtendedSet then
    Result := Result + [fcoInList, fcoNotInList];
end;

class procedure TcxFilterImageComboBoxHelper.InitializeProperties(AProperties,
  AEditProperties: TcxCustomEditProperties; AHasButtons: Boolean);
begin
  inherited InitializeProperties(AProperties, AEditProperties, AHasButtons);
  TcxImageComboBoxProperties(AProperties).DropDownListStyle := lsFixedList;
  TcxImageComboBoxProperties(AProperties).ImmediateDropDownWhenKeyPressed := True;
  TcxImageComboBoxProperties(AProperties).ShowDescriptions := True;
end;

initialization
  GetRegisteredEditProperties.Register(TcxImageComboBoxProperties, scxSEditRepositoryImageComboBoxItem);
  FilterEditsController.Register(TcxImageComboBoxProperties, TcxFilterImageComboBoxHelper);

finalization
  FilterEditsController.Unregister(TcxImageComboBoxProperties, TcxFilterImageComboBoxHelper);

end.
