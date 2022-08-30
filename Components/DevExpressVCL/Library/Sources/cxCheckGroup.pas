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

unit cxCheckGroup;

{$I cxVer.inc}

interface

uses
  Windows, Classes, Graphics, Messages, Controls,
  dxCore, cxContainer, cxGraphics, cxGeometry, cxLookAndFeels,cxEdit, cxGroupBox, dxGDIPlusClasses,
  cxCheckBox, cxFilterControlUtils, cxCheckComboBox,
  cxLookAndFeelPainters;

type
  { TcxCheckGroupItem }

  TcxCheckGroupItem = class(TcxButtonGroupItem)
  published
    property Caption;
    property Enabled;
    property Tag;
  end;

  { TcxCheckGroupItems }

  TcxCheckGroupItems = class(TcxButtonGroupItems)
  private
    function GetItem(Index: Integer): TcxCheckGroupItem;
    procedure SetItem(Index: Integer; Value: TcxCheckGroupItem);
  public
    function Add: TcxCheckGroupItem;
    property Items[Index: Integer]: TcxCheckGroupItem
      read GetItem write SetItem; default;
  end;

  { TcxCheckGroupButtonViewInfo }

  TcxCheckGroupButtonViewInfo = class(TcxButtonGroupButtonViewInfo)
  public
    State: TcxCheckBoxState;
    function GetGlyphRect(const AGlyphSize: TSize; AAlignment: TLeftRight; AIsPaintCopy: Boolean): TRect; override;
  end;

  TcxCustomCheckGroup = class;

  { TcxCheckGroupViewInfo }

  TcxCheckGroupViewInfo = class(TcxButtonGroupViewInfo)
  private
    function GetEdit: TcxCustomCheckGroup;
  protected
    procedure DrawButtonCaption(ACanvas: TcxCanvas;
      AButtonViewInfo: TcxGroupBoxButtonViewInfo; const AGlyphRect: TRect); override;
    procedure DrawButtonGlyph(ACanvas: TcxCanvas;
      AButtonViewInfo: TcxGroupBoxButtonViewInfo; const AGlyphRect: TRect); override;
    function GetButtonViewInfoClass: TcxEditButtonViewInfoClass; override;
    function IsButtonGlyphTransparent(AButtonViewInfo: TcxGroupBoxButtonViewInfo): Boolean; override;

    procedure SetOnDrawBackground(AValue: TcxEditDrawBackgroundEvent); override;
  public
    CheckBorderStyle: TcxEditCheckBoxBorderStyle;
    property Edit: TcxCustomCheckGroup read GetEdit;
  end;

  { TcxCheckGroupViewData }

  TcxCheckGroupViewData = class(TcxButtonGroupViewData)
  protected
    function GetCaptionRectExtent: TRect; override;
    procedure GetEditMetrics(AAutoHeight: Boolean; ACanvas: TcxCanvas; out AMetrics: TcxEditMetrics); override;
    function GetRealItemAlignment(AAlignment: TLeftRight): TLeftRight;
  public
    procedure Calculate(ACanvas: TcxCanvas; const ABounds: TRect; const P: TPoint;
      Button: TcxMouseButton; Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo;
      AIsMouseEvent: Boolean); override;
    procedure EditValueToDrawValue(const AEditValue: TcxEditValue;
      AViewInfo: TcxCustomEditViewInfo); override;
  end;

  { TcxCustomCheckGroupProperties }

  TcxCustomCheckGroupProperties = class(TcxCustomButtonGroupProperties, IdxMultiPartGlyphSupport)
  private
    FAllowGrayed: Boolean;
    FEditValueFormat: TcxCheckStatesValueFormat;
    FGlyph: TdxSmartGlyph;
    FGlyphCount: Integer;
    FItemAlignment: TLeftRight;
    FOnEditValueToStates: TcxValueToCheckStatesEvent;
    FOnStatesToEditValue: TcxCheckStatesToValueEvent;
    function GetItems: TcxCheckGroupItems;
    procedure SetAllowGrayed(Value: Boolean);
    procedure SetEditValueFormat(Value: TcxCheckStatesValueFormat);
    procedure SetGlyph(Value: TdxSmartGlyph);
    procedure SetGlyphCount(Value: Integer);
    procedure SetItemAlignment(Value: TLeftRight);
    procedure SetItems(Value: TcxCheckGroupItems);

    procedure CheckEditValueFormat;
    function GetRealEditValueFormat: TcxCheckStatesValueFormatEx;
    procedure ItemsChanged(Sender: TObject; AItem: TCollectionItem);
  protected
    function CreateItems: TcxButtonGroupItems; override;
    procedure DoAssign(AProperties: TcxCustomEditProperties); override;
    class function GetViewDataClass: TcxCustomEditViewDataClass; override;
    function HasDisplayValue: Boolean; override;

    // IdxMultiPartGlyphSupport
    function GetGlyphCount: Integer;
    function GetStateCaption(AIndex: Integer): string;

    procedure CalculateCheckStatesByEditValue(Sender: TObject;
      const AEditValue: TcxEditValue; var ACheckStates: TcxCheckStates); virtual;
    procedure CalculateEditValueByCheckStates(Sender: TObject;
      const ACheckStates: TcxCheckStates; out AEditValue: TcxEditValue); virtual;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    function CompareDisplayValues(const AEditValue1, AEditValue2: TcxEditValue): Boolean; override;
    class function GetContainerClass: TcxContainerClass; override;
    function GetDisplayText(const AEditValue: TcxEditValue;
      AFullText: Boolean = False; AIsInplace: Boolean = True): string; override;
    function GetSpecialFeatures: TcxEditSpecialFeatures; override;
    class function GetViewInfoClass: TcxContainerViewInfoClass; override;
    // !!!
    property AllowGrayed: Boolean read FAllowGrayed write SetAllowGrayed default False;
    property EditValueFormat: TcxCheckStatesValueFormat read FEditValueFormat write SetEditValueFormat default cvfIndices;
    property Glyph: TdxSmartGlyph read FGlyph write SetGlyph;
    property GlyphCount: Integer read FGlyphCount write SetGlyphCount default 6;
    property ItemAlignment: TLeftRight read FItemAlignment write SetItemAlignment default taLeftJustify;
    property Items: TcxCheckGroupItems read GetItems write SetItems;
    property OnEditValueToStates: TcxValueToCheckStatesEvent read FOnEditValueToStates write FOnEditValueToStates;
    property OnStatesToEditValue: TcxCheckStatesToValueEvent read FOnStatesToEditValue write FOnStatesToEditValue;
  end;

  { TcxCheckGroupProperties }

  TcxCheckGroupProperties = class(TcxCustomCheckGroupProperties)
  published
    property AllowGrayed;
    property AssignedValues;
    property ClearKey;
    property Columns;
    property EditValueFormat;
    property Glyph;
    property GlyphCount;
    property ImmediatePost;
    property ItemAlignment;
    property Items;
    property ReadOnly;
    property ShowEndEllipsis;
    property WordWrap;
    property OnChange;
    property OnEditValueChanged;
    property OnEditValueToStates;
    property OnStatesToEditValue;
  end;

  { TcxCheckGroupCheckButtonViewInfo }

  TcxCheckGroupCheckButtonViewInfo = class(TcxCustomCheckBoxViewInfo)
  protected
    procedure InternalPaint(ACanvas: TcxCanvas); override;
  end;

  { TcxCheckGroupButtonProperties }

  TcxCheckGroupButtonProperties = class(TcxCheckBoxProperties)
  protected
    function InternalGetGlyph: TdxSmartGlyph; override;
    function IsEmbeddedEdit: Boolean; override;
  public
    class function GetViewInfoClass: TcxContainerViewInfoClass; override;
  end;

  { TcxCheckGroupButton }

  TcxCheckGroupButton = class(TcxCustomCheckBox, IcxContainerInnerControl)
  private
    FColumn: Integer;
    FRow: Integer;
    function GetCheckGroup: TcxCustomCheckGroup;
  protected
    function CanAutoSize: Boolean; override;
    function CanFocusOnClick: Boolean; override;
    procedure DoExit; override;
    procedure Initialize; override;
    function IsNativeBackground: Boolean; override;
    function IsNativeStyle: Boolean; override;
    function IsTransparent: Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); override;
    procedure WndProc(var Message: TMessage); override;

    // IcxContainerInnerControl
    function GetControl: TWinControl;
    function GetControlContainer: TcxContainer;

    procedure CheckTransparentBorder;
    property CheckGroup: TcxCustomCheckGroup read GetCheckGroup;
  public
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
  end;

  { TcxCustomCheckGroup }

  TcxCustomCheckGroup = class(TcxCustomButtonGroup)
  private
    FButtonStatesChanging: Boolean;
    FFocusedItemIndex: Integer;
    FStatesItems: Boolean;
    procedure DoButtonChange(Sender: TObject);
    procedure DoButtonEditing(Sender: TObject; var CanEdit: Boolean);
    procedure DoButtonFocusChanged(Sender: TObject);
    function GetActiveProperties: TcxCustomCheckGroupProperties;
    function GetButton(Index: Integer): TcxCheckGroupButton;
    function GetProperties: TcxCustomCheckGroupProperties;
    function GetState(Index: Integer): TcxCheckBoxState;
    procedure SetProperties(Value: TcxCustomCheckGroupProperties);
    procedure SetState(Index: Integer; Value: TcxCheckBoxState);
  protected
    procedure ArrangeButtons; override;
    procedure DoSetFocusWhenActivate; override;
    function GetButtonDC(AButtonIndex: Integer): THandle; override;
    function GetButtonInstance: TWinControl; override;
    procedure Initialize; override;
    procedure InternalSetEditValue(const Value: TcxEditValue; AValidateEditValue: Boolean); override;
    procedure InternalValidateDisplayValue(const ADisplayValue: TcxEditValue); override;
    function IsEditValueStored: Boolean; override;
    procedure ParentBackgroundChanged; override;
    procedure PropertiesChanged(Sender: TObject); override;
    procedure SynchronizeButtonsStyle; override;
    procedure SynchronizeDisplayValue; override;
    procedure SynchronizeModifiedAfterEnter; virtual;
    procedure InternalUpdateButtons; override;
    function WantNavigationKeys: Boolean; override;
    function GetFirstEnabledItemIndex(AStartIndex: Integer): Integer;

    property Buttons[Index: Integer]: TcxCheckGroupButton read GetButton;
    property StatesItems: Boolean read FStatesItems write FStatesItems stored False;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    procedure GetTabOrderList(List: TList); override;
    procedure SetFocus; override;
    property ActiveProperties: TcxCustomCheckGroupProperties read GetActiveProperties;
    property Properties: TcxCustomCheckGroupProperties read GetProperties write SetProperties;
    property States[Index: Integer]: TcxCheckBoxState read GetState write SetState; default;
    property Transparent;
  end;

  { TcxCheckGroup }

  TcxCheckGroup = class(TcxCustomCheckGroup)
  private
    function GetActiveProperties: TcxCheckGroupProperties;
    function GetProperties: TcxCheckGroupProperties;
    procedure SetProperties(Value: TcxCheckGroupProperties);
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxCheckGroupProperties read GetActiveProperties;
  published
    property Alignment;
    property Anchors;
    property BiDiMode;
    property Caption;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EditValue;
    property Enabled;
    property ParentBackground;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Properties: TcxCheckGroupProperties read GetProperties write SetProperties;
    property ShowHint;
    property StatesItems;
    property Style;
    property StyleDisabled;
    property TabOrder;
    property TabStop;
    property Transparent;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnFocusChanged;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  { TcxFilterCheckGroupHelper }

  TcxFilterCheckGroupHelper = class(TcxFilterChecksHelper)
  protected
    class function GetEditValueFormat(
      AEditProperties: TcxCustomEditProperties): TcxCheckStatesValueFormat; override;
    class function GetItems(
      AEditProperties: TcxCustomEditProperties): IcxCheckItems; override;
    class procedure InitializeItems(AProperties,
      AEditProperties: TcxCustomEditProperties); override;
  end;

implementation

uses
  Variants, Types, SysUtils, StrUtils, cxClasses, cxControls, cxVariants,
  cxExtEditConsts, cxEditUtils, dxThemeManager, cxEditPaintUtils;


{ TcxCheckGroupItems }

function TcxCheckGroupItems.Add: TcxCheckGroupItem;
begin
  Result := TcxCheckGroupItem(inherited Add);
end;

function TcxCheckGroupItems.GetItem(Index: Integer): TcxCheckGroupItem;
begin
  Result := TcxCheckGroupItem(inherited Items[Index]);
end;

procedure TcxCheckGroupItems.SetItem(Index: Integer; Value: TcxCheckGroupItem);
begin
  inherited Items[Index] := Value;
end;

{ TcxCheckGroupButtonViewInfo }

function TcxCheckGroupButtonViewInfo.GetGlyphRect(
  const AGlyphSize: TSize; AAlignment: TLeftRight; AIsPaintCopy: Boolean): TRect;

  procedure CorrectCheckRect(var ACheckRect: TRect);
  begin
    if AIsPaintCopy then
      if AAlignment = taRightJustify then
        OffsetRect(ACheckRect, 2, 0)
      else
        OffsetRect(ACheckRect, -2, 0);
  end;

begin
  case AAlignment of
    taLeftJustify:
      AAlignment := taRightJustify;
    taRightJustify:
      AAlignment := taLeftJustify;
  end;
  Result := inherited GetGlyphRect(AGlyphSize, AAlignment, AIsPaintCopy);
  CorrectCheckRect(Result);
end;

{ TcxCheckGroupViewInfo }

procedure TcxCheckGroupViewInfo.DrawButtonCaption(ACanvas: TcxCanvas;
  AButtonViewInfo: TcxGroupBoxButtonViewInfo; const AGlyphRect: TRect);

  function GetCaptionRect(const ACheckRect: TRect): TRect;
  var
    ACaptionExtent: TRect;
  begin
    Result := AButtonViewInfo.Bounds;
    if UseRightToLeftAlignment then
      ACaptionExtent := TdxRightToLeftLayoutConverter.ConvertOffsets(CaptionExtent)
    else
      ACaptionExtent := CaptionExtent;
    if Alignment = taLeftJustify then
    begin
      Result.Left := ACheckRect.Right + ACaptionExtent.Left;
      Dec(Result.Right, ACaptionExtent.Right);
    end
    else
    begin
      Result.Right := ACheckRect.Left - ACaptionExtent.Right;
      Inc(Result.Left, ACaptionExtent.Left);
    end;
  end;

begin
  DrawCheckBoxText(ACanvas, AButtonViewInfo.Caption, Font, AButtonViewInfo.TextColor,
    GetCaptionRect(AGlyphRect), DrawTextFlags, Enabled);
end;

procedure TcxCheckGroupViewInfo.DrawButtonGlyph(ACanvas: TcxCanvas;
  AButtonViewInfo: TcxGroupBoxButtonViewInfo; const AGlyphRect: TRect);
const
  ACheckStateByButtonState: array[TcxEditButtonState] of TcxEditCheckState =
    (ecsDisabled, ecsNormal, ecsPressed, ecsHot);

  function GetSkinPainter: TcxCustomLookAndFeelPainter;
  begin
    if UseSkins then
      Result := Painter
    else
      Result := nil;
  end;

  function CanDrawBackground: Boolean;
  begin
    Result := not IsBackgroundTransparent and (IsInplace or (GetSkinPainter = nil));
  end;

var
  ACheckBorderStyle: TcxEditBorderStyle;
  AEditProperties: TcxCustomCheckGroupProperties;
begin
  if (CheckBorderStyle = ebsFlat) and (AButtonViewInfo.Data.State = ebsSelected) then
    ACheckBorderStyle := ebs3D
  else
    ACheckBorderStyle := CheckBorderStyle;
  AEditProperties := TcxCustomCheckGroupProperties(EditProperties);

  DrawScaledEditCheck(ACanvas, AGlyphRect, TcxCheckGroupButtonViewInfo(AButtonViewInfo).State,
    ACheckStateByButtonState[AButtonViewInfo.Data.State], AEditProperties.Glyph,
    AEditProperties.GlyphCount, ACheckBorderStyle, AButtonViewInfo.Data.NativeStyle,
    clBtnText, BackgroundColor, CanDrawBackground, IsDesigning, False, True,
    GetSkinPainter, ScaleFactor);
end;

function TcxCheckGroupViewInfo.GetButtonViewInfoClass: TcxEditButtonViewInfoClass;
begin
  Result := TcxCheckGroupButtonViewInfo;
end;

function TcxCheckGroupViewInfo.IsButtonGlyphTransparent(AButtonViewInfo: TcxGroupBoxButtonViewInfo): Boolean;
begin
  Result := IsBackgroundTransparent;
end;

procedure TcxCheckGroupViewInfo.SetOnDrawBackground(AValue: TcxEditDrawBackgroundEvent);
var
  I: Integer;
begin
  inherited SetOnDrawBackground(AValue);
  if Edit <> nil then
    for I := 0 to Edit.InternalButtons.Count - 1 do
      TcxCheckGroupButton(Edit.InternalButtons[I]).ViewInfo.OnDrawBackground := AValue;
end;

function TcxCheckGroupViewInfo.GetEdit: TcxCustomCheckGroup;
begin
  Result := TcxCustomCheckGroup(FEdit);
end;

{ TcxCheckGroupViewData }

procedure TcxCheckGroupViewData.Calculate(ACanvas: TcxCanvas;
  const ABounds: TRect; const P: TPoint; Button: TcxMouseButton;
  Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo; AIsMouseEvent: Boolean);
const
  ABorderStyle: array[TcxLookAndFeelKind] of TcxEditCheckBoxBorderStyle =
    (ebsFlat, ebs3D, ebsUltraFlat, ebsOffice11);

  function GetSkinPainter: TcxCustomLookAndFeelPainter;
  begin
    if AViewInfo.UseSkins then
      Result := AViewInfo.Painter
    else
      Result := nil;
  end;

var
  ACheckGroupProperties: TcxCustomCheckGroupProperties;
begin
  inherited Calculate(ACanvas, ABounds, P, Button, Shift, AViewInfo,
    AIsMouseEvent);
  ACheckGroupProperties := Properties as TcxCustomCheckGroupProperties;
  TcxCheckGroupViewInfo(AViewInfo).GlyphSize :=
    GetScaledEditCheckSize(ACanvas, IsButtonNativeStyle(Style.LookAndFeel), ACheckGroupProperties.Glyph,
    ACheckGroupProperties.GlyphCount, GetSkinPainter, ScaleFactor);
  TcxCheckGroupViewInfo(AViewInfo).Alignment := GetRealItemAlignment(ACheckGroupProperties.ItemAlignment);
  TcxCheckGroupViewInfo(AViewInfo).CheckBorderStyle :=
    ABorderStyle[Style.LookAndFeel.Kind];
  AViewInfo.BackgroundColor := Style.Color;
end;

procedure TcxCheckGroupViewData.EditValueToDrawValue(
  const AEditValue: TcxEditValue; AViewInfo: TcxCustomEditViewInfo);
var
  ASender: TObject;
  I: Integer;
  ACheckStates: TcxCheckStates;
begin
  with TcxCustomCheckGroupProperties(Properties) do
  begin
    if IsInplace then
      ASender := nil
    else
      ASender := Edit;
    TcxCheckGroupViewInfo(AViewInfo).SetButtonCount(Items.Count);
    if PreviewMode then
    begin
      if Items.Count > 0 then
        TcxCheckGroupButtonViewInfo(AViewInfo.ButtonsInfo[0]).State := cbsChecked
    end
    else
    begin
      CalculateCheckStatesByEditValue(ASender, AEditValue, ACheckStates);
      for I := 0 to Items.Count - 1 do
        TcxCheckGroupButtonViewInfo(AViewInfo.ButtonsInfo[I]).State := ACheckStates[I];
    end;
  end;
  if epoAutoHeight in PaintOptions then
    Include(AViewInfo.PaintOptions, epoAutoHeight);
end;

function TcxCheckGroupViewData.GetCaptionRectExtent: TRect;
begin
  Result.Top := 0;
  Result.Bottom := 0;
  if TcxCustomCheckGroupProperties(Properties).ItemAlignment = taLeftJustify then
  begin
    Result.Left := 3;
    Result.Right := 2;
  end
  else
  begin
    Result.Right := 2;
    Result.Left := 2;
  end;
end;

procedure TcxCheckGroupViewData.GetEditMetrics(AAutoHeight: Boolean;
  ACanvas: TcxCanvas; out AMetrics: TcxEditMetrics);
var
  ACaptionRectExtent: TRect;
begin
  AMetrics.ClientLeftBoundCorrection := ScaleFactor.Apply(6 - (5 +
    Integer(TcxCustomCheckGroupProperties(Properties).ItemAlignment =
    taRightJustify)) * Integer(IsInplace));
  AMetrics.ClientWidthCorrection := ScaleFactor.Apply(5 * Integer(IsInplace) - 6);
  AMetrics.ColumnOffset := ScaleFactor.Apply(6);
  if ACanvas = nil then
    Exit;

  AMetrics.ButtonSize := GetScaledEditCheckSize(ACanvas,
    IsButtonNativeStyle(Style.LookAndFeel),
    TcxCustomCheckGroupProperties(Properties).FGlyph,
    TcxCustomCheckGroupProperties(Properties).GlyphCount,
    Style.LookAndFeel.Painter, ScaleFactor);
  ACaptionRectExtent := GetCaptionRectExtent;
  AMetrics.AutoHeightColumnWidthCorrection := ACaptionRectExtent.Left + ACaptionRectExtent.Right;
  AMetrics.ColumnWidthCorrection := AMetrics.AutoHeightColumnWidthCorrection;
  AMetrics.WidthCorrection := ScaleFactor.Apply(6 - 5 * Integer(IsInplace));
  AMetrics.AutoHeightWidthCorrection := AMetrics.ClientWidthCorrection;
end;

function TcxCheckGroupViewData.GetRealItemAlignment(AAlignment: TLeftRight): TLeftRight;
begin
  Result := AAlignment;
  if UseRightToLeftAlignment then
    Result := TdxRightToLeftLayoutConverter.ConvertAlignment(Result);
end;

{ TcxCustomCheckGroupProperties }

constructor TcxCustomCheckGroupProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FEditValueFormat := cvfIndices;
  FItemAlignment := taLeftJustify;
  FGlyph := TdxSmartGlyph.Create;
  FGlyph.OnChange := ChangeHandler;
  FGlyphCount := 6;
  Items.OnChange := ItemsChanged;
end;

destructor TcxCustomCheckGroupProperties.Destroy;
begin
  FreeAndNil(FGlyph);
  inherited Destroy;
end;

function TcxCustomCheckGroupProperties.CompareDisplayValues(
  const AEditValue1, AEditValue2: TcxEditValue): Boolean;
var
  ACheckStates1, ACheckStates2: TcxCheckStates;
begin
  CalculateCheckStatesByEditValue(nil, AEditValue1, ACheckStates1);
  CalculateCheckStatesByEditValue(nil, AEditValue2, ACheckStates2);
  Result := (Length(ACheckStates1) = Length(ACheckStates2)) and
    CompareMem(@ACheckStates1[0], @ACheckStates2[0], SizeOf(TcxCheckBoxState) * Length(ACheckStates1));
end;

class function TcxCustomCheckGroupProperties.GetContainerClass: TcxContainerClass;
begin
  Result := TcxCheckGroup;
end;

function TcxCustomCheckGroupProperties.GetDisplayText(const AEditValue: TcxEditValue;
  AFullText: Boolean = False; AIsInplace: Boolean = True): string;
var
  ACheckStateNames: array[TcxCheckBoxState] of WideString;

  procedure InitCheckStateNames;
  begin
    ACheckStateNames[cbsUnchecked] :=
      cxGetResourceString(@cxSEditCheckGroupUnchecked);
    ACheckStateNames[cbsChecked] :=
      cxGetResourceString(@cxSEditCheckGroupChecked);
    ACheckStateNames[cbsGrayed] :=
      cxGetResourceString(@cxSEditCheckGroupGrayed);
  end;

var
  I: Integer;
  ACheckStates: TcxCheckStates;
begin
  CalculateCheckStatesByEditValue(nil, AEditValue, ACheckStates);
  InitCheckStateNames;

  Result := '';
  for I := 0 to Items.Count - 1 do
  begin
    Result := Result + Items[I].Caption +
      cxGetResourceString(@cxSEditCheckGroupCaptionStateDelimiter) +
      ACheckStateNames[ACheckStates[I]];
    if I < Items.Count - 1 then
      Result := Result + cxGetResourceString(@cxSEditCheckGroupFilterColumnSeparator);
  end;
end;

function TcxCustomCheckGroupProperties.GetSpecialFeatures: TcxEditSpecialFeatures;
begin
  Result := inherited GetSpecialFeatures + [esfMultiRow];
end;

class function TcxCustomCheckGroupProperties.GetViewInfoClass: TcxContainerViewInfoClass;
begin
  Result := TcxCheckGroupViewInfo;
end;

function TcxCustomCheckGroupProperties.CreateItems: TcxButtonGroupItems;
begin
  Result := TcxCheckGroupItems.Create(Self, TcxCheckGroupItem);
end;

procedure TcxCustomCheckGroupProperties.DoAssign(AProperties: TcxCustomEditProperties);
begin
  inherited;
  if AProperties is TcxCustomCheckGroupProperties then
    with TcxCustomCheckGroupProperties(AProperties) do
    begin
      Self.AllowGrayed := AllowGrayed;
      Self.ItemAlignment := ItemAlignment;
      Self.WordWrap := WordWrap;
      Self.Glyph := Glyph;
      Self.GlyphCount := GlyphCount;
      Self.EditValueFormat := EditValueFormat;
      Self.OnEditValueToStates := OnEditValueToStates;
      Self.OnStatesToEditValue := OnStatesToEditValue;
    end;
end;

class function TcxCustomCheckGroupProperties.GetViewDataClass: TcxCustomEditViewDataClass;
begin
  Result := TcxCheckGroupViewData;
end;

function TcxCustomCheckGroupProperties.HasDisplayValue: Boolean;
begin
  Result := True;
end;

function TcxCustomCheckGroupProperties.GetGlyphCount: Integer;
begin
  Result := FGlyphCount;
end;

function TcxCustomCheckGroupProperties.GetStateCaption(AIndex: Integer): string;
begin
  Result := GetCheckBoxStateCaptionByGlyphIndex(AIndex);
end;

procedure TcxCustomCheckGroupProperties.CalculateCheckStatesByEditValue(
  Sender: TObject; const AEditValue: TcxEditValue; var ACheckStates: TcxCheckStates);
begin
  if Assigned(OnEditValueToStates) then
  begin
    SetLength(ACheckStates, Items.Count);
    OnEditValueToStates(Sender, AEditValue, ACheckStates);
  end
  else
    cxCheckBox.CalculateCheckStates(AEditValue, Items, EditValueFormat, ACheckStates);
end;

procedure TcxCustomCheckGroupProperties.CalculateEditValueByCheckStates(
  Sender: TObject; const ACheckStates: TcxCheckStates; out AEditValue: TcxEditValue);
begin
  if Assigned(OnStatesToEditValue) then
    OnStatesToEditValue(Sender, ACheckStates, AEditValue)
  else
    AEditValue := cxCheckBox.CalculateCheckStatesValue(ACheckStates, Items, EditValueFormat);
end;

function TcxCustomCheckGroupProperties.GetItems: TcxCheckGroupItems;
begin
  Result := TcxCheckGroupItems(inherited Items);
end;

procedure TcxCustomCheckGroupProperties.SetAllowGrayed(Value: Boolean);
begin
  if Value <> FAllowGrayed then
  begin
    FAllowGrayed := Value;
    Changed;
  end;
end;

procedure TcxCustomCheckGroupProperties.SetEditValueFormat(Value: TcxCheckStatesValueFormat);
begin
  if Value <> FEditValueFormat then
  begin
    FEditValueFormat := Value;
    Changed;
    CheckEditValueFormat;
  end;
end;

procedure TcxCustomCheckGroupProperties.SetGlyph(Value: TdxSmartGlyph);
begin
  Glyph.Assign(Value);
end;

procedure TcxCustomCheckGroupProperties.SetGlyphCount(Value: Integer);
begin
  if FGlyphCount <> Value then
  begin
    FGlyphCount := Value;
    if FGlyph <> nil then
      Changed;
  end;
end;

procedure TcxCustomCheckGroupProperties.SetItemAlignment(Value: TLeftRight);
begin
  if Value <> FItemAlignment then
  begin
    FItemAlignment := Value;
    Changed;
  end;
end;

procedure TcxCustomCheckGroupProperties.SetItems(Value: TcxCheckGroupItems);
begin
  inherited Items.Assign(Value);
end;

procedure TcxCustomCheckGroupProperties.CheckEditValueFormat;
begin
  if (GetRealEditValueFormat = cvfInteger) and (Items.Count > 64) then
    raise EdxException.Create(cxGetResourceString(@cxSCheckControlIncorrectItemCount));
end;

function TcxCustomCheckGroupProperties.GetRealEditValueFormat: TcxCheckStatesValueFormatEx;
begin
  if Assigned(OnEditValueToStates) and Assigned(OnStatesToEditValue) then
    Result := cvfCustom
  else
    Result := EditValueFormat;
end;

procedure TcxCustomCheckGroupProperties.ItemsChanged(Sender: TObject; AItem: TCollectionItem);
begin
  CheckEditValueFormat;
end;

{ TcxCheckGroupCheckButtonViewInfo }

procedure TcxCheckGroupCheckButtonViewInfo.InternalPaint(ACanvas: TcxCanvas);
var
  ARect: TRect;
  ABitmap: TBitmap;
  ABitmapCanvas: TcxCanvas;
begin
  if UseSkins then
  begin
    ARect := ClientRect;
    ABitmap := cxCreateBitmap(ARect, pf32bit);
    ABitmapCanvas := TcxCanvas.Create(ABitmap.Canvas);
    try
      Painter.DrawGroupBoxBackground(ABitmapCanvas, ARect,
        Rect(0, 0, ABitmap.Width, ABitmap.Height));
      inherited InternalPaint(ABitmapCanvas);
      ACanvas.Draw(0, 0, ABitmap);
    finally
      ABitmapCanvas.Free;
      ABitmap.Free;
    end;
  end
  else
    inherited InternalPaint(ACanvas);
end;

{ TcxCheckGroupButtonProperties }

function TcxCheckGroupButtonProperties.InternalGetGlyph: TdxSmartGlyph;
begin
  Result :=
   (TcxCheckGroupButton(Owner).Owner as TcxCustomCheckGroup).ActiveProperties.Glyph;
end;

function TcxCheckGroupButtonProperties.IsEmbeddedEdit: Boolean;
begin
  Result := True;
end;

class function TcxCheckGroupButtonProperties.GetViewInfoClass: TcxContainerViewInfoClass;
begin
  Result := TcxCheckGroupCheckButtonViewInfo;
end;

{ TcxCheckGroupButton }

destructor TcxCheckGroupButton.Destroy;
begin
  TcxCustomCheckGroup(GetOwner).InternalButtons.Remove(Self);
  inherited Destroy;
end;

function TcxCheckGroupButton.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or
    CheckGroup.DataBinding.ExecuteAction(Action);
end;

class function TcxCheckGroupButton.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCheckGroupButtonProperties;
end;

function TcxCheckGroupButton.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or
    CheckGroup.DataBinding.UpdateAction(Action);
end;

function TcxCheckGroupButton.CanAutoSize: Boolean;
begin
  Result := False;
end;

function TcxCheckGroupButton.CanFocusOnClick: Boolean;
begin
  Result := inherited CanFocusOnClick and CheckGroup.NeedFocusOnClick;
end;

procedure TcxCheckGroupButton.DoExit;
begin
  inherited DoExit;
  CheckGroup.FFocusedItemIndex := CheckGroup.InternalButtons.IndexOf(Self);
end;

procedure TcxCheckGroupButton.Initialize;
begin
  inherited Initialize;
  CheckGroup.InternalButtons.Add(Self);
  Style.LookAndFeel.MasterLookAndFeel := CheckGroup.LookAndFeel;
  Keys := Keys + [kArrows];
  CheckTransparentBorder;
end;

function TcxCheckGroupButton.IsNativeBackground: Boolean;
begin
  Result := CheckGroup.IsNativeBackground;
end;

function TcxCheckGroupButton.IsNativeStyle: Boolean;
begin
  Result := CheckGroup.IsButtonNativeStyle;
end;

function TcxCheckGroupButton.IsTransparent: Boolean;
begin
  Result := (Style.LookAndFeel.SkinPainter <> nil) or CheckGroup.IsTransparent;
end;

procedure TcxCheckGroupButton.KeyDown(var Key: Word; Shift: TShiftState);

  procedure FocusNearestItem(ADown: Boolean);
  var
    AIndex, I, ADelta: Integer;
  begin
    with CheckGroup do
    begin
      AIndex := InternalButtons.IndexOf(Self);
      ADelta := Integer(ADown) - Integer(not ADown);
      I := AIndex;
      repeat
        I := I + ADelta;
        if I = InternalButtons.Count then
          I := 0
        else
          if I < 0 then
            I := InternalButtons.Count - 1;
        if Buttons[I].Enabled then
          Break;
      until I = AIndex;
      if I <> AIndex then
        Buttons[I].SetFocus;
    end;
  end;
var
  APrevKeyDownEvent: TKeyEvent;
begin
  _TcxContainerAccess.KeyDown(CheckGroup, Key, Shift);
  if Key = 0 then
    Exit;
  if not CheckGroup.IsInplace then
    if Key in [VK_UP, VK_LEFT] then
      FocusNearestItem(False)
    else
      if Key in [VK_DOWN, VK_RIGHT] then
        FocusNearestItem(True);
  APrevKeyDownEvent := OnKeyDown;
  OnKeyDown := nil;
  inherited KeyDown(Key, Shift);
  OnKeyDown := APrevKeyDownEvent;
end;

procedure TcxCheckGroupButton.LookAndFeelChanged(Sender: TcxLookAndFeel;
  AChangedValues: TcxLookAndFeelValues);
begin
  inherited LookAndFeelChanged(Sender, AChangedValues);
  CheckTransparentBorder;
end;

procedure TcxCheckGroupButton.WndProc(var Message: TMessage);
begin
  with Message do
    if ((Msg = WM_LBUTTONDOWN) or (Msg = WM_LBUTTONDBLCLK)) and
      (CheckGroup.DragMode = dmAutomatic) and not(csDesigning in CheckGroup.ComponentState) then
    begin
      _TcxContainerAccess.BeginAutoDrag(CheckGroup);
      Exit;
    end;
  inherited WndProc(Message);
end;

function TcxCheckGroupButton.GetControl: TWinControl;
begin
  Result := Self;
end;

function TcxCheckGroupButton.GetControlContainer: TcxContainer;
begin
  Result := CheckGroup;
end;

procedure TcxCheckGroupButton.CheckTransparentBorder;
begin
  if not CheckGroup.IsInplace then
    Style.TransparentBorder := IsNativeStyle;
end;

function TcxCheckGroupButton.GetCheckGroup: TcxCustomCheckGroup;
begin
  Result := TcxCustomCheckGroup(Owner);
end;

{ TcxCustomCheckGroup }

class function TcxCustomCheckGroup.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCustomCheckGroupProperties;
end;

procedure TcxCustomCheckGroup.GetTabOrderList(List: TList);
var
  AFocusedItemIndex: Integer;
begin
  inherited GetTabOrderList(List);
  List.Remove(Self);
  if Focused then
    AFocusedItemIndex := GetFocusedButtonIndex
  else
    AFocusedItemIndex := GetFirstEnabledItemIndex(FFocusedItemIndex);
  if (AFocusedItemIndex <> -1) and TabStop then
    List.Add(InternalButtons[AFocusedItemIndex]);
end;

procedure TcxCustomCheckGroup.SetFocus;
begin
  inherited SetFocus;
  FFocusedItemIndex := GetFirstEnabledItemIndex(FFocusedItemIndex);
  if FFocusedItemIndex <> -1 then
    TWinControl(InternalButtons[FFocusedItemIndex]).SetFocus;
end;

procedure TcxCustomCheckGroup.ArrangeButtons;
var
  AButtonViewInfo: TcxGroupBoxButtonViewInfo;
  I: Integer;
begin
  inherited ArrangeButtons;
  for I := 0 to InternalButtons.Count - 1 do
    with Buttons[I] do
    begin
      AButtonViewInfo := TcxGroupBoxButtonViewInfo(Self.ViewInfo.ButtonsInfo[I]);
      FColumn := AButtonViewInfo.Column;
      FRow := AButtonViewInfo.Row;
    end;
end;

procedure TcxCustomCheckGroup.DoSetFocusWhenActivate;
begin
  FFocusedItemIndex := 0;
  inherited;
end;

function TcxCustomCheckGroup.GetButtonDC(AButtonIndex: Integer): THandle;
begin
  Result := Buttons[AButtonIndex].Canvas.Handle;
end;

function TcxCustomCheckGroup.GetButtonInstance: TWinControl;
begin
  Result := TcxCheckGroupButton.Create(Self, IsInplace);
end;

procedure TcxCustomCheckGroup.Initialize;
begin
  inherited Initialize;
  ControlStyle := ControlStyle - [csAcceptsControls];
  FFocusedItemIndex := -1;
  InternalEditValue := '';
end;

procedure TcxCustomCheckGroup.InternalSetEditValue(const Value: TcxEditValue; AValidateEditValue: Boolean);
begin
  inherited InternalSetEditValue(Value, AValidateEditValue);
  SynchronizeModifiedAfterEnter;
end;

procedure TcxCustomCheckGroup.InternalValidateDisplayValue(const ADisplayValue: TcxEditValue);
var
  APrevEditValue: TcxEditValue;
  I: Integer;
  ACheckStates: TcxCheckStates;
begin
  APrevEditValue := FEditValue;
  SetLength(ACheckStates, InternalButtons.Count);
  for I := 0 to Length(ACheckStates) - 1 do
    ACheckStates[I] := Buttons[I].State;
  ActiveProperties.CalculateEditValueByCheckStates(GetStandaloneEventSender(Self), ACheckStates, FEditValue);
  if not InternalVarEqualsExact(APrevEditValue, FEditValue) then
    DoEditValueChanged;
end;

function TcxCustomCheckGroup.IsEditValueStored: Boolean;
begin
  case ActiveProperties.EditValueFormat of
    cvfCaptions, cvfIndices:
      Result := EditValue <> '';
    cvfInteger:
      Result := EditValue <> 0;
    cvfStatesString:
      Result := (Pos('1', EditValue) > 0) or (Pos('2', EditValue) > 0);
  else
    Result := False;
  end;
end;

procedure TcxCustomCheckGroup.ParentBackgroundChanged;
var
  I: Integer;
begin
  for I := 0 to InternalButtons.Count - 1 do
    Buttons[I].ParentBackground := ParentBackground;
end;

procedure TcxCustomCheckGroup.PropertiesChanged(Sender: TObject);
begin
  inherited PropertiesChanged(Sender);
  InternalValidateDisplayValue(Null);
  DataBinding.UpdateDisplayValue;
end;

procedure TcxCustomCheckGroup.SynchronizeButtonsStyle;
var
  AButton: TcxCheckGroupButton;
  I: Integer;
begin
  inherited SynchronizeButtonsStyle;
  for I := 0 to InternalButtons.Count - 1 do
  begin
    AButton := Buttons[I];
    AButton.Style.Assign(Style);
    AButton.StyleDisabled.Assign(StyleDisabled);
    AButton.StyleFocused.Assign(StyleFocused);
    AButton.StyleHot.Assign(StyleHot);
    AButton.CheckTransparentBorder;
    AButton.Transparent := Transparent; // to repaint button
    AButton.ParentFont := True;
  end;
end;

procedure TcxCustomCheckGroup.SynchronizeDisplayValue;
var
  I: Integer;
  ACheckStates: TcxCheckStates;
begin
  if ActiveProperties.Items.ItemChanged then
    Exit;
  with ActiveProperties do
    CalculateCheckStatesByEditValue(GetStandaloneEventSender(Self), EditValue, ACheckStates);
  if not ActiveProperties.AllowGrayed and Focused then
    for I := 0 to Length(ACheckStates) - 1 do
      if ACheckStates[I] = cbsGrayed then
        ACheckStates[I] := cbsUnchecked;

  FButtonStatesChanging := True;
  try
    for I := 0 to InternalButtons.Count - 1 do
      Buttons[I].State := ACheckStates[I];
  finally
    FButtonStatesChanging := False;
  end;
end;

procedure TcxCustomCheckGroup.SynchronizeModifiedAfterEnter;

    function NeedSynchronize: Boolean;
    begin
      Result := not ModifiedAfterEnter or ActiveProperties.ImmediatePost;
    end;

var
  I: Integer;
begin
  if NeedSynchronize then
    for I := 0 to InternalButtons.Count - 1 do
      Buttons[I].ModifiedAfterEnter := False;
end;

procedure TcxCustomCheckGroup.InternalUpdateButtons;
const
  ALookAndFeelKindMap: array [TcxEditButtonStyle] of TcxLookAndFeelKind =
    (lfStandard, lfStandard, lfFlat, lfStandard, lfStandard,
    lfUltraFlat, lfOffice11);
var
 AButton: TcxCheckGroupButton;
 I: Integer;
begin
  inherited;
  if FFocusedItemIndex >= InternalButtons.Count then
    FFocusedItemIndex := -1;
  for I := 0 to InternalButtons.Count - 1 do
  begin
    AButton := Buttons[I];
    AButton.Properties.BeginUpdate;
    try
      AButton.Properties.Alignment := ActiveProperties.ItemAlignment;
      AButton.Properties.AllowGrayed := ActiveProperties.AllowGrayed;
      AButton.Caption := ActiveProperties.Items[I].Caption;
      AButton.Properties.ShowEndEllipsis := ActiveProperties.ShowEndEllipsis;
      AButton.Properties.MultiLine := ActiveProperties.WordWrap;
      AButton.Properties.GlyphCount := ActiveProperties.GlyphCount;
      AButton.Properties.ReadOnly := ActiveProperties.ReadOnly and
        DataBinding.IsDataAvailable;
      if Length(ViewInfo.ButtonsInfo) > 0 then
        AButton.LookAndFeel.Kind := ALookAndFeelKindMap[ViewInfo.ButtonsInfo[0].Data.Style];
    finally
      AButton.Properties.EndUpdate;
      AButton.OnEditing := DoButtonEditing;
      AButton.OnFocusChanged := DoButtonFocusChanged;
      AButton.Properties.OnChange := DoButtonChange;
    end;
  end;
//  SynchronizeDisplayValue;
end;

function TcxCustomCheckGroup.WantNavigationKeys: Boolean;
begin
  Result := not IsInplace;
end;

function TcxCustomCheckGroup.GetFirstEnabledItemIndex(AStartIndex: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  if AStartIndex = -1 then
    AStartIndex := 0;
  for I := 0 to InternalButtons.Count - 1 do
    if TWinControl(InternalButtons[(I + AStartIndex) mod InternalButtons.Count]).CanFocus then
    begin
      Result := (I + AStartIndex) mod InternalButtons.Count;
      Break;
    end;
end;

procedure TcxCustomCheckGroup.DoButtonChange(Sender: TObject);
begin
  if FButtonStatesChanging then
    Exit;
  LockChangeEvents(True);
  try
    InternalValidateDisplayValue(Null);
    ChangeHandler(Self);
    if ActiveProperties.ImmediatePost and CanPostEditValue and InternalValidateEdit then
    begin
      InternalPostEditValue;
      SynchronizeModifiedAfterEnter;
    end;
  finally
    LockChangeEvents(False);
  end;
end;

procedure TcxCustomCheckGroup.DoButtonEditing(Sender: TObject; var CanEdit: Boolean);
begin
  CanEdit := DoEditing;
end;

procedure TcxCustomCheckGroup.DoButtonFocusChanged(Sender: TObject);
begin
  FocusChanged;
end;

function TcxCustomCheckGroup.GetActiveProperties: TcxCustomCheckGroupProperties;
begin
  Result := TcxCustomCheckGroupProperties(InternalGetActiveProperties);
end;

function TcxCustomCheckGroup.GetButton(Index: Integer): TcxCheckGroupButton;
begin
  Result := TcxCheckGroupButton(InternalButtons[Index]);
end;

function TcxCustomCheckGroup.GetProperties: TcxCustomCheckGroupProperties;
begin
  Result := TcxCustomCheckGroupProperties(inherited Properties);
end;

function TcxCustomCheckGroup.GetState(Index: Integer): TcxCheckBoxState;
begin
  Result := Buttons[Index].State;
end;

procedure TcxCustomCheckGroup.SetProperties(Value: TcxCustomCheckGroupProperties);
begin
  Properties.Assign(Value);
end;

procedure TcxCustomCheckGroup.SetState(Index: Integer; Value: TcxCheckBoxState);
begin
  Buttons[Index].State := Value;
end;

{ TcxCheckGroup }

class function TcxCheckGroup.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCheckGroupProperties;
end;

function TcxCheckGroup.GetActiveProperties: TcxCheckGroupProperties;
begin
  Result := TcxCheckGroupProperties(InternalGetActiveProperties);
end;

function TcxCheckGroup.GetProperties: TcxCheckGroupProperties;
begin
  Result := TcxCheckGroupProperties(inherited Properties);
end;

procedure TcxCheckGroup.SetProperties(Value: TcxCheckGroupProperties);
begin
  Properties.Assign(Value);
end;

{ TcxFilterCheckGroupHelper }

class function TcxFilterCheckGroupHelper.GetEditValueFormat(
  AEditProperties: TcxCustomEditProperties): TcxCheckStatesValueFormat;
begin
  Result := TcxCustomCheckGroupProperties(AEditProperties).EditValueFormat;
end;

class function TcxFilterCheckGroupHelper.GetItems(
  AEditProperties: TcxCustomEditProperties): IcxCheckItems;
begin
  Result := TcxCustomCheckGroupProperties(AEditProperties).Items;
end;

class procedure TcxFilterCheckGroupHelper.InitializeItems(AProperties,
  AEditProperties: TcxCustomEditProperties);
begin
  inherited InitializeItems(AProperties, AEditProperties);
  TcxCustomCheckComboBoxProperties(AProperties).Glyph :=
    TcxCustomCheckGroupProperties(AEditProperties).Glyph;
  TcxCustomCheckComboBoxProperties(AProperties).GlyphCount :=
    TcxCustomCheckGroupProperties(AEditProperties).GlyphCount;
end;

initialization
  GetRegisteredEditProperties.Register(TcxCheckGroupProperties,
    scxSEditRepositoryCheckGroupItem);
  FilterEditsController.Register(TcxCheckGroupProperties,
    TcxFilterCheckGroupHelper);

finalization
  FilterEditsController.Unregister(TcxCheckGroupProperties,
    TcxFilterCheckGroupHelper);
  GetRegisteredEditProperties.Unregister(TcxCheckGroupProperties);

end.
