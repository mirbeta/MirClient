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

unit cxHyperLinkEdit;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Messages, Windows, Types, SysUtils, Graphics, Classes, Controls, Menus, Math,
  dxCore, cxGraphics, cxContainer,
  cxControls, cxEdit, cxTextEdit, cxEditConsts, cxFilterControlUtils;

const
  cxHyperLinkEditDefaultLinkColor = clBlue;

type
  { TcxHyperLinkEditViewInfo }

  TcxCustomHyperLinkEdit = class;
  TcxCustomHyperLinkEditProperties = class;

  TcxHyperLinkEditViewInfo = class(TcxCustomTextEditViewInfo)
  private
    FWasFocusedBeforeClick: Boolean;
    FSingleClick: Boolean;
    FIsStandardAlignment: Boolean;

    procedure CorrectTextBounds(var ATextBounds: TRect; const AHorzAlignment: TAlignment; const ARect: TRect);
  protected
    function CanOpenHyperLink(const P: TPoint; AIsFirstClick: Boolean): Boolean;
    function CanHandPointCursor(const P: TPoint): Boolean;
    function GetActuallyTextBounds: TRect;
    procedure GetColorSettingsByPainter(out ABackground, ATextColor: TColor); override;
    function GetCurrentCursor(const AMousePos: TPoint): TCursor; override;
    function GetHyperLinkTextColor: TColor; virtual;
    procedure InplaceMouseDown(AButton: TMouseButton; const AShift: TShiftState; X, Y: Integer); override;
    function IsMouseHoveredOverText(const AMousePos: TPoint): Boolean;
  public
    function IsHotTrack: Boolean; override;
    function IsHotTrack(P: TPoint): Boolean; override;
    procedure PrepareCanvasFont(ACanvas: TCanvas); override;
  end;

  { TcxImageViewData }

  TcxHyperLinkEditViewData = class(TcxCustomTextEditViewData)
  public
    procedure Calculate(ACanvas: TcxCanvas; const ABounds: TRect; const P: TPoint;
      Button: TcxMouseButton; Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo;
      AIsMouseEvent: Boolean); override;
  end;

  { TcxCustomHyperLinkEditProperties }

  TcxHyperLinkEditUsePrefix = (upAlways, upOnlyOnExecute, upNever);

  TcxCustomHyperLinkEditProperties = class(TcxCustomTextEditProperties)
  private
    FSingleClick: Boolean;
    FLinkColor: TColor;
    FOnStartClick: TNotifyEvent;
    FPrefix: string;
    FStartKey: TShortCut;
    FUsePrefix: TcxHyperLinkEditUsePrefix;
    function GetAutoComplete: Boolean;
    function GetPrefixStored: Boolean;
    procedure ReadPrefix(Reader: TReader);
    procedure SetAutoComplete(Value: Boolean);
    procedure SetLinkColor(const Value: TColor);
    procedure SetSingleClick(Value: Boolean);
    procedure WritePrefix(Writer: TWriter);
  protected
    procedure DefineProperties(AFiler: TFiler); override;
    procedure DoAssign(AProperties: TcxCustomEditProperties); override;
    function AddPrefixTo(const AStr: string): string; virtual;
  public
    constructor Create(AOwner: TPersistent); override;
    class function GetContainerClass: TcxContainerClass; override;
    class function GetStyleClass: TcxCustomEditStyleClass; override;
    function GetSupportedOperations: TcxEditSupportedOperations; override;
    class function GetViewDataClass: TcxCustomEditViewDataClass; override;
    class function GetViewInfoClass: TcxContainerViewInfoClass; override;
    procedure ValidateDisplayValue(var ADisplayValue: TcxEditValue;
      var AErrorText: TCaption; var Error: Boolean; AEdit: TcxCustomEdit); override;
    // !!!
    property AutoComplete: Boolean read GetAutoComplete write SetAutoComplete
      stored False; // deprecated
    property AutoSelect default False;
    property LinkColor: TColor read FLinkColor write SetLinkColor
      default cxHyperLinkEditDefaultLinkColor;
    property Prefix: string read FPrefix write FPrefix stored False;
    property SingleClick: Boolean read FSingleClick write SetSingleClick
      default False;
    property StartKey: TShortCut read FStartKey write FStartKey
      default VK_RETURN + scCtrl;
    property UsePrefix: TcxHyperLinkEditUsePrefix read FUsePrefix
      write FUsePrefix default upAlways;
    property OnStartClick: TNotifyEvent read FOnStartClick write FOnStartClick;
  end;

  { TcxHyperLinkEditProperties }

  TcxHyperLinkEditProperties = class(TcxCustomHyperLinkEditProperties)
  published
    property Alignment;
    property AssignedValues;
    property AutoComplete; // deprecated hidden
    property AutoSelect;
    property ClearKey;
    property ImeMode;
    property ImeName;
    property IncrementalSearch;
    property LinkColor;
    property LookupItems;
    property LookupItemsSorted;
    property Nullstring;
    property Prefix;
    property ReadOnly;
    property StartKey;
    property SingleClick;
    property UseLeftAlignmentOnEditing;
    property UseNullString;
    property UsePrefix;
    property ValidateOnEnter;
    property ValidationErrorIconAlignment;
    property ValidationOptions;
    property OnChange;
    property OnEditValueChanged;
    property OnStartClick;
    property OnValidate;
  end;

  { TcxHyperLinkStyle }

  TcxHyperLinkStyle = class(TcxEditStyle)
  protected
    function GetTextColor: TColor; override;
    function GetTextStyle: TFontStyles; override;
  end;

  { TcxCustomHyperLinkEdit }

  TcxCustomHyperLinkEdit = class(TcxCustomTextEdit)
  private
    function GetActiveProperties: TcxCustomHyperLinkEditProperties;
    function GetProperties: TcxCustomHyperLinkEditProperties;
    function GetStyle: TcxHyperLinkStyle;
    procedure SetProperties(const Value: TcxCustomHyperLinkEditProperties);
    procedure SetStyle(Value: TcxHyperLinkStyle);
  protected
    class procedure OpenHyperLink(const AHyperLink: string; AActiveProperties: TcxCustomHyperLinkEditProperties);
    function DoOnStartClick: Boolean;
    procedure DoStart; virtual;
    function GetCurrentCursor(X, Y: Integer): TCursor; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure PropertiesChanged(Sender: TObject); override;
  public
    procedure ActivateByMouse(Shift: TShiftState; X, Y: Integer; var AEditData: TcxCustomEditData); override;
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    function InnerControlMenuHandler(var Message: TMessage): Boolean; override;
    procedure SelectAll; override;

    property ActiveProperties: TcxCustomHyperLinkEditProperties read GetActiveProperties;
    property Properties: TcxCustomHyperLinkEditProperties read GetProperties
      write SetProperties;
    property Style: TcxHyperLinkStyle read GetStyle write SetStyle;
  end;

  { TcxHyperLinkEdit }

  TcxHyperLinkEdit = class(TcxCustomHyperLinkEdit)
  private
    function GetActiveProperties: TcxHyperLinkEditProperties;
    function GetProperties: TcxHyperLinkEditProperties;
    procedure SetProperties(Value: TcxHyperLinkEditProperties);
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxHyperLinkEditProperties read GetActiveProperties;
  published
    property Anchors;
    property AutoSize;
    property BeepOnEnter;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Properties: TcxHyperLinkEditProperties read GetProperties write SetProperties;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property Text;
    property TextHint;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnEditing;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property BiDiMode;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
  end;

  { TcxFilterHyperLinkEditHelper }

  TcxFilterHyperLinkEditHelper = class(TcxFilterTextEditHelper)
  public
    class function GetFilterEditClass: TcxCustomEditClass; override;
  end;

implementation

uses
  ShellAPI, Variants, Forms, cxGeometry, cxVariants, cxClasses, cxLibraryConsts, cxLookAndFeelPainters, cxDrawTextUtils;

type
  TcxCustomTextEditAccess = class(TcxCustomTextEdit);

{ TcxHyperLinkEditViewInfo }

function TcxHyperLinkEditViewInfo.CanOpenHyperLink(const P: TPoint; AIsFirstClick: Boolean): Boolean;
begin
  if FSingleClick and AIsFirstClick or not FSingleClick and not AIsFirstClick then
    if FIsStandardAlignment or FWasFocusedBeforeClick then
      Result := IsMouseHoveredOverText(P)
    else
      Result := PtInRect(ClientRect, P)
  else
    Result := False;
end;

function TcxHyperLinkEditViewInfo.CanHandPointCursor(const P: TPoint): Boolean;
begin
  if FIsStandardAlignment or Focused then
    Result := IsMouseHoveredOverText(P)
  else
    Result := PtInRect(ClientRect, P);
end;

procedure TcxHyperLinkEditViewInfo.CorrectTextBounds(var ATextBounds: TRect; const AHorzAlignment: TAlignment; const ARect: TRect);
begin
  case AHorzAlignment of
    taRightJustify:
      ATextBounds := cxRectOffsetHorz(ATextBounds, ARect.Right - ATextBounds.Right);
    taCenter:
      ATextBounds := cxRectCenter(ARect, cxRectSize(ATextBounds));
  end;
end;

function TcxHyperLinkEditViewInfo.GetActuallyTextBounds: TRect;
var
  I: Integer;
  ATextWidth: Integer;
  ARows: TcxTextRows;
  ARow: TcxTextRow;
  AHorzAlignment: TAlignment;
begin
  if Edit <> nil then
  begin
    Result := cxRect(Bounds.Left, InnerEditRect.Top, Bounds.Right, InnerEditRect.Bottom);
    if FEdit is TcxCustomTextEdit then
    begin
      AHorzAlignment := FEdit.ActiveProperties.Alignment.Horz;
      ATextWidth := cxTextWidth(TcxCustomTextEditAccess(Edit).Font, TcxCustomTextEditAccess(Edit).InnerTextEdit.EditValue);
      Result.Right := Min(Result.Right, InnerEditRect.Left + ATextWidth);
      if not(Focused and FEdit.ActiveProperties.UseLeftAlignmentOnEditing) then
        CorrectTextBounds(Result, AHorzAlignment, Bounds);
    end;
  end
  else
  begin
    Result := cxNullRect;
    ARows := TextOutData.TextRows;
    if ARows.Count > 0 then
    begin
      ARow := cxGetTextRow(ARows, 0)^;
      Result.Left := ARow.TextOriginX;
      Result.Top := ARow.TextOriginY;
      Result.Right := Result.Left + ARow.TextExtents.cx;
      Result.Bottom := Result.Top + ARow.TextExtents.cy;
      for I := 1 to ARows.Count - 1 do
      begin
        ARow := cxGetTextRow(ARows, I)^;
        Result.Left := Min(Result.Left, ARow.TextOriginX);
        Result.Right := Max(Result.Right, ARow.TextOriginX + ARow.TextExtents.cx);
        Result.Bottom := ARow.TextOriginY + ARow.TextExtents.cy;
      end;
    end;
  end;
  if UseRightToLeftAlignment then
    Result := TdxRightToLeftLayoutConverter.ConvertRect(Result, Bounds);
end;

procedure TcxHyperLinkEditViewInfo.GetColorSettingsByPainter(
  out ABackground, ATextColor: TColor);
begin
  inherited GetColorSettingsByPainter(ABackground, ATextColor);
  ATextColor := GetHyperLinkTextColor;
end;

function TcxHyperLinkEditViewInfo.GetCurrentCursor(const AMousePos: TPoint): TCursor;
begin
  Result := crDefault;
  if CanHandPointCursor(AMousePos) then
    Result := crcxHandPoint;
end;

function TcxHyperLinkEditViewInfo.GetHyperLinkTextColor: TColor;
begin
  Result := TcxCustomHyperLinkEditProperties(EditProperties).LinkColor;
  if UseSkins and (Result = cxHyperLinkEditDefaultLinkColor) then
    Result := Painter.DefaultHyperlinkTextColor
end;

procedure TcxHyperLinkEditViewInfo.InplaceMouseDown(AButton: TMouseButton; const AShift: TShiftState; X, Y: Integer);
begin
  if (AButton = mbLeft) and CanOpenHyperLink(Point(X, Y), not (ssDouble in AShift)) then
    TcxHyperLinkEdit.OpenHyperLink(Text, EditProperties as TcxCustomHyperLinkEditProperties);
end;

function TcxHyperLinkEditViewInfo.IsMouseHoveredOverText(const AMousePos: TPoint): Boolean;
begin
  Result := PtInRect(GetActuallyTextBounds, AMousePos);
end;

function TcxHyperLinkEditViewInfo.IsHotTrack: Boolean;
begin
  Result := inherited IsHotTrack or
    TcxCustomHyperLinkEditProperties(EditProperties).SingleClick;
end;

function TcxHyperLinkEditViewInfo.IsHotTrack(P: TPoint): Boolean;
begin
  Result := IsHotTrack;
end;

procedure TcxHyperLinkEditViewInfo.PrepareCanvasFont(ACanvas: TCanvas);
begin
  inherited PrepareCanvasFont(ACanvas);
  if Edit = nil then
  begin
    ACanvas.Font.Style := ACanvas.Font.Style + [fsUnderline];
    if IsSelected then
      ACanvas.Font.Color := TextColor
    else
      ACanvas.Font.Color := GetHyperLinkTextColor;
  end;
end;

{ TcxCustomHyperLinkEditProperties }

constructor TcxCustomHyperLinkEditProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  AutoSelect := False;
  FLinkColor := clBlue;
  FPrefix := cxGetResourceString(@scxSHyperLinkPrefix);
  FSingleClick := False;
  FStartKey := VK_RETURN + scCtrl;
  FUsePrefix := upAlways;
end;

function TcxCustomHyperLinkEditProperties.GetAutoComplete: Boolean;
begin
  Result := not (UsePrefix = upNever);
end;

function TcxCustomHyperLinkEditProperties.GetPrefixStored: Boolean;
begin
  Result := FPrefix <> cxGetResourceString(@scxSHyperLinkPrefix);
end;

procedure TcxCustomHyperLinkEditProperties.ReadPrefix(Reader: TReader);
begin
  Prefix := Reader.ReadString;
end;

procedure TcxCustomHyperLinkEditProperties.SetAutoComplete(Value: Boolean);
begin
  if Value then
    UsePrefix := upAlways
  else
    UsePrefix := upNever;
end;

procedure TcxCustomHyperLinkEditProperties.SetLinkColor(
  const Value: TColor);
begin
  if FLinkColor <> Value then
  begin
    FLinkColor := Value;
    Changed;
  end;
end;

procedure TcxCustomHyperLinkEditProperties.SetSingleClick(Value: Boolean);
begin
  if Value <> FSingleClick then
  begin
    FSingleClick := Value;
    Changed;
  end;
end;

procedure TcxCustomHyperLinkEditProperties.WritePrefix(Writer: TWriter);
begin
  Writer.WriteString(Prefix);
end;

class function TcxCustomHyperLinkEditProperties.GetContainerClass: TcxContainerClass;
begin
  Result := TcxHyperLinkEdit;
end;

class function TcxCustomHyperLinkEditProperties.GetStyleClass: TcxCustomEditStyleClass;
begin
  Result := TcxHyperLinkStyle;
end;

function TcxCustomHyperLinkEditProperties.GetSupportedOperations: TcxEditSupportedOperations;
begin
  Result := inherited GetSupportedOperations;
  if SingleClick then
   Include(Result, esoAlwaysHotTrack);
end;

class function TcxCustomHyperLinkEditProperties.GetViewDataClass: TcxCustomEditViewDataClass;
begin
  Result := TcxHyperLinkEditViewData;
end;

class function TcxCustomHyperLinkEditProperties.GetViewInfoClass: TcxContainerViewInfoClass;
begin
  Result := TcxHyperLinkEditViewInfo;
end;

procedure TcxCustomHyperLinkEditProperties.ValidateDisplayValue(
  var ADisplayValue: TcxEditValue; var AErrorText: TCaption; var Error: Boolean;
    AEdit: TcxCustomEdit);
begin
  if UsePrefix = upAlways then
    ADisplayValue := AddPrefixTo(VarToStr(ADisplayValue));
  inherited ValidateDisplayValue(ADisplayValue, AErrorText, Error, AEdit);
end;

procedure TcxCustomHyperLinkEditProperties.DefineProperties(AFiler: TFiler);
begin
  inherited DefineProperties(AFiler);
  AFiler.DefineProperty('Prefix', ReadPrefix, WritePrefix, GetPrefixStored);
end;

procedure TcxCustomHyperLinkEditProperties.DoAssign(AProperties: TcxCustomEditProperties);
begin
  inherited;
  if AProperties is TcxCustomHyperLinkEditProperties then
    with TcxCustomHyperLinkEditProperties(AProperties) do
    begin
      Self.LinkColor := LinkColor;
      Self.Prefix := Prefix;
      Self.SingleClick := SingleClick;
      Self.StartKey := StartKey;
      Self.UsePrefix := UsePrefix;
      Self.OnStartClick := OnStartClick;
    end;
end;

function TcxCustomHyperLinkEditProperties.AddPrefixTo(const AStr: string): string;
begin
  Result := Trim(AStr);
  if (Prefix <> '') and (Result <> '') and (Pos(Prefix, Result) <> 1) then
    Result := Trim(Prefix + Result);
end;

{ TcxHyperLinkStyle }

function TcxHyperLinkStyle.GetTextColor: TColor;
begin
  if DirectAccessMode then
    Result := inherited GetTextColor
  else
  begin
    if (Container = nil) or (TcxCustomHyperLinkEdit(Container).ActiveProperties = nil) then
      Result := cxHyperLinkEditDefaultLinkColor
    else
      Result := TcxCustomHyperLinkEdit(Container).ActiveProperties.LinkColor;
  end;
end;

function TcxHyperLinkStyle.GetTextStyle: TFontStyles;
begin
  Result := inherited GetTextStyle + [fsUnderline];
end;

{ TcxCustomHyperLinkEdit }

procedure TcxCustomHyperLinkEdit.ActivateByMouse(Shift: TShiftState; X, Y: Integer; var AEditData: TcxCustomEditData);
begin
  TcxHyperLinkEditViewInfo(ViewInfo).FWasFocusedBeforeClick := False;
  inherited ActivateByMouse(Shift, X, Y, AEditData);
end;

function TcxCustomHyperLinkEdit.DoOnStartClick: Boolean;
begin
  Result := Assigned(Properties.OnStartClick) or Assigned(ActiveProperties.OnStartClick);
  CallNotify(Properties.OnStartClick, Self);
  if RepositoryItem <> nil then
    CallNotify(ActiveProperties.OnStartClick, Self);
end;

procedure TcxCustomHyperLinkEdit.DoStart;
begin
  if not DoOnStartClick then
    OpenHyperLink(DisplayText, ActiveProperties);
end;

function TcxCustomHyperLinkEdit.GetActiveProperties: TcxCustomHyperLinkEditProperties;
begin
  Result := TcxCustomHyperLinkEditProperties(InternalGetActiveProperties);
end;

function TcxCustomHyperLinkEdit.GetCurrentCursor(X, Y: Integer): TCursor;
begin
  Result := crDefault;
  if not IsDesigning then
    Result := TcxHyperLinkEditViewInfo(ViewInfo).GetCurrentCursor(Point(X, Y));
end;

function TcxCustomHyperLinkEdit.GetProperties: TcxCustomHyperLinkEditProperties;
begin
  Result := TcxCustomHyperLinkEditProperties(inherited Properties);
end;

function TcxCustomHyperLinkEdit.GetStyle: TcxHyperLinkStyle;
begin
  Result := TcxHyperLinkStyle(FStyles.Style);
end;

function TcxCustomHyperLinkEdit.InnerControlMenuHandler(var Message: TMessage): Boolean;
begin
  if Message.Msg = WM_LBUTTONDOWN then
    TcxHyperLinkEditViewInfo(ViewInfo).FWasFocusedBeforeClick := ViewInfo.Focused;
  Result := inherited InnerControlMenuHandler(Message);
end;

class procedure TcxCustomHyperLinkEdit.OpenHyperLink(const AHyperLink: string; AActiveProperties: TcxCustomHyperLinkEditProperties);
var
  ALink: string;
begin
  ALink := Trim(AHyperLink);
  if AActiveProperties.UsePrefix <> upNever then
    ALink := AActiveProperties.AddPrefixTo(ALink);
  if ALink <> '' then
    dxShellExecute(ALink, SW_SHOWMAXIMIZED);
end;

procedure TcxCustomHyperLinkEdit.SetProperties(
  const Value: TcxCustomHyperLinkEditProperties);
begin
  Properties.Assign(Value);
end;

procedure TcxCustomHyperLinkEdit.SetStyle(Value: TcxHyperLinkStyle);
begin
  FStyles.Style := Value;
end;

procedure TcxCustomHyperLinkEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (ShortCut(Key, Shift) <> 0) and (ActiveProperties.StartKey = ShortCut(Key, Shift)) then
  begin
    DoStart;
    Key := 0;
  end
  else
    inherited KeyDown(Key, Shift);
end;

procedure TcxCustomHyperLinkEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if (Button = mbLeft) and (ssDouble in Shift) and TcxHyperLinkEditViewInfo(ViewInfo).CanOpenHyperLink(Point(X, Y), False) then
    DoStart;
end;

procedure TcxCustomHyperLinkEdit.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if (Button = mbLeft) and (SelLength = 0) and TcxHyperLinkEditViewInfo(ViewInfo).CanOpenHyperLink(Point(X, Y), not(ssDouble in Shift)) then
    DoStart;
end;

procedure TcxCustomHyperLinkEdit.PropertiesChanged(Sender: TObject);
begin
  inherited PropertiesChanged(Sender);
  ContainerStyleChanged(Style);
end;

class function TcxCustomHyperLinkEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCustomHyperLinkEditProperties;
end;

procedure TcxCustomHyperLinkEdit.SelectAll;
begin
  if not (IsInplace and ActiveProperties.SingleClick) then
    inherited SelectAll;
end;

{ TcxHyperLinkEdit }

class function TcxHyperLinkEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxHyperLinkEditProperties;
end;

function TcxHyperLinkEdit.GetActiveProperties: TcxHyperLinkEditProperties;
begin
  Result := TcxHyperLinkEditProperties(InternalGetActiveProperties);
end;

function TcxHyperLinkEdit.GetProperties: TcxHyperLinkEditProperties;
begin
  Result := TcxHyperLinkEditProperties(inherited Properties);
end;

procedure TcxHyperLinkEdit.SetProperties(Value: TcxHyperLinkEditProperties);
begin
  Properties.Assign(Value);
end;

{ TcxFilterHyperLinkEditHelper }

class function TcxFilterHyperLinkEditHelper.GetFilterEditClass: TcxCustomEditClass;
begin
  Result := TcxHyperLinkEdit;
end;

{ TcxHyperLinkEditViewData }

procedure TcxHyperLinkEditViewData.Calculate(ACanvas: TcxCanvas;
  const ABounds: TRect; const P: TPoint; Button: TcxMouseButton;
  Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo; AIsMouseEvent: Boolean);
var
  AHyperLinkViewInfo: TcxHyperLinkEditViewInfo;
  AProperties: TcxCustomHyperLinkEditProperties;
begin
  inherited;
  AHyperLinkViewInfo := AViewInfo as TcxHyperLinkEditViewInfo;
  AProperties := Properties as TcxCustomHyperLinkEditProperties;
  AHyperLinkViewInfo.FSingleClick := AProperties.SingleClick;
  AHyperLinkViewInfo.FIsStandardAlignment := not AProperties.UseLeftAlignmentOnEditing or (AProperties.Alignment.Horz = taLeftJustify);
end;

initialization
  GetRegisteredEditProperties.Register(TcxHyperLinkEditProperties, scxSEditRepositoryHyperLinkItem);
  FilterEditsController.Register(TcxHyperLinkEditProperties, TcxFilterHyperLinkEditHelper);

finalization
  FilterEditsController.Unregister(TcxHyperLinkEditProperties, TcxFilterHyperLinkEditHelper);

end.
