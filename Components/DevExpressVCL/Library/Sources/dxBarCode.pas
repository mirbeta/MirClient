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


unit dxBarCode;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Variants, Windows, SysUtils, Classes, Clipbrd, Controls, Graphics,
  dxCore, dxCoreClasses, cxClasses, cxContainer, cxControls, cxGraphics, cxLookAndFeels, cxLookAndFeelPainters,
  cxDataUtils, cxVariants, dxTouch, dxSkinsCore, dxCoreGraphics, cxEdit, cxEditConsts, dxBarCodeUtils;

const
  dxBarCodeDefaultInplaceHeight = 15;

type
  TdxCustomBarCode = class;
  TdxCustomBarCodeProperties = class;

  TdxCustomBarCodeSymbologyAccess = class(TdxCustomBarCodeSymbology);

  { TdxBarCodeViewInfo }

  TdxBarCodeViewInfo = class(TcxCustomEditViewInfo)
  strict private
    function GetTransformMatrix: XFORM;  protected
  protected
    procedure InternalPaint(ACanvas: TcxCanvas); override;
  public
    BarCodeErrorType: TdxBarCodeErrorType;
    Center: Boolean;
    FitMode: TdxBarCodeFitMode;
    ModuleColor: TColor;
    ModuleWidth: Integer;
    RotationAngle: TcxRotationAngle;
    ShowText: Boolean;
    Symbology: TdxCustomBarCodeSymbology;
    Value: Variant;
    TopLeft: TPoint;
  end;

  { TdxBarCodeViewData }

  TdxBarCodeViewData = class(TcxCustomEditViewData)
  public
    procedure Calculate(ACanvas: TcxCanvas; const ABounds: TRect; const P: TPoint; Button: TcxMouseButton; Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo; AIsMouseEvent: Boolean); override;
    procedure EditValueToDrawValue(const AEditValue: TcxEditValue; AViewInfo: TcxCustomEditViewInfo); override;
    function GetEditContentSize(ACanvas: TcxCanvas; const AEditValue: TcxEditValue; const AEditSizeProperties: TcxEditSizeProperties; AErrorData: TcxEditValidateInfo = nil): TSize; override;
  end;

  { TdxCustomBarCodeProperties }

  TdxCustomBarCodeProperties = class(TcxCustomEditProperties)
  strict private
    FBarCodeSymbologyClass: TdxCustomBarCodeSymbologyClass;
    FFitMode: TdxBarCodeFitMode;
    FModuleColor: TColor;
    FModuleWidth: Integer;
    FShowText: Boolean;
    FSymbology: TdxCustomBarCodeSymbology;
    FRotationAngle: TcxRotationAngle;

    function GetBarCodeSymbologyClassName: string;
    procedure SetBarCodeSymbologyClass(AValue: TdxCustomBarCodeSymbologyClass);
    procedure SetBarCodeSymbologyClassName(AValue: string);
    procedure SetFitMode(AValue: TdxBarCodeFitMode);
    procedure SetModuleColor(AValue: TColor);
    procedure SetModuleWidth(AValue: Integer);
    procedure SetShowText(AValue: Boolean);
    procedure SetSymbology(AValue: TdxCustomBarCodeSymbology);
    procedure SetRotationAngle(AValue: TcxRotationAngle);
  protected
    procedure ChangeScale(M, D: Integer); override;
    procedure DoAssign(AProperties: TcxCustomEditProperties); override;
    class function GetViewDataClass: TcxCustomEditViewDataClass; override;

    procedure BarCodeSymbologyChangedHandler(Sender: TObject); virtual;
    function IsDesigning: Boolean;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    class function GetContainerClass: TcxContainerClass; override;
    class function GetViewInfoClass: TcxContainerViewInfoClass; override;
    function GetSpecialFeatures: TcxEditSpecialFeatures; override;
    function GetSupportedOperations: TcxEditSupportedOperations; override;

    property BarCodeSymbologyClass: TdxCustomBarCodeSymbologyClass read FBarCodeSymbologyClass write SetBarCodeSymbologyClass;
    property BarCodeSymbologyClassName: string read GetBarCodeSymbologyClassName write SetBarCodeSymbologyClassName;
    property FitMode: TdxBarCodeFitMode read FFitMode write SetFitMode;
    property ModuleColor: TColor read FModuleColor write SetModuleColor;
    property ModuleWidth: Integer read FModuleWidth write SetModuleWidth;
    property RotationAngle: TcxRotationAngle read FRotationAngle write SetRotationAngle;
    property ShowText: Boolean read FShowText write SetShowText default True;
    property Symbology: TdxCustomBarCodeSymbology read FSymbology write SetSymbology;
  end;

  { TdxBarCodeProperties }

  TdxBarCodeProperties = class(TdxCustomBarCodeProperties)
  public
    constructor Create(AOwner: TPersistent); override;
  published
    property BarCodeSymbologyClassName;
    property Symbology;
    property FitMode default ifmNormal;
    property ModuleColor default clBlack;
    property ModuleWidth default 2;
    property RotationAngle default ra0;
    property ShowText default True;
  end;

  { TdxCustomBarCode }

  TdxCustomBarCode = class(TcxCustomEdit)
  strict private
    function GetText: string;
    function GetProperties: TdxBarCodeProperties;
    function GetViewInfo: TdxBarCodeViewInfo;
    function GetActiveProperties: TdxBarCodeProperties;
    procedure SetText(AValue: string);
    procedure SetProperties(const Value: TdxBarCodeProperties);
  protected
    //TControl
    procedure AssignTo(Dest: TPersistent); override;
    function CanAutoWidth: Boolean; override;

    procedure Initialize; override;
    procedure InitializeViewData(AViewData: TcxCustomEditViewData); override;
    procedure InternalSetEditValue(const Value: Variant; AValidateEditValue: Boolean); override;

    procedure KeyPress(var Key: Char); override;

    property ViewInfo: TdxBarCodeViewInfo read GetViewInfo;
  public
    function CanFocus: Boolean; override;
    procedure CopyToClipboard; override;
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;

    property ActiveProperties: TdxBarCodeProperties read GetActiveProperties;
    property Text: string read GetText write SetText;
    property Properties: TdxBarCodeProperties read GetProperties write SetProperties;
  end;

  { TdxBarCode }

  TdxBarCode = class(TdxCustomBarCode)
  public
    constructor Create(AOwner: TComponent; AIsInplace: Boolean); override;
  published
    property Anchors;
    property AutoSize default True;
    property Text;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Properties;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleHot;
    property Transparent;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
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

uses
  Math,
  cxGeometry, cxConverterUtils, cxDrawTextUtils;

{ TdxBarCodeViewInfo }

procedure TdxBarCodeViewInfo.InternalPaint(ACanvas: TcxCanvas);
const
  cxRotationAngleToAngle: array[TcxRotationAngle] of Single = (0, 90, -90, 180);
var
  AText: string;
  ARect: TRect;
  ADifferenceSize: Integer;
begin
  inherited InternalPaint(ACanvas);
  ACanvas.SaveDC;
  try
    if RotationAngle <> ra0 then
    begin
      SetGraphicsMode(ACanvas.Handle, GM_ADVANCED);
      SetWorldTransform(ACanvas.Handle, GetTransformMatrix);
    end;
    if RotationAngle in [ra0, ra180] then
      ARect := ClientRect
    else
    begin
      ADifferenceSize := (cxRectWidth(ClientRect) - cxRectHeight(ClientRect)) div 2;
      ARect.Left := ClientRect.Left + ADifferenceSize - 1;
      ARect.Right := ClientRect.Right - ADifferenceSize + 1;
      ARect.Top:= ClientRect.Top - ADifferenceSize - 1;
      ARect.Bottom := ClientRect.Bottom + ADifferenceSize + 1;
    end;
    if Value <> Null then
    begin
      TdxCustomBarCodeSymbologyAccess(Symbology).FitMode := FitMode;
      TdxCustomBarCodeSymbologyAccess(Symbology).ModuleColor := ModuleColor;
      TdxCustomBarCodeSymbologyAccess(Symbology).ModuleWidth := ModuleWidth;
      TdxCustomBarCodeSymbologyAccess(Symbology).ShowText := ShowText;
      BarCodeErrorType := TdxCustomBarCodeSymbologyAccess(Symbology).Paint(ACanvas, ARect, VarToStr(Value), Font);
    end;
  finally
    ACanvas.RestoreDC;
  end;
  if BarCodeErrorType <> bceNone then
  begin
    case BarCodeErrorType of
      bceInvalidCharacters:
        AText := cxGetResourceString(@sdxBarCodeInvalidCharactersError);
      bceInvalidTextFormat:
        AText := cxGetResourceString(@sdxBarCodeInvalidTextFormatError);
      bceBoundsTooSmall:
        AText := cxGetResourceString(@sdxBarCodeControlTooNarrowError);
    else
      AText := '';
    end;
    cxTextOut(ACanvas.Handle, AText, ClientRect, CXTO_CENTER_HORIZONTALLY or CXTO_CENTER_VERTICALLY or CXTO_WORDBREAK, Font);
  end;
end;

function TdxBarCodeViewInfo.GetTransformMatrix: XFORM;
var
  ACenter: TPoint;
begin
  FillMemory(@Result, SizeOf(Result), 0);
  ACenter := cxRectCenter(ClientRect);
  case RotationAngle of
    raPlus90:
      begin
        Result.eM12 := 1;
        Result.eM21 := -1;
        Result.eDx := ACenter.X + ACenter.Y;
        Result.eDy := ACenter.Y - ACenter.X;
      end;
    raMinus90:
      begin
        Result.eM12 := -1;
        Result.eM21 := 1;
        Result.eDx := ACenter.X - ACenter.Y;
        Result.eDy := ACenter.Y + ACenter.X;
      end;
    ra180:
      begin
        Result.eM11 := -1;
        Result.eM22 := -1;
        Result.eDx := ACenter.X + ACenter.X;
        Result.eDy := ACenter.Y + ACenter.Y;
      end;
  end;
end;

{ TdxBarCodeViewData }

procedure TdxBarCodeViewData.Calculate(ACanvas: TcxCanvas; const ABounds: TRect; const P: TPoint;
  Button: TcxMouseButton; Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo; AIsMouseEvent: Boolean);
var
  AProperties: TdxCustomBarCodeProperties;
  ABarCodeViewInfo: TdxBarCodeViewInfo;
begin
  inherited Calculate(ACanvas, ABounds, P, Button, Shift, AViewInfo, AIsMouseEvent);
  if IsRectEmpty(ABounds) then
    Exit;
  AProperties := TdxCustomBarCodeProperties(Properties);
  ABarCodeViewInfo := TdxBarCodeViewInfo(AViewInfo);
  ABarCodeViewInfo.RotationAngle := AProperties.RotationAngle;
  ABarCodeViewInfo.Symbology := AProperties.Symbology;
  ABarCodeViewInfo.FitMode := AProperties.FitMode;
  ABarCodeViewInfo.ShowText := AProperties.ShowText;
  ABarCodeViewInfo.ModuleWidth := ScaleFactor.Apply(AProperties.ModuleWidth, AProperties.ScaleFactor);
  ABarCodeViewInfo.ModuleColor := AProperties.ModuleColor;
  ABarCodeViewInfo.BackgroundColor := Style.Color;
  if not (csvTextColor in Style.AssignedValues) then
    AViewInfo.Font.Color := AProperties.ModuleColor;
end;

procedure TdxBarCodeViewData.EditValueToDrawValue(const AEditValue: TcxEditValue; AViewInfo: TcxCustomEditViewInfo);
begin
  (AViewInfo as TdxBarCodeViewInfo).Value := AEditValue;
end;

function TdxBarCodeViewData.GetEditContentSize(ACanvas: TcxCanvas; const AEditValue: TcxEditValue;
  const AEditSizeProperties: TcxEditSizeProperties; AErrorData: TcxEditValidateInfo): TSize;

  function GetBarCodeSize(AProperties: TdxCustomBarCodeProperties): TSize;
  var
    ARect: TRect;
    ABarCodeSize: TSize;
    ABarCodeErrorType: TdxBarCodeErrorType;
  begin
    TdxCustomBarCodeSymbologyAccess(AProperties.Symbology).FitMode := AProperties.FitMode;
    TdxCustomBarCodeSymbologyAccess(AProperties.Symbology).ModuleWidth := ScaleFactor.Apply(AProperties.ModuleWidth, AProperties.ScaleFactor);
    TdxCustomBarCodeSymbologyAccess(AProperties.Symbology).ShowText := AProperties.ShowText;
    ABarCodeErrorType := TdxCustomBarCodeSymbologyAccess(AProperties.Symbology).CalculateSize(
      VarToStr(AEditValue), Style.Font, ABarCodeSize.cx, ABarCodeSize.cy);
    if ABarCodeErrorType <> bceNone then
    begin
      ARect := Rect(0, 0, ScaleFactor.Apply(100), ScaleFactor.Apply(dxBarCodeDefaultInplaceHeight));
      case ABarCodeErrorType of
        bceInvalidCharacters:
          cxGetTextRect(ARect, cxGetResourceString(@sdxBarCodeInvalidCharactersError), ACanvas.Font, DT_CENTER or DT_WORDBREAK);
        bceInvalidTextFormat:
          cxGetTextRect(ARect, cxGetResourceString(@sdxBarCodeInvalidTextFormatError), ACanvas.Font, DT_CENTER or DT_WORDBREAK);
      end;
      Result := cxRectSize(ARect);
    end
    else
    begin
      if AProperties.RotationAngle in [ra0, ra180] then
        Result := ABarCodeSize
      else
        Result := cxSize(ABarCodeSize.cy, ABarCodeSize.cx);
    end;
  end;

var
  AProperties: TdxCustomBarCodeProperties;
  ABorderExtent: TRect;
begin
  Result := inherited GetEditContentSize(ACanvas, AEditValue, AEditSizeProperties, AErrorData);
  AProperties := Properties as TdxCustomBarCodeProperties;
  if IsInplace then
  begin
    Result := Size(0, ScaleFactor.Apply(dxBarCodeDefaultInplaceHeight));
    if Edit <> nil then
    begin
      Result := Size(Edit.Width, Edit.Height);
      ABorderExtent := GetBorderExtent;
      Result.cx := Result.cx - (ABorderExtent.Left + ABorderExtent.Right);
      Result.cy := Result.cy - (ABorderExtent.Top + ABorderExtent.Bottom);
    end
    else
      if not VarIsNull(AEditValue) then
        Result := GetBarCodeSize(AProperties);
  end
  else
    Result := GetBarCodeSize(AProperties);
end;

{ TdxCustomBarCodeProperties }

constructor TdxCustomBarCodeProperties.Create(AOwner: TPersistent);
begin
  inherited;
  BarCodeSymbologyClass := TdxBarCodeEAN13Symbology;
end;

destructor TdxCustomBarCodeProperties.Destroy;
begin
  FreeAndNil(FSymbology);
  inherited;
end;

class function TdxCustomBarCodeProperties.GetContainerClass: TcxContainerClass;
begin
  Result := TdxBarCode;
end;

class function TdxCustomBarCodeProperties.GetViewInfoClass: TcxContainerViewInfoClass;
begin
  Result := TdxBarCodeViewInfo;
end;

function TdxCustomBarCodeProperties.GetSpecialFeatures: TcxEditSpecialFeatures;
begin
  Result := inherited GetSpecialFeatures + [esfMultiRow];
end;

function TdxCustomBarCodeProperties.GetSupportedOperations: TcxEditSupportedOperations;
begin
  Result := inherited GetSupportedOperations + [esoAutoHeight];
end;

procedure TdxCustomBarCodeProperties.ChangeScale(M: Integer; D: Integer);
begin
  inherited ChangeScale(M, D);
  ModuleWidth := MulDiv(ModuleWidth, M, D);
end;

procedure TdxCustomBarCodeProperties.DoAssign(AProperties: TcxCustomEditProperties);
var
  ABarCodeProperties: TdxCustomBarCodeProperties;
begin
  inherited;
  if AProperties is TdxCustomBarCodeProperties then
  begin
    ABarCodeProperties := TdxCustomBarCodeProperties(AProperties);
    FitMode := ABarCodeProperties.FitMode;
    ModuleColor := ABarCodeProperties.ModuleColor;
    ModuleWidth := ScaleFactor.Apply(ABarCodeProperties.ModuleWidth, ABarCodeProperties.ScaleFactor);
    RotationAngle := ABarCodeProperties.RotationAngle;
    ShowText := ABarCodeProperties.ShowText;
    BarCodeSymbologyClass := ABarCodeProperties.BarCodeSymbologyClass;
    Symbology := ABarCodeProperties.Symbology;
  end;
end;

class function TdxCustomBarCodeProperties.GetViewDataClass: TcxCustomEditViewDataClass;
begin
  Result := TdxBarCodeViewData;
end;

procedure TdxCustomBarCodeProperties.BarCodeSymbologyChangedHandler(Sender: TObject);
begin
  Changed;
end;

function TdxCustomBarCodeProperties.IsDesigning: Boolean;
var
  AOwner: TPersistent;
begin
  AOwner := GetOwner;
  Result := (AOwner is TComponent) and
    (csDesigning in (AOwner as TComponent).ComponentState);
end;

function TdxCustomBarCodeProperties.GetBarCodeSymbologyClassName: string;
begin
  if FSymbology = nil then
    Result := ''
  else
    Result := FSymbology.ClassName;
end;

procedure TdxCustomBarCodeProperties.SetBarCodeSymbologyClass(AValue: TdxCustomBarCodeSymbologyClass);
begin
  if (FBarCodeSymbologyClass <> AValue) and (AValue <> nil) then
  begin
    FBarCodeSymbologyClass := AValue;
    FreeAndNil(FSymbology);
    FSymbology := FBarCodeSymbologyClass.Create;
    TdxCustomBarCodeSymbologyAccess(FSymbology).OnChanged := BarCodeSymbologyChangedHandler;
    Changed;
  end;
end;

procedure TdxCustomBarCodeProperties.SetBarCodeSymbologyClassName(AValue: string);
begin
  BarCodeSymbologyClass := TdxCustomBarCodeSymbologyClass(dxGetRegisteredBarCodeSymbologies.FindByClassName(AValue));
end;

procedure TdxCustomBarCodeProperties.SetFitMode(AValue: TdxBarCodeFitMode);
begin
  if FFitMode <> AValue then
  begin
    FFitMode := AValue;
    Changed;
  end;
end;

procedure TdxCustomBarCodeProperties.SetModuleColor(AValue: TColor);
begin
  if FModuleColor <> AValue then
  begin
    FModuleColor := AValue;
    Changed;
  end;
end;

procedure TdxCustomBarCodeProperties.SetModuleWidth(AValue: Integer);
begin
  AValue := Max(AValue, 1);
  if FModuleWidth <> AValue then
  begin
    FModuleWidth := AValue;
    Changed;
  end;
end;

procedure TdxCustomBarCodeProperties.SetShowText(AValue: Boolean);
begin
  if FShowText <> AValue then
  begin
    FShowText := AValue;
    Changed;
  end;
end;

procedure TdxCustomBarCodeProperties.SetSymbology(AValue: TdxCustomBarCodeSymbology);
begin
  if (FSymbology <> nil) and (AValue <> nil) then
    FSymbology.Assign(AValue);
end;

procedure TdxCustomBarCodeProperties.SetRotationAngle(AValue: TcxRotationAngle);
begin
  if FRotationAngle <> AValue then
  begin
    FRotationAngle := AValue;
    Changed;
  end;
end;

{ TdxCustomBarCode }

function TdxCustomBarCode.CanFocus: Boolean;
begin
  Result := IsInplace;
end;

procedure TdxCustomBarCode.CopyToClipboard;
var
  ABitmap: TcxBitmap;
begin
  ABitmap := TcxBitmap.CreateSize(ViewInfo.ClientRect);
  try
    ViewInfo.InternalPaint(ABitmap.cxCanvas);
    Clipboard.Assign(ABitmap);
  finally
    ABitmap.Free;
  end;
end;

class function TdxCustomBarCode.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TdxBarCodeProperties;
end;

procedure TdxCustomBarCode.AssignTo(Dest: TPersistent);
var
  ACanvas: TcxCanvas;
begin
  if Dest is TBitmap then
  begin
    if HandleAllocated then
    begin
      TBitmap(Dest).SetSize(cxRectWidth(ViewInfo.ClientRect), cxRectHeight(ViewInfo.ClientRect));
      ACanvas := TcxCanvas.Create(TBitmap(Dest).Canvas);
      try
        ViewInfo.InternalPaint(ACanvas);
      finally
        ACanvas.Free;
      end;
    end;
  end
  else
    inherited;
end;

function TdxCustomBarCode.CanAutoWidth: Boolean;
begin
  Result := True;
end;

procedure TdxCustomBarCode.Initialize;
begin
  inherited;
  EditValue := '';
  Width := 230;
  Height := 100;
end;

procedure TdxCustomBarCode.InitializeViewData(AViewData: TcxCustomEditViewData);
begin
  inherited;
  AViewData.EditValueToDrawValue(EditValue, ViewInfo);
end;

procedure TdxCustomBarCode.InternalSetEditValue(const Value: Variant; AValidateEditValue: Boolean);
begin
  inherited;
  ShortRefreshContainer(False);
end;

procedure TdxCustomBarCode.KeyPress(var Key: Char);
begin
  inherited;
  BeginUserAction;
  try
    if Key = ^C then
      CopyToClipBoard;
  finally
    EndUserAction;
  end;
end;

function TdxCustomBarCode.GetActiveProperties: TdxBarCodeProperties;
begin
  Result := TdxBarCodeProperties(InternalGetActiveProperties);
end;

function TdxCustomBarCode.GetText: string;
begin
  if VarIsNull(EditValue) then
    Result := ''
  else
    Result := VarToStr(EditValue);
end;

function TdxCustomBarCode.GetProperties: TdxBarCodeProperties;
begin
  Result := TdxBarCodeProperties(inherited Properties);
end;

function TdxCustomBarCode.GetViewInfo: TdxBarCodeViewInfo;
begin
  Result := TdxBarCodeViewInfo(inherited ViewInfo);
end;

procedure TdxCustomBarCode.SetText(AValue: string);
begin
  InternalEditValue := AValue;
end;

procedure TdxCustomBarCode.SetProperties(const Value: TdxBarCodeProperties);
begin
  Properties.Assign(Value);
end;

{ TdxBarCode }

constructor TdxBarCode.Create(AOwner: TComponent; AIsInplace: Boolean);
begin
  inherited;
  AutoSize := True;
end;

{ TdxBarCodeProperties }

constructor TdxBarCodeProperties.Create(AOwner: TPersistent);
begin
  inherited;
  ModuleWidth := 2;
  ShowText := True;
end;

initialization
  GetRegisteredEditProperties.Register(TdxBarCodeProperties, scxSEditRepositoryBarCodeItem);

end.

