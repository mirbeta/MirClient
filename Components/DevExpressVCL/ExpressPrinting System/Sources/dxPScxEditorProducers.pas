{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPrinting System                                   }
{                                                                    }
{           Copyright (C) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                      }
{   EXECUTABLE PROGRAM ONLY                                          }
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

unit dxPScxEditorProducers;

interface

{$I cxVer.inc}

uses
  Windows, Classes, Controls, Graphics, cxLookAndFeels, cxGraphics, cxControls, cxContainer, cxEdit, cxMemo, cxTextEdit,
  cxCheckBox, dxCheckGroupBox, cxRadioGroup, cxBlobEdit, cxHyperLinkEdit, cxCurrencyEdit, cxGroupBox, cxDropDownEdit,
  cxImageComboBox, dxToggleSwitch, cxImage, dxThemeManager, dxPSGlbl, dxPScxCommon, dxPSContainerLnk, dxPSCore,
  cxLookAndFeelPainters, cxDrawTextUtils, dxPSReportRenderCanvas, dxSparkline, dxBarCode, dxBarCodeUtils, dxTokenEdit;

type

  { TdxPScxControlProducer }

  TdxPScxControlProducer = class(TdxPSContainerCustomWinControlProducer)
  public
    function Control: TcxControl; reintroduce; overload;
    class function ControlClass: TControlClass; override;
  end;

  { TdxPScxContainerProducer }

  TdxPScxContainerProducer = class(TdxPSContainerCustomWinControlProducer)
  private
    function GetIsNativeStyle: Boolean;
    function GetStyle:  TcxCustomContainerStyle ;
  protected
    function GetBorderClass: TdxPSCellBorderClass; virtual;
    function GetBorderColor: TColor; virtual;
    function GetContentColor: TColor; override;
    function GetFont: TFont; override;
    function GetFontColor: TColor; override;
    function GetFontStyle: TFontStyles; override;
    function GetThemedBorderColor: TColor; virtual;

    function CanProcessChild(AChildControl: TControl): Boolean; override;
    function CanUseNativeStyle(AThemedObjectType: TdxThemedObjectType): Boolean; virtual;
    function ObjectExpandHeight: Boolean; override;

    property BorderClass: TdxPSCellBorderClass read GetBorderClass;
    property BorderColor: TColor read GetBorderColor;
    property IsNativeStyle: Boolean read GetIsNativeStyle;
    property ThemedBorderColor: TColor read GetThemedBorderColor;
  public
    function Control: TcxContainer; reintroduce; overload;
    class function ControlClass: TControlClass; override;
    class function HasNativeSupportForBorders: Boolean; override;

    property Style:  TcxCustomContainerStyle  read GetStyle;
  end;

  { TdxPScxNativePrintableControlProducer }

  TdxPScxNativePrintableControlProducer = class(TdxPSNativePrintableControlProducer)
  private
    function GetBorderColor: TColor;
  protected
    function GetContentColor: TColor; override;
    function GetFont: TFont; override;
    procedure InitializeNativePrintableControlHost(AnItem: TdxReportVisualItem); override;
    property BorderColor: TColor read GetBorderColor;
  public
    function Control: TcxContainer; reintroduce; overload;
    class function ControlClass: TControlClass; override;
    function Style: TcxContainerStyle; overload; virtual;

    class function HasNativeSupportForBorders: Boolean; override;
  end;

  { TdxPScxCustomEditProducer }

  TdxPScxCustomEditProducer = class(TdxPScxContainerProducer)
  protected
    function GetControlBoundsRect: TRect; override;
    procedure InitializeItem(AnItem: TdxReportVisualItem); override;
    function ObjectShrinkHeight: Boolean; override;
  public
    function Control: TcxCustomEdit; reintroduce; overload;
    class function ControlClass: TControlClass; override;
    function Properties: TcxCustomEditProperties; virtual;

    property FontColor: TColor read GetFontColor;
  end;

  { TdxPScxCustomTextEditProducer }

  TdxPScxCustomTextEditProducer = class(TdxPScxCustomEditProducer)
  protected
    function GetDisplayText: string; virtual;
    procedure InitializeItem(AnItem: TdxReportVisualItem); override;
    function ItemClass: TdxReportVisualItemClass; override;
  public
    function Control: TcxCustomTextEdit; reintroduce; overload;
    class function ControlClass: TControlClass; override;
    function Properties: TcxCustomTextEditProperties; reintroduce; overload;
  end;

  { TdxPScxCustomMemoEditProducer }

  TdxPScxCustomMemoEditProducer = class(TdxPScxCustomTextEditProducer)
  protected
    function ObjectExpandHeight: Boolean; override;
    procedure InitializeItem(AnItem: TdxReportVisualItem); override;
  public
    function Control: TcxCustomMemo; reintroduce; overload;
    class function ControlClass: TControlClass; override;

    function ProducingObjectFriendlyName: string; override;
    function Properties: TcxCustomMemoProperties; reintroduce; overload;
  end;

  { TcxPScxCustomHyperLinkEditProducer }

  TcxPScxCustomHyperLinkEditProducer = class(TdxPScxCustomTextEditProducer)
  protected
    function GetFontColor: TColor; override;
    function GetFontStyle: TFontStyles; override;
  public
    function Control: TcxCustomHyperLinkEdit; reintroduce; overload;
    class function ControlClass: TControlClass; override;
    function Properties: TcxCustomHyperLinkEditProperties; reintroduce; overload;
  end;

  { TcxPScxCustomCurrencyEditProducer }

  TcxPScxCustomCurrencyEditProducer = class(TdxPScxCustomTextEditProducer)
  protected
    function GetDisplayText: string; override;
  public
    function Control: TcxCustomCurrencyEdit; reintroduce; overload;
    class function ControlClass: TControlClass; override;
    function Properties: TcxCustomCurrencyEditProperties; reintroduce; overload;
  end;

  { TdxPScxCustomComboBoxProducer }

  TdxPScxCustomComboBoxProducer = class(TdxPScxCustomTextEditProducer)
  public
    function Control: TcxCustomComboBox; reintroduce; overload;
    class function ControlClass: TControlClass; override;
    function Properties: TcxCustomComboBoxProperties; reintroduce; overload;
  end;

  { TdxPScxCustomImageComboBoxProducer }

  TdxPScxCustomImageComboBoxProducer = class(TdxPScxCustomComboBoxProducer)
  protected
    procedure GetImageLists(AProc: TdxPSGetImageListProc); override;
    procedure InitializeItem(AnItem: TdxReportVisualItem); override;
    function ItemClass: TdxReportVisualItemClass; override;
  public
    function Control: TcxCustomImageComboBox; reintroduce; overload;
    class function ControlClass: TControlClass; override;
    function Properties: TcxCustomImageComboBoxProperties; reintroduce; overload;
  end;

  { TdxPScxCustomCheckBoxProducer }

  TdxPScxCustomCheckBoxProducer = class(TdxPScxCustomEditProducer)
  protected
    procedure InitializeItem(AnItem: TdxReportVisualItem); override;
    function ItemClass: TdxReportVisualItemClass; override;
  public
    function Control: TcxCustomCheckBox; reintroduce; overload;
    class function ControlClass: TControlClass; override;
    function Properties: TcxCustomCheckBoxProperties; reintroduce; overload;
  end;

  { TdxPSdxSparklines }

  TdxPSdxSparklines = class(TdxPScxCustomEditProducer)
  protected
    procedure InitializeItem(AnItem: TdxReportVisualItem); override;
    function ItemClass: TdxReportVisualItemClass; override;
  public
    function Control: TdxCustomSparklineEdit; reintroduce; overload;
    class function ControlClass: TControlClass; override;
  end;

  { TdxPSdxTokenEdit }

  TdxPSdxTokenEdit = class(TdxPScxCustomEditProducer)
  protected
    procedure InitializeItem(AnItem: TdxReportVisualItem); override;
    function ItemClass: TdxReportVisualItemClass; override;
  public
    function Control: TdxCustomTokenEdit; reintroduce; overload;
    class function ControlClass: TControlClass; override;
  end;

  { TdxPSdxBarCode }

  TdxCustomBarCodeAccess = class(TdxCustomBarCode);

  TdxPSdxBarCode = class(TdxPScxCustomEditProducer)
  protected
    procedure InitializeItem(AnItem: TdxReportVisualItem); override;
    function ItemClass: TdxReportVisualItemClass; override;
    function ObjectExpandHeight: Boolean; override;
    function ObjectShrinkHeight: Boolean; override;
  public
    function Control: TdxCustomBarCode; reintroduce; overload;
    class function ControlClass: TControlClass; override;
  end;

  { TdxPSdxToggleSwitchProducer }

  TdxPSdxToggleSwitchProducer = class(TdxPScxCustomCheckBoxProducer)
  protected
    procedure InitializeItem(AnItem: TdxReportVisualItem); override;
  public
    function Control: TdxToggleSwitch; reintroduce; overload;
    class function ControlClass: TControlClass; override;
  end;

  { TdxPScxRadioButtonProducer }

  TdxPScxRadioButtonProducer = class(TdxPSRadioButtonProducer)
  protected
    procedure InitializeItem(AnItem: TdxReportVisualItem); override;
    function ItemClass: TdxReportVisualItemClass; override;
  public
    function Control: TcxRadioButton; reintroduce; overload;
    class function ControlClass: TControlClass; override;
  end;

  { TdxPScxCustomBlobEditProducer }

  TdxPScxCustomBlobEditProducer = class(TdxPSCustomDelegateProducer)
  protected
    function CanProcessChild(AChildControl: TControl): Boolean; override;
    function HostClass: TdxReportCellClass; override;
    function Producer: TdxPSCustomContainerItemProducer; override;
  public
    function Control: TcxCustomBlobEdit; reintroduce; overload;
    class function ControlClass: TControlClass; override;
  end;

  { TdxPScxDefaultBlobEditProducer }

  TdxPScxDefaultBlobEditProducer = class(TdxPScxCustomTextEditProducer)
  protected
    procedure InitializeItem(AnItem: TdxReportVisualItem); override;
  public
    function Control: TcxCustomBlobEdit; reintroduce; overload;
  end;

  { TdxPScxPictureBlobProducer}

  TdxPScxPictureBlobProducer = class(TdxPScxCustomEditProducer)
  protected
    procedure GetImageLists(AProc: TdxPSGetImageListProc); override;
    procedure InitializeItem(AnItem: TdxReportVisualItem); override;
    function ItemClass: TdxReportVisualItemClass; override;
  public
    function Control: TcxCustomBlobEdit; reintroduce; overload;
  end;

  { TdxPScxTextBlobEditProducer }

  TdxPScxTextBlobEditProducer = class(TdxPScxCustomTextEditProducer)
  protected
    procedure InitializeItem(AnItem: TdxReportVisualItem); override;
  public
    function Control: TcxCustomBlobEdit; reintroduce; overload;
  end;

  { TdxPScxCustomImageEditProducer }

  TdxPScxCustomImageEditProducer = class(TdxPScxCustomEditProducer)
  protected
    function GetHasImage: Boolean; virtual;
    procedure InitializeItem(AnItem: TdxReportVisualItem); override;
    procedure InitializeItemAsImage(AnItem: TdxReportCellGraphic); virtual;
    procedure InitializeItemAsText(AnItem: TdxReportCellString); virtual;
    function ItemClass: TdxReportVisualItemClass; override;
    function ObjectShrinkHeight: Boolean; override;
    function ObjectExpandHeight: Boolean; override;
  public
    function Control: TcxCustomImage; reintroduce; overload;
    class function ControlClass: TControlClass; override;
    function Properties: TcxCustomImageProperties; reintroduce; overload;

    property HasImage: Boolean read GetHasImage;
  end;

  { TdxPScxCustomGroupBoxProducer }

  TdxPScxCustomGroupBoxProducer = class(TdxPScxCustomEditProducer)
  protected
    function CanProcessChild(AChildControl: TControl): Boolean; override;
    procedure InitializeItem(AnItem: TdxReportVisualItem); override;
    procedure InitializeItemLookAndFeel(AnItem: TdxReportVisualItem; ALookAndFeel: TdxPSReportGroupLookAndFeel); virtual;
    function ItemClass: TdxReportVisualItemClass; override;
    function LookAndFeelClass: TdxPSReportGroupLookAndFeelClass; virtual;
  public
    class function CanHasAvailableChildren: Boolean; override;
    function Control: TcxCustomGroupBox; reintroduce; overload;
    class function ControlClass: TControlClass; override;
    function Properties: TcxCustomGroupBoxProperties; reintroduce; overload;
  end;

  { TdxPSdxCustomCheckGroupBoxProducer }

  TdxPSdxCustomCheckGroupBoxProducer = class(TdxPScxCustomGroupBoxProducer)
  protected
    procedure InitializeItem(AnItem: TdxReportVisualItem); override;
  public
    function Control: TdxCustomCheckGroupBox; reintroduce; overload;
    class function ControlClass: TControlClass; override;
    function Properties: TdxCheckGroupBoxProperties; reintroduce; overload;
  end;

  { TdxPScxCustomButtonGroupProducer }

  TdxPScxCustomButtonGroupProducer = class(TdxPScxCustomGroupBoxProducer)
  protected
    procedure CreateItems(AButtonGroup: TdxCustomReportButtonGroup); virtual;
    procedure InitializeButton(AGroup: TdxCustomReportButtonGroup;
      AButton: TdxCustomReportCellCheck; AnIndex: Integer); virtual;
    procedure InitializeItem(AnItem: TdxReportVisualItem); override;
    procedure InitializeItemLookAndFeel(AnItem: TdxReportVisualItem; ALookAndFeel: TdxPSReportGroupLookAndFeel); override;
    function ItemClass: TdxReportVisualItemClass; override;
  public
    function Control: TcxCustomButtonGroup; reintroduce; overload;
    class function ControlClass: TControlClass; override;
    function Properties: TcxCustomButtonGroupProperties; reintroduce; overload;
  end;

  { TdxPScxCustomRadioGroupProducer }

  TdxPScxCustomRadioGroupProducer = class(TdxPScxCustomButtonGroupProducer)
  protected
    function CanProcessChild(AChildControl: TControl): Boolean; override;
    procedure InitializeButton(AGroup: TdxCustomReportButtonGroup;
      AButton: TdxCustomReportCellCheck; AnIndex: Integer); override;
    procedure InitializeItem(AnItem: TdxReportVisualItem); override;
    function ItemClass: TdxReportVisualItemClass; override;
  public
    function Control: TcxCustomRadioGroup; reintroduce; overload;
    class function ControlClass: TControlClass; override;
    function Properties: TcxRadioGroupProperties; reintroduce; overload;
  end;

  { TdxPSBarCodePainter }

  TdxPSBarCodePainter = class(TdxBarCodeCustomPainter)
  strict private
    FCanvas: TdxPSReportRenderCustomCanvas;
  public
    procedure FillRect(const ARect: TRect; AColor: TColor); override;
    procedure DrawText(const ARect: TRect; const AText: string; AFormat: Integer; AFont: TFont); override;

    property Canvas: TdxPSReportRenderCustomCanvas read FCanvas write FCanvas;
  end;

  { TdxReportCellBarCode }

  TdxCustomBarCodeSymbologyAccess = class(TdxCustomBarCodeSymbology);

  TdxReportCellBarCode = class(TAbstractdxReportCellData)
  strict private
    FFitMode: TdxBarCodeFitMode;
    FModuleColor: TColor;
    FModuleWidth: Integer;
    FRotationAngle: TcxRotationAngle;
    FShowText: Boolean;
    FSymbology: TdxCustomBarCodeSymbology;
    FSymbologyClass: TdxCustomBarCodeSymbologyClass;
    FSymbologyClassName: string;
    FText: string;

    function GetBarCodeSize: TSize;
    function GetTransformMatrix(ARotationAngle: TcxRotationAngle): XFORM;
    procedure SetSymbologyClassName(AValue: string);
  protected
    procedure ReadData(AReader: TdxPSDataReader); override;
    procedure WriteData(AWriter: TdxPSDataWriter); override;
  public
    procedure Assign(Source: TPersistent); override;

    procedure DrawContent(ACanvas: TdxPSReportRenderCustomCanvas; AStage: TdxPSRenderStages); override;

    function MeasureContentHeight(ACanvas: TdxPSReportRenderCustomCanvas): Integer; override;
    function MeasureContentWidth(ACanvas: TdxPSReportRenderCustomCanvas): Integer; override;
    destructor Destroy; override;

    property FitMode: TdxBarCodeFitMode read FFitMode write FFitMode;
    property ModuleColor: TColor read FModuleColor write FModuleColor;
    property ModuleWidth: Integer read FModuleWidth write FModuleWidth;
    property RotationAngle: TcxRotationAngle read FRotationAngle write FRotationAngle;
    property ShowText: Boolean read FShowText write FShowText;
    property Symbology: TdxCustomBarCodeSymbology read FSymbology;
    property SymbologyClassName: string read FSymbologyClassName write SetSymbologyClassName;
    property Text: string read FText write FText;
  end;

  { TdxPSBarCodeDataMap }

  TdxPSBarCodeDataMap = class(TdxPSCustomDataMap)
  protected
    class procedure InitializeItem(AnItem: TAbstractdxReportCellData; AProperties: TcxCustomEditProperties;
      const AValue: TcxEditValue; const ACellParams: IdxPSCellParams; var AViewParams: TdxReportItemViewParams;
      AnIsPreview: Boolean = False; ARecordIndex: Integer = 0; AOwner: TObject = nil); override;
    class function ItemClass(AProperties: TcxCustomEditProperties; AnIsPreview: Boolean = False): TdxReportCellDataClass; override;
    class function PropertiesClass: TcxCustomEditPropertiesClass; override;
  end;

{ Utilities }

function cxBordersMap(ABorders: TcxBorders): TdxCellSides;

const
  CheckBorderStyleMap: array[TcxContainerBorderStyle] of TdxCheckButtonEdgeStyle =
    (cbesNone, {cbesUltraFlat}cbesSingle, cbesBoldFlat, cbesSoft3D, cbes3D, cbesUltraFlat
   , cbesUltraFlat );
  CheckPosMap: array[TAlignment] of TdxCellCheckPos = (ccpLeft, ccpRight, ccpCenter);
  ContainerBorderStyleMap: array[TcxContainerBorderStyle] of TdxPSCellBorderClass =
    (TdxPSCellNullBorder, TdxPSCellUltraFlatBorder, TdxPSCellBoldFlatBorder,
     TdxPSCellSunkenSoftBorder, TdxPSCellSunkenBorder, TdxPSCellUltraFlatBorder
    , TdxPSCellUltraFlatBorder );
  EditBorderStyleMap: array[TcxEditBorderStyle] of TdxPSCellBorderClass =
    (TdxPSCellNullBorder, TdxPSCellUltraFlatBorder, TdxPSCellBoldFlatBorder,
     TdxPSCellSunkenSoftBorder, TdxPSCellSunkenBorder, TdxPSCellUltraFlatBorder
    , TdxPSCellSunkenSoftBorder);
  RadioLookAndFeelKindMap: array[TcxLookAndFeelKind] of TdxCheckButtonEdgeStyle =
    (cbesSoft3D, cbes3D, cbesUltraFlat, cbesUltraFlat );
  RadioBorderStyleMap: array[TcxContainerBorderStyle] of TdxCheckButtonEdgeStyle =
    (cbesNone, cbesUltraFlat, cbesBoldFlat, cbesSoft3D, cbes3D, cbesUltraFlat
   , cbesUltraFlat );
  GroupBoxCaptionAlignmentMap: array[TcxCaptionAlignment] of TcxTextAlignX =
    (taLeft, taCenterX, taRight, taLeft, taLeft, taLeft, taLeft, taLeft, taLeft, taLeft, taLeft, taLeft, taLeft);

implementation

uses
  Types, Variants, SysUtils, dxThemeConsts, dxUxTheme, cxVariants, dxPSUtl,
  dxPScxEditorLnks, dxPScxListBoxLnk, cxGeometry, dxGDIPlusClasses;

type
  TcxCustomGroupBoxAccess = class(TcxCustomGroupBox);
  TdxCustomCheckGroupBoxAccess = class(TdxCustomCheckGroupBox);
  TdxToggleSwitchAccess = class(TdxToggleSwitch);
  TdxToggleSwitchViewInfoAccess = class(TdxCustomToggleSwitchViewInfo);

var
  FPicture: TPicture;

function Picture: TPicture;
begin
  if FPicture = nil then
    FPicture := TPicture.Create;
  Result := FPicture;
end;

{ Helpers }

function cxCheckBox_GetState(AControl: TcxCustomEdit): TcxCheckBoxState;
begin
  Result := TcxCustomCheckBox(AControl).State;
end;

function cxCurrencyEdit_GetValue(AControl: TcxCustomEdit): Double;
begin
  Result := TcxCustomCurrencyEdit(AControl).Value;
end;

function cxGroupBox_GetPanelStyle(AControl: TcxCustomEdit): TcxPanelStyle;
begin
  Result := TcxCustomGroupBoxAccess(AControl).PanelStyle;
end;

function cxGroupBox_GetAlignment(AControl: TcxCustomEdit): TcxCaptionAlignment;
begin
  Result := TcxCustomGroupBox(AControl).Alignment;
end;

function cxImageComboBox_GetItemIndex(AControl: TcxCustomEdit): Integer;
begin
  Result := TcxCustomImageComboBox(AControl).ItemIndex;
end;

{ Utilities}

function cxBordersMap(ABorders: TcxBorders): TdxCellSides;
begin
  Result := [];
  if bLeft in ABorders then Include(Result, csLeft);
  if bTop in ABorders then Include(Result, csTop);
  if bRight in ABorders then Include(Result, csRight);
  if bBottom in ABorders then Include(Result, csBottom);
end;

{ TdxPScxControlProducer }

function TdxPScxControlProducer.Control: TcxControl;
begin
  Result := inherited Control as TcxControl;
end;

class function TdxPScxControlProducer.ControlClass: TControlClass;
begin
  Result := TcxControl;
end;

{ TdxPScxContainerProducer }

function TdxPScxContainerProducer.Control: TcxContainer;
begin
  Result := inherited Control as TcxContainer;
end;

class function TdxPScxContainerProducer.ControlClass: TControlClass;
begin
  Result := TcxContainer;
end;

class function TdxPScxContainerProducer.HasNativeSupportForBorders: Boolean;
begin
  Result := True;
end;

function TdxPScxContainerProducer.GetBorderClass: TdxPSCellBorderClass;
begin
  if CanUseNativeStyle(totEdit) then
    Result := TdxPSCellUltraFlatBorder
  else
    Result := ContainerBorderStyleMap[Style.BorderStyle];
end;

function TdxPScxContainerProducer.GetBorderColor: TColor;
begin
  if CanUseNativeStyle(totEdit) then
    Result := ThemedBorderColor
  else
    if Style.BorderStyle in [cbsSingle, cbsThick] then
      Result := Style.BorderColor
    else
      Result := dxPSCore.dxDefaultGridLineColor;
end;

function TdxPScxContainerProducer.GetContentColor: TColor;
begin
  Result := Style.Color;
end;

function TdxPScxContainerProducer.GetFont: TFont;
begin
  Result := Style.Font;
end;

function TdxPScxContainerProducer.GetFontColor: TColor;
begin
  Result := Style.TextColor;
end;

function TdxPScxContainerProducer.GetFontStyle: TFontStyles;
begin
  Result := Style.TextStyle;
end;

function TdxPScxContainerProducer.GetThemedBorderColor: TColor;
var
  Theme: TdxTheme;
  C: COLORREF;
begin
  Theme := dxThemeManager.OpenTheme(totEdit);
  if Succeeded(dxUxTheme.GetThemeColor(Theme, 0, 0, TMT_BORDERCOLOR, C)) then
    Result := C
  else
    Result := dxPSCore.dxDefaultGridLineColor;
end;

function TdxPScxContainerProducer.CanProcessChild(AChildControl: TControl): Boolean;
begin
  Result := inherited CanProcessChild(AChildControl) and not Supports(AChildControl, IcxContainerInnerControl);
end;

function TdxPScxContainerProducer.CanUseNativeStyle(AThemedObjectType: TdxThemedObjectType): Boolean;
begin
  Result := IsNativeStyle and
   dxThemeManager.AreVisualStylesAvailable(AThemedObjectType);
end;

function TdxPScxContainerProducer.ObjectExpandHeight: Boolean;
begin
  Result := True;
end;

function TdxPScxContainerProducer.GetIsNativeStyle: Boolean;
begin
  Result := Style.LookAndFeel.NativeStyle;
end;

function TdxPScxContainerProducer.GetStyle:  TcxCustomContainerStyle ;
begin
  if Control.Enabled then
    Result := Control.Styles[csNormal]
  else
    Result := Control.Styles[csDisabled]
end;

{ TdxPScxNativePrintableControlProducer }

function TdxPScxNativePrintableControlProducer.Control: TcxContainer;
begin
  Result := inherited Control as TcxContainer;
end;

class function TdxPScxNativePrintableControlProducer.ControlClass: TControlClass;
begin
  Result := TcxContainer;
end;

function TdxPScxNativePrintableControlProducer.Style: TcxContainerStyle;
begin
  Result := Control.Styles[csNormal];
end;

class function TdxPScxNativePrintableControlProducer.HasNativeSupportForBorders: Boolean;
begin
  Result := True;
end;

function TdxPScxNativePrintableControlProducer.GetContentColor: TColor;
begin
  Result := Style.Color;
end;

function TdxPScxNativePrintableControlProducer.GetFont: TFont;
begin
  Result := Style.Font;
end;

procedure TdxPScxNativePrintableControlProducer.InitializeNativePrintableControlHost(AnItem: TdxReportVisualItem);
begin
  with AnItem do
  begin
    BorderClass := ContainerBorderStyleMap[Style.BorderStyle];
    BorderColor := Style.BorderColor;
    CellSides := cxBordersMap(Style.Edges);
    ShowShadow := Style.Shadow;
  end;
  inherited;
end;

function TdxPScxNativePrintableControlProducer.GetBorderColor: TColor;
begin
  if Style.BorderStyle in [cbsSingle, cbsThick] then
    Result := Style.BorderColor
  else
    Result := clWindowText;
end;

{ TdxPScxCustomEditProducer }

function TdxPScxCustomEditProducer.Control: TcxCustomEdit;
begin
  Result := inherited Control as TcxCustomEdit;
end;

class function TdxPScxCustomEditProducer.ControlClass: TControlClass;
begin
  Result := TcxCustomEdit;
end;

function TdxPScxCustomEditProducer.Properties: TcxCustomEditProperties;
begin
  Result := cxEdit_GetProperties(Control);
end;

function TdxPScxCustomEditProducer.GetControlBoundsRect: TRect;
const
  ShadowDepth: Integer = 3; // Actually should be borrowed from AnItem.ShadowDepth
begin
  Result := inherited GetControlBoundsRect;
  if Style.Shadow then
    with Result do
    begin
      Dec(Right, ShadowDepth);
      Dec(Bottom, ShadowDepth);
    end;
end;

procedure TdxPScxCustomEditProducer.InitializeItem(AnItem: TdxReportVisualItem);
begin
  inherited;
  AnItem.BorderClass := BorderClass;
  AnItem.BorderColor := BorderColor;
  AnItem.CellSides := cxBordersMap(Style.Edges);
  AnItem.ShowShadow := Style.Shadow;
end;

function TdxPScxCustomEditProducer.ObjectShrinkHeight: Boolean;
begin
  Result := False;//True;
end;

{ TdxPScxCustomTextEditProducer }

function TdxPScxCustomTextEditProducer.Control: TcxCustomTextEdit;
begin
  Result := inherited Control as TcxCustomTextEdit;
end;

class function TdxPScxCustomTextEditProducer.ControlClass: TControlClass;
begin
  Result := TcxCustomTextEdit;
end;

function TdxPScxCustomTextEditProducer.Properties: TcxCustomTextEditProperties;
begin
  Result := inherited Properties as TcxCustomTextEditProperties;
end;

function TdxPScxCustomTextEditProducer.GetDisplayText: string;
begin
  Result := Properties.GetDisplayText(Control.EditValue, False, False);
end;

procedure TdxPScxCustomTextEditProducer.InitializeItem(AnItem: TdxReportVisualItem);
var
  EditAlignment: TcxEditAlignment;
begin
  inherited;
  TdxReportCellString(AnItem).Text := GetDisplayText;
  EditAlignment := cxEditProperties_GetAlignment(Properties);
  TdxReportCellString(AnItem).TextAlignX := EditTextAlignXMap[EditAlignment.Horz];
  TdxReportCellString(AnItem).TextAlignY := EditTextAlignYMap[EditAlignment.Vert];
end;

function TdxPScxCustomTextEditProducer.ItemClass: TdxReportVisualItemClass;
begin
  Result := TdxReportCellString;
end;

{ TdxPScxCustomMemoEditProducer }

function TdxPScxCustomMemoEditProducer.Control: TcxCustomMemo;
begin
  Result := inherited Control as TcxCustomMemo;
end;

class function TdxPScxCustomMemoEditProducer.ControlClass: TControlClass;
begin
  Result := TcxCustomMemo;
end;

function TdxPScxCustomMemoEditProducer.ProducingObjectFriendlyName: string;
begin
  Result := '';
  if not IsDesigning and (Control.Lines.Count <> 0) then
  begin
    Result := Control.Lines[0];
    if Length(Result) > MaxCaptionLength then
    begin
      Delete(Result, MaxCaptionLength, Length(Result) - MaxCaptionLength);
      Result := Result + '...';
    end;
  end;
  if Result = '' then
    Result := inherited ProducingObjectFriendlyName;
end;

function TdxPScxCustomMemoEditProducer.Properties: TcxCustomMemoProperties;
begin
  Result := inherited Properties as TcxCustomMemoProperties;
end;

function TdxPScxCustomMemoEditProducer.ObjectExpandHeight: Boolean;
begin
  Result := False;
end;

procedure TdxPScxCustomMemoEditProducer.InitializeItem(AnItem: TdxReportVisualItem);
begin
  inherited;
  TdxReportCellString(AnItem).MultiLine := True;
  TdxReportCellString(AnItem).Text := Control.Text;
  TdxReportCellString(AnItem).TextAlignX := TextAlignXMap[cxMemoProperties_GetAlignment(Properties)];
  TdxReportCellString(AnItem).TextAlignY := taTop;
end;

{ TcxPScxCustomHyperLinkEditProducer }

function TcxPScxCustomHyperLinkEditProducer.Control: TcxCustomHyperLinkEdit;
begin
  Result := inherited Control as TcxCustomHyperLinkEdit;
end;

class function TcxPScxCustomHyperLinkEditProducer.ControlClass: TControlClass;
begin
  Result := TcxCustomHyperLinkEdit;
end;

function TcxPScxCustomHyperLinkEditProducer.Properties: TcxCustomHyperLinkEditProperties;
begin
  Result := inherited Properties as TcxCustomHyperLinkEditProperties;
end;

function TcxPScxCustomHyperLinkEditProducer.GetFontColor: TColor;
begin
  Result := cxHyperLinkEditProperties_GetLinkColor(Properties);
end;

function TcxPScxCustomHyperLinkEditProducer.GetFontStyle: TFontStyles;
begin
  Result := inherited GetFontStyle + [fsUnderline];
end;

{ TcxPScxCustomCurrencyEditProducer }

function TcxPScxCustomCurrencyEditProducer.Control: TcxCustomCurrencyEdit;
begin
  Result := inherited Control as TcxCustomCurrencyEdit;
end;

class function TcxPScxCustomCurrencyEditProducer.ControlClass: TControlClass;
begin
  Result := TcxCustomCurrencyEdit;
end;

function TcxPScxCustomCurrencyEditProducer.Properties: TcxCustomCurrencyEditProperties;
begin
  Result := inherited Properties as TcxCustomCurrencyEditProperties;
end;

function TcxPScxCustomCurrencyEditProducer.GetDisplayText: string;
begin
  try
    Result := inherited GetDisplayText;
  except
    if cxCurrencyEdit_GetValue(Control) = 0 then
      Result := cxCurrencyProperties_GetNullString(Properties)
    else
      Result := '';
  end;
end;

{ TdxPScxCustomComboBoxProducer }

function TdxPScxCustomComboBoxProducer.Control: TcxCustomComboBox;
begin
  Result := inherited Control as TcxCustomComboBox;
end;

class function TdxPScxCustomComboBoxProducer.ControlClass: TControlClass;
begin
  Result := TcxCustomComboBox;
end;

function TdxPScxCustomComboBoxProducer.Properties: TcxCustomComboBoxProperties;
begin
  Result := inherited Properties as TcxCustomComboBoxProperties;
end;

{ TdxPScxCustomImageComboBoxProducer }

function TdxPScxCustomImageComboBoxProducer.Control: TcxCustomImageComboBox;
begin
  Result := inherited Control as TcxCustomImageComboBox;
end;

class function TdxPScxCustomImageComboBoxProducer.ControlClass: TControlClass;
begin
  Result := TcxCustomImageComboBox;
end;

function TdxPScxCustomImageComboBoxProducer.Properties: TcxCustomImageComboBoxProperties;
begin
  Result := inherited Properties as TcxCustomImageComboBoxProperties;
end;

procedure TdxPScxCustomImageComboBoxProducer.GetImageLists(AProc: TdxPSGetImageListProc);
begin
  inherited;
  AProc(cxImageComboBoxProperties_GetImages(Properties));
end;

procedure TdxPScxCustomImageComboBoxProducer.InitializeItem(AnItem: TdxReportVisualItem);
var
  ItemIndex: Integer;
  ComboItems: TcxImageComboBoxItems;
begin
  inherited;
  ItemIndex := cxImageComboBox_GetItemIndex(Control);
  if ItemIndex <> -1 then
  begin
    ComboItems := cxImageComboBoxProperties_GetItems(Properties);
    TdxReportCellImage(AnItem).ImageIndex := ComboItems[ItemIndex].ImageIndex;
    TdxReportCellImage(AnItem).Text := ComboItems[ItemIndex].Description;
  end
  else
  begin
    TdxReportCellImage(AnItem).ImageIndex := cxImageComboBoxProperties_GetDefaultImageIndex(Properties);
    TdxReportCellImage(AnItem).Text := cxImageComboBoxProperties_GetDefaultDescription(Properties);
  end;

  TdxReportCellImage(AnItem).ImageLayout := ImageLayoutMap[cxImageComboBoxProperties_GetImageAlignment(Properties)];
  TdxReportCellImage(AnItem).ImageList := cxImageComboBoxProperties_GetImages(Properties);
  TdxReportCellImage(AnItem).MakeSpaceForEmptyImage := True;
  TdxReportCellImage(AnItem).Multiline := cxImageComboBoxProperties_GetIsMultilined(Properties);
  if not cxImageComboBoxProperties_GetShowDescription(Properties) then
  begin
    TdxReportCellImage(AnItem).ImageLayout := ilImageCenterCenter;
    TdxReportCellImage(AnItem).Text := '';
  end;
end;

function TdxPScxCustomImageComboBoxProducer.ItemClass: TdxReportVisualItemClass;
begin
  Result := TdxReportCellImage;
end;

{ TdxPScxCustomCheckBoxProducer }

function TdxPScxCustomCheckBoxProducer.Control: TcxCustomCheckBox;
begin
  Result := inherited Control as TcxCustomCheckBox;
end;

class function TdxPScxCustomCheckBoxProducer.ControlClass: TControlClass;
begin
  Result := TcxCustomCheckBox;
end;

function TdxPScxCustomCheckBoxProducer.Properties: TcxCustomCheckBoxProperties;
begin
  Result := inherited Properties as TcxCustomCheckBoxProperties;
end;

procedure TdxPScxCustomCheckBoxProducer.InitializeItem(AnItem: TdxReportVisualItem);
var
  CheckNullStyle: TcxCheckBoxNullValueShowingStyle;
  CheckState: TcxCheckBoxState;
  CheckGlyph: TdxSmartGlyph;
begin
  inherited;
  CheckNullStyle := cxCheckBoxProperties_GetNullStyle(Properties);
  CheckState := cxCheckBox_GetState(Control);

  TdxReportCellCheckImage(AnItem).CellSides := [];
  TdxReportCellCheckImage(AnItem).Checked := (CheckState = cbsChecked) or ((CheckState = cbsGrayed) and (CheckNullStyle = nssGrayedChecked));
  TdxReportCellCheckImage(AnItem).ButtonEdgeStyle := CheckBorderStyleMap[Style.BorderStyle];
  TdxReportCellCheckImage(AnItem).CheckPos := CheckPosMap[cxCheckBoxProperties_GetAlignment(Properties)];
  TdxReportCellCheckImage(AnItem).Enabled := Control.Enabled and not ((CheckState = cbsGrayed) and (CheckNullStyle in [nssInactive, nssGrayedChecked]));

  CheckGlyph := cxCheckBoxProperties_GetGlyph(Properties);
  if not CheckGlyph.Empty then
  begin
    TdxReportCellCheckImage(AnItem).Glyph.Assign(CheckGlyph);
    TdxReportCellCheckImage(AnItem).GlyphCount := cxCheckBoxProperties_GetGlyphCount(Properties);
  end;
  TdxReportCellCheckImage(AnItem).Multiline := cxCheckBoxProperties_GetIsMultilined(Properties);
  TdxReportCellCheckImage(AnItem).Text := Control_GetText(Control);
  TdxReportCellCheckImage(AnItem).TextAlignY := taCenterY;
  TdxReportCellCheckImage(AnItem).TextAlignX := taLeft;
end;

function TdxPScxCustomCheckBoxProducer.ItemClass: TdxReportVisualItemClass;
begin
  Result := TdxReportCellCheckImage;
end;

type
  TdxSparklineImage = class(TdxReportCellSparklineImage);

{ TdxPSdxSparklines  }

procedure TdxPSdxSparklines.InitializeItem(AnItem: TdxReportVisualItem);
begin
  if TdxSparklineImage(AnItem).ViewData = nil then
  begin
    TdxSparklineImage(AnItem).ViewData := Control.Properties.CreateViewData(DefaultEditStyleController.Style, True);
    TdxSparklineImage(AnItem).ViewInfo := Control.Properties.GetViewInfoClass.Create as TdxSparkLineViewInfo;
    TdxSparklineImage(AnItem).Transparent := True;
    TdxSparklineImage(AnItem).ViewData.EditValueToDrawValue(Control.EditValue, TdxSparklineImage(AnItem).ViewInfo);
  end;
  inherited;
  TdxSparklineImage(AnItem).BoundsChanged;
end;

function TdxPSdxSparklines.ItemClass: TdxReportVisualItemClass;
begin
  Result := TdxReportCellSparklineImage;
end;

function TdxPSdxSparklines.Control: TdxCustomSparklineEdit;
begin
  Result := inherited Control as TdxCustomSparklineEdit;
end;

class function TdxPSdxSparklines.ControlClass: TControlClass;
begin
  Result := TdxCustomSparklineEdit;
end;

{ TdxPSdxTokenEdit }

function TdxPSdxTokenEdit.Control: TdxCustomTokenEdit;
begin
  Result := inherited Control as TdxCustomTokenEdit;
end;

class function TdxPSdxTokenEdit.ControlClass: TControlClass;
begin
  Result := TdxCustomTokenEdit;
end;

procedure TdxPSdxTokenEdit.InitializeItem(AnItem: TdxReportVisualItem);
var
  AViewParams: TdxReportItemViewParams;
begin
  inherited InitializeItem(AnItem);
  ZeroMemory(@AViewParams, SizeOf(TdxReportItemViewParams));
  dxPSDataMaps.InitializeItem(AnItem as TdxReportCellTokens, Control.ActiveProperties, Control.EditValue, nil, AViewParams);
end;

function TdxPSdxTokenEdit.ItemClass: TdxReportVisualItemClass;
begin
  Result := TdxReportCellTokens;
end;

{ TdxPSdxBarCode }

function TdxPSdxBarCode.Control: TdxCustomBarCode;
begin
  Result := inherited Control as TdxCustomBarCode;
end;

class function TdxPSdxBarCode.ControlClass: TControlClass;
begin
  Result := TdxCustomBarCode;
end;

procedure TdxPSdxBarCode.InitializeItem(AnItem: TdxReportVisualItem);
var
  ABarCodeProperties: TdxBarCodeProperties;
  AItem: TdxReportCellBarCode;
begin
  inherited;
  if Properties is TdxBarCodeProperties then
  begin
    ABarCodeProperties := TdxBarCodeProperties(Properties);
    AItem := AnItem as TdxReportCellBarCode;
    AItem.SymbologyClassName := ABarCodeProperties.BarCodeSymbologyClassName;
    AItem.Symbology.Assign(ABarCodeProperties.Symbology);
    AItem.RotationAngle := ABarCodeProperties.RotationAngle;
    AItem.ModuleWidth := ABarCodeProperties.ModuleWidth;
    AItem.ModuleColor := ABarCodeProperties.ModuleColor;
    AItem.ShowText := ABarCodeProperties.ShowText;
    AItem.Text := VarToStr(Control.EditValue);
    AItem.FitMode := ABarCodeProperties.FitMode;
    AItem.Transparent := TdxCustomBarCodeAccess(Control).Transparent;
    if not (csvTextColor in Control.Style.AssignedValues) then
      AItem.Font.Color := ABarCodeProperties.ModuleColor;
    AItem.CellSides := [];
  end;
end;

function TdxPSdxBarCode.ItemClass: TdxReportVisualItemClass;
begin
  Result := TdxReportCellBarCode;
end;

function TdxPSdxBarCode.ObjectExpandHeight: Boolean;
begin
  Result := False;
end;

function TdxPSdxBarCode.ObjectShrinkHeight: Boolean;
begin
  Result := False;
end;

{ TdxPSdxToggleSwitchProducer }

function TdxPSdxToggleSwitchProducer.Control: TdxToggleSwitch;
begin
  Result := inherited Control as TdxToggleSwitch;
end;

class function TdxPSdxToggleSwitchProducer.ControlClass: TControlClass;
begin
  Result := TdxToggleSwitch;
end;

procedure TdxPSdxToggleSwitchProducer.InitializeItem(AnItem: TdxReportVisualItem);

  procedure DrawToggleSwitch(ABitmap: TcxBitmap);
  const
    dxToggleSwitchTextOffset: Integer = 7;
  var
    AViewInfo: TdxToggleSwitchViewInfoAccess;
    ARect: TRect;
  begin
    AViewInfo := TdxToggleSwitchViewInfoAccess(TdxToggleSwitchAccess(Control).ViewInfo);
    ABitmap.SetSize(AViewInfo.ClientRect);
    TdxReportCellCheckImage(AnItem).GlyphCount := 1;
    AViewInfo.Paint(ABitmap.cxCanvas);
    ARect := ABitmap.ClientRect;
    ARect.Left := ARect.Left + cxRectWidth(AViewInfo.TextRect) + cxGetValueCurrentDPI(dxToggleSwitchTextOffset);
    ABitmap.Canvas.CopyRect(cxRectSetNullOrigin(ARect), ABitmap.Canvas, ARect);
    ABitmap.SetSize(cxRectWidth(ARect), cxRectHeight(ARect));
  end;

var
  ABitmap: TcxBitmap;
begin
  inherited;
  ABitmap := TcxBitmap.Create;
  try
    DrawToggleSwitch(ABitmap);
    TdxReportCellCheckImage(AnItem).Glyph.Assign(ABitmap);
  finally
    ABitmap.Free;
  end;
end;

{ TdxPScxRadioButtonProducer }

function TdxPScxRadioButtonProducer.Control: TcxRadioButton;
begin
  Result := inherited Control as TcxRadioButton;
end;

class function TdxPScxRadioButtonProducer.ControlClass: TControlClass;
begin
  Result := TcxRadioButton;
end;

procedure TdxPScxRadioButtonProducer.InitializeItem(AnItem: TdxReportVisualItem);
begin
  inherited;
  TdxReportCellRadio(AnItem).ButtonEdgeStyle := RadioLookAndFeelKindMap[Control.LookAndFeel.Kind];
  TdxReportCellRadio(AnItem).Multiline := Control.WordWrap;
end;

function TdxPScxRadioButtonProducer.ItemClass: TdxReportVisualItemClass;
begin
  Result := TdxReportCellRadio;
end;

{ TdxPScxCustomBlobEditProducer }

function TdxPScxCustomBlobEditProducer.Control: TcxCustomBlobEdit;
begin
  Result := inherited Control as TcxCustomBlobEdit;
end;

class function TdxPScxCustomBlobEditProducer.ControlClass: TControlClass;
begin
  Result := TcxCustomBlobEdit;
end;

function TdxPScxCustomBlobEditProducer.CanProcessChild(AChildControl: TControl): Boolean;
begin
  Result := False;
end;

function TdxPScxCustomBlobEditProducer.HostClass: TdxReportCellClass;
begin
  Result := TdxReportWinControlHost;
end;

function TdxPScxCustomBlobEditProducer.Producer: TdxPSCustomContainerItemProducer;
const
  ProducerClasses: array[TcxBlobPaintStyle] of TdxPSCustomContainerItemProducerClass =
    (TdxPScxDefaultBlobEditProducer, TdxPScxPictureBlobProducer, TdxPScxTextBlobEditProducer);
var
  ProducerClass: TdxPSCustomContainerItemProducerClass;
begin
  ProducerClass := ProducerClasses[cxBlobEditProperties_GetPaintStyle(cxEdit_GetProperties(Control))];
  Result := TdxPSCustomContainerItemProducer(ReportLink.ProducersByClass[ProducerClass, Control]);
end;

{ TdxPScxDefaultBlobEditProducer }

function TdxPScxDefaultBlobEditProducer.Control: TcxCustomBlobEdit;
begin
  Result := inherited Control as TcxCustomBlobEdit;
end;

procedure TdxPScxDefaultBlobEditProducer.InitializeItem(AnItem: TdxReportVisualItem);
begin
  inherited;
  TdxReportCellString(AnItem).Text :=
    cxBlobEdit.GetBlobText(Control.EditValue, TcxCustomBlobEditProperties(cxEdit_GetProperties(Control)), False);
end;

{ TdxPScxPictureBlobProducer }

function TdxPScxPictureBlobProducer.Control: TcxCustomBlobEdit;
begin
  Result := inherited Control as TcxCustomBlobEdit;
end;

procedure TdxPScxPictureBlobProducer.GetImageLists(AProc: TdxPSGetImageListProc);
begin
  inherited;
  AProc(GetBlobImages);
end;

procedure TdxPScxPictureBlobProducer.InitializeItem(AnItem: TdxReportVisualItem);

  function HasEditValue: Boolean;
  begin
    Result := not VarIsNull(Control.EditValue) and not VarIsEmpty(Control.EditValue);
  end;

const
  ImageIndexes: array[TcxBlobEditKind] of Integer = (0, 2, 4, 6, 0);
begin
  inherited;
  TdxReportCellGraphic(AnItem).Center := True;
  TdxReportCellGraphic(AnItem).ImageList := GetBlobImages;
  TdxReportCellGraphic(AnItem).ImageIndex := Ord(HasEditValue) + ImageIndexes[cxBlobEditProperties_GetEditKind(cxEdit_GetProperties(Control))];
end;

function TdxPScxPictureBlobProducer.ItemClass: TdxReportVisualItemClass;
begin
  Result := TdxReportCellGraphic;
end;

{ TdxPScxTextBlobEditProducer }

function TdxPScxTextBlobEditProducer.Control: TcxCustomBlobEdit;
begin
  Result := inherited Control as TcxCustomBlobEdit;
end;

procedure TdxPScxTextBlobEditProducer.InitializeItem(AnItem: TdxReportVisualItem);
begin
  inherited;
  TdxReportCellString(AnItem).EndEllipsis := True;
  TdxReportCellString(AnItem).Multiline := False;
  TdxReportCellString(AnItem).Text := cxBlobEdit.GetBlobText(Control.EditValue, TcxCustomBlobEditProperties(cxEdit_GetProperties(Control)), False);
end;

{ TdxPScxCustomImageEditProducer }

function TdxPScxCustomImageEditProducer.Control: TcxCustomImage;
begin
  Result := inherited Control as TcxCustomImage;
end;

class function TdxPScxCustomImageEditProducer.ControlClass: TControlClass;
begin
  Result := TcxCustomImage;
end;

function TdxPScxCustomImageEditProducer.Properties: TcxCustomImageProperties;
begin
  Result := inherited Properties as TcxCustomImageProperties;
end;

function TdxPScxCustomImageEditProducer.GetHasImage: Boolean;
var
  ImagePicture: TPicture;
begin
  ImagePicture := cxImage_GetPicture(Control);
  Result := (ImagePicture.Graphic <> nil) and not ImagePicture.Graphic.Empty;
end;

procedure TdxPScxCustomImageEditProducer.InitializeItem(AnItem: TdxReportVisualItem);
begin
  inherited;
  if HasImage then
    InitializeItemAsImage(TdxReportCellGraphic(AnItem))
  else
    InitializeItemAsText(TdxReportCellString(AnItem));
end;

procedure TdxPScxCustomImageEditProducer.InitializeItemAsImage(AnItem: TdxReportCellGraphic);
var
  GraphicClass: TClass;
begin
  Picture.Assign(cxImage_GetPicture(Control));

  if Picture.Graphic <> nil then
    GraphicClass := Picture.Graphic.ClassType
  else
    GraphicClass := nil;

  AnItem.Center := cxImageProperties_GetCenter(Properties);
  AnItem.Image := Picture.Graphic;
  AnItem.ImageTransparent := ((GraphicClass <> nil) and GraphicClass.InheritsFrom(TIcon)) or
    ReportLink.OptionsTransparent.Graphics;
  AnItem.Stretch := (GraphicClass <> nil) and not GraphicClass.InheritsFrom(TIcon) and
    cxImageProperties_GetStretch(Properties);
end;

procedure TdxPScxCustomImageEditProducer.InitializeItemAsText(AnItem: TdxReportCellString);
begin
  AnItem.Text := cxImageProperties_GetCaption(Properties);
  AnItem.TextAlignX := taCenterX;
  AnItem.TextAlignY := taCenterY;
end;

function TdxPScxCustomImageEditProducer.ItemClass: TdxReportVisualItemClass;
const
  ItemClasses: array[Boolean] of TdxReportCellDataClass = (TdxReportCellString, TdxReportCellGraphic);
begin
  Result := ItemClasses[HasImage];
end;

function TdxPScxCustomImageEditProducer.ObjectShrinkHeight: Boolean;
begin
  Result := False;
end;

function TdxPScxCustomImageEditProducer.ObjectExpandHeight: Boolean;
begin
  Result := False;
end;

{ TdxPScxCustomGroupBoxProducer }

class function TdxPScxCustomGroupBoxProducer.CanHasAvailableChildren: Boolean;
begin
  Result := True;
end;

function TdxPScxCustomGroupBoxProducer.CanProcessChild(AChildControl: TControl): Boolean;
begin
  Result := True;
end;

function TdxPScxCustomGroupBoxProducer.Control: TcxCustomGroupBox;
begin
  Result := inherited Control as TcxCustomGroupBox;
end;

class function TdxPScxCustomGroupBoxProducer.ControlClass: TControlClass;
begin
  Result := TcxCustomGroupBox;
end;

function TdxPScxCustomGroupBoxProducer.Properties: TcxCustomGroupBoxProperties;
begin
  Result := inherited Properties as TcxCustomGroupBoxProperties;
end;

procedure TdxPScxCustomGroupBoxProducer.InitializeItem(AnItem: TdxReportVisualItem);
begin
  inherited InitializeItem(AnItem);
  TdxReportGroup(AnItem).Transparent := OptionsTransparent.Containers;
  TdxReportGroup(AnItem).BorderClass := ContainerBorderStyleMap[Self.Style.BorderStyle];
  TdxReportGroup(AnItem).CaptionAlignment := GroupBoxCaptionAlignmentMap[cxGroupBox_GetAlignment(Control)];
  TdxReportGroup(AnItem).CaptionText := Control_GetText(Control);
  TdxReportGroup(AnItem).CaptionTransparent := TdxReportGroup(AnItem).Transparent;
  TdxReportGroup(AnItem).UseOwnBorderClass := True;

  TdxReportGroup(AnItem).LookAndFeel := ReportLink.CreateGroupLookAndFeel(LookAndFeelClass, False);
  InitializeItemLookAndFeel(AnItem, TdxReportGroup(AnItem).LookAndFeel);
  TdxReportGroup(AnItem).LookAndFeel.Prepare(Canvas);
  TdxReportGroup(AnItem).CalculateCaptionTextWidth(Canvas);
end;

procedure TdxPScxCustomGroupBoxProducer.InitializeItemLookAndFeel(AnItem: TdxReportVisualItem;
  ALookAndFeel: TdxPSReportGroupLookAndFeel);
begin
  ALookAndFeel.CaptionFontIndex := AnItem.FontIndex;
  ALookAndFeel.Color := ContentColor;
  ALookAndFeel.FontIndex := AnItem.FontIndex;
end;

function TdxPScxCustomGroupBoxProducer.ItemClass: TdxReportVisualItemClass;
begin
  Result := TdxReportGroup;
end;

function TdxPScxCustomGroupBoxProducer.LookAndFeelClass: TdxPSReportGroupLookAndFeelClass;
begin
  if cxGroupBox_GetPanelStyle(Control).Active then
    Result := TdxPSReportGroupPanelStyleLookAndFeel
  else
    Result := TdxPSReportGroupStandardLookAndFeel;
end;

{ TdxPSdxCustomCheckGroupBoxProducer }

function TdxPSdxCustomCheckGroupBoxProducer.Control: TdxCustomCheckGroupBox;
begin
  Result := inherited Control as TdxCustomCheckGroupBox;
end;

class function TdxPSdxCustomCheckGroupBoxProducer.ControlClass: TControlClass;
begin
  Result := TdxCustomCheckGroupBox;
end;

function TdxPSdxCustomCheckGroupBoxProducer.Properties: TdxCheckGroupBoxProperties;
begin
  Result := inherited Properties as TdxCheckGroupBoxProperties;
end;

procedure TdxPSdxCustomCheckGroupBoxProducer.InitializeItem(AnItem: TdxReportVisualItem);
begin
  inherited InitializeItem(AnItem);
  TdxReportGroup(AnItem).CheckBoxVisible := TdxCustomCheckGroupBoxAccess(Control).CheckBox.Visible;
  TdxReportGroup(AnItem).CheckBoxChecked := TdxCustomCheckGroupBoxAccess(Control).CheckBox.Checked;
end;

{ TdxPScxCustomButtonGroupProducer }

function TdxPScxCustomButtonGroupProducer.Control: TcxCustomButtonGroup;
begin
  Result := inherited Control as TcxCustomButtonGroup;
end;

class function TdxPScxCustomButtonGroupProducer.ControlClass: TControlClass;
begin
  Result := TcxCustomButtonGroup;
end;

function TdxPScxCustomButtonGroupProducer.Properties: TcxCustomButtonGroupProperties;
begin
  Result := inherited Properties as TcxCustomButtonGroupProperties;
end;

procedure TdxPScxCustomButtonGroupProducer.CreateItems(AButtonGroup: TdxCustomReportButtonGroup);
var
  Items: TcxButtonGroupItems;
  I: Integer;
  Button: TdxCustomReportCellCheck;
begin
  Items := cxButtonGroupProperties_GetItems(Properties);
  for I := 0 to Items.Count - 1 do
  begin
    Button := AButtonGroup.Add(cxButtonGroupItem_GetCaption(Items[I]));
    InitializeButton(AButtonGroup, Button, I);
  end;
end;

procedure TdxPScxCustomButtonGroupProducer.InitializeButton(AGroup: TdxCustomReportButtonGroup;
  AButton: TdxCustomReportCellCheck; AnIndex: Integer);
begin
  AButton.BorderColor := Style.BorderColor;
  AButton.ButtonEdgeStyle := AGroup.ButtonEdgeStyle;
end;

procedure TdxPScxCustomButtonGroupProducer.InitializeItem(AnItem: TdxReportVisualItem);
begin
  inherited;
  TdxCustomReportButtonGroup(AnItem).ButtonEdgeStyle := RadioBorderStyleMap[Style.BorderStyle];
  TdxCustomReportButtonGroup(AnItem).ColumnCount := cxButtonGroupProperties_GetColumnCount(Properties);
  CreateItems(TdxCustomReportButtonGroup(AnItem));
  TdxCustomReportButtonGroup(AnItem).ShowCaption := (TdxCustomReportButtonGroup(AnItem).CaptionText <> '') and
    (cxGroupBox_GetAlignment(Control) <> alCenterCenter);
  TdxCustomReportButtonGroup(AnItem).AdjustContent(Canvas);
end;

procedure TdxPScxCustomButtonGroupProducer.InitializeItemLookAndFeel(AnItem: TdxReportVisualItem;
  ALookAndFeel: TdxPSReportGroupLookAndFeel);
begin
  inherited;
end;

function TdxPScxCustomButtonGroupProducer.ItemClass: TdxReportVisualItemClass;
begin
  Result := TdxCustomReportButtonGroup;
end;

{ TdxPScxRadioGroupProducer }

function TdxPScxCustomRadioGroupProducer.Control: TcxCustomRadioGroup;
begin
  Result := inherited Control as TcxCustomRadioGroup;
end;

class function TdxPScxCustomRadioGroupProducer.ControlClass: TControlClass;
begin
  Result := TcxCustomRadioGroup;
end;

function TdxPScxCustomRadioGroupProducer.Properties: TcxRadioGroupProperties;
begin
  Result := inherited Properties as TcxRadioGroupProperties;
end;

function TdxPScxCustomRadioGroupProducer.CanProcessChild(AChildControl: TControl): Boolean;
begin
  Result := inherited CanProcessChild(AChildControl) and
    not (AChildControl is TcxCustomRadioGroupButton); // RadioGroup buttons are processed inside
end;

procedure TdxPScxCustomRadioGroupProducer.InitializeItem(AnItem: TdxReportVisualItem);
begin
  inherited;
  TdxReportRadioGroup(AnItem).ItemIndex := Control.ItemIndex;
  TdxReportRadioGroup(AnItem).CheckPos := ccpLeft;
end;

procedure TdxPScxCustomRadioGroupProducer.InitializeButton(AGroup: TdxCustomReportButtonGroup;
  AButton: TdxCustomReportCellCheck; AnIndex: Integer);
begin
  inherited;
  //TdxReportCellRadio(AButton).ButtonEdgeStyle := AGroup.ButtonEdgeStyle;
  TdxReportCellRadio(AButton).Enabled := Control.Enabled;
end;

function TdxPScxCustomRadioGroupProducer.ItemClass: TdxReportVisualItemClass;
begin
  Result := TdxReportRadioGroup;
end;

{ TdxPSBarCodePainter }

procedure TdxPSBarCodePainter.DrawText(const ARect: TRect; const AText: string; AFormat: Integer; AFont: TFont);
var
  ATextRect: TRect;
begin
  inherited;
  ATextRect := ARect;
  Canvas.DrawText(ATextRect, AText, AFont, AFormat);
end;

procedure TdxPSBarCodePainter.FillRect(const ARect: TRect; AColor: TColor);
begin
  inherited;
  Canvas.FillRect(ARect, AColor);
end;

{ TdxReportCellBarCode }

destructor TdxReportCellBarCode.Destroy;
begin
  FreeAndNil(FSymbology);
  inherited;
end;

procedure TdxReportCellBarCode.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TdxReportCellBarCode then
  begin
    SymbologyClassName := TdxReportCellBarCode(Source).SymbologyClassName;
    Symbology.Assign(TdxReportCellBarCode(Source).Symbology);
    Text := TdxReportCellBarCode(Source).Text;
    RotationAngle := TdxReportCellBarCode(Source).RotationAngle;
    ModuleColor := TdxReportCellBarCode(Source).ModuleColor;
    ModuleWidth := TdxReportCellBarCode(Source).ModuleWidth;
    ShowText := TdxReportCellBarCode(Source).ShowText;
    FitMode := TdxReportCellBarCode(Source).FitMode;
  end;
end;

procedure TdxReportCellBarCode.DrawContent(ACanvas: TdxPSReportRenderCustomCanvas; AStage: TdxPSRenderStages);
var
  ASymbology: TdxCustomBarCodeSymbology;
  ARect: TRect;
  ADifferenceSize: Integer;
  APainter: TdxPSBarCodePainter;
begin
  if Symbology <> nil then
  begin
    DrawBackground(ACanvas);
    if IsBordersDrawn then
      DrawBorders(ACanvas);
    ASymbology := FSymbologyClass.Create;
    try
      ASymbology.Assign(Symbology);
      TdxCustomBarCodeSymbologyAccess(ASymbology).ModuleWidth := ConvertValue(ModuleWidth);
      TdxCustomBarCodeSymbologyAccess(ASymbology).FitMode := FitMode;
      TdxCustomBarCodeSymbologyAccess(ASymbology).ModuleColor := ModuleColor;
      TdxCustomBarCodeSymbologyAccess(ASymbology).ShowText := ShowText;
      ACanvas.SaveState;
      try
        if RotationAngle <> ra0 then
          ACanvas.WorldTransform := GetTransformMatrix(RotationAngle);
        if RotationAngle in [ra0, ra180] then
          ARect := BoundsRect
        else
        begin
          ADifferenceSize := (cxRectWidth(BoundsRect) - cxRectHeight(BoundsRect)) div 2;
          ARect := cxRectInflate(BoundsRect, -ADifferenceSize + 1, ADifferenceSize + 1, -ADifferenceSize + 1, ADifferenceSize + 1);
        end;
        APainter := TdxPSBarCodePainter.Create;
        try
          APainter.Canvas := ACanvas;
          TdxCustomBarCodeSymbologyAccess(ASymbology).Paint(APainter, ARect, Text, Font);
        finally
          APainter.Free;
        end;
      finally
        ACanvas.RestoreState;
      end;
    finally
      ASymbology.Free;
    end;
  end;
end;

function TdxReportCellBarCode.MeasureContentHeight(ACanvas: TdxPSReportRenderCustomCanvas): Integer;
begin
  Result := GetBarCodeSize.cy;
end;

function TdxReportCellBarCode.MeasureContentWidth(ACanvas: TdxPSReportRenderCustomCanvas): Integer;
begin
  Result := GetBarCodeSize.cx;
end;

procedure TdxReportCellBarCode.ReadData(AReader: TdxPSDataReader);

  procedure ReadSymbology;
  begin
    if FSymbologyClass = TdxBarCodeInterleaved2Of5Symbology then
    begin
      TdxBarCodeInterleaved2Of5Symbology(Symbology).Checksum := AReader.ReadBoolean;
      TdxBarCodeInterleaved2Of5Symbology(Symbology).WideNarrowRatio := AReader.ReadSingle;
    end;
    if FSymbologyClass = TdxBarCode39Symbology then
    begin
      TdxBarCode39Symbology(Symbology).Checksum := AReader.ReadBoolean;
      TdxBarCode39Symbology(Symbology).WideNarrowRatio := AReader.ReadSingle;
    end;
    if FSymbologyClass = TdxBarCodeQRCodeSymbology then
    begin
      TdxBarCodeQRCodeSymbology(Symbology).CompactionMode := TdxQRCodeCompactionMode(AReader.ReadInteger);
      TdxBarCodeQRCodeSymbology(Symbology).ErrorCorrectionLevel := TdxQRCodeErrorCorrectionLevel(AReader.ReadInteger);
      TdxBarCodeQRCodeSymbology(Symbology).Version := AReader.ReadInteger;
    end;
  end;

var
  AFitMode: Integer;
begin
  inherited;
  AFitMode := AReader.ReadInteger;
  FitMode := TcxImageFitMode(AFitMode);
  ModuleColor := AReader.ReadInteger;
  ModuleWidth := AReader.ReadInteger;
  RotationAngle := TcxRotationAngle(AReader.ReadInteger);
  ShowText := AReader.ReadBoolean;
  SymbologyClassName := AReader.ReadString;
  ReadSymbology;
  Text := AReader.ReadString;
end;

procedure TdxReportCellBarCode.WriteData(AWriter: TdxPSDataWriter);

  procedure WriteSymbology;
  begin
    if FSymbologyClass = TdxBarCodeInterleaved2Of5Symbology then
    begin
      AWriter.WriteBoolean(TdxBarCodeInterleaved2Of5Symbology(Symbology).Checksum);
      AWriter.WriteSingle(TdxBarCodeInterleaved2Of5Symbology(Symbology).WideNarrowRatio);
    end;
    if FSymbologyClass = TdxBarCode39Symbology then
    begin
      AWriter.WriteBoolean(TdxBarCode39Symbology(Symbology).Checksum);
      AWriter.WriteSingle(TdxBarCode39Symbology(Symbology).WideNarrowRatio);
    end;
    if FSymbologyClass = TdxBarCodeQRCodeSymbology then
    begin
      AWriter.WriteInteger(Integer(TdxBarCodeQRCodeSymbology(Symbology).CompactionMode));
      AWriter.WriteInteger(Integer(TdxBarCodeQRCodeSymbology(Symbology).ErrorCorrectionLevel));
      AWriter.WriteInteger(TdxBarCodeQRCodeSymbology(Symbology).Version);
    end;
  end;

begin
  inherited;
  AWriter.WriteInteger(Integer(FitMode));
  AWriter.WriteInteger(ModuleColor);
  AWriter.WriteInteger(ModuleWidth);
  AWriter.WriteInteger(Integer(RotationAngle));
  AWriter.WriteBoolean(ShowText);
  AWriter.WriteString(SymbologyClassName);
  WriteSymbology;
  AWriter.WriteString(Text);
end;

function TdxReportCellBarCode.GetBarCodeSize: TSize;
var
  AWidth, AHeight: Integer;
begin
  Result := cxNullSize;
  if (Symbology <> nil) then
  begin
    TdxCustomBarCodeSymbologyAccess(Symbology).ModuleWidth :=
      ConvertValue(TdxCustomBarCodeSymbologyAccess(Symbology).ModuleWidth);
    try
      if (TdxCustomBarCodeSymbologyAccess(Symbology).CalculateSize(Text, Font, AWidth, AHeight) = bceNone) then
        Result := cxSize(MulDiv(AWidth, PixelsDenominator, PixelsNumerator),
          MulDiv(AHeight, PixelsDenominator, PixelsNumerator));
    finally
      TdxCustomBarCodeSymbologyAccess(Symbology).ModuleWidth :=
        MulDiv(TdxCustomBarCodeSymbologyAccess(Symbology).ModuleWidth, PixelsDenominator, PixelsNumerator);
    end;
  end;
end;

function TdxReportCellBarCode.GetTransformMatrix(ARotationAngle: TcxRotationAngle): XFORM;
var
  ACenter: TPoint;
begin
  FillMemory(@Result, SizeOf(Result), 0);
  ACenter := cxRectCenter(BoundsRect);
  case ARotationAngle of
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


procedure TdxReportCellBarCode.SetSymbologyClassName(AValue: string);
begin
  if FSymbologyClassName <> AValue then
  begin
    FSymbologyClassName := AValue;
    FreeAndNil(FSymbology);
    FSymbologyClass := TdxCustomBarCodeSymbologyClass(dxGetRegisteredBarCodeSymbologies.FindByClassName(SymbologyClassName));
    FSymbology := FSymbologyClass.Create;
  end;
end;

{ TdxPSBarCodeDataMap }

class procedure TdxPSBarCodeDataMap.InitializeItem(AnItem: TAbstractdxReportCellData;
  AProperties: TcxCustomEditProperties; const AValue: TcxEditValue; const ACellParams: IdxPSCellParams;
  var AViewParams: TdxReportItemViewParams; AnIsPreview: Boolean; ARecordIndex: Integer; AOwner: TObject);
var
  ABarCodeProperties: TdxBarCodeProperties;
  AItem: TdxReportCellBarCode;
begin
  if AProperties is TdxBarCodeProperties then
  begin
    ABarCodeProperties := TdxBarCodeProperties(AProperties);
    AItem := AnItem as TdxReportCellBarCode;
    AItem.SymbologyClassName := ABarCodeProperties.BarCodeSymbologyClassName;
    AItem.Symbology.Assign(ABarCodeProperties.Symbology);
    AItem.RotationAngle := ABarCodeProperties.RotationAngle;
    AItem.ModuleColor := ABarCodeProperties.ModuleColor;
    AItem.ModuleWidth := ABarCodeProperties.ModuleWidth;
    AItem.ShowText := ABarCodeProperties.ShowText;
    AItem.Text := VarToStr(AValue);
    AItem.FitMode := ABarCodeProperties.FitMode;
    AItem.Font.Color := ABarCodeProperties.ModuleColor;
    AItem.CellSides := [csLeft, csTop, csRight, csBottom];
  end;
end;

class function TdxPSBarCodeDataMap.ItemClass(AProperties: TcxCustomEditProperties;
  AnIsPreview: Boolean): TdxReportCellDataClass;
begin
  Result := TdxReportCellBarCode;
end;

class function TdxPSBarCodeDataMap.PropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TdxCustomBarCodeProperties;
end;

procedure RegisterAssistants;
begin
  TdxPScxNativePrintableControlProducer.Register;

  //TdxPScxControlProducer.Register;
  //TdxPScxContainerProducer.Register;
  //TdxPScxCustomEditProducer.Register;
  TdxPScxCustomTextEditProducer.Register;
  TdxPScxCustomMemoEditProducer.Register;
  TcxPScxCustomHyperLinkEditProducer.Register;
  TcxPScxCustomCurrencyEditProducer.Register;
  TdxPScxCustomComboBoxProducer.Register;
  TdxPScxCustomImageComboBoxProducer.Register;
  TdxPScxCustomCheckBoxProducer.Register;
  TdxPScxRadioButtonProducer.Register;
  TdxPScxCustomBlobEditProducer.Register;
  TdxPScxCustomImageEditProducer.Register;
  TdxPScxCustomGroupBoxProducer.Register;
  TdxPScxCustomRadioGroupProducer.Register;
  TdxPSdxCustomCheckGroupBoxProducer.Register;
  TdxPSdxToggleSwitchProducer.Register;
  TdxPSdxSparklines.Register;
  TdxPSdxTokenEdit.Register;
  TdxPSdxBarCode.Register;
  TdxReportCellBarCode.Register;
  TdxPSBarCodeDataMap.Register;
end;

procedure UnregisterAssistants;
begin
  TdxPSBarCodeDataMap.Unregister;
  TdxReportCellBarCode.Unregister;
  TdxPSdxBarCode.Unregister;
  TdxPSdxSparklines.Unregister;
  TdxPSdxTokenEdit.Unregister;
  TdxPSdxToggleSwitchProducer.Unregister;
  TdxPScxCustomRadioGroupProducer.Unregister;
  TdxPScxCustomGroupBoxProducer.Unregister;
  TdxPScxCustomImageEditProducer.Unregister;
  TdxPScxCustomBlobEditProducer.Unregister;
  TdxPScxRadioButtonProducer.Unregister;
  TdxPScxCustomCheckBoxProducer.Unregister;
  TdxPScxCustomImageComboBoxProducer.Unregister;
  TdxPScxCustomComboBoxProducer.Unregister;
  TcxPScxCustomCurrencyEditProducer.Unregister;
  TcxPScxCustomHyperLinkEditProducer.Unregister;
  TdxPScxCustomMemoEditProducer.Unregister;
  TdxPScxCustomTextEditProducer.Unregister;
  //TdxPScxCustomEditProducer.Unregister;
  //TdxPScxContainerProducer.Unregister;
  //TdxPScxControlProducer.Unregister;
  TdxPSdxCustomCheckGroupBoxProducer.UnRegister;

  TdxPScxNativePrintableControlProducer.Unregister;
end;

initialization
  RegisterAssistants;

finalization
  UnregisterAssistants;
  FreeAndNil(FPicture);

end.

