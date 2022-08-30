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
{   EXECUTABLE PROGRAM ONLY.                                         }
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

unit dxPSGlbl;

interface

(*$HPPEMIT '#include <lmcons.h>' *)

{$I cxVer.inc}

uses
  Classes, Windows, Messages, SysUtils, Graphics, Menus, Dialogs, ExtDlgs, IniFiles,
  dxCore, dxMessages, cxControls, cxColorComboBox;

type
  TDWORDBits = 0..31;

  PBooleanArray = ^TBooleanArray;
  TBooleanArray = array[0..0] of Boolean;

  PIntArray = ^TIntArray;
  TIntArray = array[0..0] of Integer;

  TIntegers = array of Integer;
  TdxPSPageIndexes = TIntegers;

  TdxDriveType = (dtUnknown, dtNoRootDir, dtRemovable, dtFixed, dtRemote, dtCDROM, dtRamDisk);
  TdxPageOrder = (poOverThenDown, poDownThenOver);
  TdxPageNumberFormat = (pnfNumeral, pnfChars, pnfUpperChars, pnfRoman, pnfUpperRoman);
  TdxUpDownGlyph = (udgUp, udgDown);

  TdxPageNumbers = (pnAll, pnEven, pnOdd);
  TdxPageRanges = (prAll, prCurrent, prRange, prSelection);

  TdxPSPreviewDialogStyle = type string;

  TdxPSVersion = record
    Major: Integer;
    Minor: Integer;
    SubMinor: Integer;
  end;

const
  NullDC = 0;

  cMenuSeparator = Menus.cLineCaption;
  cPageRangeSeparator = '-';
  cPageSeparator = ',';

  WMPS_PRINTSTYLELISTCHANGED = WM_DX + 1;
  WMPS_PRINTERLISTCHANGED = WM_DX + 2;
  WMPS_INITIALIZEPRINTER = WM_DX + 3;

  GLYPH_UPARROW = 53;
  GLYPH_DOWNARROW = 54;

  APPCOMMAND_BROWSER_BACKWARD = 1;
  {$EXTERNALSYM APPCOMMAND_BROWSER_BACKWARD}
  APPCOMMAND_BROWSER_FORWARD  = 2;
  {$EXTERNALSYM APPCOMMAND_BROWSER_FORWARD}
  APPCOMMAND_BROWSER_HOME     = 7;
  {$EXTERNALSYM APPCOMMAND_BROWSER_HOME}
  APPCOMMAND_HELP             = 27;
  {$EXTERNALSYM APPCOMMAND_HELP}
  APPCOMMAND_FIND             = 28;
  {$EXTERNALSYM APPCOMMAND_FIND}
  APPCOMMAND_NEW              = 29;
  {$EXTERNALSYM APPCOMMAND_NEW}
  APPCOMMAND_OPEN             = 30;
  {$EXTERNALSYM APPCOMMAND_OPEN}
  APPCOMMAND_CLOSE            = 31;
  {$EXTERNALSYM APPCOMMAND_CLOSE}
  APPCOMMAND_SAVE             = 32;
  {$EXTERNALSYM APPCOMMAND_SAVE}
  APPCOMMAND_PRINT            = 33;
  {$EXTERNALSYM APPCOMMAND_PRINT}
  APPCOMMAND_UNDO             = 34;
  {$EXTERNALSYM APPCOMMAND_UNDO}
  APPCOMMAND_REDO             = 35;
  {$EXTERNALSYM APPCOMMAND_REDO}
  APPCOMMAND_COPY             = 36;
  {$EXTERNALSYM APPCOMMAND_COPY}
  APPCOMMAND_CUT              = 37;
  {$EXTERNALSYM APPCOMMAND_CUT}
  APPCOMMAND_PASTE            = 38;
  {$EXTERNALSYM APPCOMMAND_PASTE}

  FAPPCOMMAND_MOUSE           = $00008000;
  {$EXTERNALSYM FAPPCOMMAND_MOUSE}
  FAPPCOMMAND_KEY             = $00000000;
  {$EXTERNALSYM FAPPCOMMAND_KEY}
  FAPPCOMMAND_OEM             = $00001000;
  {$EXTERNALSYM FAPPCOMMAND_OEM}
  FAPPCOMMAND_MASK            = $0000F000;
  {$EXTERNALSYM FAPPCOMMAND_MASK}

  HDF_SORTDOWN = $0200;
  HDF_SORTUP = $0400;

 {$EXTERNALSYM HDF_SORTDOWN}
 {$EXTERNALSYM HDF_SORTUP}

  { Cell Sides }
  dxFormatLeftSide                                      = $00000001;
  dxFormatTopSide                                       = $00000002;
  dxFormatRightSide                                     = $00000004;
  dxFormatBottomSide                                    = $00000008;
  dxFormatRect                                          = $0000000F;
  { Horizontal Alignment }
  dxFormatTextAlignXOffset                              = $00000004;
  dxFormatTextAlignXLeft                                = $00000000;
  dxFormatTextAlignXCenter                              = $00000010;
  dxFormatTextAlignXRight                               = $00000020;
  dxFormatTextAlignXJustify                             = $00000030;
  dxFormatTextAlignXDistribute                          = $00000040;
  dxFormatTextAlignXMask                                =
    dxFormatTextAlignXLeft or dxFormatTextAlignXCenter or dxFormatTextAlignXRight or
    dxFormatTextAlignXJustify or dxFormatTextAlignXDistribute;
  { Vertical Alignment }
  dxFormatTextAlignYOffset                              = $00000007;
  dxFormatTextAlignYTop                                 = $00000000;
  dxFormatTextAlignYCenter                              = $00000080;
  dxFormatTextAlignYBottom                              = $00000100;
  dxFormatTextAlignYDistribute                          = $00000180;
  dxFormatTextAlignYMask                                =
    dxFormatTextAlignYTop or dxFormatTextAlignYCenter or
    dxFormatTextAlignYBottom or dxFormatTextAlignYDistribute;
  { Text Attributes }
  dxFormatEndEllipsis                                   = $00000200;
  dxFormatMultiline                                     = $00000400;
  dxFormatTransparent                                   = $00000800;
  dxFormatBreakByChars                                  = $40000000;
  dxFormatHidePrefix                                    = $00080000;
  { Checkmark Attributes }
  dxFormatCheckChecked                                  = $00001000;
  dxFormatCheckEnabled                                  = $00002000;
  dxFormatCheckButtonEdgeStyleOffset                    = $0000000E;
  dxFormatCheckEdgeStyleNone                            = $00000000;
  dxFormatCheckEdgeStyle3D                              = $00004000;
  dxFormatCheckEdgeStyleSoft3D                          = $00008000;
  dxFormatCheckEdgeStyleBoldFlat                        = $0000C000;
  dxFormatCheckEdgeStyleUltraFlat                       = $00010000;
  dxFormatCheckEdgeStyleSingle                          = $00014000;
  dxFormatCheckButtonEdgeStyleMask                      =
    dxFormatCheckEdgeStyle3D or dxFormatCheckEdgeStyleSoft3D or
    dxFormatCheckEdgeStyleBoldFlat or dxFormatCheckEdgeStyleUltraFlat or
    dxFormatCheckEdgeStyleSingle;
  dxFormatCheckFlat                                     = $00004000; // obsolete
  dxFormatCheckBold                                     = $00008000; // obsolete
  { Text Exceeds}
  dxFormatPreventLeftTextExceed                         = $00100000;
  dxFormatPreventTopTextExceed                          = $00200000;
  { Sort Order }
  dxFormatSortOrderOffset                               = $00000016;
  dxFormatSortNo                                        = $00000000;
  dxFormatSortUp                                        = $00400000;
  dxFormatSortDown                                      = $00800000;
  dxFormatSortOrderMask                                 = dxFormatSortUp or dxFormatSortDown;
  { Misc }
  dxFormatMakeSpaceForEmptyImage                        = $01000000;
  dxFormatImageTransparent                              = $02000000;
  dxFormatIsTextDrawnForCenteredImage                   = $10000000;
  dxFormatIsTextShiftedForHorizontallyCenteredImage     = $20000000;
  { Check Mark position }
  dxFormatCheckPosOffset                                = $0000001A;
  dxFormatCheckPosLeft                                  = $00000000;
  dxFormatCheckPosCenter                                = $04000000;
  dxFormatCheckPosRight                                 = $08000000;
  dxFormatCheckPosMask                                  = dxFormatCheckPosCenter or dxFormatCheckPosRight;
  dxFormatCheckGlyphCountOffset                         = $0000001C;
  dxFormatCheckGlyphCountMask                           = $70000000; // we use 3 bits for GlyphCount
  { Shadow }
  dxFormatShowShadow                                    = $00020000;
  { Cliping }
  dxFormatExcludeFromClipRgn                            = $00040000;
  dxFormatVisible                                       = $80000000;
  { ReportCells }
  dxFormatClipChildren                                  = $00080000;
  { ReportGroups }
  dxFormatOwnBorderClass                                = $00000200; // dxFormatEndEllipsis

  { Bits Overriding }
  { since we don't need some FormatBits for rendering several items we are allowed to safely override them }

  { Image Layout }
  { overriden bits ->> dxFormatCheckChecked, dxFormatCheckEnabled, dxFormatCheckEdgeStyleXXX }
  dxFormatImageLayoutOffset                             = $0000000C;
  dxFormatImageLayoutTopLeft                            = $00000000;
  dxFormatImageLayoutTopCenter                          = $00001000;
  dxFormatImageLayoutTopRight                           = $00002000;
  dxFormatImageLayoutCenterLeft                         = $00003000;
  dxFormatImageLayoutCenterCenter                       = $00004000;
  dxFormatImageLayoutCenterRight                        = $00005000;
  dxFormatImageLayoutBottomLeft                         = $00006000;
  dxFormatImageLayoutBottomCenter                       = $00007000;
  dxFormatImageLayoutBottomRight                        = $00008000;
  dxFormatImageLayoutMask                               =
    dxFormatImageLayoutTopLeft or dxFormatImageLayoutTopCenter or
    dxFormatImageLayoutTopRight or dxFormatImageLayoutCenterLeft or
    dxFormatImageLayoutCenterCenter or dxFormatImageLayoutCenterRight or
    dxFormatImageLayoutBottomLeft or dxFormatImageLayoutBottomCenter or
    dxFormatImageLayoutBottomRight;
  { Graphic DrawMode }
  { overriden bits ->> Text Horz. Alignment }
  dxFormatGraphicDrawModeOffset                         = $00000004;
  dxFormatGraphicDrawModeNone                           = $00000000;
  dxFormatGraphicDrawModeCenter                         = $00000010;
  dxFormatGraphicDrawModeStretch                        = $00000020;
  dxFormatGraphicDrawModeStretchProportional            = $00000030;
  dxFormatGraphicDrawModeCenterAndStretchProportional   = $00000040;
  dxFormatGraphicDrawModeMask                           =
    dxFormatGraphicDrawModeCenter or dxFormatGraphicDrawModeStretch or
    dxFormatGraphicDrawModeStretchProportional or dxFormatGraphicDrawModeCenterAndStretchProportional;
  { Graphic Buffering }
  { overriden bits ->> Text Vert. Alignment }
  dxFormatGraphicBufferingOffset                        = $00000007;
  dxFormatGraphicBufferingDefault                       = $00000000;
  dxFormatGraphicBufferingNone                          = $00000080;
  dxFormatGraphicBufferingAlways                        = $00000100;
  dxFormatGraphicBufferingMask                          =
    dxFormatGraphicBufferingNone or dxFormatGraphicBufferingAlways;
  { Expand Button }
  { overriden bits ->> Text Alignment (Horz and Vert), dxFormatEndEllipsis, dxFormatMultiline and dxFormatCheckChecked }
  dxFormatExpandButtonBorder3D                          = $00000010;
  dxFormatExpandButtonBorder3DSoft                      = $00000020;
  dxFormatExpandButtonBorderShadow                      = $00000040;
  dxFormatExpandButtonExpanded                          = $00000080;
  dxFormatExpandButtonKeepOddSize                       = $00000100;
  dxFormatExpandButtonTransparent                       = $00000200;
  dxFormatExpandButtonShowBorder                        = $00000400;
  dxFormatExpandButtonVisible                           = $00001000;
  { TreeLines }
  { overriden bits ->> dxFormatSortXXX and dxFormatMakeSpaceForEmptyImage }
  dxFormatTreeLineModeOffset                            = $00000016;
  dxFormatTreeLineModeNone                              = $00000000;
  dxFormatTreeLineModeVertical                          = $00400000;
  dxFormatTreeLineModeCross                             = $00800000;
  dxFormatTreeLineModeTopRightCorner                    = $00C00000;
  dxFormatTreeLineModeBottomRightCorner                 = $01000000;
  dxFormatTreeLineModeMask                              =
    dxFormatTreeLineModeVertical or dxFormatTreeLineModeCross or
    dxFormatTreeLineModeTopRightCorner or dxFormatTreeLineModeBottomRightCorner;
  { ExpandButtonEx -> FormatEx codes }
  dxFormatExButtonAlignHorzOffset                       = $00000000;
  dxFormatExButtonAlignHorzLeft                         = $00000000;
  dxFormatExButtonAlignHorzCenter                       = $00000001;
  dxFormatExButtonAlignHorzRight                        = $00000002;
  dxFormatExButtonAlignHorzMask                         =
    dxFormatExButtonAlignHorzCenter or dxFormatExButtonAlignHorzRight;
  dxFormatExButtonAlignVertOffset                       = $00000002;
  dxFormatExButtonAlignVertTop                          = $00000000;
  dxFormatExButtonAlignVertCenter                       = $00000004;
  dxFormatExButtonAlignVertBottom                       = $00000008;
  dxFormatExButtonAlignVertMask                         =
    dxFormatExButtonAlignVertCenter or dxFormatExButtonAlignVertBottom;
  { Shapes }
  { overriden bits ->> dxFormatImageLayoutXXX }
  dxFormatShapeTransparent                              = $10000000;
  { Report groups }
  { overriden bits ->> dxFormatImageLayoutXXX }
  dxFormatGroupShowCaption                              = $10000000;
  dxFormatGroupCaptionTransparent                       = $20000000;

  sdxPSRegPath = 'Software\Developer Express\PrintingSystem';                                  // Don't localize
  sdxPSRegPathCustomColors = 'CustomColors';                                                  // Don't localize
  sdxPSRegPathDesignTime = sdxPSRegPath + '\DesignTime';                                       // Don't localize
  sdxPSRegPathRunTime = sdxPSRegPath + '\RunTime';                                             // Don't localize
  sdxPSRegPathRunTimeFormLayouts = '\Developer Express\PrintingSystem\FormLayouts';            // Don't localize

  dxDefaultMinPrintableArea: Integer = 50000;                                                  // In thousandths of mm
  dxDefaultInitialMargins: TRect = (Left: 12700; Top: 12700; Right: 12700; Bottom: 12700);     // In thousandths of mm
  dxDefaultPhysicalPaperOffsets: TPoint = (X: 6350; Y: 6350);                                  // In thousandths of mm
  dxDefaultInitialHeader: Integer = 6350;                                                      // In thousandths of mm
  dxDefaultInitialFooter: Integer = 6350;                                                      // In thousandths of mm

  dxStyleGlyphSize: TPoint = (X: 32; Y: 32);

  { HelpContext constants }

  { DesignTime }
  dxhcAddEmptyLinkDlg: THelpContext = 0;
  dxhcAddLinkDlg: THelpContext = 0;
  dxhcAddStyleDlg: THelpContext = 0;
  dxhcEditDescriptionDlg: THelpContext = 0;
  dxhcPrintStylesDesignWindow: THelpContext = 0;
  dxhcReportLinkDesignWindow: THelpContext = 0;

  { DesignTime - RunTime }
  dxhcCompositionReportLinkDesigner: THelpContext = 0;
  dxhcStringGridReportLinkDesigner: THelpContext = 0;
  dxhcDrawGridReportLinkDesigner: THelpContext = 0;
  dxhcListBoxReportLinkDesigner: THelpContext = 0;
  dxhcCheckListBoxReportLinkDesigner: THelpContext = 0;
  dxhcListViewReportLinkDesigner: THelpContext = 0;
  dxhcTextReportLinkDesigner: THelpContext = 0;
  dxhcTreeViewReportLinkDesigner: THelpContext = 0;
  dxhcTreeListReportLinkDesigner: THelpContext = 0;
  dxhcOrgChartReportLinkDesigner: THelpContext = 0;
  dxhcFlowChartReportLinkDesigner: THelpContext = 0;
  dxhcInspectorGridReportLinkDesigner: THelpContext = 0;
  dxhcMasterViewReportLinkDesigner: THelpContext = 0;
  dxhccxMCListBoxReportLinkDesigner: THelpContext = 0;
  dxhccxSpreadSheetReportLinkDesigner: THelpContext = 0;
  dxhccxGridReportLinkDesigner: THelpContext = 0;
  dxhccxTreeListReportLinkDesigner: THelpContext = 0;
  dxhccxVerticalGridReportLinkDesigner: THelpContext = 0;
  dxhccxSchedulerReportLinkDesigner: THelpContext = 0;
  dxhccxPivotGridReportLinkDesigner: THelpContext = 0;

  dxhcAddComponentsToComponentPrinterDlg: THelpContext = 0;
  dxhcAddItemsToCompositionDlg: THelpContext = 0;
  dxhcAutoHFTextEntriesDlg: THelpContext = 0;
  dxhcDateTimeFormatDlg: THelpContext = 0;
  dxhcDefinePrintStyleDlg: THelpContext = 0;
  dxhcFEFDlg: THelpContext = 0;
  dxhcFootnotesPropertiesDlg: THelpContext = 0;
  dxhcPageNumberFormatDlg: THelpContext = 0;
  dxhcPageSetupDlg: THelpContext = 0;
  dxhcPreviewPreferencesDlg: THelpContext = 0;
  dxhcPrintDlg: THelpContext = 0;
  dxhcPrintSetupDlg: THelpContext = 0;
  dxhcPSReportPropertiesDlg: THelpContext = 0;
  dxhcSelectComponentsDlg: THelpContext = 0;
  dxhcTitlePropertiesDlg: THelpContext = 0;
  dxhcZoomDlg: THelpContext = 0;

  dxPSVersion: TdxPSVersion = (Major: 0; Minor: 0; SubMinor: 0);
  dxPSStorageVersion: Integer = 2;
  dxPSInvalidStorageVersion: Integer = -1;

  sdxHelpButtonName = 'btnHelp';                 // Don't localize

var
  AskPrinterForSupportedFonts: Boolean = False;

  IsComCtrlVersion471: Boolean;
  IsComCtrlVersion500: Boolean;
  IsComCtrlVersion600: Boolean;

  IsDesignTime: Boolean = False;

  CheckHeight: Integer;
  CheckWidth: Integer;

  PathDelimiter: Char;
  PercentSymbol:  Char = '%' ; //TODO: move up

  PSCanShowHintFunc: function: Boolean = nil;

function CanShowHints: Boolean;
function GetPSRegRoot: string;
function dxPSCheckVersion(const AVersion: TdxPSVersion; AMajor, AMinor: Integer): Boolean;
function dxPSVer: Integer;
function dxPSVerMajor: Integer;
function dxPSVerMinor: Integer;
function dxPSVersionString: string;
function dxPSVerSubMinor: Integer;

function ColorDialog: TcxColorDialogHelper;
function FontDialog: TFontDialog;
function FontDialogDevice: TFontDialogDevice;

function StringToPSVersion(AVersion: string; const ASeparator: string = '.'): TdxPSVersion;
function ComparePSVersions(V1, V2: TdxPSVersion): Integer;

function GET_APPCOMMAND_LPARAM(lParam: LPARAM): Integer;
{$EXTERNALSYM GET_APPCOMMAND_LPARAM}
function GET_DEVICE_LPARAM(lParam: LPARAM): Integer;
{$EXTERNALSYM GET_DEVICE_LPARAM}
function GET_MOUSEORKEY_LPARAM(lParam: LPARAM): Integer;
{$EXTERNALSYM GET_MOUSEORKEY_LPARAM}
function GET_FLAGS_LPARAM(lParam: LPARAM): Integer;
{$EXTERNALSYM GET_FLAGS_LPARAM}
function GET_KEYSTATE_LPARAM(lParam: LPARAM): Integer;
{$EXTERNALSYM GET_KEYSTATE_LPARAM}

implementation

uses
  Registry, ComCtrls, dxPSUtl, dxPSEngn, dxPrnDev, cxLookAndFeels;


type

  { TdxPSColorDialogHelper }

  TdxPSColorDialogHelper = class(TcxColorDialogHelper)
  protected
    procedure RestoreCustomColors;
    procedure SaveCustomColors;
    function GetLookAndFeel: TcxLookAndFeel; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

var
  FColorDialog: TcxColorDialogHelper;
  FFontDialog: TFontDialog;

function CanShowHints: Boolean;
begin
  Result := not Assigned(PSCanShowHintFunc) or PSCanShowHintFunc;
end;

function GetPSRegRoot: string;
const
  Paths: array[Boolean] of string = (sdxPSRegPathRunTime, sdxPSRegPathDesignTime);
begin
  Result := Paths[IsDesignTime];
end;

function dxPSCheckVersion(const AVersion: TdxPSVersion; AMajor, AMinor: Integer): Boolean;
begin
  Result := (AVersion.Major > AMajor) or (AVersion.Major = AMajor) and (AVersion.Minor >= AMinor);
end;

function dxPSVer: Integer;
begin
  Result := dxPSVersion.Major;
end;

function dxPSVerMajor: Integer;
begin
  Result := dxPSVersion.Major;
end;

function dxPSVerMinor: Integer;
begin
  Result := dxPSVersion.Minor;
end;

function dxPSVerSubMinor: Integer;
begin
  Result := dxPSVersion.SubMinor;
end;

function dxPSVersionString: string;
begin
  Result := '4.' + IntToStr(dxBuildNumber);
end;

function ColorDialog: TcxColorDialogHelper;
begin
  if FColorDialog = nil then
    FColorDialog := TdxPSColorDialogHelper.Create;
  Result := FColorDialog;
end;

{ TdxPSColorDialog }

constructor TdxPSColorDialogHelper.Create;
begin
  inherited Create;
  DialogShowFull := True;
  DialogType := cxcdtAdvanced;
  RestoreCustomColors;
end;

destructor TdxPSColorDialogHelper.Destroy;
begin
  SaveCustomColors;
  inherited Destroy;
end;

function TdxPSColorDialogHelper.GetLookAndFeel: TcxLookAndFeel;
begin
  Result := dxPSEngine.DialogsLookAndFeel;
end;

procedure TdxPSColorDialogHelper.RestoreCustomColors;
var
  AIniFile: TCustomIniFile;
  AStrings: TStringList;
  I: Integer;
begin
  CustomColors.Clear;
  if dxPSStoringManager.BeginStoring(AIniFile) then
  try
    AStrings := TStringList.Create;
    try
      AIniFile.ReadSection(sdxPSRegPathCustomColors, AStrings);
      for I := 0 to AStrings.Count - 1 do
        CustomColors.Add(AStrings[I] + '=' + AIniFile.ReadString(sdxPSRegPathCustomColors, AStrings[I], ''));
    finally
      AStrings.Free;
    end;
  finally
    dxPSStoringManager.EndStoring(AIniFile);
  end;
end;

procedure TdxPSColorDialogHelper.SaveCustomColors;
var
  AIniFile: TCustomIniFile;
  I: Integer;
begin
  if dxPSStoringManager.BeginStoring(AIniFile) then
  try
    AIniFile.EraseSection(sdxPSRegPathCustomColors);
    for I := 0 to CustomColors.Count - 1 do
      AIniFile.WriteString(sdxPSRegPathCustomColors, CustomColors.Names[I], CustomColors.ValueFromIndex[I]);
  finally
    dxPSStoringManager.EndStoring(AIniFile);
  end;
end;

{ TdxPSFontDialog }

type
  TdxPSFontDialog = class(TFontDialog)
  public
    constructor Create(AOwner: TComponent); override;
    function Execute: Boolean; override;
  end;

constructor TdxPSFontDialog.Create(AOwner: TComponent);
begin
  inherited;
  Options := [fdEffects, fdScalableOnly, fdTrueTypeOnly];
end;

function TdxPSFontDialog.Execute: Boolean;
begin
  Device := FontDialogDevice;
  Result := inherited Execute;
end;

function FontDialog: TFontDialog;
begin
  if FFontDialog = nil then
    FFontDialog := TdxPSFontDialog.Create(nil);
  Result := FFontDialog;
end;

function FontDialogDevice: TFontDialogDevice;
begin
  if AskPrinterForSupportedFonts and dxPrnDev.dxInitPrintDevice(False) then
    Result := fdPrinter
  else
    Result := fdScreen;
end;

function GET_APPCOMMAND_LPARAM(lParam: LPARAM): Integer;
begin
  Result := Shortint(HiWord(lParam) and not FAPPCOMMAND_MASK);
end;

function GET_DEVICE_LPARAM(lParam: LPARAM): Integer;
begin
  Result := Word(HiWord(lParam) and FAPPCOMMAND_MASK);
end;

function GET_MOUSEORKEY_LPARAM(lParam: LPARAM): Integer;
begin
  Result := GET_DEVICE_LPARAM(lParam);
end;

function GET_FLAGS_LPARAM(lParam: LPARAM): Integer;
begin
  Result := LoWord(lParam);
end;

function GET_KEYSTATE_LPARAM(lParam: LPARAM): Integer;
begin
  Result := GET_FLAGS_LPARAM(lParam);
end;

function StringToPSVersion(AVersion: string; const ASeparator: string = '.'): TdxPSVersion;

  function GetVersionFromPos(var AVersion: string): Integer;
  var
    ACode: Integer;
    ANextPos: Integer;
  begin
    ANextPos := Pos(ASeparator, AVersion);
    if (ANextPos = 0) then
      ANextPos := Length(AVersion) + 1;
    Val(Copy(AVersion, 1, ANextPos - 1), Result, ACode);
    AVersion := Copy(AVersion, ANextPos + 1, MaxInt);
  end;

begin
  Result.Major := GetVersionFromPos(AVersion);
  Result.Minor := GetVersionFromPos(AVersion);
  Result.SubMinor := GetVersionFromPos(AVersion);
end;

function ComparePSVersions(V1, V2: TdxPSVersion): Integer;
begin
  Result := dxCompareValues(V1.Major, V2.Major);
  if Result = 0 then
  begin
    Result := dxCompareValues(V1.Minor, V2.Minor);
    if Result = 0 then
      Result := dxCompareValues(V1.SubMinor, V2.SubMinor);
  end;
end;

procedure InitVersionInfo;
begin
  dxPSVersion := StringToPSVersion(dxPSVersionString);
end;

procedure InitComCtlrs;
begin
  IsComCtrlVersion471 := ComCtrls.GetComCtlVersion >= ComCtlVersionIE401;
  IsComCtrlVersion500 := ComCtrls.GetComCtlVersion >= ComCtlVersionIE5;
  IsComCtrlVersion600 := ComCtrls.GetComCtlVersion >= ComCtlVersionIE6;
end;

procedure InitGlobalConsts;
begin
  with TBitmap.Create do
  try
    Handle := LoadBitmap(0, PChar(OBM_CHECKBOXES));

    CheckWidth := Width div 4;
    CheckHeight := Height div 3;
  finally
    Free;
  end;
  PathDelimiter := SysUtils.PathDelim;
end;

initialization
  InitComCtlrs;
  InitGlobalConsts;
  InitVersionInfo;

finalization
  FreeAndNil(FColorDialog);
  FreeAndNil(FFontDialog);

end.
