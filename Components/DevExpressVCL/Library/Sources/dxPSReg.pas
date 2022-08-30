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

unit dxPSReg;

interface

{$I cxVer.inc}

uses
  DesignIntf, DesignEditors, PicEdit, Classes, dxPSCore, dxPSPrVw;

const
  dxPSProductName = 'ExpressPrinting System';
  dxPSProductPage = 'ExpressPrinting System'; // Don't localize
  dxPSAutoFillReportLinkUnits = True;

type

  { TdxReportLinkComponentPropertyEditor }

  TdxReportLinkComponentPropertyEditor = class(TComponentProperty)
  private
    FComponents: TStringList;
    function GetReportLink: TBasedxReportLink;
  protected
    procedure AddComponent(const AName: string);
    procedure GetComponents; virtual;
    function IsComponentValid(AComponent: TComponent): Boolean; virtual;
  public
    destructor Destroy; override;
    procedure Initialize; override;
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    property ReportLink: TBasedxReportLink read GetReportLink;
  end;

procedure Register;
procedure dxPSRegisterReportLinkUnit(const AUnitName: string; ALinkClass: TdxReportLinkClass);

implementation

{$R dxPSDsgImgs.res}

uses
  DesignConst, DesignMenus, VCLEditors, RTLConsts, ToolsAPI,
  Windows, SysUtils, FileCtrl, Graphics, Dialogs, Controls, ExtCtrls,
  TabNotBk, ImgList, Forms, TypInfo,

  dxCoreReg, cxDrawTextUtils, cxPC,
  dxRegEd, dxPSGlbl, dxPSUtl, dxWrap, dxBase,
  dxPSEngn, dxPreVw, dxPrnPg, dxBkgnd, dxPrnDev, dxPgsDlg, dxPrnDlg, dxPPAttr,
  dxPSfmTtl, dxPSFileBasedXplorer, dxPSGraphicLnk, dxPSContainerLnk, dxPSTextLnk,
  dxBrhDlg, dxPSfmEditDesc, dxPSPrVwDsg, dxPSfmStlAdd, dxPSfmLnkDsg, dxPSfmLnkAdd,
  dxPSfmLnkAddE, dxPSDsgProxies, dxPSfmStlDsg, dxPSPDFExportDialog, cxControls,
  cxLibraryReg;

const
  sdxDefault = 'Default';
  sdxRestoreDefaultIcon = 'Type "default" to restore original Icon';
  sdxRestoreDefaultGlyph = 'Type "default" to restore original Glyph';
  sdxCreateNewStyleManager = '<Create a new StyleManager>';

  sdxReportLinks = 'ReportLinks...';
  sdxPrintStyles = 'PrintStyles...';
  sdxAddNewStyle = '&Add...';
  sdxAddNewLink = '&Add...';
  sdxAddNewEmtpyLink = 'Add E&mpty...';
  sdxPageBackground = 'Page Bac&kground...';
  sdxClearBackground = 'Clea&r Background';
  sdxRestoreDefaults = 'Rest&ore Defaults';
  sdxRestoreOriginal = 'Restore Or&iginal';
  sdxMakeCurrent = '&Make Current';
  sdxPageSetupDialog = 'PageSetup Dialog...';
  sdxPageSetup = 'Page Set&up...';
  sdxPrintDialog = 'Print Dialog...';
  sdxPrint = '&Print...';
  sdxPrintPreview = 'Print Pre&view...';
  sdxShowReportDesigner = 'Show D&esigner...';
  sdxCreateFolders = 'Create &Folders';
  sdxDesignerNoAvailable = 'Designer not available';
  sdxClickForPageSetup = 'Click for Page Setup...';
  sdxCustomPaperSize = 'CustomSize - DMPAPER_USER #';
  sdxCustomBin = 'CustomBin - DMBIN_USER #';
  sdxInThousandths = '(in thousandths)';

  IDB_DXPSDESIGN_BIN_AUTOTRAY = 'IDB_DXPSDESIGN_BIN_AUTOTRAY';
  IDB_DXPSDESIGN_BIN_MANUALTRAY = 'IDB_DXPSDESIGN_BIN_MANUALTRAY';
  IDB_DXPSDESIGN_PAPERORIENTATION_LANDSCAPE = 'IDB_DXPSDESIGN_PAPERORIENTATION_LANDSCAPE';
  IDB_DXPSDESIGN_PAPERORIENTATION_PORTRAIT = 'IDB_DXPSDESIGN_PAPERORIENTATION_PORTRAIT';
  IDB_DXPSDESIGN_PAPER_ENVELOPE = 'IDB_DXPSDESIGN_PAPER_ENVELOPE';
  IDB_DXPSDESIGN_PAPER_STANDARD = 'IDB_DXPSDESIGN_PAPER_STANDARD';
  IDB_DXPSDESIGN_PRINTORDER_OVERTHENDOWN = 'IDB_DXPSDESIGN_PRINTORDER_OVERTHENDOWN';
  IDB_DXPSDESIGN_PRINTORDER_DOWNTHENOVER = 'IDB_DXPSDESIGN_PRINTORDER_DOWNTHENOVER';

  IDB_DXPSDESIGN_DOADD = 'IDB_DXPSDESIGN_DOADD';
  IDB_DXPSDESIGN_DOADDEMPTY = 'IDB_DXPSDESIGN_DOADDEMPTY';
  IDB_DXPSDESIGN_DODESIGNER = 'IDB_DXPSDESIGN_DODESIGNER';
  IDB_DXPSDESIGN_DOPAGEBACKGROUND = 'IDB_DXPSDESIGN_DOPAGEBACKGROUND';
  IDB_DXPSDESIGN_DOPAGESETUP = 'IDB_DXPSDESIGN_DOPAGESETUP';
  IDB_DXPSDESIGN_DOPREVIEW = 'IDB_DXPSDESIGN_DOPREVIEW';
  IDB_DXPSDESIGN_DOPRINT = 'IDB_DXPSDESIGN_DOPRINT';
  IDB_DXPSDESIGN_DOPROPERTIES = 'IDB_DXPSDESIGN_DOPROPERTIES';
  IDB_DXPSDESIGN_DOWEB = 'IDB_DXPSDESIGN_DOWEB';

var
  ilPapers: TImageList;
  ilBins: TImageList;
  ilPrintOrders: TImageList;
  ilPaperOrientations: TImageList;

type
  TdxCustomPrinterEditor = class(TdxComponentEditor)
  protected
    function GetProductName: string; override;
  end;

  { TdxComponentPrinterEditor }

  TdxComponentPrinterEditor = class(TdxCustomPrinterEditor)
  protected
    function InternalGetVerb(AIndex: Integer): string; override;
    function InternalGetVerbCount: Integer; override;
    procedure InternalExecuteVerb(AIndex: Integer); override;
  end;

  { TdxReportLinkEditor }

  TdxReportLinkEditor = class(TdxCustomPrinterEditor)
  private
    function ReportLink: TBasedxReportLink;
  protected
    function InternalGetVerb(AIndex: Integer): string; override;
    function InternalGetVerbCount: Integer; override;
    procedure InternalExecuteVerb(AIndex: Integer); override;
  public
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem); override;
  end;

  { TdxPrintStyleManagerEditor }

  TdxPrintStyleManagerEditor = class(TdxCustomPrinterEditor)
  protected
    function InternalGetVerb(AIndex: Integer): string; override;
    function InternalGetVerbCount: Integer; override;
    procedure InternalExecuteVerb(AIndex: Integer); override;
  end;

  { TdxPrintStyleEditor }

  TdxPrintStyleEditor = class(TdxCustomPrinterEditor)
  private
    function PrintStyle: TBasedxPrintStyle;
  protected
    function InternalGetVerb(AIndex: Integer): string; override;
    function InternalGetVerbCount: Integer; override;
    procedure InternalExecuteVerb(AIndex: Integer); override;
  public
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem); override;
  end;

  { TdxPageSetupDialogEditor }

  TdxPageSetupDialogEditor = class(TdxCustomPrinterEditor)
  protected
    function InternalGetVerb(AIndex: Integer): string; override;
    function InternalGetVerbCount: Integer; override;
    procedure InternalExecuteVerb(AIndex: Integer); override;
  public
    procedure PrepareItem(Index: Integer; const AItem: IMenuItem); override;
  end;

  { TdxPrintDialogEditor }

  TdxPrintDialogEditor = class(TdxCustomPrinterEditor)
  protected
    function InternalGetVerb(AIndex: Integer): string; override;
    function InternalGetVerbCount: Integer; override;
    procedure InternalExecuteVerb(AIndex: Integer); override;
  end;

  { TdxCompositionReportLinkPropertyEditor }

  TdxCompositionReportLinkPropertyEditor = class(TComponentProperty)
  private
    FComponents: TStringList;
    function GetItems: TdxCompositionLinkItems;
    procedure CheckProc(const S: string);
    function IsComponentValid(AComponent: TComponent): Boolean;
  protected
  public
    destructor Destroy; override;
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure Initialize; override;
    property Items: TdxCompositionLinkItems read GetItems;
  end;

  { TdxIndexPropertyEditor }

  TdxIndexPropertyEditor = class(TIntegerProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
  end;

  { TdxReportLinkDesignerPropertyEditor }

  TdxReportLinkDesignerPropertyEditor = class(TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure Edit; override;
  end;

  { TdxPSExplorerFilterPropertyEditor }

  TdxPSExplorerFilterPropertyEditor = class(TStringProperty, ICustomPropertyListDrawing)
  protected
    function GetImageIndex(const Value: string): Integer;
    function GetImageList: TCustomImageList;
    function IsAcceptableComponentClass(AComponentClass: TClass): Boolean; virtual;
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas; var AWidth: Integer);
    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas; var AHeight: Integer);
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
    //
    property ImageIndexes[const Value: string]: Integer read GetImageIndex;
    property ImageList: TCustomImageList read GetImageList;
  end;

 { TdxPrintStyleImageIndexPropertyEditor }

  TdxPrintStyleImageIndexPropertyEditor = class(TIntegerProperty,
    ICustomPropertyDrawing, ICustomPropertyListDrawing)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas; var AHeight: Integer);
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas; var AWidth: Integer);
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
    { ICustomPropertyDrawing }
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
  end;

  { TdxCustomDrawListPropertyEditor }

  TdxCustomDrawListPropertyEditor = class(TEnumProperty, ICustomPropertyListDrawing)
  protected
    function GetImageIndex(const Value: string): Integer; virtual; abstract;
    function GetImageList: TImageList; virtual; abstract;
  public
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas; var AWidth: Integer);
    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas; var AHeight: Integer);
    procedure ListDrawValue(const Value: string;
      ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);

    property ImageIndexes[const Value: string]: Integer read GetImageIndex;
    property ImageList: TImageList read GetImageList;
  end;

  { TdxPaperOrientationPropertyEditor }

  TdxPaperOrientationPropertyEditor = class(TdxCustomDrawListPropertyEditor)
  protected
    function GetImageIndex(const Value: string): Integer; override;
    function GetImageList: TImageList; override;
  end;

  { TdxPrintOrderPropertyEditor }

  TdxPrintOrderPropertyEditor = class(TdxCustomDrawListPropertyEditor)
  protected
    function GetImageIndex(const Value: string): Integer; override;
    function GetImageList: TImageList; override;
  end;

  { TdxPSPreviewWindowProperty }

  TdxPSPreviewWindowProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  { TdxPathPropertyEditor }

  TdxPathPropertyEditor = class(TStringProperty)
  private
    function GetExplorer(Index: Integer): TdxPSFileBasedExplorer;
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    property Explorers[Index: Integer]: TdxPSFileBasedExplorer read GetExplorer;
  end;

  { TdxRegistryPathPropertyEditor }

  TdxRegistryPathPropertyEditor = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  { TdxExplorerStubLinkPropertyEditor }

  TdxExplorerStubLinkPropertyEditor = class(TComponentProperty)
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  { TdxCustomContainerReportLinkComponentPropertyEditor }

  TdxCustomContainerReportLinkComponentPropertyEditor = class(
    TdxReportLinkComponentPropertyEditor, ICustomPropertyListDrawing)
  private
    function GetImageIndex(const Value: string): Integer;
    function GetImages: TCustomImageList;
  protected
    procedure GetComponents; override;
    function GetProject: IOTAProject;
    function IsComponentValid(AComponent: TComponent): Boolean; override;
    function IsSupportedModuleType(const AModuleType: TOTAModuleType): Boolean;
  public
    procedure Initialize; override;
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas; var AWidth: Integer);
    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas; var AHeight: Integer);
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);

    property ImageIndexes[const Value: string]: Integer read GetImageIndex;
    property Images: TCustomImageList read GetImages;
  end;

  { TdxMeasurementUnitsPropertyEditor }

  TdxMeasurementUnitsPropertyEditor = class(TEnumProperty)
  public
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
    function GetValue: string; override;
  end;

  { TdxPDFOptionsPropertyEditor }

  TdxPDFOptionsPropertyEditor = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  { TdxPrinterPagePropertyEditor }

  TdxPrinterPagePropertyEditor = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  { TdxReportTitlePropertyEditor }

  TdxReportTitlePropertyEditor = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  { TdxReportFootnotesPropertyEditor }

  TdxReportFootnotesPropertyEditor = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  { TdxReportTitleTextAlignXPropertyEditor }

  TdxReportTitleTextAlignXPropertyEditor = class(TEnumProperty)
  public
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  { TdxReportTitleTextAlignYPropertyEditor }

  TdxReportTitleTextAlignYPropertyEditor = class(TEnumProperty)
  public
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  { TdxHeaderFooterTextAlignYPropertyEditor }

  TdxHeaderFooterTextAlignYPropertyEditor = class(TEnumProperty)
  public
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  { TdxPrinterPagePropertyEditor2 }

  TdxPrinterPagePropertyEditor2 = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  { TdxPointWrapperEditor }

  TdxPointWrapperEditor = class(TClassProperty)
  private
    function PointWrapper: TdxPointWrapper;
  public
    function GetValue: string; override;
  end;

  { TdxRectWrapperEditor }

  TdxRectWrapperEditor = class(TClassProperty)
  private
    function RectWrapper: TdxRectWrapper;
  public
    function GetValue: string; override;
  end;

  { TdxBackgroundPropertyEditor }

  TdxBackgroundPropertyEditor = class(TClassProperty)
  private
    function GetBackground: TdxBackground;
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    property Background: TdxBackground read GetBackground;
  end;

  { TdxBrushPropertyEditor }

  TdxBrushPropertyEditor = class(TClassProperty)
  private
    function GetBrush: TBrush;
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    property Brush: TBrush read GetBrush;
  end;

  { TTTFontPropertyEditor }

  TTTFontPropertyEditor = class(TFontProperty)
  public
    procedure Edit; override;
  end;

  { TdxShowPgsDlgPropertyEditor }

  TdxShowPgsDlgPropertyEditor = class(TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure Edit; override;
  end;

  { TdxStyleGlyphPropertyEditor }

  TdxStyleGlyphPropertyEditor = class(TGraphicProperty)
  private
    function GetPrintStyle: TBasedxPrintStyle;
  public
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
    property PrintStyle: TBasedxPrintStyle read GetPrintStyle;
  end;

  { TdxCustomListDrawingIntegerPropertyEditor }

  TdxCustomListDrawingIntegerPropertyEditor = class(TIntegerProperty, ICustomPropertyListDrawing)
  protected
    function GetImageIndex(const Value: string): Integer; virtual; abstract;
    function GetImageList: TImageList; virtual; abstract;
  public
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas; var AWidth: Integer);
    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas; var AHeight: Integer);
    procedure ListDrawValue(const Value: string;
      ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);

    property ImageIndexes[const Value: string]: Integer read GetImageIndex;
    property ImageList: TImageList read GetImageList;
  end;

  { TdxDMPaperPropertyEditor }

  TdxDMPaperPropertyEditor = class(TdxCustomListDrawingIntegerPropertyEditor)
  protected
    function GetImageIndex(const Value: string): Integer; override;
    function GetImageList: TImageList; override;
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  { TdxPaperSourcePropertyEditor }

  TdxPaperSourcePropertyEditor = class(TdxCustomListDrawingIntegerPropertyEditor)
  protected
    function GetImageIndex(const Value: string): Integer; override;
    function GetImageList: TImageList; override;
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  { TdxPageNumberFormatsPropertyEditor }

  TdxPageNumberFormatsPropertyEditor = class(TIntegerProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  { TdxReportLinkStyleManagerPropertyEditor }

  TdxReportLinkStyleManagerPropertyEditor = class(TComponentProperty)
  private
   function GetRoot: TComponent;
   function GetUniqueName: string;
  protected
    function CanCreateStyleManager: Boolean;
    function CreateNewStyleManager: TdxPrintStyleManager;
    property Root: TComponent read GetRoot;
    property UniqueName: string read GetUniqueName;
  public
    function AutoFill: Boolean; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  { TdxReportDocumentDescriptionPropertyEditor }

  TdxReportDocumentDescriptionPropertyEditor = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  { TdxDateFormatsPropertyEditor }

  TdxDateFormatsPropertyEditor = class(TIntegerProperty)
  private
    FStrings: TStrings;
  protected
  public
    destructor Destroy; override;
    procedure Initialize; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  { TdxTimeFormatsPropertyEditor }

  TdxTimeFormatsPropertyEditor = class(TIntegerProperty)
  private
    FStrings: TStrings;
  protected
  public
    destructor Destroy; override;
    procedure Initialize; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  { TdxAutoHFTextEntriesPropertyEditor }

  TdxAutoHFTextEntriesPropertyEditor = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  { TdxPreviewIconPropertyEditor }

  TdxPreviewIconPropertyEditor = class(TGraphicProperty)
  private
    function GetPreviewOptions: TdxPreviewOptions;
  public
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
    property PreviewOptions: TdxPreviewOptions read GetPreviewOptions;
  end;

  { TdxTextReportLinkAlignmentPropertyEditor }

  TdxTextReportLinkAlignmentPropertyEditor = class(TEnumProperty)
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  { TdxComponentPrinterSelectionEditor }

  TdxComponentPrinterSelectionEditor = class(TSelectionEditor)
  protected
    procedure ProcessContainer(AContainer: TWinControl; Proc: TGetStrProc);
  public
    procedure PopulatePreviewDialogsUnits(Proc: TGetStrProc);
    procedure PopulateReportLinksUnits(Proc: TGetStrProc); overload;
    procedure PopulateReportLinksUnits(Proc: TGetStrProc; AComponentPrinter: TCustomdxComponentPrinter); overload;
    procedure PopulateReportLinkUnits(Proc: TGetStrProc; ALinkClass: TdxReportLinkClass); overload;
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  { TdxPrintStyleManagerSelectionEditor }

  TdxPrintStyleManagerSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  { TdxPageSetupDialogSelectionEditor }

  TdxPageSetupDialogSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  { TdxPrintDialogSelectionEditor }

  TdxPrintDialogSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

var
  ReportLinkUnitNameList: TStringList;

function TdxCustomPrinterEditor.GetProductName: string;
begin
  Result := dxPSProductName;
end;

{ TdxComponentPrinterEditor }
function TdxComponentPrinterEditor.InternalGetVerb(AIndex: Integer): string;
begin
  Result := sdxReportLinks;
end;

function TdxComponentPrinterEditor.InternalGetVerbCount: Integer;
begin
  Result := 1;
end;

procedure TdxComponentPrinterEditor.InternalExecuteVerb(AIndex: Integer);
begin
  dxShowReportLinkDesigner(TCustomdxComponentPrinter(Component), Designer);
end;

{ TdxReportLinkEditor }

function TdxReportLinkEditor.ReportLink: TBasedxReportLink;
begin
  Result := TBasedxReportLink(Component);
end;

procedure TdxReportLinkEditor.InternalExecuteVerb(AIndex: Integer);
begin
  case AIndex of
    0:
      if ReportLink.ComponentPrinter.DesignReport(ReportLink) then Designer.Modified;
    1:
      begin
        ReportLink.IsCurrentLink := True;
        Designer.Modified;
      end;
    3:
      begin
        ReportLink.RestoreFromOriginal;
        Designer.Modified;
      end;
    4:
      begin
        ReportLink.RestoreDefaults;
        ReportLink.PrinterPage.RestoreDefaults;
        Designer.Modified;
      end;
    6:
      begin
        if ReportLink.PrinterPage.Background.SetupEffects then
          Designer.Modified;
      end;
    7:
      begin
        ReportLink.PrinterPage.Background.Mode := bmNone;
        ReportLink.PrinterPage.Background.Picture := nil;
        Designer.Modified;
      end;
    9:
      begin
        if ReportLink.PageSetup then
          Designer.Modified;
      end;
    10:
      begin
        ReportLink.IsCurrentLink := True;
        dxShowPreviewWindow(ReportLink.ComponentPrinter, Designer);
      end;
    11:
      if ReportLink.Print(True, nil) then
        Designer.Modified;
  end;
end;

function TdxReportLinkEditor.InternalGetVerb(AIndex: Integer): string;
begin
  case AIndex of
    0: Result := sdxShowReportDesigner;
    1: Result := sdxMakeCurrent;
    2: Result := '-';
    3: Result := sdxRestoreOriginal;
    4: Result := sdxRestoreDefaults;
    5: Result := '-';
    6: Result := sdxPageBackground;
    7: Result := sdxClearBackground;
    8: Result := '-';
    9: Result := sdxPageSetup;
    10: Result := sdxPrintPreview;
    11: Result := sdxPrint;
  end;
end;

function TdxReportLinkEditor.InternalGetVerbCount: Integer;
begin
  Result := 12;
end;

procedure TdxReportLinkEditor.PrepareItem(Index: Integer; const AItem: IMenuItem);
begin
  case Index of
    0:
      AItem.Enabled := TBasedxReportLink(Component).CheckToDesign;
    1:
      AItem.Enabled := not TBasedxReportLink(Component).IsCurrentLink;
    9:
      AItem.Enabled := rlcPageSetup in TBasedxReportLink(Component).Capabilities;
    10:
      AItem.Enabled := TBasedxReportLink(Component).DataProviderPresent;
    11:
      AItem.Enabled := TBasedxReportLink(Component).DataProviderPresent;
  end;
end;

{ TdxReportLinkComponentPropertyEditor }

destructor TdxReportLinkComponentPropertyEditor.Destroy;
begin
  FComponents.Free;
  inherited Destroy;
end;

procedure TdxReportLinkComponentPropertyEditor.Initialize;
begin
  inherited Initialize;
  FComponents := TStringList.Create;
end;

function TdxReportLinkComponentPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes - [paMultiSelect];
end;

procedure TdxReportLinkComponentPropertyEditor.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  GetComponents;
  for I := 0 to FComponents.Count - 1 do
    Proc(FComponents[I]);
end;

function TdxReportLinkComponentPropertyEditor.GetReportLink: TBasedxReportLink;
begin
  Result := TBasedxReportLink(GetComponent(0));
end;

procedure TdxReportLinkComponentPropertyEditor.AddComponent(const AName: string);
begin
  if IsComponentValid(Designer.GetComponent(AName)) then FComponents.Add(AName);
end;

procedure TdxReportLinkComponentPropertyEditor.GetComponents;
begin
  FComponents.Clear;
  Designer.GetComponentNames(GetTypeData(PTypeInfo(TComponent.ClassInfo)), AddComponent);
end;

function TdxReportLinkComponentPropertyEditor.IsComponentValid(AComponent: TComponent): Boolean;
begin
  Result := (AComponent <> nil) and not (AComponent is TBasedxReportLink) and
    not (AComponent is TCustomdxComponentPrinter) and ReportLink.Supports(AComponent);
end;

{ TdxCompositionReportLinkPropertyEditor }

destructor TdxCompositionReportLinkPropertyEditor.Destroy;
begin
  FComponents.Free;
  inherited Destroy;
end;

function TdxCompositionReportLinkPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes - [paMultiSelect];
end;

procedure TdxCompositionReportLinkPropertyEditor.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  FComponents.Clear;
  Designer.GetComponentNames(GetTypeData(PTypeInfo(TComponent.ClassInfo)), CheckProc);
  for I := 0 to FComponents.Count - 1 do
    Proc(FComponents[I]);
end;

procedure TdxCompositionReportLinkPropertyEditor.Initialize;
begin
  inherited Initialize;
  FComponents := TStringList.Create;
end;

function TdxCompositionReportLinkPropertyEditor.GetItems: TdxCompositionLinkItems;
begin
  Result := TdxCompositionLinkItems(TdxCompositionLinkItem(GetComponent(0)).Collection);
end;

procedure TdxCompositionReportLinkPropertyEditor.CheckProc(const S: string);
begin
  if IsComponentValid(Designer.GetComponent(S)) then
    FComponents.Add(S);
end;

function TdxCompositionReportLinkPropertyEditor.IsComponentValid(AComponent: TComponent): Boolean;
begin
  Result := (AComponent is TBasedxReportLink) and Items.IsLinkComposable(TBasedxReportLink(AComponent));
end;

{ TdxIndexPropertyEditor }

function TdxIndexPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes - [paMultiSelect];
end;

{ TdxReportLinkDesignerPropertyEditor }

function TdxReportLinkDesignerPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paReadOnly];
  if TBasedxReportLink(GetComponent(0)).CheckToDesign then
    Result := Result + [paDialog];
end;

function TdxReportLinkDesignerPropertyEditor.GetValue: string;
begin
  if TBasedxReportLink(GetComponent(0)).CheckToDesign then
    Result := DropAmpersand(sdxShowReportDesigner)
  else
    Result := sdxDesignerNoAvailable;
end;

procedure TdxReportLinkDesignerPropertyEditor.Edit;
begin
  if TBasedxReportLink(GetComponent(0)).DesignReport then
    Designer.Modified;
end;

{ TdxPSExplorerFilterPropertyEditor }

function TdxPSExplorerFilterPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paValueList, paSortList];
end;

procedure TdxPSExplorerFilterPropertyEditor.GetValues(Proc: TGetStrProc);
var
  List: TdxClassList;
  I: Integer;
  ComponentClass: TClass;
begin
  List := TdxClassList.Create;
  try
    dxIdeImagesProvider.Refresh;
    dxPSGetSupportedComponentsList(List);
    for I := 0 to List.Count - 1 do
    begin
      ComponentClass := List[I];
      if IsAcceptableComponentClass(ComponentClass) then Proc(ComponentClass.ClassName);
    end;
  finally
    List.Free;
  end;
end;

procedure TdxPSExplorerFilterPropertyEditor.ListMeasureWidth(const Value: string;
  ACanvas: TCanvas; var AWidth: Integer);
begin
  Inc(AWidth, 1 + ImageList.Height + 2);
end;

procedure TdxPSExplorerFilterPropertyEditor.ListMeasureHeight(const Value: string;
  ACanvas: TCanvas; var AHeight: Integer);
begin
  AHeight := 1 + ImageList.Height + 1;
end;

procedure TdxPSExplorerFilterPropertyEditor.ListDrawValue(const Value: string;
  ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
var
  R: TRect;
begin
  R := ARect;
  ACanvas.FillRect(R);
  ImageList.Draw(ACanvas, R.Left + 1, R.Top + 1, ImageIndexes[Value]);
  Inc(R.Left, 1 + ImageList.Width + 2);
  InflateRect(R, 0, -(R.Bottom - R.Top - ACanvas.TextHeight(Value)) div 2);
  DefaultPropertyListDrawValue(Value, ACanvas, R, ASelected);
end;

function TdxPSExplorerFilterPropertyEditor.GetImageIndex(const Value: string): Integer;
begin
  Result := dxIdeImagesProvider.ImageIndexes[TComponentClass(GetClass(Value))];
end;

function TdxPSExplorerFilterPropertyEditor.GetImageList: TCustomImageList;
begin
  Result := dxIdeImagesProvider.Images;
end;

function TdxPSExplorerFilterPropertyEditor.IsAcceptableComponentClass(AComponentClass: TClass): Boolean;
begin
  Result := (AComponentClass <> nil) and
    not AComponentClass.InheritsFrom(ExtCtrls.TPage) and
    not AComponentClass.InheritsFrom(TabNotBk.TTabPage);
end;

{ TdxPrintStyleImageIndexPropertyEditor }

function TdxPrintStyleImageIndexPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paSortList, paRevertable];
end;

procedure TdxPrintStyleImageIndexPropertyEditor.GetValues(Proc: TGetStrProc);
var
  PrintStyle: TBasedxPrintStyle;
  I: Integer;
begin
  PrintStyle := TBasedxPrintStyle(GetComponent(0));
  if PrintStyle.StyleManager.Images <> nil then
    for I := 0 to PrintStyle.StyleManager.Images.Count - 1 do
      Proc(IntToStr(I));
end;

procedure TdxPrintStyleImageIndexPropertyEditor.ListMeasureHeight(const Value: string;
  ACanvas: TCanvas; var AHeight: Integer);
var
  Images: TCustomImageList;
begin
  Images := TBasedxPrintStyle(GetComponent(0)).StyleManager.Images;
  if Images <> nil then
    AHeight := Images.Height + 2 + 2;
end;

procedure TdxPrintStyleImageIndexPropertyEditor.ListMeasureWidth(const Value: string;
  ACanvas: TCanvas; var AWidth: Integer);
var
  Images: TCustomImageList;
begin
  Images := TBasedxPrintStyle(GetComponent(0)).StyleManager.Images;
  if Images <> nil then
    AWidth := AWidth + Images.Width + 2 + 2;
end;

procedure TdxPrintStyleImageIndexPropertyEditor.ListDrawValue(const Value: string;
  ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
var
  R: TRect;
  Images: TCustomImageList;
  I: Integer;
begin
  R := ARect;
  ACanvas.FillRect(R);
  Images := TBasedxPrintStyle(GetComponent(0)).StyleManager.Images;
  if Images <> nil then
  try
    I := StrToInt(Value);
    if (I > -1) and (I < Images.Count) then
    begin
      Images.Draw(ACanvas, R.Left + 2, R.Top + 2, I);
      Inc(R.Left, Images.Width + 2);
    end;
  except
  end;
  DefaultPropertyListDrawValue(Value, ACanvas, R, ASelected);
end;

procedure TdxPrintStyleImageIndexPropertyEditor.PropDrawValue(ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean);
var
  S: string;
begin
  S := GetVisualValue;
  if (S <> '') and ASelected then
    ListDrawValue(S, ACanvas, ARect, ASelected)
  else
    DefaultPropertyDrawValue(Self, ACanvas, ARect);
end;

{ ICustomPropertyDrawing }

procedure TdxPrintStyleImageIndexPropertyEditor.PropDrawName(ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean);
begin
  DefaultPropertyDrawName(Self, ACanvas, ARect);
end;

{ TdxCustomDrawListPropertyEditor }

procedure TdxCustomDrawListPropertyEditor.ListMeasureWidth(const Value: string;
  ACanvas: TCanvas; var AWidth: Integer);
var
  W: Integer;
begin
  W := 1 + ImageList.Width + 1 + 2 + ACanvas.TextWidth(Value) + 2;
  if W > AWidth then AWidth := W;
end;

procedure TdxCustomDrawListPropertyEditor.ListMeasureHeight(const Value: string;
  ACanvas: TCanvas; var AHeight: Integer);
begin
  AHeight := ImageList.Height + 2;
end;

procedure TdxCustomDrawListPropertyEditor.ListDrawValue(const Value: string;
  ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
var
  R: TRect;
begin
  ACanvas.FillRect(ARect);
  ImageList.Draw(ACanvas, ARect.Left + 1, ARect.Top + 1, ImageIndexes[Value]);
  R := ARect;
  Inc(R.Left, 1 + ImageList.Width + 1);
  ACanvas.TextRect(R, R.Left + 1, R.Top + (R.Bottom - R.Top - ACanvas.TextHeight(Value)) div 2, Value);
end;

{ TdxPaperOrientationPropertyEditor }

function TdxPaperOrientationPropertyEditor.GetImageIndex(const Value: string): Integer;
begin
  Result := GetEnumValue(TypeInfo(TdxPrinterOrientation), Value);
end;

function TdxPaperOrientationPropertyEditor.GetImageList: TImageList;
begin
  Result := ilPaperOrientations;
end;

{ TdxPrintOrderPropertyEditor }

function TdxPrintOrderPropertyEditor.GetImageIndex(const Value: string): Integer;
begin
  Result := GetEnumValue(TypeInfo(TdxPageOrder), Value);
end;

function TdxPrintOrderPropertyEditor.GetImageList: TImageList;
begin
  Result := ilPrintOrders;
end;

{ TdxPSPreviewWindowProperty }

function TdxPSPreviewWindowProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paValueList, paSortList] - [paReadOnly];
end;

procedure TdxPSPreviewWindowProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := 0 to dxPSPreviewDialogManager.Count - 1 do
    Proc(dxPSPreviewDialogManager.Items[I].GetName);
end;

{ TdxPathPropertyEditor }

procedure TdxPathPropertyEditor.Edit;
var
  I: Integer;
begin
  if Explorers[0].ShowChangeRootPathDlg then
    for I := 1 to PropCount - 1 do
      Explorers[I].RootPath := Explorers[0].RootPath;
end;

function TdxPathPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paMultiSelect, paDialog];
end;

function TdxPathPropertyEditor.GetExplorer(Index: Integer): TdxPSFileBasedExplorer;
begin
   Result := TdxPSFileBasedExplorer(GetComponent(Index));
end;

{ TdxRegistryPathPropertyEditor }

procedure TdxRegistryPathPropertyEditor.Edit;
var
  S: string;
begin
  S := Value;
{$IFNDEF LIMITED_EDITION}
  if dxGetRegistryPath(S) then
  begin
    Value := S;
    Designer.Modified;
  end;
 {$ENDIF}
end;

function TdxRegistryPathPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes {$IFNDEF LIMITED_EDITION}+ [paDialog]{$ENDIF};
end;

{ TdxExplorerStubLinkPropertyEditor }

procedure TdxExplorerStubLinkPropertyEditor.GetValues(Proc: TGetStrProc);
var
  Links: TList;
  I: Integer;
  Link: TBasedxReportLink;
begin
  Links := TList.Create;
  try
    TdxComponentPrinter(GetComponent(0)).GetLinks(Links);
    for I := 0 to Links.Count - 1 do
    begin
      Link := TBasedxReportLink(Links[I]);
      if Link.CanBeUsedAsStub then Proc(Link.Name);
    end;
  finally
    Links.Free;
  end;
end;

{ TdxCustomContainerReportLinkComponentPropertyEditor }

procedure TdxCustomContainerReportLinkComponentPropertyEditor.Initialize;
begin
  inherited;
  dxPSDsgProxies.dxIdeImagesProvider.Refresh;
end;

procedure TdxCustomContainerReportLinkComponentPropertyEditor.ListMeasureWidth(const Value: string;
  ACanvas: TCanvas; var AWidth: Integer);
var
  W: Integer;
begin
  W := 1 + Images.Width + 1 + 2 + ACanvas.TextWidth(Value) + 2;
  if W > AWidth then AWidth := W;
end;

procedure TdxCustomContainerReportLinkComponentPropertyEditor.ListMeasureHeight(const Value: string;
  ACanvas: TCanvas; var AHeight: Integer);
begin
  Aheight := 1 + Images.Height + 1;
end;

procedure TdxCustomContainerReportLinkComponentPropertyEditor.ListDrawValue(const Value: string;
  ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
var
  X, Y: Integer;
begin
  ACanvas.FillRect(ARect);
  Images.Draw(ACanvas, ARect.Left + 1, ARect.Top + 1, ImageIndexes[Value]);
  with ARect do
  begin
    X := Left + 1 + Images.Width + 1 + 2;
    Y := Top + (Bottom - Top - ACanvas.TextHeight(Value)) div 2;
  end;
  ACanvas.Brush.Style := bsClear;
  ACanvas.TextRect(ARect, X, Y, Value);
  ACanvas.Brush.Style := bsSolid;
end;

procedure TdxCustomContainerReportLinkComponentPropertyEditor.GetComponents;
var
  Project: IOTAProject;
  I: Integer;
  ModuleInfo: IOTAModuleInfo;
begin
  inherited GetComponents;

  Project := GetProject;
  if Project <> nil then
    for I := 0 to Project.GetModuleCount - 1 do
    begin
      ModuleInfo := Project.GetModule(I);
      if IsSupportedModuleType(ModuleInfo.ModuleType) then
        AddComponent(ModuleInfo.FormName);
    end;
end;

function TdxCustomContainerReportLinkComponentPropertyEditor.GetProject: IOTAProject;
var
  Module: IOTAModule;
  HasMultipleProjects: Boolean;
  I: Integer;
  ProjectGroup: IOTAProjectGroup;
  Project: IOTAProject;
begin
  Result := nil;
  HasMultipleProjects := False;
  with BorlandIDEServices as IOTAModuleServices do
    for I := 0 to GetModuleCount - 1 do
    begin
      Module := GetModule(I);
      if Supports(Module, IOTAProjectGroup, ProjectGroup) then
      begin
        Result := ProjectGroup.ActiveProject;
        Exit;
      end
      else
        if Supports(Module, IOTAProject, Project) then
          if Result = nil then
            Result := Project
          else
            HasMultipleProjects := True;
    end;
  if HasMultipleProjects then Result := nil;
end;

function TdxCustomContainerReportLinkComponentPropertyEditor.IsComponentValid(AComponent: TComponent): Boolean;
begin
  Result := inherited IsComponentValid(AComponent) and
    ((AComponent = nil) or (AComponent = Designer.GetRoot) or (AComponent.Owner = Designer.GetRoot));
end;

function TdxCustomContainerReportLinkComponentPropertyEditor.IsSupportedModuleType(
  const AModuleType: TOTAModuleType): Boolean;
begin
  Result := AModuleType = omtForm;
end;

function TdxCustomContainerReportLinkComponentPropertyEditor.GetImageIndex(const Value: string): Integer;
begin
  Result := dxPSDsgProxies.dxIdeImagesProvider.ImageIndexesByObject[Designer.GetObject(Value)];
end;

function TdxCustomContainerReportLinkComponentPropertyEditor.GetImages: TCustomImageList;
begin
  Result := dxPSDsgProxies.dxIdeImagesProvider.Images;
end;

{ TdxMeasurementUnitsPropertyEditor }

procedure TdxMeasurementUnitsPropertyEditor.GetValues(Proc: TGetStrProc);

  function DropPrefix(const Source: string): string;
  const
    Prefix: string = 'mu';
  var
    P: Integer;
  begin
    Result := Source;
    P := Pos(Result, Prefix);
    if P <> 0 then
      Delete(Result, P, Length(Prefix));
  end;

const
  Default: string = 'muDefault';
var
  EnumType: PTypeInfo;
  I: Integer;
  EnumName, S: string;
begin
  EnumType := GetPropType;
  with GetTypeData(EnumType)^ do
    for I := MinValue to MaxValue do
    begin
      EnumName := GetEnumName(EnumType, I);
      if CompareStr(EnumName, Default) = 0 then
      begin
        S := GetEnumName(TypeInfo(TdxMeasurementUnits), Integer(GetDefaultMeasurementUnits));
        EnumName := EnumName + ' (' + DropPrefix(S) + ')';
      end;
      Proc(EnumName);
    end;
end;

function TdxMeasurementUnitsPropertyEditor.GetValue: string;
begin
  Result := inherited GetValue + ' ' + sdxInThousandths;
end;

procedure TdxMeasurementUnitsPropertyEditor.SetValue(const Value: string);

  function IsDefaultMeasurementUnitsValue: Boolean;
  var
    DefaultEnumName: string;
  begin
    DefaultEnumName := GetEnumName(TypeInfo(TdxMeasurementUnits), Integer(muDefault));
    Result := Pos(DefaultEnumName, Value) <> 0;
  end;

begin
  if IsDefaultMeasurementUnitsValue then
    SetOrdValue(Integer(muDefault))
  else
    inherited;
end;

{ TdxPDFOptionsPropertyEditor }

procedure TdxPDFOptionsPropertyEditor.Edit;
var
  AReportLink: TBasedxReportLink;
  I: Integer;
begin
  AReportLink := TBasedxReportLink(GetComponent(0));
  if dxPSShowPDFSettingsDialog(AReportLink.PDFExportOptions, True) then
  begin
    if PropCount > 1 then
    begin
      for I := 1 to PropCount - 1 do
        TBasedxReportLink(GetComponent(I)).PDFExportOptions.Assign(AReportLink.PDFExportOptions);
    end;
    Designer.Modified;
  end;
end;

function TdxPDFOptionsPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

{ TdxPrinterPagePropertyEditor }

procedure TdxPrinterPagePropertyEditor.Edit;
var
  ReportLink: TBasedxReportLink;
  I: Integer;
begin
  ReportLink := TBasedxReportLink(GetComponent(0));
  if ReportLink.PageSetup then
  begin
    if PropCount > 1 then
    begin
      for I := 1 to PropCount - 1 do
        TBasedxReportLink(GetComponent(I)).PrinterPage.Assign(ReportLink.PrinterPage);
    end;
    Designer.Modified;
  end;
end;

function TdxPrinterPagePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

{ TdxReportFootnotesPropertyEditor }

procedure TdxReportFootnotesPropertyEditor.Edit;
var
  AReportLink: TBasedxReportLink;
  I: Integer;
begin
  AReportLink := TBasedxReportLink(GetComponent(0));
  if AReportLink.ShowFootnotesPropertiesDlg then
  begin
    for I := 1 to PropCount - 1 do
      TBasedxReportLink(GetComponent(I)).ReportFootnotes.Assign(AReportLink.ReportFootnotes);
    Designer.Modified;
  end;
end;

function TdxReportFootnotesPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

{ TdxReportTitlePropertyEditor }

procedure TdxReportTitlePropertyEditor.Edit;
var
  AReportLink: TBasedxReportLink;
  I: Integer;
begin
  AReportLink := TBasedxReportLink(GetComponent(0));
  if AReportLink.ShowTitlePropertiesDlg then
  begin
    for I := 1 to PropCount - 1 do
      TBasedxReportLink(GetComponent(I)).ReportTitle.Assign(AReportLink.ReportTitle);
    Designer.Modified;
  end;
end;

function TdxReportTitlePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

{ TdxReportTitleTextAlignXPropertyEditor }

procedure TdxReportTitleTextAlignXPropertyEditor.GetValues(Proc: TGetStrProc);
var
  EnumType: PTypeInfo;
  I: Integer;
begin
  EnumType := GetPropType;
  for I := Integer(taLeft) to Integer(taRight) do
    Proc(GetEnumName(EnumType, I));
end;

procedure TdxReportTitleTextAlignXPropertyEditor.SetValue(const Value: string);
var
  I: Integer;
begin
  I := GetEnumValue(GetPropType, Value);
  if not (TcxTextAlignX(I) in [taLeft, taCenterX, taRight]) then
      raise EPropertyError.CreateRes(@SInvalidPropertyValue);
  SetOrdValue(I);
end;

{ TdxReportTitleTextAlignYPropertyEditor }

procedure TdxReportTitleTextAlignYPropertyEditor.GetValues(Proc: TGetStrProc);
var
  EnumType: PTypeInfo;
  I: Integer;
begin
  EnumType := GetPropType;
  for I := Integer(taTop) to Integer(taBottom) do
    Proc(GetEnumName(EnumType, I));
end;

procedure TdxReportTitleTextAlignYPropertyEditor.SetValue(const Value: string);
var
  I: Integer;
begin
  I := GetEnumValue(GetPropType, Value);
  if not (TcxTextAlignY(I) in [taTop, taCenterY, taBottom]) then
    raise EPropertyError.CreateRes(@SInvalidPropertyValue);
  SetOrdValue(I);
end;

{ TdxHeaderFooterTextAlignYPropertyEditor }

procedure TdxHeaderFooterTextAlignYPropertyEditor.GetValues(Proc: TGetStrProc);
var
  EnumType: PTypeInfo;
  I: Integer;
begin
  EnumType := GetPropType;
  for I := Integer(taTop) to Integer(taBottom) do
    Proc(GetEnumName(EnumType, I));
end;

procedure TdxHeaderFooterTextAlignYPropertyEditor.SetValue(const Value: string);
var
  I: Integer;
begin
  I := GetEnumValue(GetPropType, Value);
  if not (TcxTextAlignY(I) in [taTop, taCenterY, taBottom]) then
    raise EPropertyError.CreateRes(@SInvalidPropertyValue);
  SetOrdValue(I);
end;

{ TdxPrinterPagePropertyEditor2 }

procedure TdxPrinterPagePropertyEditor2.Edit;
var
  I: Integer;
  PrintStyle: TBasedxPrintStyle;
begin
  PrintStyle := TBasedxPrintStyle(GetComponent(0));
  if PrintStyle.PageSetup then
  begin
    if PropCount > 1 then
      for I := 1 to PropCount - 1 do
        TBasedxPrintStyle(GetComponent(I)).PrinterPage.Assign(PrintStyle.PrinterPage);
    Designer.Modified;
  end;
end;

function TdxPrinterPagePropertyEditor2.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

{ TdxPointWrapperEditor }

function TdxPointWrapperEditor.PointWrapper: TdxPointWrapper;
begin
  Result := TdxPointWrapper(GetOrdValue);
end;

function TdxPointWrapperEditor.GetValue: string;
var
  Pt: TdxPointWrapper;
begin
  Pt := PointWrapper;
  if Pt <> nil then
    with Pt do
      Result := Format('(X: %d; Y: %d)', [X, Y])
  else
    Result := inherited GetValue;
end;

{ TdxRectWrapperEditor }

function TdxRectWrapperEditor.RectWrapper: TdxRectWrapper;
begin
  Result := TdxRectWrapper(GetOrdValue);
end;

function TdxRectWrapperEditor.GetValue: string;
var
  R: TdxRectWrapper;
begin
  R := RectWrapper;
  if R <> nil then
    with R do
      Result := Format('(Bottom: %d; Left: %d; Right: %d; Top: %d)', [Bottom, Left, Right, Top])
  else
    Result := inherited GetValue;
end;

{ TdxBackgroundPropertyEditor }

procedure TdxBackgroundPropertyEditor.Edit;
var
  I: Integer;
begin
  if Background.SetupEffects then
  begin
    for I := 1 to PropCount - 1 do
      TdxBackground(GetOrdValueAt(I)).Assign(Background);
    Designer.Modified;
  end;
end;

function TdxBackgroundPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

function TdxBackgroundPropertyEditor.GetBackground: TdxBackground;
begin
  Result := TdxBackground(GetOrdValueAt(0));
end;

{ TdxBrushPropertyEditor }

procedure TdxBrushPropertyEditor.Edit;
var
  I: Integer;
begin
  if ChooseBrush(Brush) then
  begin
    for I := 1 to PropCount - 1 do
      TBrush(GetOrdValueAt(I)).Assign(Brush);
    Designer.Modified;
  end;
end;

function TdxBrushPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

function TdxBrushPropertyEditor.GetBrush: TBrush;
begin
  Result := TBrush(GetOrdValue);
end;

{ TTTFontPropertyEditor }

procedure TTTFontPropertyEditor.Edit;
const
  hcDFontEditor = 25000;
var
  FontDialog: TFontDialog;
begin
  FontDialog := TFontDialog.Create(Application);
  try
    FontDialog.Font := TFont(GetOrdValue);
    FontDialog.Device := dxPSGlbl.FontDialogDevice;
    FontDialog.HelpContext := hcDFontEditor;
    FontDialog.Options := [fdEffects, fdScalableOnly, fdTrueTypeOnly];
    if FontDialog.Execute then
      SetOrdValue(Longint(FontDialog.Font));
  finally
    FontDialog.Free;
  end;
end;

{ TdxPrintStyleManagerEditor  }

function TdxPrintStyleManagerEditor.InternalGetVerb(AIndex: Integer): string;
begin
  Result := sdxPrintStyles;
end;

function TdxPrintStyleManagerEditor.InternalGetVerbCount: Integer;
begin
  Result := 1;
end;

procedure TdxPrintStyleManagerEditor.InternalExecuteVerb(AIndex: Integer);
begin
  dxShowPrintStylesDesigner(TdxPrintStyleManager(Component), Designer);
end;

{ TdxPrintStyleEditor }

function TdxPrintStyleEditor.PrintStyle: TBasedxPrintStyle;
begin
  Result := TBasedxPrintStyle(Component);
end;

function TdxPrintStyleEditor.InternalGetVerb(AIndex: Integer): string;
begin
  case AIndex of
    0: Result := sdxPageSetup;
    1: Result := sdxMakeCurrent;
    2: Result := '-';
    3: Result := sdxRestoreDefaults;
    4: Result := '-';
    5: Result := sdxPageBackground;
    6: Result := sdxClearBackground;
  end;
end;

function TdxPrintStyleEditor.InternalGetVerbCount: Integer;
begin
  Result := 7;
end;

procedure TdxPrintStyleEditor.InternalExecuteVerb(AIndex: Integer);
begin
  case AIndex of
    0:
      if PrintStyle.PageSetup then Designer.Modified;
    1:
      begin
        PrintStyle.IsCurrentStyle := True;
        Designer.Modified;
      end;
    3:begin
        PrintStyle.RestoreDefaults;
        Designer.Modified;
      end;
    5:
       if PrintStyle.PrinterPage.Background.SetupEffects then
         Designer.Modified;
    6:
      begin
        PrintStyle.PrinterPage.Background.Mode := bmNone;
        PrintStyle.PrinterPage.Background.Picture := nil;
        Designer.Modified;
      end;
  end;
end;

procedure TdxPrintStyleEditor.PrepareItem(Index: Integer; const AItem: IMenuItem);
begin
  if Index = 1 then
    AItem.Enabled := not TBasedxPrintStyle(Component).IsCurrentStyle;
end;

{ TdxPageSetupDialogEditor }

function TdxPageSetupDialogEditor.InternalGetVerb(AIndex: Integer): string;
begin
  Result := sdxPageSetupDialog;
end;

function TdxPageSetupDialogEditor.InternalGetVerbCount: Integer;
begin
  Result := 1;
end;

procedure TdxPageSetupDialogEditor.InternalExecuteVerb(AIndex: Integer);
begin
  TdxPageSetupDialog(Component).Execute;
end;

procedure TdxPageSetupDialogEditor.PrepareItem(Index: Integer; const AItem: IMenuItem);
begin
  if Index = 0 then
    AItem.Enabled := TdxPageSetupDialog(Component).PrintStyle <> nil;
end;

{ TdxPrintDialogEditor }

function TdxPrintDialogEditor.InternalGetVerb(AIndex: Integer): string;
begin
  Result := sdxPrintDialog;
end;

function TdxPrintDialogEditor.InternalGetVerbCount: Integer;
begin
  Result := 1;
end;

procedure TdxPrintDialogEditor.InternalExecuteVerb(AIndex: Integer);
begin
  TdxPrintDialog(Component).Execute;
end;

{ TdxShowPgsDlgPropertyEditor }

procedure TdxShowPgsDlgPropertyEditor.Edit;
begin
  if TBasedxPrintStyle(GetComponent(0)).PageSetup then
    Designer.Modified;
end;

function TdxShowPgsDlgPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TdxShowPgsDlgPropertyEditor.GetValue: string;
begin
  Result := sdxClickForPageSetup;
end;

{ TdxStyleGlyphPropertyEditor }

function TdxStyleGlyphPropertyEditor.GetValue: string;
begin
  Result := inherited GetValue;
  if PrintStyle.IsStyleGlyphAssigned then
    Result := Result + ' - ' + sdxRestoreDefaultGlyph;
end;

procedure TdxStyleGlyphPropertyEditor.SetValue(const Value: string);
begin
  if CompareText(Value, sdxDefault) = 0 then
    PrintStyle.RestoreDefaultGlyph
  else
    inherited;
end;

function TdxStyleGlyphPropertyEditor.GetPrintStyle: TBasedxPrintStyle;
begin
  Result := TBasedxPrintStyle(GetComponent(0));
end;

{ TdxCustomListDrawingIntegerPropertyEditor }

procedure TdxCustomListDrawingIntegerPropertyEditor.ListMeasureWidth(const Value: string;
  ACanvas: TCanvas; var AWidth: Integer);
var
  W: Integer;
begin
  W := 1 + ImageList.Width + 1 + 2 + ACanvas.TextWidth(Value) + 2;
  if W > AWidth then AWidth := W;
end;

procedure TdxCustomListDrawingIntegerPropertyEditor.ListMeasureHeight(const Value: string;
  ACanvas: TCanvas; var AHeight: Integer);
begin
  AHeight := ImageList.Height + 2;
end;

procedure TdxCustomListDrawingIntegerPropertyEditor.ListDrawValue(const Value: string;
  ACanvas: TCanvas;  const ARect: TRect; ASelected: Boolean);
var
  R: TRect;
begin
  ACanvas.FillRect(ARect);
  ImageList.Draw(ACanvas, ARect.Left + 1, ARect.Top + 1, ImageIndexes[Value]);
  R := ARect;
  Inc(R.Left, 1 + ImageList.Width + 1);
  ACanvas.TextRect(R, R.Left + 1, R.Top + (R.Bottom - R.Top - ACanvas.TextHeight(Value)) div 2, Value);
end;

{ TdxDMPaperPropertyEditor }

function TdxDMPaperPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog, paValueList];
end;

function TdxDMPaperPropertyEditor.GetValue: string;
var
  Index: Integer;
begin
  Index := dxPPAttr.Papers.FindByDMPaper(GetOrdValue);
  if Index <> -1 then
    Result := dxPPAttr.Papers[Index].Name
  else
    Result := sdxCustomPaperSize + IntToStr(GetOrdValue);
end;

procedure TdxDMPaperPropertyEditor.SetValue(const Value: string);
var
  Index: Integer;
begin
  Index := dxPPAttr.Papers.FindByName(Value);
  if Index <> -1 then
    SetOrdValue(dxPPAttr.Papers[Index].DMPaper)
  else
    inherited SetValue(Value);
end;

procedure TdxDMPaperPropertyEditor.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := 0 to dxPPAttr.Papers.Count - 1 do
    Proc(dxPPAttr.Papers[I].Name);
end;

function TdxDMPaperPropertyEditor.GetImageIndex(const Value: string): Integer;
begin
  Result := Integer(dxIsEnvelopePaper(Value));
end;

function TdxDMPaperPropertyEditor.GetImageList: TImageList;
begin
  Result := ilPapers;
end;

{ TdxPaperSourcePropertyEditor }

function TdxPaperSourcePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog, paValueList];
end;

function TdxPaperSourcePropertyEditor.GetValue: string;
var
  Index: Integer;
begin
  Index := dxPPAttr.Bins.FindByValue(GetOrdValue);
  if Index <> -1 then
    Result := dxPPAttr.Bins[Index].Name
  else
    Result := sdxCustomBin + IntToStr(GetOrdValue);
end;

procedure TdxPaperSourcePropertyEditor.SetValue(const Value: string);
var
  Index: Integer;
begin
  Index := dxPPAttr.Bins.FindByName(Value);
  if Index <> -1 then
    SetOrdValue(dxPPAttr.Bins[Index].Value)
  else
    inherited SetValue(Value);
end;

procedure TdxPaperSourcePropertyEditor.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := 0 to dxPPAttr.Bins.Count - 1 do
    Proc(dxPPAttr.Bins[I].Name);
end;

function TdxPaperSourcePropertyEditor.GetImageIndex(const Value: string): Integer;
begin
  Result := Integer(not dxIsAutoSelectBin(Value));
end;

function TdxPaperSourcePropertyEditor.GetImageList: TImageList;
begin
  Result := ilBins;
end;

{ TdxPageNumberFormatEditor }

function TdxPageNumberFormatsPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog, paValueList];
end;

function TdxPageNumberFormatsPropertyEditor.GetValue: string;
begin
  Result := PageNumberFormats[GetOrdValue];
end;

procedure TdxPageNumberFormatsPropertyEditor.SetValue(const Value: string);
var
  I: Integer;
begin
  // case insensitive - PageNumberFormats.IndexOf(Value);
  for I := 0 to PageNumberFormats.Count - 1 do
    if dxSameStr(PageNumberFormats[I], Value) then
    begin
      SetOrdValue(I);
      Exit;
    end;
  inherited SetValue(Value);
end;

procedure TdxPageNumberFormatsPropertyEditor.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := 0 to PageNumberFormats.Count - 1 do
    Proc(PageNumberFormats[I]);
end;

{ TdxReportLinkStyleManagerPropertyEditor }

function TdxReportLinkStyleManagerPropertyEditor.AutoFill: Boolean;
begin
  Result := False;
end;

procedure TdxReportLinkStyleManagerPropertyEditor.GetValues(Proc: TGetStrProc);
begin
  if CanCreateStyleManager then Proc(sdxCreateNewStyleManager);
  inherited;
end;

procedure TdxReportLinkStyleManagerPropertyEditor.SetValue(const Value: string);
begin
  if Value = sdxCreateNewStyleManager then
    SetOrdValue(Integer(CreateNewStyleManager))
  else
    inherited;
end;

function TdxReportLinkStyleManagerPropertyEditor.CanCreateStyleManager: Boolean;
begin
  Result := (Root <> nil) and not Designer.IsSourceReadOnly and not (csInline in Root.ComponentState);
end;

function TdxReportLinkStyleManagerPropertyEditor.CreateNewStyleManager: TdxPrintStyleManager;
begin
  Result := TdxPrintStyleManager.Create(Root);
  Result.Name := UniqueName;
end;

function TdxReportLinkStyleManagerPropertyEditor.GetUniqueName: string;
var
  S: string;
begin
  S := TdxPrintStyleManager.ClassName;
  S := Copy(S, 2, Length(S) - 1);
  Result := Designer.UniqueName(S);
end;

function TdxReportLinkStyleManagerPropertyEditor.GetRoot: TComponent;
begin
  Result := Designer.Root;
end;

{ TdxReportDocumentDescriptionPropertyEditor }

procedure TdxReportDocumentDescriptionPropertyEditor.Edit;
var
  S: string;
begin
  S := Value;
  if dxPSfmEditDesc.dxEditDescriptionDlg(S) then
  begin
    Value := S;
    Designer.Modified;
  end;
end;

function TdxReportDocumentDescriptionPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

{ TdxDateFormatEditor }

procedure TdxDateFormatsPropertyEditor.Initialize;
begin
  inherited Initialize;
  FStrings := TStringList.Create;
  GetFormatedDateStrings(Now, DateFormats, FStrings);
end;

destructor TdxDateFormatsPropertyEditor.Destroy;
begin
  FStrings.Free;
  inherited Destroy;
end;

function TdxDateFormatsPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog, paValueList];
end;

function TdxDateFormatsPropertyEditor.GetValue: string;
begin
  Result := FStrings[GetOrdValue];
end;

procedure TdxDateFormatsPropertyEditor.SetValue(const Value: string);
var
  Index: Integer;
begin
  Index := FStrings.IndexOf(Value);
  if Index <> -1 then
    SetOrdValue(Index)
  else
    inherited SetValue(Value);
end;

procedure TdxDateFormatsPropertyEditor.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := 0 to FStrings.Count - 1 do
    Proc(FStrings[I]);
end;

{ TdxTimeFormatEditor }

procedure TdxTimeFormatsPropertyEditor.Initialize;
begin
  inherited Initialize;
  FStrings := TStringList.Create;
  GetFormatedTimeStrings(Now, TimeFormats, FStrings);
end;

destructor TdxTimeFormatsPropertyEditor.Destroy;
begin
  FStrings.Free;
  inherited Destroy;
end;

function TdxTimeFormatsPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog, paValueList];
end;

function TdxTimeFormatsPropertyEditor.GetValue: string;
begin
  Result := FStrings[GetOrdValue];
end;

procedure TdxTimeFormatsPropertyEditor.SetValue(const Value: string);
var
  Index: Integer;
begin
  Index := FStrings.IndexOf(Value);
  if Index <> -1 then
    SetOrdValue(Index)
  else
    inherited SetValue(Value);
end;

procedure TdxTimeFormatsPropertyEditor.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := 0 to FStrings.Count - 1 do
    Proc(FStrings[I]);
end;

{ TdxAutoHFTextEntriesPropertyEditor }

procedure TdxAutoHFTextEntriesPropertyEditor.Edit;
begin
  TdxPrintStyleManager(GetComponent(0)).ShowAutoHFTextEntriesDlg;
end;

function TdxAutoHFTextEntriesPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paReadOnly, paDialog];
end;

{ TdxPreviewIconPropertyEditor }

function TdxPreviewIconPropertyEditor.GetValue: string;
begin
  Result := inherited GetValue;
  if PreviewOptions.IsIconAssigned then
    Result := Result + ' - ' + sdxRestoreDefaultIcon;
end;

procedure TdxPreviewIconPropertyEditor.SetValue(const Value: string);
begin
  if CompareText(Value, sdxDefault) = 0 then
    PreviewOptions.RestoreOriginalIcon
  else
    inherited;
end;

function TdxPreviewIconPropertyEditor.GetPreviewOptions: TdxPreviewOptions;
begin
  Result := TdxPreviewOptions(GetComponent(0));
end;

{ TdxTextReportLinkAlignmentPropertyEditor }

procedure TdxTextReportLinkAlignmentPropertyEditor.GetValues(Proc: TGetStrProc);
var
  EnumType: PTypeInfo;
begin
  EnumType := GetPropType;
  Proc(GetEnumName(EnumType, Integer(taLeft)));
  Proc(GetEnumName(EnumType, Integer(taCenterX)));
  Proc(GetEnumName(EnumType, Integer(taRight)));
  Proc(GetEnumName(EnumType, Integer(taDistributeX)));
end;

{ TdxComponentPrinterSelectionEditor }

procedure TdxComponentPrinterSelectionEditor.PopulatePreviewDialogsUnits(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := 0 to dxPSPreviewDialogManager.Count - 1 do
    Proc(dxPSPreviewDialogManager.Items[I].GetUnitName);
end;

procedure TdxComponentPrinterSelectionEditor.PopulateReportLinksUnits(Proc: TGetStrProc);
var
  I: Integer;
begin
  if dxPSAutoFillReportLinkUnits then
  begin
    Proc('dxPScxPageControlProducer');
    for I := 0 to dxComponentPrintersList.Count - 1 do
      PopulateReportLinksUnits(Proc, TCustomdxComponentPrinter(dxComponentPrintersList.List[I]));
  end;
end;

procedure TdxComponentPrinterSelectionEditor.PopulateReportLinksUnits(
  Proc: TGetStrProc; AComponentPrinter: TCustomdxComponentPrinter);
var
  ALink: TBasedxReportLink;
  I: Integer;
begin
  for I := 0 to AComponentPrinter.LinkCount - 1 do
  begin
    ALink := AComponentPrinter.ReportLink[I];
    if ALink is TdxCustomContainerReportLink then
      ProcessContainer(TdxCustomContainerReportLink(ALink).Container, Proc);
    PopulateReportLinkUnits(Proc, TdxReportLinkClass(ALink.ClassType));
  end;
end;

procedure TdxComponentPrinterSelectionEditor.PopulateReportLinkUnits(
  Proc: TGetStrProc; ALinkClass: TdxReportLinkClass);
var
  I: Integer;
begin
  if ALinkClass <> nil then
  begin
    for I := 0 to ReportLinkUnitNameList.Count - 1 do
    begin
      if ReportLinkUnitNameList.Objects[I] = TObject(ALinkClass) then
        Proc(ReportLinkUnitNameList.Strings[I]);
    end;
  end;
end;

procedure TdxComponentPrinterSelectionEditor.ProcessContainer(
  AContainer: TWinControl; Proc: TGetStrProc);
var
  AChildControl: TControl;
  I: Integer;
begin
  if AContainer = nil then Exit;
  for I := 0 to AContainer.ControlCount - 1 do
  begin
    AChildControl := AContainer.Controls[I];
    PopulateReportLinkUnits(Proc, dxPSLinkClassByCompClass(AChildControl));
    if AChildControl is TWinControl then
      ProcessContainer(TWinControl(AChildControl), Proc);
  end;
end;

procedure TdxComponentPrinterSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
  Proc('dxPSGlbl');
  Proc('dxPSUtl');
  Proc('dxPSEngn');
  Proc('dxPrnPg');
  Proc('dxBkgnd');
  Proc('dxWrap');
  Proc('dxPrnDev');
  Proc('dxPSCompsProvider');
  Proc('dxPSFillPatterns');
  Proc('dxPSEdgePatterns');
  Proc('dxPSPDFExportCore');
  Proc('dxPSPDFExport');
  Proc('cxDrawTextUtils');
  dxSkinsRequiresAdditionalUnits(TcxPageControl, Proc);
  PopulatePreviewDialogsUnits(Proc);
  PopulateReportLinksUnits(Proc);
end;

{ TdxPrintStyleManagerSelectionEditor }

procedure TdxPrintStyleManagerSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
  Proc('dxPSGlbl');
  Proc('dxPSUtl');
  Proc('dxPrnPg');
  Proc('dxBkgnd');
  Proc('dxWrap');
  Proc('dxPrnDev');
end;

{ TdxPageSetupDialogSelectionEditor }

procedure TdxPageSetupDialogSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
  Proc('dxPSGlbl');
  Proc('dxPSUtl');
  Proc('dxPrnPg');
  Proc('dxBkgnd');
  Proc('dxWrap');
end;

{ TdxPrintDialogSelectionEditor }

procedure TdxPrintDialogSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
  Proc('dxPrnDev');
end;

//

procedure HideProperty(PropertyType: PTypeInfo; ComponentClass: TClass; const PropertyName: string);
begin
  RegisterPropertyEditor(PropertyType, ComponentClass, PropertyName, nil);
end;

procedure Register;
begin
  dxPSGlbl.IsDesignTime := True;

  RegisterComponents(dxPSProductPage, [TdxComponentPrinter, TdxPrintStyleManager,
    TdxPrintDialog, TdxPageSetupDialog, TdxPSEngineController, TdxPSFileBasedExplorer]);

  RegisterNoIcon([TBasedxReportLink, TdxCompositionReportLink, TdxCustomContainerReportLink]);
  RegisterNoIcon([TdxPictureReportLink, TdxTextReportLink]);
  RegisterNoIcon([TBasedxPrintStyle, TdxPSPrintStyle]);

  RegisterComponents(dxPSProductPage, [TdxPSPreviewWindow]);
  RegisterComponentEditor(TdxPSPreviewWindow, TdxCustomPrinterEditor);
  RegisterPropertyEditor(TypeInfo(TComponent), TdxPSPreviewWindow, 'PreviewPopupMenu', TcxControlPopupMenuProperty);

  RegisterComponentEditor(TdxComponentPrinter, TdxComponentPrinterEditor);
  RegisterComponentEditor(TdxPrintStyleManager, TdxPrintStyleManagerEditor);
  RegisterComponentEditor(TBasedxReportLink, TdxReportLinkEditor);
  RegisterComponentEditor(TBasedxPrintStyle, TdxPrintStyleEditor);
  RegisterComponentEditor(TdxPageSetupDialog, TdxPageSetupDialogEditor);
  RegisterComponentEditor(TdxPrintDialog, TdxPrintDialogEditor);
  RegisterComponentEditor(TCustomdxPSExplorer, TdxCustomPrinterEditor);
  RegisterComponentEditor(TdxPSEngineController, TdxCustomPrinterEditor);

  RegisterPropertyEditor(TypeInfo(TdxPSPreviewDialogStyle), nil, 'PreviewDialogStyle', TdxPSPreviewWindowProperty);

  RegisterPropertyEditor(TypeInfo(string), TdxPSFileBasedExplorer, 'RootPath', TdxPathPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TdxComponentPrinter, 'DateFormat', TdxDateFormatsPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TdxComponentPrinter, 'TimeFormat', TdxTimeFormatsPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TIcon), TdxPreviewOptions, 'Icon', TdxPreviewIconPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TdxPageNumberFormat), TdxComponentPrinter, 'PageNumberFormat', TdxPageNumberFormatsPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TdxComponentPrinter, 'RegistryPath', TdxRegistryPathPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TBasedxReportLink), TdxComponentPrinter, 'ExplorerStubLink', TdxExplorerStubLinkPropertyEditor);

  RegisterPropertyEditor(TypeInfo(TComponent), TBasedxReportLink, 'Component', TdxReportLinkComponentPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TComponent), TdxCustomContainerReportLink, 'Component', TdxCustomContainerReportLinkComponentPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TBasedxReportLink, 'Index', TdxIndexPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Boolean), TBasedxReportLink, 'ShowDesigner', TdxReportLinkDesignerPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TdxPrinterPage), TBasedxReportLink, 'PrinterPage', TdxPrinterPagePropertyEditor);
  RegisterPropertyEditor(TypeInfo(TdxPSPDFReportExportOptions), TBasedxReportLink, 'PDFExportOptions', TdxPDFOptionsPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TdxReportTitle), TBasedxReportLink, 'ReportTitle', TdxReportTitlePropertyEditor);
  RegisterPropertyEditor(TypeInfo(TdxReportFootnotes), TBasedxReportLink, 'ReportFootnotes', TdxReportFootnotesPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TFont), TBasedxReportLink, '', TTTFontPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TBasedxReportLink, 'DateFormat', TdxDateFormatsPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TBasedxReportLink, 'TimeFormat', TdxTimeFormatsPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TdxPageNumberFormat), TBasedxReportLink, 'PageNumberFormat', TdxPageNumberFormatsPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TdxPrintStyleManager), TBasedxReportLink, 'StyleManager', TdxReportLinkStyleManagerPropertyEditor);

  RegisterPropertyEditor(TypeInfo(string), TdxPSReportDocument, 'Description', TdxReportDocumentDescriptionPropertyEditor);

  RegisterPropertyEditor(TypeInfo(Integer), TBasedxPrintStyle, 'ImageIndex',  TdxPrintStyleImageIndexPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TCustomdxPSExplorer, 'FilterLink',  TdxPSExplorerFilterPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TdxPrinterOrientation), nil, '', TdxPaperOrientationPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TdxPageOrder), nil, '', TdxPrintOrderPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TComponent), TdxCompositionLinkItem, 'ReportLink', TdxCompositionReportLinkPropertyEditor);

  RegisterPropertyEditor(TypeInfo(TStrings), TdxPrintStyleManager, 'AutoHFTextEntries', TdxAutoHFTextEntriesPropertyEditor);

  RegisterPropertyEditor(TypeInfo(Boolean), TBasedxPrintStyle, 'ShowPageSetupDlg', TdxShowPgsDlgPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TdxPrinterPage), TBasedxPrintStyle, 'PrinterPage', TdxPrinterPagePropertyEditor2);
  RegisterPropertyEditor(TypeInfo(Integer), TBasedxPrintStyle, 'Index', TdxIndexPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TBitmap), TBasedxPrintStyle, 'StyleGlyph', TdxStyleGlyphPropertyEditor);

  RegisterPropertyEditor(TypeInfo(Integer), TdxPrinterPage, 'DMPaper', TdxDMPaperPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Integer), TdxPrinterPage, 'PaperSource', TdxPaperSourcePropertyEditor);
  RegisterPropertyEditor(TypeInfo(TdxMeasurementUnits), TdxPrinterPage, 'MeasurementUnits', TdxMeasurementUnitsPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TcxTextAlignY), TCustomdxPageObject, '', TdxHeaderFooterTextAlignYPropertyEditor);

  RegisterPropertyEditor(TypeInfo(TcxTextAlignX), TdxReportTitle, 'TextAlignX', TdxReportTitleTextAlignXPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TcxTextAlignY), TdxReportTitle, 'TextAlignY', TdxReportTitleTextAlignYPropertyEditor);

  RegisterPropertyEditor(TypeInfo(TdxBackground), nil, '', TdxBackgroundPropertyEditor);

  RegisterPropertyEditor(TypeInfo(TdxPointWrapper), nil, '', TdxPointWrapperEditor);
  RegisterPropertyEditor(TypeInfo(TdxRectWrapper), nil, '', TdxRectWrapperEditor);

  RegisterPropertyEditor(TypeInfo(string), TdxPSEngineController, 'RegistryPath', TdxRegistryPathPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TdxPSOptionsStoring, 'RegistryPath', TdxRegistryPathPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TdxPageSetupDialog, 'RegistryPath', TdxRegistryPathPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TdxPrintDialog, 'RegistryPath', TdxRegistryPathPropertyEditor);

  RegisterPropertyEditor(TypeInfo(TBrush), nil, '', TdxBrushPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TcxTextAlignX), TdxCustomTextReportLink, 'Alignment', TdxTextReportLinkAlignmentPropertyEditor);

  RegisterSelectionEditor(TCustomdxComponentPrinter, TdxComponentPrinterSelectionEditor);
  RegisterSelectionEditor(TdxPrintStyleManager, TdxPrintStyleManagerSelectionEditor);
  RegisterSelectionEditor(TdxPageSetupDialog, TdxPageSetupDialogSelectionEditor);
  RegisterSelectionEditor(TdxPrintDialog, TdxPrintDialogSelectionEditor);

  // hide unneeded property editors
  HideProperty(TypeInfo(string), TdxPreviewOptions, 'HelpFile');
  HideProperty(TypeInfo(string), TdxPreviewOptions, 'RegistryPath');
  HideProperty(TypeInfo(Boolean), TdxPreviewOptions, 'SavePosition');
  HideProperty(TypeInfo(string), TBasedxReportLink, 'ReportTitleText');
  HideProperty(TypeInfo(string), TBasedxReportLink, 'Caption');
  HideProperty(TypeInfo(TDateTime), TBasedxReportLink, 'DateTime');
  HideProperty(TypeInfo(string), TBasedxReportLink, 'Description');
  HideProperty(TypeInfo(TdxReportTitleMode), TBasedxReportLink, 'ReportTitleMode');

  HideProperty(TypeInfo(TComponent), TdxCompositionReportLink, 'Component');
  HideProperty(TypeInfo(TdxReportTitle), TdxCompositionReportLink, 'ReportTitle');
  HideProperty(TypeInfo(TNotifyEvent), TdxCompositionReportLink, 'OnChangeComponent');
  HideProperty(TypeInfo(TdxCustomDrawReportLinkHFEvent), TdxCompositionReportLink, 'OnCustomDrawPageFooter');
  HideProperty(TypeInfo(TdxCustomDrawReportLinkHFEvent), TdxCompositionReportLink, 'OnCustomDrawPageHeader');
  HideProperty(TypeInfo(TdxCustomDrawReportLinkTitleEvent), TdxCompositionReportLink, 'OnCustomDrawReportLinkTitle');
  HideProperty(TypeInfo(TdxMeasureReportLinkTitleEvent), TdxCompositionReportLink, 'OnMeasureReportLinkTitle');

//  HideProperty(TypeInfo(TComponent), TdxCustomReportLink, 'Component');
//  HideProperty(TypeInfo(TNotifyEvent), TdxCustomReportLink, 'OnChangeComponent');

  HideProperty(TypeInfo(TComponent), TdxPictureReportLink, 'Component');
  HideProperty(TypeInfo(TNotifyEvent), TdxPictureReportLink, 'OnChangeComponent');

  HideProperty(TypeInfo(TComponent), TdxTextReportLink, 'Component');
  HideProperty(TypeInfo(TNotifyEvent), TdxTextReportLink, 'OnChangeComponent');
end;

const
  ResFileName: string = 'dxPSDsgImgs';

function CreateImageList(AnAllocBy: Integer): TImageList;
begin
  Result := TImageList.Create(nil);
  Result.AllocBy := AnAllocBy;
end;

procedure dxPSRegisterReportLinkUnit(const AUnitName: string; ALinkClass: TdxReportLinkClass);
begin
  ReportLinkUnitNameList.AddObject(AUnitName, TObject(ALinkClass));
end;

procedure LoadImage(AImageList: TImageList; const AResName: string; AMasked: Boolean);
var
  Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create;
  try
    Bitmap.LoadFromResourceName(hInstance, AResName);
    AImageList.Height := Bitmap.Height;
    AImageList.Width := Bitmap.Width;
    if AMasked then
      AImageList.AddMasked(Bitmap, clDefault)
    else
      AImageList.Add(Bitmap, nil);
  finally
    Bitmap.Free;
  end;
end;

function CreatePaperImages: TImageList;
begin
  Result := CreateImageList(2);
  LoadImage(Result, 'IDB_DXPSDESIGN_PAPER_STANDARD', True);
  LoadImage(Result, 'IDB_DXPSDESIGN_PAPER_ENVELOPE', True);
end;

function CreateBinImages: TImageList;
begin
  Result := CreateImageList(2);
  LoadImage(Result, 'IDB_DXPSDESIGN_BIN_AUTOTRAY', True);
  LoadImage(Result, 'IDB_DXPSDESIGN_BIN_MANUALTRAY', True);
end;

function CreatePrintOrderImages: TImageList;
begin
  Result := CreateImageList(2);
  LoadImage(Result, 'IDB_DXPSDESIGN_PRINTORDER_OVERTHENDOWN', False);
  LoadImage(Result, 'IDB_DXPSDESIGN_PRINTORDER_DOWNTHENOVER', False);
end;

function CreatePaperOrientationImages: TImageList;
begin
  Result := CreateImageList(2);
  LoadImage(Result, 'IDB_DXPSDESIGN_PAPERORIENTATION_PORTRAIT', True);
  LoadImage(Result, 'IDB_DXPSDESIGN_PAPERORIENTATION_LANDSCAPE', True);
end;

initialization
  ilBins := CreateBinImages;
  ilPapers := CreatePaperImages;
  ilPrintOrders := CreatePrintOrderImages;
  ilPaperOrientations := CreatePaperOrientationImages;
  ReportLinkUnitNameList := TStringList.Create;

finalization
  FreeAndNil(ReportLinkUnitNameList);
  FreeAndNil(ilBins);
  FreeAndNil(ilPapers);
  FreeAndNil(ilPrintOrders);
  FreeAndNil(ilPaperOrientations);

end.
