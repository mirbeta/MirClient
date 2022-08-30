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

unit dxPSRichEditControlLnk.Printing;

{$I cxVer.inc}

{.$DEFINE DXLOGGING}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Types, Windows, Messages, Classes, SysUtils, Graphics, Generics.Defaults, Generics.Collections, Controls,
  StdCtrls, Forms, ActiveX, dxCoreClasses, cxGeometry, cxLookAndFeels, cxGraphics, cxControls, cxClasses,
  dxMessages, dxGDIPlusClasses, cxLookAndFeelPainters, dxPSCore, dxSkinsCore, dxSkinInfo,

  dxRichEdit.Types,
  dxRichEdit.InnerControl,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Boxes.Simple,
  dxRichEdit.DocumentModel.Boxes,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentLayout.UnitConverter,
  dxRichEdit.View.Core,
  dxRichEdit.Printing,
  dxPSRichEditControlLnk.DocumentExporter;

type
  { TdxRichEditPrinterBase }

  TdxRichEditPrinterBase = class abstract (TcxIUnknownObject)
  strict private
    FDocumentDPI: Integer;
    FDocumentModel: TdxDocumentModel;
    FExportCommentDocumentModel: TdxDocumentModel;
    FExportDocumentModel: TdxDocumentModel;
    FExtensionCommentId: TDictionary<Integer, TdxExtensionCommentInfo>;
    FFontScaleFactor: Single;
    FMeasurer: TdxBoxMeasurer;
    FServer: TdxInnerRichEditDocumentServer;
  protected
    function CalculateDocumentDPI(ADocumentModel: TdxDocumentModel): Integer;
    function CalculatePrintDocumentLayout: TdxDocumentLayout; virtual;
    procedure BeginDocumentRendering; virtual;
    procedure EndDocumentRendering; virtual;
    procedure InitializeEmptyDocumentModel(const ADocumentModel: TdxDocumentModel); virtual;
    function CreateDocumentPrinter(ADocumentModel: TdxDocumentModel): TdxDocumentPrinter; virtual; abstract;

    property DocumentModel: TdxDocumentModel read FDocumentModel;
    property ExtensionCommentId: TDictionary<Integer, TdxExtensionCommentInfo> read FExtensionCommentId write FExtensionCommentId;
    property Server: TdxInnerRichEditDocumentServer read FServer;
  public
    constructor Create(ADocumentModel: TdxDocumentModel); overload;
    constructor Create(AServer: TdxInnerRichEditDocumentServer); overload;
    destructor Destroy; override;
    procedure Initialize(ALink: TBasedxReportLink); virtual;
    procedure Finalize(ALink: TBasedxReportLink); virtual;
    //
    property DocumentDPI: Integer read FDocumentDPI;
    property FontScaleFactor: Single read FFontScaleFactor;
    property Measurer: TdxBoxMeasurer read FMeasurer;
  end;

  { TdxRichEditPrinter }

  TdxRichEditPrinter = class(TdxRichEditPrinterBase)
  strict private
    function GetCreatesIntersectedBricks: Boolean;
  private
    function GetPageCount: Integer;
  protected
    FDocumentLayout: TdxDocumentLayout;
    FCommentDocumentLayout: TdxDocumentLayout;
    function GetUseGdiPlus: Boolean; virtual;
    function CreateDocumentExporter(ADocumentModel: TdxDocumentModel): TdxPrintingDocumentExporter; virtual;
    function CreateDocumentPrinter(ADocumentModel: TdxDocumentModel): TdxDocumentPrinter; override;

    property UseGdiPlus: Boolean read GetUseGdiPlus;
  public
    function HasPropertyEditor: Boolean;
    procedure AcceptChanges;
    procedure RejectChanges;
    procedure ShowHelp;
    function SupportsHelp: Boolean;
    procedure CreateArea(ALink: TBasedxReportLink);
    procedure Initialize(ALink: TBasedxReportLink); override;
    procedure Finalize(ALink: TBasedxReportLink); override;

    property CreatesIntersectedBricks: Boolean read GetCreatesIntersectedBricks;
    property PageCount: Integer read GetPageCount;
    property DocumentLayout: TdxDocumentLayout read FDocumentLayout;
  end;

  { TdxRichEditControlPrinter }

  TdxRichEditControlPrinter = class(TdxRichEditPrinter)
  strict private
    FControl: TdxInnerRichEditControl;
  protected
    function GetUseGdiPlus: Boolean; override;
    procedure BeginDocumentRendering; override;
    procedure EndDocumentRendering; override;

    property InnerControl: TdxInnerRichEditControl read FControl;
  public
    constructor Create(AControl: TdxInnerRichEditControl);
    class function GetDocumentModel(AControl: TdxInnerRichEditControl): TdxDocumentModel; static;
  end;

implementation

uses
  Math, Dialogs, ShellAPI, ComObj, ShlObj, UxTheme, DwmApi, dxTypeHelpers, dxCore, dxPrnPg,

  dxRichEdit.DocumentLayout.UnitDocumentConverter,
  dxRichEdit.DocumentLayout.UnitPixelsConverter,
  dxRichEdit.DocumentLayout.UnitTwipsConverter,
  dxRichEdit.Utils.TextColors,
  dxRichEdit.Utils.Mouse,
  dxRichEdit.DocumentLayout,
  dxRichEdit.LayoutEngine.BoxMeasurer,
  dxRichEdit.Platform.Win.FontCache,
  dxRichEdit.DocumentModel.DocumentProperties,
  dxRichEdit.DocumentModel.Fields,
  dxRichEdit.Options;

{ TdxRichEditPrinterBase }

constructor TdxRichEditPrinterBase.Create(AServer: TdxInnerRichEditDocumentServer);
begin
  Assert(AServer <> nil);
  Create(AServer.DocumentModel);
  FServer := AServer;
end;

constructor TdxRichEditPrinterBase.Create(ADocumentModel: TdxDocumentModel);
begin
  inherited Create;
  Assert(ADocumentModel <> nil);
  FDocumentModel := ADocumentModel;
  FExtensionCommentId := TDictionary<Integer, TdxExtensionCommentInfo>.Create;
end;

destructor TdxRichEditPrinterBase.Destroy;
begin
  FreeAndNil(FExtensionCommentId);
  inherited Destroy;
end;

function TdxRichEditPrinterBase.CalculateDocumentDPI(ADocumentModel: TdxDocumentModel): Integer;
begin
  Result := Round(ADocumentModel.LayoutUnitConverter.Dpi);
end;

function TdxRichEditPrinterBase.CalculatePrintDocumentLayout: TdxDocumentLayout;
var
  APrinter: TdxDocumentPrinter;
begin
  BeginDocumentRendering;
  try
    FExportDocumentModel := DocumentModel.CreateDocumentModelForExport(InitializeEmptyDocumentModel);
    FExportDocumentModel.EnsureImagesLoadComplete;
    FDocumentDPI := CalculateDocumentDPI(FExportDocumentModel);
    FFontScaleFactor := FExportDocumentModel.LayoutUnitConverter.FontSizeScaleForPrinting;

    APrinter := CreateDocumentPrinter(FExportDocumentModel);
    try
      APrinter.Format;
      FMeasurer := APrinter.Measurer;
      Result := APrinter.DocumentLayout;
    finally
      APrinter.Free;
    end;
  finally
    EndDocumentRendering;
  end;
end;


procedure TdxRichEditPrinterBase.BeginDocumentRendering;
begin
end;

procedure TdxRichEditPrinterBase.EndDocumentRendering;
begin
end;

procedure TdxRichEditPrinterBase.Initialize(ALink: TBasedxReportLink);
begin
end;

procedure TdxRichEditPrinterBase.Finalize(ALink: TBasedxReportLink);
begin
  FreeAndNil(FMeasurer);
  FreeAndNil(FExportDocumentModel);
  FreeAndNil(FExportCommentDocumentModel);
end;

procedure TdxRichEditPrinterBase.InitializeEmptyDocumentModel(const ADocumentModel: TdxDocumentModel);
begin
end;


{ TdxRichEditPrinter }

function TdxRichEditPrinter.GetCreatesIntersectedBricks: Boolean;
begin
  Result := True;
end;

function TdxRichEditPrinter.GetPageCount: Integer;
begin
  if FDocumentLayout <> nil then
    Result := FDocumentLayout.Pages.Count
  else
    Result := 1;
end;

function TdxRichEditPrinter.HasPropertyEditor: Boolean;
begin
  Result := False;
end;

procedure TdxRichEditPrinter.AcceptChanges;
begin
end;

procedure TdxRichEditPrinter.RejectChanges;
begin
end;

procedure TdxRichEditPrinter.ShowHelp;
begin
end;

function TdxRichEditPrinter.SupportsHelp: Boolean;
begin
  Result := False;
end;

function TdxRichEditPrinter.GetUseGdiPlus: Boolean;
begin
  Result := False;
end;

procedure TdxRichEditPrinter.CreateArea(ALink: TBasedxReportLink);
var
  AExporter: TdxPrintingDocumentExporter;
begin
  begin
    AExporter := CreateDocumentExporter(TdxDocumentModel(FDocumentLayout.DocumentModel));
    try
      AExporter.Export(FDocumentLayout, ALink);
    finally
      AExporter.Free;
    end;
  end;
end;

function TdxRichEditPrinter.CreateDocumentExporter(ADocumentModel: TdxDocumentModel): TdxPrintingDocumentExporter;
begin
  Result := TdxPrintingDocumentExporter.Create(ADocumentModel, TdxTextColors.Defaults);
end;

procedure TdxRichEditPrinter.Initialize(ALink: TBasedxReportLink);
begin
  inherited Initialize(ALink);
  FDocumentLayout := CalculatePrintDocumentLayout;


end;

procedure TdxRichEditPrinter.Finalize(ALink: TBasedxReportLink);
begin
  inherited Finalize(ALink);
  FreeAndNil(FDocumentLayout);
  FreeAndNil(FCommentDocumentLayout);
end;

function TdxRichEditPrinter.CreateDocumentPrinter(ADocumentModel: TdxDocumentModel): TdxDocumentPrinter;
begin
  Result := TdxBrickDocumentPrinter.Create(ADocumentModel);
end;

{ TdxRichEditControlPrinter }

constructor TdxRichEditControlPrinter.Create(AControl: TdxInnerRichEditControl);
begin
  inherited Create(AControl);
  FControl := AControl;
end;

class function TdxRichEditControlPrinter.GetDocumentModel(AControl: TdxInnerRichEditControl): TdxDocumentModel;
begin
  Assert(AControl <> nil);
  Result := AControl.DocumentModel;
end;

function TdxRichEditControlPrinter.GetUseGdiPlus: Boolean;
begin
  Result := False;
end;

procedure TdxRichEditControlPrinter.BeginDocumentRendering;
begin
  InnerControl.BeginDocumentRendering;
end;

procedure TdxRichEditControlPrinter.EndDocumentRendering;
begin
  InnerControl.EndDocumentRendering;
end;


end.
