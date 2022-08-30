{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
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

unit dxRichEdit.Printing;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

{.$DEFINE DXLOGGING}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Types, Windows, Messages, Classes, SysUtils, Graphics, Generics.Defaults, Generics.Collections, Controls,
  dxMessages, dxGDIPlusClasses,

  dxGenerics,
  dxRichEdit.Types,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.Graphics,
  dxRichEdit.Utils.Properties,
  dxRichEdit.InnerControl,
  dxRichEdit.Control.HitTest,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Selection,
  dxRichEdit.DocumentModel.Boxes.Simple,
  dxRichEdit.DocumentModel.Boxes,
  dxRichEdit.DocumentModel.Styles,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.DocumentLayout.UnitConverter,
  dxRichEdit.DocumentLayout.CommentPadding,
  dxRichEdit.DocumentLayout.Painters,
  dxRichEdit.LayoutEngine.DocumentFormatter,
  dxRichEdit.LayoutEngine.Formatter,
  dxRichEdit.View.Core,
  dxRichEdit.View.ViewInfo,
  dxRichEdit.View.Simple,
  dxRichEdit.View.PrintLayout,
  dxRichEdit.View.Draft,
  dxRichEdit.Platform.Win.Painter,
  dxRichEdit.Platform.Win.Control,
  dxRichEdit.Platform.Font;

type
  { TdxDocumentPrinterController }

  TdxDocumentPrinterController = class abstract
  public
    function BeginFormat(ADocumentModel: TdxDocumentModel): TdxGraphics; virtual; abstract;
    procedure EndFormat; virtual; abstract;
  end;

  { TdxDocumentPrinter }

  TdxDocumentPrinter = class abstract
  strict private
    FDocumentLayout: TdxDocumentLayout;
    FDocumentModel: TdxDocumentModel;
    FMeasurer: TdxBoxMeasurer;
    FController: TdxDocumentFormattingController;
    FFrameParagraphIndex: TdxParagraphIndex;
    function GetPages: TdxPageCollection;
  protected
    function GetPieceTable: TdxPieceTable; virtual;
    function GetPageNumberSource: TdxPage; virtual;
    procedure Format(AGraphics: TdxGraphics); overload;
    function Format(AGraphics: TdxGraphics; AMaxHeight: Integer): Integer; overload;
    procedure SubscribeToFormattingControllerEvents; virtual;
    procedure UnsubscribeFromFormattingControllerEvents; virtual;
    function CreateDocumentLayout(AMeasurer: TdxBoxMeasurer): TdxDocumentLayout; virtual;
    function CreateDocumentFormattingController(ADocumentLayout: TdxDocumentLayout): TdxDocumentFormattingController; virtual; abstract;
    function CreateDocumentPrinterController: TdxDocumentPrinterController; virtual; abstract;
    function CreateMeasurer(AGraphics: TdxGraphics): TdxBoxMeasurer; virtual; abstract;
    function PerformPrimaryFormat(AMaxHeight: Integer): Integer; virtual;
    procedure PerformFinalFormat; virtual;
    procedure PerformFinalFormatCore(AFinalFormatter: TdxParagraphFinalFormatter); virtual;
    procedure Export(const AExporter: IdxDocumentLayoutExporter); virtual;

    property PageNumberSource: TdxPage read GetPageNumberSource;
    property Controller: TdxDocumentFormattingController read FController;
  public
    constructor Create(ADocumentModel: TdxDocumentModel);
    destructor Destroy; override;
    procedure Format; overload;

    property Pages: TdxPageCollection read GetPages;
    property DocumentModel: TdxDocumentModel read FDocumentModel;
    property DocumentLayout: TdxDocumentLayout read FDocumentLayout;
    property Measurer: TdxBoxMeasurer read FMeasurer;
    property PieceTable: TdxPieceTable read GetPieceTable;
    property FrameParagraphIndex: TdxParagraphIndex read FFrameParagraphIndex write FFrameParagraphIndex;
  end;

  { TdxBrickDocumentPrinter }

  TdxBrickDocumentPrinter = class(TdxDocumentPrinter)
  protected
    function CreateMeasurer(AGraphics: TdxGraphics): TdxBoxMeasurer; override;
    function CreateDocumentFormattingController(ADocumentLayout: TdxDocumentLayout): TdxDocumentFormattingController; override;
    function CreateDocumentPrinterController: TdxDocumentPrinterController; override;
  end;

  { TdxPrintColumnController }

  TdxPrintColumnController = class(TdxColumnController)
  strict private
    FColumnLocation: TPoint;
    FSize: TSize;
  public
    function CalculateColumnBoundsCore(AColumnIndex: Integer): TRect; override;
    function GetNextColumnCore(AColumn: TdxColumn): TdxColumn; override;
    function GetCurrentPageBounds(ACurrentPage: TdxPage; ACurrentColumn: TdxColumn): TRect; override;
    function GetCurrentPageClientBounds(ACurrentPage: TdxPage; ACurrentColumn: TdxColumn): TRect; override;

    property ColumnLocation: TPoint read FColumnLocation write FColumnLocation;
    property Size: TSize read FSize write FSize;
  end;

  { TdxPrintPageController }

  TdxPrintPageController = class(TdxPageController)
  public
    constructor Create(ADocumentLayout: TdxDocumentLayout);
    function CreateHitTestCalculator(const ARequest: TdxRichEditHitTestRequest;
      AResult: TdxRichEditHitTestResult): TdxBoxHitTestCalculator; override;
    function CreatePageBoundsCalculator: TdxPageBoundsCalculator; override;
    function CompleteCurrentPageFormatting: TdxCompleteFormattingResult; override;
    function GetNextPage(AKeepFloatingObjects: Boolean): TdxPage; override;
  end;

  { TdxPrintPageAreController }

  TdxPrintPageAreController = class(TdxPageAreaController)
  public
    function CompleteCurrentAreaFormatting: TdxCompleteFormattingResult; override;
    function GetNextPageArea(AKeepFloatingObjects: Boolean): TdxPageArea; override;
  end;

  { TdxDocumentPrinterFormattingController }

  TdxDocumentPrinterFormattingController = class(TdxDocumentFormattingController)
  strict private
    function GetColumnLocation: TPoint;
    procedure SetColumnLocation(const AValue: TPoint);
    function GetColumnSize: TSize;
    procedure SetColumnSize(const AValue: TSize);
  protected
    function CreatePageController: TdxPageController; override;
    function CreateColumnController: TdxColumnController; override;
    function CreatePageAreaController: TdxPageAreaController; override;
    function CreateRowController: TdxRowsController; override;
  public
    constructor Create(ADocumentLayout: TdxDocumentLayout; APieceTable: TdxPieceTable);

    property ColumnLocation: TPoint read GetColumnLocation write SetColumnLocation;
    property ColumnSize: TSize read GetColumnSize write SetColumnSize;
  end;

  { TdxSimpleDocumentPrinter }

  TdxSimpleDocumentPrinter = class abstract(TdxDocumentPrinter)
  strict private
    FColumnLocation: TPoint;
    FColumnSize: TSize;
    function GetColumns: TdxColumnCollection;
  protected
    function CreateDocumentFormattingController(ADocumentLayout: TdxDocumentLayout): TdxDocumentFormattingController; override;
    function CreateDocumentPrinterFormattingController(ADocumentLayout: TdxDocumentLayout;
      APieceTable: TdxPieceTable): TdxDocumentPrinterFormattingController; virtual;
    function GetRowBottom(ARow: TdxRow): Integer; virtual;
    function CalculateFloatingObjectsBottom(APage: TdxPage): Integer; overload;
    function CalculateFloatingObjectsBottom(AFloatingObjects: TdxFloatingObjectBoxList): Integer; overload;
    procedure Export(const AExporter: IdxDocumentLayoutExporter); override;
    procedure PerformFinalFormatCore(AFinalFormatter: TdxParagraphFinalFormatter); override;
  public
    function GetEffectiveHeight: Integer;

    property Columns: TdxColumnCollection read GetColumns;
    property ColumnSize: TSize read FColumnSize write FColumnSize;
    property ColumnLocation: TPoint read FColumnLocation write FColumnLocation;
  end;

  { TdxTextBoxPrintPageController }

  TdxTextBoxPrintPageController = class(TdxPrintPageController)
  strict private
    FPieceTable: TdxPieceTable;
  protected
    function GetPieceTable: TdxPieceTable; override;
  public
    constructor Create(ADocumentLayout: TdxDocumentLayout; APieceTable: TdxPieceTable);
    function AppendFloatingObjectsToPage(APage: TdxPage): Boolean; override;
  end;

  { TdxTextBoxDocumentPrinterFormattingController }

  TdxTextBoxDocumentPrinterFormattingController = class(TdxDocumentPrinterFormattingController)
  protected
    function CreatePageController: TdxPageController; override;
  end;

  { TdxTextBoxFloatingObjectContentPrinter }

  TdxTextBoxFloatingObjectContentPrinter = class(TdxSimpleDocumentPrinter)
  strict private
    FTextBoxContentType: TdxContentTypeBase;
    FPageNumberSource: TdxPage;
    FMeasurer: TdxBoxMeasurer;
  protected
    function GetPieceTable: TdxPieceTable; override;
    function GetPageNumberSource: TdxPage; override;
    function CreateDocumentPrinterController: TdxDocumentPrinterController; override;
    function CreateDocumentPrinterFormattingController(ADocumentLayout: TdxDocumentLayout; APieceTable: TdxPieceTable): TdxDocumentPrinterFormattingController; override;
    function CreateMeasurer(AGr: TdxGraphics): TdxBoxMeasurer; override;
  public
    constructor Create(ATextBoxContentType: TdxContentTypeBase; APageNumberSource: TdxPage; AMeasurer: TdxBoxMeasurer);
    function Format(AMaxHeight: Integer): Integer; overload;
  end;

  { TdxPlatformDocumentPrinterController }

  TdxPlatformDocumentPrinterController = class(TdxDocumentPrinterController)
  strict private
    FGraphics: TdxGraphics;
    FModifier: TdxGraphicsToLayoutUnitsModifier;
  public
    function BeginFormat(ADocumentModel: TdxDocumentModel): TdxGraphics; override;
    procedure EndFormat; override;
  end;

  { TdxExtensionCommentInfo }

  TdxExtensionCommentInfo = record
    Comment: TdxComment;
    PageOrdinal: Integer;
    constructor Create(AComment: TdxComment; APageOrdinal: Integer);
  end;

implementation

uses
  Math, dxTypeHelpers, dxCore,

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

{ TdxDocumentPrinter }

constructor TdxDocumentPrinter.Create(ADocumentModel: TdxDocumentModel);
begin
  Assert(ADocumentModel <> nil);
  inherited Create;
  FDocumentModel := ADocumentModel;
  FFrameParagraphIndex := -1;
end;

destructor TdxDocumentPrinter.Destroy;
begin
  UnsubscribeFromFormattingControllerEvents;
  FreeAndNil(FController);
  inherited Destroy;
end;

function TdxDocumentPrinter.GetPages: TdxPageCollection;
begin
  if FController = nil then
    Result := nil
  else
    Result := FController.PageController.Pages;
end;

function TdxDocumentPrinter.GetPieceTable: TdxPieceTable;
begin
  Result := FDocumentModel.MainPieceTable;
end;

function TdxDocumentPrinter.GetPageNumberSource: TdxPage;
begin
  Result := nil;
end;

procedure TdxDocumentPrinter.Format;
var
  APrinterController: TdxDocumentPrinterController;
  AGraphics: TdxGraphics;
begin
  APrinterController := CreateDocumentPrinterController;
  try
    AGraphics := APrinterController.BeginFormat(DocumentModel);
    try
      Format(AGraphics);
    finally
      APrinterController.EndFormat;
    end;
  finally
    APrinterController.Free;
  end;
end;

procedure TdxDocumentPrinter.Format(AGraphics: TdxGraphics);
begin
  Format(AGraphics, MaxInt);
end;

function TdxDocumentPrinter.Format(AGraphics: TdxGraphics; AMaxHeight: Integer): Integer;
var
  AActualHeight: Integer;
begin
  Assert(FMeasurer = nil);
  FMeasurer := CreateMeasurer(AGraphics);
  FDocumentLayout := CreateDocumentLayout(FMeasurer);
  FController := CreateDocumentFormattingController(FDocumentLayout);
  SubscribeToFormattingControllerEvents;
  AActualHeight := PerformPrimaryFormat(AMaxHeight);
  PerformFinalFormat;
  Result := Min(AActualHeight, AMaxHeight);
end;

procedure TdxDocumentPrinter.SubscribeToFormattingControllerEvents;
begin
end;

procedure TdxDocumentPrinter.UnsubscribeFromFormattingControllerEvents;
begin
end;

function TdxDocumentPrinter.CreateDocumentLayout(AMeasurer: TdxBoxMeasurer): TdxDocumentLayout;
begin
  Result := TdxDocumentLayout.Create(DocumentModel, TdxExplicitBoxMeasurerProvider.Create(AMeasurer) as IdxBoxMeasurerProvider);
end;

function TdxDocumentPrinter.PerformPrimaryFormat(AMaxHeight: Integer): Integer;
var
  APage: TdxPage;
  AFormatter: TdxSimplePieceTablePrimaryFormatter;
begin
  APage := TdxPage.Create(PageNumberSource);
  try
    APage.PageOrdinal := 1;
    if FrameParagraphIndex <> -1 then
      FController.RowsController.FrameParagraphIndex := FrameParagraphIndex;
    AFormatter := TdxSimplePieceTablePrimaryFormatter.Create(PieceTable, FDocumentLayout.Measurer, Controller.RowsController, PieceTable.VisibleTextFilter, APage);
    try
      Result := AFormatter.Format(AMaxHeight, Controller);
      if FController.RowsController.FrameParagraphIndex <> -1 then
        FrameParagraphIndex := FController.RowsController.FrameParagraphIndex;
    finally
      AFormatter.Free;
    end;
  finally
    APage.Free;
  end;
end;

procedure TdxDocumentPrinter.PerformFinalFormat;
var
  AFinalFormatter: TdxParagraphFinalFormatter;
begin
  AFinalFormatter := TdxParagraphFinalFormatter.Create(FDocumentLayout);
  try
    AFinalFormatter.PieceTable := PieceTable;
    AFinalFormatter.BookmarkCalculator.ExportToPdf := True;
    PerformFinalFormatCore(AFinalFormatter);
  finally
    AFinalFormatter.Free;
  end;
end;

procedure TdxDocumentPrinter.PerformFinalFormatCore(AFinalFormatter: TdxParagraphFinalFormatter);
begin
  AFinalFormatter.FormatPages(Pages);
end;

procedure TdxDocumentPrinter.Export(const AExporter: IdxDocumentLayoutExporter);
begin
  Pages.ExportTo(AExporter);
end;

{ TdxBrickDocumentPrinter }

function TdxBrickDocumentPrinter.CreateMeasurer(AGraphics: TdxGraphics): TdxBoxMeasurer;
begin
  Result := TdxGdiBoxMeasurer.Create(DocumentModel, AGraphics);
end;

function TdxBrickDocumentPrinter.CreateDocumentFormattingController(ADocumentLayout: TdxDocumentLayout): TdxDocumentFormattingController;
begin
  Result := TdxPrintLayoutViewDocumentFormattingController.Create(ADocumentLayout, PieceTable);
end;

function TdxBrickDocumentPrinter.CreateDocumentPrinterController: TdxDocumentPrinterController;
begin
  Result := TdxPlatformDocumentPrinterController.Create;
end;

{ TdxPlatformDocumentPrinterController }

function TdxPlatformDocumentPrinterController.BeginFormat(ADocumentModel: TdxDocumentModel): TdxGraphics;
begin
  FGraphics := TdxGraphics.Create;
  FModifier := TdxGraphicsToLayoutUnitsModifier.Create(FGraphics, ADocumentModel.LayoutUnitConverter);
  Result := FGraphics;
end;

procedure TdxPlatformDocumentPrinterController.EndFormat;
begin
  FreeAndNil(FModifier);
  FreeAndNil(FGraphics);
end;

{ TdxPrintColumnController }

function TdxPrintColumnController.GetNextColumnCore(AColumn: TdxColumn): TdxColumn;
begin
  Result := TdxColumn.Create;
  if AColumn = nil then
    Result.Bounds := TRect.CreateSize(FColumnLocation.X, FColumnLocation.Y, FSize.Width, FSize.Height)
  else
    Result.Bounds := TRect.CreateSize(FColumnLocation.X, AColumn.Rows.Last.Bounds.Bottom, FSize.Width, FSize.Height);
end;

function TdxPrintColumnController.CalculateColumnBoundsCore(AColumnIndex: Integer): TRect;
begin
  Result := ColumnsBounds[AColumnIndex];
end;

function TdxPrintColumnController.GetCurrentPageBounds(ACurrentPage: TdxPage; ACurrentColumn: TdxColumn): TRect;
var
  APageBounds: TRect;
begin
  APageBounds := inherited GetCurrentPageBounds(ACurrentPage, ACurrentColumn);
  Result := APageBounds;
  Result.Union(ACurrentColumn.Bounds);
end;

function TdxPrintColumnController.GetCurrentPageClientBounds(ACurrentPage: TdxPage; ACurrentColumn: TdxColumn): TRect;
var
  APageClientBounds: TRect;
begin
  APageClientBounds := inherited GetCurrentPageClientBounds(ACurrentPage, ACurrentColumn);
  Result := APageClientBounds;
  Result.Union(ACurrentColumn.Bounds);
end;

{ TdxPrintPageController }

constructor TdxPrintPageController.Create(ADocumentLayout: TdxDocumentLayout);
begin
  inherited Create(ADocumentLayout, nil, nil);
end;

function TdxPrintPageController.CreatePageBoundsCalculator: TdxPageBoundsCalculator;
begin
  Result := TdxPageBoundsCalculator.Create(DocumentLayout.DocumentModel.ToDocumentLayoutUnitConverter);
end;

function TdxPrintPageController.CreateHitTestCalculator(const ARequest: TdxRichEditHitTestRequest;
  AResult: TdxRichEditHitTestResult): TdxBoxHitTestCalculator;
begin
  Result := TdxPrintLayoutViewBoxHitTestCalculator.Create(ARequest, AResult);
end;

function TdxPrintPageController.CompleteCurrentPageFormatting: TdxCompleteFormattingResult;
begin
  if Pages.Count > 0 then
    Result := inherited CompleteCurrentPageFormatting
  else
    Result := TdxCompleteFormattingResult.Success;
end;

function TdxPrintPageController.GetNextPage(AKeepFloatingObjects: Boolean): TdxPage;
begin
  if Pages.Count > 0 then
    Result := Pages.Last
  else
    Result := inherited GetNextPage(AKeepFloatingObjects);
end;

{ TdxPrintPageAreController }

function TdxPrintPageAreController.CompleteCurrentAreaFormatting: TdxCompleteFormattingResult;
begin
  if (PageController.Pages.Count > 0) and (Areas.Count > 0) then
    Exit(TdxCompleteFormattingResult.Success);
  Result := inherited CompleteCurrentAreaFormatting;
end;

function TdxPrintPageAreController.GetNextPageArea(AKeepFloatingObjects: Boolean): TdxPageArea;
begin
  if (PageController.Pages.Count > 0) and (Areas.Count > 0) then
    Exit(Areas.Last);
  Result := inherited GetNextPageArea(AKeepFloatingObjects);
end;

{ TdxDocumentPrinterFormattingController }

constructor TdxDocumentPrinterFormattingController.Create(ADocumentLayout: TdxDocumentLayout; APieceTable: TdxPieceTable);
begin
  inherited Create(ADocumentLayout, APieceTable, nil, nil);
end;

function TdxDocumentPrinterFormattingController.GetColumnLocation: TPoint;
var
  AColumnController: TdxPrintColumnController;
begin
  AColumnController := TdxPrintColumnController(ColumnController);
  Result := AColumnController.ColumnLocation;
end;

procedure TdxDocumentPrinterFormattingController.SetColumnLocation(const AValue: TPoint);
var
  AColumnController: TdxPrintColumnController;
begin
  if AValue.IsEqual(ColumnLocation) then
    Exit;
  AColumnController := TdxPrintColumnController(ColumnController);
  AColumnController.ColumnLocation := AValue;
  Reset(False);
end;

function TdxDocumentPrinterFormattingController.GetColumnSize: TSize;
var
  AColumnController: TdxPrintColumnController;
begin
  AColumnController := TdxPrintColumnController(ColumnController);
  Result := AColumnController.Size;
end;

procedure TdxDocumentPrinterFormattingController.SetColumnSize(const AValue: TSize);
var
  AColumnController: TdxPrintColumnController;
begin
  if AValue.IsEqual(ColumnSize) then
    Exit;
  AColumnController := TdxPrintColumnController(ColumnController);
  AColumnController.Size := AValue;
  Reset(False);
end;

function TdxDocumentPrinterFormattingController.CreatePageController: TdxPageController;
begin
  Result := TdxPrintPageController.Create(DocumentLayout);
end;

function TdxDocumentPrinterFormattingController.CreateColumnController: TdxColumnController;
begin
  Result := TdxPrintColumnController.Create(PageAreaController);
end;

function TdxDocumentPrinterFormattingController.CreatePageAreaController: TdxPageAreaController;
begin
  Result := TdxPrintPageAreController.Create(PageController);
end;

function TdxDocumentPrinterFormattingController.CreateRowController: TdxRowsController;
begin
  Result := TdxRowsController.Create(PieceTable, ColumnController, DocumentModel.LayoutOptions.PrintLayoutView.MatchHorizontalTableIndentsToTextEdge);
end;

{ TdxSimpleDocumentPrinter }

function TdxSimpleDocumentPrinter.GetColumns: TdxColumnCollection;
begin
  if Controller = nil then
    Result := nil
  else
    Result := Controller.ColumnController.Columns;
end;

function TdxSimpleDocumentPrinter.CreateDocumentFormattingController(
  ADocumentLayout: TdxDocumentLayout): TdxDocumentFormattingController;
var
  AFormattingController: TdxDocumentPrinterFormattingController;
begin
  AFormattingController := CreateDocumentPrinterFormattingController(ADocumentLayout, PieceTable);
  AFormattingController.ColumnLocation := ColumnLocation;
  AFormattingController.ColumnSize := ColumnSize;
  Result := AFormattingController;
end;

function TdxSimpleDocumentPrinter.CreateDocumentPrinterFormattingController(ADocumentLayout: TdxDocumentLayout;
  APieceTable: TdxPieceTable): TdxDocumentPrinterFormattingController;
begin
  Result := TdxDocumentPrinterFormattingController.Create(ADocumentLayout, APieceTable);
end;

function TdxSimpleDocumentPrinter.GetEffectiveHeight: Integer;
var
  AColumnCount, ARowsCount, AFloatingObjectsBottom: Integer;
  AColumn: TdxColumn;
  ALastRow: TdxRow;
begin
  AColumnCount := Columns.Count;
  AColumn := Columns[AColumnCount - 1];
  ARowsCount := AColumn.Rows.Count;
  ALastRow := AColumn.Rows[ARowsCount - 1];
  if (((AColumnCount = 1) and (ARowsCount = 1)) and (ALastRow.Boxes.Count = 1)) and
      (ALastRow.Boxes[0] is TdxParagraphMarkBox) then
    Exit(0);

  AFloatingObjectsBottom := CalculateFloatingObjectsBottom(Pages[0]);
  Result := Math.Max(AFloatingObjectsBottom, GetRowBottom(ALastRow));
end;

function TdxSimpleDocumentPrinter.GetRowBottom(ARow: TdxRow): Integer;
begin
  Result := ARow.Bounds.Bottom;
end;

function TdxSimpleDocumentPrinter.CalculateFloatingObjectsBottom(APage: TdxPage): Integer;
begin
  Result := 0;
  Result := Math.Max(CalculateFloatingObjectsBottom(APage.BackgroundFloatingObjects), Result);
  Result := Math.Max(CalculateFloatingObjectsBottom(APage.FloatingObjects), Result);
  Result := Math.Max(CalculateFloatingObjectsBottom(APage.ForegroundFloatingObjects), Result);
end;

function TdxSimpleDocumentPrinter.CalculateFloatingObjectsBottom(AFloatingObjects: TdxFloatingObjectBoxList): Integer;
var
  ACount, I: Integer;
begin
  Result := 0;
  ACount := AFloatingObjects.Count;
  for I := 0 to ACount - 1 do
    Result := Math.Max(AFloatingObjects[I].ExtendedBounds.Bottom, Result);
end;

procedure TdxSimpleDocumentPrinter.Export(const AExporter: IdxDocumentLayoutExporter);
var
  AColumns: TdxColumnCollection;
begin
  AColumns := Columns;
  if AColumns <> nil then
    AColumns.ExportTo(AExporter);
end;

procedure TdxSimpleDocumentPrinter.PerformFinalFormatCore(AFinalFormatter: TdxParagraphFinalFormatter);
var
  I: Integer;
  AColumns: TdxColumnCollection;
begin
  AColumns := Columns;
  if AColumns <> nil then
    for I := 0 to AColumns.Count - 1 do
      AFinalFormatter.FormatColumn(AColumns[I]);
end;

{ TdxTextBoxPrintPageController }

constructor TdxTextBoxPrintPageController.Create(ADocumentLayout: TdxDocumentLayout; APieceTable: TdxPieceTable);
begin
  inherited Create(ADocumentLayout);
  Assert(APieceTable <> nil);
  FPieceTable := APieceTable;
end;

function TdxTextBoxPrintPageController.GetPieceTable: TdxPieceTable;
begin
  Result := FPieceTable;
end;

function TdxTextBoxPrintPageController.AppendFloatingObjectsToPage(APage: TdxPage): Boolean;
begin
  Result := True;
end;

{ TdxTextBoxDocumentPrinterFormattingController }

function TdxTextBoxDocumentPrinterFormattingController.CreatePageController: TdxPageController;
begin
  Result := TdxTextBoxPrintPageController.Create(DocumentLayout, PieceTable);
end;

{ TdxTextBoxFloatingObjectContentPrinter }

constructor TdxTextBoxFloatingObjectContentPrinter.Create(ATextBoxContentType: TdxContentTypeBase;
  APageNumberSource: TdxPage; AMeasurer: TdxBoxMeasurer);
begin
  inherited Create(TdxDocumentModel(ATextBoxContentType.DocumentModel));
  FTextBoxContentType := ATextBoxContentType;
  FPageNumberSource := APageNumberSource;
  FMeasurer := AMeasurer;
end;

function TdxTextBoxFloatingObjectContentPrinter.GetPieceTable: TdxPieceTable;
begin
  Result := TdxPieceTable(FTextBoxContentType.PieceTable);
end;

function TdxTextBoxFloatingObjectContentPrinter.GetPageNumberSource: TdxPage;
begin
  Result := FPageNumberSource;
end;

function TdxTextBoxFloatingObjectContentPrinter.Format(AMaxHeight: Integer): Integer;
var
  AOldPieceTable: TdxPieceTable;
begin
  AOldPieceTable := TdxPieceTable(FMeasurer.PieceTable);
  FMeasurer.PieceTable := PieceTable;
  try
    Result := Format(nil, AMaxHeight);
  finally
    FMeasurer.PieceTable := AOldPieceTable;
  end;
end;

function TdxTextBoxFloatingObjectContentPrinter.CreateDocumentPrinterController: TdxDocumentPrinterController;
begin
  Result := nil;
end;

function TdxTextBoxFloatingObjectContentPrinter.CreateDocumentPrinterFormattingController(ADocumentLayout: TdxDocumentLayout; APieceTable: TdxPieceTable): TdxDocumentPrinterFormattingController;
begin
  Result := TdxTextBoxDocumentPrinterFormattingController.Create(ADocumentLayout, APieceTable);
end;

function TdxTextBoxFloatingObjectContentPrinter.CreateMeasurer(AGr: TdxGraphics): TdxBoxMeasurer;
begin
  Result := FMeasurer;
end;

{ TdxExtensionCommentInfo }

constructor TdxExtensionCommentInfo.Create(AComment: TdxComment; APageOrdinal: Integer);
begin
  Comment := AComment;
  PageOrdinal := APageOrdinal;
end;


end.
