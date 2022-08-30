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

unit dxRichEdit.Api.Sections;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes, Generics.Defaults, Generics.Collections,

  dxRichEdit.NativeApi,
  dxRichEdit.Api.NativeDocument,

  dxRichEdit.Types,
  dxRichEdit.Utils.Types,
  dxRichEdit.InnerControl,

  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Section;

type
  { TdxNativeSection }

  TdxNativeSection = class(TInterfacedObject, IdxRichEditSection)
  strict private
    FDocument: TdxNativeDocument;
    FInnerSection: TdxSection;
    FMargins: IdxRichEditSectionMargins;
    FPage: IdxRichEditSectionPage;
    FColumns: IdxRichEditSectionColumns;
    FLineNumbering: IdxRichEditSectionLineNumbering;
    FPageNumbering: IdxRichEditSectionPageNumbering;
    FParagraphs: IdxRichEditReadOnlyParagraphCollection;
    FIsValid: Boolean;
    procedure CheckValid;
    function CreateSubDocument(APieceTable: TdxPieceTable;
      AServer: TdxInnerRichEditDocumentServer): IdxRichEditSubDocument;

    function IsLinkedToPrevious(AType: TdxRichEditHeaderFooterType;
      AHeadersFooters: TdxSectionHeadersFootersBase): Boolean;
    procedure LinkToPrevious(AType: TdxRichEditHeaderFooterType;
      AHeadersFooters: TdxSectionHeadersFootersBase);
    procedure UnlinkFromPrevious(AType: TdxRichEditHeaderFooterType;
      AHeadersFooters: TdxSectionHeadersFootersBase);

    function IsLinkedToNext(AType: TdxRichEditHeaderFooterType;
      AHeadersFooters: TdxSectionHeadersFootersBase): Boolean;
    procedure LinkToNext(AType: TdxRichEditHeaderFooterType;
      AHeadersFooters: TdxSectionHeadersFootersBase);
    procedure UnlinkFromNext(AType: TdxRichEditHeaderFooterType;
      AHeadersFooters: TdxSectionHeadersFootersBase);
  protected
    function GetColumns: IdxRichEditSectionColumns;
    function GetDifferentFirstPage: Boolean;
    function GetFirstPageTray: Integer;
    function GetLineNumbering: IdxRichEditSectionLineNumbering;
    function GetMargins: IdxRichEditSectionMargins;
    function GetOtherPagesTray: Integer;
    function GetPage: IdxRichEditSectionPage;
    function GetPageNumbering: IdxRichEditSectionPageNumbering;
    function GetParagraphs: IdxRichEditReadOnlyParagraphCollection;
    function GetStartType: TdxRichEditSectionStartType;
    procedure SetDifferentFirstPage(const Value: Boolean);
    procedure SetFirstPageTray(const Value: Integer);
    procedure SetOtherPagesTray(const Value: Integer);
    procedure SetStartType(const Value: TdxRichEditSectionStartType);

    function BeginUpdateHeader: IdxRichEditSubDocument; overload;
    function BeginUpdateHeader(AType: TdxRichEditHeaderFooterType): IdxRichEditSubDocument; overload;
    procedure EndUpdateHeader(const ADocument: IdxRichEditSubDocument);
    function HasHeader(AType: TdxRichEditHeaderFooterType): Boolean;

    function BeginUpdateFooter: IdxRichEditSubDocument; overload;
    function BeginUpdateFooter(AType: TdxRichEditHeaderFooterType): IdxRichEditSubDocument; overload;
    procedure EndUpdateFooter(const ADocument: IdxRichEditSubDocument);
    function HasFooter(AType: TdxRichEditHeaderFooterType): Boolean;

    function IsHeaderLinkedToPrevious: Boolean; overload;
    procedure LinkHeaderToPrevious; overload;
    procedure UnlinkHeaderFromPrevious; overload;

    function IsFooterLinkedToPrevious: Boolean; overload;
    procedure LinkFooterToPrevious; overload;
    procedure UnlinkFooterFromPrevious; overload;

    function IsHeaderLinkedToPrevious(AType: TdxRichEditHeaderFooterType): Boolean; overload;
    procedure LinkHeaderToPrevious(AType: TdxRichEditHeaderFooterType); overload;
    procedure UnlinkHeaderFromPrevious(AType: TdxRichEditHeaderFooterType); overload;

    function IsFooterLinkedToPrevious(AType: TdxRichEditHeaderFooterType): Boolean; overload;
    procedure LinkFooterToPrevious(AType: TdxRichEditHeaderFooterType); overload;
    procedure UnlinkFooterFromPrevious(AType: TdxRichEditHeaderFooterType); overload;

    function IsHeaderLinkedToNext: Boolean; overload;
    procedure LinkHeaderToNext; overload;
    procedure UnlinkHeaderFromNext; overload;

    function IsFooterLinkedToNext: Boolean; overload;
    procedure LinkFooterToNext; overload;
    procedure UnlinkFooterFromNext; overload;

    function IsHeaderLinkedToNext(AType: TdxRichEditHeaderFooterType): Boolean; overload;
    procedure LinkHeaderToNext(AType: TdxRichEditHeaderFooterType); overload;
    procedure UnlinkHeaderFromNext(AType: TdxRichEditHeaderFooterType); overload;

    function IsFooterLinkedToNext(AType: TdxRichEditHeaderFooterType): Boolean; overload;
    procedure LinkFooterToNext(AType: TdxRichEditHeaderFooterType); overload;
    procedure UnlinkFooterFromNext(AType: TdxRichEditHeaderFooterType); overload;
  public
    constructor Create(ADocument: TdxNativeDocument; AInnerSection: TdxSection);
    property InnerSection: TdxSection read FInnerSection;
    property Document: TdxNativeDocument read FDocument;

    property IsValid: Boolean read FIsValid write FIsValid;
  end;

implementation

uses
  Contnrs, dxCore,

  dxRichEdit.DocumentModel.SectionFormatting,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.Utils.Exceptions.Strs,
  dxRichEdit.Api.Paragraphs;

type

  { TdxNativeSectionMargins }

  TdxNativeSectionMargins = class(TInterfacedObject, IdxRichEditSectionMargins)
  strict private
    FDocument: TdxNativeDocument;
    FMargins: TdxSectionMargins;

    function GetBottom: Single;
    function GetFooterOffset: Single;
    function GetHeaderOffset: Single;
    function GetLeft: Single;
    function GetRight: Single;
    function GetTop: Single;
    procedure SetBottom(const Value: Single);
    procedure SetFooterOffset(const Value: Single);
    procedure SetHeaderOffset(const Value: Single);
    procedure SetLeft(const Value: Single);
    procedure SetRight(const Value: Single);
    procedure SetTop(const Value: Single);
  public
    constructor Create(ADocument: TdxNativeDocument; AMargins: TdxSectionMargins);

    property Document: TdxNativeDocument read FDocument;
    property Margins: TdxSectionMargins read FMargins;

    property Bottom: Single read GetBottom write SetBottom;
    property FooterOffset: Single read GetFooterOffset write SetFooterOffset;
    property HeaderOffset: Single read GetHeaderOffset write SetHeaderOffset;
    property Left: Single read GetLeft write SetLeft;
    property Right: Single read GetRight write SetRight;
    property Top: Single read GetTop write SetTop;
  end;

  { TdxNativeSectionPage }

  TdxNativeSectionPage = class(TInterfacedObject, IdxRichEditSectionPage)
  private
    type
      TSetValueDelegate<T> = reference to procedure (const Value: T);
  strict private
    FDocument: TdxNativeDocument;
    FPage: TdxSectionPage;

    function GetHeight: Single;
    function GetLandscape: Boolean;
    function GetPaperKind: TdxRichEditPaperKind;
    function GetWidth: Single;
    procedure SetHeight(const Value: Single);
    procedure SetLandscape(const Value: Boolean);
    procedure SetPaperKind(const Value: TdxRichEditPaperKind);
    procedure SetWidth(const Value: Single);
  protected
    function GetActualHeight: Single; virtual;
    function GetActualWidth: Single; virtual;
    procedure SetActualHeight(const Value: Single); virtual;
    procedure SetActualWidth(const Value: Single); virtual;

    procedure SetPageProperty<T>(const ASetter: TSetValueDelegate<T>; const Value: T);
    procedure SetLandscapeCore(const Value: Boolean); virtual;
    procedure SetPageHeightCore(const Value: Integer); virtual;
    procedure SetPageWidthCore(const Value: Integer); virtual;
    procedure SetPaperKindCore(const Value: TdxPaperKind); virtual;

    property ActualWidth: Single read GetActualWidth write SetActualWidth;
    property ActualHeight: Single read GetActualHeight write SetActualHeight;
  public
    constructor Create(ADocument: TdxNativeDocument; APage: TdxSectionPage);

    property Document: TdxNativeDocument read FDocument;
    property Page: TdxSectionPage read FPage;

    property Height: Single read GetHeight write SetHeight;
    property Landscape: Boolean read GetLandscape write SetLandscape;
    property PaperKind: TdxRichEditPaperKind read GetPaperKind write SetPaperKind;
    property Width: Single read GetWidth write SetWidth;
  end;

  { TdxNativeSectionColumn }

  TdxNativeSectionColumn = class(TInterfacedObject, IdxRichEditSectionColumn)
  strict private
    FWidth: Single;
    FSpacing: Single;
    function GetSpacing: Single;
    function GetWidth: Single;
    procedure SetSpacing(const Value: Single);
    procedure SetWidth(const Value: Single);
  public
    constructor Create(AWidth, ASpacing: Single);

    property Width: Single read FWidth write FWidth;
    property Spacing: Single read FSpacing write FSpacing;
  end;

  { TdxNativeSectionColumns }

  TdxNativeSectionColumns = class(TInterfacedObject, IdxRichEditSectionColumns)
  strict private
    FDocument: TdxNativeDocument;
    FColumns: TdxSectionColumns;
    FPage: IdxRichEditSectionPage;
    function GetCount: Integer;
  protected
    function CreateNonUniformColumns: IdxRichEditSectionColumnCollection; virtual;
    function IsEqualWidthColumns(const AColumns: IdxRichEditSectionColumnCollection): Boolean; virtual;
    procedure SetUniformColumns(ASpacing: Single; AColumnCount: Integer); virtual;
    procedure SetNonUniformColumns(const AColumns: IdxRichEditSectionColumnCollection); virtual;
  public
    constructor Create(ADocument: TdxNativeDocument;
      AColumns: TdxSectionColumns; const APage: IdxRichEditSectionPage);

    function GetColumns: IdxRichEditSectionColumnCollection;
    procedure SetColumns(const AColumns: IdxRichEditSectionColumnCollection);
    function CreateUniformColumns(AColumnWidth, AColumnSpacing: Single; AColumnCount: Integer): IdxRichEditSectionColumnCollection; overload;
    function CreateUniformColumns(const APage: IdxRichEditSectionPage; AColumnSpacing: Single; AColumnCount: Integer): IdxRichEditSectionColumnCollection; overload;

    function GetActualColumnCount: Integer;

    property Columns: TdxSectionColumns read FColumns;
    property Page: IdxRichEditSectionPage read FPage;
    property Document: TdxNativeDocument read FDocument;
    property Count: Integer read GetCount;
  end;

  { TdxNativeSectionColumnCollection }

  TdxNativeSectionColumnCollection = class(TdxIUnknownList<IdxRichEditSectionColumn>, IdxRichEditSectionColumnCollection)
  strict private
    function GetCount: Integer;
  end;

  { TdxNativeSectionLineNumbering }

  TdxNativeSectionLineNumbering = class(TInterfacedObject, IdxRichEditSectionLineNumbering)
  strict private
    FDocument: TdxNativeDocument;
    FLineNumbering: TdxSectionLineNumbering;
    function GetCountBy: Integer;
    function GetDistance: Single;
    function GetRestartType: TdxRichEditLineNumberingRestart;
    function GetStart: Integer;
    procedure SetCountBy(const Value: Integer);
    procedure SetDistance(const Value: Single);
    procedure SetRestartType(const Value: TdxRichEditLineNumberingRestart);
    procedure SetStart(const Value: Integer);
  public
    constructor Create(ADocument: TdxNativeDocument; ALineNumbering: TdxSectionLineNumbering);

    property Document: TdxNativeDocument read FDocument;
    property LineNumbering: TdxSectionLineNumbering read FLineNumbering;

    property CountBy: Integer read GetCountBy write SetCountBy;
    property Distance: Single read GetDistance write SetDistance;
    property RestartType: TdxRichEditLineNumberingRestart read GetRestartType write SetRestartType;
    property Start: Integer read GetStart write SetStart;
  end;

  { TdxNativeSectionPageNumbering }

  TdxNativeSectionPageNumbering = class(TInterfacedObject, IdxRichEditSectionPageNumbering)
  strict private
    FDocument: TdxNativeDocument;
    FPageNumbering: TdxSectionPageNumbering;
    function GetContinueNumbering: Boolean;
    function GetFirstPageNumber: Integer;
    function GetNumberingFormat: TdxRichEditNumberingFormat;
    function GetStart: Integer;
    procedure SetContinueNumbering(const Value: Boolean);
    procedure SetFirstPageNumber(const Value: Integer);
    procedure SetNumberingFormat(const Value: TdxRichEditNumberingFormat);
    procedure SetStart(const Value: Integer);
  public
    constructor Create(ADocument: TdxNativeDocument; APageNumbering: TdxSectionPageNumbering);

    property Document: TdxNativeDocument read FDocument;
    property PageNumbering: TdxSectionPageNumbering read FPageNumbering;

    property ContinueNumbering: Boolean read GetContinueNumbering write SetContinueNumbering;
    property FirstPageNumber: Integer read GetFirstPageNumber write SetFirstPageNumber;
    property NumberingFormat: TdxRichEditNumberingFormat read GetNumberingFormat write SetNumberingFormat;
    property Start: Integer read GetStart write SetStart;
  end;

  { TdxNativeSectionParagraphCollection }

  TdxNativeSectionParagraphCollection = class(TInterfacedObject, IdxRichEditReadOnlyParagraphCollection)
  strict private
    FParagraphs: IdxRichEditParagraphCollection;
    FInnerSection: TdxSection;
    function GetCount: Integer;
    function GetItem(Index: Integer): IdxRichEditParagraph;
  public
    constructor Create(const AParagraphs: IdxRichEditParagraphCollection; AInnerSection: TdxSection);

    function Get(const APos: IdxRichEditDocumentPosition): IdxRichEditParagraph; overload;
    function Get(const ARange: IdxRichEditDocumentRange): IdxRichEditReadOnlyParagraphCollection; overload;

    property Count: Integer read GetCount;
    property Self[Index: Integer]: IdxRichEditParagraph read GetItem; default;
  end;

{ TdxNativeSectionMargins }

constructor TdxNativeSectionMargins.Create(ADocument: TdxNativeDocument; AMargins: TdxSectionMargins);
begin
  inherited Create;
  FDocument := ADocument;
  FMargins := AMargins;
end;

function TdxNativeSectionMargins.GetLeft: Single;
begin
  Result := Document.ModelUnitsToUnits(Margins.Left);
end;

procedure TdxNativeSectionMargins.SetLeft(const Value: Single);
begin
  Margins.Left := Document.UnitsToModelUnits(Value);
end;

function TdxNativeSectionMargins.GetTop: Single;
begin
  Result := Document.ModelUnitsToUnits(Margins.Top);
end;

procedure TdxNativeSectionMargins.SetTop(const Value: Single);
begin
  Margins.Top := Document.UnitsToModelUnits(Value);
end;

function TdxNativeSectionMargins.GetRight: Single;
begin
  Result := Document.ModelUnitsToUnits(Margins.Right);
end;

procedure TdxNativeSectionMargins.SetRight(const Value: Single);
begin
  Margins.Right := Document.UnitsToModelUnits(Value);
end;

function TdxNativeSectionMargins.GetBottom: Single;
begin
  Result := Document.ModelUnitsToUnits(Margins.Bottom);
end;

procedure TdxNativeSectionMargins.SetBottom(const Value: Single);
begin
  Margins.Bottom := Document.UnitsToModelUnits(Value);
end;

function TdxNativeSectionMargins.GetHeaderOffset: Single;
begin
  Result := Document.ModelUnitsToUnits(Margins.HeaderOffset);
end;

procedure TdxNativeSectionMargins.SetHeaderOffset(const Value: Single);
begin
  Margins.HeaderOffset := Document.UnitsToModelUnits(Value);
end;

function TdxNativeSectionMargins.GetFooterOffset: Single;
begin
  Result := Document.ModelUnitsToUnits(Margins.FooterOffset);
end;

procedure TdxNativeSectionMargins.SetFooterOffset(const Value: Single);
begin
  Margins.FooterOffset := Document.UnitsToModelUnits(Value);
end;

{ TdxNativeSectionPage }

constructor TdxNativeSectionPage.Create(ADocument: TdxNativeDocument; APage: TdxSectionPage);
begin
  inherited Create;
  FDocument := ADocument;
  FPage := APage;
end;

function TdxNativeSectionPage.GetWidth: Single;
begin
  if Landscape then
    Result := ActualHeight
  else
    Result := ActualWidth;
end;

procedure TdxNativeSectionPage.SetWidth(const Value: Single);
begin
  if Landscape then
    ActualHeight := Value
  else
    ActualWidth := Value;
end;

function TdxNativeSectionPage.GetHeight: Single;
begin
  if Landscape then
    Result := ActualWidth
  else
    Result := ActualHeight;
end;

procedure TdxNativeSectionPage.SetHeight(const Value: Single);
begin
  if Landscape then
    ActualWidth := Value
  else
    ActualHeight := Value;
end;

function TdxNativeSectionPage.GetPaperKind: TdxRichEditPaperKind;
begin
  Result := Page.PaperKind;
end;

procedure TdxNativeSectionPage.SetPaperKind(const Value: TdxRichEditPaperKind);
begin
  if Page.PaperKind = Value then
    Exit;
  SetPageProperty<TdxPaperKind>(SetPaperKindCore, Value);
end;

function TdxNativeSectionPage.GetLandscape: Boolean;
begin
  Result := Page.Landscape;
end;

procedure TdxNativeSectionPage.SetLandscape(const Value: Boolean);
begin
  if Page.Landscape = Value then
    Exit;
  SetPageProperty<Boolean>(SetLandscapeCore, Value);
end;

function TdxNativeSectionPage.GetActualWidth: Single;
begin
  Result := Document.ModelUnitsToUnits(Page.Width);
end;

procedure TdxNativeSectionPage.SetActualWidth(const Value: Single);
var
  AWidth: Integer;
begin
  AWidth := Document.UnitsToModelUnits(Value);
  if AWidth = Page.Width then
    Exit;
  SetPageProperty<Integer>(SetPageWidthCore, AWidth);
end;

function TdxNativeSectionPage.GetActualHeight: Single;
begin
  Result := Document.ModelUnitsToUnits(Page.Height);
end;

procedure TdxNativeSectionPage.SetActualHeight(const Value: Single);
var
  AHeight: Integer;
begin
  AHeight := Document.UnitsToModelUnits(Value);
  if AHeight = Page.Height then
    Exit;
  SetPageProperty<Integer>(SetPageHeightCore, AHeight);
end;

procedure TdxNativeSectionPage.SetPageProperty<T>(const ASetter: TSetValueDelegate<T>; const Value: T);
begin
  Document.InternalAPI.DocumentModel.BeginUpdate;
  try
    Page.BeginUpdate;
    try
      ASetter(Value);
    finally
      Page.EndUpdate;
    end;
  finally
    Document.InternalAPI.DocumentModel.EndUpdate;
  end;
end;

procedure TdxNativeSectionPage.SetPaperKindCore(const Value: TdxPaperKind);
var
  APaperSizeInTwips, APaperSizeInModelUnits: TSize;
begin
  if Value <> TdxPaperKind.Custom then
  begin
    APaperSizeInTwips := TdxPaperSizeCalculator.CalculatePaperSize(Value);
    APaperSizeInModelUnits := FDocument.DocumentModel.UnitConverter.TwipsToModelUnits(APaperSizeInTwips);
    Width := Document.ModelUnitsToUnits(APaperSizeInModelUnits.cx);
    Height := Document.ModelUnitsToUnits(APaperSizeInModelUnits.cy);
  end;
  Page.PaperKind := Value;
end;

procedure TdxNativeSectionPage.SetPageWidthCore(const Value: Integer);
begin
  Page.Width := Value;
  Page.PaperKind := TdxPaperKind.Custom;
end;

procedure TdxNativeSectionPage.SetPageHeightCore(const Value: Integer);
begin
  Page.Height := Value;
  Page.PaperKind := TdxPaperKind.Custom;
end;

procedure TdxNativeSectionPage.SetLandscapeCore(const Value: Boolean);
var
  AWidth: Integer;
begin
  Page.Landscape := Value;
  AWidth := Page.Width;
  Page.Width := Page.Height;
  Page.Height := AWidth;
end;

{ TdxNativeSectionColumn }

constructor TdxNativeSectionColumn.Create(AWidth, ASpacing: Single);
begin
  inherited Create;
  FWidth := AWidth;
  FSpacing := ASpacing;
end;

function TdxNativeSectionColumn.GetSpacing: Single;
begin
  Result := FSpacing;
end;

function TdxNativeSectionColumn.GetWidth: Single;
begin
  Result := FWidth;
end;

procedure TdxNativeSectionColumn.SetSpacing(const Value: Single);
begin
  FSpacing := Value;
end;

procedure TdxNativeSectionColumn.SetWidth(const Value: Single);
begin
  FWidth := Value;
end;

{ TdxNativeSectionColumns }

constructor TdxNativeSectionColumns.Create(ADocument: TdxNativeDocument;
  AColumns: TdxSectionColumns; const APage: IdxRichEditSectionPage);
begin
  inherited Create;
  FDocument := ADocument;
  FColumns := AColumns;
  FPage := APage;
end;

function TdxNativeSectionColumns.GetCount: Integer;
begin
  Result := GetActualColumnCount;
end;

function TdxNativeSectionColumns.GetColumns: IdxRichEditSectionColumnCollection;
begin
  if Columns.EqualWidthColumns then
    Result := CreateUniformColumns(Page, Document.ModelUnitsToUnits(Columns.Space), Count)
  else
    Result := CreateNonUniformColumns;
end;

procedure TdxNativeSectionColumns.SetColumns(const AColumns: IdxRichEditSectionColumnCollection);
var
  ACount: Integer;
begin
  ACount := AColumns.Count;
  if ACount <= 0 then
    Exit;

  if IsEqualWidthColumns(AColumns) then
    SetUniformColumns(AColumns[0].Spacing, ACount)
  else
    SetNonUniformColumns(AColumns);
end;

function TdxNativeSectionColumns.CreateUniformColumns(AColumnWidth, AColumnSpacing: Single;
  AColumnCount: Integer): IdxRichEditSectionColumnCollection;
var
  AResult: TdxNativeSectionColumnCollection;
  I: Integer;
begin
  AResult := TdxNativeSectionColumnCollection.Create;
  for I := 0 to AColumnCount - 1 do
    AResult.Add(TdxNativeSectionColumn.Create(AColumnWidth, AColumnSpacing));
  Result := AResult;
end;

function TdxNativeSectionColumns.CreateUniformColumns(const APage: IdxRichEditSectionPage;
  AColumnSpacing: Single; AColumnCount: Integer): IdxRichEditSectionColumnCollection;
var
  AColumnWidth: Single;
begin
  if AColumnCount <= 0 then
    Exit(TdxNativeSectionColumnCollection.Create);
  AColumnWidth := (APage.Width - (AColumnSpacing * (AColumnCount - 1))) / AColumnCount;
  Result := CreateUniformColumns(AColumnWidth, AColumnSpacing, AColumnCount);
end;

function TdxNativeSectionColumns.GetActualColumnCount: Integer;
var
  AColumns: TdxColumnInfoCollection;
begin
  if Columns.EqualWidthColumns then
    Exit(Columns.ColumnCount);

  AColumns := Columns.GetColumns;
  try
    Result := AColumns.Count;
  finally
    AColumns.Free;
  end;
end;

function TdxNativeSectionColumns.CreateNonUniformColumns: IdxRichEditSectionColumnCollection;
var
  AResult: TdxNativeSectionColumnCollection;
  AInnerColumns: TdxColumnInfoCollection;
  ACount, I: Integer;
  AWidth, ASpacing: Single;
begin
  AResult := TdxNativeSectionColumnCollection.Create;
  AInnerColumns := Columns.GetColumns;
  ACount := AInnerColumns.Count;
  for I := 0 to ACount - 1 do
  begin
    AWidth := Document.ModelUnitsToUnitsF(AInnerColumns[I].Width);
    ASpacing := Document.ModelUnitsToUnitsF(AInnerColumns[I].Space);
    AResult.Add(TdxNativeSectionColumn.Create(AWidth, ASpacing));
  end;
  Result := AResult;
end;

function TdxNativeSectionColumns.IsEqualWidthColumns(const AColumns: IdxRichEditSectionColumnCollection): Boolean;
var
  ACount, I: Integer;
  AFirstWidth, AFirstSpacing, ATotalWidth, AWidth, ASpacing: Single;
begin
  ACount := AColumns.Count;
  if ACount <= 0 then
    Exit(False);

  AFirstWidth := AColumns[0].Width;
  AFirstSpacing := AColumns[0].Spacing;
  ATotalWidth := -AColumns[ACount - 1].Spacing;
  for I := 0 to ACount - 1 do
  begin
    AWidth := AColumns[I].Width;
    if AWidth <> AFirstWidth then
      Exit(False);
    ASpacing := AColumns[I].Spacing;
    if ASpacing <> AFirstSpacing then
      Exit(False);
    ATotalWidth := ATotalWidth + AWidth + ASpacing;
  end;
  Result := ATotalWidth = Page.Width;
end;

procedure TdxNativeSectionColumns.SetUniformColumns(ASpacing: Single; AColumnCount: Integer);
begin
  Columns.BeginUpdate;
  try
    Columns.EqualWidthColumns := True;
    Columns.ColumnCount := AColumnCount;
    Columns.Space := Document.UnitsToModelUnits(ASpacing);
  finally
    Columns.EndUpdate;
  end;
end;

procedure TdxNativeSectionColumns.SetNonUniformColumns(const AColumns: IdxRichEditSectionColumnCollection);
var
  ACount, I: Integer;
  AInnerColumns: TdxColumnInfoCollection;
  AColumn: TdxColumnInfo;
begin
  ACount := AColumns.Count;
  Columns.BeginUpdate;
  try
    Columns.EqualWidthColumns := False;
    AInnerColumns := TdxColumnInfoCollection.Create;
    try
      for I := 0 to ACount - 1 do
      begin
        AColumn := TdxColumnInfo.Create;
        AColumn.Width := Document.UnitsToModelUnits(AColumns[I].Width);
        AColumn.Space := Document.UnitsToModelUnits(AColumns[I].Spacing);
        AInnerColumns.Add(AColumn);
      end;
      Columns.SetColumns(AInnerColumns);
    finally
      AInnerColumns.Free;
    end;
  finally
    Columns.EndUpdate;
  end;
end;

{ TdxNativeSectionColumnCollection }

function TdxNativeSectionColumnCollection.GetCount: Integer;
begin
  Result := inherited Count;
end;

{ TdxNativeSectionLineNumbering }

constructor TdxNativeSectionLineNumbering.Create(ADocument: TdxNativeDocument; ALineNumbering: TdxSectionLineNumbering);
begin
  inherited Create;
  FDocument := ADocument;
  FLineNumbering := ALineNumbering;
end;

function TdxNativeSectionLineNumbering.GetDistance: Single;
begin
  Result := Document.ModelUnitsToUnits(LineNumbering.Distance);
end;

procedure TdxNativeSectionLineNumbering.SetDistance(const Value: Single);
begin
  LineNumbering.Distance := Document.UnitsToModelUnits(Value);
end;

function TdxNativeSectionLineNumbering.GetStart: Integer;
begin
  Result := LineNumbering.StartingLineNumber;
end;

procedure TdxNativeSectionLineNumbering.SetStart(const Value: Integer);
begin
  LineNumbering.StartingLineNumber := Value;
end;

function TdxNativeSectionLineNumbering.GetCountBy: Integer;
begin
  Result := LineNumbering.Step;
end;

procedure TdxNativeSectionLineNumbering.SetCountBy(const Value: Integer);
begin
  LineNumbering.Step := Value;
end;

function TdxNativeSectionLineNumbering.GetRestartType: TdxRichEditLineNumberingRestart;
begin
  Result := LineNumbering.NumberingRestartType;
end;

procedure TdxNativeSectionLineNumbering.SetRestartType(const Value: TdxRichEditLineNumberingRestart);
begin
  LineNumbering.NumberingRestartType := Value;
end;

{ TdxNativeSectionPageNumbering }

constructor TdxNativeSectionPageNumbering.Create(ADocument: TdxNativeDocument; APageNumbering: TdxSectionPageNumbering);
begin
  inherited Create;
  FDocument := ADocument;
  FPageNumbering := APageNumbering;
end;

function TdxNativeSectionPageNumbering.GetStart: Integer;
begin
  if not FPageNumbering.ContinueNumbering then
    Exit(FPageNumbering.FirstPageNumber);
  if FPageNumbering.FirstPageNumber < 0 then
    Result := FPageNumbering.FirstPageNumber
  else
    Result := 0;
end;

procedure TdxNativeSectionPageNumbering.SetStart(const Value: Integer);
begin
  FPageNumbering.FirstPageNumber := Value;
  FPageNumbering.ContinueNumbering := Value <= 0;
end;

function TdxNativeSectionPageNumbering.GetFirstPageNumber: Integer;
begin
  Result := FPageNumbering.FirstPageNumber;
end;

procedure TdxNativeSectionPageNumbering.SetFirstPageNumber(const Value: Integer);
begin
  Assert(Value >= 0);
  FPageNumbering.FirstPageNumber := Value;
end;

function TdxNativeSectionPageNumbering.GetContinueNumbering: Boolean;
begin
  Result := FPageNumbering.ContinueNumbering;
end;

procedure TdxNativeSectionPageNumbering.SetContinueNumbering(const Value: Boolean);
begin
  FPageNumbering.ContinueNumbering := Value;
end;

function TdxNativeSectionPageNumbering.GetNumberingFormat: TdxRichEditNumberingFormat;
begin
  Result := FPageNumbering.NumberingFormat;
end;

procedure TdxNativeSectionPageNumbering.SetNumberingFormat(const Value: TdxRichEditNumberingFormat);
begin
  FPageNumbering.NumberingFormat := Value;
end;

{ TdxNativeSectionParagraphCollection }

constructor TdxNativeSectionParagraphCollection.Create(const AParagraphs: IdxRichEditParagraphCollection;
  AInnerSection: TdxSection);
begin
  inherited Create;
  FParagraphs := AParagraphs;
  FInnerSection := AInnerSection;
end;

function TdxNativeSectionParagraphCollection.GetCount: Integer;
begin
  Result := FInnerSection.LastParagraphIndex - FInnerSection.FirstParagraphIndex + 1;
end;

function TdxNativeSectionParagraphCollection.GetItem(Index: Integer): IdxRichEditParagraph;
begin
  Result := FParagraphs[FInnerSection.FirstParagraphIndex + Index];
end;

function TdxNativeSectionParagraphCollection.Get(
  const ARange: IdxRichEditDocumentRange): IdxRichEditReadOnlyParagraphCollection;
var
  AResult: TdxNativeReadOnlyParagraphCollection;
  ACount, I: Integer;
  AParagraph: IdxRichEditParagraph;
begin
  AResult := TdxNativeReadOnlyParagraphCollection.Create;
  ACount := FParagraphs.Count;
  for I := 0 to ACount - 1 do
  begin
    AParagraph := FParagraphs[I];
    if (AParagraph.Range.Start.CompareTo(ARange.Start) >= 0) and (AParagraph.Range.&End.CompareTo(ARange.&End) <= 0) then
      AResult.Add(AParagraph);
  end;
  Result := AResult;
end;

function TdxNativeSectionParagraphCollection.Get(
  const APos: IdxRichEditDocumentPosition): IdxRichEditParagraph;
var
  APosition: TdxNativeDocumentPosition;
  AParagraphIndex: Integer;
begin
  APosition := TdxNativeDocumentPosition(APos);
  AParagraphIndex := APosition.Position.ParagraphIndex;
  if (AParagraphIndex < FInnerSection.FirstParagraphIndex) or
      (AParagraphIndex > FInnerSection.LastParagraphIndex) then
    Result := nil
  else
    Result := Self[AParagraphIndex - FInnerSection.FirstParagraphIndex];
end;

{ TdxNativeSection }

constructor TdxNativeSection.Create(ADocument: TdxNativeDocument; AInnerSection: TdxSection);
begin
  inherited Create;
  FDocument := ADocument;
  FInnerSection := AInnerSection;
  FParagraphs := TdxNativeSectionParagraphCollection.Create(Document.Paragraphs, InnerSection);
  FMargins := TdxNativeSectionMargins.Create(Document, AInnerSection.Margins);
  FPage := TdxNativeSectionPage.Create(Document, AInnerSection.Page);
  FColumns := TdxNativeSectionColumns.Create(Document, AInnerSection.Columns, FPage);
  FLineNumbering := TdxNativeSectionLineNumbering.Create(Document, AInnerSection.LineNumbering);
  FPageNumbering := TdxNativeSectionPageNumbering.Create(Document, AInnerSection.PageNumbering);
  FIsValid := True;
end;

function TdxNativeSection.GetColumns: IdxRichEditSectionColumns;
begin
  CheckValid;
  Result := FColumns;
end;

function TdxNativeSection.GetDifferentFirstPage: Boolean;
begin
  CheckValid;
  Result := FInnerSection.GeneralSettings.DifferentFirstPage;
end;

function TdxNativeSection.GetFirstPageTray: Integer;
begin
  CheckValid;
  Result := FInnerSection.GeneralSettings.FirstPagePaperSource;
end;

function TdxNativeSection.GetLineNumbering: IdxRichEditSectionLineNumbering;
begin
  CheckValid;
  Result := FLineNumbering;
end;

function TdxNativeSection.GetMargins: IdxRichEditSectionMargins;
begin
  CheckValid;
  Result := FMargins;
end;

function TdxNativeSection.GetOtherPagesTray: Integer;
begin
  CheckValid;
  Result := FInnerSection.GeneralSettings.OtherPagePaperSource;
end;

function TdxNativeSection.GetPage: IdxRichEditSectionPage;
begin
  CheckValid;
  Result := FPage;
end;

function TdxNativeSection.GetPageNumbering: IdxRichEditSectionPageNumbering;
begin
  CheckValid;
  Result := FPageNumbering;
end;

function TdxNativeSection.GetParagraphs: IdxRichEditReadOnlyParagraphCollection;
begin
  CheckValid;
  Result := FParagraphs;
end;

function TdxNativeSection.GetStartType: TdxRichEditSectionStartType;
begin
  CheckValid;
  Result := FInnerSection.GeneralSettings.StartType;
end;

procedure TdxNativeSection.SetDifferentFirstPage(const Value: Boolean);
begin
  CheckValid;
  FInnerSection.GeneralSettings.DifferentFirstPage := Value;
end;

procedure TdxNativeSection.SetFirstPageTray(const Value: Integer);
begin
  CheckValid;
  FInnerSection.GeneralSettings.FirstPagePaperSource := Value;
end;

procedure TdxNativeSection.SetOtherPagesTray(const Value: Integer);
begin
  CheckValid;
  FInnerSection.GeneralSettings.OtherPagePaperSource := Value;
end;

procedure TdxNativeSection.SetStartType(const Value: TdxRichEditSectionStartType);
begin
  CheckValid;
  FInnerSection.GeneralSettings.StartType := Value;
end;

function TdxNativeSection.BeginUpdateHeader: IdxRichEditSubDocument;
begin
  Result := BeginUpdateHeader(TdxHeaderFooterType.Odd);
end;

function TdxNativeSection.BeginUpdateHeader(AType: TdxRichEditHeaderFooterType): IdxRichEditSubDocument;
var
  AHeader: TdxSectionHeader;
begin
  CheckValid;
  FInnerSection.DocumentModel.BeginUpdate;
  AHeader := FInnerSection.Headers.GetObject(AType);
  if AHeader = nil then
  begin
    FInnerSection.Headers.Add(AType);
    AHeader := FInnerSection.Headers.GetObject(AType);
  end;
  Result := CreateSubDocument(TdxPieceTable(AHeader.PieceTable), FDocument.DocumentServer);
end;

procedure TdxNativeSection.EndUpdateHeader(const ADocument: IdxRichEditSubDocument);
begin
  CheckValid;
  FInnerSection.DocumentModel.EndUpdate;
end;

function TdxNativeSection.HasHeader(AType: TdxRichEditHeaderFooterType): Boolean;
begin
  Result := FInnerSection.Headers.GetObject(AType) <> nil;
end;

function TdxNativeSection.BeginUpdateFooter: IdxRichEditSubDocument;
begin
  Result := BeginUpdateFooter(TdxHeaderFooterType.Odd);
end;

function TdxNativeSection.BeginUpdateFooter(AType: TdxRichEditHeaderFooterType): IdxRichEditSubDocument;
var
  AFooter: TdxSectionFooter;
begin
  CheckValid;
  FInnerSection.DocumentModel.BeginUpdate;
  AFooter := FInnerSection.Footers.GetObject(AType);
  if AFooter = nil then
  begin
    FInnerSection.Footers.Add(AType);
    AFooter := FInnerSection.Footers.GetObject(AType);
  end;
  Result := CreateSubDocument(TdxPieceTable(AFooter.PieceTable), FDocument.DocumentServer);
end;

procedure TdxNativeSection.EndUpdateFooter(const ADocument: IdxRichEditSubDocument);
begin
  FInnerSection.DocumentModel.EndUpdate;
end;

function TdxNativeSection.HasFooter(AType: TdxRichEditHeaderFooterType): Boolean;
begin
  Result := FInnerSection.Footers.GetObject(AType) <> nil;
end;

function TdxNativeSection.IsHeaderLinkedToPrevious: Boolean;
begin
  Result := IsLinkedToPrevious(TdxHeaderFooterType.Odd, FInnerSection.Headers);
end;

procedure TdxNativeSection.LinkHeaderToPrevious;
begin
  LinkToPrevious(TdxHeaderFooterType.Odd, FInnerSection.Headers);
end;

procedure TdxNativeSection.UnlinkHeaderFromPrevious;
begin
  UnlinkFromPrevious(TdxHeaderFooterType.Odd, FInnerSection.Headers);
end;

function TdxNativeSection.IsFooterLinkedToPrevious: Boolean;
begin
  Result := IsLinkedToPrevious(TdxHeaderFooterType.Odd, FInnerSection.Footers);
end;

procedure TdxNativeSection.LinkFooterToPrevious;
begin
  LinkToPrevious(TdxHeaderFooterType.Odd, FInnerSection.Footers);
end;

procedure TdxNativeSection.UnlinkFooterFromPrevious;
begin
  UnlinkFromPrevious(TdxHeaderFooterType.Odd, FInnerSection.Footers);
end;

function TdxNativeSection.IsHeaderLinkedToPrevious(AType: TdxRichEditHeaderFooterType): Boolean;
begin
  Result := IsLinkedToPrevious(AType, FInnerSection.Headers);
end;

procedure TdxNativeSection.LinkHeaderToPrevious(AType: TdxRichEditHeaderFooterType);
begin
  LinkToPrevious(AType, FInnerSection.Headers);
end;

procedure TdxNativeSection.UnlinkHeaderFromPrevious(AType: TdxRichEditHeaderFooterType);
begin
  UnlinkFromPrevious(AType, FInnerSection.Headers);
end;

function TdxNativeSection.IsFooterLinkedToPrevious(AType: TdxRichEditHeaderFooterType): Boolean;
begin
  Result := IsLinkedToPrevious(AType, FInnerSection.Footers);
end;

procedure TdxNativeSection.LinkFooterToPrevious(AType: TdxRichEditHeaderFooterType);
begin
  LinkToPrevious(AType, FInnerSection.Footers);
end;

procedure TdxNativeSection.UnlinkFooterFromPrevious(AType: TdxRichEditHeaderFooterType);
begin
  UnlinkFromPrevious(AType, FInnerSection.Footers);
end;

function TdxNativeSection.IsHeaderLinkedToNext: Boolean;
begin
  Result := IsLinkedToNext(TdxHeaderFooterType.Odd, FInnerSection.Headers);
end;

procedure TdxNativeSection.LinkHeaderToNext;
begin
  LinkToNext(TdxHeaderFooterType.Odd, FInnerSection.Headers);
end;

procedure TdxNativeSection.UnlinkHeaderFromNext;
begin
  UnlinkFromNext(TdxHeaderFooterType.Odd, FInnerSection.Headers);
end;

function TdxNativeSection.IsFooterLinkedToNext: Boolean;
begin
  Result := IsLinkedToNext(TdxHeaderFooterType.Odd, FInnerSection.Footers);
end;

procedure TdxNativeSection.LinkFooterToNext;
begin
  LinkToNext(TdxHeaderFooterType.Odd, FInnerSection.Footers);
end;

procedure TdxNativeSection.UnlinkFooterFromNext;
begin
  UnlinkFromNext(TdxHeaderFooterType.Odd, FInnerSection.Footers);
end;

function TdxNativeSection.IsHeaderLinkedToNext(AType: TdxRichEditHeaderFooterType): Boolean;
begin
  Result := IsLinkedToNext(AType, FInnerSection.Headers);
end;

procedure TdxNativeSection.LinkHeaderToNext(AType: TdxRichEditHeaderFooterType);
begin
  LinkToNext(AType, FInnerSection.Headers);
end;

procedure TdxNativeSection.UnlinkHeaderFromNext(AType: TdxRichEditHeaderFooterType);
begin
  UnlinkFromNext(AType, FInnerSection.Headers);
end;

function TdxNativeSection.IsFooterLinkedToNext(AType: TdxRichEditHeaderFooterType): Boolean;
begin
  Result := IsLinkedToNext(AType, FInnerSection.Footers);
end;

procedure TdxNativeSection.LinkFooterToNext(AType: TdxRichEditHeaderFooterType);
begin
  LinkToNext(AType, FInnerSection.Footers);
end;

procedure TdxNativeSection.UnlinkFooterFromNext(AType: TdxRichEditHeaderFooterType);
begin
  UnlinkFromNext(AType, FInnerSection.Footers);
end;

procedure TdxNativeSection.CheckValid;
begin
  if not FIsValid then
    TdxRichEditExceptions.ThrowInvalidOperationException(cxGetResourceString(@sdxRichEditExceptionUseDeletedSectionError));
end;

function TdxNativeSection.CreateSubDocument(APieceTable: TdxPieceTable;
  AServer: TdxInnerRichEditDocumentServer): IdxRichEditSubDocument;
begin
  Result := TdxNativeSubDocument.Create(APieceTable, AServer);
end;

function TdxNativeSection.IsLinkedToPrevious(AType: TdxRichEditHeaderFooterType;
  AHeadersFooters: TdxSectionHeadersFootersBase): Boolean;
var
  AHeaderFooter: TdxSectionHeaderFooterBase;
begin
  CheckValid;
  FInnerSection.DocumentModel.BeginUpdate;
  try
    AHeaderFooter := AHeadersFooters.GetObjectCore(AType);
    Result := (AHeaderFooter <> nil) and
      AHeaderFooter.GetContainer(InnerSection).IsLinkedToPrevious(AHeaderFooter.&Type)
  finally
    FInnerSection.DocumentModel.EndUpdate;
  end;
end;

procedure TdxNativeSection.LinkToPrevious(AType: TdxRichEditHeaderFooterType;
  AHeadersFooters: TdxSectionHeadersFootersBase);
var
  AHeaderFooter: TdxSectionHeaderFooterBase;
  AContainer: TdxSectionHeadersFootersBase;
begin
  CheckValid;
  FInnerSection.DocumentModel.BeginUpdate;
  try
    AHeaderFooter := AHeadersFooters.GetObjectCore(AType);
    if AHeaderFooter = nil then
    begin
      AHeadersFooters.Add(AType);
      AHeaderFooter := AHeadersFooters.GetObjectCore(AType);
    end;
    AContainer := AHeaderFooter.GetContainer(InnerSection);
    if not AContainer.IsLinkedToPrevious(AHeaderFooter.&Type) then
      AContainer.LinkToPrevious(AHeaderFooter.&Type);
  finally
    FInnerSection.DocumentModel.EndUpdate;
  end;
end;

procedure TdxNativeSection.UnlinkFromPrevious(AType: TdxRichEditHeaderFooterType;
  AHeadersFooters: TdxSectionHeadersFootersBase);
var
  AHeaderFooter: TdxSectionHeaderFooterBase;
  AContainer: TdxSectionHeadersFootersBase;
begin
  CheckValid;
  FInnerSection.DocumentModel.BeginUpdate;
  try
    AHeaderFooter := AHeadersFooters.GetObjectCore(AType);
    if AHeaderFooter = nil then
    begin
      AHeadersFooters.Add(AType);
      AHeaderFooter := AHeadersFooters.GetObjectCore(AType);
    end;
    AContainer := AHeaderFooter.GetContainer(InnerSection);
    if AContainer.IsLinkedToPrevious(AHeaderFooter.&Type) then
      AContainer.UnlinkFromPrevious(AHeaderFooter.&Type);
  finally
    FInnerSection.DocumentModel.EndUpdate;
  end;
end;

function TdxNativeSection.IsLinkedToNext(AType: TdxRichEditHeaderFooterType;
  AHeadersFooters: TdxSectionHeadersFootersBase): Boolean;
var
  AHeaderFooter: TdxSectionHeaderFooterBase;
begin
  CheckValid;
  FInnerSection.DocumentModel.BeginUpdate;
  try
    AHeaderFooter := AHeadersFooters.GetObjectCore(AType);
    Result := (AHeaderFooter <> nil) and
      AHeaderFooter.GetContainer(InnerSection).IsLinkedToNext(AHeaderFooter.&Type);
  finally
    FInnerSection.DocumentModel.EndUpdate;
  end;
end;

procedure TdxNativeSection.LinkToNext(AType: TdxRichEditHeaderFooterType;
  AHeadersFooters: TdxSectionHeadersFootersBase);
var
  AHeaderFooter: TdxSectionHeaderFooterBase;
  AContainer: TdxSectionHeadersFootersBase;
begin
  CheckValid;
  FInnerSection.DocumentModel.BeginUpdate;
  try
    AHeaderFooter := AHeadersFooters.GetObjectCore(AType);
    if AHeaderFooter = nil then
    begin
      AHeadersFooters.Add(AType);
      AHeaderFooter := AHeadersFooters.GetObjectCore(AType);
    end;
    AContainer := AHeaderFooter.GetContainer(InnerSection);
    if not AContainer.IsLinkedToNext(AHeaderFooter.&Type) then
      AContainer.LinkToNext(AHeaderFooter.&Type);
  finally
    FInnerSection.DocumentModel.EndUpdate;
  end;
end;

procedure TdxNativeSection.UnlinkFromNext(AType: TdxRichEditHeaderFooterType;
  AHeadersFooters: TdxSectionHeadersFootersBase);
var
  AHeaderFooter: TdxSectionHeaderFooterBase;
  AContainer: TdxSectionHeadersFootersBase;
begin
  CheckValid;
  FInnerSection.DocumentModel.BeginUpdate;
  try
    AHeaderFooter := AHeadersFooters.GetObjectCore(AType);
    if AHeaderFooter = nil then
    begin
      AHeadersFooters.Add(AType);
      AHeaderFooter := AHeadersFooters.GetObjectCore(AType);
    end;
    AContainer := AHeaderFooter.GetContainer(InnerSection);
    if AContainer.IsLinkedToNext(AHeaderFooter.&Type) then
      AContainer.UnlinkFromNext(AHeaderFooter.&Type);
  finally
    FInnerSection.DocumentModel.EndUpdate;
  end;
end;

end.
