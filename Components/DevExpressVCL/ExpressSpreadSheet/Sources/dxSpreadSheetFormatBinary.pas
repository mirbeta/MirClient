{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressSpreadSheet                                       }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSPREADSHEET CONTROL AND ALL    }
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

unit dxSpreadSheetFormatBinary;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, SysUtils, Classes, Generics.Defaults, Generics.Collections, dxCore, cxVariants,
  dxSpreadSheetCore, dxSpreadSheetTypes, dxSpreadSheetClasses, dxSpreadSheetHyperlinks, dxSpreadSheetProtection,
  dxSpreadSheetCoreStyles;

const
  dxSpreadSheetBinaryFormatVersion = 12;

type

  { TdxSpreadSheetBinaryFormatHeader }

  TdxSpreadSheetBinaryFormatHeader = packed record
    ID: Int64;
    Version: Cardinal;
    procedure Initialize;
    function IsValid: Boolean;
  end;

  { TdxSpreadSheetBinaryFormat }

  TdxSpreadSheetBinaryFormat = class(TdxSpreadSheetCustomFormat)
  public
    class function CanReadFromStream(AStream: TStream): Boolean; override;
    class function CreateFormatSettings: TdxSpreadSheetFormatSettings; override;
    class function GetExt: string; override;
    class function GetReader: TdxSpreadSheetCustomReaderClass; override;
    class function GetWriter: TdxSpreadSheetCustomWriterClass; override;
  end;

  { TdxSpreadSheetBinaryReader }

  TdxSpreadSheetBinaryReader = class(TdxSpreadSheetCustomReader)
  strict private
    FCellStyles: TList<TdxSpreadSheetCellStyleHandle>;
    FContainerHyperlinks: TDictionary<Integer, TdxSpreadSheetContainer>;
    FFormulaRefs: TdxSpreadSheetFormulaAsTextInfoList;
    FReader: TcxReader;

    procedure CheckSectionHeader;
  protected
    function CreateProgressHelper: TdxSpreadSheetCustomFilerProgressHelper; override;
    procedure ReadCellStyles; virtual;
    procedure ReadDefinedNames; virtual;
    procedure ReadExternalLinks; virtual;
    procedure ReadHyperlinks; virtual;
    procedure ReadProperties; virtual;
    function ReadProtectionInfo: IdxSpreadSheetProtectionInfo;
    //
    procedure ReadTableView(AView: TdxSpreadSheetTableView); virtual;
    procedure ReadTableViewCells(AView: TdxSpreadSheetTableView); virtual;
    procedure ReadTableViewConditionalFormatting(AView: TdxSpreadSheetTableView); virtual;
    procedure ReadTableViewItems(AItems: TdxSpreadSheetTableItems); virtual;
    procedure ReadTableViewItemsGroupChildren(AGroup: TdxSpreadSheetTableItemGroup); virtual;
    procedure ReadTableViewMergedCells(AView: TdxSpreadSheetTableView); virtual;
    procedure ReadTableViewOptionsPrint(AOptions: TdxSpreadSheetTableViewOptionsPrint); virtual;
    procedure ReadTableViewProperties(AView: TdxSpreadSheetTableView); virtual;
    procedure ReadTableViewSelection(AView: TdxSpreadSheetTableView); virtual;
    //
    procedure ReadView(AView: TdxSpreadSheetCustomView); virtual;
    procedure ReadViewContainers(AView: TdxSpreadSheetCustomView); virtual;
    procedure ReadViews; virtual;
    //
    procedure ResolveContainerHyperlinks;
    procedure ResolveFormulas;
    //
    property CellStyles: TList<TdxSpreadSheetCellStyleHandle> read FCellStyles;
    property ContainerHyperlinks: TDictionary<Integer, TdxSpreadSheetContainer> read FContainerHyperlinks;
    property Reader: TcxReader read FReader;
  public
    constructor Create(AOwner: TdxCustomSpreadSheet; AStream: TStream); override;
    destructor Destroy; override;
    procedure ReadData; override;
  end;

  { TdxSpreadSheetBinaryWriter }

  TdxSpreadSheetBinaryWriter = class(TdxSpreadSheetCustomWriter)
  strict private
    FCellStyles: TList<TdxSpreadSheetCellStyleHandle>;
    FWriter: TcxWriter;

    procedure WriteCount(const ACount: Integer; const ACountFieldPosition: Int64);
    procedure WriteNullCount(out ACount: Integer; out ACountFieldPosition: Int64);
  protected
    function CreateProgressHelper: TdxSpreadSheetCustomFilerProgressHelper; override;
    procedure WriteCellStyles; virtual;
    procedure WriteDefinedNames; virtual;
    procedure WriteExternalLinks; virtual;
    procedure WriteHyperlinks; virtual;
    procedure WriteProperties; virtual;
    procedure WriteProtectionInfo(AProtectionInfo: IdxSpreadSheetProtectionInfo);
    procedure WriteSectionHeader; virtual;

    procedure WriteTableView(AView: TdxSpreadSheetTableView); virtual;
    procedure WriteTableViewCells(AView: TdxSpreadSheetTableView); virtual;
    procedure WriteTableViewConditionalFormatting(AView: TdxSpreadSheetTableView); virtual;
    procedure WriteTableViewItems(AItems: TdxSpreadSheetTableItems); virtual;
    procedure WriteTableViewItemsGroupChildren(AGroup: TdxSpreadSheetTableItemGroup); virtual;
    procedure WriteTableViewMergedCells(AView: TdxSpreadSheetTableView); virtual;
    procedure WriteTableViewOptionsPrint(AOptions: TdxSpreadSheetTableViewOptionsPrint); virtual;
    procedure WriteTableViewProperties(AView: TdxSpreadSheetTableView); virtual;
    procedure WriteTableViewSelection(AView: TdxSpreadSheetTableView); virtual;

    procedure WriteView(AView: TdxSpreadSheetCustomView); virtual;
    procedure WriteViewContainer(AContainer: TdxSpreadSheetContainer); virtual;
    procedure WriteViewContainers(AView: TdxSpreadSheetCustomView); virtual;
    procedure WriteViews; virtual;
    //
    property CellStyles: TList<TdxSpreadSheetCellStyleHandle> read FCellStyles;
    property Writer: TcxWriter read FWriter;
  public
    constructor Create(AOwner: TdxCustomSpreadSheet; AStream: TStream); override;
    destructor Destroy; override;
    procedure WriteData; override;
  end;

implementation

uses
  dxHashUtils, dxSpreadSheetStrs, cxClasses, Math, dxSpreadSheetConditionalFormatting, cxGraphics, dxSpreadSheetCoreStrs,
  dxSpreadSheetPrinting;

const
  dxSpreadSheetBinaryFormatHeaderID: Int64 = $4642327653535844;
  dxSpreadSheetBinaryFormatSectionID = $20534642;

type
  TdxDynamicItemListAccess = class(TdxDynamicItemList);
  TdxHashTableAccess = class(TdxHashTable);
  TdxSpreadSheetCellAccess = class(TdxSpreadSheetCell);
  TdxSpreadSheetContainerAccess = class(TdxSpreadSheetContainer);
  TdxSpreadSheetDefinedNameAccess = class(TdxSpreadSheetDefinedName);
  TdxSpreadSheetHyperlinksAccess = class(TdxSpreadSheetHyperlinks);
  TdxSpreadSheetTableItemAccess = class(TdxSpreadSheetTableItem);
  TdxSpreadSheetTableItemGroupAccess = class(TdxSpreadSheetTableItemGroup);
  TdxSpreadSheetTableItemGroupsAccess = class(TdxSpreadSheetTableItemGroups);
  TdxSpreadSheetTableRowAccess = class(TdxSpreadSheetTableRow);

{ TdxSpreadSheetBinaryFormatHeader }

procedure TdxSpreadSheetBinaryFormatHeader.Initialize;
begin
  ID := dxSpreadSheetBinaryFormatHeaderID;
  Version := dxSpreadSheetBinaryFormatVersion;
end;

function TdxSpreadSheetBinaryFormatHeader.IsValid: Boolean;
begin
  Result := (ID = dxSpreadSheetBinaryFormatHeaderID) and InRange(Version, 1, dxSpreadSheetBinaryFormatVersion);
end;

{ TdxSpreadSheetBinaryFormat }

class function TdxSpreadSheetBinaryFormat.CanReadFromStream(AStream: TStream): Boolean;
var
  AHeader: TdxSpreadSheetBinaryFormatHeader;
  ASavedPosition: Int64;
begin
  ASavedPosition := AStream.Position;
  try
    try
      AStream.ReadBuffer(AHeader, SizeOf(AHeader));
      Result := AHeader.IsValid;
    except
      Result := False;
    end;
  finally
    AStream.Position := ASavedPosition;
  end;
end;

class function TdxSpreadSheetBinaryFormat.CreateFormatSettings: TdxSpreadSheetFormatSettings;
begin
  Result := TdxSpreadSheetFormatSettings.Create;
end;

class function TdxSpreadSheetBinaryFormat.GetExt: string;
begin
  Result := '.bin';
end;

class function TdxSpreadSheetBinaryFormat.GetReader: TdxSpreadSheetCustomReaderClass;
begin
  Result := TdxSpreadSheetBinaryReader;
end;

class function TdxSpreadSheetBinaryFormat.GetWriter: TdxSpreadSheetCustomWriterClass;
begin
  Result := TdxSpreadSheetBinaryWriter;
end;

{ TdxSpreadSheetBinaryReader }

constructor TdxSpreadSheetBinaryReader.Create(AOwner: TdxCustomSpreadSheet; AStream: TStream);
begin
  inherited Create(AOwner, AStream);
  FFormulaRefs := TdxSpreadSheetFormulaAsTextInfoList.Create(SpreadSheet);
  FCellStyles := TList<TdxSpreadSheetCellStyleHandle>.Create;
  FContainerHyperlinks := TDictionary<Integer, TdxSpreadSheetContainer>.Create;
  FReader := TcxReader.Create(AStream);
end;

destructor TdxSpreadSheetBinaryReader.Destroy;
begin
  FreeAndNil(FFormulaRefs);
  FreeAndNil(FContainerHyperlinks);
  FreeAndNil(FCellStyles);
  FreeAndNil(FReader);
  inherited Destroy;
end;

procedure TdxSpreadSheetBinaryReader.ReadData;
var
  AHeader: TdxSpreadSheetBinaryFormatHeader;
begin
  Stream.ReadBuffer(AHeader, SizeOf(AHeader));
  if not AHeader.IsValid then
  begin
    DoError(cxGetResourceString(@sdxErrorUnsupportedDocumentFormat), ssmtError);
    Exit;
  end;

  Reader.Version := AHeader.Version;
  ReadProperties;
  ReadExternalLinks;
  ReadCellStyles;
  ReadViews;
  ReadDefinedNames;
  ReadHyperlinks;
  ResolveFormulas;
  ResolveContainerHyperlinks;
end;

function TdxSpreadSheetBinaryReader.CreateProgressHelper: TdxSpreadSheetCustomFilerProgressHelper;
begin
  Result := TdxSpreadSheetCustomFilerProgressHelper.Create(Self, 2);
end;

procedure TdxSpreadSheetBinaryReader.ReadCellStyles;
var
  ACount: Integer;
  I: Integer;
begin
  CheckSectionHeader;
  ACount := Reader.ReadInteger;
  ProgressHelper.BeginStage(ACount);
  try
    for I := 0 to ACount - 1 do
    begin
      CellStyles.Add(SpreadSheet.CellStyles.CreateStyleFromStream(Reader));
      ProgressHelper.NextTask;
    end;
  finally
    ProgressHelper.EndStage;
  end;
end;

procedure TdxSpreadSheetBinaryReader.ReadProperties;
begin
  CheckSectionHeader;
  SpreadSheet.OptionsView.DateTimeSystem := TdxSpreadSheetDateTimeSystem(Reader.ReadInteger);
  SpreadSheet.OptionsView.R1C1Reference := Reader.ReadInteger <> 0;

  if Reader.Version > 2 then
  begin
    SpreadSheet.OptionsView.GridLines := Reader.ReadBoolean;
    SpreadSheet.OptionsView.ShowFormulas := Reader.ReadBoolean;
    SpreadSheet.OptionsView.ZeroValues := Reader.ReadBoolean;
    SpreadSheet.OptionsView.Headers := Reader.ReadBoolean;
    SpreadSheet.OptionsView.HorizontalScrollBar := Reader.ReadBoolean;
    SpreadSheet.OptionsView.VerticalScrollBar := Reader.ReadBoolean;

    SpreadSheet.OptionsProtection.Protected := Reader.ReadBoolean;
    SpreadSheet.OptionsBehavior.IterativeCalculation := Reader.ReadBoolean;
    SpreadSheet.OptionsBehavior.IterativeCalculationMaxCount := Reader.ReadInteger;
  end;

  if Reader.Version > 8 then
  begin
    SpreadSheet.OptionsProtection.AllowChangeStructure := Reader.ReadBoolean;
    SpreadSheet.OptionsProtection.ProtectionInfo := ReadProtectionInfo;
  end;
end;

function TdxSpreadSheetBinaryReader.ReadProtectionInfo: IdxSpreadSheetProtectionInfo;
begin
  if Reader.ReadBoolean then
    Result := TdxSpreadSheetStandardProtectionInfo.Create(Reader.ReadWideString)
  else
    Result := nil;
end;

procedure TdxSpreadSheetBinaryReader.ReadTableView(AView: TdxSpreadSheetTableView);
begin
  ReadTableViewProperties(AView);
  ReadTableViewItems(AView.Columns);
  ReadTableViewItems(AView.Rows);
  ReadTableViewCells(AView);
  ReadTableViewMergedCells(AView);
  ReadTableViewSelection(AView);
  ReadTableViewConditionalFormatting(AView);
  if Reader.Version >= 12 then
    ReadTableViewOptionsPrint(AView.OptionsPrint);
end;

procedure TdxSpreadSheetBinaryReader.ReadTableViewCells(AView: TdxSpreadSheetTableView);
var
  ACell: TdxSpreadSheetCellAccess;
  ACellCount: Integer;
  ARow: TdxSpreadSheetTableRow;
  ARowCount: Integer;
begin
  ARowCount := Reader.ReadInteger;
  while ARowCount > 0 do
  begin
    ARow := AView.Rows.CreateItem(Reader.ReadInteger);

    ACellCount := Reader.ReadInteger;
    while ACellCount > 0 do
    begin
      ACell := TdxSpreadSheetCellAccess(ARow.CreateCell(Reader.ReadInteger));
      ACell.StyleHandle := CellStyles[Reader.ReadInteger];
      ACell.LoadFromStream(Reader, FFormulaRefs);
      Dec(ACellCount);
    end;

    ProgressHelper.NextTask;
    Dec(ARowCount);
  end;
end;

procedure TdxSpreadSheetBinaryReader.ReadTableViewConditionalFormatting(AView: TdxSpreadSheetTableView);
var
  AClass: TPersistentClass;
  ACount: Integer;
  ARule: TdxSpreadSheetConditionalFormattingCustomRule;
  ASize: Integer;
begin
  if Reader.Version > 5 then
  begin
    CheckSectionHeader;
    AView.BeginUpdate;
    try
      ACount := Reader.ReadInteger;
      while ACount > 0 do
      begin
        CheckSectionHeader;
        AClass := GetClass(Reader.ReadWideString);
        ASize := Reader.ReadInteger;
        if (AClass <> nil) and AClass.InheritsFrom(TdxSpreadSheetConditionalFormattingCustomRule) then
        begin
          ARule := TdxSpreadSheetConditionalFormattingCustomRuleClass(AClass).Create(AView.ConditionalFormatting);
          ARule.LoadFromStream(Reader);
        end
        else
          Reader.Stream.Seek(ASize, soCurrent);

        Dec(ACount);
      end;
    finally
      AView.EndUpdate;
    end;
  end;
end;

procedure TdxSpreadSheetBinaryReader.ReadTableViewItems(AItems: TdxSpreadSheetTableItems);
var
  ACount: Integer;
  AItem: TdxSpreadSheetTableItemAccess;
  AItemVisible: Boolean;
begin
  CheckSectionHeader;

  AItems.DefaultSize := Reader.ReadInteger;
  ACount := Reader.ReadInteger;
  while ACount > 0 do
  begin
    AItem := TdxSpreadSheetTableItemAccess(AItems.CreateItem(Reader.ReadInteger));
    AItem.Style.Handle := CellStyles[Reader.ReadInteger];
    AItemVisible := Reader.ReadBoolean;
    if not Reader.ReadBoolean then
      AItem.Size := Reader.ReadInteger;
    if Reader.Version > 1 then
      AItem.IsCustomSize := Reader.ReadBoolean;
    AItem.Visible := AItemVisible;
    Dec(ACount);
  end;

  if Reader.Version > 4 then
  begin
    CheckSectionHeader;
    ReadTableViewItemsGroupChildren(TdxSpreadSheetTableItemGroupsAccess(AItems.Groups).Root);
  end;
end;

procedure TdxSpreadSheetBinaryReader.ReadTableViewItemsGroupChildren(AGroup: TdxSpreadSheetTableItemGroup);
var
  ACount: Integer;
  AFinishIndex: Integer;
  AStartIndex: Integer;
  ASubGroup: TdxSpreadSheetTableItemGroup;
begin
  AGroup.BeginUpdate;
  try
    ACount := Reader.ReadInteger;
    while ACount > 0 do
    begin
      AStartIndex := Reader.ReadInteger;
      AFinishIndex := Reader.ReadInteger;
      ASubGroup := TdxSpreadSheetTableItemGroup.Create(AGroup.Owner, AGroup, AStartIndex, AFinishIndex);
      TdxSpreadSheetTableItemGroupAccess(ASubGroup).FCollapsedByUser := Reader.ReadBoolean;
      ReadTableViewItemsGroupChildren(ASubGroup);
      Dec(ACount);
    end;
  finally
    AGroup.EndUpdate;
  end;
end;

procedure TdxSpreadSheetBinaryReader.ReadTableViewMergedCells(AView: TdxSpreadSheetTableView);
var
  ACount: Integer;
begin
  ACount := Reader.ReadInteger;
  while ACount > 0 do
  begin
    AView.MergedCells.Add(Reader.ReadRect);
    Dec(ACount);
  end;
end;

procedure TdxSpreadSheetBinaryReader.ReadTableViewOptionsPrint(AOptions: TdxSpreadSheetTableViewOptionsPrint);

  procedure ReadText(AValue: TdxSpreadSheetTableViewOptionsPrintHeaderFooterText);
  begin
    AValue.LeftSection := Reader.ReadWideString;
    AValue.CenterSection := Reader.ReadWideString;
    AValue.RightSection := Reader.ReadWideString;
    AValue.Assigned := Reader.ReadBoolean;
  end;

var
  I: Integer;
begin
  // OptionsPrint.HeaderFooter
  AOptions.HeaderFooter.AlignWithMargins := TdxDefaultBoolean(Reader.ReadByte);
  AOptions.HeaderFooter.ScaleWithDocument := TdxDefaultBoolean(Reader.ReadByte);
  ReadText(AOptions.HeaderFooter.CommonFooter);
  ReadText(AOptions.HeaderFooter.CommonHeader);
  ReadText(AOptions.HeaderFooter.EvenPagesFooter);
  ReadText(AOptions.HeaderFooter.EvenPagesHeader);
  ReadText(AOptions.HeaderFooter.FirstPageFooter);
  ReadText(AOptions.HeaderFooter.FirstPageHeader);

  // OptionsPrint.Page
  AOptions.Page.FirstPageNumber := Reader.ReadInteger;
  AOptions.Page.FitToHeight := Reader.ReadInteger;
  AOptions.Page.FitToWidth := Reader.ReadInteger;
  AOptions.Page.Orientation := TdxSpreadSheetTableViewOptionsPrintPageOrientation(Reader.ReadByte);
  AOptions.Page.Scale := Reader.ReadInteger;
  AOptions.Page.ScaleMode := TdxSpreadSheetTableViewOptionsPrintPageScaleMode(Reader.ReadByte);
  // OptionsPrint.Page.Margins
  AOptions.Page.Margins.Footer := Reader.ReadSingle;
  AOptions.Page.Margins.Header := Reader.ReadSingle;
  AOptions.Page.Margins.Bottom := Reader.ReadSingle;
  AOptions.Page.Margins.Left := Reader.ReadSingle;
  AOptions.Page.Margins.Right := Reader.ReadSingle;
  AOptions.Page.Margins.Top := Reader.ReadSingle;
  AOptions.Page.Margins.Assigned := Reader.ReadBoolean;
  // OptionsPrint.Page.Paper
  AOptions.Page.Paper.SizeID := Reader.ReadInteger;
  AOptions.Page.Paper.CustomSize.X := Reader.ReadSingle;
  AOptions.Page.Paper.CustomSize.Y := Reader.ReadSingle;
  AOptions.Page.Paper.Assigned := Reader.ReadBoolean;

  // OptionsPrint.Pagination
  AOptions.Pagination.ColumnPageBreaks.Clear;
  for I := 0 to Reader.ReadInteger - 1 do
    AOptions.Pagination.ColumnPageBreaks.Add(Reader.ReadInteger);
  AOptions.Pagination.RowPageBreaks.Clear;
  for I := 0 to Reader.ReadInteger - 1 do
    AOptions.Pagination.RowPageBreaks.Add(Reader.ReadInteger);

  // OptionsPrint.Printing
  AOptions.Printing.BlackAndWhite := TdxDefaultBoolean(Reader.ReadByte);
  AOptions.Printing.Copies := Reader.ReadByte;
  AOptions.Printing.Draft := TdxDefaultBoolean(Reader.ReadByte);
  AOptions.Printing.HorizontalCentered := TdxDefaultBoolean(Reader.ReadByte);
  AOptions.Printing.PageOrder := TdxSpreadSheetTableViewOptionsPrintPrintingPageOrder(Reader.ReadByte);
  AOptions.Printing.VerticalCentered := TdxDefaultBoolean(Reader.ReadByte);

  // OptionsPrint.Source
  AOptions.Source.Area.Rect := Reader.ReadRect;
  AOptions.Source.Area.Assigned := Reader.ReadBoolean;
  AOptions.Source.CellComments := TdxSpreadSheetTableViewOptionsPrintSourceCellComments(Reader.ReadByte);
  AOptions.Source.ColumnsToRepeat.Rect := Reader.ReadRect;
  AOptions.Source.ColumnsToRepeat.Assigned := Reader.ReadBoolean;
  AOptions.Source.RowsToRepeat.Rect := Reader.ReadRect;
  AOptions.Source.RowsToRepeat.Assigned := Reader.ReadBoolean;
  AOptions.Source.ErrorIndication := TdxSpreadSheetTableViewOptionsPrintSourceErrorIndication(Reader.ReadByte);
  AOptions.Source.GridLines := TdxDefaultBoolean(Reader.ReadByte);
  AOptions.Source.Headers := TdxDefaultBoolean(Reader.ReadByte);
end;

procedure TdxSpreadSheetBinaryReader.ReadTableViewProperties(AView: TdxSpreadSheetTableView);
begin
  AView.FrozenColumn := Reader.ReadInteger;
  AView.FrozenRow := Reader.ReadInteger;
  AView.Options.ZoomFactor := Reader.ReadInteger;
  AView.Options.GridLines := TdxDefaultBoolean(Reader.ReadInteger);
  AView.Options.ShowFormulas := TdxDefaultBoolean(Reader.ReadInteger);
  AView.Options.ZeroValues := TdxDefaultBoolean(Reader.ReadInteger);

  if Reader.Version > 2 then
  begin
    AView.Options.DefaultColumnWidth := Reader.ReadInteger;
    AView.Options.DefaultRowHeight := Reader.ReadInteger;
    AView.Options.Headers := TdxDefaultBoolean(Reader.ReadInteger);
    AView.Options.HorizontalScrollBar := TdxDefaultBoolean(Reader.ReadInteger);
    AView.OptionsProtection.Protected := Reader.ReadBoolean;
    AView.Options.VerticalScrollBar := TdxDefaultBoolean(Reader.ReadInteger);
  end;

  if Reader.Version > 8 then
  begin
    AView.OptionsProtection.AllowDeleteColumns := Reader.ReadBoolean;
    AView.OptionsProtection.AllowDeleteRows := Reader.ReadBoolean;
    AView.OptionsProtection.AllowResizeColumns := Reader.ReadBoolean;
    AView.OptionsProtection.AllowEditContainers := Reader.ReadBoolean;
    AView.OptionsProtection.AllowEditHyperlinks := Reader.ReadBoolean;
    AView.OptionsProtection.AllowResizeRows := Reader.ReadBoolean;
    AView.OptionsProtection.AllowFormatCells := Reader.ReadBoolean;
    AView.OptionsProtection.AllowInsertColumns := Reader.ReadBoolean;
    AView.OptionsProtection.AllowInsertRows := Reader.ReadBoolean;
    AView.OptionsProtection.AllowSelectLockedCells := Reader.ReadBoolean;
    AView.OptionsProtection.AllowSelectUnlockedCells := Reader.ReadBoolean;
    AView.OptionsProtection.AllowSort := Reader.ReadBoolean;
    AView.OptionsProtection.ProtectionInfo := ReadProtectionInfo;
  end;
end;

procedure TdxSpreadSheetBinaryReader.ReadTableViewSelection(AView: TdxSpreadSheetTableView);
var
  ACount: Integer;
begin
  AView.Selection.Clear;
  ACount := Reader.ReadInteger;
  while ACount > 0 do
  begin
    AView.Selection.Add(Reader.ReadRect, [ssCtrl]);
    Dec(ACount);
  end;
end;

procedure TdxSpreadSheetBinaryReader.ReadView(AView: TdxSpreadSheetCustomView);
begin
  AView.Visible := Reader.ReadBoolean;
  if AView is TdxSpreadSheetTableView then
    ReadTableView(TdxSpreadSheetTableView(AView));
  ReadViewContainers(AView);
end;

procedure TdxSpreadSheetBinaryReader.ReadViewContainers(AView: TdxSpreadSheetCustomView);
var
  AClass: TClass;
  ACount: Integer;
  AContainer: TdxSpreadSheetContainerAccess;
begin
  ACount := Reader.ReadInteger;
  while ACount > 0 do
  begin
    AClass := GetClass(Reader.ReadWideString);
    if (AClass = nil) or not AClass.InheritsFrom(TdxSpreadSheetContainer) then
    begin
      DoError(cxGetResourceString(@sdxErrorUnsupportedSheetType), ssmtError);
      Abort;
    end;
    AContainer := TdxSpreadSheetContainerAccess(AView.Containers.Add(TdxSpreadSheetContainerClass(AClass)));
    AContainer.LoadFromStream(Reader);
    if (Reader.Version >= 7) and Reader.ReadBoolean then
      ContainerHyperlinks.Add(Reader.ReadInteger, AContainer);
    Dec(ACount);
  end;
end;

procedure TdxSpreadSheetBinaryReader.ReadDefinedNames;
var
  ACount: Integer;
begin
  CheckSectionHeader;
  ACount := Reader.ReadInteger;
  while ACount > 0 do
  begin
    SpreadSheet.DefinedNames.AddFromStream(Reader);
    Dec(ACount);
  end;
end;

procedure TdxSpreadSheetBinaryReader.ReadExternalLinks;
var
  ACount: Integer;
begin
  CheckSectionHeader;
  ACount := Reader.ReadInteger;
  while ACount > 0 do
  begin
    SpreadSheet.ExternalLinks.Add(Reader.ReadWideString);
    Dec(ACount);
  end;
end;

procedure TdxSpreadSheetBinaryReader.ReadHyperlinks;
var
  I: Integer;
begin
  if Reader.Version <= 6 then Exit;
  for I := 0 to SpreadSheet.SheetCount - 1 do
    if SpreadSheet.Sheets[I] is TdxSpreadSheetTableView then
      TdxSpreadSheetHyperlinksAccess(TdxSpreadSheetTableView(SpreadSheet.Sheets[I]).Hyperlinks).LoadFromStream(Reader);
end;

procedure TdxSpreadSheetBinaryReader.ReadViews;
var
  AClass: TClass;
  ACount: Integer;
begin
  CheckSectionHeader;
  ACount := Reader.ReadInteger;
  ProgressHelper.BeginStage(ACount);
  try
    while ACount > 0 do
    begin
      CheckSectionHeader;

      AClass := GetClass(Reader.ReadWideString);
      if (AClass = nil) or not AClass.InheritsFrom(TdxSpreadSheetCustomView) then
      begin
        DoError(cxGetResourceString(@sdxErrorUnsupportedSheetType), ssmtError);
        Abort;
      end;

      ReadView(SpreadSheet.AddSheet(Reader.ReadWideString, TdxSpreadSheetCustomViewClass(AClass)));
      ProgressHelper.NextTask;
      Dec(ACount);
    end;
  finally
    ProgressHelper.EndStage;
  end;
  SpreadSheet.ActiveSheetIndex := Reader.ReadInteger;
end;

procedure TdxSpreadSheetBinaryReader.ResolveContainerHyperlinks;
var
  I: Integer;
  AContainer: TdxSpreadSheetContainer;
begin
  for I in ContainerHyperlinks.Keys do
  begin
    AContainer := ContainerHyperlinks[I];
    AContainer.Hyperlink := TdxSpreadSheetTableView(AContainer.Parent).Hyperlinks[I];
  end;
end;

procedure TdxSpreadSheetBinaryReader.ResolveFormulas;
begin
  FFormulaRefs.ResolveReferences;
  FFormulaRefs.Clear;
end;

procedure TdxSpreadSheetBinaryReader.CheckSectionHeader;
begin
  if Reader.ReadCardinal <> dxSpreadSheetBinaryFormatSectionID then
  begin
    DoError(cxGetResourceString(@sdxErrorDocumentIsCorrupted), ssmtError);
    Abort;
  end;
end;

{ TdxSpreadSheetBinaryWriter }

constructor TdxSpreadSheetBinaryWriter.Create(AOwner: TdxCustomSpreadSheet; AStream: TStream);
begin
  inherited Create(AOwner, AStream);
  FCellStyles := TList<TdxSpreadSheetCellStyleHandle>.Create;
  FWriter := TcxWriter.Create(AStream);
end;

destructor TdxSpreadSheetBinaryWriter.Destroy;
begin
  FreeAndNil(FCellStyles);
  FreeAndNil(FWriter);
  inherited Destroy;
end;

procedure TdxSpreadSheetBinaryWriter.WriteData;
var
  AHeader: TdxSpreadSheetBinaryFormatHeader;
begin
  AHeader.Initialize;
  Writer.Version := AHeader.Version;
  Stream.WriteBuffer(AHeader, SizeOf(AHeader));

  WriteProperties;
  WriteExternalLinks;
  WriteCellStyles;
  WriteViews;
  WriteDefinedNames;
  WriteHyperlinks;
end;

function TdxSpreadSheetBinaryWriter.CreateProgressHelper: TdxSpreadSheetCustomFilerProgressHelper;
begin
  Result := TdxSpreadSheetCustomFilerProgressHelper.Create(Self, 1);
end;

procedure TdxSpreadSheetBinaryWriter.WriteCellStyles;
var
  I: Integer;
begin
  WriteSectionHeader;

  CellStyles.Capacity := 256;
  CellStyles.Add(SpreadSheet.CellStyles.DefaultStyle);
  TdxHashTableAccess(SpreadSheet.CellStyles).ForEach(
    procedure(AItem: TdxDynamicListItem)
    begin
      CellStyles.Add(TdxSpreadSheetCellStyleHandle(AItem));
    end);

  Writer.WriteInteger(CellStyles.Count);
  for I := 0 to CellStyles.Count - 1 do
    CellStyles.Items[I].SaveToStream(Writer);
end;

procedure TdxSpreadSheetBinaryWriter.WriteDefinedNames;
var
  I: Integer;
begin
  WriteSectionHeader;
  Writer.WriteInteger(SpreadSheet.DefinedNames.Count);
  for I := 0 to SpreadSheet.DefinedNames.Count - 1 do
    TdxSpreadSheetDefinedNameAccess(SpreadSheet.DefinedNames[I]).SaveToStream(Writer);
end;

procedure TdxSpreadSheetBinaryWriter.WriteExternalLinks;
var
  I: Integer;
begin
  WriteSectionHeader;
  Writer.WriteInteger(SpreadSheet.ExternalLinks.Count);
  for I := 0 to SpreadSheet.ExternalLinks.Count - 1 do
    Writer.WriteWideString(SpreadSheet.ExternalLinks[I].Target);
end;

procedure TdxSpreadSheetBinaryWriter.WriteHyperlinks;
var
  I: Integer;
begin
  for I := 0 to SpreadSheet.SheetCount - 1 do
    if SpreadSheet.Sheets[I] is TdxSpreadSheetTableView then
      TdxSpreadSheetHyperlinksAccess(TdxSpreadSheetTableView(SpreadSheet.Sheets[I]).Hyperlinks).SaveToStream(Writer);
end;

procedure TdxSpreadSheetBinaryWriter.WriteProperties;
begin
  WriteSectionHeader;
  Writer.WriteInteger(Ord(SpreadSheet.OptionsView.DateTimeSystem));
  Writer.WriteInteger(Ord(SpreadSheet.OptionsView.R1C1Reference));

  Writer.WriteBoolean(SpreadSheet.OptionsView.GridLines);
  Writer.WriteBoolean(SpreadSheet.OptionsView.ShowFormulas);
  Writer.WriteBoolean(SpreadSheet.OptionsView.ZeroValues);
  Writer.WriteBoolean(SpreadSheet.OptionsView.Headers);
  Writer.WriteBoolean(SpreadSheet.OptionsView.HorizontalScrollBar);
  Writer.WriteBoolean(SpreadSheet.OptionsView.VerticalScrollBar);

  Writer.WriteBoolean(SpreadSheet.OptionsProtection.Protected);
  Writer.WriteBoolean(SpreadSheet.OptionsBehavior.IterativeCalculation);
  Writer.WriteInteger(SpreadSheet.OptionsBehavior.IterativeCalculationMaxCount);

  Writer.WriteBoolean(SpreadSheet.OptionsProtection.AllowChangeStructure);
  WriteProtectionInfo(SpreadSheet.OptionsProtection.ProtectionInfo);
end;

procedure TdxSpreadSheetBinaryWriter.WriteProtectionInfo(AProtectionInfo: IdxSpreadSheetProtectionInfo);
begin
  if AProtectionInfo is TdxSpreadSheetCustomProtectionInfo then
  begin
    Writer.WriteBoolean(True);
    Writer.WriteWideString(TdxSpreadSheetCustomProtectionInfo(AProtectionInfo).Password);
  end
  else
    Writer.WriteBoolean(False);
end;

procedure TdxSpreadSheetBinaryWriter.WriteSectionHeader;
begin
  Writer.WriteCardinal(dxSpreadSheetBinaryFormatSectionID);
end;

procedure TdxSpreadSheetBinaryWriter.WriteTableView(AView: TdxSpreadSheetTableView);
begin
  WriteTableViewProperties(AView);
  WriteTableViewItems(AView.Columns);
  WriteTableViewItems(AView.Rows);
  WriteTableViewCells(AView);
  WriteTableViewMergedCells(AView);
  WriteTableViewSelection(AView);
  WriteTableViewConditionalFormatting(AView);
  WriteTableViewOptionsPrint(AView.OptionsPrint);
end;

procedure TdxSpreadSheetBinaryWriter.WriteTableViewCells(AView: TdxSpreadSheetTableView);
var
  ARowCount: Integer;
  ARowCountPosition: Int64;
begin
  WriteNullCount(ARowCount, ARowCountPosition);

  TdxDynamicItemListAccess(AView.Rows).ForEach(
    procedure (Item: TdxDynamicListItem)
    var
      ACellCount: Integer;
      ACellCountPosition: Int64;
    begin
      Writer.WriteInteger(Item.Index);
      WriteNullCount(ACellCount, ACellCountPosition);

      TdxDynamicItemListAccess(TdxSpreadSheetTableRowAccess(Item).RowCells).ForEach(
        procedure (Item: TdxDynamicListItem)
        var
          ACell: TdxSpreadSheetCellAccess;
        begin
          ACell := TdxSpreadSheetCellAccess(Item);
          Writer.WriteInteger(ACell.ColumnIndex);
          Writer.WriteInteger(CellStyles.IndexOf(ACell.StyleHandle));
          ACell.SaveToStream(Writer);
          Inc(ACellCount);
        end);

      WriteCount(ACellCount, ACellCountPosition);
      Inc(ARowCount);
    end);

  WriteCount(ARowCount, ARowCountPosition);
end;

procedure TdxSpreadSheetBinaryWriter.WriteTableViewConditionalFormatting(AView: TdxSpreadSheetTableView);
var
  ACount: Integer;
  ACountPosition: Int64;
  ARule: TdxSpreadSheetCustomConditionalFormattingRule;
  I: Integer;
begin
  WriteSectionHeader;
  Writer.WriteInteger(AView.ConditionalFormatting.RuleCount);
  for I := 0 to AView.ConditionalFormatting.RuleCount - 1 do
  begin
    WriteSectionHeader;
    ARule := AView.ConditionalFormatting.Rules[I];
    Writer.WriteWideString(ARule.ClassName);
    WriteNullCount(ACount, ACountPosition);
    ARule.SaveToStream(Writer);
    WriteCount(Writer.Stream.Position - ACountPosition, ACountPosition);
  end;
end;

procedure TdxSpreadSheetBinaryWriter.WriteTableViewItems(AItems: TdxSpreadSheetTableItems);
var
  ACount: Integer;
  ACountPosition: Int64;
begin
  WriteSectionHeader;
  Writer.WriteInteger(AItems.DefaultSize);
  WriteNullCount(ACount, ACountPosition);

  TdxDynamicItemListAccess(AItems).ForEach(
    procedure (Item: TdxDynamicListItem)
    var
      AItem: TdxSpreadSheetTableItemAccess;
    begin
      AItem := TdxSpreadSheetTableItemAccess(Item);
      Writer.WriteInteger(AItem.Index);
      Writer.WriteInteger(CellStyles.IndexOf(AItem.Style.Handle));
      Writer.WriteBoolean(AItem.Visible);
      Writer.WriteBoolean(AItem.DefaultSize);
      if not AItem.DefaultSize then
        Writer.WriteInteger(AItem.CustomSize);
      Writer.WriteBoolean(AItem.IsCustomSize);
      Inc(ACount);
    end);

  WriteCount(ACount, ACountPosition);

  WriteSectionHeader;
  WriteTableViewItemsGroupChildren(TdxSpreadSheetTableItemGroupsAccess(AItems.Groups).Root);
end;

procedure TdxSpreadSheetBinaryWriter.WriteTableViewItemsGroupChildren(AGroup: TdxSpreadSheetTableItemGroup);
var
  AChildGroup: TdxSpreadSheetTableItemGroup;
  I: Integer;
begin
  Writer.WriteInteger(AGroup.Count);
  for I := 0 to AGroup.Count - 1 do
  begin
    AChildGroup := AGroup.Items[I];
    Writer.WriteInteger(AChildGroup.StartIndex);
    Writer.WriteInteger(AChildGroup.FinishIndex);
    Writer.WriteBoolean(TdxSpreadSheetTableItemGroupAccess(AChildGroup).IsCollapsedByUser);
    WriteTableViewItemsGroupChildren(AChildGroup);
  end;
end;

procedure TdxSpreadSheetBinaryWriter.WriteTableViewMergedCells(AView: TdxSpreadSheetTableView);
var
  I: Integer;
begin
  Writer.WriteInteger(AView.MergedCells.Count);
  for I := 0 to AView.MergedCells.Count - 1 do
    Writer.WriteRect(AView.MergedCells.Items[I].Area);
end;

procedure TdxSpreadSheetBinaryWriter.WriteTableViewOptionsPrint(AOptions: TdxSpreadSheetTableViewOptionsPrint);

  procedure WriteText(AValue: TdxSpreadSheetTableViewOptionsPrintHeaderFooterText);
  begin
    Writer.WriteWideString(AValue.LeftSection);
    Writer.WriteWideString(AValue.CenterSection);
    Writer.WriteWideString(AValue.RightSection);
    Writer.WriteBoolean(AValue.Assigned);
  end;

var
  I: Integer;
begin
  // OptionsPrint.HeaderFooter
  Writer.WriteByte(Ord(AOptions.HeaderFooter.AlignWithMargins));
  Writer.WriteByte(Ord(AOptions.HeaderFooter.ScaleWithDocument));
  WriteText(AOptions.HeaderFooter.CommonFooter);
  WriteText(AOptions.HeaderFooter.CommonHeader);
  WriteText(AOptions.HeaderFooter.EvenPagesFooter);
  WriteText(AOptions.HeaderFooter.EvenPagesHeader);
  WriteText(AOptions.HeaderFooter.FirstPageFooter);
  WriteText(AOptions.HeaderFooter.FirstPageHeader);

  // OptionsPrint.Page
  Writer.WriteInteger(AOptions.Page.FirstPageNumber);
  Writer.WriteInteger(AOptions.Page.FitToHeight);
  Writer.WriteInteger(AOptions.Page.FitToWidth);
  Writer.WriteByte(Ord(AOptions.Page.Orientation));
  Writer.WriteInteger(AOptions.Page.Scale);
  Writer.WriteByte(Ord(AOptions.Page.ScaleMode));
  // OptionsPrint.Page.Margins
  Writer.WriteSingle(AOptions.Page.Margins.Footer);
  Writer.WriteSingle(AOptions.Page.Margins.Header);
  Writer.WriteSingle(AOptions.Page.Margins.Bottom);
  Writer.WriteSingle(AOptions.Page.Margins.Left);
  Writer.WriteSingle(AOptions.Page.Margins.Right);
  Writer.WriteSingle(AOptions.Page.Margins.Top);
  Writer.WriteBoolean(AOptions.Page.Margins.Assigned);
  // OptionsPrint.Page.Paper
  Writer.WriteInteger(AOptions.Page.Paper.SizeID);
  Writer.WriteSingle(AOptions.Page.Paper.CustomSize.X);
  Writer.WriteSingle(AOptions.Page.Paper.CustomSize.Y);
  Writer.WriteBoolean(AOptions.Page.Paper.Assigned);

  // OptionsPrint.Pagination
  Writer.WriteInteger(AOptions.Pagination.ColumnPageBreaks.Count);
  for I := 0 to AOptions.Pagination.ColumnPageBreaks.Count - 1 do
    Writer.WriteInteger(AOptions.Pagination.ColumnPageBreaks[I]);
  Writer.WriteInteger(AOptions.Pagination.RowPageBreaks.Count);
  for I := 0 to AOptions.Pagination.RowPageBreaks.Count - 1 do
    Writer.WriteInteger(AOptions.Pagination.RowPageBreaks[I]);

  // OptionsPrint.Printing
  Writer.WriteByte(Ord(AOptions.Printing.BlackAndWhite));
  Writer.WriteByte(AOptions.Printing.Copies);
  Writer.WriteByte(Ord(AOptions.Printing.Draft));
  Writer.WriteByte(Ord(AOptions.Printing.HorizontalCentered));
  Writer.WriteByte(Ord(AOptions.Printing.PageOrder));
  Writer.WriteByte(Ord(AOptions.Printing.VerticalCentered));

  // OptionsPrint.Source
  Writer.WriteRect(AOptions.Source.Area.Rect);
  Writer.WriteBoolean(AOptions.Source.Area.Assigned);
  Writer.WriteByte(Ord(AOptions.Source.CellComments));
  Writer.WriteRect(AOptions.Source.ColumnsToRepeat.Rect);
  Writer.WriteBoolean(AOptions.Source.ColumnsToRepeat.Assigned);
  Writer.WriteRect(AOptions.Source.RowsToRepeat.Rect);
  Writer.WriteBoolean(AOptions.Source.RowsToRepeat.Assigned);
  Writer.WriteByte(Ord(AOptions.Source.ErrorIndication));
  Writer.WriteByte(Ord(AOptions.Source.GridLines));
  Writer.WriteByte(Ord(AOptions.Source.Headers));
end;

procedure TdxSpreadSheetBinaryWriter.WriteTableViewProperties(AView: TdxSpreadSheetTableView);
begin
  Writer.WriteInteger(AView.FrozenColumn);
  Writer.WriteInteger(AView.FrozenRow);
  Writer.WriteInteger(AView.Options.ZoomFactor);
  Writer.WriteInteger(Ord(AView.Options.GridLines));
  Writer.WriteInteger(Ord(AView.Options.ShowFormulas));
  Writer.WriteInteger(Ord(AView.Options.ZeroValues));

  Writer.WriteInteger(AView.Options.DefaultColumnWidth);
  Writer.WriteInteger(AView.Options.DefaultRowHeight);
  Writer.WriteInteger(Ord(AView.Options.Headers));
  Writer.WriteInteger(Ord(AView.Options.HorizontalScrollBar));
  Writer.WriteBoolean(AView.OptionsProtection.Protected);
  Writer.WriteInteger(Ord(AView.Options.VerticalScrollBar));

  Writer.WriteBoolean(AView.OptionsProtection.AllowDeleteColumns);
  Writer.WriteBoolean(AView.OptionsProtection.AllowDeleteRows);
  Writer.WriteBoolean(AView.OptionsProtection.AllowResizeColumns);
  Writer.WriteBoolean(AView.OptionsProtection.AllowEditContainers);
  Writer.WriteBoolean(AView.OptionsProtection.AllowEditHyperlinks);
  Writer.WriteBoolean(AView.OptionsProtection.AllowResizeRows);
  Writer.WriteBoolean(AView.OptionsProtection.AllowFormatCells);
  Writer.WriteBoolean(AView.OptionsProtection.AllowInsertColumns);
  Writer.WriteBoolean(AView.OptionsProtection.AllowInsertRows);
  Writer.WriteBoolean(AView.OptionsProtection.AllowSelectLockedCells);
  Writer.WriteBoolean(AView.OptionsProtection.AllowSelectUnlockedCells);
  Writer.WriteBoolean(AView.OptionsProtection.AllowSort);
  WriteProtectionInfo(AView.OptionsProtection.ProtectionInfo);
end;

procedure TdxSpreadSheetBinaryWriter.WriteTableViewSelection(AView: TdxSpreadSheetTableView);
var
  I: Integer;
begin
  Writer.WriteInteger(AView.Selection.Count);
  for I := 0 to AView.Selection.Count - 1 do
    Writer.WriteRect(AView.Selection[I].Rect);
end;

procedure TdxSpreadSheetBinaryWriter.WriteView(AView: TdxSpreadSheetCustomView);
begin
  WriteSectionHeader;
  Writer.WriteWideString(AView.ClassName);
  Writer.WriteWideString(AView.Caption);
  Writer.WriteBoolean(AView.Visible);
  if AView is TdxSpreadSheetTableView then
    WriteTableView(TdxSpreadSheetTableView(AView));
  WriteViewContainers(AView);
end;

procedure TdxSpreadSheetBinaryWriter.WriteViewContainer(AContainer: TdxSpreadSheetContainer);
begin
  Writer.WriteWideString(AContainer.ClassName);
  TdxSpreadSheetContainerAccess(AContainer).SaveToStream(Writer);
  Writer.WriteBoolean(AContainer.Hyperlink <> nil);
  if AContainer.Hyperlink <> nil then
    Writer.WriteInteger(AContainer.Hyperlink.Index);
end;

procedure TdxSpreadSheetBinaryWriter.WriteViewContainers(AView: TdxSpreadSheetCustomView);
var
  I: Integer;
begin
  Writer.WriteInteger(AView.Containers.Count);
  for I := 0 to AView.Containers.Count - 1 do
    WriteViewContainer(AView.Containers[I]);
end;

procedure TdxSpreadSheetBinaryWriter.WriteViews;
var
  I: Integer;
begin
  WriteSectionHeader;
  Writer.WriteInteger(SpreadSheet.SheetCount);

  ProgressHelper.BeginStage(SpreadSheet.SheetCount);
  try
    for I := 0 to SpreadSheet.SheetCount - 1 do
    begin
      WriteView(SpreadSheet.Sheets[I]);
      ProgressHelper.NextTask;
    end;
  finally
    ProgressHelper.EndStage;
  end;

  Writer.WriteInteger(SpreadSheet.ActiveSheetIndex);
end;

procedure TdxSpreadSheetBinaryWriter.WriteCount(const ACount: Integer; const ACountFieldPosition: Int64);
var
  APosition: Int64;
begin
  APosition := Writer.Stream.Position;
  Writer.Stream.Position := ACountFieldPosition;
  Writer.WriteInteger(ACount);
  Writer.Stream.Position := APosition;
end;

procedure TdxSpreadSheetBinaryWriter.WriteNullCount(out ACount: Integer; out ACountFieldPosition: Int64);
begin
  ACount := 0;
  ACountFieldPosition := Writer.Stream.Position;
  Writer.WriteInteger(ACount);
end;

initialization
  TdxSpreadSheetBinaryFormat.Register;

finalization
  TdxSpreadSheetBinaryFormat.Unregister;
end.
